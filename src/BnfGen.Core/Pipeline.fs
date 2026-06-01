namespace BnfGen

/// The end-to-end facade used by the CLI and the web UI: parse a grammar,
/// analyze it, and (when viable) enumerate a deduplicated set of sample strings
/// up to a size bound.
module Pipeline =

    open Ast

    /// A generated sample plus metadata about how it was derived.
    type Sample =
        { Text: string
          Size: int
          Rules: Set<string> }

    /// Whole-grammar metrics computed over the enumerated derivations.
    type Summary =
        { Language: LanguageKind
          /// Size of the smallest derivation (None if none fit the bound).
          MinSize: int option
          RulesCovered: int
          RulesTotal: int
          /// Alternatives (every `|`, at any nesting) taken at least once.
          BranchesCovered: int
          BranchesTotal: int
          /// Smallest size at which the covered set stopped growing, i.e. the
          /// "you are now exhaustive (for this criterion)" point. Only fully
          /// meaningful when FullyCovered is true; otherwise it is where the
          /// achieved (partial) coverage plateaued within the bound.
          SaturationSize: int option
          /// True when every coverable rule and branch was hit within the bound.
          FullyCovered: bool
          /// Largest number of repetitions of a single loop seen in any sample.
          MaxLoopReps: int
          /// Deepest single-rule self-nesting seen (>= 2 means recursion).
          MaxRecursionDepth: int }

    /// The full result of a generation request.
    type Output =
        { ParseError: Parser.ParseError option
          Diagnostics: Diagnostic list
          Samples: Sample list
          /// Distinct sample strings found (may exceed the displayed `Samples`).
          DistinctCount: int
          /// True if enumeration hit the internal scan cap before exhausting.
          Truncated: bool
          /// True if two distinct derivations produced the same string.
          Ambiguous: bool
          /// True if a parse error or fatal diagnostic prevented enumeration.
          Fatal: bool
          /// Whole-grammar metrics (None on parse error or fatal analysis).
          Summary: Summary option }

    /// Upper bound on derivations scanned per request, to stay responsive even
    /// for explosive grammars at large sizes.
    let private scanCap = 20000

    let private emptyOutput =
        { ParseError = None
          Diagnostics = []
          Samples = []
          DistinctCount = 0
          Truncated = false
          Ambiguous = false
          Fatal = true
          Summary = None }

    /// Parse + analyze + enumerate.
    ///   `maxSize` bounds the derivation-tree node count.
    ///   `limit`   bounds how many distinct samples are returned for display.
    let generate (source: string) (maxSize: int) (limit: int) : Output =
        match Parser.parse source with
        | Result.Error pe -> { emptyOutput with ParseError = Some pe }
        | Result.Ok grammar ->
            let report = Analysis.analyze grammar

            if report.Fatal then
                { emptyOutput with
                    Diagnostics = report.Diagnostics
                    Fatal = true }
            else
                let opts = Render.defaultOptions

                // Scan up to the cap (plus one to detect truncation).
                let raw =
                    Enumerate.enumerate grammar maxSize
                    |> Seq.truncate (scanCap + 1)
                    |> Seq.toList

                let truncated = List.length raw > scanCap
                let raw = if truncated then List.truncate scanCap raw else raw

                // Deduplicate by rendered string, preserving first occurrence
                // and flagging ambiguity (same string from different trees).
                let samplesRev, _, ambiguous =
                    raw
                    |> List.fold
                        (fun (acc, seen, amb) node ->
                            let text = Render.render opts node

                            if Set.contains text seen then
                                (acc, seen, true)
                            else
                                let sample =
                                    { Text = text
                                      Size = nodeSize node
                                      Rules = Render.rulesUsed node }

                                (sample :: acc, Set.add text seen, amb))
                        ([], Set.empty, false)

                let distinct = List.rev samplesRev
                let distinctCount = List.length distinct

                let samples =
                    distinct
                    |> List.sortBy (fun s -> s.Size, s.Text)
                    |> List.truncate limit

                // Coverage + loop/recursion metrics over the same scan.
                let useful = Set.intersect report.Productive report.Reachable
                let targets = Coverage.targets grammar useful

                let infos =
                    raw |> List.map (fun node -> nodeSize node, Coverage.analyzeDerivation grammar node)

                let coveredAll =
                    infos |> List.fold (fun acc (_, di) -> Set.union acc di.Tokens) Set.empty

                let minSize =
                    match infos with
                    | [] -> None
                    | _ -> Some(infos |> List.map fst |> List.min)

                let maxReps = infos |> List.fold (fun m (_, di) -> max m di.MaxReps) 0
                let maxDepth = infos |> List.fold (fun m (_, di) -> max m di.MaxDepth) 0

                // Smallest size at which cumulative coverage reaches its maximum.
                let saturation =
                    let target = Set.count coveredAll
                    let sorted = infos |> List.sortBy fst
                    let mutable cumulative = Set.empty
                    let mutable result = None

                    for (sz, di) in sorted do
                        if Option.isNone result then
                            cumulative <- Set.union cumulative di.Tokens

                            if Set.count cumulative = target && target > 0 then
                                result <- Some sz

                    result

                let rulesTotal = Coverage.countKind "R:" targets
                let branchesTotal = Coverage.countKind "B:" targets
                let rulesCovered = Coverage.countKind "R:" coveredAll
                let branchesCovered = Coverage.countKind "B:" coveredAll

                let summary =
                    { Language = report.Language
                      MinSize = minSize
                      RulesCovered = rulesCovered
                      RulesTotal = rulesTotal
                      BranchesCovered = branchesCovered
                      BranchesTotal = branchesTotal
                      SaturationSize = saturation
                      FullyCovered =
                        rulesTotal > 0
                        && rulesCovered = rulesTotal
                        && branchesCovered = branchesTotal
                      MaxLoopReps = maxReps
                      MaxRecursionDepth = maxDepth }

                { ParseError = None
                  Diagnostics = report.Diagnostics
                  Samples = samples
                  DistinctCount = distinctCount
                  Truncated = truncated
                  Ambiguous = ambiguous
                  Fatal = false
                  Summary = Some summary }
