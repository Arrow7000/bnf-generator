namespace BnfGen

/// The end-to-end facade used by the CLI and the web UI: parse a grammar,
/// analyze it, and (when viable) enumerate a deduplicated set of sample strings
/// up to a size bound, with optional width/depth filters and coverage metrics.
module Pipeline =

    open Ast

    /// A generated sample plus metadata about how it was derived.
    type Sample =
        { Text: string
          /// Structured rendering, so the UI can highlight char-class reps.
          Segments: Render.Segment list
          Size: int
          Rules: Set<string>
          /// True if this sample is part of the minimal covering set.
          InMinimalCover: bool }

    /// Optional width/depth filters applied on top of the size bound. `None`
    /// means unconstrained.
    type Filters =
        { MaxReps: int option
          MaxDepth: int option }

    let noFilters = { MaxReps = None; MaxDepth = None }

    /// Whole-grammar metrics computed over the enumerated derivations.
    type Summary =
        { Language: LanguageKind
          MinSize: int option
          RulesCovered: int
          RulesTotal: int
          BranchesCovered: int
          BranchesTotal: int
          SaturationSize: int option
          FullyCovered: bool
          MaxLoopReps: int
          MaxRecursionDepth: int
          /// Number of samples in the minimal covering set (greedy set-cover).
          MinimalCoverSize: int
          /// (size, cumulative distinct samples with that size or smaller),
          /// for the growth curve.
          Growth: (int * int) list }

    /// The full result of a generation request.
    type Output =
        { ParseError: Parser.ParseError option
          Diagnostics: Diagnostic list
          Samples: Sample list
          /// Distinct samples found across the whole explored space.
          DistinctCount: int
          /// Distinct samples within the requested display size range.
          DisplayCount: int
          Truncated: bool
          Ambiguous: bool
          Fatal: bool
          Summary: Summary option }

    // Upper bound on derivations scanned for the sample list, growth curve and
    // minimal-cover demonstration. Coverage and saturation are computed
    // statically and do not depend on this, so a modest cap keeps the tool
    // responsive even for explosive grammars.
    let private scanCap = 6000

    let private emptyOutput =
        { ParseError = None
          Diagnostics = []
          Samples = []
          DistinctCount = 0
          DisplayCount = 0
          Truncated = false
          Ambiguous = false
          Fatal = true
          Summary = None }

    /// Greedy set cover: choose candidate keys until `target` is covered,
    /// always taking the candidate that adds the most still-uncovered tokens.
    let private greedyCover (target: Set<string>) (candidates: (string * Set<string>) list) : Set<string> =
        let rec loop (covered: Set<string>) (chosen: Set<string>) =
            if Set.isSubset target covered then
                chosen
            else
                let scored =
                    candidates
                    |> List.map (fun (key, toks) -> key, toks, Set.count (Set.difference toks covered))

                match scored |> List.sortByDescending (fun (_, _, gain) -> gain) |> List.tryHead with
                | Some (key, toks, gain) when gain > 0 -> loop (Set.union covered toks) (Set.add key chosen)
                | _ -> chosen

        loop Set.empty Set.empty

    /// Parse + analyze + enumerate, with width/depth filters.
    ///   `maxSize`        bounds the explored space (coverage/saturation depend on it).
    ///   `minDisplaySize` hides samples below this size from the list (a view-only
    ///                    filter; it does not change coverage, and cover members
    ///                    are always shown).
    let generateWith (filters: Filters) (source: string) (maxSize: int) (minDisplaySize: int) (limit: int) : Output =
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

                // The sample scan is bounded two ways: by a count cap and by a
                // work budget (for explosive grammars). Either makes the sample
                // list incomplete, which we report via `truncated`.
                let budget = ref Enumerate.defaultWorkBudget

                let rawAll =
                    Enumerate.enumerateWithin grammar maxSize budget
                    |> Seq.truncate (scanCap + 1)
                    |> Seq.toList

                let hitScanCap = List.length rawAll > scanCap
                let truncated = hitScanCap || budget.Value <= 0
                let rawAll = if hitScanCap then List.truncate scanCap rawAll else rawAll

                // Per-node coverage/metric info, computed once.
                let withInfo =
                    rawAll
                    |> List.map (fun node -> node, nodeSize node, Coverage.analyzeDerivation grammar node)

                // Width/depth filters.
                let passes (info: Coverage.DerivInfo) =
                    (match filters.MaxReps with
                     | Some m -> info.MaxReps <= m
                     | None -> true)
                    && (match filters.MaxDepth with
                        | Some m -> info.MaxDepth <= m
                        | None -> true)

                let filtered = withInfo |> List.filter (fun (_, _, info) -> passes info)

                // Deduplicate by rendered string, keeping the first derivation.
                // Class members are varied deterministically by hashing the node.
                let distinctRev, _, ambiguous =
                    filtered
                    |> List.fold
                        (fun (acc, seen, amb) (node, size, info: Coverage.DerivInfo) ->
                            let segments = Render.renderSegments (hash node) opts node
                            let text = Render.segmentText segments

                            if Set.contains text seen then
                                (acc, seen, true)
                            else
                                let entry =
                                    {| Text = text
                                       Segments = segments
                                       Size = size
                                       Rules = Render.rulesUsed node
                                       Tokens = info.Tokens |}

                                (entry :: acc, Set.add text seen, amb))
                        ([], Set.empty, false)

                let distinct = List.rev distinctRev
                let distinctCount = List.length distinct

                // Coverage is computed STATICALLY: a target is covered at bound N
                // iff its minimum-cover size <= N. This is monotonic and immune to
                // scan-cap truncation (unlike counting over the scanned set, which
                // can undercount an explosive grammar's branches).
                let useful = Set.intersect report.Productive report.Reachable

                let targetSizes =
                    Coverage.targetMinSizes grammar report.Productive report.Reachable report.MinCost

                let targetKeys = targetSizes |> Map.toSeq |> Seq.map fst |> Set.ofSeq

                let coveredKeys =
                    targetSizes
                    |> Map.toSeq
                    |> Seq.filter (fun (_, sz) -> sz <= maxSize)
                    |> Seq.map fst
                    |> Set.ofSeq

                let rulesTotal = Coverage.countKind "R:" targetKeys
                let branchesTotal = Coverage.countKind "B:" targetKeys
                let rulesCovered = Coverage.countKind "R:" coveredKeys
                let branchesCovered = Coverage.countKind "B:" coveredKeys

                let fullyCovered =
                    rulesTotal > 0
                    && rulesCovered = rulesTotal
                    && branchesCovered = branchesTotal

                // Static: smallest derivation size the grammar admits at all.
                let minSize =
                    match Map.tryFind grammar.Start report.MinCost with
                    | Some v -> Some(1 + v)
                    | None -> None

                // Static: where coverage saturates, regardless of the slider.
                let saturation =
                    if Map.isEmpty targetSizes then
                        None
                    else
                        Some(targetSizes |> Map.toSeq |> Seq.map snd |> Seq.max)

                // What the actually-scanned samples demonstrate (may be less than
                // the static coverage when truncated).
                let scanCovered = distinct |> List.fold (fun acc e -> Set.union acc e.Tokens) Set.empty

                let maxReps = filtered |> List.fold (fun m (_, _, i: Coverage.DerivInfo) -> max m i.MaxReps) 0
                let maxDepth = filtered |> List.fold (fun m (_, _, i: Coverage.DerivInfo) -> max m i.MaxDepth) 0

                // Growth curve: cumulative distinct samples by size.
                let growth =
                    match minSize with
                    | None -> []
                    | Some lo ->
                        let bySize = distinct |> List.countBy (fun e -> e.Size) |> Map.ofList
                        let mutable cumulative = 0

                        [ for n in lo..maxSize do
                              cumulative <- cumulative + (Map.tryFind n bySize |> Option.defaultValue 0)
                              yield (n, cumulative) ]

                // Minimal covering set, over the actual scanned samples. Only
                // offered when those samples genuinely demonstrate full coverage
                // (they may not, if an explosive grammar was truncated even though
                // it is statically fully covered). Candidates are all distinct
                // samples, smallest first.
                let coverDemonstrable = fullyCovered && Set.isSubset targetKeys scanCovered

                let coverKeys =
                    if coverDemonstrable then
                        distinct
                        |> List.sortBy (fun e -> e.Size, e.Text)
                        |> List.map (fun e -> e.Text, e.Tokens)
                        |> greedyCover targetKeys
                    else
                        Set.empty

                // Apply the min-size view filter, then show the smallest `limit`,
                // but always include every cover member (regardless of the filter
                // or the display limit) so the cover is never hidden.
                let sortedAll = distinct |> List.sortBy (fun e -> e.Size, e.Text)
                let inRange = sortedAll |> List.filter (fun e -> e.Size >= minDisplaySize)
                let displayCount = List.length inRange
                let shown = inRange |> List.truncate limit
                let shownTexts = shown |> List.map (fun e -> e.Text) |> Set.ofList

                let extraCover =
                    sortedAll
                    |> List.filter (fun e -> Set.contains e.Text coverKeys && not (Set.contains e.Text shownTexts))

                let samples =
                    (shown @ extraCover)
                    |> List.map (fun e ->
                        { Text = e.Text
                          Segments = e.Segments
                          Size = e.Size
                          Rules = e.Rules
                          InMinimalCover = Set.contains e.Text coverKeys })
                    |> List.sortBy (fun s -> s.Size, s.Text)

                let summary =
                    { Language = report.Language
                      MinSize = minSize
                      RulesCovered = rulesCovered
                      RulesTotal = rulesTotal
                      BranchesCovered = branchesCovered
                      BranchesTotal = branchesTotal
                      SaturationSize = saturation
                      FullyCovered = fullyCovered
                      MaxLoopReps = maxReps
                      MaxRecursionDepth = maxDepth
                      MinimalCoverSize = Set.count coverKeys
                      Growth = growth }

                { ParseError = None
                  Diagnostics = report.Diagnostics
                  Samples = samples
                  DistinctCount = distinctCount
                  DisplayCount = displayCount
                  Truncated = truncated
                  Ambiguous = ambiguous
                  Fatal = false
                  Summary = Some summary }

    /// Parse + analyze + enumerate with no filters.
    let generate (source: string) (maxSize: int) (limit: int) : Output =
        generateWith noFilters source maxSize 1 limit
