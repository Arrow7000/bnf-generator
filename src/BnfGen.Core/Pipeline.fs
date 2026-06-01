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
          DistinctCount: int
          Truncated: bool
          Ambiguous: bool
          Fatal: bool
          Summary: Summary option }

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
    let generateWith (filters: Filters) (source: string) (maxSize: int) (limit: int) : Output =
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

                let rawAll =
                    Enumerate.enumerate grammar maxSize
                    |> Seq.truncate (scanCap + 1)
                    |> Seq.toList

                let truncated = List.length rawAll > scanCap
                let rawAll = if truncated then List.truncate scanCap rawAll else rawAll

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
                let distinctRev, _, ambiguous =
                    filtered
                    |> List.fold
                        (fun (acc, seen, amb) (node, size, info: Coverage.DerivInfo) ->
                            let text = Render.render opts node

                            if Set.contains text seen then
                                (acc, seen, true)
                            else
                                let entry =
                                    {| Text = text
                                       Segments = Render.renderSegments opts node
                                       Size = size
                                       Rules = Render.rulesUsed node
                                       Tokens = info.Tokens |}

                                (entry :: acc, Set.add text seen, amb))
                        ([], Set.empty, false)

                let distinct = List.rev distinctRev
                let distinctCount = List.length distinct

                // Coverage.
                let useful = Set.intersect report.Productive report.Reachable
                let targets = Coverage.targets grammar useful
                let coveredAll = distinct |> List.fold (fun acc e -> Set.union acc e.Tokens) Set.empty
                let rulesTotal = Coverage.countKind "R:" targets
                let branchesTotal = Coverage.countKind "B:" targets
                let rulesCovered = Coverage.countKind "R:" coveredAll
                let branchesCovered = Coverage.countKind "B:" coveredAll

                let fullyCovered =
                    rulesTotal > 0
                    && rulesCovered = rulesTotal
                    && branchesCovered = branchesTotal

                let minSize =
                    match distinct with
                    | [] -> None
                    | _ -> Some(distinct |> List.map (fun e -> e.Size) |> List.min)

                let maxReps = filtered |> List.fold (fun m (_, _, i: Coverage.DerivInfo) -> max m i.MaxReps) 0
                let maxDepth = filtered |> List.fold (fun m (_, _, i: Coverage.DerivInfo) -> max m i.MaxDepth) 0

                // Saturation: smallest size at which cumulative coverage maxes.
                let saturation =
                    let target = Set.count coveredAll
                    let sorted = distinct |> List.sortBy (fun e -> e.Size)
                    let mutable cumulative = Set.empty
                    let mutable result = None

                    for e in sorted do
                        if Option.isNone result then
                            cumulative <- Set.union cumulative e.Tokens

                            if Set.count cumulative = target && target > 0 then
                                result <- Some e.Size

                    result

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

                // Minimal covering set. Samples up to the saturation size already
                // achieve full coverage, so we only search among those.
                let coverCutoff =
                    match saturation with
                    | Some n -> n
                    | None -> maxSize

                let coverCandidates =
                    distinct
                    |> List.filter (fun e -> e.Size <= coverCutoff)
                    |> List.map (fun e -> e.Text, e.Tokens)

                let coverKeys = greedyCover coveredAll coverCandidates

                let samples =
                    distinct
                    |> List.map (fun e ->
                        { Text = e.Text
                          Segments = e.Segments
                          Size = e.Size
                          Rules = e.Rules
                          InMinimalCover = Set.contains e.Text coverKeys })
                    |> List.sortBy (fun s -> s.Size, s.Text)
                    |> List.truncate limit

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
                  Truncated = truncated
                  Ambiguous = ambiguous
                  Fatal = false
                  Summary = Some summary }

    /// Parse + analyze + enumerate with no width/depth filters.
    let generate (source: string) (maxSize: int) (limit: int) : Output =
        generateWith noFilters source maxSize limit
