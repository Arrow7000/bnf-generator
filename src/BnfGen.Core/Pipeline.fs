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
          Fatal: bool }

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
          Fatal = true }

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

                { ParseError = None
                  Diagnostics = report.Diagnostics
                  Samples = samples
                  DistinctCount = distinctCount
                  Truncated = truncated
                  Ambiguous = ambiguous
                  Fatal = false }
