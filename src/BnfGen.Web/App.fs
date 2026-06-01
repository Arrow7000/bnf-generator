module BnfGen.Web.App

open Elmish
open Elmish.React
open Feliz
open BnfGen

// ---------------------------------------------------------------------------
// Model
// ---------------------------------------------------------------------------

let private sampleGrammar =
    "expr   ::= term (\"+\" term)*\n"
    + "term   ::= factor (\"*\" factor)*\n"
    + "factor ::= [0-9] | \"(\" expr \")\""

/// Built-in grammars, ordered roughly simple -> complex, including one that is
/// deliberately broken to show the error path.
let private presets : (string * string) list =
    [ "Greeting (finite)", "greeting ::= (\"hello\" | \"hi\") \" \" (\"world\" | \"there\")"
      "Optional and repetition", "s ::= \"a\" \"b\"? \"c\"*"
      "Balanced parentheses", "parens ::= \"(\" parens \")\" | \"\""
      "Recursive list (left)", "list ::= list \",\" \"x\" | \"x\""
      "Arithmetic (infinite)", sampleGrammar
      "JSON value",
      ("value    ::= object | array | string | number | \"true\" | \"false\" | \"null\"\n"
       + "object   ::= \"{}\" | \"{\" members \"}\"\n"
       + "members  ::= pair | pair \",\" members\n"
       + "pair     ::= string \":\" value\n"
       + "array    ::= \"[]\" | \"[\" elements \"]\"\n"
       + "elements ::= value | value \",\" elements\n"
       + "string   ::= '\"' [a-z]+ '\"'\n"
       + "number   ::= [0-9]+")
      "Non-productive (error demo)", "a ::= \"x\" a" ]

let private displayLimit = 200

type Model =
    { Source: string
      MaxSize: int
      Output: Pipeline.Output }

type Msg =
    | SetSource of string
    | SetSize of int

let private regenerate (model: Model) : Model =
    { model with Output = Pipeline.generate model.Source model.MaxSize displayLimit }

let private init () : Model =
    regenerate
        { Source = sampleGrammar
          MaxSize = 18
          Output = Pipeline.generate "" 0 0 }

let private update (msg: Msg) (model: Model) : Model =
    match msg with
    | SetSource s -> regenerate { model with Source = s }
    | SetSize n -> regenerate { model with MaxSize = n }

// ---------------------------------------------------------------------------
// View
// ---------------------------------------------------------------------------

let private severityClass (s: Ast.Severity) =
    match s with
    | Ast.Error -> "diag diag-error"
    | Ast.Warning -> "diag diag-warn"
    | Ast.Info -> "diag diag-info"

let private severityLabel (s: Ast.Severity) =
    match s with
    | Ast.Error -> "error"
    | Ast.Warning -> "warning"
    | Ast.Info -> "info"

let private laneLabel (l: Ast.Lane) =
    match l with
    | Ast.Generation -> "generation"
    | Ast.Parsing -> "parsing"
    | Ast.Structure -> "structure"

/// A hover tooltip: a small "i" marker that reveals styled, multi-paragraph
/// content. Pure CSS (no state), so it works anywhere.
let private infoTip (paragraphs: string list) =
    Html.span
        [ prop.className "tip"
          prop.children
              [ Html.span [ prop.className "tip-icon"; prop.text "i" ]
                Html.span
                    [ prop.className "tip-content"
                      prop.children (paragraphs |> List.map (fun p -> Html.p [ prop.className "tip-p"; prop.text p ])) ] ] ]

let private metric (label: string) (tip: string list) (value: ReactElement) =
    Html.div
        [ prop.className "metric"
          prop.children
              [ Html.span
                    [ prop.className "metric-label"
                      prop.children [ Html.span [ prop.text label ]; infoTip tip ] ]
                Html.span [ prop.className "metric-value"; prop.children [ value ] ] ] ]

let private textValue (s: string) = Html.span [ prop.text s ]

let private languageBadge (k: Ast.LanguageKind) =
    let cls, txt =
        match k with
        | Ast.Empty -> "badge badge-error", "empty"
        | Ast.Finite -> "badge badge-ok", "finite"
        | Ast.Infinite -> "badge badge-info", "infinite"

    Html.span [ prop.className cls; prop.text txt ]

let private summaryView (out: Pipeline.Output) =
    match out.Summary with
    | None -> Html.none
    | Some s ->
        let optInt =
            function
            | Some n -> string n
            | None -> "-"

        let coverageNote =
            if s.FullyCovered then "fully covered"
            else "partial within bound"

        // A partial plateau within a too-small bound is not real saturation, so
        // only show the size when coverage is actually complete.
        let saturationText =
            if s.FullyCovered then optInt s.SaturationSize else "raise size"

        Html.div
            [ prop.className "panel"
              prop.children
                  [ Html.h2 [ prop.text "Grammar summary" ]
                    Html.div
                        [ prop.className "summary-grid"
                          prop.children
                              [ metric
                                    "Language"
                                    [ "Finite = a fixed set of strings you could list in full."
                                      "Infinite = a loop or recursion lets strings grow without bound, e.g. a* or list ::= list ',' x."
                                      "Empty = no finite string exists at all (rejected as a fatal error)." ]
                                    (languageBadge s.Language)
                                metric
                                    "Min size"
                                    [ "Node count of the smallest possible derivation tree: the cheapest string the grammar can make."
                                      "Bigger means the grammar forces more structure before it can 'bottom out' to terminals." ]
                                    (textValue (optInt s.MinSize))
                                metric
                                    "Rule coverage"
                                    [ "How many useful rules have appeared in at least one sample, out of the total."
                                      "N / N means every rule has been exercised somewhere." ]
                                    (textValue (sprintf "%d / %d" s.RulesCovered s.RulesTotal))
                                metric
                                    "Branch coverage"
                                    [ "How many alternatives (every '|', including nested ones) have been taken, out of the total."
                                      "This is the practical exhaustiveness meter: N / N means every choice has been demonstrated at least once." ]
                                    (textValue (sprintf "%d / %d" s.BranchesCovered s.BranchesTotal))
                                metric
                                    "Saturates at size"
                                    [ "The smallest size bound at which coverage stops growing."
                                      "Past this point, raising the slider yields more and longer samples but no new rules or branches: you've structurally seen everything."
                                      "Shows 'raise size' while coverage is still partial: some branches need a bigger bound than the slider currently allows." ]
                                    (textValue saturationText)
                                metric
                                    "Coverage"
                                    [ "Fully covered = every rule and branch within reach was hit."
                                      "Partial within bound = some rules/branches need a larger size than the current slider allows." ]
                                    (textValue coverageNote)
                                metric
                                    "Max loop reps"
                                    [ "The most times any single * or + loop repeated in a sample."
                                      "0 = loops were always skipped; 2+ = the generator actually entered loops instead of bailing at the first exit." ]
                                    (textValue (string s.MaxLoopReps))
                                metric
                                    "Max recursion depth"
                                    [ "The deepest a single rule nested inside itself across all samples."
                                      "1 = no recursion exercised; 2+ = recursive structure was explored, e.g. nested parentheses." ]
                                    (textValue (string s.MaxRecursionDepth)) ] ] ] ]

let private diagnosticsView (out: Pipeline.Output) =
    let items =
        match out.ParseError with
        | Some pe ->
            [ Html.div
                  [ prop.className "diag diag-error"
                    prop.children
                        [ Html.span [ prop.className "diag-tag"; prop.text "parse error" ]
                          Html.span [ prop.text (sprintf "line %d, col %d: %s" pe.Line pe.Column pe.Message) ] ] ] ]
        | None ->
            out.Diagnostics
            |> List.map (fun d ->
                Html.div
                    [ prop.className (severityClass d.Severity)
                      prop.children
                          [ Html.span [ prop.className "diag-tag"; prop.text (severityLabel d.Severity) ]
                            Html.span [ prop.className "diag-lane"; prop.text (laneLabel d.Lane) ]
                            Html.span [ prop.text d.Message ] ] ])

    Html.div
        [ prop.className "panel"
          prop.children
              [ Html.h2 [ prop.text "Diagnostics" ]
                if List.isEmpty items then
                    Html.p [ prop.className "muted"; prop.text "No issues found." ]
                else
                    Html.div [ prop.className "diag-list"; prop.children items ] ] ]

let private samplesView (out: Pipeline.Output) =
    let header =
        if out.Fatal then
            Html.p [ prop.className "muted"; prop.text "No samples: the grammar has fatal errors." ]
        else
            Html.div
                [ prop.className "samples-meta"
                  prop.children
                      [ Html.span
                            [ prop.className "metric-label"
                              prop.children
                                  [ Html.span [ prop.text (sprintf "%d distinct sample(s)" out.DistinctCount) ]
                                    infoTip
                                        [ "Distinct rendered strings among all derivations with tree size <= the current bound."
                                          "Different derivations can render to the same string (that is what ambiguity is); those duplicates are merged here and flagged 'ambiguous'." ] ] ]
                        if out.Truncated then
                            Html.span [ prop.className "badge badge-warn"; prop.text "truncated" ]
                        if out.Ambiguous then
                            Html.span [ prop.className "badge badge-info"; prop.text "ambiguous" ] ] ]

    let columns =
        Html.div
            [ prop.className "sample sample-head"
              prop.children
                  [ Html.span
                        [ prop.className "sample-size metric-label"
                          prop.children [ Html.span [ prop.text "size" ]; infoTip [ "Derivation-tree node count for this sample. This is NOT a line number and NOT string length."; "Values can skip (some sizes are unreachable) and repeat (different strings can share a size). Samples are sorted smallest-first." ] ] ]
                    Html.span [ prop.className "sample-text muted"; prop.text "sample" ] ] ]

    let rows =
        out.Samples
        |> List.map (fun s ->
            Html.div
                [ prop.className "sample"
                  prop.children
                      [ Html.span [ prop.className "sample-size"; prop.text (string s.Size) ]
                        Html.code
                            [ prop.className "sample-text"
                              prop.text (if s.Text = "" then "(empty string)" else s.Text) ] ] ])

    Html.div
        [ prop.className "panel"
          prop.children
              [ Html.h2 [ prop.text "Samples" ]
                header
                Html.div [ prop.className "sample-list"; prop.children (columns :: rows) ] ] ]

let private view (model: Model) (dispatch: Msg -> unit) =
    Html.div
        [ prop.className "app"
          prop.children
              [ Html.header
                    [ prop.className "app-header"
                      prop.children
                          [ Html.h1 [ prop.text "EBNF Sample Generator" ]
                            Html.p
                                [ prop.className "subtitle"
                                  prop.text
                                      "Bounded exhaustive enumeration of derivations from a W3C-style EBNF grammar." ] ] ]
                Html.div
                    [ prop.className "layout"
                      prop.children
                          [ Html.div
                                [ prop.className "panel editor"
                                  prop.children
                                      [ Html.div
                                            [ prop.className "editor-head"
                                              prop.children
                                                  [ Html.h2 [ prop.text "Grammar" ]
                                                    Html.select
                                                        [ prop.className "preset-select"
                                                          prop.value ""
                                                          prop.onChange (fun (name: string) ->
                                                              presets
                                                              |> List.tryFind (fun (n, _) -> n = name)
                                                              |> Option.iter (fun (_, src) -> dispatch (SetSource src)))
                                                          prop.children
                                                              [ Html.option [ prop.value ""; prop.text "Load a preset..." ]
                                                                for (name, _) in presets do
                                                                    Html.option [ prop.value name; prop.text name ] ] ] ] ]
                                        Html.textarea
                                            [ prop.className "grammar-input"
                                              prop.value model.Source
                                              prop.onChange (SetSource >> dispatch) ]
                                        Html.div
                                            [ prop.className "size-control"
                                              prop.children
                                                  [ Html.label
                                                        [ prop.htmlFor "size"
                                                          prop.className "metric-label"
                                                          prop.children
                                                              [ Html.span [ prop.text (sprintf "Max derivation size (tree nodes): %d" model.MaxSize) ]
                                                                infoTip
                                                                    [ "Bounds the number of nodes in each derivation tree. It is NOT string length."
                                                                      "Every rule expansion and every loop repetition costs at least one node, so a finite budget guarantees the search terminates (even with left recursion)."
                                                                      "Raise it to explore deeper and longer derivations; watch the coverage and saturation numbers respond." ] ] ]
                                                    Html.input
                                                        [ prop.id "size"
                                                          prop.type' "range"
                                                          prop.min 1
                                                          prop.max 40
                                                          prop.value model.MaxSize
                                                          prop.onChange (fun (v: string) ->
                                                              dispatch (SetSize(int v))) ] ] ] ] ]
                            Html.div
                                [ prop.className "results"
                                  prop.children
                                      [ summaryView model.Output
                                        diagnosticsView model.Output
                                        samplesView model.Output ] ] ] ] ] ]

// ---------------------------------------------------------------------------
// Program
// ---------------------------------------------------------------------------

Program.mkSimple init update view
|> Program.withReactSynchronous "app"
|> Program.run
