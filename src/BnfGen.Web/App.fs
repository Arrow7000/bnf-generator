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

// The rep/depth sliders treat their maximum as "unlimited".
let private repSliderMax = 8
let private depthSliderMax = 8

type Model =
    { Source: string
      MaxSize: int
      ShowAdvanced: bool
      RepLimit: int
      DepthLimit: int
      Output: Pipeline.Output }

type Msg =
    | SetSource of string
    | SetSize of int
    | ToggleAdvanced
    | SetReps of int
    | SetDepth of int

let private filtersOf (m: Model) : Pipeline.Filters =
    if not m.ShowAdvanced then
        Pipeline.noFilters
    else
        { MaxReps = (if m.RepLimit >= repSliderMax then None else Some m.RepLimit)
          MaxDepth = (if m.DepthLimit >= depthSliderMax then None else Some m.DepthLimit) }

let private regenerate (model: Model) : Model =
    { model with Output = Pipeline.generateWith (filtersOf model) model.Source model.MaxSize displayLimit }

let private init () : Model =
    regenerate
        { Source = sampleGrammar
          MaxSize = 18
          ShowAdvanced = false
          RepLimit = repSliderMax
          DepthLimit = depthSliderMax
          Output = Pipeline.generate "" 0 0 }

let private update (msg: Msg) (model: Model) : Model =
    match msg with
    | SetSource s -> regenerate { model with Source = s }
    | SetSize n -> regenerate { model with MaxSize = n }
    | ToggleAdvanced -> regenerate { model with ShowAdvanced = not model.ShowAdvanced }
    | SetReps n -> regenerate { model with RepLimit = n }
    | SetDepth n -> regenerate { model with DepthLimit = n }

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

let private optInt =
    function
    | Some n -> string n
    | None -> "-"

/// Fixed properties of the grammar itself - these never change as you move the
/// sliders.
let private grammarView (out: Pipeline.Output) =
    match out.Summary with
    | None -> Html.none
    | Some s ->
        Html.div
            [ prop.className "panel"
              prop.children
                  [ Html.div
                        [ prop.className "metric-label panel-head"
                          prop.children
                              [ Html.h2 [ prop.text "Grammar (fixed)" ]
                                infoTip
                                    [ "These are intrinsic properties of the grammar, computed by static analysis."
                                      "They do not depend on the size slider or filters - moving those never changes anything here." ] ] ]
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
                                    "Saturates at size"
                                    [ "The smallest size bound at which every rule and branch becomes reachable - the point of full coverage."
                                      "This is a fixed property of the grammar: it can be computed without generating anything. Your slider tells you whether you've reached it yet (see 'This run')." ]
                                    (textValue (optInt s.SaturationSize)) ] ] ] ]

let private coverageBar (label: string) (covered: int) (total: int) =
    let pct =
        if total = 0 then 0.0 else float covered / float total * 100.0

    let full = total > 0 && covered = total

    Html.div
        [ prop.className "cov-row"
          prop.children
              [ Html.span
                    [ prop.className "cov-label"
                      prop.children
                          [ Html.span [ prop.text (label + " ") ]
                            Html.span
                                [ prop.className (if full then "cov-num cov-done" else "cov-num cov-partial")
                                  prop.text (string covered) ]
                            Html.span [ prop.className "metric-muted"; prop.text (sprintf " / %d" total) ] ] ]
                Html.div
                    [ prop.className "cov-track"
                      prop.children
                          [ Html.div
                                [ prop.className (if full then "cov-fill cov-fill-done" else "cov-fill")
                                  prop.style [ style.width (length.percent pct) ] ] ] ] ] ]

/// A small line chart of cumulative distinct samples vs size, with a dashed
/// marker at the saturation size.
let private growthChart (points: (int * int) list) (saturation: int option) =
    match points with
    | []
    | [ _ ] -> Html.p [ prop.className "muted"; prop.text "Not enough data to plot growth." ]
    | _ ->
        let w, h = 240.0, 70.0
        let xs = points |> List.map (fst >> float)
        let minX, maxX = List.min xs, List.max xs
        let maxY = points |> List.map (snd >> float) |> List.max |> max 1.0
        let sx x = if maxX = minX then 0.0 else (x - minX) / (maxX - minX) * w
        let sy y = h - (y / maxY) * h

        let pointStr =
            points
            |> List.map (fun (x, y) -> sprintf "%.1f,%.1f" (sx (float x)) (sy (float y)))
            |> String.concat " "

        let satLine =
            match saturation with
            | Some s when float s >= minX && float s <= maxX ->
                let x = sx (float s)
                [ Svg.line
                      [ svg.x1 x
                        svg.y1 0.0
                        svg.x2 x
                        svg.y2 h
                        svg.stroke "#ffb454"
                        svg.strokeWidth 1.0
                        svg.strokeDasharray [| 3; 3 |] ] ]
            | _ -> []

        Svg.svg
            [ svg.viewBox (0, 0, int w, int h)
              svg.className "growth-svg"
              svg.children (satLine @ [ Svg.polyline [ svg.points pointStr; svg.fill "none"; svg.stroke "#6ea8fe"; svg.strokeWidth 1.5 ] ]) ]

/// Values that depend on your current size bound and filters.
let private thisRunView (out: Pipeline.Output) =
    match out.Summary with
    | None -> Html.none
    | Some s ->
        let statusValue =
            if s.FullyCovered then
                Html.span [ prop.className "status-done"; prop.text "fully covered" ]
            else
                let hint =
                    match s.SaturationSize with
                    | Some n -> sprintf "partial - full at size %d" n
                    | None -> "partial"

                Html.span [ prop.className "status-partial"; prop.text hint ]

        Html.div
            [ prop.className "panel"
              prop.children
                  [ Html.div
                        [ prop.className "metric-label panel-head"
                          prop.children
                              [ Html.h2 [ prop.text "This run (your settings)" ]
                                infoTip
                                    [ "These values reflect what your current size bound and filters actually produced."
                                      "Move the size slider or the width/depth filters and watch them respond." ] ] ]
                    Html.div
                        [ prop.className "summary-grid"
                          prop.children
                              [ metric
                                    "Distinct samples"
                                    [ "Distinct rendered strings produced at the current settings." ]
                                    (textValue (string out.DistinctCount))
                                metric
                                    "Coverage"
                                    [ "Whether the current size bound is enough to exercise every rule and branch."
                                      "If partial, raise the size to the saturation point shown under 'Grammar (fixed)'." ]
                                    statusValue
                                metric
                                    "Max loop reps"
                                    [ "The most times any single * or + loop repeated in a sample so far."
                                      "0 = loops were always skipped; 2+ = the generator entered loops rather than bailing at the first exit." ]
                                    (textValue (string s.MaxLoopReps))
                                metric
                                    "Max recursion depth"
                                    [ "The deepest a single rule nested inside itself so far."
                                      "1 = no recursion exercised; 2+ = recursive structure was explored, e.g. nested parentheses." ]
                                    (textValue (string s.MaxRecursionDepth))
                                metric
                                    "Minimal cover"
                                    [ "The smallest set of samples that together hit every rule and branch reached so far (greedy set-cover)."
                                      "Exhaustiveness as a handful of examples rather than a flood: these samples are starred in the list below." ]
                                    (textValue (sprintf "%d sample(s)" s.MinimalCoverSize)) ] ]
                    coverageBar "Rules" s.RulesCovered s.RulesTotal
                    coverageBar "Branches" s.BranchesCovered s.BranchesTotal
                    Html.div [ prop.className "chart-wrap"; prop.children [ growthChart s.Growth s.SaturationSize ] ]
                    Html.p
                        [ prop.className "chart-caption"
                          prop.text "distinct samples (y) vs derivation size (x); dashed line = saturation" ] ] ]

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

    let renderSegments (segs: Render.Segment list) =
        if List.isEmpty segs then
            [ Html.span [ prop.className "muted"; prop.text "(empty string)" ] ]
        else
            segs
            |> List.map (fun seg ->
                match seg with
                | Render.TextSeg t -> Html.span [ prop.text t ]
                | Render.ClassSeg (rep, label) ->
                    // Custom (instant) tooltip rather than the native title,
                    // which has a long hover delay.
                    Html.span
                        [ prop.className "class-rep tip"
                          prop.children
                              [ Html.span [ prop.text rep ]
                                Html.span
                                    [ prop.className "tip-content tip-content-sm"
                                      prop.children
                                          [ Html.p
                                                [ prop.className "tip-p"
                                                  prop.text (sprintf "%s class - one member shown; the class is not exhausted." label) ] ] ] ] ])

    let rows =
        out.Samples
        |> List.map (fun s ->
            Html.div
                [ prop.className (if s.InMinimalCover then "sample sample-cover" else "sample")
                  prop.children
                      [ Html.span [ prop.className "sample-size"; prop.text (string s.Size) ]
                        Html.code [ prop.className "sample-text"; prop.children (renderSegments s.Segments) ]
                        if s.InMinimalCover then
                            Html.span
                                [ prop.className "cover-chip"
                                  prop.title "Part of the minimal covering set"
                                  prop.text "cover" ] ] ])

    Html.div
        [ prop.className "panel"
          prop.children
              [ Html.h2 [ prop.text "Samples" ]
                header
                columns
                Html.div [ prop.className "sample-list"; prop.children rows ] ] ]

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
                                                    let currentPreset =
                                                        presets
                                                        |> List.tryFind (fun (_, src) -> src = model.Source)
                                                        |> Option.map fst

                                                    Html.select
                                                        [ prop.className "preset-select"
                                                          prop.value (defaultArg currentPreset "")
                                                          prop.onChange (fun (name: string) ->
                                                              presets
                                                              |> List.tryFind (fun (n, _) -> n = name)
                                                              |> Option.iter (fun (_, src) -> dispatch (SetSource src)))
                                                          prop.children
                                                              [ Html.option
                                                                    [ prop.value ""
                                                                      prop.text (
                                                                          if Option.isSome currentPreset then "Load a preset..."
                                                                          else "Custom grammar"
                                                                      ) ]
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
                                                          prop.onChange (fun (v: string) -> dispatch (SetSize(int v))) ]
                                                    Html.button
                                                        [ prop.className "advanced-toggle"
                                                          prop.onClick (fun _ -> dispatch ToggleAdvanced)
                                                          prop.text (if model.ShowAdvanced then "Hide full control" else "Full control") ]
                                                    if model.ShowAdvanced then
                                                        Html.div
                                                            [ prop.className "advanced"
                                                              prop.children
                                                                  [ Html.label
                                                                        [ prop.className "metric-label"
                                                                          prop.children
                                                                              [ Html.span
                                                                                    [ prop.text (
                                                                                          if model.RepLimit >= repSliderMax then "Max loop reps: none"
                                                                                          else sprintf "Max loop reps: %d" model.RepLimit
                                                                                      ) ]
                                                                                infoTip
                                                                                    [ "Width control: caps how many times any * or + loop may repeat."
                                                                                      "Lower it to keep samples short and wide-but-shallow; at the maximum it is unconstrained." ] ] ]
                                                                    Html.input
                                                                        [ prop.type' "range"
                                                                          prop.min 0
                                                                          prop.max repSliderMax
                                                                          prop.value model.RepLimit
                                                                          prop.onChange (fun (v: string) -> dispatch (SetReps(int v))) ]
                                                                    Html.label
                                                                        [ prop.className "metric-label"
                                                                          prop.children
                                                                              [ Html.span
                                                                                    [ prop.text (
                                                                                          if model.DepthLimit >= depthSliderMax then "Max recursion depth: none"
                                                                                          else sprintf "Max recursion depth: %d" model.DepthLimit
                                                                                      ) ]
                                                                                infoTip
                                                                                    [ "Depth control: caps how deeply any single rule may nest inside itself."
                                                                                      "Lower it to keep recursion shallow; at the maximum it is unconstrained." ] ] ]
                                                                    Html.input
                                                                        [ prop.type' "range"
                                                                          prop.min 1
                                                                          prop.max depthSliderMax
                                                                          prop.value model.DepthLimit
                                                                          prop.onChange (fun (v: string) -> dispatch (SetDepth(int v))) ] ] ] ] ] ] ]
                            Html.div
                                [ prop.className "results"
                                  prop.children
                                      [ grammarView model.Output
                                        thisRunView model.Output
                                        diagnosticsView model.Output
                                        samplesView model.Output ] ] ] ] ] ]

// ---------------------------------------------------------------------------
// Program
// ---------------------------------------------------------------------------

Program.mkSimple init update view
|> Program.withReactSynchronous "app"
|> Program.run
