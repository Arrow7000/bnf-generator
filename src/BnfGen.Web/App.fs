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
          MaxSize = 16
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
                      [ Html.span [ prop.text (sprintf "%d distinct sample(s)" out.DistinctCount) ]
                        if out.Truncated then
                            Html.span [ prop.className "badge badge-warn"; prop.text "truncated" ]
                        if out.Ambiguous then
                            Html.span [ prop.className "badge badge-info"; prop.text "ambiguous" ] ] ]

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
                                      [ Html.h2 [ prop.text "Grammar" ]
                                        Html.textarea
                                            [ prop.className "grammar-input"
                                              prop.value model.Source
                                              prop.onChange (SetSource >> dispatch) ]
                                        Html.div
                                            [ prop.className "size-control"
                                              prop.children
                                                  [ Html.label
                                                        [ prop.htmlFor "size"
                                                          prop.text (sprintf "Max derivation size (tree nodes): %d" model.MaxSize) ]
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
                                  prop.children [ diagnosticsView model.Output; samplesView model.Output ] ] ] ] ] ]

// ---------------------------------------------------------------------------
// Program
// ---------------------------------------------------------------------------

Program.mkSimple init update view
|> Program.withReactSynchronous "app"
|> Program.run
