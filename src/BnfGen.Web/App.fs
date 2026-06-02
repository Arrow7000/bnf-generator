module BnfGen.Web.App

open Elmish
open Elmish.React
open Feliz
open Fable.Core
open Fable.Core.JsInterop
open Fable.SimpleHttp
open BnfGen

// ---------------------------------------------------------------------------
// Configuration
// ---------------------------------------------------------------------------

let private presets = Presets.all

/// The backend base URL is injected at build time by Vite (VITE_API_URL). In
/// local dev it falls back to the API's default port.
let private apiBase: string =
    let raw: obj = emitJsExpr () "import.meta.env.VITE_API_URL"

    if isNullOrUndefined raw then
        "http://localhost:8099"
    else
        let s = unbox<string> raw
        if s = "" then "http://localhost:8099" else s

// ---------------------------------------------------------------------------
// Wire types for the /api/generate response (decoded dynamically to avoid a
// JSON-decoder dependency)
// ---------------------------------------------------------------------------

type Diag = { Severity: string; Lane: string; Message: string }

type Facts =
    { Language: string
      MinSize: int option
      SaturationSize: int option }

type ApiResult =
    { Samples: string list
      Gbnf: string
      Diagnostics: Diag list
      Facts: Facts option
      Model: string option
      Error: string option
      ParseError: (int * int * string) option }

let private decode (text: string) : ApiResult =
    let o = JS.JSON.parse text

    let arr (x: obj) : obj[] =
        if isNullOrUndefined x then [||] else unbox x

    let optInt (x: obj) : int option =
        if isNullOrUndefined x then None else Some(unbox<int> x)

    let optStr (x: obj) : string option =
        if isNullOrUndefined x then None else Some(unbox<string> x)

    let diagnostics =
        arr (o?diagnostics)
        |> Array.map (fun d ->
            { Severity = unbox d?severity
              Lane = unbox d?lane
              Message = unbox d?message })
        |> Array.toList

    let samples =
        arr (o?samples) |> Array.map (fun s -> unbox<string> s) |> Array.toList

    let facts =
        if isNullOrUndefined (o?facts) then
            None
        else
            Some
                { Language = unbox o?facts?language
                  MinSize = optInt (o?facts?minSize)
                  SaturationSize = optInt (o?facts?saturationSize) }

    let parseError =
        if isNullOrUndefined (o?parseError) then
            None
        else
            Some(unbox<int> o?parseError?line, unbox<int> o?parseError?column, unbox<string> o?parseError?message)

    { Samples = samples
      Gbnf = (if isNullOrUndefined (o?gbnf) then "" else unbox o?gbnf)
      Diagnostics = diagnostics
      Facts = facts
      Model = optStr (o?model)
      Error = optStr (o?error)
      ParseError = parseError }

// ---------------------------------------------------------------------------
// Model
// ---------------------------------------------------------------------------

type Model =
    { Source: string
      Count: int
      Loading: bool
      Result: ApiResult option
      Failure: string option
      ShowGbnf: bool }

type Msg =
    | SetSource of string
    | SelectPreset of string
    | SetCount of int
    | Generate
    | GotResult of ApiResult
    | Failed of string
    | ToggleGbnf

let private generateCmd (model: Model) : Cmd<Msg> =
    let payload =
        sprintf "{\"grammar\":%s,\"count\":%d}" (JS.JSON.stringify model.Source) model.Count

    let run () =
        async {
            let! response =
                Http.request (apiBase.TrimEnd('/') + "/api/generate")
                |> Http.method POST
                |> Http.content (BodyContent.Text payload)
                |> Http.header (Headers.contentType "application/json")
                |> Http.send

            return decode response.responseText
        }

    Cmd.OfAsync.either run () GotResult (fun ex -> Failed ex.Message)

let private init () : Model * Cmd<Msg> =
    let model =
        { Source = Presets.defaultSource
          Count = 10
          Loading = true
          Result = None
          Failure = None
          ShowGbnf = false }

    model, generateCmd model

let private update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | SetSource s -> { model with Source = s }, Cmd.none
    | SelectPreset s ->
        let m = { model with Source = s; Loading = true; Failure = None }
        m, generateCmd m
    | SetCount n -> { model with Count = n }, Cmd.none
    | Generate ->
        let m = { model with Loading = true; Failure = None }
        m, generateCmd m
    | GotResult r ->
        { model with
            Loading = false
            Result = Some r
            Failure = None },
        Cmd.none
    | Failed e ->
        { model with
            Loading = false
            Failure = Some e },
        Cmd.none
    | ToggleGbnf -> { model with ShowGbnf = not model.ShowGbnf }, Cmd.none

// ---------------------------------------------------------------------------
// View
// ---------------------------------------------------------------------------

let private severityClass =
    function
    | "error" -> "diag diag-error"
    | "warning" -> "diag diag-warn"
    | _ -> "diag diag-info"

let private diagnosticsView (diags: Diag list) =
    if List.isEmpty diags then
        Html.none
    else
        Html.div
            [ prop.className "panel"
              prop.children
                  [ Html.h2 [ prop.text "Diagnostics" ]
                    Html.div
                        [ prop.className "diag-list"
                          prop.children
                              [ for d in diags ->
                                    Html.div
                                        [ prop.className (severityClass d.Severity)
                                          prop.children
                                              [ Html.span [ prop.className "diag-tag"; prop.text d.Severity ]
                                                Html.span [ prop.className "diag-lane"; prop.text d.Lane ]
                                                Html.span [ prop.text d.Message ] ] ] ] ] ] ]

let private optInt =
    function
    | Some n -> string n
    | None -> "-"

let private factsView (facts: Facts) =
    Html.div
        [ prop.className "facts"
          prop.children
              [ Html.span
                    [ prop.className "fact"
                      prop.children
                          [ Html.span [ prop.className "fact-label"; prop.text "language" ]
                            Html.span [ prop.className "fact-value"; prop.text facts.Language ] ] ]
                Html.span
                    [ prop.className "fact"
                      prop.children
                          [ Html.span [ prop.className "fact-label"; prop.text "min size" ]
                            Html.span [ prop.className "fact-value"; prop.text (optInt facts.MinSize) ] ] ]
                Html.span
                    [ prop.className "fact"
                      prop.children
                          [ Html.span [ prop.className "fact-label"; prop.text "saturates at" ]
                            Html.span [ prop.className "fact-value"; prop.text (optInt facts.SaturationSize) ] ] ] ] ]

let private samplesView (model: Model) =
    let body =
        if model.Loading then
            [ Html.p [ prop.className "muted"; prop.text "Generating samples..." ] ]
        else
            match model.Failure with
            | Some e ->
                [ Html.div
                      [ prop.className "diag diag-error"
                        prop.children
                            [ Html.span [ prop.className "diag-tag"; prop.text "network" ]
                              Html.span [ prop.text (sprintf "Could not reach the generator: %s" e) ] ] ] ]
            | None ->
                match model.Result with
                | None -> [ Html.p [ prop.className "muted"; prop.text "Press Generate to create samples." ] ]
                | Some r ->
                    match r.ParseError with
                    | Some (line, col, m) ->
                        [ Html.div
                              [ prop.className "diag diag-error"
                                prop.children
                                    [ Html.span [ prop.className "diag-tag"; prop.text "parse error" ]
                                      Html.span [ prop.text (sprintf "line %d, col %d: %s" line col m) ] ] ] ]
                    | None ->
                        match r.Error with
                        | Some e when List.isEmpty r.Samples ->
                            [ Html.div
                                  [ prop.className "diag diag-error"
                                    prop.children
                                        [ Html.span [ prop.className "diag-tag"; prop.text "error" ]
                                          Html.span [ prop.text e ] ] ] ]
                        | _ when List.isEmpty r.Samples ->
                            [ Html.p [ prop.className "muted"; prop.text "No samples were returned." ] ]
                        | _ ->
                            [ Html.div
                                  [ prop.className "sample-list"
                                    prop.children
                                        [ for s in r.Samples ->
                                              Html.div
                                                  [ prop.className "sample"
                                                    prop.children
                                                        [ Html.code
                                                              [ prop.className "sample-text"
                                                                prop.text (if s = "" then "(empty string)" else s) ] ] ] ] ] ]

    let meta =
        match model.Result with
        | Some r when not (List.isEmpty r.Samples) ->
            Html.div
                [ prop.className "samples-meta"
                  prop.children
                      [ Html.span [ prop.text (sprintf "%d distinct sample(s)" (List.length r.Samples)) ]
                        match r.Model with
                        | Some m -> Html.span [ prop.className "model-chip"; prop.text m ]
                        | None -> Html.none ] ]
        | _ -> Html.none

    Html.div
        [ prop.className "panel"
          prop.children [ Html.h2 [ prop.text "Samples" ]; meta; yield! body ] ]

let private gbnfView (model: Model) (dispatch: Msg -> unit) =
    match model.Result with
    | Some r when r.Gbnf <> "" ->
        Html.div
            [ prop.className "panel"
              prop.children
                  [ Html.button
                        [ prop.className "advanced-toggle"
                          prop.onClick (fun _ -> dispatch ToggleGbnf)
                          prop.text (if model.ShowGbnf then "Hide compiled grammar" else "Show compiled grammar") ]
                    if model.ShowGbnf then
                        Html.pre [ prop.className "gbnf"; prop.text r.Gbnf ] ] ]
    | _ -> Html.none

let private view (model: Model) (dispatch: Msg -> unit) =
    let factsPanel =
        match model.Result with
        | Some r ->
            match r.Facts with
            | Some f -> factsView f
            | None -> Html.none
        | None -> Html.none

    let diagsPanel =
        match model.Result with
        | Some r -> diagnosticsView r.Diagnostics
        | None -> Html.none

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
                                      "Realistic example strings from a W3C-style EBNF grammar, generated by a grammar-constrained LLM." ] ] ]
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
                                                              |> Option.iter (fun (_, src) -> dispatch (SelectPreset src)))
                                                          prop.children
                                                              [ Html.option
                                                                    [ prop.value ""
                                                                      prop.text (
                                                                          if Option.isSome currentPreset then
                                                                              "Load a preset..."
                                                                          else
                                                                              "Custom grammar"
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
                                                        [ prop.className "metric-label"
                                                          prop.children [ Html.span [ prop.text (sprintf "Samples: %d" model.Count) ] ] ]
                                                    Html.input
                                                        [ prop.type' "range"
                                                          prop.min 1
                                                          prop.max 30
                                                          prop.value model.Count
                                                          prop.onChange (fun (v: string) -> dispatch (SetCount(int v))) ] ] ]
                                        Html.button
                                            [ prop.className "generate-btn"
                                              prop.disabled model.Loading
                                              prop.onClick (fun _ -> dispatch Generate)
                                              prop.text (if model.Loading then "Generating..." else "Generate") ] ] ]
                            Html.div
                                [ prop.className "results"
                                  prop.children [ factsPanel; samplesView model; diagsPanel; gbnfView model dispatch ] ] ] ] ] ]

// ---------------------------------------------------------------------------
// Program
// ---------------------------------------------------------------------------

Program.mkProgram init update view
|> Program.withReactSynchronous "app"
|> Program.run
