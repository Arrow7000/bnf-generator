module BnfGen.Api.Program

open System
open System.Net.Http
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection
open BnfGen
open BnfGen.Api

// ---------------------------------------------------------------------------
// Request / response DTOs (plain shapes so System.Text.Json is happy with F#)
// ---------------------------------------------------------------------------

[<CLIMutable>]
type GenerateRequest =
    { Grammar: string
      Count: Nullable<int>
      Temperature: Nullable<float>
      MaxTokens: Nullable<int>
      Model: string }

[<CLIMutable>]
type DiagnosticDto = { Severity: string; Lane: string; Message: string }

[<CLIMutable>]
type FactsDto =
    { Language: string
      MinSize: Nullable<int>
      SaturationSize: Nullable<int> }

[<CLIMutable>]
type GenerateResponse =
    { Samples: string[]
      Gbnf: string
      Diagnostics: DiagnosticDto[]
      Facts: FactsDto
      Model: string }

[<CLIMutable>]
type ParseErrorDto = { Line: int; Column: int; Message: string }

[<CLIMutable>]
type ErrorResponse =
    { Error: string
      ParseError: ParseErrorDto
      Diagnostics: DiagnosticDto[]
      Gbnf: string }

// ---------------------------------------------------------------------------
// Mapping helpers
// ---------------------------------------------------------------------------

let private sevStr =
    function
    | Ast.Error -> "error"
    | Ast.Warning -> "warning"
    | Ast.Info -> "info"

let private laneStr =
    function
    | Ast.Generation -> "generation"
    | Ast.Parsing -> "parsing"
    | Ast.Structure -> "structure"

let private langStr =
    function
    | Ast.Empty -> "empty"
    | Ast.Finite -> "finite"
    | Ast.Infinite -> "infinite"

let private toDiag (d: Ast.Diagnostic) : DiagnosticDto =
    { Severity = sevStr d.Severity
      Lane = laneStr d.Lane
      Message = d.Message }

let private diagsOf (report: Analysis.Report) =
    report.Diagnostics |> List.map toDiag |> List.toArray

let private factsOf (g: Ast.Grammar) (report: Analysis.Report) : FactsDto =
    let minSize =
        match Map.tryFind g.Start report.MinCost with
        | Some v -> Nullable(1 + v)
        | None -> Nullable()

    let sat =
        match Coverage.saturationSize g report.Productive report.Reachable report.MinCost with
        | Some n -> Nullable n
        | None -> Nullable()

    { Language = langStr report.Language
      MinSize = minSize
      SaturationSize = sat }

let private intOr (d: int) (n: Nullable<int>) = if n.HasValue then n.Value else d
let private floatOr (d: float) (n: Nullable<float>) = if n.HasValue then n.Value else d

// ---------------------------------------------------------------------------
// Prompts. The grammar already guarantees validity, so we only ask for variety.
// ---------------------------------------------------------------------------

let private systemPrompt =
    "You generate one example string that conforms to a formal grammar. The decoder is already constrained to the grammar, so every token you produce is valid - your job is purely to make the example varied and realistic: vary length and structure, exercise different alternatives, and prefer values a human would plausibly write. Reply with the example only - no quotes, labels, or commentary."

let private userPrompt =
    "Produce one realistic, valid example for the grammar. Make it meaningfully different from an obvious or minimal example."

// ---------------------------------------------------------------------------
// App
// ---------------------------------------------------------------------------

[<EntryPoint>]
let main args =
    let builder = WebApplication.CreateBuilder(args)

    let allowedOrigins = Environment.GetEnvironmentVariable "ALLOWED_ORIGINS"

    builder.Services.AddCors(fun options ->
        options.AddDefaultPolicy(fun policy ->
            (if String.IsNullOrWhiteSpace allowedOrigins then
                 policy.AllowAnyOrigin() |> ignore
             else
                 let origins =
                     allowedOrigins.Split(',')
                     |> Array.map (fun s -> s.Trim())
                     |> Array.filter (fun s -> s <> "")

                 policy.WithOrigins origins |> ignore)

            policy.AllowAnyHeader().AllowAnyMethod() |> ignore))
    |> ignore

    let app = builder.Build()
    app.UseCors() |> ignore

    let envOr (fallback: string) (name: string) =
        match Environment.GetEnvironmentVariable name with
        | null -> fallback
        | s when s.Trim() = "" -> fallback
        | s -> s

    let cfg: Fireworks.Config =
        { ApiKey = envOr "" "FIREWORKS_API_KEY"
          Model = envOr "accounts/fireworks/models/llama-v3p1-8b-instruct" "FIREWORKS_MODEL"
          BaseUrl = envOr "https://api.fireworks.ai/inference/v1" "FIREWORKS_BASE_URL" }

    let http = new HttpClient()
    http.Timeout <- TimeSpan.FromSeconds 90.0

    app.MapGet("/healthz", Func<IResult>(fun () -> Results.Text("ok", "text/plain"))) |> ignore

    app.MapPost(
        "/api/generate",
        Func<GenerateRequest, System.Threading.Tasks.Task<IResult>>(fun req ->
            task {
                if isNull (box req) || String.IsNullOrWhiteSpace req.Grammar then
                    return
                        Results.Json(
                            { Error = "A grammar is required."
                              ParseError = Unchecked.defaultof<ParseErrorDto>
                              Diagnostics = Array.empty
                              Gbnf = null },
                            statusCode = 400
                        )
                else
                    match Parser.parse req.Grammar with
                    | Result.Error pe ->
                        return
                            Results.Json(
                                { Error = "The grammar could not be parsed."
                                  ParseError =
                                    { Line = pe.Line
                                      Column = pe.Column
                                      Message = pe.Message }
                                  Diagnostics = Array.empty
                                  Gbnf = null },
                                statusCode = 400
                            )
                    | Result.Ok grammar ->
                        let report = Analysis.analyze grammar
                        let diags = diagsOf report

                        if report.Fatal then
                            return
                                Results.Json(
                                    { Error = "The grammar has fatal errors, so no samples can be generated."
                                      ParseError = Unchecked.defaultof<ParseErrorDto>
                                      Diagnostics = diags
                                      Gbnf = null },
                                    statusCode = 422
                                )
                        else
                            let gbnf = Gbnf.compile grammar
                            let model = if String.IsNullOrWhiteSpace req.Model then cfg.Model else req.Model
                            let runCfg = { cfg with Model = model }
                            let count = req.Count |> intOr 10 |> max 1 |> min 50
                            let temperature = req.Temperature |> floatOr 0.7
                            let maxTokens = req.MaxTokens |> intOr 512 |> max 1 |> min 4096

                            let! result =
                                Fireworks.generateMany http runCfg gbnf systemPrompt userPrompt temperature maxTokens count
                                |> Async.StartAsTask

                            match result with
                            | Ok samples ->
                                return
                                    Results.Json(
                                        { Samples = List.toArray samples
                                          Gbnf = gbnf
                                          Diagnostics = diags
                                          Facts = factsOf grammar report
                                          Model = model }
                                    )
                            | Error err ->
                                let status =
                                    match err with
                                    | Fireworks.Configuration _ -> 500
                                    | _ -> 502

                                return
                                    Results.Json(
                                        { Error = Fireworks.describe err
                                          ParseError = Unchecked.defaultof<ParseErrorDto>
                                          Diagnostics = diags
                                          Gbnf = gbnf },
                                        statusCode = status
                                    )
            })
    )
    |> ignore

    let port = envOr "8080" "PORT"
    app.Run(sprintf "http://0.0.0.0:%s" port)
    0
