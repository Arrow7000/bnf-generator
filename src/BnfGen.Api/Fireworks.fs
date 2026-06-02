namespace BnfGen.Api

open System
open System.Net.Http
open System.Net.Http.Headers
open System.Text
open System.Text.Json

/// A thin client for Fireworks AI's chat-completions endpoint in *grammar mode*
/// (`response_format: { type: "grammar", grammar: <GBNF> }`). The grammar mask
/// guarantees every returned string is in the language, so the model's only job
/// is to make varied, realistic choices among the allowed tokens.
module Fireworks =

    type Config =
        { ApiKey: string
          Model: string
          BaseUrl: string }

    type GenError =
        | Configuration of string
        | Upstream of status: int * body: string
        | Transport of string

    let describe (e: GenError) : string =
        let truncate (s: string) =
            if String.length s > 600 then s.Substring(0, 600) + "..." else s

        match e with
        | Configuration m -> "configuration error: " + m
        | Upstream (code, body) -> sprintf "upstream error %d: %s" code (truncate body)
        | Transport m -> "transport error: " + m

    let private endpoint (baseUrl: string) =
        baseUrl.TrimEnd('/') + "/chat/completions"

    /// Serialize the request body with a JSON writer so the GBNF grammar (full of
    /// quotes and backslashes) is escaped correctly.
    let private buildBody
        (model: string)
        (gbnf: string)
        (systemPrompt: string)
        (userPrompt: string)
        (temperature: float)
        (maxTokens: int)
        (seed: int)
        : string =
        use ms = new IO.MemoryStream()
        use w = new Utf8JsonWriter(ms)
        w.WriteStartObject()
        w.WriteString("model", model)
        w.WriteNumber("temperature", temperature)
        w.WriteNumber("max_tokens", maxTokens)
        w.WriteNumber("seed", seed)

        w.WriteStartObject("response_format")
        w.WriteString("type", "grammar")
        w.WriteString("grammar", gbnf)
        w.WriteEndObject()

        w.WriteStartArray("messages")
        w.WriteStartObject()
        w.WriteString("role", "system")
        w.WriteString("content", systemPrompt)
        w.WriteEndObject()
        w.WriteStartObject()
        w.WriteString("role", "user")
        w.WriteString("content", userPrompt)
        w.WriteEndObject()
        w.WriteEndArray()

        w.WriteEndObject()
        w.Flush()
        Encoding.UTF8.GetString(ms.ToArray())

    // Some models (e.g. gpt-oss harmony) leak control tokens like "<|return|>"
    // into the content even under grammar mode. Strip them defensively.
    let private controlToken =
        System.Text.RegularExpressions.Regex(@"<\|[a-z_]+\|>")

    let private clean (s: string) = controlToken.Replace(s, "").Trim()

    /// One completion: the cleaned text plus the finish reason ("stop" means the
    /// model ended naturally; "length" means it was truncated at the token cap -
    /// usually a runaway on a permissive grammar, which we discard).
    type Completion = { Text: string; Finish: string }

    let private parseContent (json: string) : Result<Completion, GenError> =
        try
            use doc = JsonDocument.Parse json
            let root = doc.RootElement

            match root.TryGetProperty "choices" with
            | true, choices when choices.ValueKind = JsonValueKind.Array && choices.GetArrayLength() > 0 ->
                let choice = choices.[0]
                let message = choice.GetProperty "message"

                let finish =
                    match choice.TryGetProperty "finish_reason" with
                    | true, f when f.ValueKind = JsonValueKind.String -> f.GetString()
                    | _ -> ""

                match message.TryGetProperty "content" with
                | true, c when c.ValueKind = JsonValueKind.String -> Ok { Text = clean (c.GetString()); Finish = finish }
                | _ -> Error(Upstream(200, "response had no message content"))
            | _ ->
                match root.TryGetProperty "error" with
                | true, e -> Error(Upstream(200, e.ToString()))
                | _ -> Error(Upstream(200, "unexpected response shape: " + json))
        with ex ->
            Error(Transport("could not parse response: " + ex.Message))

    /// One grammar-constrained completion. `seed` varies between calls so a batch
    /// yields varied samples.
    let generateOne
        (http: HttpClient)
        (cfg: Config)
        (gbnf: string)
        (systemPrompt: string)
        (userPrompt: string)
        (temperature: float)
        (maxTokens: int)
        (seed: int)
        : Async<Result<Completion, GenError>> =
        async {
            let body =
                buildBody cfg.Model gbnf systemPrompt userPrompt temperature maxTokens seed

            use req = new HttpRequestMessage(HttpMethod.Post, endpoint cfg.BaseUrl)
            req.Headers.Authorization <- AuthenticationHeaderValue("Bearer", cfg.ApiKey)
            req.Content <- new StringContent(body, Encoding.UTF8, "application/json")

            try
                let! resp = http.SendAsync req |> Async.AwaitTask
                let! text = resp.Content.ReadAsStringAsync() |> Async.AwaitTask

                if resp.IsSuccessStatusCode then
                    return parseContent text
                else
                    return Error(Upstream(int resp.StatusCode, text))
            with ex ->
                return Error(Transport ex.Message)
        }

    /// Fire `count` constrained completions concurrently (bounded), de-duplicate
    /// by string, and preserve order. Returns an error only if *every* call fails.
    let generateMany
        (http: HttpClient)
        (cfg: Config)
        (gbnf: string)
        (systemPrompt: string)
        (userPrompt: string)
        (temperature: float)
        (maxTokens: int)
        (count: int)
        : Async<Result<string list, GenError>> =
        async {
            if String.IsNullOrWhiteSpace cfg.ApiKey then
                return Error(Configuration "FIREWORKS_API_KEY is not set on the server")
            else
                // Over-issue a little: naturally-completed samples are the keepers,
                // and some calls get discarded (truncated runaways), so we ask for
                // a few extra to still land near `count`.
                let issued = min 50 (count + max 2 (count / 2))

                let calls =
                    [ for i in 1..issued -> generateOne http cfg gbnf systemPrompt userPrompt temperature maxTokens i ]

                let! results = Async.Parallel(calls, maxDegreeOfParallelism = 8)

                let oks =
                    results
                    |> Array.choose (function
                        | Ok c -> Some c
                        | _ -> None)
                    |> Array.toList

                if List.isEmpty oks then
                    let firstErr =
                        results
                        |> Array.tryPick (function
                            | Error e -> Some e
                            | _ -> None)

                    return Error(defaultArg firstErr (Transport "no results"))
                else
                    let dedup (items: Completion list) =
                        let seen = System.Collections.Generic.HashSet<string>()

                        items
                        |> List.map (fun c -> c.Text)
                        |> List.filter (fun t -> t <> "" && seen.Add t)

                    // Prefer completions the model ended on its own; fall back to
                    // whatever we got if a grammar never completes within budget.
                    let completed = oks |> List.filter (fun c -> c.Finish = "stop") |> dedup
                    let chosen = if List.isEmpty completed then dedup oks else completed
                    return Ok(chosen |> List.truncate count)
        }
