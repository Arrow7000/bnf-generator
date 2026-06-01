module BnfGen.Cli.Program

open System
open BnfGen

let private severityTag =
    function
    | Ast.Error -> "error"
    | Ast.Warning -> "warn "
    | Ast.Info -> "info "

let private printDiagnostics (diags: Ast.Diagnostic list) =
    if not (List.isEmpty diags) then
        eprintfn "Diagnostics:"

        for d in diags do
            eprintfn "  [%s] %s" (severityTag d.Severity) d.Message

let private usage () =
    eprintfn "Usage: bnfgen [grammar-file] [--size N] [--limit M] [--sep S]"
    eprintfn ""
    eprintfn "  grammar-file   Path to an EBNF grammar (reads stdin if omitted)."
    eprintfn "  --size N       Max derivation-tree node count (default 12)."
    eprintfn "  --limit M      Max number of samples to print (default 50)."
    eprintfn "  --sep S        Separator inserted between sequence elements (default none)."

/// Minimal argument parsing: positional file plus --flag value pairs.
let rec private parseArgs (file, size, limit) args =
    match args with
    | [] -> Ok(file, size, limit)
    | "--size" :: v :: rest ->
        match Int32.TryParse v with
        | true, n -> parseArgs (file, n, limit) rest
        | _ -> Error(sprintf "Invalid --size value: %s" v)
    | "--limit" :: v :: rest ->
        match Int32.TryParse v with
        | true, n -> parseArgs (file, size, n) rest
        | _ -> Error(sprintf "Invalid --limit value: %s" v)
    | ("--help" | "-h") :: _ -> Error "help"
    | flag :: _ when flag.StartsWith "--" -> Error(sprintf "Unknown or incomplete option: %s" flag)
    | f :: rest -> parseArgs (Some f, size, limit) rest

[<EntryPoint>]
let main argv =
    match parseArgs (None, 12, 50) (List.ofArray argv) with
    | Error "help" ->
        usage ()
        0
    | Error msg ->
        eprintfn "%s" msg
        usage ()
        2
    | Ok (file, size, limit) ->
        let source =
            match file with
            | Some path -> IO.File.ReadAllText path
            | None -> Console.In.ReadToEnd()

        let result = Pipeline.generate source size limit

        match result.ParseError with
        | Some pe ->
            eprintfn "Parse error at line %d, column %d: %s" pe.Line pe.Column pe.Message
            1
        | None ->
            printDiagnostics result.Diagnostics

            if result.Fatal then
                eprintfn "Cannot generate samples: the grammar has fatal errors."
                1
            else
                printfn "Samples (%d distinct, size <= %d):" result.DistinctCount size

                for s in result.Samples do
                    printfn "  (%2d) %s" s.Size s.Text

                if result.Truncated then
                    printfn "  ... (enumeration truncated at the scan cap)"

                if result.Ambiguous then
                    printfn "Note: grammar is ambiguous (distinct derivations produced identical strings)."

                0
