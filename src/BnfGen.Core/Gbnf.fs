namespace BnfGen

/// Compile a parsed grammar into GBNF - the extended-BNF grammar format used by
/// llama.cpp and Fireworks AI to constrain LLM decoding token-by-token. Because
/// the engine masks the model to the grammar at every step, the model literally
/// cannot emit a string outside the language; this is what makes even a tiny
/// model safe on "looks-like-JSON-but-subtly-isn't" specs.
///
/// The mapping from our AST is essentially 1:1:
///   Terminal -> "..."   CharClass -> [a-z]/[^0-9]   NonTerminal -> rule ref
///   Seq -> juxtaposition   Alt -> a | b   Opt/Star/Plus -> ?/*/+   Epsilon -> ""
///
/// Kept free of .NET-specific APIs so it stays Fable-compatible and can also run
/// in the browser (e.g. to preview the compiled grammar).
module Gbnf =

    open Ast

    // -- rule-name sanitization ------------------------------------------------
    // GBNF rule names allow ASCII letters, digits and '-'. Our source names can
    // also contain '_', and the start rule must be emitted as `root`.

    let private isAsciiLetter (c: char) =
        (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

    let private isAsciiDigit (c: char) = c >= '0' && c <= '9'

    /// Reduce a source rule name to a GBNF-legal identifier fragment.
    let private sanitizeFragment (name: string) : string =
        let mapped =
            name
            |> Seq.map (fun c ->
                if isAsciiLetter c || isAsciiDigit c then string c
                elif c = '_' || c = '-' then "-"
                else "")
            |> String.concat ""

        if mapped.Length = 0 then "rule"
        elif not (isAsciiLetter mapped.[0]) then "r-" + mapped
        else mapped

    /// Unique source rule names in declaration order (first occurrence wins).
    let private orderedNames (g: Grammar) : string list =
        g.Rules
        |> List.map (fun r -> r.Name)
        |> List.fold
            (fun (seen, acc) n -> if Set.contains n seen then (seen, acc) else (Set.add n seen, n :: acc))
            (Set.empty, [])
        |> snd
        |> List.rev

    /// Map every source rule name to a unique GBNF identifier. The start symbol
    /// becomes `root` (GBNF's mandatory entry rule); `root` is reserved so no
    /// other rule can collide with it.
    let private buildNameMap (g: Grammar) : Map<string, string> =
        let mutable used = Set.singleton "root"
        let mutable m = Map.add g.Start "root" Map.empty

        for n in orderedNames g do
            if n <> g.Start then
                let baseId =
                    let s = sanitizeFragment n
                    if s = "root" then "root-r" else s

                let rec uniquify (i: int) =
                    let candidate = if i = 0 then baseId else sprintf "%s-%d" baseId i
                    if Set.contains candidate used then uniquify (i + 1) else candidate

                let chosen = uniquify 0
                used <- Set.add chosen used
                m <- Map.add n chosen m

        m

    // -- terminal / character-class escaping ----------------------------------

    /// Escape one character for inside a GBNF "double-quoted" literal.
    let private escStringChar (c: char) : string =
        match c with
        | '"' -> "\\\""
        | '\\' -> "\\\\"
        | '\n' -> "\\n"
        | '\r' -> "\\r"
        | '\t' -> "\\t"
        | _ when int c < 0x20 || int c = 0x7F -> sprintf "\\u%04X" (int c)
        | _ -> string c

    let private quoteTerminal (s: string) : string =
        let body = s |> Seq.map escStringChar |> String.concat ""
        "\"" + body + "\""

    /// Escape one character for inside a GBNF `[...]` character class.
    let private escClassChar (c: char) : string =
        match c with
        | '\\' -> "\\\\"
        | ']' -> "\\]"
        | '[' -> "\\["
        | '^' -> "\\^"
        | '-' -> "\\-"
        | '\n' -> "\\n"
        | '\r' -> "\\r"
        | '\t' -> "\\t"
        | _ when int c < 0x20 || int c = 0x7F -> sprintf "\\u%04X" (int c)
        | _ -> string c

    let private classStr (cs: CharSet) : string =
        // A negated class with no ranges means "any character"; GBNF's `.` is the
        // natural rendering and avoids an empty `[^]`.
        if cs.Negated && List.isEmpty cs.Ranges then
            "."
        else
            let body =
                cs.Ranges
                |> List.map (fun r ->
                    if r.Lo = r.Hi then
                        escClassChar r.Lo
                    else
                        escClassChar r.Lo + "-" + escClassChar r.Hi)
                |> String.concat ""

            "[" + (if cs.Negated then "^" else "") + body + "]"

    // -- expression rendering (precedence: alt < seq < postfix < atom) --------

    let rec private atomStr (nm: Map<string, string>) (e: Expr) : string =
        match e with
        | Terminal s -> quoteTerminal s
        | Epsilon -> "\"\""
        | CharClass cs -> classStr cs
        | NonTerminal n ->
            match Map.tryFind n nm with
            | Some g -> g
            | None -> sanitizeFragment n // undefined ref (already fatal upstream)
        // Anything composite must be grouped before a postfix can bind to it.
        | _ -> "(" + altStr nm e + ")"

    and private elemStr (nm: Map<string, string>) (e: Expr) : string =
        match e with
        // Alternation binds looser than concatenation, so group it inside a seq.
        | Alt _ -> "(" + altStr nm e + ")"
        // Nested sequences flatten - concatenation is associative.
        | Seq xs -> xs |> List.map (elemStr nm) |> String.concat " "
        | Opt x -> atomStr nm x + "?"
        | Star x -> atomStr nm x + "*"
        | Plus x -> atomStr nm x + "+"
        | _ -> atomStr nm e

    and private seqStr (nm: Map<string, string>) (e: Expr) : string =
        match e with
        | Seq xs -> xs |> List.map (elemStr nm) |> String.concat " "
        | _ -> elemStr nm e

    and private altStr (nm: Map<string, string>) (e: Expr) : string =
        match e with
        | Alt xs -> xs |> List.map (seqStr nm) |> String.concat " | "
        | _ -> seqStr nm e

    /// Render a single expression to GBNF (exposed mainly for testing).
    let renderExpr (nameMap: Map<string, string>) (e: Expr) : string = altStr nameMap e

    /// Compile a whole grammar to a GBNF document. The start rule is emitted
    /// first as `root`, followed by the remaining rules in declaration order.
    let compile (g: Grammar) : string =
        let nm = buildNameMap g

        let lineFor (name: string) =
            let body = Map.tryFind name g.RuleMap |> Option.defaultValue Epsilon
            sprintf "%s ::= %s" (Map.find name nm) (altStr nm body)

        let startLine = lineFor g.Start

        let otherLines =
            orderedNames g
            |> List.filter (fun n -> n <> g.Start)
            |> List.map lineFor

        String.concat "\n" (startLine :: otherLines)
