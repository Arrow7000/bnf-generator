namespace BnfGen

/// A hand-written recursive-descent parser for W3C-style EBNF (the notation used
/// by the XML/XPath specs). Hand-written rather than FParsec-based so it compiles
/// cleanly to JavaScript via Fable.
///
/// Supported syntax:
///   Rule       ::= Symbol "::=" Expression
///   Expression ::= Sequence ("|" Sequence)*
///   Sequence   ::= Term*
///   Term       ::= Primary ("?" | "*" | "+")?
///   Primary    ::= '"' ... '"' | "'" ... "'" | "[" CharClass "]"
///                | "#x" Hex+ | "(" Expression ")" | Symbol
///   Symbol     ::= [A-Za-z_] [A-Za-z0-9_]*
/// Comments use /* ... */. The set-difference operator `-` is not supported.
module Parser =

    open Ast

    /// A structured parse failure with a human-friendly location.
    type ParseError =
        { Message: string
          Position: int
          Line: int
          Column: int }

    exception private Fail of string * int

    type private State = { Src: string; mutable Pos: int }

    let private peek (st: State) : char option =
        if st.Pos < st.Src.Length then Some st.Src.[st.Pos] else None

    let private peekAt (st: State) (offset: int) : char option =
        let i = st.Pos + offset
        if i < st.Src.Length then Some st.Src.[i] else None

    let private advance (st: State) = st.Pos <- st.Pos + 1

    let private fail (st: State) (msg: string) : 'a = raise (Fail(msg, st.Pos))

    let private isIdentStart (c: char) =
        (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'

    let private isIdentCont (c: char) =
        isIdentStart c || (c >= '0' && c <= '9')

    let private isHexDigit (c: char) =
        (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

    /// Skip whitespace and /* ... */ comments.
    let rec private skipTrivia (st: State) =
        match peek st with
        | Some c when c = ' ' || c = '\t' || c = '\r' || c = '\n' ->
            advance st
            skipTrivia st
        | Some '/' when peekAt st 1 = Some '*' ->
            advance st
            advance st

            let rec loop () =
                match peek st with
                | None -> fail st "Unterminated comment"
                | Some '*' when peekAt st 1 = Some '/' ->
                    advance st
                    advance st
                | Some _ ->
                    advance st
                    loop ()

            loop ()
            skipTrivia st
        | _ -> ()

    let private readIdentifier (st: State) : string =
        match peek st with
        | Some c when isIdentStart c ->
            let start = st.Pos
            advance st

            let rec loop () =
                match peek st with
                | Some c when isIdentCont c ->
                    advance st
                    loop ()
                | _ -> ()

            loop ()
            st.Src.Substring(start, st.Pos - start)
        | _ -> fail st "Expected an identifier"

    /// True if, looking ahead from the current position, we see `Symbol ::=`
    /// (the start of the next rule) without consuming any input.
    let private ruleStartAhead (st: State) : bool =
        let saved = st.Pos

        let result =
            try
                skipTrivia st

                match peek st with
                | Some c when isIdentStart c ->
                    readIdentifier st |> ignore
                    skipTrivia st

                    peek st = Some ':'
                    && peekAt st 1 = Some ':'
                    && peekAt st 2 = Some '='
                | _ -> false
            with Fail _ ->
                false

        st.Pos <- saved
        result

    let private readHexChar (st: State) : char =
        // Assumes the leading "#x" has already been consumed.
        let start = st.Pos

        let rec loop () =
            match peek st with
            | Some c when isHexDigit c ->
                advance st
                loop ()
            | _ -> ()

        loop ()

        if st.Pos = start then
            fail st "Expected hexadecimal digits after #x"

        let hex = st.Src.Substring(start, st.Pos - start)
        let code = System.Convert.ToInt32(hex, 16)
        char code

    /// Parse a single character inside a `[...]` class, supporting `#xNN`.
    let private readClassChar (st: State) : char =
        match peek st with
        | Some '#' when peekAt st 1 = Some 'x' ->
            advance st
            advance st
            readHexChar st
        | Some c ->
            advance st
            c
        | None -> fail st "Unterminated character class"

    let private parseCharClass (st: State) : Expr =
        // Assumes the opening '[' has already been consumed.
        let negated =
            match peek st with
            | Some '^' ->
                advance st
                true
            | _ -> false

        let ranges = System.Collections.Generic.List<CharRange>()

        let rec loop () =
            match peek st with
            | None -> fail st "Unterminated character class"
            | Some ']' -> advance st
            | Some _ ->
                let lo = readClassChar st

                match peek st with
                | Some '-' when peekAt st 1 <> Some ']' ->
                    advance st
                    let hi = readClassChar st
                    ranges.Add { Lo = lo; Hi = hi }
                | _ -> ranges.Add { Lo = lo; Hi = lo }

                loop ()

        loop ()

        if ranges.Count = 0 && not negated then
            fail st "Empty character class"

        CharClass
            { Negated = negated
              Ranges = List.ofSeq ranges }

    let private parseStringLiteral (st: State) (quote: char) : Expr =
        advance st // opening quote
        let start = st.Pos

        let rec loop () =
            match peek st with
            | None -> fail st "Unterminated string literal"
            | Some c when c = quote -> ()
            | Some _ ->
                advance st
                loop ()

        loop ()
        let text = st.Src.Substring(start, st.Pos - start)
        advance st // closing quote
        Terminal text

    let rec private parsePrimary (st: State) : Expr =
        skipTrivia st

        match peek st with
        | Some '(' ->
            advance st
            let e = parseExpression st
            skipTrivia st

            match peek st with
            | Some ')' -> advance st
            | _ -> fail st "Expected ')'"

            e
        | Some '"' -> parseStringLiteral st '"'
        | Some '\'' -> parseStringLiteral st '\''
        | Some '[' ->
            advance st
            parseCharClass st
        | Some '#' when peekAt st 1 = Some 'x' ->
            advance st
            advance st
            Terminal(string (readHexChar st))
        | Some c when isIdentStart c -> NonTerminal(readIdentifier st)
        | Some '-' -> fail st "The set-difference operator '-' is not supported"
        | Some c -> fail st (sprintf "Unexpected character '%c'" c)
        | None -> fail st "Unexpected end of input"

    and private parseTerm (st: State) : Expr =
        let primary = parsePrimary st

        // Postfix operators must immediately follow the primary (no trivia).
        let rec applyPostfix (e: Expr) =
            match peek st with
            | Some '?' ->
                advance st
                applyPostfix (Opt e)
            | Some '*' ->
                advance st
                applyPostfix (Star e)
            | Some '+' ->
                advance st
                applyPostfix (Plus e)
            | _ -> e

        applyPostfix primary

    and private startsTerm (st: State) : bool =
        match peek st with
        | Some c -> c = '"' || c = '\'' || c = '[' || c = '(' || c = '#' || isIdentStart c
        | None -> false

    and private parseSequence (st: State) : Expr =
        let terms = System.Collections.Generic.List<Expr>()
        skipTrivia st

        let rec loop () =
            if startsTerm st && not (ruleStartAhead st) then
                terms.Add(parseTerm st)
                skipTrivia st
                loop ()

        loop ()

        match terms.Count with
        | 0 -> Epsilon
        | 1 -> terms.[0]
        | _ -> Seq(List.ofSeq terms)

    and private parseExpression (st: State) : Expr =
        let alts = System.Collections.Generic.List<Expr>()
        alts.Add(parseSequence st)
        skipTrivia st

        let rec loop () =
            match peek st with
            | Some '|' ->
                advance st
                alts.Add(parseSequence st)
                skipTrivia st
                loop ()
            | _ -> ()

        loop ()

        match alts.Count with
        | 1 -> alts.[0]
        | _ -> Alt(List.ofSeq alts)

    let private parseRule (st: State) : Rule =
        skipTrivia st
        let name = readIdentifier st
        skipTrivia st

        if peek st = Some ':' && peekAt st 1 = Some ':' && peekAt st 2 = Some '=' then
            advance st
            advance st
            advance st
        else
            fail st (sprintf "Expected '::=' after rule name '%s'" name)

        let body = parseExpression st
        { Name = name; Body = body }

    let private lineCol (src: string) (pos: int) : int * int =
        let mutable line = 1
        let mutable col = 1
        let bound = min pos src.Length

        for i in 0 .. bound - 1 do
            if src.[i] = '\n' then
                line <- line + 1
                col <- 1
            else
                col <- col + 1

        line, col

    /// Parse a complete grammar. The first declared rule becomes the start symbol.
    let parse (source: string) : Result<Grammar, ParseError> =
        let st = { Src = source; Pos = 0 }

        try
            let rules = System.Collections.Generic.List<Rule>()
            skipTrivia st

            while st.Pos < st.Src.Length do
                rules.Add(parseRule st)
                skipTrivia st

            if rules.Count = 0 then
                Result.Error
                    { Message = "Grammar is empty"
                      Position = 0
                      Line = 1
                      Column = 1 }
            else
                let ruleList = List.ofSeq rules
                // On duplicate names the last definition wins; Analysis warns.
                let ruleMap =
                    ruleList
                    |> List.fold (fun m r -> Map.add r.Name r.Body m) Map.empty

                Result.Ok
                    { Rules = ruleList
                      RuleMap = ruleMap
                      Start = ruleList.[0].Name }
        with Fail (msg, pos) ->
            let line, col = lineCol source pos

            Result.Error
                { Message = msg
                  Position = pos
                  Line = line
                  Column = col }
