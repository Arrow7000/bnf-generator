module Tests

open System.Text.RegularExpressions
open Xunit
open FsCheck
open FsCheck.FSharp
open BnfGen
open BnfGen.Ast

let private anyMatches (pattern: string) (ts: Set<string>) =
    ts |> Set.exists (fun t -> Regex.IsMatch(t, pattern))

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

let private gen src size : Pipeline.Output = Pipeline.generate src size 200

let private texts (out: Pipeline.Output) : Set<string> =
    out.Samples |> List.map (fun s -> s.Text) |> Set.ofList

let private hasError (out: Pipeline.Output) =
    out.Diagnostics |> List.exists (fun d -> d.Severity = Error)

let private messages (out: Pipeline.Output) =
    out.Diagnostics |> List.map (fun d -> d.Message) |> String.concat " | "

/// Structurally verify that `node` is a legal derivation of expression `e`.
/// Independent re-implementation used to check enumeration soundness.
let rec private validate (g: Grammar) (e: Expr) (node: Node) : bool =
    match e, node with
    | Terminal s, NTerm t -> s = t
    | CharClass cs, NClass cs2 -> cs = cs2
    | Epsilon, NEps -> true
    | NonTerminal name, NRule (nm, child) ->
        name = nm
        && (match Map.tryFind name g.RuleMap with
            | Some body -> validate g body child
            | None -> false)
    | Alt alts, _ -> alts |> List.exists (fun a -> validate g a node)
    | Seq items, NSeq children ->
        List.length items = List.length children
        && List.forall2 (validate g) items children
    | Opt _, NEps -> true
    | Opt inner, _ -> validate g inner node
    | Star inner, NSeq children -> children |> List.forall (validate g inner)
    | Plus inner, NSeq children ->
        not (List.isEmpty children) && children |> List.forall (validate g inner)
    | _ -> false

// ---------------------------------------------------------------------------
// Golden grammar tests
// ---------------------------------------------------------------------------

[<Fact>]
let ``EBNF meta-grammar parses and is viable (dogfood)`` () =
    let meta =
        """
        Grammar    ::= Rule+
        Rule       ::= Symbol "::=" Expression
        Expression ::= Sequence ("|" Sequence)*
        Sequence   ::= Term*
        Term       ::= Primary Postfix?
        Postfix    ::= "?" | "*" | "+"
        Primary    ::= String | Class | "(" Expression ")" | Symbol
        String     ::= '"' Char* '"'
        Class      ::= "[" Char+ "]"
        Symbol     ::= Letter (Letter | Digit)*
        Char       ::= [a-z]
        Letter     ::= [a-zA-Z]
        Digit      ::= [0-9]
        """

    match Parser.parse meta with
    | Result.Ok _ ->
        let out = gen meta 12
        Assert.False(out.Fatal, messages out)
    | Result.Error pe -> Assert.Fail(sprintf "Expected meta-grammar to parse, got error at %d:%d %s" pe.Line pe.Column pe.Message)

[<Fact>]
let ``arithmetic grammar explores loops and recursion`` () =
    let src =
        """
        expr   ::= term ("+" term)*
        term   ::= factor ("*" factor)*
        factor ::= [0-9] | "(" expr ")"
        """

    let out = gen src 18
    let ts = texts out
    Assert.False(out.Fatal, messages out)
    // Char-class members vary, so match shapes rather than exact digits.
    Assert.True(anyMatches @"^[0-9]$" ts, "a bare digit")
    Assert.True(anyMatches @"^[0-9]\+[0-9]$" ts, "digit + digit")
    Assert.True(anyMatches @"^[0-9]\*[0-9]$" ts, "digit * digit")
    Assert.True(anyMatches @"^\([0-9]\)$" ts, "parenthesised digit")

[<Fact>]
let ``left-recursive grammar terminates and is flagged`` () =
    let src = """ list ::= list "," "x" | "x" """
    let out = gen src 22
    let ts = texts out
    Assert.False(out.Fatal, messages out)
    Assert.Contains("x", ts)
    Assert.Contains("x,x", ts)
    Assert.Contains("x,x,x", ts)
    Assert.True(out.Diagnostics |> List.exists (fun d -> d.Severity = Info && d.Message.Contains "left-recursive"))

[<Fact>]
let ``non-productive (infinite-only) grammar is a fatal error`` () =
    let out = gen """ A ::= "x" A """ 20
    Assert.True(out.Fatal)
    Assert.True(hasError out)
    Assert.Contains("non-productive", messages out)

[<Fact>]
let ``undefined nonterminal is a fatal error`` () =
    let out = gen """ A ::= "x" B """ 20
    Assert.True(out.Fatal)
    Assert.Contains("undefined", messages out)

[<Fact>]
let ``unreachable rule is warned but not fatal`` () =
    let src =
        """
        A ::= "x"
        B ::= "y"
        """

    let out = gen src 6
    Assert.False(out.Fatal, messages out)
    Assert.Contains("unreachable", messages out)

[<Fact>]
let ``regex shorthand classes are supported`` () =
    let digits = gen """ n ::= \d \d """ 8
    Assert.True(digits.ParseError.IsNone)
    Assert.True(texts digits |> Set.forall (fun t -> Regex.IsMatch(t, @"^[0-9][0-9]$")))

    let word = gen """ w ::= \w """ 6
    Assert.True(word.ParseError.IsNone)
    Assert.True(texts word |> Set.forall (fun t -> Regex.IsMatch(t, @"^[A-Za-z0-9_]$")))

    // Negated shorthand: \D is any non-digit.
    let nond = gen """ x ::= \D """ 6
    Assert.True(nond.ParseError.IsNone)
    Assert.True(texts nond |> Set.forall (fun t -> not (Regex.IsMatch(t, @"^[0-9]$"))))

[<Fact>]
let ``character class renders a single member`` () =
    let out = gen """ D ::= [0-9] """ 6
    Assert.Equal(1, out.DistinctCount)
    let t = (List.head out.Samples).Text
    Assert.True(Regex.IsMatch(t, @"^[0-9]$"), sprintf "expected one digit, got '%s'" t)

[<Fact>]
let ``ambiguous grammar is detected`` () =
    let out = gen """ S ::= "a" | "a" """ 6
    Assert.True(out.Ambiguous)

[<Fact>]
let ``optional produces both presence and absence`` () =
    let out = gen """ S ::= "a" "b"? """ 8
    let ts = texts out
    Assert.Contains("a", ts)
    Assert.Contains("ab", ts)

[<Fact>]
let ``star produces increasing repetitions including empty`` () =
    let out = gen """ S ::= "a"* """ 8
    let ts = texts out
    Assert.Contains("", ts)
    Assert.Contains("a", ts)
    Assert.Contains("aa", ts)

// ---------------------------------------------------------------------------
// Built-in presets
// ---------------------------------------------------------------------------

[<Fact>]
let ``every preset parses`` () =
    for (name, src) in Presets.all do
        match Parser.parse src with
        | Result.Ok _ -> ()
        | Result.Error e -> Assert.Fail(sprintf "preset '%s' failed to parse: %d:%d %s" name e.Line e.Column e.Message)

[<Fact>]
let ``presets are viable except the error demo`` () =
    // Generous bound so every viable preset yields at least one sample even
    // for the chunkier grammars (some have larger minimum derivations).
    for (name, src) in Presets.all do
        let out = gen src 40
        Assert.True(out.ParseError.IsNone, sprintf "preset '%s' has a parse error" name)

        if name.Contains "error" then
            Assert.True(out.Fatal, sprintf "preset '%s' should be fatal" name)
        else
            Assert.False(out.Fatal, sprintf "preset '%s' should be viable: %s" name (messages out))

            Assert.True(
                not (List.isEmpty out.Samples),
                sprintf "preset '%s' produced no samples (minSize=%A)" name (out.Summary |> Option.bind (fun s -> s.MinSize))
            )

// ---------------------------------------------------------------------------
// Language classification (Empty / Finite / Infinite)
// ---------------------------------------------------------------------------

let private classify src =
    match Parser.parse src with
    | Result.Ok g ->
        let r = Analysis.analyze g
        r.Language
    | Result.Error e -> failwithf "parse failed: %s" e.Message

[<Fact>]
let ``finite language is classified finite`` () =
    Assert.Equal(Finite, classify """ S ::= ("a" | "b") "c"? """)

[<Fact>]
let ``star over non-empty makes the language infinite`` () =
    Assert.Equal(Infinite, classify """ S ::= "a"* """)

[<Fact>]
let ``recursion that emits a terminal makes the language infinite`` () =
    Assert.Equal(Infinite, classify """ list ::= list "," "x" | "x" """)

[<Fact>]
let ``unit cycle without growth stays finite`` () =
    // A <-> B is a cycle, but it emits no terminals, so the language is just {"x"}.
    Assert.Equal(Finite, classify "A ::= B\nB ::= A | \"x\"")

[<Fact>]
let ``star over an empty-only expression stays finite`` () =
    // The inner can only ever derive the empty string, so repetition adds nothing.
    Assert.Equal(Finite, classify "S ::= E*\nE ::= \"\"")

[<Fact>]
let ``non-productive grammar has empty language`` () =
    Assert.Equal(Empty, classify """ A ::= "x" A """)

// ---------------------------------------------------------------------------
// Coverage and saturation
// ---------------------------------------------------------------------------

[<Fact>]
let ``nested alternations are counted as branches`` () =
    let out = gen """ greeting ::= ("hello" | "hi") " " ("world" | "there") """ 12
    match out.Summary with
    | Some s ->
        Assert.Equal(4, s.BranchesTotal)
        Assert.Equal(4, s.BranchesCovered)
        Assert.True(s.FullyCovered)
    | None -> Assert.Fail "expected a summary"

[<Fact>]
let ``coverage saturates at a finite size for an infinite grammar`` () =
    let out = gen """ expr ::= term ("+" term)*
                      term ::= factor ("*" factor)*
                      factor ::= [0-9] | "(" expr ")" """ 18
    match out.Summary with
    | Some s ->
        Assert.Equal(Infinite, s.Language)
        Assert.True(s.FullyCovered)
        Assert.True(s.SaturationSize.IsSome)
        Assert.True(s.MaxLoopReps >= 1)
    | None -> Assert.Fail "expected a summary"

// ---------------------------------------------------------------------------
// Filters, minimal cover, and structured rendering
// ---------------------------------------------------------------------------

[<Fact>]
let ``max-reps filter of zero removes all loop repetitions`` () =
    let filters = { Pipeline.noFilters with MaxReps = Some 0 }
    let out = Pipeline.generateWith filters """ S ::= "a"* """ 8 1 200
    let ts = texts out
    Assert.Contains("", ts)
    Assert.DoesNotContain("a", ts)
    Assert.DoesNotContain("aa", ts)

[<Fact>]
let ``max-depth filter bounds recursion`` () =
    let filters = { Pipeline.noFilters with MaxDepth = Some 2 }
    let out = Pipeline.generateWith filters """ parens ::= "(" parens ")" | "" """ 30 1 200

    match out.Summary with
    | Some s -> Assert.True(s.MaxRecursionDepth <= 2)
    | None -> Assert.Fail "expected a summary"

[<Fact>]
let ``min display size hides smaller samples but keeps coverage`` () =
    let src = """ list ::= list "," "x" | "x" """
    let full = Pipeline.generate src 30 200
    let filtered = Pipeline.generateWith Pipeline.noFilters src 30 12 200

    // Every displayed sample respects the min size...
    Assert.True(filtered.Samples |> List.forall (fun s -> s.Size >= 12 || s.InMinimalCover))
    // ...but coverage/saturation are unchanged by the view filter.
    match full.Summary, filtered.Summary with
    | Some a, Some b ->
        Assert.Equal(a.SaturationSize, b.SaturationSize)
        Assert.Equal(a.BranchesCovered, b.BranchesCovered)
        Assert.Equal(a.BranchesTotal, b.BranchesTotal)
    | _ -> Assert.Fail "expected summaries"

[<Fact>]
let ``minimal cover is small and present among the samples`` () =
    // A single (0) exercises every rule and both factor branches.
    let out = gen """ expr ::= term ("+" term)*
                      term ::= factor ("*" factor)*
                      factor ::= [0-9] | "(" expr ")" """ 18

    match out.Summary with
    | Some s ->
        Assert.True(s.MinimalCoverSize >= 1)
        Assert.True(s.MinimalCoverSize <= out.DistinctCount)
        let coverCount = out.Samples |> List.filter (fun x -> x.InMinimalCover) |> List.length
        Assert.Equal(s.MinimalCoverSize, coverCount)
    | None -> Assert.Fail "expected a summary"

[<Fact>]
let ``char classes render as a highlightable segment`` () =
    let out = gen """ ip ::= [0-9] "." [0-9] """ 8

    let hasClassSegment =
        out.Samples
        |> List.exists (fun s ->
            s.Segments
            |> List.exists (fun seg ->
                match seg with
                | Render.ClassSeg (_, label) -> label = "[0-9]"
                | _ -> false))

    Assert.True(hasClassSegment)

[<Fact>]
let ``static saturation size matches the empirical one`` () =
    // The statically computed saturation must equal the smallest size at which
    // enumeration actually achieves full coverage.
    let empirical src =
        [ 1..40 ]
        |> List.tryFind (fun n ->
            match (gen src n).Summary with
            | Some s -> s.FullyCovered
            | None -> false)

    let check src =
        let staticSat =
            match (gen src 40).Summary with
            | Some s -> s.SaturationSize
            | None -> None

        Assert.Equal<int option>(empirical src, staticSat)

    check """ greeting ::= ("hello" | "hi") " " ("world" | "there") """
    check """ list ::= list "," "x" | "x" """
    check """ parens ::= "(" parens ")" | "" """

    check
        """ expr ::= term ("+" term)*
            term ::= factor ("*" factor)*
            factor ::= [0-9] | "(" expr ")" """

[<Fact>]
let ``static saturation is independent of the size bound`` () =
    let src = """ expr ::= term ("+" term)*
                  term ::= factor ("*" factor)*
                  factor ::= [0-9] | "(" expr ")" """

    let satAt n =
        match (gen src n).Summary with
        | Some s -> s.SaturationSize
        | None -> None

    // Same answer whether the slider is below, at, or above saturation.
    Assert.Equal<int option>(satAt 8, satAt 40)
    Assert.Equal<int option>(satAt 18, satAt 40)

[<Fact>]
let ``growth curve is non-decreasing`` () =
    let out = gen """ list ::= list "," "x" | "x" """ 20

    match out.Summary with
    | Some s ->
        let counts = s.Growth |> List.map snd
        let sorted = counts |> List.sort
        Assert.Equal<int list>(sorted, counts)
    | None -> Assert.Fail "expected a summary"

// ---------------------------------------------------------------------------
// Property-based tests over randomly generated small grammars
// ---------------------------------------------------------------------------

let private ruleNames = [ "A"; "B"; "C" ]

let private genTerminal = Gen.elements [ "a"; "b"; ""; "ab" ] |> Gen.map Terminal

let private genCharClass =
    Gen.elements
        [ { Negated = false; Ranges = [ { Lo = 'a'; Hi = 'c' } ] }
          { Negated = false; Ranges = [ { Lo = '0'; Hi = '1' } ] } ]
    |> Gen.map CharClass

let private genNonTerminal = Gen.elements ruleNames |> Gen.map NonTerminal

let rec private genExpr (size: int) : Gen<Expr> =
    if size <= 0 then
        Gen.oneof [ genTerminal; genCharClass; genNonTerminal ]
    else
        let sub = genExpr (size / 2)

        Gen.oneof
            [ genTerminal
              genCharClass
              genNonTerminal
              Gen.listOfLength 2 sub |> Gen.map Seq
              Gen.listOfLength 2 sub |> Gen.map Alt
              sub |> Gen.map Opt
              sub |> Gen.map Star
              sub |> Gen.map Plus ]

let private genGrammar : Gen<Grammar> =
    Gen.sized (fun s ->
        let size = min s 4

        Gen.listOfLength (List.length ruleNames) (genExpr size)
        |> Gen.map (fun bodies ->
            let rules = List.map2 (fun name body -> { Name = name; Body = body }) ruleNames bodies
            let map = rules |> List.fold (fun m r -> Map.add r.Name r.Body m) Map.empty
            { Rules = rules; RuleMap = map; Start = List.head ruleNames }))

let private arbGrammar = Arb.fromGen genGrammar

[<Fact>]
let ``enumeration respects the size bound and is sound`` () =
    let prop =
        Prop.forAll arbGrammar (fun g ->
            [ 0..6 ]
            |> List.forall (fun maxSize ->
                Enumerate.enumerate g maxSize
                |> Seq.truncate 300
                |> Seq.forall (fun node ->
                    nodeSize node <= maxSize && validate g (NonTerminal g.Start) node)))

    Check.QuickThrowOnFailure prop

[<Fact>]
let ``derivation count is monotonic in the size bound`` () =
    let prop =
        Prop.forAll arbGrammar (fun g ->
            [ 0..5 ]
            |> List.forall (fun n -> Enumerate.countUpTo g n 2000 <= Enumerate.countUpTo g (n + 1) 2000))

    Check.QuickThrowOnFailure prop

[<Fact>]
let ``every covered token is a declared coverage target`` () =
    // Validates that the static target paths and the dynamic co-walk paths agree
    // (no off-by-one in the path identity scheme).
    let prop =
        Prop.forAll arbGrammar (fun g ->
            let productive = Analysis.productiveSet g
            let useful = Set.intersect productive (Analysis.reachableSet g)
            let targets = Coverage.targets g useful productive

            let covered =
                Enumerate.enumerate g 7
                |> Seq.truncate 500
                |> Seq.fold (fun acc node -> Set.union acc (Coverage.analyzeDerivation g node).Tokens) Set.empty

            Set.isSubset covered targets)

    Check.QuickThrowOnFailure prop

[<Fact>]
let ``enumeration always terminates within the scan cap`` () =
    // The mere completion of this test demonstrates termination even for
    // pathological generated grammars (left recursion, epsilon loops, etc.).
    let prop =
        Prop.forAll arbGrammar (fun g ->
            let n = Enumerate.countUpTo g 8 5000
            n >= 0)

    Check.QuickThrowOnFailure prop
