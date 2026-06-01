module Tests

open Xunit
open FsCheck
open FsCheck.FSharp
open BnfGen
open BnfGen.Ast

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
    Assert.Contains("0", ts)
    Assert.Contains("0+0", ts)
    Assert.Contains("0*0", ts)
    Assert.Contains("(0)", ts)

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
let ``character class renders a representative character`` () =
    let out = gen """ D ::= [0-9] """ 6
    Assert.Equal<Set<string>>(Set.ofList [ "0" ], texts out)

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
let ``enumeration always terminates within the scan cap`` () =
    // The mere completion of this test demonstrates termination even for
    // pathological generated grammars (left recursion, epsilon loops, etc.).
    let prop =
        Prop.forAll arbGrammar (fun g ->
            let n = Enumerate.countUpTo g 8 5000
            n >= 0)

    Check.QuickThrowOnFailure prop
