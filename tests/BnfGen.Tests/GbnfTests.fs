module GbnfTests

open System.Text.RegularExpressions
open Xunit
open BnfGen

let private parse src =
    match Parser.parse src with
    | Result.Ok g -> g
    | Result.Error e -> failwithf "parse failed at %d:%d %s" e.Line e.Column e.Message

let private compile src = Gbnf.compile (parse src)

// ---------------------------------------------------------------------------
// Golden GBNF output (verifies precedence, grouping, naming and escaping)
// ---------------------------------------------------------------------------

[<Fact>]
let ``sequence renders as juxtaposition`` () =
    Assert.Equal("root ::= \"a\" \"b\"", compile " S ::= \"a\" \"b\" ")

[<Fact>]
let ``postfix operators bind to the preceding atom`` () =
    Assert.Equal("root ::= \"a\" \"b\"? \"c\"*", compile " s ::= \"a\" \"b\"? \"c\"* ")

[<Fact>]
let ``alternation inside a sequence is grouped`` () =
    Assert.Equal(
        "root ::= (\"hello\" | \"hi\") \" \" (\"world\" | \"there\")",
        compile " greeting ::= (\"hello\" | \"hi\") \" \" (\"world\" | \"there\") "
    )

[<Fact>]
let ``top-level alternation of sequences needs no grouping`` () =
    Assert.Equal("root ::= \"a\" \"b\" | \"c\"", compile " S ::= \"a\" \"b\" | \"c\" ")

[<Fact>]
let ``postfix over a group keeps the parentheses`` () =
    Assert.Equal("root ::= (\"a\" | \"b\")*", compile " S ::= (\"a\" | \"b\")* ")

[<Fact>]
let ``character classes including negation pass through`` () =
    Assert.Equal("root ::= [0-9]", compile " D ::= [0-9] ")
    Assert.Equal("root ::= [^0-9]", compile " x ::= [^0-9] ")

[<Fact>]
let ``start rule becomes root and references use sanitized names`` () =
    let out = compile " s ::= a b\n a ::= \"x\"\n b ::= \"y\" "
    Assert.Equal("root ::= a b\na ::= \"x\"\nb ::= \"y\"", out)

[<Fact>]
let ``camelCase is preserved and underscores become dashes`` () =
    let out = compile " start_rule ::= other_one\n other_one ::= \"x\" "
    Assert.Contains("root ::= other-one", out)
    Assert.Contains("other-one ::= \"x\"", out)

[<Fact>]
let ``terminals escape quotes and backslashes`` () =
    // A single-quoted literal may contain a double quote.
    Assert.Equal("root ::= \"a\\\"b\"", compile " S ::= 'a\"b' ")
    Assert.Equal("root ::= \"a\\\\b\"", compile " S ::= 'a\\b' ")

[<Fact>]
let ``empty terminal renders as an empty literal`` () =
    Assert.Equal("root ::= \"\" | \"a\"", compile " S ::= \"\" | \"a\" ")

// ---------------------------------------------------------------------------
// Whole-grammar well-formedness over every built-in preset
// ---------------------------------------------------------------------------

[<Fact>]
let ``every viable preset compiles to a well-formed GBNF document`` () =
    let lineRe = Regex(@"^[A-Za-z][A-Za-z0-9-]* ::= ")

    for (name, src) in Presets.all do
        match Parser.parse src with
        | Result.Error _ -> () // parse failures are covered elsewhere
        | Result.Ok g ->
            let report = Analysis.analyze g

            if not report.Fatal then
                let gbnf = Gbnf.compile g
                Assert.StartsWith("root ::= ", gbnf)

                let lines = gbnf.Split('\n')

                let uniqueNames =
                    g.Rules |> List.map (fun r -> r.Name) |> List.distinct |> List.length

                Assert.Equal(uniqueNames, lines.Length)

                for line in lines do
                    Assert.True(lineRe.IsMatch line, sprintf "preset '%s' produced a malformed line: %s" name line)

                let roots =
                    lines |> Array.filter (fun l -> l.StartsWith "root ::= ") |> Array.length

                Assert.Equal(1, roots)

                // Rule names are unique (left-hand side of each line).
                let names = lines |> Array.map (fun l -> l.Split([| " ::= " |], System.StringSplitOptions.None).[0])
                Assert.Equal(names.Length, (names |> Array.distinct |> Array.length))
