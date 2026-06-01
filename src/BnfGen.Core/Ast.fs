namespace BnfGen

/// Core grammar and derivation data types, shared across parsing, analysis,
/// enumeration and rendering. Kept free of any .NET-specific APIs so the whole
/// module is Fable-compatible and can run in the browser.
module Ast =

    /// An inclusive character range, e.g. `a-z` -> { Lo = 'a'; Hi = 'z' }.
    type CharRange = { Lo: char; Hi: char }

    /// A character class such as `[a-z]` or `[^0-9]`. Stays symbolic in the
    /// grammar and derivation; a representative character is only chosen at
    /// render time so a class never explodes the enumeration.
    type CharSet =
        { Negated: bool
          Ranges: CharRange list }

    /// A grammar expression (the right-hand side of a rule, recursively).
    type Expr =
        /// Literal string terminal, e.g. "while". May be empty (nullable).
        | Terminal of string
        /// Symbolic character class, e.g. [a-z].
        | CharClass of CharSet
        /// Reference to a named rule.
        | NonTerminal of string
        /// Concatenation of expressions.
        | Seq of Expr list
        /// Alternation (choice) between expressions.
        | Alt of Expr list
        /// Zero or one occurrence (postfix ?).
        | Opt of Expr
        /// Zero or more occurrences (postfix *).
        | Star of Expr
        /// One or more occurrences (postfix +).
        | Plus of Expr
        /// The empty string. Produced by an empty alternative.
        | Epsilon

    /// A single named production: `Name ::= Body`.
    type Rule = { Name: string; Body: Expr }

    /// A complete grammar: ordered rules, a lookup map, and a start symbol
    /// (defaults to the first declared rule).
    type Grammar =
        { Rules: Rule list
          RuleMap: Map<string, Expr>
          Start: string }

    /// A concrete derivation tree produced by enumeration. Character classes
    /// remain symbolic (`NClass`) and are concretized only at render time.
    type Node =
        | NTerm of string
        | NClass of CharSet
        | NRule of string * Node
        | NSeq of Node list
        | NEps

    type Severity =
        | Error
        | Warning
        | Info

    /// Which concern a diagnostic belongs to. Generability and parseability are
    /// independent axes: a grammar can be perfectly generable yet hostile to a
    /// parser (left-recursive, ambiguous), or structurally malformed.
    type Lane =
        /// Affects whether *we* can generate samples.
        | Generation
        /// Affects whether a downstream *parser* would have a good time.
        | Parsing
        /// General structural hygiene (undefined refs, unreachable rules, ...).
        | Structure

    type Diagnostic =
        { Severity: Severity
          Lane: Lane
          Message: string }

    /// Whether a grammar's language is empty (no finite strings), finite, or
    /// infinite (unboundedly many finite strings).
    type LanguageKind =
        | Empty
        | Finite
        | Infinite

    /// Number of tree nodes in a derivation. This is the measure bounded during
    /// enumeration, which is what guarantees termination even for left-recursive
    /// or epsilon-looping grammars.
    let rec nodeSize (n: Node) : int =
        match n with
        | NTerm _
        | NClass _
        | NEps -> 1
        | NRule (_, child) -> 1 + nodeSize child
        | NSeq children -> 1 + List.sumBy nodeSize children

    /// Helpers for working with character classes.
    [<RequireQualifiedAccess>]
    module CharSet =

        /// Does this class match the given character?
        let contains (c: char) (cs: CharSet) : bool =
            let inRange =
                cs.Ranges |> List.exists (fun r -> c >= r.Lo && c <= r.Hi)

            if cs.Negated then not inRange else inRange

        /// Is this (positive) class empty, i.e. matches nothing?
        let isEmpty (cs: CharSet) : bool =
            if cs.Negated then
                // A negated class is empty only if its ranges cover every char,
                // which we don't bother detecting; treat as non-empty.
                false
            else
                cs.Ranges |> List.forall (fun r -> r.Lo > r.Hi)

        /// A single representative character used when rendering a sample.
        let representative (cs: CharSet) : char =
            if not cs.Negated then
                match cs.Ranges with
                | r :: _ when r.Lo <= r.Hi -> r.Lo
                | _ -> '?'
            else
                // First printable ASCII character that the class admits.
                let mutable found = '?'
                let mutable i = 33
                let mutable stop = false

                while not stop && i <= 126 do
                    let c = char i

                    if contains c cs then
                        found <- c
                        stop <- true

                    i <- i + 1

                found
