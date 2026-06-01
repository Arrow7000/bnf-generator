namespace BnfGen

/// Built-in example grammars, ordered roughly simple -> complex. These are real
/// grammars adapted to our W3C-style EBNF (faithfully, not invented): JSON from
/// json.org, the number grammar from json.org, ISO 8601, the canonical Roman
/// numeral grammar, the textbook NLP sentence grammar, a simplified regex
/// grammar, Brainfuck (esolangs.org), and EBNF describing itself. A couple of
/// tiny teaching grammars and one deliberately broken grammar are included too.
module Presets =

    /// Strip the common leading indentation (and surrounding blank lines) from a
    /// triple-quoted block, so the grammars can be written tidily in source yet
    /// display flush-left in the editor.
    let private dedent (raw: string) : string =
        let isBlank (l: string) = l.Trim() = ""

        let lines =
            raw.Replace("\r\n", "\n").Split('\n')
            |> Array.toList
            |> List.skipWhile isBlank
            |> List.rev
            |> List.skipWhile isBlank
            |> List.rev

        let indents =
            lines
            |> List.filter (fun l -> not (isBlank l))
            |> List.map (fun l -> l.Length - l.TrimStart().Length)

        let minIndent =
            match indents with
            | [] -> 0
            | _ -> List.min indents

        lines
        |> List.map (fun l -> if l.Length >= minIndent then l.Substring minIndent else l)
        |> String.concat "\n"

    let all: (string * string) list =
        [ "Greeting (finite)",
          dedent
              """
              greeting ::= ("hello" | "hi") " " ("world" | "there")
              """

          "Optional and repetition",
          dedent
              """
              s ::= "a" "b"? "c"*
              """

          "IPv4 address",
          dedent
              """
              address ::= octet "." octet "." octet "." octet
              octet   ::= [0-9] | [0-9] [0-9] | [0-9] [0-9] [0-9]
              """

          "ISO 8601 date-time",
          dedent
              """
              datetime ::= date | date "T" time
              date     ::= [0-9] [0-9] [0-9] [0-9] "-" [0-9] [0-9] "-" [0-9] [0-9]
              time     ::= [0-9] [0-9] ":" [0-9] [0-9] ":" [0-9] [0-9]
              """

          "Roman numerals",
          dedent
              """
              roman     ::= thousands hundreds tens units
              thousands ::= "" | "M" | "MM" | "MMM"
              hundreds  ::= "" | "C" | "CC" | "CCC" | "CD" | "D" | "DC" | "DCC" | "DCCC" | "CM"
              tens      ::= "" | "X" | "XX" | "XXX" | "XL" | "L" | "LX" | "LXX" | "LXXX" | "XC"
              units     ::= "" | "I" | "II" | "III" | "IV" | "V" | "VI" | "VII" | "VIII" | "IX"
              """

          "English sentences",
          dedent
              """
              sentence   ::= nounPhrase " " verbPhrase
              nounPhrase ::= determiner " " noun
              verbPhrase ::= verb " " nounPhrase
              determiner ::= "the" | "a"
              noun       ::= "dog" | "cat" | "robot" | "child"
              verb       ::= "saw" | "chased" | "liked"
              """

          "Balanced parentheses",
          dedent
              """
              parens ::= "(" parens ")" | ""
              """

          "Recursive list (left)",
          dedent
              """
              list ::= list "," "x" | "x"
              """

          "Arithmetic (infinite)",
          dedent
              """
              expr   ::= term ("+" term)*
              term   ::= factor ("*" factor)*
              factor ::= [0-9] | "(" expr ")"
              """

          "JSON number",
          dedent
              """
              number   ::= integer fraction exponent
              integer  ::= digit | onenine digits | "-" digit | "-" onenine digits
              digits   ::= digit | digit digits
              digit    ::= "0" | onenine
              onenine  ::= [1-9]
              fraction ::= "" | "." digits
              exponent ::= "" | "E" sign digits | "e" sign digits
              sign     ::= "" | "+" | "-"
              """

          "JSON value",
          dedent
              """
              value    ::= object | array | string | number | "true" | "false" | "null"
              object   ::= "{}" | "{" members "}"
              members  ::= pair | pair "," members
              pair     ::= string ":" value
              array    ::= "[]" | "[" elements "]"
              elements ::= value | value "," elements
              string   ::= '"' [a-z]+ '"'
              number   ::= [0-9]+
              """

          "S-expression (Lisp)",
          dedent
              """
              sexpr    ::= atom | list
              list     ::= "(" elements ")"
              elements ::= "" | sexpr (" " sexpr)*
              atom     ::= [a-z]+
              """

          "Brainfuck",
          dedent
              """
              program ::= op*
              op      ::= "+" | "-" | "<" | ">" | "." | "," | "[" program "]"
              """

          "Regular expression",
          dedent
              """
              regex  ::= term | term "|" regex
              term   ::= factor | factor term
              factor ::= atom | atom "*" | atom "+" | atom "?"
              atom   ::= char | "(" regex ")"
              char   ::= [a-z]
              """

          "EBNF (self-describing)",
          dedent
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

          "Non-productive (error demo)",
          dedent
              """
              a ::= "x" a
              """ ]

    /// The grammar shown on first load.
    let defaultSource =
        all
        |> List.tryFind (fun (name, _) -> name = "Arithmetic (infinite)")
        |> Option.map snd
        |> Option.defaultValue ""
