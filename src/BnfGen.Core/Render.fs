namespace BnfGen

/// Render a derivation tree to a concrete string, concretizing each symbolic
/// character class to a representative character only at this final step.
module Render =

    open Ast

    /// Rendering options. `Separator` is inserted between the elements of a
    /// sequence (empty for scannerless grammars; a space for token grammars).
    type Options =
        { Separator: string
          Representative: CharSet -> char }

    let defaultOptions: Options =
        { Separator = ""
          Representative = CharSet.representative }

    /// Render a derivation node to its string form.
    let render (opts: Options) (node: Node) : string =
        let rec go (n: Node) : string =
            match n with
            | NTerm s -> s
            | NClass cs -> string (opts.Representative cs)
            | NEps -> ""
            | NRule (_, child) -> go child
            | NSeq children -> children |> List.map go |> String.concat opts.Separator

        go node

    /// The set of rule names used in a derivation (for coverage reporting).
    let rec rulesUsed (node: Node) : Set<string> =
        match node with
        | NRule (name, child) -> Set.add name (rulesUsed child)
        | NSeq children -> children |> List.fold (fun acc c -> Set.union acc (rulesUsed c)) Set.empty
        | NTerm _
        | NClass _
        | NEps -> Set.empty
