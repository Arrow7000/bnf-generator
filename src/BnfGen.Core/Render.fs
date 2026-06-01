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

    /// A piece of a rendered sample: either a literal fragment, or a single
    /// representative character chosen for a character class (which the UI can
    /// highlight to make clear it is one member, not the whole class).
    type Segment =
        | TextSeg of string
        | ClassSeg of representative: string * classLabel: string

    /// A source-like label for a character class, e.g. "[a-z]" or "[^0-9]".
    let describe (cs: CharSet) : string =
        let rangeStr (r: CharRange) =
            if r.Lo = r.Hi then string r.Lo else sprintf "%c-%c" r.Lo r.Hi

        let body = cs.Ranges |> List.map rangeStr |> String.concat ""
        sprintf "[%s%s]" (if cs.Negated then "^" else "") body

    /// Merge adjacent text segments so the output is tidy.
    let private coalesce (segments: Segment list) : Segment list =
        let folder acc seg =
            match acc, seg with
            | TextSeg a :: rest, TextSeg b -> TextSeg(a + b) :: rest
            | _ -> seg :: acc

        segments |> List.fold folder [] |> List.rev

    /// Render a derivation node into structured segments, keeping character
    /// classes distinguishable from literal text.
    let renderSegments (opts: Options) (node: Node) : Segment list =
        let sep = if opts.Separator = "" then [] else [ TextSeg opts.Separator ]

        let rec go (n: Node) : Segment list =
            match n with
            | NTerm "" -> []
            | NTerm s -> [ TextSeg s ]
            | NClass cs -> [ ClassSeg(string (opts.Representative cs), describe cs) ]
            | NEps -> []
            | NRule (_, child) -> go child
            | NSeq children ->
                children
                |> List.map go
                |> List.filter (fun segs -> not (List.isEmpty segs))
                |> List.mapi (fun i segs -> if i = 0 then segs else sep @ segs)
                |> List.concat

        coalesce (go node)

    /// The set of rule names used in a derivation (for coverage reporting).
    let rec rulesUsed (node: Node) : Set<string> =
        match node with
        | NRule (name, child) -> Set.add name (rulesUsed child)
        | NSeq children -> children |> List.fold (fun acc c -> Set.union acc (rulesUsed c)) Set.empty
        | NTerm _
        | NClass _
        | NEps -> Set.empty
