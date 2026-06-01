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

    /// A piece of a rendered sample: either a literal fragment, or a single
    /// member chosen for a character class (which the UI can highlight to make
    /// clear it is one member, not the whole class).
    type Segment =
        | TextSeg of string
        | ClassSeg of representative: string * classLabel: string

    /// A source-like label for a character class, e.g. "[a-z]" or "[^0-9]".
    let describe (cs: CharSet) : string =
        let rangeStr (r: CharRange) =
            if r.Lo = r.Hi then string r.Lo else sprintf "%c-%c" r.Lo r.Hi

        let body = cs.Ranges |> List.map rangeStr |> String.concat ""
        sprintf "[%s%s]" (if cs.Negated then "^" else "") body

    // A small linear-congruential step, kept in a positive 30-bit range so it
    // behaves identically under .NET and Fable.
    let private nextSeed (s: int) = (s * 1103515245 + 12345) &&& 0x3FFFFFFF

    /// Pick a class member deterministically from a seed. Different seeds yield
    /// different members, so classes look varied; the same seed always yields
    /// the same member, so output is stable across re-renders.
    let private memberOf (cs: CharSet) (seed: int) : char =
        let k = (abs seed)

        if not cs.Negated then
            let total = cs.Ranges |> List.sumBy (fun r -> int r.Hi - int r.Lo + 1)

            if total <= 0 then
                '?'
            else
                let mutable idx = k % total
                let mutable result = '?'
                let mutable found = false

                for r in cs.Ranges do
                    if not found then
                        let n = int r.Hi - int r.Lo + 1

                        if idx < n then
                            result <- char (int r.Lo + idx)
                            found <- true
                        else
                            idx <- idx - n

                result
        else
            let allowed =
                [ for i in 33..126 do
                      let c = char i
                      if CharSet.contains c cs then yield c ]

            match allowed with
            | [] -> '?'
            | _ -> List.item (k % List.length allowed) allowed

    /// Merge adjacent text segments so the output is tidy.
    let private coalesce (segments: Segment list) : Segment list =
        let folder acc seg =
            match acc, seg with
            | TextSeg a :: rest, TextSeg b -> TextSeg(a + b) :: rest
            | _ -> seg :: acc

        segments |> List.fold folder [] |> List.rev

    /// Render a derivation node into structured segments. `seed` drives the
    /// (deterministic) choice of character-class members; pass `hash node` for
    /// per-derivation variety that is stable across renders.
    let renderSegments (seed: int) (opts: Options) (node: Node) : Segment list =
        let mutable state = seed

        let pick cs =
            state <- nextSeed state
            memberOf cs state

        let sep = if opts.Separator = "" then [] else [ TextSeg opts.Separator ]

        let rec go (n: Node) : Segment list =
            match n with
            | NTerm "" -> []
            | NTerm s -> [ TextSeg s ]
            | NClass cs -> [ ClassSeg(string (pick cs), describe cs) ]
            | NEps -> []
            | NRule (_, child) -> go child
            | NSeq children ->
                children
                |> List.map go
                |> List.filter (fun segs -> not (List.isEmpty segs))
                |> List.mapi (fun i segs -> if i = 0 then segs else sep @ segs)
                |> List.concat

        coalesce (go node)

    /// Flatten segments back to a plain string.
    let segmentText (segments: Segment list) : string =
        segments
        |> List.map (fun s ->
            match s with
            | TextSeg t -> t
            | ClassSeg (rep, _) -> rep)
        |> String.concat ""

    /// Render a derivation node to its string form, with seeded class members.
    let render (seed: int) (opts: Options) (node: Node) : string =
        renderSegments seed opts node |> segmentText

    /// The set of rule names used in a derivation (for coverage reporting).
    let rec rulesUsed (node: Node) : Set<string> =
        match node with
        | NRule (name, child) -> Set.add name (rulesUsed child)
        | NSeq children -> children |> List.fold (fun acc c -> Set.union acc (rulesUsed c)) Set.empty
        | NTerm _
        | NClass _
        | NEps -> Set.empty
