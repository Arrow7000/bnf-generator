namespace BnfGen

/// Bounded exhaustive enumeration of derivations.
///
/// Every derivation produced has `nodeSize <= maxSize`. Because each rule
/// expansion (and every repetition of a `*`/`+`) consumes at least one unit of
/// the size budget, the budget strictly decreases down every path. That makes
/// enumeration provably terminating even for left-recursive or epsilon-looping
/// grammars: there is simply no way to build an unbounded tree within a finite
/// node budget.
///
/// Enumeration is lazy (`seq`), so callers can take a prefix, count up to a cap,
/// or stream results without materializing everything.
module Enumerate =

    open Ast

    /// Round-robin merge of several lazy sequences. Used so that alternatives
    /// are explored fairly: small derivations from every branch appear early,
    /// rather than exhausting one branch before starting the next. This keeps
    /// coverage correct even when a later scan cap truncates the stream.
    let private interleave (sources: seq<'T> list) : seq<'T> =
        match sources with
        | [] -> Seq.empty
        | [ one ] -> one
        | _ ->
            seq {
                let enumerators = sources |> List.map (fun s -> s.GetEnumerator()) |> List.toArray
                let alive = Array.create enumerators.Length true
                let mutable remaining = enumerators.Length

                while remaining > 0 do
                    for i in 0 .. enumerators.Length - 1 do
                        if alive.[i] then
                            if enumerators.[i].MoveNext() then
                                yield enumerators.[i].Current
                            else
                                alive.[i] <- false
                                remaining <- remaining - 1
            }

    /// All derivations of `e` whose node size is at most `fuel`, paired with
    /// their size. `fuel` is the remaining size budget for this subtree.
    let rec private enumExpr (g: Grammar) (fuel: int) (e: Expr) : seq<Node * int> =
        if fuel <= 0 then
            Seq.empty
        else
            match e with
            | Terminal s -> Seq.singleton (NTerm s, 1)
            | CharClass cs -> Seq.singleton (NClass cs, 1)
            | Epsilon -> Seq.singleton (NEps, 1)
            | NonTerminal name ->
                match Map.tryFind name g.RuleMap with
                | None -> Seq.empty
                | Some body ->
                    // The rule node itself costs one unit, so the body must fit
                    // in `fuel - 1`. This is the strict decrease that bounds
                    // recursion.
                    enumExpr g (fuel - 1) body
                    |> Seq.map (fun (child, sz) -> NRule(name, child), sz + 1)
            | Alt alts -> alts |> List.map (enumExpr g fuel) |> interleave
            | Seq items ->
                enumSeqItems g (fuel - 1) items
                |> Seq.map (fun (children, sz) -> NSeq children, sz + 1)
            | Opt inner ->
                seq {
                    yield (NEps, 1)
                    yield! enumExpr g fuel inner
                }
            | Star inner ->
                enumMany g (fuel - 1) inner
                |> Seq.map (fun (children, sz) -> NSeq children, sz + 1)
            | Plus inner ->
                enumMany g (fuel - 1) inner
                |> Seq.filter (fun (children, _) -> not (List.isEmpty children))
                |> Seq.map (fun (children, sz) -> NSeq children, sz + 1)

    /// Enumerate the concatenation of a list of items, distributing the size
    /// budget across them. Returns each combination of child nodes with the
    /// total size consumed.
    and private enumSeqItems (g: Grammar) (fuel: int) (items: Expr list) : seq<Node list * int> =
        if fuel < 0 then
            Seq.empty
        else
            match items with
            | [] -> Seq.singleton ([], 0)
            | x :: rest ->
                seq {
                    for (nx, sx) in enumExpr g fuel x do
                        for (nrest, srest) in enumSeqItems g (fuel - sx) rest do
                            yield (nx :: nrest, sx + srest)
                }

    /// Enumerate zero-or-more copies of `inner` whose combined size is at most
    /// `fuel`. Each copy costs at least one unit, so the number of copies is
    /// bounded by `fuel` and recursion terminates.
    and private enumMany (g: Grammar) (fuel: int) (inner: Expr) : seq<Node list * int> =
        seq {
            yield ([], 0)

            for (n, s) in enumExpr g fuel inner do
                for (ns, ss) in enumMany g (fuel - s) inner do
                    yield (n :: ns, s + ss)
        }

    /// All derivations of the grammar's start symbol with node size at most
    /// `maxSize`, smallest-first is *not* guaranteed (callers sort if needed).
    let enumerate (g: Grammar) (maxSize: int) : seq<Node> =
        enumExpr g maxSize (NonTerminal g.Start) |> Seq.map fst

    /// Count derivations up to `maxSize`, scanning at most `cap` of them. Used
    /// for diagnostics and property tests (count is monotonic in `maxSize`).
    let countUpTo (g: Grammar) (maxSize: int) (cap: int) : int =
        enumerate g maxSize |> Seq.truncate cap |> Seq.length
