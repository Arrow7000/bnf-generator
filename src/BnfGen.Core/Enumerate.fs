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
/// Enumeration is lazy (`seq`). Because the naive lazy enumeration re-computes
/// shared sub-derivations and can be very slow for explosive grammars, a
/// `workBudget` caps the total number of derivation elements drawn across the
/// whole (recursive) computation, guaranteeing the tool stays responsive. When
/// the budget runs out the stream simply ends early; coverage and saturation are
/// computed statically and do not depend on it.
module Enumerate =

    open Ast

    /// Default cap on total enumeration steps. Comfortably more than enough for
    /// well-behaved grammars; a safety valve for explosive ones (keeps a single
    /// generation responsive, ~1-1.5s worst case).
    let defaultWorkBudget = 2000000

    /// Enumerate, decrementing the caller-owned `budget` as work is done. After
    /// consuming the sequence the caller can inspect `budget` to learn whether
    /// the search was cut short.
    let enumerateWithin (g: Grammar) (maxSize: int) (budget: int ref) : seq<Node> =
        // Decrement once per element that passes through; stop the stream when
        // the shared budget is exhausted.
        let throttle (s: seq<Node * int>) : seq<Node * int> =
            s
            |> Seq.takeWhile (fun _ ->
                if budget.Value <= 0 then
                    false
                else
                    budget.Value <- budget.Value - 1
                    true)

        let rec enumExpr (fuel: int) (e: Expr) : seq<Node * int> =
            if fuel <= 0 || budget.Value <= 0 then
                Seq.empty
            else
                let result =
                    match e with
                    | Terminal s -> Seq.singleton (NTerm s, 1)
                    | CharClass cs -> Seq.singleton (NClass cs, 1)
                    | Epsilon -> Seq.singleton (NEps, 1)
                    | NonTerminal name ->
                        match Map.tryFind name g.RuleMap with
                        | None -> Seq.empty
                        | Some body ->
                            enumExpr (fuel - 1) body
                            |> Seq.map (fun (child, sz) -> NRule(name, child), sz + 1)
                    | Alt alts -> alts |> Seq.ofList |> Seq.collect (enumExpr fuel)
                    | Seq items ->
                        enumSeqItems (fuel - 1) items
                        |> Seq.map (fun (children, sz) -> NSeq children, sz + 1)
                    | Opt inner ->
                        seq {
                            yield (NEps, 1)
                            yield! enumExpr fuel inner
                        }
                    | Star inner ->
                        enumMany (fuel - 1) inner
                        |> Seq.map (fun (children, sz) -> NSeq children, sz + 1)
                    | Plus inner ->
                        enumMany (fuel - 1) inner
                        |> Seq.filter (fun (children, _) -> not (List.isEmpty children))
                        |> Seq.map (fun (children, sz) -> NSeq children, sz + 1)

                throttle result

        and enumSeqItems (fuel: int) (items: Expr list) : seq<Node list * int> =
            if fuel < 0 || budget.Value <= 0 then
                Seq.empty
            else
                match items with
                | [] -> Seq.singleton ([], 0)
                | x :: rest ->
                    seq {
                        for (nx, sx) in enumExpr fuel x do
                            for (nrest, srest) in enumSeqItems (fuel - sx) rest do
                                yield (nx :: nrest, sx + srest)
                    }

        and enumMany (fuel: int) (inner: Expr) : seq<Node list * int> =
            seq {
                yield ([], 0)

                for (n, s) in enumExpr fuel inner do
                    for (ns, ss) in enumMany (fuel - s) inner do
                        yield (n :: ns, s + ss)
            }

        enumExpr maxSize (NonTerminal g.Start) |> Seq.map fst

    /// All derivations of the grammar's start symbol with node size at most
    /// `maxSize` (subject to the default work budget). Smallest-first is *not*
    /// guaranteed (callers sort if needed).
    let enumerate (g: Grammar) (maxSize: int) : seq<Node> =
        enumerateWithin g maxSize (ref defaultWorkBudget)

    /// Count derivations up to `maxSize`, scanning at most `cap` of them. Used
    /// for diagnostics and property tests (count is monotonic in `maxSize`).
    let countUpTo (g: Grammar) (maxSize: int) (cap: int) : int =
        enumerate g maxSize |> Seq.truncate cap |> Seq.length
