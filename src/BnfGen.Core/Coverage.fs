namespace BnfGen

/// Coverage and loop/recursion metrics over derivations.
///
/// We track two finite, saturating coverage notions:
///   * rule coverage   - was each useful rule used at all?
///   * branch coverage  - was each alternative (every `|`, at any nesting depth)
///                        taken at least once?
///
/// Both are finite and have a well-defined "done" point, unlike full
/// enumeration. Coverage tokens are plain strings ("R:rule" / "B:rule:path:i")
/// so the covered set and the target set are trivially comparable.
module Coverage =

    open Ast

    type DerivInfo =
        { Tokens: Set<string>
          /// Largest number of repetitions of any single `*`/`+` loop.
          MaxReps: int
          /// Deepest self-nesting of any single rule (>= 2 means recursion).
          MaxDepth: int }

    let private emptyInfo =
        { Tokens = Set.empty
          MaxReps = 0
          MaxDepth = 0 }

    let private ruleToken (rule: string) = "R:" + rule

    /// `path` is accumulated head-first while descending, so it is reversed here
    /// to give a stable, position-based identity for an alternation.
    let private branchToken (rule: string) (path: int list) (branch: int) =
        let p = path |> List.rev |> List.map string |> String.concat "."
        sprintf "B:%s:%s:%d" rule p branch

    /// Structural check that `node` is a legal derivation of expression `e`.
    let rec matches (g: Grammar) (e: Expr) (node: Node) : bool =
        match e, node with
        | Terminal s, NTerm t -> s = t
        | CharClass cs, NClass cs2 -> cs = cs2
        | Epsilon, NEps -> true
        | NonTerminal name, NRule (nm, child) ->
            name = nm
            && (match Map.tryFind name g.RuleMap with
                | Some body -> matches g body child
                | None -> false)
        | Alt alts, _ -> alts |> List.exists (fun a -> matches g a node)
        | Seq items, NSeq children ->
            List.length items = List.length children
            && List.forall2 (matches g) items children
        | Opt _, NEps -> true
        | Opt inner, _ -> matches g inner node
        | Star inner, NSeq children -> children |> List.forall (matches g inner)
        | Plus inner, NSeq children ->
            not (List.isEmpty children) && children |> List.forall (matches g inner)
        | _ -> false

    /// All coverage targets (rule + branch tokens) for the useful rules.
    /// Branches that can never complete a finite derivation (non-productive
    /// sub-expressions) are excluded, since they are not coverable.
    let targets (g: Grammar) (useful: Set<string>) (productive: Set<string>) : Set<string> =
        let rec branchTokensOf (rule: string) (path: int list) (e: Expr) : string list =
            if not (Analysis.exprProductive productive e) then
                []
            else
                match e with
                | Alt bs ->
                    let productiveBranches =
                        bs
                        |> List.mapi (fun i b -> i, b)
                        |> List.filter (fun (_, b) -> Analysis.exprProductive productive b)

                    let here = productiveBranches |> List.map (fun (i, _) -> branchToken rule path i)

                    let nested =
                        productiveBranches |> List.collect (fun (i, b) -> branchTokensOf rule (i :: path) b)

                    here @ nested
                | Seq xs -> xs |> List.mapi (fun i x -> branchTokensOf rule (i :: path) x) |> List.concat
                | Opt x
                | Star x
                | Plus x -> branchTokensOf rule (0 :: path) x
                | NonTerminal _
                | Terminal _
                | CharClass _
                | Epsilon -> []

        g.Rules
        |> List.filter (fun r -> Set.contains r.Name useful)
        |> List.collect (fun r -> ruleToken r.Name :: branchTokensOf r.Name [] r.Body)
        |> Set.ofList

    /// The size at which coverage saturates, computed statically (independent of
    /// any size bound): the smallest bound at which every coverable rule and
    /// branch has at least one derivation. Equals the max over all targets of
    /// the minimum derivation size that exercises that target.
    let saturationSize
        (g: Grammar)
        (productive: Set<string>)
        (reachable: Set<string>)
        (minCost: Map<string, int>)
        : int option =
        let useful = Set.intersect productive reachable |> Set.toList
        let INF = Analysis.inf
        let costExpr e = Analysis.costOfExpr minCost e

        // Min added size to expand `e` while leaving one direct occurrence of
        // nonterminal `b` as a hole, expanding everything else minimally.
        let rec routeExpr (e: Expr) (b: string) : int =
            match e with
            | NonTerminal c -> if c = b then 0 else INF
            | Terminal _
            | CharClass _
            | Epsilon -> INF
            | Opt x -> routeExpr x b
            | Star x
            | Plus x ->
                let r = routeExpr x b
                if r >= INF then INF else 1 + r
            | Alt xs -> xs |> List.map (fun x -> routeExpr x b) |> List.min
            | Seq xs ->
                let costs = xs |> List.map costExpr
                let total = costs |> List.fold (fun a c -> if a >= INF || c >= INF then INF else a + c) 0
                let mutable best = INF

                xs
                |> List.iteri (fun j xj ->
                    let rj = routeExpr xj b

                    if rj < INF && total < INF then
                        let cand = 1 + rj + (total - List.item j costs)
                        if cand < best then best <- cand)

                best

        let routeCost (a: string) (b: string) : int =
            match Map.tryFind a g.RuleMap with
            | Some body ->
                let r = routeExpr body b
                if r >= INF then INF else 1 + r
            | None -> INF

        // contextMin[n] = min size of a start-derivation hosting an n-hole (the
        // hole excludes n's own rule node). Bellman-Ford; edge weights >= 0.
        let edges =
            [ for a in useful do
                  for b in useful do
                      let w = routeCost a b
                      if w < INF then yield (a, b, w) ]

        let mutable dist =
            useful |> List.map (fun n -> n, (if n = g.Start then 0 else INF)) |> Map.ofList

        let mutable changed = true

        while changed do
            changed <- false

            for (a, b, w) in edges do
                if dist.[a] < INF && dist.[a] + w < dist.[b] then
                    dist <- Map.add b (dist.[a] + w) dist
                    changed <- true

        // For each coverable branch: cover size = context to host R + R's rule
        // node + the branch body's minimum size.
        let coverSizes =
            useful
            |> List.collect (fun r ->
                let branches =
                    match Map.tryFind r g.RuleMap with
                    | Some (Alt bs) -> bs
                    | Some body -> [ body ]
                    | None -> []

                branches
                |> List.filter (Analysis.exprProductive productive)
                |> List.map (fun branchExpr ->
                    let d = dist.[r]
                    let cb = costExpr branchExpr
                    if d >= INF || cb >= INF then INF else d + 1 + cb))

        match coverSizes with
        | [] -> None
        | _ ->
            let m = List.max coverSizes
            if m >= INF then None else Some m

    /// Count tokens of a given kind ("R:" rules, "B:" branches).
    let countKind (prefix: string) (tokens: Set<string>) : int =
        tokens |> Set.filter (fun t -> t.StartsWith prefix) |> Set.count

    /// Walk a derivation alongside the grammar, collecting coverage tokens and
    /// the maximum loop repetition and recursion depth it reaches.
    let analyzeDerivation (g: Grammar) (root: Node) : DerivInfo =
        let rec go (rule: string) (path: int list) (e: Expr) (node: Node) (depths: Map<string, int>) (acc: DerivInfo) : DerivInfo =
            match e, node with
            | NonTerminal name, NRule (_, child) ->
                let depth = (defaultArg (Map.tryFind name depths) 0) + 1

                let acc =
                    { acc with
                        Tokens = Set.add (ruleToken name) acc.Tokens
                        MaxDepth = max acc.MaxDepth depth }

                let body =
                    match Map.tryFind name g.RuleMap with
                    | Some b -> b
                    | None -> Epsilon

                // A new rule body starts a fresh path namespace.
                go name [] body child (Map.add name depth depths) acc
            | Alt bs, _ ->
                match bs |> List.tryFindIndex (fun b -> matches g b node) with
                | Some i ->
                    let acc =
                        { acc with Tokens = Set.add (branchToken rule path i) acc.Tokens }

                    go rule (i :: path) (List.item i bs) node depths acc
                | None -> acc
            | Seq items, NSeq children when List.length items = List.length children ->
                let indexed = List.mapi (fun i x -> i, x) items

                List.fold2 (fun a (i, it) ch -> go rule (i :: path) it ch depths a) acc indexed children
            | Opt _, NEps -> acc
            | Opt inner, _ -> go rule (0 :: path) inner node depths acc
            | Star inner, NSeq children
            | Plus inner, NSeq children ->
                let acc =
                    { acc with MaxReps = max acc.MaxReps (List.length children) }

                children |> List.fold (fun a ch -> go rule (0 :: path) inner ch depths a) acc
            | _ -> acc

        // The synthetic root carries a dummy rule/path; the NonTerminal case
        // immediately switches into the start rule's own namespace.
        go "" [] (NonTerminal g.Start) root Map.empty emptyInfo
