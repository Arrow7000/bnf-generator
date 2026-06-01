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
    let targets (g: Grammar) (useful: Set<string>) : Set<string> =
        let rec branchTokensOf (rule: string) (path: int list) (e: Expr) : string list =
            match e with
            | Alt bs ->
                let here = bs |> List.mapi (fun i _ -> branchToken rule path i)
                let nested = bs |> List.mapi (fun i b -> branchTokensOf rule (i :: path) b) |> List.concat
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
