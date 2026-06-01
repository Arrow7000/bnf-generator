namespace BnfGen

/// Static analysis of a grammar performed before enumeration:
///   * productivity   - can a nonterminal derive a finite all-terminal string?
///   * nullability    - can a nonterminal derive the empty string?
///   * reachability   - which nonterminals are reachable from the start symbol?
///   * undefined refs - references to rules that are not defined
///   * minimum cost   - smallest derivation size per nonterminal
///   * left recursion - informational, the enumerator handles it regardless
module Analysis =

    open Ast

    /// "Infinity" sentinel for the minimum-cost fixpoint. Divided down from
    /// Int32.MaxValue so additions can't overflow.
    let private inf = System.Int32.MaxValue / 4

    /// Nonterminals directly referenced anywhere in an expression.
    let rec refsOf (e: Expr) : Set<string> =
        match e with
        | NonTerminal n -> Set.singleton n
        | Terminal _
        | CharClass _
        | Epsilon -> Set.empty
        | Opt x
        | Star x
        | Plus x -> refsOf x
        | Seq xs
        | Alt xs -> xs |> List.fold (fun acc x -> Set.union acc (refsOf x)) Set.empty

    /// Least fixed point: the set of nonterminals that can derive some finite,
    /// all-terminal string.
    let productiveSet (g: Grammar) : Set<string> =
        let rec prod (set: Set<string>) (e: Expr) : bool =
            match e with
            | Terminal _ -> true
            | CharClass cs -> not (CharSet.isEmpty cs)
            | Epsilon -> true
            | NonTerminal n -> Set.contains n set
            | Seq xs -> xs |> List.forall (prod set)
            | Alt xs -> xs |> List.exists (prod set)
            | Opt _ -> true
            | Star _ -> true
            | Plus x -> prod set x

        let mutable current = Set.empty
        let mutable changed = true

        while changed do
            changed <- false

            for r in g.Rules do
                if not (Set.contains r.Name current) && prod current r.Body then
                    current <- Set.add r.Name current
                    changed <- true

        current

    /// Least fixed point: the set of nonterminals that can derive the empty
    /// string.
    let nullableSet (g: Grammar) : Set<string> =
        let rec nullable (set: Set<string>) (e: Expr) : bool =
            match e with
            | Terminal s -> s = ""
            | CharClass _ -> false
            | Epsilon -> true
            | NonTerminal n -> Set.contains n set
            | Seq xs -> xs |> List.forall (nullable set)
            | Alt xs -> xs |> List.exists (nullable set)
            | Opt _ -> true
            | Star _ -> true
            | Plus x -> nullable set x

        let mutable current = Set.empty
        let mutable changed = true

        while changed do
            changed <- false

            for r in g.Rules do
                if not (Set.contains r.Name current) && nullable current r.Body then
                    current <- Set.add r.Name current
                    changed <- true

        current

    /// Nonterminals reachable from the start symbol.
    let reachableSet (g: Grammar) : Set<string> =
        let rec walk (seen: Set<string>) (frontier: string list) =
            match frontier with
            | [] -> seen
            | n :: rest when Set.contains n seen -> walk seen rest
            | n :: rest ->
                let seen = Set.add n seen

                let next =
                    match Map.tryFind n g.RuleMap with
                    | Some body -> refsOf body |> Set.toList
                    | None -> []

                walk seen (next @ rest)

        walk Set.empty [ g.Start ]

    /// Minimum derivation size (node count) for each nonterminal. Non-productive
    /// nonterminals are absent from the result map.
    let minCostMap (g: Grammar) : Map<string, int> =
        let clampAdd a b = if a >= inf || b >= inf then inf else a + b

        let rec cost (m: Map<string, int>) (e: Expr) : int =
            match e with
            | Terminal _ -> 1
            | CharClass _ -> 1
            | Epsilon -> 1
            | NonTerminal n ->
                match Map.tryFind n m with
                | Some v -> clampAdd 1 v
                | None -> inf
            | Seq xs -> xs |> List.fold (fun acc x -> clampAdd acc (cost m x)) 1
            | Alt xs -> xs |> List.map (cost m) |> List.min
            | Opt _ -> 1
            | Star _ -> 1
            | Plus x -> clampAdd 1 (cost m x)

        let mutable m =
            g.Rules |> List.map (fun r -> r.Name, inf) |> Map.ofList

        let mutable changed = true

        while changed do
            changed <- false

            for r in g.Rules do
                let c = cost m r.Body

                if c < m.[r.Name] then
                    m <- Map.add r.Name c m
                    changed <- true

        m |> Map.filter (fun _ v -> v < inf)

    /// Nonterminals that are left-recursive (can re-derive themselves as the
    /// leftmost symbol). Informational only.
    let leftRecursiveSet (g: Grammar) : Set<string> =
        let nullable = nullableSet g

        // The set of nonterminals that can appear as the leftmost symbol of an
        // expression, walking through nullable prefixes in sequences.
        let rec leftmost (e: Expr) : Set<string> =
            match e with
            | NonTerminal n -> Set.singleton n
            | Terminal _
            | CharClass _
            | Epsilon -> Set.empty
            | Opt x
            | Star x
            | Plus x -> leftmost x
            | Alt xs -> xs |> List.fold (fun acc x -> Set.union acc (leftmost x)) Set.empty
            | Seq xs ->
                let rec scan items =
                    match items with
                    | [] -> Set.empty
                    | h :: t ->
                        let here = leftmost h

                        if isNullable h then Set.union here (scan t) else here

                scan xs

        and isNullable (e: Expr) : bool =
            match e with
            | Terminal s -> s = ""
            | CharClass _ -> false
            | Epsilon -> true
            | NonTerminal n -> Set.contains n nullable
            | Opt _
            | Star _ -> true
            | Plus x -> isNullable x
            | Seq xs -> xs |> List.forall isNullable
            | Alt xs -> xs |> List.exists isNullable

        // Direct leftmost edges for each rule.
        let edges =
            g.Rules
            |> List.map (fun r -> r.Name, leftmost r.Body)
            |> Map.ofList

        // A is left-recursive if A is in the transitive closure of its own
        // leftmost edges.
        let reaches (start: string) : Set<string> =
            let rec walk seen frontier =
                match frontier with
                | [] -> seen
                | n :: rest when Set.contains n seen -> walk seen rest
                | n :: rest ->
                    let seen = Set.add n seen

                    let next =
                        match Map.tryFind n edges with
                        | Some s -> Set.toList s
                        | None -> []

                    walk seen (next @ rest)

            let direct =
                match Map.tryFind start edges with
                | Some s -> Set.toList s
                | None -> []

            walk Set.empty direct

        g.Rules
        |> List.filter (fun r -> Set.contains r.Name (reaches r.Name))
        |> List.map (fun r -> r.Name)
        |> Set.ofList

    /// The aggregate result of analyzing a grammar.
    type Report =
        { Diagnostics: Diagnostic list
          Productive: Set<string>
          Nullable: Set<string>
          Reachable: Set<string>
          MinCost: Map<string, int>
          LeftRecursive: Set<string>
          /// True when an Error-level diagnostic makes enumeration impossible.
          Fatal: bool }

    let analyze (g: Grammar) : Report =
        let productive = productiveSet g
        let nullable = nullableSet g
        let reachable = reachableSet g
        let minCost = minCostMap g
        let leftRec = leftRecursiveSet g

        let definedNames = g.Rules |> List.map (fun r -> r.Name) |> Set.ofList

        let diagnostics = System.Collections.Generic.List<Diagnostic>()

        // Duplicate rule definitions.
        g.Rules
        |> List.countBy (fun r -> r.Name)
        |> List.filter (fun (_, n) -> n > 1)
        |> List.iter (fun (name, n) ->
            diagnostics.Add
                { Severity = Warning
                  Message = sprintf "Rule '%s' is defined %d times; the last definition wins." name n })

        // Undefined references (fatal).
        let undefined =
            g.Rules
            |> List.collect (fun r -> refsOf r.Body |> Set.toList |> List.map (fun ref -> r.Name, ref))
            |> List.filter (fun (_, ref) -> not (Set.contains ref definedNames))
            |> List.distinct

        for (fromRule, ref) in undefined do
            diagnostics.Add
                { Severity = Error
                  Message = sprintf "Rule '%s' references undefined nonterminal '%s'." fromRule ref }

        // Start symbol non-productive (fatal): only infinite strings, or nothing.
        let startProductive = Set.contains g.Start productive

        if not startProductive then
            diagnostics.Add
                { Severity = Error
                  Message =
                    sprintf
                        "Start symbol '%s' is non-productive: it cannot derive any finite string (the grammar requires infinitely long strings or is empty)."
                        g.Start }

        // Reachable but non-productive sub-rules (warning).
        for name in reachable do
            if name <> g.Start && Set.contains name definedNames && not (Set.contains name productive) then
                diagnostics.Add
                    { Severity = Warning
                      Message =
                        sprintf "Rule '%s' is reachable but non-productive (it can never complete a finite string)." name }

        // Unreachable rules (warning).
        for r in g.Rules do
            if not (Set.contains r.Name reachable) then
                diagnostics.Add
                    { Severity = Warning
                      Message = sprintf "Rule '%s' is unreachable from the start symbol '%s'." r.Name g.Start }

        // Left recursion (info).
        for name in leftRec do
            diagnostics.Add
                { Severity = Info
                  Message = sprintf "Rule '%s' is left-recursive. Enumeration is bounded by size and stays safe." name }

        let diags = List.ofSeq diagnostics
        let fatal = diags |> List.exists (fun d -> d.Severity = Error)

        { Diagnostics = diags
          Productive = productive
          Nullable = nullable
          Reachable = reachable
          MinCost = minCost
          LeftRecursive = leftRec
          Fatal = fatal }
