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

    /// Is `e` productive given the set of productive nonterminals so far?
    let rec exprProductive (productive: Set<string>) (e: Expr) : bool =
        match e with
        | Terminal _ -> true
        | CharClass cs -> not (CharSet.isEmpty cs)
        | Epsilon -> true
        | NonTerminal n -> Set.contains n productive
        | Seq xs -> xs |> List.forall (exprProductive productive)
        | Alt xs -> xs |> List.exists (exprProductive productive)
        | Opt _ -> true
        | Star _ -> true
        | Plus x -> exprProductive productive x

    /// Least fixed point: the set of nonterminals that can derive some finite,
    /// all-terminal string.
    let productiveSet (g: Grammar) : Set<string> =
        let mutable current = Set.empty
        let mutable changed = true

        while changed do
            changed <- false

            for r in g.Rules do
                if not (Set.contains r.Name current) && exprProductive current r.Body then
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

    /// Can `e` derive a terminal string of length >= 1, given the set of
    /// nonterminals known to be able to do so? (Requires the surrounding
    /// derivation to be able to complete, hence the productivity guards.)
    let rec canBeNonEmptyExpr (ne: Set<string>) (productive: Set<string>) (e: Expr) : bool =
        match e with
        | Terminal s -> s.Length >= 1
        | CharClass cs -> not (CharSet.isEmpty cs)
        | Epsilon -> false
        | NonTerminal n -> Set.contains n ne
        | Seq xs ->
            List.forall (exprProductive productive) xs
            && List.exists (canBeNonEmptyExpr ne productive) xs
        | Alt xs -> xs |> List.exists (fun x -> exprProductive productive x && canBeNonEmptyExpr ne productive x)
        | Opt x
        | Star x
        | Plus x -> canBeNonEmptyExpr ne productive x

    /// Least fixed point: nonterminals that can derive at least one non-empty
    /// string.
    let canBeNonEmptySet (g: Grammar) (productive: Set<string>) : Set<string> =
        let mutable current = Set.empty
        let mutable changed = true

        while changed do
            changed <- false

            for r in g.Rules do
                if not (Set.contains r.Name current) && canBeNonEmptyExpr current productive r.Body then
                    current <- Set.add r.Name current
                    changed <- true

        current

    /// Classify a grammar's language as Empty, Finite, or Infinite. This is an
    /// exact decision, not an over-approximation: the language is Infinite iff
    /// there is a reachable, productive pumping site -- either a `*`/`+` over a
    /// non-empty-capable expression, or a "growth cycle" among nonterminals (a
    /// recursive cycle that emits at least one terminal each time round).
    let classifyLanguage
        (g: Grammar)
        (productive: Set<string>)
        (reachable: Set<string>)
        (canBeNonEmpty: Set<string>)
        : LanguageKind =
        if not (Set.contains g.Start productive) then
            Empty
        else
            let useful = Set.intersect productive reachable

            // (a) A repetition over something that can be non-empty, sitting in
            // a context that can complete.
            let hasPumpingStarPlus () =
                let rec scan (e: Expr) : bool =
                    match e with
                    | Star x
                    | Plus x -> canBeNonEmptyExpr canBeNonEmpty productive x || scan x
                    | Opt x -> scan x
                    | Seq xs -> List.forall (exprProductive productive) xs && List.exists scan xs
                    | Alt xs -> xs |> List.exists (fun x -> exprProductive productive x && scan x)
                    | NonTerminal _
                    | Terminal _
                    | CharClass _
                    | Epsilon -> false

                g.Rules
                |> List.exists (fun r -> Set.contains r.Name useful && scan r.Body)

            // (b) Occurrences of useful nonterminals in a body, each flagged
            // with whether reaching it can be accompanied by >= 1 terminal.
            let occurrences (body: Expr) : (string * bool) list =
                let rec collect (e: Expr) (ctxEmit: bool) : (string * bool) seq =
                    match e with
                    | NonTerminal b -> Seq.singleton (b, ctxEmit)
                    | Terminal _
                    | CharClass _
                    | Epsilon -> Seq.empty
                    | Opt x -> collect x ctxEmit
                    | Star x
                    | Plus x -> collect x (ctxEmit || canBeNonEmptyExpr canBeNonEmpty productive x)
                    | Alt xs ->
                        xs
                        |> Seq.ofList
                        |> Seq.collect (fun b -> if exprProductive productive b then collect b ctxEmit else Seq.empty)
                    | Seq xs ->
                        if not (List.forall (exprProductive productive) xs) then
                            Seq.empty
                        else
                            xs
                            |> List.mapi (fun i xi ->
                                let siblingEmits =
                                    xs
                                    |> List.mapi (fun j xj -> j, xj)
                                    |> List.exists (fun (j, xj) ->
                                        j <> i && canBeNonEmptyExpr canBeNonEmpty productive xj)

                                collect xi (ctxEmit || siblingEmits))
                            |> Seq.concat

                collect body false
                |> Seq.filter (fun (b, _) -> Set.contains b useful)
                |> Seq.toList

            let occOf (a: string) =
                match Map.tryFind a g.RuleMap with
                | Some body -> occurrences body
                | None -> []

            let adj =
                useful
                |> Set.toList
                |> List.map (fun a -> a, occOf a |> List.map fst |> Set.ofList)
                |> Map.ofList

            let canReach (src: string) (dst: string) : bool =
                let rec bfs visited frontier =
                    match frontier with
                    | [] -> false
                    | n :: rest ->
                        if n = dst then true
                        elif Set.contains n visited then bfs visited rest
                        else
                            let next =
                                match Map.tryFind n adj with
                                | Some s -> Set.toList s
                                | None -> []

                            bfs (Set.add n visited) (next @ rest)

                bfs Set.empty [ src ]

            let hasGrowthCycle () =
                useful
                |> Set.exists (fun a -> occOf a |> List.exists (fun (b, growth) -> growth && canReach b a))

            if hasPumpingStarPlus () || hasGrowthCycle () then
                Infinite
            else
                Finite

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
          Language: LanguageKind
          /// True when an Error-level diagnostic makes enumeration impossible.
          Fatal: bool }

    let analyze (g: Grammar) : Report =
        let productive = productiveSet g
        let nullable = nullableSet g
        let reachable = reachableSet g
        let minCost = minCostMap g
        let leftRec = leftRecursiveSet g
        let canBeNonEmpty = canBeNonEmptySet g productive
        let language = classifyLanguage g productive reachable canBeNonEmpty

        let definedNames = g.Rules |> List.map (fun r -> r.Name) |> Set.ofList

        let diagnostics = System.Collections.Generic.List<Diagnostic>()

        // Duplicate rule definitions.
        g.Rules
        |> List.countBy (fun r -> r.Name)
        |> List.filter (fun (_, n) -> n > 1)
        |> List.iter (fun (name, n) ->
            diagnostics.Add
                { Severity = Warning
                  Lane = Structure
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
                  Lane = Structure
                  Message = sprintf "Rule '%s' references undefined nonterminal '%s'." fromRule ref }

        // Start symbol non-productive (fatal): only infinite strings, or nothing.
        let startProductive = Set.contains g.Start productive

        if not startProductive then
            diagnostics.Add
                { Severity = Error
                  Lane = Generation
                  Message =
                    sprintf
                        "Start symbol '%s' is non-productive: it cannot derive any finite string (the grammar requires infinitely long strings or is empty)."
                        g.Start }

        // Reachable but non-productive sub-rules (warning).
        for name in reachable do
            if name <> g.Start && Set.contains name definedNames && not (Set.contains name productive) then
                diagnostics.Add
                    { Severity = Warning
                      Lane = Generation
                      Message =
                        sprintf "Rule '%s' is reachable but non-productive (it can never complete a finite string)." name }

        // Unreachable rules (warning).
        for r in g.Rules do
            if not (Set.contains r.Name reachable) then
                diagnostics.Add
                    { Severity = Warning
                      Lane = Structure
                      Message = sprintf "Rule '%s' is unreachable from the start symbol '%s'." r.Name g.Start }

        // Left recursion (info, parsing lane).
        for name in leftRec do
            diagnostics.Add
                { Severity = Info
                  Lane = Parsing
                  Message =
                    sprintf
                        "Rule '%s' is left-recursive: fine for generation, but a top-down (LL/recursive-descent) parser would loop. Bottom-up (LR) or general (Earley/GLR) parsers handle it."
                        name }

        let diags = List.ofSeq diagnostics
        let fatal = diags |> List.exists (fun d -> d.Severity = Error)

        { Diagnostics = diags
          Productive = productive
          Nullable = nullable
          Reachable = reachable
          MinCost = minCost
          LeftRecursive = leftRec
          Language = language
          Fatal = fatal }
