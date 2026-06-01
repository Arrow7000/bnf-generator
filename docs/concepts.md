# Concepts: exhaustiveness, parsers, and what to measure

A consultable overview of the domain this tool lives in: why "generate all valid
strings" is usually impossible, what "exhaustive" can sensibly mean, how grammar
generation relates to grammar parsing, and which metrics are worth surfacing.

---

## 1. Two different "sizes": the language vs the derivation set

A grammar defines a **language**: the (often infinite) set of strings it accepts.
It also defines a set of **derivations**: the distinct ways of *building* a string
by choosing alternatives and repetitions. These are not the same:

- One string can have many derivations -- that is exactly what *ambiguity* is.
- The language can be infinite even though every individual string is finite.

The tool enumerates **derivations**, then renders each to a string. We care about
derivations because "coverage" is about exercising the *choices* in the grammar,
not just collecting output strings.

## 2. Three states of a grammar: empty / finite / infinite

For a *specific* grammar these are **definite, decidable** facts -- not "possibly".
The subtlety is only in which *check* you implement:

- A cheap structural check ("is there a cycle in the rule graph?") is an
  *over-approximation* -> it can only claim "**possibly** infinite", because not
  every cycle pumps. Counterexample: `A ::= B` / `B ::= A | "x"` has a cycle but
  the language is just `{"x"}`.
- The **exact** check claims "**definitely** finite/infinite". The classic
  algorithm: delete useless symbols, eliminate the cycles that don't add length
  (unit/epsilon cycles), then a remaining cycle means infinite.

The three states:

| State | Meaning | Tool behaviour |
| --- | --- | --- |
| **Empty** | no finite strings exist (non-productive) | fatal error, we refuse to generate |
| **Finite** | finitely many strings; "all strings" is achievable | enumerable in full |
| **Infinite** | infinitely many finite strings | "all strings" impossible; must pick a coverage criterion |

> The tool computes the **exact** classification (`Analysis.classifyLanguage`): the
> language is Infinite iff there is a reachable, productive **pumping site** --
> either a `*`/`+` over a non-empty-capable expression, or a "growth cycle" among
> nonterminals (a recursive cycle that emits >= 1 terminal each time round).

## 3. Parsers: the families, and how each copes

"Left recursion is a problem" is really "a problem for *one family* of parsers".
The families:

| Family | Examples | Direction | Left recursion | Ambiguity |
| --- | --- | --- | --- | --- |
| **Top-down, predictive** | LL(k), recursive descent, PEG/packrat | builds the tree from the root, scanning input left-to-right | **loops forever** (recurses on the leftmost symbol without consuming input) | cannot represent it; picks one path |
| **Bottom-up** | LR(0), SLR, LALR, LR(k), shift-reduce | builds the tree from the leaves up, shifting tokens onto a stack and reducing | **handled fine**; right recursion costs stack instead | reports conflicts (shift/reduce) |
| **General CFG** | Earley, GLR, CYK | explore all possibilities (chart / dynamic programming / parse forest) | **handled fine** | **handled**; can return a parse *forest* of all derivations |

How the non-top-down ones avoid the left-recursion trap:

- **Bottom-up (LR):** it never "calls a rule" speculatively. It shifts real
  tokens onto a stack and only *reduces* `X Y Z -> A` once it has actually seen
  `X Y Z`. Because every shift consumes a token, there is no way to spin without
  progress. Left recursion just means the stack grows on the left; that is fine.
- **General (Earley/GLR/CYK):** they track *sets* of partial parses positionally
  (a chart indexed by input position). A left-recursive rule simply adds an item
  to the chart at the same position; it cannot create an infinite loop because
  there are finitely many distinct items per position. They also represent
  *every* derivation, so ambiguity is data, not a crash.

Rule of thumb: top-down parsers are easy to hand-write but fragile (no left
recursion, limited lookahead); bottom-up parsers are what tools like yacc/bison
generate; general parsers are the most permissive and are the right choice when
you do not control the grammar.

## 4. Generability vs parseability are independent axes

A grammar can sit in any cell of this matrix:

|  | parser-friendly | parser-hostile (left-recursive / ambiguous) |
| --- | --- | --- |
| **generable** | the happy case | still fully generable by us |
| **non-generable** (non-productive) | we refuse (fatal) | we refuse (fatal) |

So the tool reports diagnostics in **two lanes** (plus a structural one):

- **Generation lane:** does it block *us*? Non-productive start (fatal),
  reachable-but-non-productive sub-rules (warning).
- **Parsing lane:** would a downstream parser-author have a bad time? Left
  recursion (info), ambiguity (info/flag).
- **Structure lane:** general hygiene -- undefined references (fatal),
  unreachable rules, duplicate definitions.

### The duality (why generation and parsing fail differently)

| | Parser | Generator |
| --- | --- | --- |
| what is "progress"? | consuming input tokens | approaching terminal leaves |
| the nightmare | recursion that consumes no input | recursion that reaches no base case |
| direction-sensitive? | yes -- *left* recursion specifically | no -- *any* unproductive recursion |
| our detector | left-recursion note (Parsing lane) | non-productivity (fatal, Generation lane) |

The generator's analogue of the parser's left-recursion trap is **non-productivity**
("the only derivations are infinite"). A naive generator that always expands the
recursive alternative has the *mirror-image bug*; we avoid it by bounding the
derivation-tree size (a strictly decreasing measure) and by enumerating the base
alternative too. "There is a non-recursive base production" == "productive" ==
"generable".

### Should we flag *all* issues of *all* parser families?

No -- that would be noisy and many are parser-generator-specific (e.g. LALR
shift/reduce conflicts depend on the exact tool). The useful, family-agnostic
properties to surface are: **left recursion** (kills top-down), **ambiguity**
(forces a forest / hides bugs), and -- if we ever want it -- **LL(1)
conflicts** (FIRST/FIRST and FIRST/FOLLOW). These are informational; they never
block generation.

## 5. The exhaustiveness ladder

"Exhaustive" is a dial, not a point. From weakest/cheapest to strongest:

| Criterion | "Done" when... | Finite? | Notes |
| --- | --- | --- | --- |
| **Rule coverage** | every useful rule used >= 1 time | yes | coarsest |
| **Branch coverage** | every `|` alternative (any nesting) taken | yes | **what the tool reports today** |
| **Loop-boundary coverage** | every `*`/`+` at 0, 1, >=2 reps | yes | forces strings that *enter* loops |
| **Recursion-depth coverage** | every recursive rule at depth 0,1,2 | yes (fixed depth) | recursion analogue of loop boundaries |
| **k-context coverage** | every rule used *in the context of* every caller | yes, larger | principled "some combinations" (pairwise-style) |
| **Bounded-exhaustive (size <= N)** | every derivation with <= N nodes | finite per N, unbounded as N grows | complete *relative to a bound* |
| **Full language** | every string | **no** for looping grammars | infeasible in general |

The key fact you cannot escape: **"every combination of rules" = the full
derivation set = infinite** for any looping grammar. Only *bounded* combinations
exist. So instead of one unreachable top, offer a ladder of finite rungs, each
with a saturation point, and climb until it gets too expensive.

### "How many generations to reach exhaustivity?"

- Under **bounded-exhaustive (size <= N)**: the answer is the derivation *count*
  at that bound (`DistinctCount`); it grows without bound, so "exhaustive" always
  means "up to N".
- Under **rule/branch coverage**: there is a finite minimal number, and it
  **saturates** -- beyond some size `N*`, more derivations add no new coverage.
  The tool reports this `N*` as the saturation size.

## 6. What the tool computes and surfaces today

Implemented in `BnfGen.Core` and shown in the CLI and web UI:

- **Language classification** -- exact Empty / Finite / Infinite badge
  (`Analysis.classifyLanguage`).
- **Diagnostics in two lanes** (generation / parsing) plus structure
  (`Ast.Lane`).
- **Rule coverage** and **branch coverage** (`Coverage`, every `|` at any
  nesting), with covered/total and a **saturation size**.
- **Minimum derivation size**, **max loop repetitions**, and **max recursion
  depth** reached within the current bound.
- **Bounded-exhaustive enumeration** by derivation-tree node count, with an
  ambiguity flag and a truncation flag.

Computable next steps (no engine change needed): loop-boundary as its own
*selectable criterion* (not just a reported metric); a `count(N)` growth chart;
and, for round-trip checking, an Earley recognizer driven by the user's grammar
(general, so it sidesteps left recursion, and only assert derivation-equality
for grammars confirmed unambiguous).
