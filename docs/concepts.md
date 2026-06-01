# Concepts: exhaustiveness, loops, and what to measure

This document is the mental model behind the tool. It explains why "generate all
valid strings" is usually impossible, what "exhaustive" can sensibly mean
instead, and which metrics are worth surfacing.

## 1. Two different "sizes": the language vs the derivation set

A grammar defines a **language**: the (often infinite) set of strings it accepts.
It also defines a set of **derivations**: the distinct ways of *building* a string
by choosing alternatives and repetitions. These are not the same thing:

- One string can have many derivations (that is exactly what *ambiguity* is).
- The language can be infinite even when each individual string is finite.

The tool enumerates **derivations**, then renders each to a string. We care about
derivations because "coverage" is about exercising the *choices* in the grammar,
not just collecting output strings.

## 2. Why "every string" is usually infinite

If any reachable, productive nonterminal can re-derive itself while contributing
at least one terminal — `A ::= "x" A | "x"`, or `list ::= list "," x | x` — then
the language is **infinite**. There is no finite set of strings that is
"complete." This is the single most important fact:

> For any grammar with a productive loop or recursion, full language enumeration
> is infinite. Full enumeration is therefore not a knob we can turn; it is simply
> off the table.

Decidable facts we *can* compute about this:

- **Productive?** Can a nonterminal derive any finite string at all? (If the start
  symbol is not productive, the grammar requires infinitely long strings — we
  reject it.) *Implemented.*
- **Finite or infinite language?** After deleting useless symbols, the language is
  infinite iff the dependency graph still has a cycle. *Computable from what we
  have; not yet surfaced.*

## 3. Notions of "exhaustive" and their feasibility

"Exhaustive" is a dial, not a point. From weakest/cheapest to strongest:

| Criterion | "Done" when... | Finite? | Notes |
| --- | --- | --- | --- |
| **Bounded-exhaustive (size ≤ N)** | every derivation with ≤ N nodes is produced | finite for each N, but grows without bound as N→∞ | **what the tool does today**; complete *relative to a bound* |
| **Production / rule coverage** | every rule alternative used ≥ 1 time | yes, finite minimal set exists | the classic target (Purdom 1972) |
| **Branch / alternative coverage** | every `|` branch taken ≥ 1 time | yes | a refinement of the above |
| **Loop-boundary coverage** | every `*`/`+` exercised at 0, 1, and ≥2 reps | yes | this is what forces strings that *enter* loops, not just bail out |
| **Recursion-depth coverage** | every recursive rule used at depth 0, 1, 2 | yes (for a fixed depth) | analogous to loop boundaries, for recursion |
| **k-context coverage** | every rule used *in the context of* every caller | yes, larger | catches interactions between rules |
| **Full language** | every string produced | **no** (infinite for looping grammars) | infeasible in general |

The crucial insight: **only coverage-based criteria have a finite, well-defined
"you are now exhaustive" point.** Bounded-exhaustive is complete only relative to
the bound you picked; full enumeration never terminates for interesting grammars.

## 4. The levers

Independent dials you might expose, roughly orthogonal:

1. **Size bound `N`** (tree-node count). *Implemented.* Governs how deep/large
   derivations may get. It is the master safety bound that guarantees termination.
2. **Coverage criterion** (table above). *Not yet exposed.* Changes *which* of the
   size-bounded derivations you keep — e.g. "smallest set hitting every
   production" instead of "all of them."
3. **Loop unrolling bound** (max repetitions of `*`/`+`). *Not separated yet* —
   currently folded into the size bound. Pulling it out lets you say "show me
   0/1/2 repetitions" cheaply, independent of overall size.
4. **Recursion-depth bound.** *Not separated yet.* Same idea for self-reference.
5. **String-length filter.** *Not yet exposed.* A secondary post-filter on the
   rendered output (distinct from tree size).
6. **Char-class concretization policy.** Currently one representative; could be
   "first/last/sample" or "every member."
7. **Dedup policy.** Currently dedup by rendered string (with an ambiguity flag);
   could switch to per-derivation.

## 5. "How many generations to reach exhaustivity?"

It depends on the criterion:

- Under **bounded-exhaustive (size ≤ N)**: the answer is just the derivation
  *count* at that bound. We already compute and show this (`DistinctCount`), and
  `countUpTo` can produce the growth curve `count(1), count(2), ...`. There is no
  finite ceiling — it keeps growing — so "exhaustive" here always means "up to N."
- Under **production coverage**: there is a finite minimal number, and it
  *saturates* — beyond some size `N*`, covering more derivations adds no new
  rules. The right signals are "productions covered: 7/7" and "saturated at size
  N*". We have the raw material (`Sample.Rules` records the rules each sample
  uses; `Analysis.minCost` gives the cheapest derivation per rule) but do **not**
  yet aggregate or surface coverage.

So: the model has the *ingredients* for coverage-based exhaustiveness, but today
the only exhaustiveness signal in the UI is `DistinctCount` + the `truncated`
flag, which is the bounded-exhaustive notion.

## 6. Metrics worth surfacing (recommended)

Cheap, high-value additions, roughly in priority order:

1. **Finite vs infinite language** — a single decisive badge.
2. **Minimum derivation size** per grammar (smallest `N` that yields any string)
   and per rule (`minCost`, already computed).
3. **Production coverage at the current `N`** — "rules hit: 7/7", "alternatives
   hit: 11/12", and the saturation size if reached.
4. **Growth curve** `count(N)` vs `N` — makes the combinatorial explosion (and
   the size at which you blow the scan cap) visible and intuitive.
5. **Loop/recursion exercise levels** — for each `*`/`+`/recursive rule, the max
   repetition/depth reached within the current bound.

Items 2–5 are computable from the existing core with modest additions; none
require changing the enumeration engine.
