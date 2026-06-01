# EBNF Sample Generator

A grammar-exploration tool. Paste a W3C-style EBNF grammar and it:

1. parses it,
2. statically checks it for pathologies (infinite-only rules, undefined
   references, unreachable rules, ambiguity, left recursion), and
3. performs **bounded exhaustive enumeration** of derivations to produce sample
   strings, up to a size bound you control.

Everything is written in F#. The core is pure and Fable-compatible, so the exact
same logic runs in the .NET CLI and in the browser (compiled to JavaScript).

## How exhaustiveness works here

Enumeration is **bounded by derivation-tree size (node count)**, not by string
length. Every rule expansion and every loop repetition consumes at least one
unit of the size budget, so the budget strictly decreases down every path. That
is what guarantees termination even for left-recursive or epsilon-looping
grammars: you cannot build an unbounded tree within a finite node budget.

At a given size bound `N`, the tool emits *every* derivation whose tree has at
most `N` nodes. Growing `N` is what forces loops and recursion to be explored
(0, 1, 2, ... repetitions; deeper nesting). See
[docs/concepts](#concepts--further-reading) below for how this relates to
"coverage" and why full language enumeration is generally infinite.

## Project layout

| Project          | Purpose                                                        |
| ---------------- | -------------------------------------------------------------- |
| `BnfGen.Core`    | Pure pipeline: AST, parser, analysis, enumeration, rendering.  |
| `BnfGen.Cli`     | Command-line front end over the pipeline.                      |
| `BnfGen.Web`     | Client-side Elmish + Feliz UI, compiled with Fable + Vite.     |
| `BnfGen.Tests`   | xUnit golden tests + FsCheck property tests.                   |

`BnfGen.Web` is intentionally **not** part of `BnfGen.slnx`: it is Fable-only and
is not built by the .NET toolchain.

## Requirements

- .NET 10 SDK
- Node.js + npm (for the web UI only)

## Running

### CLI

```bash
# from a file
dotnet run --project src/BnfGen.Cli -- path/to/grammar.ebnf --size 18 --limit 50

# or from stdin
printf 'list ::= list "," "x" | "x"\n' | dotnet run --project src/BnfGen.Cli -- --size 22
```

Flags: `--size N` (max derivation-tree node count, default 12), `--limit M` (max
samples printed, default 50), `--sep S` (separator between sequence elements).

### Web UI

```bash
cd src/BnfGen.Web
npm install          # first time only
npm run dev          # runs Fable (watch) + Vite together
```

Then open http://localhost:5173. For a production bundle: `npm run build`
(outputs to `dist/`).

### Tests

```bash
dotnet test
```

## Grammar syntax (W3C EBNF)

```
Rule       ::= Symbol "::=" Expression
Expression ::= Sequence ("|" Sequence)*
Sequence   ::= Term*
Term       ::= Primary ("?" | "*" | "+")?
Primary    ::= '"' ... '"' | "[" CharClass "]" | "#x" Hex+ | "(" Expression ")" | Symbol
```

- The first declared rule is the start symbol.
- Terminals are string literals (`"..."` / `'...'`) or `#xNN` hex characters.
- Character classes `[a-z]`, `[^0-9]` are kept symbolic and rendered to a single
  representative character.
- Comments use `/* ... */`. The set-difference operator `-` is not supported.

## Concepts / further reading

`exhaustiveness`, `loops`, and which metrics matter are discussed in
[docs/concepts.md](docs/concepts.md).
