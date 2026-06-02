# EBNF Sample Generator

A grammar-exploration tool. Paste a W3C-style EBNF grammar and it produces
**realistic example strings** that conform to it.

The web UI generates samples with a **grammar-constrained LLM**: the grammar is
compiled to GBNF and sent to [Fireworks AI](https://fireworks.ai)'s grammar
mode, which masks the model token-by-token so it **cannot emit a string outside
the language**. That guarantee is why even a small, cheap model is safe on
"looks-like-JSON-but-subtly-isn't" specs, and why the samples look like real
data (e.g. plausible emails) instead of the dense symbol-soup that exhaustive
enumeration tends to produce first for parser-hostile grammars.

The original engine - parsing, static pathology checks (infinite-only rules,
undefined references, unreachable rules, ambiguity, left recursion), and
**bounded exhaustive enumeration** of derivations - still lives in `BnfGen.Core`
and powers the CLI and tests. It is kept as the trustworthy backbone: the web
app surfaces its static facts (language class, min size, saturation size) and
diagnostics alongside the LLM samples.

Everything is written in F#. The core is pure and Fable-compatible, so the exact
same logic runs in the .NET CLI, the browser (Fable -> JS), and the backend API.

## Architecture

```
Browser SPA (GitHub Pages)  --POST /api/generate-->  BnfGen.Api (Render)  -->  Fireworks grammar mode
                                                          |
                                          BnfGen.Core: parse + analyze + Gbnf.compile
```

- `BnfGen.Core` parses the EBNF, rejects fatal grammars, and `Gbnf.compile`s the
  AST to a GBNF grammar (the mapping is ~1:1).
- `BnfGen.Api` is a thin proxy: it holds the Fireworks API key, calls grammar
  mode `count` times (varied seeds) for diverse samples, and returns them with
  the compiled GBNF and the static grammar facts/diagnostics.
- The frontend never sees the key; it only talks to `BnfGen.Api`.

### Inference engine and model

Fireworks is currently the only hosted API that accepts **arbitrary
context-free (GBNF) grammars** (Groq/Cerebras only constrain to JSON Schema,
which cannot express most EBNF). The model is set by `FIREWORKS_MODEL` and any
serverless model works, since all support grammar mode. Notes:

- Default: `accounts/fireworks/models/llama-v3p1-8b-instruct` - cheap
  (~$0.20/1M tokens, fractions of a cent per request), fast, non-reasoning.
- The truly tiny dense models (Llama 3.2 1B/3B, Qwen3 1.7B/4B) are **not on
  Fireworks serverless** (dedicated GPU only), so they aren't cheap options.
- Cheapest serverless: `accounts/fireworks/models/gpt-oss-20b` ($0.07/$0.30) -
  fast MoE, but a reasoning model, so test grammar mode before relying on it.
- Correctness does not depend on the model (the mask guarantees it); model size
  only affects sample variety/realism.

## How exhaustiveness works (CLI engine)

Enumeration is **bounded by derivation-tree size (node count)**, not by string
length. Every rule expansion and every loop repetition consumes at least one
unit of the size budget, so the budget strictly decreases down every path. That
is what guarantees termination even for left-recursive or epsilon-looping
grammars: you cannot build an unbounded tree within a finite node budget.

At a given size bound `N`, the tool emits *every* derivation whose tree has at
most `N` nodes. Growing `N` is what forces loops and recursion to be explored
(0, 1, 2, ... repetitions; deeper nesting).

Alongside the samples it reports a **grammar summary**: an exact Empty / Finite /
Infinite language classification, **rule and branch coverage** (every `|`
alternative, at any nesting), the **saturation size** at which coverage stops
growing (the practical "you are now exhaustive" point), and the deepest loop /
recursion levels reached. Diagnostics are split into a generation lane and a
parsing lane, because generability and parseability are independent. See
[docs/concepts.md](docs/concepts.md) for the full mental model.

## Project layout

| Project          | Purpose                                                          |
| ---------------- | ---------------------------------------------------------------- |
| `BnfGen.Core`    | Pure pipeline: AST, parser, analysis, enumeration, GBNF, render. |
| `BnfGen.Api`     | ASP.NET Core minimal API: proxies Fireworks grammar mode.        |
| `BnfGen.Cli`     | Command-line front end over the pipeline.                        |
| `BnfGen.Web`     | Client-side Elmish + Feliz UI, compiled with Fable + Vite.       |
| `BnfGen.Tests`   | xUnit golden tests + FsCheck property tests.                     |

`BnfGen.Web` is intentionally **not** part of `BnfGen.slnx`: it is Fable-only and
is not built by the .NET toolchain.

## Requirements

- .NET 10 SDK
- Node.js + npm (for the web UI only)
- A [Fireworks AI](https://fireworks.ai) API key (for the backend)

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

### Backend API

```bash
export FIREWORKS_API_KEY=fw-...                  # required
export FIREWORKS_MODEL=accounts/fireworks/models/llama-v3p1-8b-instruct  # optional
export ALLOWED_ORIGINS=http://localhost:5173     # optional (CORS; empty = any)
dotnet run --project src/BnfGen.Api              # listens on $PORT (default 8080)
```

`POST /api/generate` with `{ "grammar": "...", "count": 10, "temperature": 0.7 }`
returns `{ samples, gbnf, diagnostics, facts, model }`. Parse errors return 400,
fatal grammars 422, upstream failures 502. `GET /healthz` returns `ok`. Quick
check:

```bash
curl -s localhost:8080/api/generate -H 'content-type: application/json' \
  -d '{"grammar":"email ::= [a-z]+ \"@\" [a-z]+ \".com\"","count":5}'
```

### Web UI

```bash
cd src/BnfGen.Web
npm install          # first time only
npm run dev          # runs Fable (watch) + Vite together
```

Then open http://localhost:5173. The frontend calls the backend at
`VITE_API_URL` (defaults to `http://localhost:8080` in dev). For a production
bundle: `VITE_API_URL=https://your-api.onrender.com npm run build` (outputs to
`dist/`).

### Tests

```bash
dotnet test
```

## Deploying

- **Backend** -> Render.com via [`render.yaml`](render.yaml) and the
  [`Dockerfile`](Dockerfile). Set `FIREWORKS_API_KEY` as a secret and
  `ALLOWED_ORIGINS` to your Pages origin. Free tier works (cold-starts after
  idle); bump to Starter to keep it warm.
- **Frontend** -> GitHub Pages via
  [`.github/workflows/deploy-pages.yml`](.github/workflows/deploy-pages.yml).
  Set a repository Variable `VITE_API_URL` to the Render URL so the build points
  the SPA at the API.

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
