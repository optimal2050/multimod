# Working on multimod

Onboarding notes for contributors and coding agents — the things you would
otherwise discover the hard way.

This is the canonical file; `AGENTS.md` and `.github/copilot-instructions.md` just
point here. Stack-wide rules (naming, style, git policy, API principles) live in
[optimal2050 CONVENTIONS.md](https://github.com/optimal2050/.github/blob/main/CONVENTIONS.md);
this file adds only what is multimod-specific.

## What it is

A DSL for writing algebraic models in R and **rendering them to several
optimization languages**: GAMS, GMPL/MathProg, Pyomo, JuMP, LaTeX, plain R.
The model is stored as an abstract syntax tree; each `as_*()` function is a
backend that walks it.

Default branch is **`R6S7`**, not `main`.

## Object system: S3, despite the branch name

The AST is **S3**, built by `new_ast()` in `R/ast-classes.R`:

```r
structure(list(...), class = c(node_type, inherits_class, "ast"))
```

Neither R6 nor S7 is used anywhere — they are not even in `DESCRIPTION`
`Imports`. The branch name `R6S7` records an intended migration, not the
current state. Do not assume R6/S7 semantics (reference vs value, validators,
properties) when reading or writing AST code.

## Layout

```
R/as_*.R        one renderer per target language (gams, gmpl, jump, pyomo,
                latex, r, rfunction, visNetwork, multimod)
R/ast-classes.R the AST node constructors — start here
R/data_management.R, R/model_storage.R
                the workspace: save_model()/load_model(), Arrow / Parquet / CSV
tests/testthat/ 11 files; the *-solve.R ones actually invoke solvers
vignettes/      one per backend (gmpl, jump, latex), plus ast, folding,
                configuration, model_workspace
```

## Build and test

```r
devtools::load_all()
devtools::test()
devtools::document()
devtools::check()
```

## Traps

- **A renderer can silently emit nothing.** Each `as_*()` walks the AST by
  node class; an unhandled node type falls through rather than erroring. When
  a backend's output is missing a constraint, suspect a node class the
  renderer does not dispatch on — not the model.
- **The backends must agree.** A change to the AST or to a shared helper has
  to be reflected in *every* `as_*.R`, and the `test-*-solve.R` files are what
  catch a backend left behind. Run them, not just the unit tests.
- **Solver tests need solvers.** `test-gmpl-solve.R`, `test-jump-solve.R` and
  `test-pyomo-solve.R` need the corresponding toolchain installed; they skip
  otherwise, so a green run does not by itself mean the backends were
  exercised. Check for skips.
- **Snapshots.** `tests/testthat/_snaps/` holds rendered-code snapshots.
  Review a snapshot diff as generated *code*, not as text — a formatting-only
  change is fine, a changed index or coefficient is a bug.

## Further reading

- `vignette("multimod")` — the DSL and the AST
- `vignette("model_workspace")` — storage formats and lazy loading
- `CONTRIBUTING.md` — PR process, licensing (Apache-2.0)

---

*Drafted 2026-08-24 with AI assistance; content is the maintainers'
responsibility. Re-check the commands and file references at each release — a
stale instruction is worse than none.*
