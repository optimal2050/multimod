# Contributing to multimod

> Stack-wide conventions (naming, style, git policy, API
> principles, and the terms on which AI assistance is used) are unified in
> [optimal2050 CONVENTIONS.md](https://github.com/optimal2050/.github/blob/main/CONVENTIONS.md);
> this file covers the workflow of this repo.

Thanks for your interest! `multimod` is part of the optimal2050 modeling stack.
It provides a DSL for writing algebraic models in R and rendering them to
several optimization languages — GAMS, GMPL/MathProg, Pyomo, JuMP, LaTeX and
plain R.

> The repository is public and contributions are welcome — issues, ideas, and
> pull requests alike. The package is pre-1.0, so APIs may still move; open an
> issue first for larger changes.

**Before writing code, read [`.claude/CLAUDE.md`](.claude/CLAUDE.md)** for the
repo's layout and its known traps.

## Object system

The AST is **S3** (`new_ast()` in `R/ast-classes.R`). Neither R6 nor S7 is used
anywhere in the package — the default branch is named `R6S7` after an intended
migration, not the current state. Do not assume reference semantics.

## Repository layout

```
multimod/
├── R/as_*.R          one renderer per target language
├── R/ast-classes.R   the AST node constructors — start here
├── R/data_management.R, R/model_storage.R
│                     workspace: save_model()/load_model(), Arrow/Parquet/CSV
├── tests/testthat/   unit tests; the *-solve.R files invoke real solvers
├── vignettes/        one per backend, plus ast, folding, model_workspace
└── .github/workflows/  CI
```

## Development workflow

```r
# from the repo root
devtools::load_all()
devtools::test()
devtools::document()
devtools::check()
```

## Keeping the backends in step

Every renderer walks the same AST. A change to a node type, or to a shared
helper, has to be reflected in **every** `as_*.R` — and because an unhandled
node class falls through rather than erroring, a backend left behind emits
silently incomplete code rather than failing.

The `test-gmpl-solve.R`, `test-jump-solve.R` and `test-pyomo-solve.R` files are
what catch this, but they **skip** when the corresponding toolchain is missing.
A green run with skips has not exercised the backends; check the skip count.

Rendered-code snapshots live in `tests/testthat/_snaps/`. Review a snapshot
diff as generated *code*: a formatting change is fine, a changed index or
coefficient is a bug.

## Commit & PR conventions

- Open PRs against **`R6S7`** — this repo's default branch is not `main`.
- Conventional Commits style is encouraged but not enforced
  (`feat:`, `fix:`, `docs:`, `refactor:`, `test:`, `chore:`).
- CI must pass.
- User-facing changes get a bullet in `NEWS.md` under the development version,
  filed beneath one of `## Breaking changes / New features / Deprecations /
  Bug fixes / Documentation`, describing the final state in a line or two.

## License

By contributing you agree that your contributions are licensed under the Apache
License. See [LICENSE.md](LICENSE.md). Note that other packages in the stack are
Apache-2.0 or AGPL-3 — do not copy code or data across that boundary without an
explicit maintainer decision.

## Code of Conduct

Please note that this project is released with a
[Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in it you
agree to abide by its terms.
