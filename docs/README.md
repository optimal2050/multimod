# Documentation

This directory **is** the published site: GitHub Pages serves it from the
`docs/` folder on `main`, so everything here is committed, including the
built pkgdown output under `r/`.

## Current layout

```
docs/
├── .nojekyll        # keep Jekyll away from pkgdown's *_files/ figure dirs
├── 404.html         # forwards pre-2026-10 URLs (/multimod/<page>) to r/<page>
├── index.qmd        # landing page source (rendered with Quarto)
├── index.html       # ... and its rendered output, committed
├── styles.css       # landing page styling only
├── logo.png         # copied from man/figures/ by the build script
├── r/               # pkgdown site (see ../_pkgdown.yml, destination: docs/r)
└── python/          # MkDocs site of the Python binding, once it exists
```

| URL | What |
|---|---|
| `optimal2050.github.io/multimod/` | landing page, links every language |
| `optimal2050.github.io/multimod/r/` | R reference and articles |

## Rebuilding

```sh
Rscript tools/site/build_site.R   # docs/r/, docs/index.html (and docs/python/)
```

It runs `pkgdown::build_site()` into `docs/r/`, renders `docs/index.qmd`,
and copies the logo. The Python step (reference pages written by
`tools/site/python_reference.py`, built with MkDocs into `docs/python/` from
`python/.venv` with the `docs` extra) runs only when `python/pyproject.toml`
exists.

Commit the result.

There is deliberately **no CI deploy yet**: the site is built locally and
committed, as in the companion `timescales` and `geoscales` repositories.

## Planned layout

Language sections sit beside `r/`, so no existing URL moves again:

```
docs/
├── r/                    # pkgdown (as today)
├── python/               # MkDocs reference of the binding
└── core/                 # API reference of the native core
```
