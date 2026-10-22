# Native core (planned, not yet implemented)

This directory is reserved for the native implementation of multimod that
the R and Python packages will bind to. The language is open between C++17
and Rust; the directory name is neutral for that reason.

## Scope

Everything independent of the host language moves into the core: the AST and
model types with their JSON schema and validation, the GAMS and MathProg
readers, the code generators (GAMS, MathProg, JuMP, Pyomo, LaTeX), parameter
evaluation, folding and trimming, and the matrix route (LP assembly, MPS
output, solution decoding). Data crosses the boundary through the Arrow C
data interface. Constructors, printing, plots and solver sessions stay in
each language.

## Port order and gates

The port starts once the R package is on CRAN with the AST schema frozen as
version 1. Each stage is gated on agreement with the R implementation:

1. Core skeleton: AST, JSON, validation, a C ABI, and the R and Python
   bindings working on the toy model. Gate: JSON round trips byte-identical
   to `model_to_json()` for every shipped example model.
2. Readers. Gate: the parsed AST matches the R output for the field models.
3. Matrix, MPS and decoding. Gate: coefficient-for-coefficient agreement.
4. Code generators. Gate: byte-identical text.
5. Parameter evaluation, fold and trim.
6. The R package becomes a binding; the testthat suite runs unchanged.

The reference outputs for these gates will live in [`../specs/`](../specs/README.md).

## Planned layout

```
core/
├── CMakeLists.txt or Cargo.toml
├── include/multimod/     # public C ABI headers (C++ variant)
├── src/
├── tests/
└── examples/
```
