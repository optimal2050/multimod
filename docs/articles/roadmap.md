# Development Status and Roadmap

The `multimod` package is under active but experimental development.
This document outlines current capabilities, design scope, limitations,
and roadmap for future enhancements.

## Current Status (October 2026)

**Version 0.4+ (Development)** - Core features implemented, API
stabilization in progress

**WARNING**: All implementations are in active development stage - APIs
may change, edge cases remain

The package has progressed significantly beyond initial roadmap
milestones: - \[✓\] Configuration system for solver paths\
- \[✓\] Model storage (CSV, IPC, Parquet formats)\
- \[✓\] Parameter folding and model trimming\
- \[✓\] Comprehensive vignettes and documentation\
- \[~\] Readers/writers implementation and testing\
- \[~\] API stabilization and architecture decisions

------------------------------------------------------------------------

## What’s Implemented

### Core Functionality

- **Readers**:
  - GAMS: Basic support for linear models (limited to core features)
  - GMPL: Implemented but cannot parse expressions in set definitions,
    missing some language features
  - JuMP: Not yet implemented
  - Pyomo: Not yet implemented
- **Writers**:
  - GAMS: Implemented.
    [`write_gams()`](https://optimal2050.github.io/multimod/reference/write_gams.md)
    plus
    [`write_gams_data()`](https://optimal2050.github.io/multimod/reference/write_gams_data.md)
    (data as a GDX)
  - GMPL: Implemented; reproduces the reference objective
  - JuMP: Implemented; reproduces the reference objective
  - Pyomo: Implemented; reproduces the reference objective
  - LaTeX: Implemented, functional
- **Direct matrix route**: assemble the LP straight from the AST, with
  no generated model text in between -
  [`model_to_lp()`](https://optimal2050.github.io/multimod/reference/model_to_lp.md),
  [`write_mps()`](https://optimal2050.github.io/multimod/reference/write_mps.md),
  [`solve_highs()`](https://optimal2050.github.io/multimod/reference/solve_highs.md)
  /
  [`solve_mps()`](https://optimal2050.github.io/multimod/reference/solve_mps.md),
  [`read_mps_solution()`](https://optimal2050.github.io/multimod/reference/read_mps_solution.md).
  This skips the model-build step that dominates time and memory on
  large models, and is intended to become the default for linear
  problems.
- **AST**: Complete abstract syntax tree representation for linear
  models
- **Solvers**: Integration with GLPK, CBC, HiGHS, CPLEX, Gurobi (via
  external calls)

### Advanced Features

- **Parameter Folding**: Automated dimensionality reduction for uniform
  parameters
- **Model Trimming**: Remove unused sets, parameters, variables, and
  equations
- **Model Storage**: Save/load models in CSV, Apache Arrow IPC, or
  Parquet formats
- **Configuration System**: Multi-tier configuration (R options → env
  vars → YAML → auto-detect)
- **In-Memory Parsing**: Readers accept file paths or character vectors

### Data Integration & Connectivity

- **Storage Formats**:
  - Apache Arrow IPC (fast, type-safe, binary)
  - Apache Parquet (compressed, columnar)
  - CSV (human-readable, portable)
- **Data Sources**:
  - Import from `energyRt` package structures
  - Link to external Arrow/Parquet datasets for lazy loading
- **Model Export Modes**:
  - **Embedded data**: For small models (data included in generated
    code)
  - **External data**: For large models (code connects to Arrow
    repository)
  - Python/Pyomo and Julia/JuMP support both modes
- **Functions**:
  [`save_model()`](https://optimal2050.github.io/multimod/reference/save_model.md),
  [`load_model()`](https://optimal2050.github.io/multimod/reference/load_model.md),
  `import_energyRt_data()`, `link_scenario_data_with_log()`

### Documentation & Visualization

- **LaTeX Generation**: Publication-ready mathematical documentation
  with annotations
- **Expression Visualization**: Interactive AST visualization with
  visNetwork
- **Comprehensive Vignettes**: 10+ vignettes covering major features
- **Example Models**: Unified `example_models` dataset

------------------------------------------------------------------------

## Current Limitations

**WARNING**: All features are experimental and subject to change

- **Optimization Types**: Primarily designed for linear programming (LP)
  models

  - Nonlinear expressions have basic AST support but limited
    implementation
  - Integer/binary variables supported in AST but not all writers

- **Model Scope**: Designed around energy system models (`energyRt`
  structures)

  - Can handle general LP models but may require adaptation

- **Parser Completeness**:

  - **GAMS**: Basic reader for linear models, many advanced features not
    supported
  - **GMPL**: Cannot parse expressions in set definitions, missing
    language features
  - **JuMP**: Reader not yet implemented
  - **Pyomo**: Reader not yet implemented

- **Writer Maturity**:

  - **GAMS**: Implemented; writing the data needs a GDX, so `gdxtools`
    and a GAMS installation (no licence required)
  - **GMPL/JuMP/Pyomo**: Solve-verified on the reference model; edge
    cases remain
  - **LaTeX**: Stable

- **API Stability**: Function signatures and class structures may change
  as we stabilize the architecture

- **GMPL solving** needs GLPK’s `glpsol` executable. The `glpkAPI` route
  was retired when that package left CRAN; it was the only R package
  that both bundled GLPK and exposed GLPK’s MathProg translator, and
  neither `Rglpk` nor `highs` replaces it.

------------------------------------------------------------------------

## Project Philosophy

The goal of `multimod` is **not** to automate full model translation or
parsing. Instead, it aims to:

- Facilitate development and symbolic restructuring of optimization
  models
- Act as a **transparent intermediate representation** for
  multi-language export
- Assist with documentation and visualization of models

The symbolic structure enables users to adapt and refine models by hand,
especially during development, testing, or educational use.

------------------------------------------------------------------------

## Potential Use Cases

- **Model Development**: Prototype and refine optimization models across
  languages
- **Cross-Platform Translation**: Convert models between GAMS, JuMP,
  Pyomo, GMPL
- **Documentation**: Generate publication-ready LaTeX documentation with
  equations
- **Model Optimization**: Apply folding and trimming to reduce model
  size
- **Debugging**: Visualize model structure and equation dependencies
- **Teaching**: Educational tool for understanding optimization model
  structure
- **Collaboration**: Share models in language-agnostic format
- **Model Storage**: Efficiently save/load large models with structured
  data formats

------------------------------------------------------------------------

## Roadmap to v1.0

### Immediate Priorities (v0.6-0.8)

**Architecture & API Stabilization**: - \[~\] Migration from S3 to R6/S7
class system. An R6 session wrapper
([`mm_session()`](https://optimal2050.github.io/multimod/reference/mm_session.md))
is in; the wider S7 move is deferred\
- \[~\] Front-end API design and stabilization\
- \[~\] Finalize function naming conventions

**API Revision, Unification & Simplification**: - \[ \] Consolidate
overlapping functions with inconsistent interfaces\
- \[ \] Establish unified parameter naming conventions across all
functions\
- \[ \] Simplify data format handling (reduce format-specific function
variants)\
- \[ \] Create consistent return value structures (standardize output
formats)\
- \[ \] Design clear separation between user-facing and internal
functions\
- \[ \] Implement `@keywords internal` for internal helpers to clean up
documentation\
- \[ \] Unify error/warning messaging patterns across the package\
- \[ \] Standardize default argument values and behavior\
- \[ \] Create function families with predictable naming patterns:\
- `read_*()` / `write_*()` for I/O operations\
- `as_*()` for format conversion\
- `load_*()` / `save_*()` for data persistence\
- `parse_*()` for parsing operations\
- `validate_*()` for validation checks\
- \[ \] Reduce exported function count (move internal utilities to
package namespace)\
- \[ \] Design fluent API patterns for common workflows (e.g., model
%\>% fold_model() %\>% trim_model())

**Reader/Writer Completion**:\
- \[✓\] Basic GAMS reader (linear models, including subset
declarations) - \[✓\] Implement GAMS writer
([`write_gams()`](https://optimal2050.github.io/multimod/reference/write_gams.md) +
[`write_gams_data()`](https://optimal2050.github.io/multimod/reference/write_gams_data.md))\
- \[ \] Implement JuMP reader\
- \[ \] Implement Pyomo reader\
- \[ \] Finalize GMPL reader (handle set expressions, missing features)\
- \[✓\] Complete testing of GMPL/JuMP/Pyomo writers - each reproduces
the reference objective

**Direct Matrix Route**: - \[✓\] Assemble the LP from the AST
([`model_to_lp()`](https://optimal2050.github.io/multimod/reference/model_to_lp.md)),
write MPS, solve in process - \[ \] Make it the default for linear
problems, with the symbolic writers kept for interoperability,
inspection and documentation - \[ \] Expose row/column names in the MPS
as an option (they are positional today, which is what keeps the file
small)

**Core Improvements**: - \[ \] Enhanced parser error messages and
diagnostics - \[ \] Performance optimization for large models - \[ \]
Extended test coverage (\>80% goal)

### v1.0 Release Goals

- Stable API with R6/S7 classes
- Basic reader/writer suite for GAMS, GMPL, JuMP, Pyomo
- Comprehensive documentation and vignettes
- Full test suite with edge case coverage
- CRAN submission readiness

### Long-Term Vision (v2.0+)

**Performance & Scalability**: - C++ core implementation for speed -
Python API - Julia API

**Advanced Features**: - {AMPL reader/writer} - MPS/LP file format
support - Equation simplification using symbolic algebra - Automated
redundancy detection with safety checks - Automated data-driven mapping
generation - Direct solver API integration (bypass file I/O)

**Tooling**: - Interactive model explorer (Shiny app) - Equation
dependency graphs - Sensitivity analysis tools - Model comparison
utilities

------------------------------------------------------------------------

## Contributing & Support

The package is under active development and contributions are welcome!

### Ways to Contribute

- **Bug Reports**: Open issues on GitHub for any bugs or unexpected
  behavior
- **Feature Requests**: Suggest new features or improvements
- **Code Contributions**: Submit pull requests with bug fixes or
  enhancements
- **Documentation**: Improve vignettes, examples, or function
  documentation
- **Testing**: Help test with different models and edge cases
- **Use Cases**: Share your experiences and workflows

### Development Priorities

Current focus areas (contributions especially welcome): 1. Parser
robustness for edge cases 2. Performance optimization for large models
3. Additional example models and workflows 4. Extended test coverage

### Get Involved

- **GitHub**: <https://github.com/energyRt/multimod>
- **Issues**: <https://github.com/energyRt/multimod/issues>
- **Contact**: Open an issue or discussion on GitHub
