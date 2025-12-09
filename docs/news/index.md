# Changelog

## multimod 0.4.0.9007 (development version, 2025-12-08)

### New Features

#### Data Integration & Storage

- **Model workspace system**
  ([`save_model()`](https://optimal2050.github.io/multimod/reference/save_model.md),
  [`load_model()`](https://optimal2050.github.io/multimod/reference/load_model.md)):
  - Apache Arrow IPC format (fast, type-safe, binary)
  - Apache Parquet format (compressed, columnar)
  - CSV format (human-readable, portable)
  - Lazy loading support for large datasets
  - See
    [`vignette("model_workspace")`](https://optimal2050.github.io/multimod/articles/model_workspace.md)
    for details
- **Data import and linking**:
  - [`import_energyRt_data()`](https://optimal2050.github.io/multimod/reference/import_energyRt_data.md):
    Import from energyRt package structures
  - [`populate_sets_from_scenario()`](https://optimal2050.github.io/multimod/reference/populate_sets_from_scenario.md):
    Extract set members from parameters
  - [`link_scenario_data()`](https://optimal2050.github.io/multimod/reference/link_scenario_data.md):
    Link to Arrow/Parquet files for lazy loading
  - Support for OSeMOSYS example model data
- **Flexible data modes** in generated code:
  - **Embedded**: Data included directly in generated Python/Julia code
    (for small models)
  - **External**: Code connects to Arrow repository (for large models)
- **Example datasets**: Consolidated into unified `example_models`
  structure
  - Access: `data(example_models)` provides both energyRt and OSeMOSYS
    examples
  - Structure: `example_models$energyRt$multimod`,
    `example_models$OSeMOSYS$gmpl`, etc.
  - Models used: OSeMOSYS-Utopia (standard test case), energyRt-DEMO
    (BASE_UTOPIA scenario)
  - Old datasets removed; use `example_models$energyRt$multimod` instead
    of `utopia_multimod`

#### Code Generation

- **Pyomo support**: Complete Python/Pyomo code generation
  - Abstract model design with external data loading
  - Multiple solver support (HiGHS, Gurobi, CPLEX, etc.)
  - Both embedded and external data modes
  - See
    [`vignette("pyomo")`](https://optimal2050.github.io/multimod/articles/pyomo.md)
    for workflow details
- **JuMP improvements**
  ([`write_jump()`](https://optimal2050.github.io/multimod/reference/write_jump.md)):
  - Modern Julia/JuMP syntax with named constraints
  - Tuple indexing: `eqName[(h,r,y) in mapping]`
  - Short index aliases: h (tech), r (region), c (comm), y (year), ts
    (slice)
  - Clean [`get()`](https://rdrr.io/r/base/get.html) pattern for
    parameter access with defaults
  - Variable bounds from GAMS types: `>= 0`, `<= 0`, `Bin`, `Int`, free
  - Comprehensive diagnostics with CSV logging
  - Both embedded and external data modes
- **GMPL support**
  ([`write_gmpl()`](https://optimal2050.github.io/multimod/reference/write_gmpl.md),
  [`write_gmpl_data()`](https://optimal2050.github.io/multimod/reference/write_gmpl_data.md)):
  - Full GMPL/MathProg syntax support
  - Proper formatting and indentation
  - Lazy loading from Arrow/Parquet datasets
  - Memory-efficient data writing

#### Configuration & Usability

- **Configuration system**: Unified configuration for solver paths with
  4-tier priority:
  - R options \> Environment variables \> YAML config \> Auto-detection
  - New functions:
    [`get_multimod_python()`](https://optimal2050.github.io/multimod/reference/multimod-config.md),
    [`get_multimod_julia()`](https://optimal2050.github.io/multimod/reference/multimod-config.md),
    [`get_multimod_glpsol()`](https://optimal2050.github.io/multimod/reference/multimod-config.md)
  - YAML support:
    [`multimod_config_write()`](https://optimal2050.github.io/multimod/reference/multimod-config.md),
    [`multimod_config_read()`](https://optimal2050.github.io/multimod/reference/multimod-config.md),
    [`multimod_config_show()`](https://optimal2050.github.io/multimod/reference/multimod-config.md)
  - See
    [`vignette("configuration")`](https://optimal2050.github.io/multimod/articles/configuration.md)
    for details
- **In-memory parsing**: All reader functions
  ([`read_gams()`](https://optimal2050.github.io/multimod/reference/read_gams.md),
  [`read_gmpl()`](https://optimal2050.github.io/multimod/reference/read_gmpl.md),
  [`read_gmpl_data()`](https://optimal2050.github.io/multimod/reference/read_gmpl_data.md),
  [`import_gmpl_data()`](https://optimal2050.github.io/multimod/reference/import_gmpl_data.md))
  now accept character vectors in addition to file paths, enabling
  direct parsing from R objects without temporary files.

#### Model Optimization

- **Parameter folding**: Automated dimensionality reduction for uniform
  parameters
  - `fold_model()`,
    [`create_fold_spec()`](https://optimal2050.github.io/multimod/reference/create_fold_spec.md),
    [`get_fold_summary()`](https://optimal2050.github.io/multimod/reference/get_fold_summary.md)
  - See
    [`vignette("folding")`](https://optimal2050.github.io/multimod/articles/folding.md)
    for details
- **Model trimming**: Remove unused sets, parameters, variables, and
  equations
  - `trim_model()`,
    [`get_trim_summary()`](https://optimal2050.github.io/multimod/reference/get_trim_summary.md)

### Bug Fixes

- **JuMP fix**: 1-dimensional mappings now correctly loaded as
  `Set{String}` instead of `Set{Tuple{String}}`. This was causing
  membership checks like `t in mTradeCapacityVariable` to silently fail,
  resulting in missing constraint terms and incorrect model
  formulations.
  - 1D mappings: `Set{String}` for direct membership checks
  - Multi-D mappings: `Set{Tuple{...}}` for tuple membership
  - Parameters follow same pattern for consistent dictionary key types
  - Generated models now produce identical results to reference
    implementations
- **Export functions**: Updated
  [`export_model_source()`](https://optimal2050.github.io/multimod/reference/export_model_source.md)
  to use new `example_models` structure
  - `export_osemosys()` and `export_energyrt()` now work with
    consolidated datasets

### Documentation

- New vignette:
  [`vignette("configuration")`](https://optimal2050.github.io/multimod/articles/configuration.md) -
  Configuration system and solver setup
- New vignette:
  [`vignette("pyomo")`](https://optimal2050.github.io/multimod/articles/pyomo.md) -
  Complete Pyomo workflow
- New vignette:
  [`vignette("model_workspace")`](https://optimal2050.github.io/multimod/articles/model_workspace.md) -
  Model workspace and data management
- Updated vignette:
  [`vignette("jump")`](https://optimal2050.github.io/multimod/articles/jump.md) -
  Complete JuMP workflow
- Updated vignette:
  [`vignette("gmpl")`](https://optimal2050.github.io/multimod/articles/gmpl.md) -
  Complete GMPL workflow
- Updated vignette:
  [`vignette("latex")`](https://optimal2050.github.io/multimod/articles/latex.md) -
  LaTeX generation with new dataset
- Updated: Development roadmap with realistic implementation status

------------------------------------------------------------------------

## multimod 0.0.1 (2025-06-01)

- Initial GitHub release (June 1, 2025).
