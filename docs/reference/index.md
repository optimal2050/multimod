# Package index

## Multimod classes

Constructors for the declarations a model is made of, and the text route
for writing an equation.

- [`mm_equation()`](https://optimal2050.github.io/multimod/reference/mm_equation.md)
  : Write an equation as MathProg or GAMS text
- [`new_equation()`](https://optimal2050.github.io/multimod/reference/new_equation.md)
  : Create a multimod equation object
- [`new_mapping()`](https://optimal2050.github.io/multimod/reference/new_mapping.md)
  : Create a multimod mapping object
- [`new_model()`](https://optimal2050.github.io/multimod/reference/new_model.md)
  : Create a multimod model object
- [`new_parameter()`](https://optimal2050.github.io/multimod/reference/new_parameter.md)
  : Create a multimod parameter
- [`new_set()`](https://optimal2050.github.io/multimod/reference/new_set.md)
  : Create a new set object
- [`new_variable()`](https://optimal2050.github.io/multimod/reference/new_variable.md)
  : Create a multimod variable
- [`as_multimod()`](https://optimal2050.github.io/multimod/reference/as_multimod.md)
  : Convert an object to a multimod structure

## Abstract Syntax Tree (AST) classes

Nodes for expressions, references and index domains.

- [`new_ast()`](https://optimal2050.github.io/multimod/reference/new_ast.md)
  [`ast_set()`](https://optimal2050.github.io/multimod/reference/new_ast.md)
  : Create a new multimod AST (Abstract Syntax Tree) node
- [`ast_call()`](https://optimal2050.github.io/multimod/reference/ast_call.md)
  : Create a function call AST node
- [`ast_constant()`](https://optimal2050.github.io/multimod/reference/ast_constant.md)
  : Create a constant AST node
- [`ast_dims()`](https://optimal2050.github.io/multimod/reference/ast_dims.md)
  : Create an AST node for dimensions (dims) of a symbol
- [`ast_equation()`](https://optimal2050.github.io/multimod/reference/ast_equation.md)
  : Create an equation AST node
- [`ast_expression()`](https://optimal2050.github.io/multimod/reference/ast_expression.md)
  : Create an expression AST node
- [`ast_func()`](https://optimal2050.github.io/multimod/reference/ast_func.md)
  : Create a function AST node
- [`ast_mapping()`](https://optimal2050.github.io/multimod/reference/ast_mapping.md)
  : Create a mapping AST node
- [`ast_parameter()`](https://optimal2050.github.io/multimod/reference/ast_parameter.md)
  : Create a parameter AST node
- [`ast_prod()`](https://optimal2050.github.io/multimod/reference/ast_prod.md)
  : Create a product AST node
- [`ast_setmax()`](https://optimal2050.github.io/multimod/reference/ast_setmax.md)
  : Create a set maximum AST node
- [`ast_setmin()`](https://optimal2050.github.io/multimod/reference/ast_setmin.md)
  : Create a set minimum AST node
- [`ast_shift()`](https://optimal2050.github.io/multimod/reference/ast_shift.md)
  : Create an index shift AST node for ordered set references
- [`ast_sum()`](https://optimal2050.github.io/multimod/reference/ast_sum.md)
  : Create a summation AST node
- [`ast_symbol()`](https://optimal2050.github.io/multimod/reference/ast_symbol.md)
  : Create a symbol AST node (for unclassified identifiers)
- [`ast_unary()`](https://optimal2050.github.io/multimod/reference/ast_unary.md)
  : Create a unary expression AST node
- [`ast_variable()`](https://optimal2050.github.io/multimod/reference/ast_variable.md)
  : Create a variable AST node
- [`ast_when()`](https://optimal2050.github.io/multimod/reference/ast_when.md)
  : Construct a conditional expression node for multimod AST
- [`ast_where()`](https://optimal2050.github.io/multimod/reference/ast_where.md)
  : Create a "where" AST node

## Index aliases

Short index names used in rendered equations.

- [`add_index_aliases()`](https://optimal2050.github.io/multimod/reference/add_index_aliases.md)
  : Add index aliases to a multimod model
- [`generate_index_alias()`](https://optimal2050.github.io/multimod/reference/generate_index_alias.md)
  : Generate dummy variable name from set name
- [`generate_index_aliases()`](https://optimal2050.github.io/multimod/reference/generate_index_aliases.md)
  : Generate dummy variable names for a list of sets
- [`has_index_aliases()`](https://optimal2050.github.io/multimod/reference/has_index_aliases.md)
  : Check if model has index aliases

## Generic methods

Printing, plotting and summarising model objects.

- [`print(`*`<ast_comparison>`*`)`](https://optimal2050.github.io/multimod/reference/print.ast_comparison.md)
  : Print comparison results in a readable format
- [`print(`*`<ast_function>`*`)`](https://optimal2050.github.io/multimod/reference/print.ast_function.md)
  : Print method for functions created from AST
- [`print(`*`<model_validation>`*`)`](https://optimal2050.github.io/multimod/reference/print.model_validation.md)
  : Print method for model validation results
- [`print(`*`<ast>`*`)`](https://optimal2050.github.io/multimod/reference/print.md)
  [`print(`*`<func>`*`)`](https://optimal2050.github.io/multimod/reference/print.md)
  : Print methods for AST classes
- [`print(`*`<variable_comparison>`*`)`](https://optimal2050.github.io/multimod/reference/print.variable_comparison.md)
  : Print method for variable comparison results
- [`as.character(`*`<expression>`*`)`](https://optimal2050.github.io/multimod/reference/as.character.expression.md)
  : Convert AST expression to character

## Schema and serialisation

The node schema, validation, and the JSON round trip.

- [`ast_schema()`](https://optimal2050.github.io/multimod/reference/ast_schema.md)
  : The multimod AST schema
- [`schema_type()`](https://optimal2050.github.io/multimod/reference/schema_type.md)
  : Schema node type of an AST node
- [`schema_node_types()`](https://optimal2050.github.io/multimod/reference/schema_node_types.md)
  : Node types known to the schema
- [`validate_ast()`](https://optimal2050.github.io/multimod/reference/validate_ast.md)
  : Validate an AST node or model against the schema
- [`ast_json_schema()`](https://optimal2050.github.io/multimod/reference/ast_json_schema.md)
  : Emit a JSON Schema for the multimod AST
- [`model_to_json()`](https://optimal2050.github.io/multimod/reference/model_to_json.md)
  : Serialise a model to JSON
- [`model_from_json()`](https://optimal2050.github.io/multimod/reference/model_from_json.md)
  : Rebuild a model from JSON
- [`ast_to_json()`](https://optimal2050.github.io/multimod/reference/ast_to_json.md)
  : Convert an AST node to a language-neutral list
- [`ast_from_json()`](https://optimal2050.github.io/multimod/reference/ast_from_json.md)
  : Rebuild an AST node from a language-neutral list
- [`Set()`](https://optimal2050.github.io/multimod/reference/multimod-constructors.md)
  [`Mapping()`](https://optimal2050.github.io/multimod/reference/multimod-constructors.md)
  [`Parameter()`](https://optimal2050.github.io/multimod/reference/multimod-constructors.md)
  [`Variable()`](https://optimal2050.github.io/multimod/reference/multimod-constructors.md)
  [`Equation()`](https://optimal2050.github.io/multimod/reference/multimod-constructors.md)
  [`SetRef()`](https://optimal2050.github.io/multimod/reference/multimod-constructors.md)
  [`MappingRef()`](https://optimal2050.github.io/multimod/reference/multimod-constructors.md)
  [`ParameterRef()`](https://optimal2050.github.io/multimod/reference/multimod-constructors.md)
  [`VariableRef()`](https://optimal2050.github.io/multimod/reference/multimod-constructors.md)
  [`Symbol()`](https://optimal2050.github.io/multimod/reference/multimod-constructors.md)
  [`Dims()`](https://optimal2050.github.io/multimod/reference/multimod-constructors.md)
  [`Constant()`](https://optimal2050.github.io/multimod/reference/multimod-constructors.md)
  [`Expression()`](https://optimal2050.github.io/multimod/reference/multimod-constructors.md)
  [`Unary()`](https://optimal2050.github.io/multimod/reference/multimod-constructors.md)
  [`When()`](https://optimal2050.github.io/multimod/reference/multimod-constructors.md)
  [`Func()`](https://optimal2050.github.io/multimod/reference/multimod-constructors.md)
  [`Sum()`](https://optimal2050.github.io/multimod/reference/multimod-constructors.md)
  [`Prod()`](https://optimal2050.github.io/multimod/reference/multimod-constructors.md)
  [`SetMin()`](https://optimal2050.github.io/multimod/reference/multimod-constructors.md)
  [`SetMax()`](https://optimal2050.github.io/multimod/reference/multimod-constructors.md)
  [`Shift()`](https://optimal2050.github.io/multimod/reference/multimod-constructors.md)
  [`Call()`](https://optimal2050.github.io/multimod/reference/multimod-constructors.md)
  [`Where()`](https://optimal2050.github.io/multimod/reference/multimod-constructors.md)
  : Constructors under the cross-language names
- [`is_declaration()`](https://optimal2050.github.io/multimod/reference/is_declaration.md)
  : Is this node a declaration rather than a reference?

## GAMS

Read GAMS, and render a model or any node back to it.

- [`read_gams()`](https://optimal2050.github.io/multimod/reference/read_gams.md)
  : Read GAMS model file and parse its core structure.
- [`parse_equation_header()`](https://optimal2050.github.io/multimod/reference/parse_equation_header.md)
  : Parse a GAMS equation header
- [`parse_gams_equation()`](https://optimal2050.github.io/multimod/reference/parse_gams_equation.md)
  : Parse a GAMS equation into an AST object.
- [`remove_ontext_offtext()`](https://optimal2050.github.io/multimod/reference/remove_ontext_offtext.md)
  : Strip GAMS block comments
- [`is_gams_expression()`](https://optimal2050.github.io/multimod/reference/is_gams_expression.md)
  : Check if a string is a compound expression
- [`as_gams()`](https://optimal2050.github.io/multimod/reference/as_gams.md)
  : Convert multimod object to GAMS syntax
- [`write_gams(`*`<mm_model>`*`)`](https://optimal2050.github.io/multimod/reference/write_gams.mm_model.md)
  : Write a full GAMS model file from a multimod object
- [`write_gams()`](https://optimal2050.github.io/multimod/reference/write_gams.md)
  : Write a GAMS model file from a multimod object
- [`write_gams_data()`](https://optimal2050.github.io/multimod/reference/write_gams_data.md)
  : Write a model's data to a GDX for GAMS

## GMPL / MathProg

Read MathProg, render it, and solve with the glpsol executable.

- [`read_gmpl()`](https://optimal2050.github.io/multimod/reference/read_gmpl.md)
  : Read GMPL model file
- [`read_gmpl_data()`](https://optimal2050.github.io/multimod/reference/read_gmpl_data.md)
  : Read GMPL data file and parse its contents
- [`read_gmpl_declarations()`](https://optimal2050.github.io/multimod/reference/read_gmpl_declarations.md)
  : Read GMPL declarations sequentially
- [`as_gmpl()`](https://optimal2050.github.io/multimod/reference/as_gmpl.md)
  : Convert multimod object to GMPL/MathProg syntax
- [`write_gmpl()`](https://optimal2050.github.io/multimod/reference/write_gmpl.md)
  : Write a GMPL/MathProg model file from a multimod object
- [`write_gmpl_data()`](https://optimal2050.github.io/multimod/reference/write_gmpl_data.md)
  : Write GMPL data file from multimod model
- [`solve_gmpl()`](https://optimal2050.github.io/multimod/reference/solve_gmpl.md)
  : Solve GMPL model using glpsol command line tool
- [`solve_glpk()`](https://optimal2050.github.io/multimod/reference/solve_glpk.md)
  : Solve GLPK model using glpsol command line tool
- [`read_csv_results()`](https://optimal2050.github.io/multimod/reference/read_csv_results.md)
  : Read CSV results from GMPL solver solution directory and save to
  model variables
- [`export_data_gmpl()`](https://optimal2050.github.io/multimod/reference/export_data_gmpl.md)
  : Generate GMPL code to export all sets and parameters to CSV files
- [`import_gmpl_data()`](https://optimal2050.github.io/multimod/reference/import_gmpl_data.md)
  : Import GMPL Data to Model

## JuMP / Julia

- [`as_jump()`](https://optimal2050.github.io/multimod/reference/as_jump.md)
  : Convert multimod object to Julia/JuMP syntax
- [`write_jump()`](https://optimal2050.github.io/multimod/reference/write_jump.md)
  : Write a Julia/JuMP model file from a multimod object
- [`write_jump_solution_code()`](https://optimal2050.github.io/multimod/reference/write_jump_solution_code.md)
  : Julia source of the JuMP solution writer
- [`solve_jump()`](https://optimal2050.github.io/multimod/reference/solve_jump.md)
  : Solve a JuMP Model
- [`parse_jump_equation()`](https://optimal2050.github.io/multimod/reference/parse_jump_equation.md)
  : Parse one JuMP constraint into a multimod equation

## Pyomo / Python

- [`as_pyomo()`](https://optimal2050.github.io/multimod/reference/as_pyomo.md)
  : Convert a multimod AST node to Pyomo syntax

- [`write_pyomo()`](https://optimal2050.github.io/multimod/reference/write_pyomo.md)
  : Write Pyomo model artifacts

- [`solve_pyomo()`](https://optimal2050.github.io/multimod/reference/solve_pyomo.md)
  :

  Solve a Pyomo model exported by `multimod`

## LaTeX

Publication-ready rendering of equations and whole models.

- [`as_latex()`](https://optimal2050.github.io/multimod/reference/as_latex.md)
  : Convert objects to LaTeX format
- [`write_latex(`*`<mm_equation>`*`)`](https://optimal2050.github.io/multimod/reference/write_latex.mm_equation.md)
  : Write LaTeX representation of an equation
- [`write_latex(`*`<mm_model>`*`)`](https://optimal2050.github.io/multimod/reference/write_latex.mm_model.md)
  : Write LaTeX representation of a model
- [`write_latex()`](https://optimal2050.github.io/multimod/reference/write_latex.md)
  : Write LaTeX representation of an equation or model
- [`format_latex_aligned()`](https://optimal2050.github.io/multimod/reference/format_latex_aligned.md)
  : Format a LaTeX equation across multiple lines using aligned
- [`estimate_latex_length()`](https://optimal2050.github.io/multimod/reference/estimate_latex_length.md)
  : Estimate the length of a LaTeX string
- [`latex_math_brakets()`](https://optimal2050.github.io/multimod/reference/latex_math_brakets.md)
  : Generate LaTeX brackets for math expressions
- [`latex_top_level_operators()`](https://optimal2050.github.io/multimod/reference/latex_top_level_operators.md)
  : Identify top-level LaTeX operators in an expression

## Folding

Drop dimensions a parameter does not vary over.

- [`fold_model()`](https://optimal2050.github.io/multimod/reference/fold_model.md)
  : Fold model dimensions to reduce data size
- [`create_fold_spec()`](https://optimal2050.github.io/multimod/reference/create_fold_spec.md)
  : Create a fold specification for a model's parameters
- [`analyze_parameter_redundancy()`](https://optimal2050.github.io/multimod/reference/analyze_parameter_redundancy.md)
  : Analyze parameter redundancy in model
- [`analyze_dimension_redundancy()`](https://optimal2050.github.io/multimod/reference/analyze_dimension_redundancy.md)
  : Analyze if a dimension can be safely folded
- [`get_fold_summary()`](https://optimal2050.github.io/multimod/reference/get_fold_summary.md)
  : Get folding statistics summary
- [`is_folded()`](https://optimal2050.github.io/multimod/reference/is_folded.md)
  : Check if a parameter or model is folded
- [`get_active_dims()`](https://optimal2050.github.io/multimod/reference/get_active_dims.md)
  : Get active dimensions from a parameter
- [`unfold_model()`](https://optimal2050.github.io/multimod/reference/unfold_model.md)
  : Unfold model to restore original dimensions

## Trimming

Drop symbols the model does not use.

- [`trim_model()`](https://optimal2050.github.io/multimod/reference/trim_model.md)
  : Trim a model to the elements its data actually uses
- [`untrim_model()`](https://optimal2050.github.io/multimod/reference/untrim_model.md)
  : Untrim model or specific elements
- [`get_trim_summary()`](https://optimal2050.github.io/multimod/reference/get_trim_summary.md)
  : Get trim summary statistics
- [`is_trimmed()`](https://optimal2050.github.io/multimod/reference/is_trimmed.md)
  : Check if model or object is trimmed

## Direct matrix and MPS backend

Assemble the LP from the AST, write MPS, solve in process.

- [`model_to_lp()`](https://optimal2050.github.io/multimod/reference/model_to_lp.md)
  : Assemble a model into LP arrays

- [`build_col_index()`](https://optimal2050.github.io/multimod/reference/build_col_index.md)
  : Build the LP column index

- [`build_row_index()`](https://optimal2050.github.io/multimod/reference/build_row_index.md)
  : Build the LP row index

- [`build_triplets()`](https://optimal2050.github.io/multimod/reference/build_triplets.md)
  : Extract (row, column, coefficient) triplets from a model

- [`check_matrix_numbers()`](https://optimal2050.github.io/multimod/reference/check_matrix_numbers.md)
  : Report the magnitude spectrum of an assembled LP

- [`mm_session()`](https://optimal2050.github.io/multimod/reference/mm_session.md)
  : Create a solver session

- [`solve_highs()`](https://optimal2050.github.io/multimod/reference/solve_highs.md)
  : Solve a model in-process with HiGHS

- [`solve_mps()`](https://optimal2050.github.io/multimod/reference/solve_mps.md)
  : Solve a written MPS file locally and emit a HiGHS solution file

- [`write_mps()`](https://optimal2050.github.io/multimod/reference/write_mps.md)
  : Write a model to MPS, with the index tables needed to read results
  back

- [`read_mps_solution()`](https://optimal2050.github.io/multimod/reference/read_mps_solution.md)
  : Map a solved MPS back onto model symbols

- [`read_highs_solution()`](https://optimal2050.github.io/multimod/reference/read_highs_solution.md)
  : Parse a HiGHS solution file

- [`read_cuopt_solution()`](https://optimal2050.github.io/multimod/reference/read_cuopt_solution.md)
  : Parse a cuOpt solution file

- [`read_solver_solution()`](https://optimal2050.github.io/multimod/reference/read_solver_solution.md)
  : Read a solution file, whichever solver wrote it

- [`write_solution()`](https://optimal2050.github.io/multimod/reference/write_solution.md)
  : Write a solution as one table per variable

- [`write_energyrt_output()`](https://optimal2050.github.io/multimod/reference/write_energyrt_output.md)
  :

  Write a solution in energyRt's `output/` layout

## Model workspace and data management

Save, load and update a model and its data.

- [`save_model()`](https://optimal2050.github.io/multimod/reference/save_model.md)
  : Save model data on disk
- [`load_model()`](https://optimal2050.github.io/multimod/reference/load_model.md)
  : Load multimod model from disk
- [`get_data()`](https://optimal2050.github.io/multimod/reference/get_data.md)
  : Get data from model (with optional lazy loading)
- [`update_parameter()`](https://optimal2050.github.io/multimod/reference/update_parameter.md)
  : Update parameter data
- [`update_mapping()`](https://optimal2050.github.io/multimod/reference/update_mapping.md)
  : Update mapping data
- [`save_parameter()`](https://optimal2050.github.io/multimod/reference/save_parameter.md)
  : Save parameter data to disk and free memory
- [`export_model_source()`](https://optimal2050.github.io/multimod/reference/export_model_source.md)
  : Export the bundled energyRt model as a solver workspace
- [`add_set_data()`](https://optimal2050.github.io/multimod/reference/add_set_data.md)
  : Add data to a set

## Configuration and environment

Where the interpreters and solvers are found.

- [`set_multimod_python()`](https://optimal2050.github.io/multimod/reference/multimod-config.md)
  [`get_multimod_python()`](https://optimal2050.github.io/multimod/reference/multimod-config.md)
  [`set_multimod_julia()`](https://optimal2050.github.io/multimod/reference/multimod-config.md)
  [`get_multimod_julia()`](https://optimal2050.github.io/multimod/reference/multimod-config.md)
  [`set_multimod_glpsol()`](https://optimal2050.github.io/multimod/reference/multimod-config.md)
  [`get_multimod_glpsol()`](https://optimal2050.github.io/multimod/reference/multimod-config.md)
  [`set_multimod_solver()`](https://optimal2050.github.io/multimod/reference/multimod-config.md)
  [`get_multimod_solver()`](https://optimal2050.github.io/multimod/reference/multimod-config.md)
  [`multimod_config_write()`](https://optimal2050.github.io/multimod/reference/multimod-config.md)
  [`multimod_config_read()`](https://optimal2050.github.io/multimod/reference/multimod-config.md)
  [`multimod_config_path()`](https://optimal2050.github.io/multimod/reference/multimod-config.md)
  [`multimod_config_show()`](https://optimal2050.github.io/multimod/reference/multimod-config.md)
  : Configuration management for multimod solver paths
- [`mm_setup_python()`](https://optimal2050.github.io/multimod/reference/mm_setup_python.md)
  : Set up the Python environment for multimod
- [`mm_check_python()`](https://optimal2050.github.io/multimod/reference/mm_check_python.md)
  : Report on the Python environment multimod would use

## Data and examples

- [`example_models`](https://optimal2050.github.io/multimod/reference/example_models.md)
  : Example Model in Multiple Formats

## Utility functions

Helpers used across the package.

- [`validate()`](https://optimal2050.github.io/multimod/reference/validate.md)
  : Validate multimod or AST objects
- [`make_valid()`](https://optimal2050.github.io/multimod/reference/make_valid.md)
  : Make a multimod model valid by reconciling symbols
- [`get_data_path()`](https://optimal2050.github.io/multimod/reference/get_data_path.md)
  : Get data path for a parameter or mapping
- [`get_index_alias()`](https://optimal2050.github.io/multimod/reference/get_index_alias.md)
  : Get index alias for a set
- [`get_lazy_data()`](https://optimal2050.github.io/multimod/reference/get_lazy_data.md)
  : Get lazy data with automatic loading
- [`set_data_path()`](https://optimal2050.github.io/multimod/reference/set_data_path.md)
  : Set data path for a parameter or mapping
- [`is_in_memory()`](https://optimal2050.github.io/multimod/reference/is_in_memory.md)
  : Data management utilities for multimod models
- [`is_special()`](https://optimal2050.github.io/multimod/reference/is_special.md)
  : Check if a character is a special character
- [`is_trimmed_any()`](https://optimal2050.github.io/multimod/reference/is_trimmed_any.md)
  : Check if any elements are actually trimmed
- [`is_word()`](https://optimal2050.github.io/multimod/reference/is_word.md)
  : Check if a string is a word (alphabetic characters only)
- [`is_word_num()`](https://optimal2050.github.io/multimod/reference/is_word_num.md)
  : Check if a string is alphanumeric (letters, digits, or underscores)
- [`validate_fold()`](https://optimal2050.github.io/multimod/reference/validate_fold.md)
  : Validate fold integrity
- [`validate_index_aliases()`](https://optimal2050.github.io/multimod/reference/validate_index_aliases.md)
  : Validate index_aliases for a model
- [`extract_args(`*`<ast_formula>`*`)`](https://optimal2050.github.io/multimod/reference/extract_args.ast_formula.md)
  : Extract Arguments from ast_formula
- [`extract_args()`](https://optimal2050.github.io/multimod/reference/extract_args.md)
  : Extract Arguments from AST
- [`extract_ast_elements()`](https://optimal2050.github.io/multimod/reference/extract_ast_elements.md)
  : Recursively extract elements by name from a nested structure
- [`alias_ast_names()`](https://optimal2050.github.io/multimod/reference/alias_ast_names.md)
  : Apply name aliases to AST nodes
- [`depth()`](https://optimal2050.github.io/multimod/reference/depth.md)
  : Get the depth of a nested list structure (AST, multimod, and other
  objects)
- [`brackets_pair()`](https://optimal2050.github.io/multimod/reference/brackets_pair.md)
  : Get a pair of brackets
- [`wrap_brackets()`](https://optimal2050.github.io/multimod/reference/wrap_brackets.md)
  : Wrap text with brackets
- [`lp_presolve()`](https://optimal2050.github.io/multimod/reference/lp_presolve.md)
  : Presolve an assembled LP
- [`map_ast()`](https://optimal2050.github.io/multimod/reference/map_ast.md)
  : Recursively apply a function to all nodes of an AST
- [`node_hash()`](https://optimal2050.github.io/multimod/reference/node_hash.md)
  : Generate a stable hash for an AST or multimod object (excluding
  internal hash fields)
- [`node_type()`](https://optimal2050.github.io/multimod/reference/node_type.md)
  : Get the type of an AST or multimod node
- [`remap_ast_elements()`](https://optimal2050.github.io/multimod/reference/remap_ast_elements.md)
  : Replace AST elements with "ast_where" nodes
- [`split_at_top_level_operators()`](https://optimal2050.github.io/multimod/reference/split_at_top_level_operators.md)
  : Split LaTeX math string at top-level operators
- [`split_top_level_args()`](https://optimal2050.github.io/multimod/reference/split_top_level_args.md)
  : Split function arguments at the top level
- [`build_symbols_list()`](https://optimal2050.github.io/multimod/reference/build_symbols_list.md)
  : Build a list of symbols from model_info

## Experimental

Exported but untested; interfaces may change or be withdrawn.

- [`as_r()`](https://optimal2050.github.io/multimod/reference/as_r.md) :
  Convert AST to R Code
- [`as_r(`*`<ast_formula>`*`)`](https://optimal2050.github.io/multimod/reference/as_r.ast_formula.md)
  : Convert ast_formula to R Code
- [`as_rfunction()`](https://optimal2050.github.io/multimod/reference/as_rfunction.md)
  : Convert AST to R Function
- [`collect_ast_classes()`](https://optimal2050.github.io/multimod/reference/collect_ast_classes.md)
  : Collect all AST node classes from model or AST
- [`compare_ast()`](https://optimal2050.github.io/multimod/reference/compare_ast.md)
  : Compare two AST objects and report differences
- [`compare_ast_simple()`](https://optimal2050.github.io/multimod/reference/compare_ast_simple.md)
  : Compare AST objects using compare package with custom handling
- [`compare_csv_files()`](https://optimal2050.github.io/multimod/reference/compare_csv_files.md)
  : Compare CSV files in two directories
- [`compare_dataframes()`](https://optimal2050.github.io/multimod/reference/compare_dataframes.md)
  : Compare two dataframes and identify differences
- [`compare_jump_stats()`](https://optimal2050.github.io/multimod/reference/compare_jump_stats.md)
  : Compare JuMP model constraint statistics between two model
  directories
- [`compare_solution()`](https://optimal2050.github.io/multimod/reference/compare_solution.md)
  : Compare solutions from different solver outputs
- [`compare_variables()`](https://optimal2050.github.io/multimod/reference/compare_variables.md)
  : Compare variable solutions between two models
- [`quick_compare()`](https://optimal2050.github.io/multimod/reference/quick_compare.md)
  : Quick comparison showing first few differences
- [`get_network_data()`](https://optimal2050.github.io/multimod/reference/get_network_data.md)
  : Get network data for a multimod equation
- [`as_visNetwork()`](https://optimal2050.github.io/multimod/reference/as_visNetwork.md)
  : Convert a multimod ast or equation to a visNetwork object
- [`evaluate_parameters()`](https://optimal2050.github.io/multimod/reference/evaluate_parameters.md)
  : Evaluate Parameter Formulas and Defaults
- [`analyze_fold_opportunities()`](https://optimal2050.github.io/multimod/reference/analyze_fold_opportunities.md)
  : Analyze fold opportunities without applying
- [`has_ast_formulas()`](https://optimal2050.github.io/multimod/reference/has_ast_formulas.md)
  : Check if model has unevaluated AST formulas or defaults
- [`load_all_mappings()`](https://optimal2050.github.io/multimod/reference/load_all_mappings.md)
  : Load all mappings into memory
- [`load_all_parameters()`](https://optimal2050.github.io/multimod/reference/load_all_parameters.md)
  : Load all parameters into memory
- [`load_mapping()`](https://optimal2050.github.io/multimod/reference/load_mapping.md)
  : Load mapping data into memory
- [`load_parameter()`](https://optimal2050.github.io/multimod/reference/load_parameter.md)
  : Load parameter data into memory
- [`save_all_mappings()`](https://optimal2050.github.io/multimod/reference/save_all_mappings.md)
  : Save all mappings to disk
- [`save_all_parameters()`](https://optimal2050.github.io/multimod/reference/save_all_parameters.md)
  : Save all parameters to disk
- [`save_mapping()`](https://optimal2050.github.io/multimod/reference/save_mapping.md)
  : Save mapping data to disk and free memory
- [`log_solution_load()`](https://optimal2050.github.io/multimod/reference/log_solution_load.md)
  : Log solution loading from solver to variables directory
- [`restore_ast()`](https://optimal2050.github.io/multimod/reference/restore_ast.md)
  : Restore AST from preserved copies
- [`populate_sets_from_data()`](https://optimal2050.github.io/multimod/reference/populate_sets_from_data.md)
  : Populate sets from model's own mappings and parameters
- [`register_index_alias()`](https://optimal2050.github.io/multimod/reference/register_index_alias.md)
  : Register an index alias for a set
