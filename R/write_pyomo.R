#' Write Pyomo model artifacts
#'
#' This mirrors `write_jump()` but targets Python/Pyomo. The function emits a
#' `model.py` file containing component declarations, a `data_loader.py` helper
#' that knows how to stream data from the saved model directory (or use
#' embedded literals), and a lightweight `solve.py` runner.
#'
#' @inheritParams write_jump
#' @param solver Python solver name passed to `pyomo.opt.SolverFactory`
#'   (default `"highs"`).
#' @param data_mode Whether data should be stored externally in files
#'   (`"external"`, default) or embedded directly inside the generated
#'   Python code (`"embedded"`).
#' @param split Logical flag controlling whether solver execution should live
#'   in a separate `solve.py` (when `TRUE`) or be embedded directly in
#'   `model.py` (default). The embedded mode mirrors the two-file workflow used
#'   by the other writers (`model` + `data`). Set `split = TRUE` to preserve the
#'   previous three-file scaffold.
#' @export
write_pyomo <- function(model, file = NULL, model_dir = NULL, solver_dir = "pyomo",
                        model_name = "model", solver = "highs",
                        use_folded = TRUE, cleanup = TRUE,
                        data_mode = c("external", "embedded"),
                        split = FALSE, export_vars = FALSE, export_data = FALSE,
                        export_mps = FALSE, ...) {
  UseMethod("write_pyomo", model)
}

#' @export
write_pyomo.model <- function(model, file = NULL, model_dir = NULL, solver_dir = "pyomo",
                              model_name = "model", solver = "highs",
                              use_folded = TRUE, cleanup = TRUE,
                              data_mode = c("external", "embedded"),
                              split = FALSE, export_vars = FALSE, export_data = FALSE,
                              export_mps = FALSE, ...) {
  write_pyomo_internal(model, file = file, model_dir = model_dir, solver_dir = solver_dir,
                       model_name = model_name, solver = solver, use_folded = use_folded,
                       cleanup = cleanup, data_mode = data_mode, split = split,
                       export_vars = export_vars, export_data = export_data,
                       export_mps = export_mps, ...)
}

#' @export
write_pyomo.multimod <- function(model, ...) {
  write_pyomo.model(model, ...)
}

#' @export
write_pyomo.model_structure <- function(model, ...) {
  stopifnot(inherits(model, "model_structure"))
  write_pyomo_internal(model, ...)
}

write_pyomo_internal <- function(model, file = NULL, model_dir = NULL, solver_dir = "pyomo",
                                 model_name = "model", solver = "highs",
                                 use_folded = TRUE, cleanup = TRUE,
                                 data_mode = c("external", "embedded"),
                                 split = FALSE, export_vars = FALSE, export_data = FALSE,
                                 export_mps = FALSE, ...) {
  data_mode <- match.arg(data_mode)

  if (!is.null(file)) {
    warning("'file' argument is ignored for write_pyomo(); the Pyomo scaffold is a directory, not a single file.")
  }

  if (is.null(model_dir)) {
    model_dir <- model$base_path
  }
  if (is.null(model_dir) || !dir.exists(model_dir)) {
    stop("model_dir must reference a directory produced by save_model().")
  }

  solver_path <- file.path(model_dir, "solvers", solver_dir)
  if (cleanup && dir.exists(solver_path)) {
    unlink(solver_path, recursive = TRUE)
  }
  dir.create(solver_path, recursive = TRUE, showWarnings = FALSE)

  model_lines <- build_pyomo_model_lines(
    model,
    model_name = model_name,
    use_folded = use_folded,
    embed_runner = !isTRUE(split),
    runner_solver = solver,
    data_mode = data_mode,
    solver_dir = solver_dir,
    export_vars = export_vars,
    export_data = export_data,
    export_mps = export_mps
  )
  writeLines(model_lines, file.path(solver_path, "model.py"))

  loader_lines <- build_pyomo_loader_lines(model, data_mode = data_mode, use_folded = use_folded)
  writeLines(loader_lines, file.path(solver_path, "data.py"))

  if (isTRUE(split)) {
    solve_lines <- build_pyomo_solve_lines(solver = solver, data_mode = data_mode, solver_dir = solver_dir)
    writeLines(solve_lines, file.path(solver_path, "solve.py"))
  }

  invisible(file.path(solver_path, "model.py"))
}

build_pyomo_model_lines <- function(model, model_name = "model", use_folded = TRUE,
                                    embed_runner = FALSE, runner_solver = "highs",
                                    data_mode = c("external", "embedded"),
                                    solver_dir = "pyomo", export_vars = FALSE,
                                    export_data = FALSE, export_mps = FALSE) {
  data_mode <- match.arg(data_mode)
  header <- c(
    "# Auto-generated Pyomo model",
    sprintf("# Source: %s", model$name %||% "(unnamed)"),
    "from pyomo.environ import *"
  )
  if (embed_runner) {
    header <- c(header,
      "import json",
      "import os",
      "import csv",
      "from pathlib import Path",
      "from pyomo.opt import SolverFactory",
      "from data import DATA_MODE, build_dataportal"
    )
  }
  lines <- c(header, "", sprintf("%s = AbstractModel()", model_name), "")

  lines <- c(lines, pyomo_render_sets(model))
  lines <- c(lines, pyomo_render_mappings(model))
  lines <- c(lines, pyomo_render_parameters(model))
  lines <- c(lines, pyomo_render_variables(model))
  lines <- c(lines, pyomo_render_objective(model, model_name, use_folded = use_folded))
  lines <- c(lines, pyomo_render_constraints(model, use_folded = use_folded))
  if (embed_runner) {
    lines <- c(lines, pyomo_runner_body(solver = runner_solver, solver_dir = solver_dir,
                                        export_vars = export_vars, export_data = export_data,
                                        export_mps = export_mps))
  }
  lines
}

pyomo_render_sets <- function(model) {
  if (is.null(model$sets) || length(model$sets) == 0) return(character(0))
  lines <- c("# Sets")
  for (set_name in names(model$sets)) {
    set_obj <- model$sets[[set_name]]
    if (isTRUE(set_obj$trimmed)) next
    desc <- sanitize_description(set_obj$desc, set_name)
    lines <- c(lines, sprintf("model.%s = Set(doc=%s)", set_name, pyomo_quote(desc %||% set_name)))
  }
  c(lines, "")
}

pyomo_render_mappings <- function(model) {
  if (is.null(model$mappings) || length(model$mappings) == 0) return(character(0))
  lines <- c("# Mappings (sets of tuples)")
  for (mapping_name in names(model$mappings)) {
    mapping <- model$mappings[[mapping_name]]
    if (isTRUE(mapping$trimmed)) next
    dims <- pyomo_collect_dim_names(mapping$dims)
    desc <- sanitize_description(mapping$desc, mapping_name)
    dimen <- max(length(dims), 1)
    lines <- c(lines, sprintf("model.%s = Set(dimen=%s, doc=%s)", mapping_name, dimen, pyomo_quote(desc %||% mapping_name)))
  }
  c(lines, "")
}

pyomo_param_default <- function(param, model = NULL) {
  if (!is.null(param$defVal)) {
    if (inherits(param$defVal, "ast_formula")) {
      # defVal is a formula like DiscountRate[r] - convert to string representation
      # The formula will be evaluated at runtime by Pyomo's initialize mechanism
      # For now, return 0 as default and rely on data loading
      return(0)
    }
    return(param$defVal)
  }
  0
}

pyomo_render_parameters <- function(model) {
  if (is.null(model$parameters) || length(model$parameters) == 0) return(character(0))
  lines <- c("# Parameters")
  for (param_name in names(model$parameters)) {
    param <- model$parameters[[param_name]]
    if (isTRUE(param$trimmed)) next
    dims <- pyomo_collect_dim_names(param$active_dims)
    if (length(dims) == 0) dims <- pyomo_collect_dim_names(param$dims)
    desc <- sanitize_description(param$desc, param_name)
  default_val <- pyomo_param_default(param, model = model)
  default_lit <- if (is.numeric(default_val)) pyomo_format_scalar(default_val) else pyomo_quote(default_val)
    domain <- if (isTRUE(param$symbolic)) "Any" else "Reals"
    if (length(dims) == 0) {
      lines <- c(lines, sprintf("model.%s = Param(default=%s, domain=%s, doc=%s)",
                                 param_name, default_lit, domain, pyomo_quote(desc %||% param_name)))
    } else {
      base_sets <- vapply(dims, function(nm) pyomo_resolve_base_set(nm, model = model), character(1))
      index_sets <- paste(sprintf("model.%s", base_sets), collapse = ", ")
      lines <- c(lines, sprintf("model.%s = Param(%s, default=%s, domain=%s, doc=%s)",
                                 param_name, index_sets, default_lit, domain, pyomo_quote(desc %||% param_name)))
    }
  }
  c(lines, "")
}

pyomo_render_variables <- function(model) {
  if (is.null(model$variables) || length(model$variables) == 0) return(character(0))
  lines <- c("# Decision variables")
  for (var_name in names(model$variables)) {
    var <- model$variables[[var_name]]
    if (isTRUE(var$trimmed)) next
    dims <- pyomo_collect_dim_names(var$dims)
    desc <- sanitize_description(var$desc, var_name)

    within <- "Reals"
    if (!is.null(var$vtype)) {
      vt <- tolower(var$vtype)
      if (vt %in% c("binary", "bin")) within <- "Binary"
      else if (vt %in% c("integer", "int")) within <- "Integers"
      else if (vt %in% c("positive")) within <- "NonNegativeReals"
    }

    bounds <- var$bounds %||% list()
    lower <- bounds$lo %||% bounds$lb %||% var$lb
    upper <- bounds$up %||% bounds$ub %||% var$ub
    format_bound <- function(bnd, default) {
      if (is.null(bnd) || !is.finite(bnd)) return(default)
      format(bnd, scientific = FALSE, trim = TRUE)
    }
    lower_lit <- format_bound(lower, "None")
    upper_lit <- format_bound(upper, "None")

    if (!is.null(var$domain) && length(var$domain) == 1 && nzchar(var$domain)) {
      index_sets <- sprintf("model.%s", var$domain)
    } else if (length(dims) > 0) {
      base_sets <- vapply(dims, function(nm) pyomo_resolve_base_set(nm, model = model), character(1))
      index_sets <- paste(sprintf("model.%s", base_sets), collapse = ", ")
    } else {
      index_sets <- NULL
    }

    decl <- sprintf("model.%s = Var(%swithin=%s, bounds=(%s, %s))",
                    var_name,
                    if (!is.null(index_sets)) paste0(index_sets, ", ") else "",
                    within,
                    lower_lit,
                    upper_lit)
    if (!is.null(desc) && nzchar(desc)) {
      decl <- paste0(decl, sprintf("  # %s", desc))
    }
    lines <- c(lines, decl)
  }
  c(lines, "")
}

pyomo_render_objective <- function(model, model_name, use_folded = TRUE) {
  if (!is.null(model$objectives) && length(model$objectives) > 0) {
    obj <- model$objectives[[1]]
    eq_name <- obj$equation
    eq_source <- NULL
    if (!is.null(eq_name)) {
      eq_bank <- if (use_folded && !is.null(model$folded_equations)) model$folded_equations else model$equations
      eq_source <- eq_bank[[eq_name]]
    }
    if (!is.null(eq_source)) {
      expr <- as_pyomo(eq_source$rhs, model = model)
  sense <- if (tolower(obj$sense %||% "min") %in% c("min", "minimize")) "minimize" else "maximize"
  return(c("# Objective", sprintf("model.objective = Objective(rule=lambda model: %s, sense=%s)", expr, sense), ""))
    }
  }
  c("# Objective", "model.objective = Objective(rule=lambda model: model.vObjective, sense=minimize)", "")
}

pyomo_render_constraints <- function(model, use_folded = TRUE) {
  eqs <- if (use_folded && !is.null(model$folded_equations) && length(model$folded_equations) > 0) {
    model$folded_equations
  } else {
    model$equations
  }
  if (is.null(eqs) || length(eqs) == 0) return(character(0))
  
  # Collect equation names used as objectives
  objective_eqs <- character(0)
  if (!is.null(model$objectives)) {
    for (obj in model$objectives) {
      if (!is.null(obj$equation)) {
        objective_eqs <- c(objective_eqs, obj$equation)
      }
    }
  }
  
  lines <- c("# Constraints")
  for (eq_name in names(eqs)) {
    eq <- eqs[[eq_name]]
    if (isTRUE(eq$trimmed)) next
    # Skip equations that are used as objectives
    if (eq_name %in% objective_eqs) next
    block <- render_pyomo_equation(eq, model = model)
    lines <- c(lines, block$rule, block$assignment, "")
  }
  lines
}

build_pyomo_loader_lines <- function(model, data_mode = c("external", "embedded"), use_folded = TRUE) {
  data_mode <- match.arg(data_mode)
  if (data_mode == "embedded") {
    return(build_pyomo_loader_embedded(model, use_folded = use_folded))
  }
  build_pyomo_loader_external(model, use_folded = use_folded)
}

build_pyomo_loader_external <- function(model, use_folded = TRUE) {
  set_names <- names(model$sets %||% list())
  map_names <- names(model$mappings %||% list())
  param_specs <- lapply(names(model$parameters %||% list()), function(param_name) {
    param <- model$parameters[[param_name]]
    list(
      name = param_name,
      dims = pyomo_collect_dim_names(param$active_dims %||% param$dims),
      folded = use_folded && !is.null(param$folded_data),
      trimmed = isTRUE(param$trimmed)
    )
  })

  lines <- c(
    "from __future__ import annotations",
    "from pathlib import Path",
    "from typing import Iterable, Sequence",
    "from pyomo.dataportal import DataPortal",
    "from pyomo.environ import *",
    "",
    "DATA_MODE = 'external'",
    "",
    "def _read_format(base_dir: Path) -> str:",
    "    fmt_file = base_dir / 'format.txt'",
    "    if fmt_file.exists():",
    "        text = fmt_file.read_text().strip().lower()",
    "        if text:",
    "            return text",
    "    return 'csv'",
    "",
    "def _extension(fmt: str) -> str:",
    "    if fmt in ('ipc', 'arrow', 'feather'):",
    "        return '.arrow'",
    "    if fmt == 'parquet':",
    "        return '.parquet'",
    "    return '.csv'",
    "",
    "def build_dataportal(model, base_dir: Path) -> DataPortal:",
    "    data = DataPortal()",
    "    data_format = _read_format(base_dir)",
    "    ext = _extension(data_format)",
    "    # Sets",
    paste0("    set_names = ", pyomo_python_list(set_names)),
    "    for name in set_names:",
    "        path = base_dir / 'sets' / name / f'data{ext}'",
    "        if path.exists():",
    "            data.load(filename=str(path), set=getattr(model, name))",
    "    # Mappings",
    paste0("    mappings = ", pyomo_python_list(map_names)),
    "    for name in mappings:",
    "        path = base_dir / 'mappings' / name / f'data{ext}'",
    "        if path.exists():",
    "            data.load(filename=str(path), set=getattr(model, name))",
    "    # Parameters"
  )

  for (spec in param_specs) {
    if (spec$trimmed) next
    dims <- spec$dims
    index_clause <- if (length(dims) > 0) sprintf(", index=%s", pyomo_python_tuple(dims)) else ""
    lines <- c(lines,
      sprintf("    param_dir = base_dir / 'parameters' / '%s'", spec$name),
      "    file_path = param_dir / f'data{ext}'"
    )
    if (spec$folded) {
      lines <- c(lines,
        "    folded_dir = param_dir / 'folded_data'",
        "    candidate = folded_dir / f'data{ext}'",
        "    if folded_dir.exists() and candidate.exists():",
        "        file_path = candidate"
      )
    }
    lines <- c(lines,
      "    if file_path.exists():",
      sprintf("        data.load(filename=str(file_path), param=getattr(model, '%s')%s)", spec$name, index_clause)
    )
  }

  lines <- c(lines, "    return data", "")
  lines
}

pyomo_python_list <- function(values) {
  if (is.null(values) || length(values) == 0) return("[]")
  paste0("[", paste(sprintf("'%s'", values), collapse = ", "), "]")
}

pyomo_python_tuple <- function(values) {
  if (is.null(values) || length(values) == 0) return("()")
  if (length(values) == 1) {
    return(sprintf("('%s',)", values))
  }
  paste0("(", paste(sprintf("'%s'", values), collapse = ", "), ")")
}

build_pyomo_loader_embedded <- function(model, use_folded = TRUE) {
  warning("Embedded data mode is not yet implemented for Pyomo; falling back to external loader.")
  build_pyomo_loader_external(model, use_folded = use_folded)
}

build_pyomo_solve_lines <- function(solver = "highs", data_mode = c("external", "embedded"),
                                    solver_dir = "pyomo") {
  data_mode <- match.arg(data_mode)
  lines <- c(
    "from __future__ import annotations",
    "import json",
    "import os",
    "from pathlib import Path",
    "from pyomo.environ import *",
    "from pyomo.opt import SolverFactory",
    "from model import model",
    "from data_loader import DATA_MODE, build_dataportal" ,
    "",
    pyomo_runner_body(solver = solver, solver_dir = solver_dir)
  )
  lines
}

pyomo_runner_body <- function(solver = "highs", solver_dir = "pyomo", export_vars = FALSE,
                              export_data = FALSE, export_mps = FALSE) {
  lines <- c(
    "def main():",
    "    base_dir = Path(__file__).resolve().parents[2]",
    "    if DATA_MODE != 'external':",
    "        raise RuntimeError('Only external data mode is supported right now.')",
    "    data = build_dataportal(model, base_dir)"
  )
  
  # Export data as CSV files after loading data but BEFORE creating instance (like write_gmpl and write_jump)
  # This allows checking exported data even if model creation fails
  if (isTRUE(export_data)) {
    lines <- c(lines,
      "",
      "    # ========== DATA EXPORT ==========",
      sprintf("    data_export_dir = base_dir / 'solvers' / '%s' / 'data_export'", solver_dir),
      "    data_export_dir.mkdir(parents=True, exist_ok=True)",
      "    print(f'Exporting model data to {data_export_dir}...')",
      "",
      "    # Export sets from data portal",
      "    for set_name in ['REGION', 'TECHNOLOGY', 'FUEL', 'EMISSION', 'MODE_OF_OPERATION', 'YEAR', 'TIMESLICE', 'SEASON', 'DAYTYPE', 'DAILYTIMEBRACKET', 'STORAGE']:",
      "        if hasattr(model, set_name):",
      "            set_file = data_export_dir / f'{set_name}.csv'",
      "            with open(set_file, 'w', newline='') as csvfile:",
      "                writer = csv.writer(csvfile)",
      "                writer.writerow([set_name])",
      "                try:",
      "                    data_dict = data.data()",
      "                    if set_name in data_dict and None in data_dict[set_name]:",
      "                        for item in data_dict[set_name][None]:",
      "                            writer.writerow([item])",
      "                except:",
      "                    pass  # Skip if data access fails",
      "",
      "    # Export parameters from data portal",
      "    for param_name in dir(model):",
      "        component = getattr(model, param_name, None)",
      "        if isinstance(component, Param) and not param_name.startswith('_'):",
      "            param_file = data_export_dir / f'{param_name}.csv'",
      "            with open(param_file, 'w', newline='') as csvfile:",
      "                writer = csv.writer(csvfile)",
      "                try:",
      "                    data_dict = data.data()",
      "                    if param_name in data_dict:",
      "                        param_data = data_dict[param_name]",
      "                        if component.dim() == 0:",
      "                            # Scalar parameter",
      "                            writer.writerow(['value'])",
      "                            if None in param_data:",
      "                                writer.writerow([param_data[None]])",
      "                        else:",
      "                            # Indexed parameter - get index set names from component",
      "                            index_set = component.index_set()",
      "                            if hasattr(index_set, 'subsets'):",
      "                                index_names = [s.name for s in index_set.subsets()]",
      "                            elif hasattr(index_set, 'name'):",
      "                                index_names = [index_set.name]",
      "                            else:",
      "                                index_names = [f'index{i}' for i in range(component.dim())]",
      "                            writer.writerow(index_names + ['value'])",
      "                            # Write data rows (all values)",
      "                            for key, val in param_data.items():",
      "                                if isinstance(key, tuple):",
      "                                    writer.writerow(list(key) + [val])",
      "                                else:",
      "                                    writer.writerow([key, val])",
      "                except Exception as e:",
      "                    pass  # Skip parameters that can't be exported",
      "    print('Data export complete.')",
      ""
    )
  }
  
  lines <- c(lines,
    "    instance = model.create_instance(data)"
  )
  
  # Export MPS after instance is created but before solving
  if (isTRUE(export_mps)) {
    lines <- c(lines,
      "",
      "    # Export MPS format",
      sprintf("    mps_file = base_dir / 'solvers' / '%s' / 'model.mps'", solver_dir),
      "    print(f'Exporting MPS format to: {mps_file}')",
      "    instance.write(str(mps_file), io_options={'symbolic_solver_labels': True})",
      "    # Post-process MPS file to remove OBJSENSE (not supported by GLPK)",
      "    with open(mps_file, 'r') as f:",
      "        lines = f.readlines()",
      "    with open(mps_file, 'w') as f:",
      "        skip_next = False",
      "        for line in lines:",
      "            if line.strip() == 'OBJSENSE':",
      "                skip_next = True",
      "                continue",
      "            if skip_next and line.strip() in ['MIN', 'MAX']:",
      "                skip_next = False",
      "                continue",
      "            f.write(line)"
    )
  }
  
  lines <- c(lines,
    "",
    "    # Solve the model",
    sprintf("    solver_name = os.environ.get('PYOMO_SOLVER', '%s')", solver),
    "    solver = SolverFactory(solver_name)",
    "    print(f'Solving with {solver_name}...')",
    "    try:",
    "        results = solver.solve(instance, tee=True, load_solutions=False)",
    "        # results.write()  # Commented out - produces verbose output",
    "        # Load solution if one exists",
    "        if results.solver.termination_condition != TerminationCondition.infeasible:",
    "            instance.solutions.load_from(results)",
    "    except Exception as e:",
    "        print(f'Solver error (continuing): {e}')",
    "        results = None",
    "",
    "    # Save solution status",
    sprintf("    sol_dir = base_dir / 'solvers' / '%s' / 'solution'", solver_dir),
    "    sol_dir.mkdir(parents=True, exist_ok=True)",
    "    with open(sol_dir / 'status.json', 'w') as fh:",
    "        json.dump({'solver': solver_name, 'status': str(results.solver.status), 'termination': str(results.solver.termination_condition)}, fh, indent=2)"
  )
  
  if (isTRUE(export_vars)) {
    lines <- c(lines,
      "",
      "    # Export variable values",
      "    with open(sol_dir / 'variables.json', 'w') as fh:",
      "        var_dict = {}",
      "        for v in instance.component_objects(Var, active=True):",
      "            var_dict[v.name] = {str(idx): value(v[idx]) for idx in v if v[idx].value is not None}",
      "        json.dump(var_dict, fh, indent=2)"
    )
  }
  
  c(lines,
    "",
    "if __name__ == '__main__':",
    "    main()",
    ""
  )
}
