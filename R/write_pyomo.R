#' Write Pyomo model artifacts
#'
#' This mirrors `write_jump()` but targets Pyomo. It writes `model.py`, holding
#' the component declarations and, by default, the solve runner, and `data.py`,
#' which reads the model's data from the workspace. With `split = TRUE` the
#' runner is written to a separate `solve.py`.
#'
#' @inheritParams write_jump
#' @param model_dir Model workspace directory. The scaffold is written to
#'   `<model_dir>/solvers/<solver_dir>/` and reads its data from the workspace,
#'   so the directory must hold one written by [save_model()]; if it does not,
#'   the model is saved there first (CSV format). Defaults to the directory the
#'   model was loaded from or saved to (`model$base_path`).
#' @param solver Python solver name passed to `pyomo.opt.SolverFactory`
#'   (default `"highs"`).
#' @param data_mode Whether data should be stored externally in files
#'   (`"external"`, default) or embedded directly inside the generated
#'   Python code (`"embedded"`).
#' @param split Logical flag controlling whether solver execution should live
#'   in a separate `solve.py` (when `TRUE`) or be embedded directly in
#'   `model.py` (default). The embedded mode mirrors the two-file workflow used
#'   by the other writers (`model` + `data`).
#' @return Invisibly, the path of the written `model.py`.
#' @export
write_pyomo <- function(model, model_dir = NULL, solver_dir = "pyomo",
                        model_name = "model", solver = "highs",
                        use_folded = TRUE, cleanup = TRUE,
                        data_mode = c("external", "embedded"),
                        split = FALSE, export_vars = FALSE, export_data = FALSE,
                        export_mps = FALSE, ...) {
  UseMethod("write_pyomo", model)
}

#' @export
write_pyomo.mm_model <- function(model, model_dir = NULL, solver_dir = "pyomo",
                              model_name = "model", solver = "highs",
                              use_folded = TRUE, cleanup = TRUE,
                              data_mode = c("external", "embedded"),
                              split = FALSE, export_vars = FALSE, export_data = FALSE,
                              export_mps = FALSE, ...) {
  write_pyomo_internal(model, model_dir = model_dir, solver_dir = solver_dir,
                       model_name = model_name, solver = solver, use_folded = use_folded,
                       cleanup = cleanup, data_mode = data_mode, split = split,
                       export_vars = export_vars, export_data = export_data,
                       export_mps = export_mps, ...)
}

#' @export
write_pyomo.multimod <- function(model, ...) {
  write_pyomo.mm_model(model, ...)
}

#' @export
write_pyomo.model_structure <- function(model, ...) {
  stopifnot(inherits(model, "model_structure"))
  write_pyomo_internal(model, ...)
}

write_pyomo_internal <- function(model, model_dir = NULL, solver_dir = "pyomo",
                                 model_name = "model", solver = "highs",
                                 use_folded = TRUE, cleanup = TRUE,
                                 data_mode = c("external", "embedded"),
                                 split = FALSE, export_vars = FALSE, export_data = FALSE,
                                 export_mps = FALSE, ...) {
  if (inherits(model, "mm_model") || inherits(model, "multimod")) model <- load_model_data(model)
  data_mode <- match.arg(data_mode)

  if (!is.null(list(...)$file)) {
    stop("write_pyomo() writes a directory scaffold, not a single file; ",
         "pass `model_dir` instead of `file`.")
  }

  if (is.null(model_dir)) {
    model_dir <- model$base_path
  }
  if (is.null(model_dir)) {
    stop("`model_dir` is required: the Pyomo scaffold reads its data from a ",
         "model workspace (see save_model()).")
  }
  # The generated code resolves the workspace from its own location
  # (`<model_dir>/solvers/<solver_dir>/model.py`), so the data must be there.
  .ensure_workspace(model, model_dir)

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
      "from data import DATA_MODE, build_data"
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
  subset_symbols <- dual_registered_symbols(model)
  for (mapping_name in names(model$mappings)) {
    mapping <- model$mappings[[mapping_name]]
    if (isTRUE(mapping$trimmed)) next

    # Declared once among the sets. Assigning model.<name> twice makes Pyomo
    # replace the component, keeping the empty second declaration.
    if (mapping_name %in% subset_symbols) next
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
    "def _read_rows(path: Path, ext: str):",
    "    # DataPortal has no reader for .arrow or .parquet, so each file is read",
    "    # here and handed to create_instance() as a plain data dictionary.",
    "    if ext == '.csv':",
    "        import csv as _csv",
    "        with open(path, newline='') as fh:",
    "            rdr = _csv.reader(fh)",
    "            next(rdr, None)",
    "            return [tuple(r) for r in rdr if r]",
    "    if ext == '.parquet':",
    "        import pyarrow.parquet as _pq",
    "        tbl = _pq.read_table(str(path))",
    "    else:",
    "        from pyarrow import feather as _feather",
    "        tbl = _feather.read_table(str(path))",
    "    cols = [c.to_pylist() for c in tbl.columns]",
    "    return list(zip(*cols))",
    "",
    "def _coerce(v):",
    "    # DataPortal turns a numeric-looking CSV field into a number, and the",
    "    # set members it builds are matched against parameter index entries, so",
    "    # both paths have to agree on type.",
    "    if isinstance(v, str):",
    "        try:",
    "            return int(v)",
    "        except ValueError:",
    "            try:",
    "                return float(v)",
    "            except ValueError:",
    "                return v",
    "    return v",
    "",
    "def _param_dict(rows):",
    "    # The file declares its own shape; a one-dimensional parameter takes a",
    "    # scalar key, not a one-tuple.",
    "    if not rows:",
    "        return {}",
    "    n_key = len(rows[0]) - 1",
    "    if n_key <= 0:",
    "        return {None: float(rows[0][-1])}",
    "    if n_key == 1:",
    "        return {_coerce(r[0]): float(r[-1]) for r in rows}",
    "    return {tuple(_coerce(v) for v in r[:n_key]): float(r[-1]) for r in rows}",
    "",
    "def build_data(model, base_dir: Path) -> dict:",
    "    ext = _extension(_read_format(base_dir))",
    "    out = {}",
    "    # Sets",
    paste0("    set_names = ", pyomo_python_list(set_names)),
    "    for name in set_names:",
    "        path = base_dir / 'sets' / name / f'data{ext}'",
    "        if path.exists():",
    "            rows = _read_rows(path, ext)",
    "            out[name] = {None: [_coerce(r[0]) for r in rows]}",
    "    # Mappings",
    paste0("    mappings = ", pyomo_python_list(map_names)),
    "    for name in mappings:",
    "        path = base_dir / 'mappings' / name / f'data{ext}'",
    "        if path.exists():",
    "            rows = _read_rows(path, ext)",
    "            if rows and len(rows[0]) == 1:",
    "                out[name] = {None: [_coerce(r[0]) for r in rows]}",
    "            else:",
    "                out[name] = {None: [tuple(_coerce(v) for v in r) for r in rows]}",
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
      sprintf("        out[%s] = _param_dict(_read_rows(file_path, ext))",
              pyomo_quote(spec$name))
    )
  }

  lines <- c(lines, "    return {None: out}", "")
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
    "from data import DATA_MODE, build_data",
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
    "    data = build_data(model, base_dir)"
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
      "    for set_name in dir(model):",
      "        component = getattr(model, set_name, None)",
      "        if isinstance(component, Set) and not set_name.startswith('_'):",
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
    "    try:",
    "        objective_value = value(instance.objective)",
    "    except Exception:",
    "        objective_value = None",
    "    with open(sol_dir / 'status.json', 'w') as fh:",
    "        json.dump({'solver': solver_name, 'status': str(results.solver.status), 'termination': str(results.solver.termination_condition), 'objective': objective_value}, fh, indent=2)"
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
