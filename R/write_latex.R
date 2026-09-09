# Default LaTeX preamble and ending
default_preamble <- c(
  "\\documentclass{article}",
  # "\\usepackage[a4paper,margin=1in]{geometry}",
  "\\usepackage[letterpaper,margin=1in]{geometry}",
  "\\usepackage{amsmath,amssymb}",
  "\\usepackage{breqn}",
  "\\usepackage{graphicx}",
  "\\usepackage{longtable}",
  "\\usepackage{adjustbox}",
  "\\usepackage{multicol}",
  "\\usepackage{bm}",
  "\\usepackage[mathcal]{eucal}",
  "\\usepackage{tikz}",
  "\\begin{document}"
)
default_ending <- "\\end{document}"

#' Sort Model Components for LaTeX Display
#'
#' Sorts sets, parameters, variables, or mappings hierarchically and alphabetically.
#' For sets: base sets before subsets, grouped by component type.
#' For parameters/variables: grouped by component prefix (Generator, Line, etc.), alphabetically within groups.
#' For mappings: alphabetical sort.
#'
#' @param components List of model components
#' @param type Component type: "sets", "parameters", "variables", or "mappings"
#' @return Sorted list of components
#' @keywords internal
#' @noRd
sort_model_components <- function(components, type = "sets") {
  if (is.null(components) || length(components) == 0) {
    return(components)
  }
  
  component_names <- sapply(components, function(x) x$name)
  
  if (type == "sets") {
    # Build hierarchy using subset_of field
    base_sets <- character()
    subset_map <- list()  # Maps base set name to its subsets
    standalone_sets <- character()
    
    for (comp in components) {
      name <- comp$name
      if (!is.null(comp$subset_of) && !is.na(comp$subset_of) && comp$subset_of != "") {
        # This is a subset
        base_name <- comp$subset_of
        if (is.null(subset_map[[base_name]])) {
          subset_map[[base_name]] <- character()
        }
        subset_map[[base_name]] <- c(subset_map[[base_name]], name)
      } else {
        # This is potentially a base set or standalone
        if (name %in% names(subset_map)) {
          base_sets <- c(base_sets, name)
        } else {
          standalone_sets <- c(standalone_sets, name)
        }
      }
    }
    
    # Second pass: move sets from standalone to base if they have subsets
    for (name in standalone_sets) {
      if (name %in% names(subset_map)) {
        base_sets <- c(base_sets, name)
        standalone_sets <- setdiff(standalone_sets, name)
      }
    }
    
    # Build sorted order: standalone sets alphabetically, then base sets with their subsets
    sorted_names <- character()
    
    # Add standalone sets alphabetically
    sorted_names <- c(sorted_names, sort(standalone_sets))
    
    # Add base sets with their subsets, grouped and sorted
    if (length(base_sets) > 0) {
      base_sets_sorted <- sort(base_sets)
      for (base_name in base_sets_sorted) {
        sorted_names <- c(sorted_names, base_name)
        if (!is.null(subset_map[[base_name]])) {
          sorted_names <- c(sorted_names, sort(subset_map[[base_name]]))
        }
      }
    }
    
    # Return components in sorted order
    components[match(sorted_names, component_names)]
    
  } else if (type %in% c("parameters", "variables")) {
    # Group by component prefix (Generator_, Line_, Link_, etc.)
    # Extract prefix before first underscore
    component_groups <- list()
    no_prefix <- character()
    
    for (name in component_names) {
      if (grepl("^[A-Z][a-z]*[A-Z][a-z]*_", name)) {
        # Has component prefix like "Generator_" or "StorageUnit_"
        prefix <- sub("_.*$", "", name)
        if (is.null(component_groups[[prefix]])) {
          component_groups[[prefix]] <- character()
        }
        component_groups[[prefix]] <- c(component_groups[[prefix]], name)
      } else if (grepl("^[A-Z][a-z]*_", name)) {
        # Has simple prefix like "Line_" or "Bus_"
        prefix <- sub("_.*$", "", name)
        if (is.null(component_groups[[prefix]])) {
          component_groups[[prefix]] <- character()
        }
        component_groups[[prefix]] <- c(component_groups[[prefix]], name)
      } else {
        # No recognizable prefix (e.g., "efficiency", "snapshot")
        no_prefix <- c(no_prefix, name)
      }
    }
    
    # Sort groups alphabetically, then items within groups
    sorted_names <- character()
    group_names <- sort(names(component_groups))
    for (group in group_names) {
      sorted_names <- c(sorted_names, sort(component_groups[[group]]))
    }
    
    # Add items without prefix at the end
    if (length(no_prefix) > 0) {
      sorted_names <- c(sorted_names, sort(no_prefix))
    }
    
    # Return components in sorted order
    components[match(sorted_names, component_names)]
    
  } else if (type == "mappings") {
    # Simple alphabetical sort
    components[order(component_names)]
  } else {
    # Default: return as-is
    components
  }
}

#' Write LaTeX representation of an equation or model
#'
#' @param x An object of class `equation` or `model`.
#'
#' @param file Path to the output LaTeX file. If `NULL`, returns the LaTeX code as a character string.
#' @param ... Additional arguments passed to the specific method.
#'
#' @export
write_latex <- function(x, file, ...) {
  UseMethod("write_latex")
}

#' Write LaTeX representation of an equation
#'
#' @param append Logical; if `TRUE`, appends to the file instead of overwriting.
#' @param standalone Logical; if `TRUE`, writes a complete LaTeX document with preamble and ending.
#' @param preamble Character vector; LaTeX preamble to use. If `NULL`, uses the default preamble.
#' @param ending Character vector; LaTeX ending to use. If `NULL`, uses the default ending.
#' @param subsection_number Logical; if `TRUE`, includes subsection numbering in the LaTeX output.
#' @param eq_substitute Named list; substitutions for specific AST elements in the equation
#' with `ast_where` object to display conditions, indices, etc. below the equation.
#' @param verbose Logical; if `TRUE`, prints additional information during processing.
#' @param ... Additional arguments passed to the `as_latex` function for rendering the equation.
#'
#' @export
#' @method write_latex equation
write_latex.equation <- function(x,
                                 file,
                                 append = FALSE,
                                 standalone = !append,
                                 preamble = NULL,
                                 ending = NULL,
                                 subsection_number = FALSE,
                                 eq_substitute = list("when" = "condition"),
                                                      # "func" = "index",
                                                      # "sum" = "index",
                                                      # "prod" = "index"),
                                 alias_map = NULL,
                                 verbose = FALSE,
                                 ...) {

  browser()
  if (!is_empty(eq_substitute)) {
    # Substitute elements in the AST
    x <- remap_ast_elements(x,
                            ast_type = eq_substitute,
                            verbose = verbose, ...)
  }

  # Apply alias mapping if provided
  if (!is.null(alias_map)) {
    x <- alias_ast_names(x, alias_map, verbose = verbose, ...)
  }

  if (is.null(preamble)) {
    # Default LaTeX preamble
    preamble <- default_preamble
  }
  if (is.null(ending)) {
    # Default LaTeX ending
    ending <- default_ending
  }

  # Generate LaTeX body
  latex_code <- as_latex(x, subsection_number = subsection_number, ...)

  # Assemble full content if standalone
  content <- character()
  if (standalone) {
    content <- c(
      if (is.null(preamble)) default_preamble else preamble,
      latex_code,
      if (is.null(ending)) default_ending else ending
    )
  } else {
    content <- latex_code
  }

  # Open connection manually to control append behavior
  con <- file(file, open = if (append) "a" else "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)

  writeLines(content, con = con, sep = "\n")

  invisible(file)
}

#' Write LaTeX representation of a model
#'
#' @inherit write_latex.equation
#' @param model_view Character; display mode: "reduced" (folded dims, skip trimmed),
#'   "full" (original model), or "both" (comparison). Default auto-detects.
#' @param include_data Logical; if TRUE, include data context (set sizes, etc.).
#'   If FALSE, show pure theory. Default TRUE.
#' @param data_detail Character; "brief" (counts only) or "detailed" (full stats).
#' @param set_display_max_inline Maximum set elements to show inline.
#' @param set_display_head_tail_threshold Threshold for head/tail display.
#' @param set_display_head_n Number of head elements for large sets.
#' @param set_display_tail_n Number of tail elements for large sets.
#' @param include_toc Logical; include table of contents. Default FALSE.
#' @param include_sets Logical; include sets section. Default TRUE.
#' @param include_aliases Logical; include aliases section. Default TRUE.
#' @param include_index_aliases Logical; include index aliases section. Default TRUE.
#' @param include_parameters Logical; include parameters section. Default TRUE.
#' @param include_variables Logical; include variables section. Default TRUE.
#' @param include_equations Logical; include equations section. Default TRUE.
#' @param include_mappings Logical; include mappings section. Default TRUE.
#' @param use_model_aliases Logical; if TRUE, auto-use model$index_aliases. Default TRUE.
#' @param use_aliases_in_declarations Logical; if TRUE, use aliased dimension names in
#'   parameter/variable declarations. If FALSE (default), show full names in declarations
#'   but use aliases in equations.
#' @param folded_color Character; LaTeX color name for folded annotations. Default "blue".
#' @param trimmed_color Character; LaTeX color name for trimmed annotations. Default "red".
#'
#' @export
#' @method write_latex model
write_latex.model <- function(x,
                              file = NULL,
                              append = FALSE,
                              preamble = NULL,
                              ending = NULL,
                              title = paste0("Model: ", x$name),
                              subtitle = x$desc,
                              author = x$authors,
                              show_date = TRUE,
                              # math_env = "equation",
                              subsection_number = TRUE,

                              # === PHASE 1 CORE PARAMETERS ===
                              model_view = c("reduced", "full", "both"),
                              include_data = TRUE,
                              data_detail = c("brief", "detailed"),

                              # === SET DISPLAY CONTROLS ===
                              set_display_max_inline = 10,
                              set_display_head_tail_threshold = 100,
                              set_display_head_n = 3,
                              set_display_tail_n = 3,

                              # === EXISTING PARAMETERS ===
                              include_toc = FALSE,
                              include_sets = TRUE,
                              include_aliases = TRUE,
                              include_index_aliases = TRUE,
                              include_parameters = TRUE,
                              include_variables = TRUE,
                              include_equations = TRUE,
                              include_mappings = TRUE,
                              eq_substitute = list("when" = "condition"),
                              alias_map = NULL,
                              use_model_aliases = TRUE,
                              use_latex_names = TRUE,
                              use_aliases_in_declarations = FALSE,
                              folded_color = "blue",
                              trimmed_color = "red",
                              verbose = FALSE,
                              ...) {
  # browser()
  # Auto-detect model_view default
  if (missing(model_view)) {
    is_optimized <- is_folded(x) || is_trimmed(x)
    model_view <- if (is_optimized) "reduced" else "full"
  } else {
    model_view <- match.arg(model_view)
  }

  # Match data_detail argument
  data_detail <- match.arg(data_detail)

  # Determine use_folded flag from model_view
  use_folded <- (model_view != "full")

  validation <- validate(x, stop_on_error = FALSE)
  validation_note <- NULL
  if (!validation$valid) {
    warning(sprintf(
      "write_latex: model validation failed with %d error(s); LaTeX output will include a warning block.",
      length(validation$errors)
    ))
    validation_note <- c(
      "",
      "\\begin{center}",
      "\\textbf{WARNING: Model validation failed. Output may be incomplete.}",
      sprintf("\\textit{%d validation issue(s) detected. See R console for details.}", length(validation$errors)),
      "\\end{center}",
      ""
    )
  }

  # Auto-detect aliases from model if not provided
  if (is.null(alias_map) && use_model_aliases &&
      !is.null(x$index_aliases) && length(x$index_aliases) > 0) {
    # Convert named vector to list if needed
    alias_map <- if (is.list(x$index_aliases)) x$index_aliases else as.list(x$index_aliases)
  }
  # browser()
  if (!is_empty(eq_substitute)) {
    # Substitute elements in the AST
    x$equations <- lapply(x$equations, remap_ast_elements,
                            ast_type = eq_substitute,
                            verbose = verbose, ...)
  }

  model <- x
  
  # Build LaTeX names map from model metadata
  latex_names <- NULL
  if (use_latex_names && !is.null(model$metadata$latex_names)) {
    latex_names <- model$metadata$latex_names
  }
  
  if (is.null(preamble)) {
    # Default LaTeX preamble
    preamble <- default_preamble
  }

  if (is.null(ending)) {
    # Default LaTeX ending
    ending <- default_ending
  }

  # Header with model name/description
  meta_block <- character()
  if (!is.null(title)) {
    meta_block <- c(
      meta_block,
      paste0("\\title{",
             title,
             paste0("\\\\\\large ", as_latex(subtitle), "}"))
    )
  }
  if (!is.null(author)) {
    meta_block <- c(meta_block, paste0("\\author{", author, "}"))
  }
  if (show_date) {
    meta_block <- c(meta_block, "\\date{\\today}")
  }
  meta_block <- c(meta_block, "\\maketitle")

  lines <- character()

  if (!is.null(validation_note)) {
    lines <- c(lines, validation_note)
  }

  # Add table of contents if requested
  if (include_toc) {
    lines <- c(lines, "", "\\tableofcontents", "\\newpage", "")
  }

  # Add model optimization summary if model is folded or trimmed
  if (is_folded(model) || is_trimmed(model)) {
    lines <- c(lines, "", "\\section*{Model Optimization Summary}")

    if (is_folded(model)) {
      fold_summary <- get_fold_summary(model, format = "list")
      if (!is.null(fold_summary)) {
        lines <- c(lines, "\\textbf{Folding Applied:}")
        lines <- c(lines, "\\textit{Folding reduces redundancy in model data by aggregating dimensions that have uniform values. This optimization does not affect the model equations or variables, only simplifies the parameter structure.}")
        lines <- c(lines, "\\begin{itemize}")
        if (!is.null(fold_summary$n_folded)) {
          lines <- c(lines, sprintf("  \\item Parameters folded: %d / %d (%.1f\\%%)",
                                    fold_summary$n_folded,
                                    fold_summary$n_total,
                                    100 * fold_summary$n_folded / fold_summary$n_total))
        }
        if (!is.null(fold_summary$parameters) && length(fold_summary$parameters) > 0) {
          lines <- c(lines, "  \\item Dimension reductions:")
          lines <- c(lines, "  \\begin{itemize}")
          for (pinfo in fold_summary$parameters) {
            safe_param <- gsub("_", "\\\\_", pinfo$parameter)
            lines <- c(lines, sprintf("    \\item \\texttt{%s}: %s $\\rightarrow$ %s (%d $\\rightarrow$ %d rows, %s)",
                                     safe_param,
                                     pinfo$original_dims,
                                     pinfo$folded_dims,
                                     pinfo$original_rows,
                                     pinfo$folded_rows,
                                     pinfo$compression))
          }
          lines <- c(lines, "  \\end{itemize}")
        }
        lines <- c(lines, "\\end{itemize}")
      }
    }

    if (is_trimmed(model)) {
      trim_summary <- get_trim_summary(model, format = "list")
      if (!is.null(trim_summary)) {
        lines <- c(lines, "", "\\textbf{Trimming Applied:}")
        lines <- c(lines, "\\textit{Trimming removes empty and unused model elements (sets with no members, parameters with no data, variables and equations with empty domains). This optimization reduces model size by eliminating elements that do not contribute to the solution.}")
        lines <- c(lines, "\\begin{itemize}")
        for (type in c("sets", "parameters", "mappings", "variables", "equations")) {
          if (!is.null(trim_summary[[type]])) {
            total <- trim_summary[[type]]$total
            trimmed <- trim_summary[[type]]$trimmed
            remaining <- total - trimmed
            pct <- if (total > 0) round(100 * trimmed / total, 1) else 0
            lines <- c(lines, sprintf("  \\item %s: %d / %d trimmed (%.1f\\%%), %d remaining",
                                     tools::toTitleCase(type), trimmed, total, pct, remaining))
          }
        }
        lines <- c(lines, "\\end{itemize}")
      }
    }
    lines <- c(lines, "")
  }

  ## Sets ####
  if (include_sets && !is.null(model$sets)) {
    lines <- c(lines, "", "\\section{Sets}")
    # Sort sets: base sets first, then subsets, within groups alphabetically
    sorted_sets <- sort_model_components(model$sets, type = "sets")
    for (s in sorted_sets) {
      # Skip trimmed sets if model_view is "reduced"
      if (model_view == "reduced" && isTRUE(s$trimmed)) next

      # Use LaTeX name if available (from metadata), otherwise escape underscores
      latex_name <- if (!is.null(latex_names$sets)) latex_names$sets[[s$name]] else NULL
      if (!is.null(latex_name) && nchar(latex_name) > 0) {
        set_lx <- paste0("$", latex_name, "$")
      } else {
        safe_name <- gsub("_", "\\\\_", s$name)
        set_lx <- paste0("\\texttt{", safe_name, "}")
      }
      
      # Add subset relationship if present
      if (!is_empty(s$subset_of)) {
        # Get parent latex name from metadata
        parent_latex <- if (!is.null(latex_names$sets)) latex_names$sets[[s$subset_of]] else NULL
        if (!is.null(parent_latex) && nchar(parent_latex) > 0) {
          set_lx <- paste0(set_lx, " $\\subseteq ", parent_latex, "$")
        } else {
          safe_parent <- gsub("_", "\\\\_", s$subset_of)
          set_lx <- paste0(set_lx, " $\\subseteq$ \\texttt{", safe_parent, "}")
        }
      }
      
      if (!is_empty(s$desc)) {
        set_lx <- paste0(set_lx, " -- ", as_latex(s$desc))
      }

      # Add element display if include_data
      if (include_data && !is.null(s$data)) {
        elem_display <- format_set_elements(
          s$data,
          max_inline = set_display_max_inline,
          head_tail_threshold = set_display_head_tail_threshold,
          head_n = set_display_head_n,
          tail_n = set_display_tail_n
        )
        set_lx <- paste0(set_lx, "\n  \\quad \\textit{", elem_display, "}")
      }

      # Annotate trimmed if showing full model
      if (model_view %in% c("full", "both") && isTRUE(s$trimmed)) {
        set_lx <- paste0(set_lx, " \\textcolor{", trimmed_color, "}{(trimmed)}")
      }

      lines <- c(lines, set_lx, "\\\\")
    }
  }

  if (include_aliases && !is.null(model$aliases)) {
    lines <- c(lines, "", "\\section{Aliases}\\", "\\begin{flushleft}")
    # browser()
    for (a in model$aliases) {
      if (is.character(a)) {
        b <- paste0("\\texttt{", a, "}", collapse = ", ")
        # b <- latex_wrap_brackets(b, brackets = "{}")
        b <- paste0("\\{", b, "\\}")
      } else if (inherits(a, "ast")) {
        b <- as_latex(a, subsection_number = subsection_number, ...)
      } else {
        b <- as.character(a)
      }
      lines <- c(lines, paste0(b, "\\\\"))
    }
    lines <- c(lines, "\\end{flushleft}")
  }

  # Display index aliases if they exist (after aliases section)
  if (include_index_aliases && !is.null(model$index_aliases) &&
      length(model$index_aliases) > 0) {
    lines <- c(lines, "", "\\section{Index Aliases}\\", "\\begin{flushleft}")
    for (idx_name in names(model$index_aliases)) {
      short_form <- model$index_aliases[[idx_name]]
      alias_line <- sprintf("$\\texttt{%s} \\equiv \\texttt{%s}$", idx_name, short_form)
      lines <- c(lines, paste0(alias_line, "\\\\"))
    }
    lines <- c(lines, "\\end{flushleft}")
  }


  if (include_parameters && !is.null(model$parameters)) {
    lines <- c(lines, "", "\\section{Parameters}")
    # Sort parameters by component groups, alphabetically within groups
    sorted_params <- sort_model_components(model$parameters, type = "parameters")
    for (p in sorted_params) {
      # Skip trimmed parameters if model_view is "reduced"
      if (model_view == "reduced" && isTRUE(p$trimmed)) next

      p_tex <- paste0("$", as_latex(p, use_folded = use_folded, 
                                    use_index_aliases = use_aliases_in_declarations,
                                    model = model), "$")
      if (!is_empty(p$desc) && nzchar(p$desc) > 0) {
        p_tex <- paste0(p_tex, " -- ", as_latex(p$desc))
      }

      # Add row count if include_data and data_detail is "brief"
      # Handle both data.frame and vector data formats
      data_len <- if (!is.null(p$data)) {
        if (is.data.frame(p$data)) nrow(p$data) else length(p$data)
      } else 0
      if (include_data && data_detail == "brief" && data_len > 0) {
        p_tex <- paste0(p_tex, " \\quad \\textit{(",
                       format(data_len, big.mark = ","), " values)}")
      }

      # Annotate trimmed if showing full model
      if (model_view %in% c("full", "both") && isTRUE(p$trimmed)) {
        p_tex <- paste0(p_tex, " \\textcolor{", trimmed_color, "}{(trimmed)}")
      }

      # Annotate folded if parameter has active_dims (folded dimensions)
      if (!is.null(p$active_dims) && length(p$active_dims) > 0) {
        orig_dims <- if (!is.null(p$dims)) length(p$dims) else 0
        folded_dims <- length(p$active_dims)
        if (orig_dims != folded_dims) {
          folded_note <- paste0("(folded: ", orig_dims, "D $\\rightarrow$ ", folded_dims, "D")
          # Add folded data row count if available
          if (!is.null(p$folded_data) && nrow(p$folded_data) > 0) {
            folded_note <- paste0(folded_note, ", ", format(nrow(p$folded_data), big.mark = ","), " rows")
          }
          folded_note <- paste0(folded_note, ")")
          p_tex <- paste0(p_tex, " \\textcolor{", folded_color, "}{", folded_note, "}")
        }
      }

      lines <- c(lines, paste0(p_tex, "\\\\"))
    }
  }

  if (include_variables && !is.null(model$variables)) {
    lines <- c(lines, "", "\\section{Variables}")
    # Sort variables by component groups, alphabetically within groups
    sorted_vars <- sort_model_components(model$variables, type = "variables")
    for (v in sorted_vars) {
      # Skip trimmed variables if model_view is "reduced"
      if (model_view == "reduced" && isTRUE(v$trimmed)) next

      v_tex <- paste0("$", as_latex(v, use_folded = use_folded,
                                    use_index_aliases = use_aliases_in_declarations,
                                    model = model), "$")
      if (!is_empty(v$desc) && nzchar(v$desc) > 0) {
        v_tex <- paste0(v_tex, " -- ", as_latex(v$desc))
      }

      # Annotate trimmed if showing full model
      if (model_view %in% c("full", "both") && isTRUE(v$trimmed)) {
        v_tex <- paste0(v_tex, " \\textcolor{", trimmed_color, "}{(trimmed)}")
      }

      # Annotate folded if variable has active_dims (folded dimensions)
      if (!is.null(v$active_dims) && length(v$active_dims) > 0) {
        orig_dims <- if (!is.null(v$dims)) length(v$dims) else 0
        folded_dims <- length(v$active_dims)
        if (orig_dims != folded_dims) {
          folded_note <- paste0("(folded: ", orig_dims, "D $\\rightarrow$ ", folded_dims, "D")
          # Add folded data row count if available
          if (!is.null(v$folded_data) && nrow(v$folded_data) > 0) {
            folded_note <- paste0(folded_note, ", ", format(nrow(v$folded_data), big.mark = ","), " rows")
          }
          folded_note <- paste0(folded_note, ")")
          v_tex <- paste0(v_tex, " \\textcolor{", folded_color, "}{", folded_note, "}")
        }
      }

      lines <- c(lines, paste0(v_tex, "\\\\"))
    }
  }

  if (include_mappings && !is.null(model$mappings)) {
    lines <- c(lines, "", "\\section{Mappings}")
    # Sort mappings alphabetically
    sorted_mappings <- sort_model_components(model$mappings, type = "mappings")
    for (m in sorted_mappings) {
      # Skip trimmed mappings if model_view is "reduced"
      if (model_view == "reduced" && isTRUE(m$trimmed)) next

      m_tex <- paste0("$", as_latex(m, use_folded = use_folded), "$")
      if (!is_empty(m$desc) && nzchar(m$desc) > 0) {
        m_tex <- paste0(m_tex, " -- ", as_latex(m$desc))
      }

      # Add row count if include_data and data_detail is "brief"
      if (include_data && data_detail == "brief" && !is.null(m$data) && nrow(m$data) > 0) {
        m_tex <- paste0(m_tex, " \\quad \\textit{(",
                       format(nrow(m$data), big.mark = ","), " rows)}")
      }

      # Annotate trimmed if showing full model
      if (model_view %in% c("full", "both") && isTRUE(m$trimmed)) {
        m_tex <- paste0(m_tex, " \\textcolor{", trimmed_color, "}{(trimmed)}")
      }

      lines <- c(lines, paste0(m_tex, "\\\\"))
    }
  }

  if (include_equations && !is.null(model$equations)) {
    lines <- c(lines, "", "\\section{Equations}\\")
    # Render all equations using as_latex()
    for (eq in x$equations) {
      # Skip trimmed equations if model_view is "reduced"
      if (model_view == "reduced" && isTRUE(eq$trimmed)) next

      if (verbose) message(eq$name)
      eq_tex <- as_latex(eq,
                         subsection_number = subsection_number, 
                         model = model,  # Pass model for index_aliases fallback
                         ...)

      # Annotate trimmed if showing full model
      if (model_view %in% c("full", "both") && isTRUE(eq$trimmed)) {
        # Add trimmed note after equation title
        eq_tex <- sub("(\\\\subsection\\{.*?\\})", paste0("\\1 \\\\textcolor{", trimmed_color, "}{(trimmed)}"), eq_tex)
      }

      lines <- c(lines, eq_tex, "\n")
    }
  }

  # Assemble full LaTeX content
  body <- c(meta_block, lines)
  full_doc <- c(preamble, "", body, "", ending)
  output <- paste(full_doc, collapse = "\n")

  # Write or return
  if (!is.null(file)) {
    writeLines(output, con = file, useBytes = TRUE)
    return(invisible(file))
  } else {
    return(output)
  }
}
