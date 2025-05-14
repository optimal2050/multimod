#' Plot a multimod AST using visNetwork
#'
#' @param x A multimod_ast object
#' @param ... Additional arguments (currently unused)
#'
#' @return A visNetwork plot
#' @export
plot.multimod_ast <- function(x, ...) {
  nodes <- list()
  edges <- list()
  node_id <- 0

  new_id <- function() {
    node_id <<- node_id + 1
    node_id
  }

  walk_ast <- function(node, parent_id = NULL) {
    id <- new_id()

    label <- switch(node$type,
                    expression = node$op,
                    variable   = paste0("v: ", node$name),
                    parameter  = paste0("p: ", node$name),
                    constant   = as.character(node$value),
                    symbol     = node$value,
                    relation   = node$relation,
                    condition  = "$",
                    sum        = paste0("sum(", node$index, ")"),
                    prod       = paste0("prod(", node$index, ")"),
                    mapping    = paste0("map: ", node$name),
                    set        = paste0("set: ", node$name),
                    paste0("?", node$type)
    )

    nodes[[length(nodes) + 1]] <<- data.frame(id = id, label = label, group = node$type, stringsAsFactors = FALSE)

    if (!is.null(parent_id)) {
      edges[[length(edges) + 1]] <<- data.frame(from = parent_id, to = id)
    }

    # Recurse
    if (node$type == "expression") {
      walk_ast(node$lhs, id)
      walk_ast(node$rhs, id)
    } else if (node$type == "condition") {
      walk_ast(node$condition, id)
      walk_ast(node$then, id)
    } else if (node$type %in% c("sum", "prod")) {
      if (!is.null(node$domain)) walk_ast(node$domain, id)
      walk_ast(node$value, id)
    } else if (node$type %in% c("variable", "parameter", "mapping")) {
      if (!is.null(node$dims)) {
        # for (arg in node$dims) {
        #   arg_id <- new_id()
        #   nodes[[length(nodes) + 1]] <<- data.frame(id = arg_id, label = arg, group = "index", stringsAsFactors = FALSE)
        #   edges[[length(edges) + 1]] <<- data.frame(from = id, to = arg_id)
        # }
      }
    }

    invisible(id)
  }

  walk_ast(x)

  nodes_df <- do.call(rbind, nodes)
  edges_df <- if (length(edges) > 0) do.call(rbind, edges) else data.frame(from = integer(0), to = integer(0))

  visNetwork::visNetwork(nodes_df, edges_df) |>
    visNetwork::visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) |>
    # visNetwork::visHierarchicalLayout(direction = "UD")
    visNetwork::visHierarchicalLayout(direction = "LR")
}



# Plot method for AST
plot_d.multimod_ast <- function(x, ...) {
  if (!requireNamespace("DiagrammeR", quietly = TRUE)) {
    stop("DiagrammeR package is required to plot AST.")
  }

  node_counter <- 0
  dot_nodes <- c()
  dot_edges <- c()

  walk_ast <- function(node) {
    node_id <- paste0("n", node_counter <<- node_counter + 1)
    label <- switch(node$type,
                    "expression" = node$op,
                    "variable" = paste0(node$name, "[", paste(node$dims, collapse = ","), "]"),
                    "parameter" = paste0(node$name, "[", paste(node$dims, collapse = ","), "]"),
                    "constant" = as.character(node$value),
                    "symbol" = node$value,
                    node$type)
    dot_nodes <<- c(dot_nodes, sprintf("%s [label = \"%s\"]", node_id, label))

    if (node$type == "expression") {
      left_id <- walk_ast(node$lhs)
      right_id <- walk_ast(node$rhs)
      dot_edges <<- c(dot_edges, sprintf("%s -> %s", node_id, left_id))
      dot_edges <<- c(dot_edges, sprintf("%s -> %s", node_id, right_id))
    }
    node_id
  }

  walk_ast(x)

  dot_code <- paste0("digraph AST {\n",
                     "node [shape=box];\n",
                     paste(dot_nodes, collapse = "\n"), "\n",
                     paste(dot_edges, collapse = "\n"), "\n",
                     "}")

  DiagrammeR::grViz(dot_code)
}

with_dims <- function(name, dims, bracket_type = c("[", "]"), collapse = ",",
                      show_dims = TRUE) {
  if (length(dims) == 0 || !show_dims) {
    return(name)
  }
  dims_str <- paste0(bracket_type[1],
                     paste(dims, collapse = collapse),
                     bracket_type[2])
  paste0(name, dims_str)
}


#' Get network data for a multimod equation
#'
#' @param eq A multimod equation object
#' @param alias_map A named list for alias mapping (optional)
#' @param show_dims Logical, whether to show dimensions in the label (default: FALSE)
#'
#' @returns A list with two data frames: nodes and edges to be used with
#' visNetwork or similar packages for visualization.
#' @export
#'
#' @examples
#'
get_network_data <- function(eq, alias_map = NULL, show_dims = FALSE) {
  id_counter <- 0
  nodes <- list()
  edges <- list()

  # Recursive builder for expressions
  recurse_expr <- function(expr, parent_id = NULL) {
    id_counter <<- id_counter + 1
    my_id <- id_counter

    if (!is.null(alias_map) && expr$name %in% names(alias_map)) {
      expr$name <- alias_map[[expr$name]]
    }

    # if (expr$type %in% c("sum")) browser()

    label <- switch(
      expr$type,
      "expression" = expr$op,
      "unary" = expr$op,
      "variable" = with_dims(expr$name, expr$dims, show_dims = show_dims),
      "parameter" = with_dims(expr$name, expr$dims, show_dims = show_dims),
      "mapping" = with_dims(expr$name, expr$dims, show_dims = show_dims),
      "symbol" = expr$value,
      "func" = with_dims(expr$name, expr$dims, show_dims = show_dims),
      "condition" = "$",
      "sum" = paste0("sum(", as.character.ast_dims(expr$index), ")"),
      "prod" = paste0("prod(", as.character.ast_dims(expr$index), ")"),
      "logic" = expr$op,
      "constant" = as.character(expr$value),
      "compare" = expr$op,
      paste0("<", expr$type, ">")
    )

    color <- "lightgray" # Default fallback

    if (expr$type == "variable") {
      color <- "dodgerblue"
    } else if (expr$type %in% c("parameter", "constant")) {
      color <- "orange"
    } else if (expr$type == "mapping") {
      color <- "palegreen"
    } else if (expr$type %in% c("set", "dims")) {
      color <- "plum"
    } else if (expr$type %in% c("expression", "unary", "func", "sum", "prod")) {
      color <- "lightblue"
    } else if (expr$type == "condition") {
      color <- "lightcoral"
    } else {
      color <- "lightgray"
    }

    nodes[[length(nodes) + 1]] <<- data.frame(
      id = my_id,
      label = label,
      shape = "box",
      color = color,
      stringsAsFactors = FALSE
    )

    if (!is.null(parent_id)) {
      edges[[length(edges) + 1]] <<- data.frame(from = parent_id, to = my_id)
    }

    if (expr$type == "expression") {
      recurse_expr(expr$lhs, my_id)
      recurse_expr(expr$rhs, my_id)
    } else if (expr$type == "unary") {
      recurse_expr(expr$rhs, my_id)
    } else if (expr$type %in% c("sum", "prod", "condition", "logic", "compare")) {
      # browser()
      if (!is.null(expr$domain)) recurse_expr(expr$domain, my_id)
      if (!is.null(expr$lhs)) recurse_expr(expr$lhs, my_id)
      if (!is.null(expr$rhs)) recurse_expr(expr$rhs, my_id)
      if (!is.null(expr$value)) recurse_expr(expr$value, my_id)
      if (!is.null(expr$condition)) recurse_expr(expr$condition, my_id)
      if (!is.null(expr$then)) recurse_expr(expr$then, my_id)
    }

    return(my_id)
  }

  # Build LHS
  lhs_root <- recurse_expr(eq$lhs)

  # Add Relation node (central node)
  id_counter <- id_counter + 1
  relation_id <- id_counter
  nodes[[length(nodes) + 1]] <- data.frame(
    id = relation_id,
    label = eq$relation,
    shape = "circle",
    color = "coral",
    stringsAsFactors = FALSE
  )

  edges[[length(edges) + 1]] <- data.frame(from = lhs_root, to = relation_id)

  # Build RHS
  rhs_root <- recurse_expr(eq$rhs)
  edges[[length(edges) + 1]] <- data.frame(from = relation_id, to = rhs_root)

  # Assemble nodes/edges
  nodes_df <- do.call(rbind, nodes)
  edges_df <- do.call(rbind, edges)

  return(list(nodes = nodes_df, edges = edges_df))

}

#' Convert a multimod ast or equation to a visNetwork object
#'
#' @param expr A multimod object
#' @export
as_visNetwork <- function(expr) {
  UseMethod("as_visNetwork")
}


#' @export
#' @rdname as_visNetwork
#' @method as_visNetwork multimod_equation
as_visNetwork.multimod_equation <- function(eq,
                                            title = NULL,
                                  alias_map = NULL,
                                  show_dims = FALSE) {

  if (!requireNamespace("visNetwork", quietly = TRUE)) {
    stop("Please install 'visNetwork' package.")
  }

  # Title
  plot_title <- if (!is.null(title)) title else eq$name

  # Get network data
  nd <- get_network_data(eq, alias_map = alias_map, show_dims = show_dims)

  # Render
  # visNetwork::visNetwork(nodes_df, edges_df, main = plot_title) |>
  visNetwork::visNetwork(nd$nodes, nd$edges, main = plot_title) |>
    # visNetwork::visNodes(shape = "box", font = list(size = 20)) |>
    # visNetwork::visEdges(arrows = "to") |>
    visNetwork::visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) |>
    visNetwork::visHierarchicalLayout(direction = "LR")
  # visNetwork::visHierarchicalLayout(direction = "UD")  # top-down tree
}






