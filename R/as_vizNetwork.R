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
get_network_data <- function(x, alias_map = NULL, show_dims = TRUE) {
  eq <- x
  # browser()
  id_counter <- 0
  nodes <- list()
  edges <- list()
  # Recursive builder for expressions
  walk_ast <- function(expr, parent_id = NULL, label_prx = NULL) {

    if (is_empty(expr)) return(NULL)
    if (!is.null(alias_map) && expr$name %in% names(alias_map)) {
      expr$name <- alias_map[[expr$name]]
    }

    ntype <- node_type(expr)
    if (ntype == "dims") {
      if (!show_dims) return(NULL)
      label <- with_dims("", expr, show_dims = show_dims)
    } else if (ntype == "sum") {
      label <- "sum"
    } else if (ntype == "prod") {
      label <- "prod"
    } else if (ntype == "when") {
      label <- "when"
    } else if (ntype == "expression") {
      label <- expr$op
    } else if (ntype == "unary") {
      label <- expr$op
    } else if (ntype == "mapping") {
      label <- paste0("", with_dims(expr$name, expr$dims, show_dims = show_dims))
    } else if (ntype == "parameter") {
      label <- paste0("", with_dims(expr$name, expr$dims, show_dims = show_dims))
    } else if (ntype == "variable") {
      label <- paste0("", with_dims(expr$name, expr$dims, show_dims = show_dims))
    } else if (ntype == "constant") {
      label <- as.character(expr$value)
    } else if (ntype == "symbol") {
      label <- expr$name
    } else if (ntype == "func") {
      label <- paste0("func: ", with_dims(expr$name, expr$dims, show_dims = show_dims))
    } else if (ntype == "set") {
      label <- paste0("", expr$name)
    } else if (ntype == "where") {
      label <- paste0("", expr$name)
    } else {
      label <- paste0("<", node_type(expr), ">")
    }

    id_counter <<- id_counter + 1
    my_id <- id_counter

    if (grepl("<.+>", label)) browser()
    if (label == "sum" || label == "prod") {
      if (node_type(expr$index) == "dims") {
        label <- paste0(label, with_dims(expr$name, expr$index, show_dims = show_dims))
      }
    }

    label <- if (!is.null(label_prx)) paste0(label_prx, label) else label

    color <- "lightgray" # Default fallback

    if (node_type(expr) == "variable") {
      color <- "dodgerblue"
    } else if (node_type(expr) %in% c("parameter", "constant")) {
      color <- "orange"
    } else if (node_type(expr) == "mapping") {
      color <- "palegreen"
    } else if (node_type(expr) %in% c("set", "dims")) {
      color <- "plum" # should not be displayed
    } else if (node_type(expr) %in% c("expression", "unary", "func", "sum", "prod")) {
      color <- "lightblue"
    } else if (node_type(expr) == "when") {
      color <- "lightcoral"
    } else if (node_type(expr) == "where") {
      color <- "yellow"
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

    if (node_type(expr) == "expression") {
      walk_ast(expr$lhs, my_id)
      walk_ast(expr$rhs, my_id)
    } else if (node_type(expr) == "unary") {
      walk_ast(expr$rhs, my_id)
    } else if (node_type(expr) %in% c("sum", "prod", "when", "logic", "compare")) {
      # browser()
      if (!is.null(expr$domain)) {
        walk_ast(expr$domain, my_id)
      }
      if (!is.null(expr$lhs)) {
        walk_ast(expr$lhs, my_id)
      }
      if (!is.null(expr$rhs)) {
        walk_ast(expr$rhs, my_id)
      }
      if (!is.null(expr$value)) {
        walk_ast(expr$value, my_id, label_prx = "")
      }
      if (!is.null(expr$condition)) {
        walk_ast(expr$condition, my_id, label_prx = "cond: ")
      }
      if (!is.null(expr$then)) {
        # browser()
        walk_ast(expr$then, my_id, label_prx = "then: ")
      }
      if (!is.null(expr$index)) {
        if (node_type(expr$index) == "dims") {
          # nothing to do
          # browser()
          # walk_ast(expr$index, my_id, label_prx = "index: ")
        } else {
          # browser()
          walk_ast(expr$index, my_id, label_prx = "index: ")
        }
        # walk_ast(expr$index, my_id, label_prx = "index: ")
      }
      if (!is.null(expr$otherwise)) {
        walk_ast(expr$otherwise, my_id, label_prx = "otherwise: ")
      }
    }

    return(my_id)
  }

  # Build node of AST or LHS of equation
  lhs_root <- walk_ast(eq$lhs)

  if (inherits(x, "equation")) {
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
    rhs_root <- walk_ast(eq$rhs)
    edges[[length(edges) + 1]] <- data.frame(from = relation_id, to = rhs_root)
  } else if (inherits(x, "ast")) {
    # If it's a standalone AST, just recurse from the root
    walk_ast(x)
  } else {
    stop("Unsupported object type. Must be an 'equation' or 'ast'.")
  }

  # Assemble nodes/edges
  nodes_df <- do.call(rbind, nodes)
  edges_df <- do.call(rbind, edges)

  return(list(nodes = nodes_df, edges = edges_df))

}

#' Convert a multimod ast or equation to a visNetwork object
#'
#' @param x A multimod object
#' @export
as_visNetwork <- function(x, ...) {
  UseMethod("as_visNetwork")
}


#' @export
#' @rdname as_visNetwork
#' @method as_visNetwork ast
as_visNetwork.ast <- function(x,
                              title = NULL,
                              alias_map = NULL,
                              show_dims = TRUE) {

  if (!requireNamespace("visNetwork", quietly = TRUE)) {
    stop("Please install 'visNetwork' package.")
  }
  eq <- x
  # Title
  plot_title <- if (!is.null(title)) {
    title
  } else if (!is.null(eq$name)) {
    eq$name
  } else {
    NULL
  }
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
