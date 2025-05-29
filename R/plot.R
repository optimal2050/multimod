#' Plot a multimod AST using visNetwork
#'
#' @param x A ast object
#' @param ... Additional arguments (currently unused)
#'
#' @return A visNetwork plot
#' @export
plot.ast <- function(x, ...) {
  nodes <- list()
  edges <- list()
  node_id <- 0

  new_id <- function() {
    node_id <<- node_id + 1
    node_id
  }

  walk_ast <- function(node, parent_id = NULL) {
    id <- new_id()

    label <- switch(node_type(node),
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
                    paste0("?", node_type(node))
    )

    nodes[[length(nodes) + 1]] <<- data.frame(id = id, label = label, group = node_type(node), stringsAsFactors = FALSE)

    if (!is.null(parent_id)) {
      edges[[length(edges) + 1]] <<- data.frame(from = parent_id, to = id)
    }

    # Recurse
    if (node_type(node) == "expression") {
      walk_ast(node$lhs, id)
      walk_ast(node$rhs, id)
    } else if (node_type(node) == "when") {
      walk_ast(node$condition, id)
      walk_ast(node$then, id)
    } else if (node_type(node) %in% c("sum", "prod")) {
      if (!is.null(node$domain)) walk_ast(node$domain, id)
      walk_ast(node$value, id)
    } else if (node_type(node) %in% c("variable", "parameter", "mapping")) {
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
plot_d.ast <- function(x, ...) {
  if (!requireNamespace("DiagrammeR", quietly = TRUE)) {
    stop("DiagrammeR package is required to plot AST.")
  }

  node_counter <- 0
  dot_nodes <- c()
  dot_edges <- c()

  walk_ast <- function(node) {
    node_id <- paste0("n", node_counter <<- node_counter + 1)
    label <- switch(node_type(node),
                    "expression" = node$op,
                    "variable" = paste0(node$name, "[", paste(node$dims, collapse = ","), "]"),
                    "parameter" = paste0(node$name, "[", paste(node$dims, collapse = ","), "]"),
                    "constant" = as.character(node$value),
                    "symbol" = node$value,
                    node_type(node))
    dot_nodes <<- c(dot_nodes, sprintf("%s [label = \"%s\"]", node_id, label))

    if (node_type(node) == "expression") {
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

with_dims <- function(name, dims, brackets = c("[", "]"), collapse = ",",
                      show_dims = TRUE) {
  # browser()
  if (length(dims) == 0 || !show_dims) {
    return(name)
  }
  dims_str <- as.character(dims, brackets = brackets)
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
get_network_data <- function(eq, alias_map = NULL, show_dims = TRUE) {
  # browser()
  id_counter <- 0
  nodes <- list()
  edges <- list()
  # Recursive builder for expressions
  recurse_expr <- function(expr, parent_id = NULL, label_prx = NULL) {
    # browser()
    # id_counter <<- id_counter + 1
    # my_id <- id_counter

    if (!is.null(alias_map) && expr$name %in% names(alias_map)) {
      expr$name <- alias_map[[expr$name]]
    }

    # if (node_type(expr) %in% c("sum")) browser()

    # label <- switch(
    #   node_type(expr),
    #   "dims" = with_dims("", expr, show_dims = show_dims),
    #   "expression" = expr$op,
    #   "unary" = expr$op,
    #   "variable" = with_dims(expr$name, expr$dims, show_dims = show_dims),
    #   "parameter" = with_dims(expr$name, expr$dims, show_dims = show_dims),
    #   "mapping" = with_dims(expr$name, expr$dims, show_dims = show_dims),
    #   "symbol" = expr$value,
    #   "func" = with_dims(expr$name, expr$dims, show_dims = show_dims),
    #   "when" = "when",
    #   # "sum" = paste0("sum", as.character(expr$index, brackets = "()")),
    #   # "prod" = paste0("prod", as.character(expr$index, brackets = "()")),
    #   "sum" = "sum",
    #   "prod" = "prod",
    #   # "logic" = expr$op,
    #   "constant" = as.character(expr$value),
    #   # "compare" = expr$op,
    #   paste0("<", node_type(expr), ">")
    # )

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
      recurse_expr(expr$lhs, my_id)
      recurse_expr(expr$rhs, my_id)
    } else if (node_type(expr) == "unary") {
      recurse_expr(expr$rhs, my_id)
    } else if (node_type(expr) %in% c("sum", "prod", "when", "logic", "compare")) {
      # browser()
      if (!is.null(expr$domain)) {
        recurse_expr(expr$domain, my_id)
      }
      if (!is.null(expr$lhs)) {
        recurse_expr(expr$lhs, my_id)
      }
      if (!is.null(expr$rhs)) {
        recurse_expr(expr$rhs, my_id)
      }
      if (!is.null(expr$value)) {
        recurse_expr(expr$value, my_id, label_prx = "")
      }
      if (!is.null(expr$condition)) {
        recurse_expr(expr$condition, my_id, label_prx = "cond: ")
      }
      if (!is.null(expr$then)) {
        # browser()
        recurse_expr(expr$then, my_id, label_prx = "then: ")
      }
      if (!is.null(expr$index)) {
        if (node_type(expr$index) == "dims") {
          # nothing to do
          # browser()
          # recurse_expr(expr$index, my_id, label_prx = "index: ")
        } else {
          # browser()
          recurse_expr(expr$index, my_id, label_prx = "index: ")
        }
        # recurse_expr(expr$index, my_id, label_prx = "index: ")
      }
      if (!is.null(expr$otherwise)) {
        recurse_expr(expr$otherwise, my_id, label_prx = "otherwise: ")
      }
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

# get_network_data2 <- function(eqn, alias_map = NULL, show_dims = TRUE) {
#   browser()
#   nodes <- list()
#   edges <- list()
#   node_id <- 0
#   id_map <- list()
#
#   add_node <- function(label, group) {
#     key <- paste(group, label, sep = "::")
#     if (!key %in% names(id_map)) {
#       node_id <<- node_id + 1
#       id_map[[key]] <<- node_id
#       nodes[[length(nodes) + 1]] <<- list(id = node_id, label = label, group = group)
#     }
#     id_map[[key]]
#   }
#
#   walk_ast <- function(node, parent_id = NULL) {
#     browser()
#     if (is.null(node)) return(NULL)
#
#     label <- switch(node_type(node),
#                     "variable" = with_dims(node$name, node$dims, show_dims = show_dims),
#                     "parameter" = with_dims(node$name, node$dims, show_dims = show_dims),
#                     "symbol" = node$value,
#                     "constant" = as.character(node$value),
#                     "expression" = node$op,
#                     "unary" = paste0("unary ", node$op),
#                     "when" = "when",
#                     "sum" = paste0("sum(", paste(node$index$dims, collapse = ","), ")"),
#                     "prod" = paste0("prod(", paste(node$index$dims, collapse = ","), ")"),
#                     "mapping" = with_dims(node$name, node$dims, show_dims = show_dims),
#                     # "set" = node$name,
#                     "<unknown>"
#     )
#     group <- node_type(node)
#     this_id <- add_node(label, group)
#
#     if (!is.null(parent_id)) {
#       edges[[length(edges) + 1]] <<- list(from = parent_id, to = this_id)
#     }
#
#     if (group == "expression") {
#       walk_ast(node$lhs, this_id)
#       walk_ast(node$rhs, this_id)
#     } else if (group == "unary") {
#       walk_ast(node$rhs, this_id)
#     } else if (group == "when") {
#       walk_ast(node$condition, this_id)
#       walk_ast(node$then, this_id)
#       walk_ast(node$otherwise, this_id)
#     } else if (group %in% c("variable", "parameter") && !is.null(node$dims)) {
#       # for (dim in node$dims) {
#       #   dim_name <- alias_map[[dim$name]] %||% dim$name
#       #   dim_id <- add_node(dim_name, "index")
#       #   edges[[length(edges) + 1]] <<- list(from = this_id, to = dim_id)
#       # }
#     } else if (group %in% c("sum", "prod")) {
#       # walk_ast(node$domain, this_id)
#       walk_ast(node$index, this_id)
#       walk_ast(node$value, this_id)
#     }
#
#     invisible(this_id)
#   }
#
#   lhs_id <- walk_ast(eqn$lhs)
#   rel_id <- add_node(eqn$relation, "relation")
#   edges[[length(edges) + 1]] <- list(from = lhs_id, to = rel_id)
#   walk_ast(eqn$rhs, rel_id)
#
#   nodes_df <- do.call(rbind.data.frame, lapply(nodes, as.data.frame))
#   edges_df <- do.call(rbind.data.frame, lapply(edges, as.data.frame))
#
#   list(
#     nodes = nodes_df,
#     edges = edges_df,
#     title = eqn$name,
#     subtitle = eqn$desc
#   )
# }


#' Convert a multimod ast or equation to a visNetwork object
#'
#' @param expr A multimod object
#' @export
as_visNetwork <- function(expr) {
  UseMethod("as_visNetwork")
}


#' @export
#' @rdname as_visNetwork
#' @method as_visNetwork equation
as_visNetwork.equation <- function(eq,
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






