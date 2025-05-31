#' Plot a multimod AST using visNetwork
#'
#' @param x A ast object
#' @param ... Additional arguments (currently unused)
#'
#' @return A visNetwork plot
#' @noRd
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








