#' Compare two dataframes and identify differences
#' 
#' @param df1 First dataframe
#' @param df2 Second dataframe
#' @param key_col Column name(s) to use as key (default: NULL = auto-detect)
#'   If NULL, uses all columns except "value"
#'   Can be a character vector for composite keys
#' @param compare_cols Columns to compare values for (default: all except key)
#' @param label1 Label for first dataframe (default: "df1")
#' @param label2 Label for second dataframe (default: "df2")
#' @param tolerance Numeric tolerance for comparing numeric columns (default: 1e-9)
#' @param ignore.case Logical; if TRUE, perform case-insensitive comparison for character columns (default: FALSE)
#' 
#' @return List with components:
#'   - only_in_1: Rows only in df1
#'   - only_in_2: Rows only in df2
#'   - in_both: Row names that appear in both
#'   - differences: Rows where values differ between df1 and df2
#'   - summary: Text summary of differences
#'   
#' @export
compare_dataframes <- function(df1, df2, key_col = NULL, 
                              compare_cols = NULL,
                              label1 = "df1", label2 = "df2",
                              tolerance = 1e-9,
                              ignore.case = FALSE) {

  df1_work <- df1
  df2_work <- df2

  # Auto-detect key columns if not specified
  if (is.null(key_col)) {
    # Use all columns except common value field names as initial candidates
    candidate_keys <- setdiff(names(df1_work), c("value", "val", "Value"))
    if (length(candidate_keys) == 0) {
      candidate_keys <- names(df1_work)
    }

    candidate_keys <- unique(candidate_keys)
    shared_candidates <- candidate_keys[candidate_keys %in% names(df2_work)]

    if (length(shared_candidates) == 0) {
      common_columns <- intersect(names(df1_work), names(df2_work))
      if (length(common_columns) == 0) {
        stop("Unable to auto-detect key columns: dataframes do not share any columns")
      }
      key_col <- common_columns[1]
      cat(sprintf("Auto-detected key column (forced to first shared column): %s\n", key_col))
    } else {
      dropped_keys <- setdiff(candidate_keys, shared_candidates)
      key_col <- shared_candidates
      if (length(dropped_keys) > 0) {
        cat(sprintf(
          "Auto-detected key columns (removed non-shared columns: %s): %s\n",
          paste(dropped_keys, collapse = ", "),
          paste(key_col, collapse = ", ")
        ))
      } else {
        cat(sprintf("Auto-detected key columns: %s\n", paste(key_col, collapse = ", ")))
      }
    }
  }

  missing_in_df2 <- setdiff(key_col, names(df2_work))
  if (length(missing_in_df2) > 0) {
    stop(sprintf("Key column(s) not found in second dataframe: %s",
                 paste(missing_in_df2, collapse = ", ")))
  }

  build_key_info <- function(df, key_cols, ignore_case = FALSE) {
    if (length(key_cols) == 1) {
      key_vals <- df[[key_cols]]
      display_raw <- as.character(key_vals)
      compare_vals <- if (ignore_case && is.character(key_vals)) tolower(display_raw) else display_raw
    } else {
      key_subset <- df[, key_cols, drop = FALSE]
      display_raw <- apply(key_subset, 1, function(row) paste(row, collapse = "||"))
      if (ignore_case) {
        key_subset[] <- lapply(key_subset, function(col) {
          if (is.character(col)) tolower(col) else col
        })
        compare_vals <- apply(key_subset, 1, function(row) paste(row, collapse = "||"))
      } else {
        compare_vals <- display_raw
      }
    }

    occ <- ave(seq_along(compare_vals), compare_vals, FUN = seq_along)
    internal <- paste(compare_vals, occ, sep = "||__occ__||")

    list(
      display = display_raw,
      internal = internal
    )
  }

  key_info1 <- build_key_info(df1_work, key_col, ignore.case)
  key_info2 <- build_key_info(df2_work, key_col, ignore.case)
  keys1 <- key_info1$internal
  keys2 <- key_info2$internal

  only_in_internal_1 <- setdiff(keys1, keys2)
  only_in_internal_2 <- setdiff(keys2, keys1)
  in_both_internal <- intersect(keys1, keys2)

  # Map internal keys back to display strings for reporting
  only_in_1 <- key_info1$display[match(only_in_internal_1, keys1)]
  only_in_2 <- key_info2$display[match(only_in_internal_2, keys2)]
  in_both <- key_info1$display[match(in_both_internal, keys1)]
  
  cat(sprintf("=== Comparing DataFrames ===\n"))
  cat(sprintf("%s: %d rows\n", label1, nrow(df1_work)))
  cat(sprintf("%s: %d rows\n", label2, nrow(df2_work)))
  cat(sprintf("\n"))
  cat(sprintf("Only in %s: %d\n", label1, length(only_in_1)))
  cat(sprintf("Only in %s: %d\n", label2, length(only_in_2)))
  cat(sprintf("In both: %d\n", length(in_both)))
  cat(sprintf("\n"))
  
  # If compare_cols not specified, use all columns except key
  if (is.null(compare_cols)) {
    compare_cols <- setdiff(names(df1_work), key_col)
    if (length(compare_cols) == 0) {
      compare_cols <- key_col
    }
  }
  
  # For rows in both, check if values differ
  differences <- list()
  if (length(in_both) > 0) {
    df1_work$.__key_internal__ <- keys1
    df2_work$.__key_internal__ <- keys2

    match_idx1 <- match(in_both_internal, df1_work$.__key_internal__)
    match_idx2 <- match(in_both_internal, df2_work$.__key_internal__)
    valid_matches <- !is.na(match_idx1) & !is.na(match_idx2)
    if (!all(valid_matches)) {
      match_idx1 <- match_idx1[valid_matches]
      match_idx2 <- match_idx2[valid_matches]
      in_both_internal <- in_both_internal[valid_matches]
    }
    if (length(in_both_internal) == 0) {
      next
    }

    df1_subset <- df1_work[match_idx1, , drop = FALSE]
    df2_subset <- df2_work[match_idx2, , drop = FALSE]
    
    # Compare each column
    for (col in compare_cols) {
      if (!(col %in% names(df1_subset)) || !(col %in% names(df2_subset))) {
        cat(sprintf("Warning: Column '%s' not in both dataframes\n", col))
        next
      }
      
      # Check for differences (handling numeric tolerance)
      if (is.numeric(df1_subset[[col]]) && is.numeric(df2_subset[[col]])) {
        diffs <- abs(df1_subset[[col]] - df2_subset[[col]]) > tolerance
      } else if (ignore.case && is.character(df1_subset[[col]]) && is.character(df2_subset[[col]])) {
        diffs <- tolower(df1_subset[[col]]) != tolower(df2_subset[[col]])
      } else {
        diffs <- df1_subset[[col]] != df2_subset[[col]]
      }
      
      if (any(diffs, na.rm = TRUE)) {
        # Build result dataframe with key columns and values
        key_cols <- key_col
        result_df <- df1_subset[diffs, key_cols, drop = FALSE]
        result_df[[label1]] <- df1_subset[[col]][diffs]
        result_df[[label2]] <- df2_subset[[col]][diffs]
        
        differences[[col]] <- result_df
        cat(sprintf("Column '%s': %d differences\n", col, length(which(diffs))))
      }
    }
  }
  
  # Build summary
  summary_parts <- c()
  if (length(only_in_1) > 0) {
    summary_parts <- c(summary_parts, sprintf("%d only in %s", length(only_in_1), label1))
  }
  if (length(only_in_2) > 0) {
    summary_parts <- c(summary_parts, sprintf("%d only in %s", length(only_in_2), label2))
  }
  if (length(differences) > 0) {
    summary_parts <- c(summary_parts, sprintf("%d columns with differences", length(differences)))
  }
  
  if (length(summary_parts) == 0) {
    summary_text <- "DataFrames are identical"
  } else {
    summary_text <- paste("Differences found:", paste(summary_parts, collapse = ", "))
  }
  
  result <- list(
    only_in_1 = only_in_1,
    only_in_2 = only_in_2,
    in_both = in_both,
    differences = differences,
    summary = summary_text
  )
  
  cat(sprintf("\n%s\n", summary_text))
  
  invisible(result)
}


#' Quick comparison showing first few differences
#' 
#' @param df1 First dataframe
#' @param df2 Second dataframe  
#' @param key_col Column name to use as key (default: "name")
#' @param n Number of examples to show (default: 10)
#' @param tolerance Numeric tolerance for comparing numeric columns (default: 1e-9)
#' @param ignore.case Logical; if TRUE, perform case-insensitive comparison for character columns (default: FALSE)
#' 
#' @export
quick_compare <- function(df1, df2, key_col = "name", n = 10, tolerance = 1e-9, ignore.case = FALSE) {
  result <- compare_dataframes(df1, df2, key_col = key_col, 
                               label1 = "df1", label2 = "df2",
                               tolerance = tolerance,
                               ignore.case = ignore.case)
  
  cat("\n=== Examples ===\n")
  
  if (length(result$only_in_1) > 0) {
    cat(sprintf("\nFirst %d rows only in df1:\n", min(n, length(result$only_in_1))))
    print(head(df1[df1[[key_col]] %in% result$only_in_1, ], n))
  }
  
  if (length(result$only_in_2) > 0) {
    cat(sprintf("\nFirst %d rows only in df2:\n", min(n, length(result$only_in_2))))
    print(head(df2[df2[[key_col]] %in% result$only_in_2, ], n))
  }
  
  if (length(result$differences) > 0) {
    for (col_name in names(result$differences)[1:min(3, length(result$differences))]) {
      cat(sprintf("\nFirst differences in column '%s':\n", col_name))
      print(head(result$differences[[col_name]], n))
    }
  }
  
  invisible(result)
}


#' Compare CSV files in two directories
#' 
#' Finds CSV files with matching names in two directories and compares them.
#' Reports files that are only in one directory or the other, and shows
#' differences for files that exist in both.
#' 
#' @param path1 Path to first directory
#' @param path2 Path to second directory
#' @param pattern Regular expression pattern to filter CSV files (default: "\\.csv$")
#' @param tolerance Numeric tolerance for comparing numeric columns (default: 1e-9)
#' @param ignore.case Logical; if TRUE, perform case-insensitive comparison (default: FALSE)
#' @param key_col Column name(s) to use as key (default: NULL = auto-detect). If NULL, uses all columns except "value"
#' @param ... Additional arguments passed to compare_dataframes
#' 
#' @return List with components:
#'   - only_in_path1: Files only in path1
#'   - only_in_path2: Files only in path2
#'   - in_both: Files in both directories
#'   - comparisons: Named list of comparison results for each file showing differences
#'   - identical_files: Files that were compared and identical
#'   - different_files: Files that were compared and had differences
#'   
#' @export
compare_csv_files <- function(path1, path2, pattern = "\\.csv$", 
                              tolerance = 1e-9, ignore.case = FALSE,
                              key_col = NULL, ...) {
  
  # Check that directories exist
  if (!dir.exists(path1)) {
    stop(sprintf("Directory does not exist: %s", path1))
  }
  if (!dir.exists(path2)) {
    stop(sprintf("Directory does not exist: %s", path2))
  }
  
  # Find CSV files in both directories
  files1 <- list.files(path1, pattern = pattern, full.names = FALSE)
  files2 <- list.files(path2, pattern = pattern, full.names = FALSE)
  
  cat(sprintf("=== Comparing CSV Files ===\n"))
  cat(sprintf("Path 1: %s (%d files)\n", path1, length(files1)))
  cat(sprintf("Path 2: %s (%d files)\n", path2, length(files2)))
  cat(sprintf("\n"))
  
  # Find files only in one or the other
  only_in_path1 <- setdiff(files1, files2)
  only_in_path2 <- setdiff(files2, files1)
  in_both <- intersect(files1, files2)
  
  cat(sprintf("Only in path1: %d\n", length(only_in_path1)))
  if (length(only_in_path1) > 0) {
    for (f in only_in_path1) {
      cat(sprintf("  - %s\n", f))
    }
  }
  
  cat(sprintf("Only in path2: %d\n", length(only_in_path2)))
  if (length(only_in_path2) > 0) {
    for (f in only_in_path2) {
      cat(sprintf("  - %s\n", f))
    }
  }
  
  cat(sprintf("In both: %d\n", length(in_both)))
  cat(sprintf("\n"))
  
  # Compare files that exist in both directories
  comparisons <- list()
  identical_files <- character()
  different_files <- character()
  if (length(in_both) > 0) {
    for (file in in_both) {
      cat(sprintf("=== Comparing: %s ===\n", file))
      
      # Read CSV files
      df1 <- tryCatch({
        read.csv(file.path(path1, file), stringsAsFactors = FALSE)
      }, error = function(e) {
        cat(sprintf("Error reading %s from path1: %s\n", file, e$message))
        return(NULL)
      })
      
      df2 <- tryCatch({
        read.csv(file.path(path2, file), stringsAsFactors = FALSE)
      }, error = function(e) {
        cat(sprintf("Error reading %s from path2: %s\n", file, e$message))
        return(NULL)
      })
      
      if (!is.null(df1) && !is.null(df2)) {
        # Compare dataframes
        comp_result <- compare_dataframes(
          df1, df2, 
          key_col = key_col,
          label1 = "path1",
          label2 = "path2",
          tolerance = tolerance,
          ignore.case = ignore.case,
          ...
        )
        has_differences <-
          length(comp_result$only_in_1) > 0 ||
          length(comp_result$only_in_2) > 0 ||
          length(comp_result$differences) > 0

        if (has_differences) {
          comparisons[[file]] <- comp_result
          different_files <- c(different_files, file)
          cat(sprintf("Differences recorded for %s\n", file))
        } else {
          identical_files <- c(identical_files, file)
          cat("No differences detected; skipping storage.\n")
        }
      }
      
      cat("\n")
    }
  }
  
  # Overall summary
  cat(sprintf("=== Summary ===\n"))
  cat(sprintf("Files only in path1: %d\n", length(only_in_path1)))
  cat(sprintf("Files only in path2: %d\n", length(only_in_path2)))
  cat(sprintf("Files compared: %d\n", length(in_both)))

  n_identical <- length(identical_files)
  n_different <- length(different_files)
  
  cat(sprintf("  - Identical: %d\n", n_identical))
  cat(sprintf("  - Different: %d\n", n_different))
  if (n_different > 0) {
    cat("Files with differences:\n")
    for (f in different_files) {
      cat(sprintf("  - %s\n", f))
    }
  }
  
  result <- list(
    only_in_path1 = only_in_path1,
    only_in_path2 = only_in_path2,
    in_both = in_both,
    comparisons = comparisons,
    identical_files = identical_files,
    different_files = different_files
  )
  
  invisible(result)
}
