#' Automatically Detect and Convert Experimental Design Columns to Factors
#'
#' @description
#' Scans a data frame to detect grouping factors or experimental block columns
#' based on sequential run-length patterns (consecutive identical values) or
#' character representations. Columns with zero variance (all values identical)
#' are explicitly bypassed to protect downstream ANOVA operations.
#'
#' @param data A data frame containing potential experimental grouping columns.
#' @param min_consecutive Integer. The minimum length of a consecutive
#'        identical value sequence to trigger automatic factor conversion. Default is 3.
#' @param max_unique_for_periodic Integer. Maximum number of unique values allowed
#'        for a column to be considered as a periodic repetition pattern.
#'        Default is 20. Set to NULL to disable periodic detection.
#'
#' @return A data frame with verified experimental factors properly converted.
#'
#' @importFrom stats na.omit
#' @export
auto_convert_factor <- function(data, min_consecutive = 3, max_unique_for_periodic = 20) {
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data frame.")
  }

  n_rows <- nrow(data)
  if (n_rows == 0) {
    return(data)
  }

  col_names <- names(data)

  cat("\n", rep("=", 45), "\n", "STARTING EXPERIMENTAL FACTOR IDENTIFICATION", "\n", rep("=", 45), "\n", sep = "")

  for (col in col_names) {
    x <- data[[col]]


    if (is.factor(x)) {
      cat("  - Column [", col, "] is already a factor. Skipped.\n", sep = "")
      next
    }

    # Pre-check: Extract unique elements omitting missing values
    x_clean <- stats::na.omit(x)
    if (length(x_clean) == 0) next

    unique_vals <- unique(x_clean)
    n_unique <- length(unique_vals)

    # Absolute Guard: If a column has only 1 unique value, it possesses zero variance.
    if (n_unique <= 1) {
      cat("  - Column [", col, "] bypassed (Zero variance / Only 1 unique value).\n", sep = "")
      next
    }

    is_factor_candidate <- FALSE
    detection_reason <- ""

    # Heuristic 1: If explicitly stored as text/character strings
    if (is.character(x)) {
      is_factor_candidate <- TRUE
      detection_reason <- "Standard character grouping column"
    }
    # Heuristic 2: For numeric indices representing blocks or treatment values
    else if (is.numeric(x) || is.integer(x)) {
      # Leverage Run Length Encoding to calculate consecutive groupings
      run_structures <- rle(as.vector(x))
      max_consecutive_run <- max(run_structures$lengths, na.rm = TRUE)

      # Target check: sequential repetitions present, but it's not a continuous unique primary key
      if (max_consecutive_run >= min_consecutive && n_unique < (n_rows * 0.5)) {
        is_factor_candidate <- TRUE
        detection_reason <- paste0("Consecutive experimental block detected (Max run length: ", max_consecutive_run, ")")
      } else if (!is.null(max_unique_for_periodic) && n_unique <= max_unique_for_periodic && n_rows >= 2 * n_unique) {

        template <- c()
        seen <- c()
        for (val in x_clean) {
          if (!(val %in% seen)) {
            template <- c(template, val)
            seen <- c(seen, val)
            if (length(template) == n_unique) break
          }
        }

        if (length(template) == n_unique) {

          rep_times <- ceiling(n_rows / n_unique)
          pattern <- rep(template, times = rep_times)
          pattern <- pattern[1:n_rows]


          non_na_idx <- !is.na(x)
          if (sum(non_na_idx) > 0) {
            match_ratio <- sum(x[non_na_idx] == pattern[non_na_idx], na.rm = TRUE) / sum(non_na_idx)

            if (match_ratio >= 0.9) {
              is_factor_candidate <- TRUE
              detection_reason <- paste0("Periodic repetition detected (pattern length = ", n_unique, ", match ratio = ", round(match_ratio, 3), ")")
            }
          }
        }
      }
    }

    # Execute atomic transformation
    if (is_factor_candidate) {
      cat("  ✔ Column [", col, "] safely converted to FACTOR.\n",
          "    [Reason]: ", detection_reason, "\n", sep = "")
      data[[col]] <- factor(data[[col]])
    }
  }

  cat(rep("=", 45), "\n", "FACTOR CONVERSION PROCESS COMPLETE", "\n\n", sep = "")
  return(data)
}
