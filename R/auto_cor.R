#' Batch Correlation Analysis for Multiple Targets
#'
#' @description
#' Calculates Pearson, Spearman, or Kendall correlation coefficients between
#' multiple target variables and a list of predictor variables.
#' P-value adjustment is performed separately for each target variable.
#'
#' @param data A data frame containing the variables.
#' @param target_vars Character vector. The names of the response variables (e.g., c("density", "biomass")).
#' @param predictors Character vector (Optional). List of predictor variable names.
#' If NULL (default), all other numeric variables in \code{data} (excluding target_vars) will be used.
#' @param method Character string. One of "pearson", "spearman", or "kendall". Defaults to "pearson".
#' @param p_adjust_method Character string. Method for P-value adjustment. Defaults to "fdr".
#'
#' @return A data frame containing correlation results for all targets, sorted by target and correlation strength.
#'
#' @importFrom stats cor.test p.adjust sd complete.cases
#' @importFrom dplyr bind_rows
#' @export
auto_correlation <- function(data,
                              target_vars,
                              predictors = NULL,
                              method = "pearson",
                              p_adjust_method = "fdr") {

  # --- 1. Validation for Targets ---
  # Check if targets exist
  missing_targets <- setdiff(target_vars, names(data))
  if (length(missing_targets) > 0) {
    stop(paste("Target variables not found:", paste(missing_targets, collapse = ", ")))
  }

  # Check if targets are numeric
  non_num_targets <- target_vars[!sapply(data[target_vars], is.numeric)]
  if (length(non_num_targets) > 0) {
    stop(paste("Target variables must be numeric. Non-numeric:", paste(non_num_targets, collapse = ", ")))
  }

  # --- 2. Auto-select Predictors ---
  if (is.null(predictors)) {
    # Select all numeric columns EXCLUDING the target variables
    numeric_cols <- names(data)[sapply(data, is.numeric)]
    predictors <- setdiff(numeric_cols, target_vars)
  }

  # Check predictors existence
  missing_preds <- setdiff(predictors, names(data))
  if (length(missing_preds) > 0) {
    warning(paste("Predictors not found and ignored:", paste(missing_preds, collapse = ", ")))
    predictors <- intersect(predictors, names(data))
  }

  if (length(predictors) == 0) {
    stop("No valid numeric predictors found.")
  }

  message(paste0("Analyzing ", length(target_vars), " target(s) against ", length(predictors), " predictor(s)."))

  # --- 3. Nested Loop Analysis ---
  # Outer loop: Iterate through each target variable
  all_targets_res <- lapply(target_vars, function(current_target) {

    # Inner loop: Iterate through predictors for the current target
    single_target_res <- lapply(predictors, function(pred) {

      x <- data[[current_target]]
      y <- data[[pred]]

      if (!is.numeric(y)) return(NULL)

      # Handle NAs for this specific pair
      valid_idx <- stats::complete.cases(x, y)
      x_clean <- x[valid_idx]
      y_clean <- y[valid_idx]
      n_size <- length(x_clean)

      # Robustness checks (min sample size & variance)
      if (n_size < 3 || stats::sd(x_clean) == 0 || stats::sd(y_clean) == 0) {
        return(NULL) # Silently skip to keep output clean, or use warning if strict
      }

      tryCatch({
        test <- stats::cor.test(x_clean, y_clean, method = method, exact = FALSE)

        return(data.frame(
          Target = current_target,
          Predictor = pred,
          Correlation = as.numeric(test$estimate),
          P_Value = as.numeric(test$p.value),
          n = n_size,
          stringsAsFactors = FALSE
        ))
      }, error = function(e) return(NULL))
    })

    # Combine results for this specific target
    target_df <- dplyr::bind_rows(single_target_res)

    # If no results for this target, return NULL
    if (nrow(target_df) == 0) return(NULL)

    # --- 4. P-value Adjustment (Per Target) ---
    # We adjust p-values within the scope of one target variable.
    target_df$Adj_P_Value <- stats::p.adjust(target_df$P_Value, method = p_adjust_method)

    # Add Stars based on Raw P-value (Standard) or Adj P (Strict)
    target_df$Significance <- ifelse(target_df$P_Value < 0.001, "***",
                                     ifelse(target_df$P_Value < 0.01, "**",
                                            ifelse(target_df$P_Value < 0.05, "*", "NS")))

    # Sort by absolute correlation strength
    target_df <- target_df[order(-abs(target_df$Correlation)), ]

    return(target_df)
  })

  # --- 5. Final Combination ---
  final_df <- dplyr::bind_rows(all_targets_res)

  if (nrow(final_df) == 0) {
    warning("No valid correlations found.")
    return(NULL)
  }

  # Reorder columns
  final_df <- final_df[, c("Target", "Predictor", "Correlation", "P_Value", "Adj_P_Value", "Significance", "n")]

  return(final_df)
}
