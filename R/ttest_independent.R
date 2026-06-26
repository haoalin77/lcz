#' Independent Sample T-test with Automatic Homogeneity Check
#'
#' @description
#' Performs independent sample t-tests on multiple numeric variables against a grouping variable.
#' The function automatically checks for homogeneity of variance using Levene's Test (median-centered):
#' \itemize{
#'   \item If Levene's p-value > alpha: Assumes equal variance (Standard Student's t-test).
#'   \item If Levene's p-value <= alpha: Assumes unequal variance (Welch's t-test).
#' }
#'
#' @param data A data frame or tibble containing the variables.
#' @param group_var String. The name of the grouping variable (must have exactly 2 levels).
#' @param columns_to_test Character vector (Optional). Names of numeric columns to test. If NULL, all numeric columns (excluding group_var) are selected.
#' @param conf_level Numeric. Confidence level for the interval (default 0.95).
#' @param alpha Numeric. Significance threshold for Levene's test and significance stars (default 0.05).
#'
#' @return A data frame (tibble) containing:
#' \itemize{
#'   \item \code{Variable}: Name of the dependent variable.
#'   \item \code{Mean_Group1}: Mean of the first group.
#'   \item \code{Mean_Group2}: Mean of the second group.
#'   \item \code{Diff_Mean}: Difference in means.
#'   \item \code{t_value}: T-statistic.
#'   \item \code{df}: Degrees of freedom.
#'   \item \code{p_value}: P-value of the t-test.
#'   \item \code{sig}: Significance level (*, **, ***, ns).
#'   \item \code{Levene_p}: P-value of Levene's test.
#'   \item \code{Var_Assumption}: Result of variance assumption ("Equal" or "Unequal").
#'   \item \code{Test_Type}: The actual test performed (Student vs. Welch).
#' }
#'
#' @import dplyr
#' @importFrom car leveneTest
#'
#' @export
ttest_independent <- function(data,
                              group_var,
                              columns_to_test = NULL,
                              conf_level = 0.95,
                              alpha = 0.05) {

  # 1. Check for required packages
  if (!requireNamespace("car", quietly = TRUE)) {
    stop("Package 'car' is required for Levene's Test. Please install it.")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required. Please install it.")
  }

  # 2. Automatic column selection logic
  if (is.null(columns_to_test)) {
    columns_to_test <- names(data)[sapply(data, is.numeric)]
    columns_to_test <- setdiff(columns_to_test, group_var)
  }

  # 3. Validation: Ensure grouping variable is a factor
  if (!is.factor(data[[group_var]])) {
    data[[group_var]] <- as.factor(data[[group_var]])
  }

  # 4. Validation: Check if group variable has exactly 2 levels
  levels_count <- length(levels(data[[group_var]]))
  if (levels_count != 2) {
    stop(paste("Error: The group variable must have exactly 2 levels. Found:", levels_count))
  }

  # 5. Loop through each variable
  results_list <- lapply(columns_to_test, function(var) {

    # Construct formula
    f <- stats::as.formula(paste(var, "~", group_var))

    tryCatch({
      # --- A. Levene's Test for Homogeneity of Variance ---
      # center = median is robust against non-normality
      levene_out <- car::leveneTest(f, data = data, center = stats::median)
      levene_p <- levene_out[["Pr(>F)"]][1]

      # --- B. Decision Logic ---
      # If p > alpha -> Assume Homogeneity -> var.equal = TRUE
      # If p <= alpha -> Assume Heterogeneity -> var.equal = FALSE
      is_homogeneity <- levene_p > alpha

      # --- C. Perform T-test ---
      t_out <- stats::t.test(f, data = data,
                             var.equal = is_homogeneity,
                             conf.level = conf_level)

      # --- D. Format Results ---
      dplyr::tibble(
        Variable = var,
        Mean_Group1 = t_out$estimate[1],
        Mean_Group2 = t_out$estimate[2],
        Diff_Mean = t_out$estimate[1] - t_out$estimate[2],
        t_value = t_out$statistic,
        df = t_out$parameter,
        p_value = t_out$p.value,

        # Significance markers
        sig = dplyr::case_when(
          t_out$p.value < 0.001 ~ "***",
          t_out$p.value < 0.01  ~ "**",
          t_out$p.value < 0.05  ~ "*",
          TRUE                  ~ "ns"
        ),

        # Traceability info
        Levene_p = levene_p,
        Var_Assumption = ifelse(is_homogeneity, "Equal", "Unequal"),
        Test_Type = t_out$method
      )

    }, error = function(e) {
      # Error handling to prevent loop interruption
      dplyr::tibble(
        Variable = var,
        Test_Type = paste("Error:", e$message),
        p_value = NA_real_
      )
    })
  })

  # 6. Combine all results into a single data frame
  final_df <- dplyr::bind_rows(results_list)

  return(final_df)
}
