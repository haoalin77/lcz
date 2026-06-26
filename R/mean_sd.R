#' Calculate the mean and standard error by group
#'
#' @description
#' This function calculates the mean and standard error (sd/sqrt(n)) for all numeric columns
#' in a dataset, grouped by a specified factor variable.
#'
#' @param factor Character string specifying the name of the grouping variable in the data.
#' @param data A data frame containing the variables to be summarized.
#'
#' @return A data frame with summary statistics (mean and standard error) for each numeric
#' variable, grouped by the specified factor. The output columns are named using the pattern
#' \code{{variable_name}_{statistic}}.
#'
#' @details
#' The function automatically selects numeric columns for calculation (excluding the grouping variable)
#' and computes:
#' \itemize{
#'   \item Mean: \code{mean(., na.rm = TRUE)}
#'   \item Standard Error: \code{sd(., na.rm = TRUE) / sqrt(n())}
#' }
#'
#' Note that this function calculates Standard Error (SE), not Standard Deviation (SD),
#' despite the function name. Standard error represents the variability of the sample mean estimate.
#'
#' @examples
#' \dontrun{
#' # Create sample data
#' data <- data.frame(
#'   pla = rep(c("A", "B"), each = 50),
#'   var1 = rnorm(100),
#'   var2 = rnorm(100),
#'   var3 = rnorm(100)
#' )
#'
#' # Calculate mean and standard error by group
#' result <- mean_sd("pla", data)
#' print(result)
#' }
#'
#' @importFrom dplyr group_by summarise across all_of n %>%
#' @export
mean_sd <- function(factor, data) {

  # 1. Identify numeric columns (excluding the grouping factor itself)
  # We check is.numeric to avoid errors on character columns
  numeric_cols <- names(data)[sapply(data, is.numeric)]
  columns_to_summarize <- setdiff(numeric_cols, factor)

  # Check if there are columns to summarize
  if (length(columns_to_summarize) == 0) {
    warning("No numeric columns found to summarize.")
    return(NULL)
  }

  # 2. Calculate Mean and Standard Error
  summary_stats <- data %>%
    group_by(.data[[factor]]) %>%
    summarise(
      across(
        all_of(columns_to_summarize),
        list(
          mean = ~mean(., na.rm = TRUE),
          sd = ~sd(., na.rm = TRUE) / sqrt(dplyr::n())
        ),
        .names = "{.col}_{.fn}"
      ),
      .groups = "drop"
    )

  return(summary_stats)
}
