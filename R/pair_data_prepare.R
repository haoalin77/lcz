#' Reshape Data from Long to Wide with Grouped Variable Sorting
#'
#' @description
#' This function takes a long-format data frame, pivots it to a wide format based on a
#' grouping column (e.g., season), and explicitly reorders the resulting columns.
#' It ensures that all split columns belonging to the same original variable are
#' placed adjacently (e.g., wt_1, wt_2, wt_3, wt_4), rather than grouping by the
#' time point.
#'
#' @param data A data frame. The input data in long format.
#' @param id_col Character string. The name of the column identifying unique subjects/rows
#' (e.g., "site"). Default is "site".
#' @param group_col Character string. The name of the column containing the grouping
#' variable to spread across columns (e.g., "season"). Default is "season".
#' @param sep Character string. The separator used between variable name and group level.
#' Default is "_".
#'
#' @return A data frame in wide format. The first column is the `id_col`, followed by
#' the reshaped data columns sorted by variable blocks.
#'
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr select all_of
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' # Create dummy data
#' data <- data.frame(
#'   site = rep(1:25, each = 4),
#'   season = rep(1:4, 25),
#'   wt = runif(100, 10, 30),
#'   ph = runif(100, 7, 9),
#'   do = runif(100, 5, 10)
#' )
#'
#' # Run the function
#' result <- pair_data_trans(data, id_col = "site", group_col = "season")
#'
#' # Check columns
#' colnames(result)
#' }
#'
#' @export
pair_data_trans <- function(data, id_col = "site", group_col = "season", sep = "_") {

  # 1. Check if required columns exist
  if (!all(c(id_col, group_col) %in% names(data))) {
    stop(paste("Columns", id_col, "or", group_col, "not found in the data frame."))
  }

  # 2. Identify value columns (variables to pivot)
  # These are all columns except the ID and the Group column
  value_cols <- setdiff(names(data), c(id_col, group_col))

  # 3. Construct the glue string for new column names dynamically
  # e.g., "{.value}_{season}"
  glue_spec <- paste0("{.value}", sep, "{", group_col, "}")

  # 4. Pivot from Long to Wide
  # names_sort = TRUE ensures the suffixes (1,2,3,4) are ordered numerically/alphabetically
  wide_raw <- tidyr::pivot_wider(
    data = data,
    id_cols = dplyr::all_of(id_col),
    names_from = dplyr::all_of(group_col),
    values_from = dplyr::all_of(value_cols),
    names_glue = glue_spec,
    names_sort = TRUE
  )

  # 5. Build the sorted column list (Force Grouping by Variable)
  # Start with the ID column
  sorted_cols <- id_col

  # Loop through each original variable to find its corresponding new columns
  for (var in value_cols) {
    # Regex logic: "^var_"
    # Matches strings starting with "var" followed by the separator
    pattern <- paste0("^", var, sep)

    # Grep matching columns from the wide dataframe
    cols_for_this_var <- grep(pattern, names(wide_raw), value = TRUE)

    # Append to the sorting list
    sorted_cols <- c(sorted_cols, cols_for_this_var)
  }

  # 6. Reorder and return
  final_data <- wide_raw %>%
    dplyr::select(dplyr::all_of(sorted_cols))

  return(final_data)
}
