#' Convert Wide Format Data to Long Format
#'
#' This function converts data from wide format to long format using tidyr::pivot_longer.
#'
#' @param data A data frame containing the wide format data
#' @param id_column Character, the name of the identifier column. Default is "SampleID"
#' @param variable_column Character, the name for the column storing original column names. Default is "Variable"
#' @param value_column Character, the name for the column storing values. Default is "Value"
#' @param auto_create_id Logical, whether to automatically create an ID column if it doesn't exist. Default is TRUE
#'
#' @return A data frame in long format
#'
#' @examples
#' # Create sample data
#' sample_data <- data.frame(
#'   SampleID = c("A", "B", "C"),
#'   Height = c(170, 165, 180),
#'   Weight = c(65, 60, 75),
#'   Age = c(25, 30, 28)
#' )
#'
#' # Basic usage
#' long_data <- convert_to_long(sample_data)
#'
#' # Custom column names
#' long_data_custom <- convert_to_long(
#'   sample_data,
#'   variable_column = "Measurement",
#'   value_column = "Result"
#' )
#'
#' @export
#' @importFrom tidyr pivot_longer
convert_to_long <- function(data,
                            id_column = "SampleID",
                            variable_column = "Variable",
                            value_column = "Value",
                            auto_create_id = TRUE) {

  # Check if tidyr is available
  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop("tidyr package is required")
  }

  # Parameter validation
  if (!is.data.frame(data)) {
    stop("Input data must be a data frame")
  }

  if (nrow(data) == 0) {
    stop("Input data is empty")
  }

  # Check if id_column exists
  if (!id_column %in% colnames(data)) {
    if (auto_create_id) {
      # Automatically create identifier column
      data[[id_column]] <- paste0("Obs", seq_len(nrow(data)))
      message("Automatically created identifier column '", id_column, "'")
    } else {
      stop("Identifier column '", id_column, "' does not exist in the data. ",
           "Set auto_create_id = TRUE to automatically create an identifier column.")
    }
  }

  # Determine columns to pivot (exclude id_column)
  cols_to_pivot <- setdiff(colnames(data), id_column)

  # Check if there are columns to convert
  if (length(cols_to_pivot) == 0) {
    stop("No columns available for conversion")
  }

  # Convert to long format
  long_df <- tidyr::pivot_longer(
    data,
    cols = tidyselect::all_of(cols_to_pivot),
    names_to = variable_column,
    values_to = value_column
  )

  return(long_df)
}
