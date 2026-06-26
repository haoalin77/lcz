#' Batch Convert Columns to Factors
#'
#' Efficiently converts multiple specified columns in a data frame to factors.
#'
#' @param data A data frame.
#' @param cols A character vector of column names to be converted.
#'
#' @return A data frame with the specified columns converted to factors.
#' @export
#' @examples
#' \dontrun{
#'   # Convert "Temp" and "Evol" to factors
#'   data <- convert_to_factor(data, c("Temp", "Evol"))
#' }
convert_to_factor <- function(data, cols) {


  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data frame.")
  }


  missing_cols <- setdiff(cols, names(data))
  if (length(missing_cols) > 0) {
    stop(paste("The following columns were not found in the data:",
               paste(missing_cols, collapse = ", ")))
  }


  for (col in cols) {
    data[[col]] <- as.factor(data[[col]])
  }

  return(data)
}
