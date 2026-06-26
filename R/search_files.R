#' Advanced File Search Function
#' file_ending = "2\.xlsx$",file_starting = "data_",file_contains = "results"
#'
#' This function provides flexible file search capabilities based on different parts of filenames.
#'
#' @param file_ending Character, specifies the ending part of the filename (e.g., "result.xlsx" or ".xlsx")
#' @param file_starting Character, specifies the starting part of the filename (e.g., "data_")
#' @param file_contains Character, specifies text contained in the filename
#' @param search_directory Character, specifies the folder path to search. If NULL (default),
#'   uses the current working directory
#' @param search_subfolders Logical, whether to search in subfolders, defaults to FALSE
#' @param ignore_case Logical, whether to ignore case when searching, defaults to FALSE
#' @param full_paths Logical, whether to return full file paths, defaults to FALSE
#'
#' @return Returns a character vector containing filenames or full paths that match the criteria
#'
#' @details
#' The function uses regular expressions to match filenames and allows combining multiple search criteria.
#' For example, you can search for files that start with "report", contain "2023", and end with ".pdf".
#'
#' @examples
#' \dontrun{
#' # Search for all Excel files
#' excel_files <- search_files(file_ending = "\\.xlsx$")
#'
#' # Search for files starting with "data" and ending with ".csv"
#' data_files <- search_files(file_starting = "data_", file_ending = "\\.csv$")
#'
#' # Search for all files containing "results" (including subfolders)
#' result_files <- search_files(file_contains = "results", search_subfolders = TRUE)
#'
#' # Search for files in a specific directory and return full paths
#' custom_files <- search_files(
#'   file_ending = "result\\.xlsx$",
#'   search_directory = "C:/Users/Username/Documents",
#'   full_paths = TRUE
#' )
#' }
#'
#' @export
search_files <- function(file_ending = NULL, file_starting = NULL, file_contains = NULL,
                         search_directory = NULL, search_subfolders = FALSE,
                         ignore_case = FALSE, full_paths = FALSE) {
  # If no directory specified, use current working directory
  if (is.null(search_directory)) {
    search_directory <- getwd()
  }

  # Verify directory exists
  if (!dir.exists(search_directory)) {
    stop("Specified directory does not exist: ", search_directory)
  }

  # Build regex pattern
  pattern_parts <- character(0)

  if (!is.null(file_starting)) {
    pattern_parts <- c(pattern_parts, paste0("^", file_starting))
  }

  if (!is.null(file_contains)) {
    pattern_parts <- c(pattern_parts, file_contains)
  }

  if (!is.null(file_ending)) {
    pattern_parts <- c(pattern_parts, paste0(file_ending, "$"))
  }

  # If no pattern specified, match all files
  if (length(pattern_parts) == 0) {
    pattern <- ".*"
  } else {
    pattern <- paste(pattern_parts, collapse = ".*")
  }

  # Get matching file list
  file_list <- list.files(
    path = search_directory,
    pattern = pattern,
    full.names = full_paths,
    recursive = search_subfolders,
    ignore.case = ignore_case
  )

  return(file_list)
}
