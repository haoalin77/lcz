#' Clean Excel Column Names for Safe R Formula Usage
#'
#' @description
#' Automatically cleans non-syntactic column names imported from Excel (e.g.,
#' names containing spaces, brackets, percentage signs, or mathematical operators)
#' by replacing problematic characters with formula-safe underscores. It yields
#' a clean data frame alongside an internal mapping table to preserve the
#' original formatting for downstream publication.
#'
#' @param data A data frame containing column names that require sanitization.
#'
#' @return A list containing two elements:
#' \itemize{
#'   \item \code{data}: The data frame with sanitized, formula-safe column names.
#'   \item \code{mapping}: A data frame containing two columns: \code{original_name}
#'         and \code{cleaned_name}, maintaining the relationship for future restoration.
#' }
#'
#' @importFrom stats setNames
#' @export
clean_tablenames <- function(data) {
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data frame.")
  }

  orig_names <- names(data)

  # Step 1: Replace all non-alphanumeric characters with underscores
  clean_names <- gsub("[^a-zA-Z0-9_]", "_", orig_names)

  # Step 2: Collapse multiple consecutive underscores into a single underscore
  clean_names <- gsub("_+", "_", clean_names)

  # Step 3: Strip leading and trailing underscores
  clean_names <- gsub("^_+|_+$", "", clean_names)

  # Step 4: Ensure names do not start with a digit (prepend 'X')
  clean_names <- gsub("^([0-9])", "X\\1", clean_names)

  # Step 5: Handle fallback for names that became completely empty
  clean_names[clean_names == ""] <- "var"

  # Step 6: Guarantee absolute uniqueness of the cleaned column names
  clean_names <- base::make.unique(clean_names, sep = "_")

  # Create mapping table using strict explicit namespace imports
  mapping_df <- data.frame(orig_names, clean_names, stringsAsFactors = FALSE)
  mapping_df <- stats::setNames(mapping_df, c("original_name", "cleaned_name"))

  # Assign clean names back to the data frame via Base R primitive
  names(data) <- clean_names

  return(list(
    data = data,
    mapping = mapping_df
  ))
}
