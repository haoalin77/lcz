#' Read Data from Clipboard
#'
#' This function reads tabular data from the system clipboard into a data frame.
#' It is particularly useful for quickly importing data copied from spreadsheets
#' or other tabular sources.
#'
#' @param sep Character string specifying the field separator. Default is "\\t" (tab).
#' @param header Logical value indicating whether the data has a header row. Default is TRUE.
#' @param ... Additional arguments passed to \code{\link[utils]{read.table}}.
#'
#' @return A data frame containing the data from the clipboard.
#'
#' @details
#' This function is a wrapper around \code{read.table("clipboard", ...)} and is
#' primarily useful in interactive R sessions. It may not work in all environments,
#' particularly in non-interactive sessions or on some operating systems.
#'
#' @note
#' - On Windows, this function uses the system clipboard
#' - On macOS, it uses the pbpaste command
#' - On Linux, it may require xclip or xsel to be installed
#'
#' @examples
#' \dontrun{
#' # Copy some tabular data to your clipboard (e.g., from Excel)
#' # Then run:
#' my_data <- read_clipboard()
#'
#' # If your data uses a different separator (e.g., comma):
#' my_data <- read_clipboard(sep = ",")
#'
#' # If your data doesn't have a header:
#' my_data <- read_clipboard(header = FALSE)
#' }
#'
#' @seealso
#' \code{\link[utils]{read.table}} for the underlying function used to read the data
#' \code{\link[clipr]{read_clip}} for a more robust clipboard reading function
#'
#' @export
read_clipboard <- function(sep = "\t", header = TRUE, ...) {
  # Read data from clipboard
  data <- utils::read.table("clipboard", sep = sep, header = header, ...)

  return(data)
}
