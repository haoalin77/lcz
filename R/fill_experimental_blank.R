#' Smart Infill of Experimental Design Excel Blank Cells
#'
#' @description
#' Detects and automatically repairs the "implicit duplication intent" in scientific
#' Excel spreadsheets, where investigators write a factor label (e.g., Treatment, Genotype)
#' only once on the first replicate row and leave downstream consecutive replicate rows blank.
#' It fills down placeholders (empty strings or NAs) safely until the next explicit
#' block label arrives, and then upgrades those columns to compliant factors.
#'
#' @param data A data frame imported from Excel containing structural missing values.
#' @param min_consecutive_blank Integer. The minimum number of consecutive trailing
#'        blanks/NAs that must follow a valid label to qualify the column as an
#'        experimental design factor layout. Default is 2.
#'
#' @return A data frame with implicit structural blanks fully infilled and converted to factors.
#'
#' @importFrom stats na.omit
#' @export
fill_experimental_blank <- function(data, min_consecutive_blank = 2) {
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data frame.")
  }

  n_rows <- nrow(data)
  if (n_rows <= 1) {
    return(data)
  }

  col_names <- names(data)

  cat("\n", rep("=", 45), "\n", "STARTING SCIENTIFIC BLANK INFILL PIPELINE", "\n", rep("=", 45), "\n", sep = "")

  for (col in col_names) {
    x <- data[[col]]

    # Pre-processing: Standardize empty string variants commonly found in Excel to true R NAs
    # Explicit loop avoid tidyverse global variable mapping notes
    x_char <- as.character(x)
    x_char[is.na(x_char)] <- "NA"
    x_char[trimws(x_char) == ""] <- "NA"

    # Detect the pattern: How many consecutive "NA" strings exist?
    run_structures <- rle(x_char)
    lengths_vector <- run_structures$lengths
    values_vector  <- run_structures$values

    # Identify if this column contains long blocks of missing values followed by non-missing values
    has_blank_blocks <- FALSE
    na_indices <- which(values_vector == "NA")

    if (length(na_indices) > 0) {
      max_blank_len <- max(lengths_vector[na_indices], na.rm = TRUE)
      # Ensure there's at least some valid non-NA data (values_vector has more than just "NA")
      if (max_blank_len >= min_consecutive_blank && length(unique(values_vector)) > 1) {
        has_blank_blocks <- TRUE
      }
    }

    # If the layout template matches the Excel "omitted intention", perform safe localized forward-fill
    if (has_blank_blocks) {
      cat("  ✔ Column [", col, "] matching Excel structural blank pattern.\n", sep = "")

      current_valid_label <- NA
      filled_vector <- vector("character", length = n_rows)

      for (i in seq_len(n_rows)) {
        val_str <- trimws(as.character(x[i]))

        # Check if the cell contains a real, non-empty label
        if (length(val_str) > 0 && !is.na(val_str) && val_str != "") {
          current_valid_label <- val_str
          filled_vector[i] <- current_valid_label
        } else {
          # Cell is empty, infer intention from the trailing valid label upstream
          if (!is.na(current_valid_label)) {
            filled_vector[i] <- current_valid_label
          } else {
            # Absolute fallback if the very first row starts with a blank
            filled_vector[i] <- "Unassigned_Control"
          }
        }
      }

      # Commit changes back via Base R matrix-index primitives and upgrade to clean factor
      data[[col]] <- factor(filled_vector)
      cat("    -> Blanks infilled successfully and converted to FACTOR.\n")
    }
  }

  cat(rep("=", 45), "\n", "BLANK INFILL PIPELINE PROCESS COMPLETE", "\n\n", sep = "")
  return(data)
}
