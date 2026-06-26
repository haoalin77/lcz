#' Format Post-hoc Significance Letters into Wide Format
#'
#' @description
#' Reshapes a long-format data frame containing post-hoc test results
#' (where different factors are tested in separate rows) into a clean,
#' wide format. It automatically trims redundant whitespace from the
#' significance letters and creates separate columns for each tested factor.
#'
#' @param data A data frame containing the long-format post-hoc results.
#' @param factor_col Character string. The name of the column indicating the tested factor. Default is "Test_Factor".
#' @param letter_col Character string. The name of the column containing the significance letters. Default is "letters".
#'
#' @return A wide-format data frame with significance letters in separate columns.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr all_of arrange across
#' @importFrom tidyr pivot_wider
#' @export
format_posthoc_letters <- function(data, factor_col = "Test_Factor", letter_col = "letters") {

  # Step 1: Trim whitespace using Base R.
  # This avoids the need to import complex rlang operators (like := and !!)
  # which frequently cause "no visible binding" Notes in R CMD check.
  data[[letter_col]] <- trimws(data[[letter_col]])

  # Step 2: Reshape and sort using strict package-development standards
  result <- data %>%
    tidyr::pivot_wider(
      names_from = dplyr::all_of(factor_col),
      values_from = dplyr::all_of(letter_col),
      names_prefix = "letter_by_"
    ) %>%
    dplyr::arrange(dplyr::across(1:3))

  return(result)
}
