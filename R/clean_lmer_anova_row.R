#' Format and Clean LMER ANOVA Output for Downstream Export
#'
#' @description
#' Takes a raw ANOVA table from a linear mixed-effects model (lmer), 
#' appends tracking metrics (such as the specific test population or subset name), 
#' evaluates categorical significance markers ("*" or "NS"), and structures 
#' columns cleanly using base R primitives to avoid package deployment notes.
#'
#' @param raw_anova An object returned by anova(lmer(...)), typically a data frame 
#'        or matrix containing F-tests and p-values.
#' @param current_test_name A character string indicating the current split dataset 
#'        or subset key (passed directly from the lapply loop tracker).
#'
#' @return A standard data frame with structured columns, ordered perfectly 
#'         for direct Excel workbook appending.
#'
#' @export
clean_lmer_anova_row <- function(raw_anova, current_test_name) {

  if (is.null(raw_anova)) {
    stop("Input 'raw_anova' cannot be NULL.")
  }
  

  formatted_df <- as.data.frame(raw_anova)
  
  if (nrow(formatted_df) == 0) {
    return(formatted_df)
  }
  

  formatted_df[["group"]] <- rownames(formatted_df)
  formatted_df[["pair"]]  <- as.character(current_test_name)
  

  p_value_column_index <- which(names(formatted_df) %in% c("Pr(>F)", "Pr(>Chisq)", "p.value"))
  
  if (length(p_value_column_index) > 0) {
    p_vector <- formatted_df[[p_value_column_index[1]]]
    
    
    significance_flags <- rep("NS", nrow(formatted_df))
    
    
    valid_significant_indices <- !is.na(p_vector) & p_vector <= 0.05
    significance_flags[valid_significant_indices] <- "*"
    
    formatted_df[["sig"]] <- significance_flags
  } else {
    
    formatted_df[["sig"]] <- "NS"
  }
  

  head_columns <- c("pair", "group")
  tail_columns <- "sig"
  

  core_numeric_columns <- setdiff(names(formatted_df), c(head_columns, tail_columns))
  

  optimized_column_sequence <- c(head_columns, core_numeric_columns, tail_columns)
  formatted_df <- formatted_df[, optimized_column_sequence, drop = FALSE]
  

  rownames(formatted_df) <- NULL
  
  return(formatted_df)
}
