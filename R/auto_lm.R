#' Batch Linear Mixed Model Analysis
#'
#' @description
#' Performs batch linear mixed effects model (LMM) analysis on multiple response variables.
#' It optionally applies Log(x+1) transformation, computes ANOVA tables with P-values,
#' and performs post-hoc pairwise comparisons.
#'
#' @param data A data frame containing all variables.
#' @param response_cols A character vector specifying the numeric response variables to analyze (e.g., c("density", "biomass")).
#' @param fixed_var A character string specifying the fixed effect grouping variable (e.g., "season").
#' @param random_var A character string specifying the random effect grouping variable (e.g., "site").
#' @param log_transform Logical. Defaults to TRUE. Whether to apply log(x + 1) transformation to response variables.
#' @param output_file Character string. Defaults to NULL. If specified (e.g., "results.xlsx"), results will be exported to an Excel file.
#'
#' @return A list containing two data frames:
#' \item{main_results}{Data frame containing ANOVA main effects and P-values.}
#' \item{pair_results}{Data frame containing pairwise comparison results.}
#'
#' @importFrom lmerTest lmer
#' @importFrom emmeans emmeans
#' @importFrom dplyr select bind_rows %>% everything
#' @importFrom openxlsx write.xlsx
#' @importFrom stats anova as.formula reformulate
#' @importFrom methods is
#'
#' @export
lm_analyze <- function(data,
                              response_cols,
                              fixed_var,
                              random_var,
                              log_transform = TRUE,
                              output_file = NULL) {

  # No library() calls here! Dependencies are handled by NAMESPACE.

  # Data preprocessing: Ensure grouping variables are factors
  data[[fixed_var]] <- as.factor(data[[fixed_var]])
  data[[random_var]] <- as.factor(data[[random_var]])

  message(paste0("Starting analysis... Total variables: ", length(response_cols)))

  # Loop through response columns
  all_results <- lapply(response_cols, function(col) {

    tryCatch({
      # A. Transformation logic
      if (log_transform) {
        target_col <- paste0("log_", col)
        # Log(x + 1) to avoid -Inf
        data[[target_col]] <- log(data[[col]] + 1)
      } else {
        target_col <- col
      }

      # B. Build formula
      # Format: target ~ fixed + (1 | random)
      f_str <- paste0(target_col, " ~ ", fixed_var, " + (1 | ", random_var, ")")
      model_formula <- as.formula(f_str)

      # C. Run Model
      # Using lmer from lmerTest to ensure P-values are calculated in ANOVA
      model <- lmerTest::lmer(model_formula, data = data)

      # D. Extract ANOVA Main Effects
      # Explicitly use stats::anova, though lmerTest method will intercept
      anova_res <- stats::anova(model)
      main_df <- as.data.frame(anova_res)
      main_df$Term <- rownames(main_df)
      main_df$Dependent_Var <- target_col

      # Add significance stars
      # Dynamic lookup for Pr(>F) column
      p_col_idx <- grep("Pr\\(>F\\)", colnames(main_df))
      if (length(p_col_idx) > 0) {
        p_vals <- main_df[, p_col_idx]
        main_df$Significance <- ifelse(p_vals < 0.001, "***",
                                       ifelse(p_vals < 0.01, "**",
                                              ifelse(p_vals < 0.05, "*", "NS")))
      }

      # Reorder columns (dplyr::select)
      main_df <- main_df %>% select("Dependent_Var", "Term", everything())

      # E. Post-hoc Pairwise Comparisons
      # Dynamic formula for emmeans
      em_formula <- as.formula(paste("pairwise ~", fixed_var))
      em_res <- emmeans::emmeans(model, em_formula)
      pair_df <- as.data.frame(summary(em_res$contrasts)) # Default is tukey

      pair_df$Dependent_Var <- target_col
      pair_df$Significance <- ifelse(pair_df$p.value < 0.001, "***",
                                     ifelse(pair_df$p.value < 0.01, "**",
                                            ifelse(pair_df$p.value < 0.05, "*", "NS")))

      pair_df <- pair_df %>% select("Dependent_Var", everything())

      return(list(main = main_df, pair = pair_df))

    }, error = function(e) {
      warning(paste("Variable", col, "failed:", e$message))
      return(NULL)
    })
  })

  # --- Result Merging ---
  # Filter out NULLs
  all_results <- all_results[!sapply(all_results, is.null)]

  if (length(all_results) == 0) {
    stop("All variables failed analysis. Please check your data.")
  }

  final_main <- bind_rows(lapply(all_results, function(x) x$main))
  final_pair <- bind_rows(lapply(all_results, function(x) x$pair))

  result_list <- list(
    "ANOVA_Main_Effects" = final_main,
    "Pairwise_Comparisons" = final_pair
  )

  # --- Export ---
  if (!is.null(output_file)) {
    write.xlsx(result_list, output_file)
    message(paste("Results saved to:", output_file))
  }

  return(result_list)
}

# Define global variables to satisfy R CMD check (for dplyr pipes)
utils::globalVariables(c("Dependent_Var", "Term", "p.value", "everything"))
