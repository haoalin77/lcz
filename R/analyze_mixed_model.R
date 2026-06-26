# --- Handle Global Variables for R CMD check ---
utils::globalVariables(c(
  "Variable", "Term", "Interaction_Status", "Pr(>F)", "Sig",
  "Comparison_Type", ".group", "p.value", "Letters"
))

#' Batch Mixed Model Analysis (Two Fixed Effects + One Random Effect)
#'
#' This function performs batch analysis for multiple numeric response variables.
#' It constructs a linear mixed model (`lmer`) with the formula:
#' `Response ~ Fixed1 * Fixed2 + (1|Random)`.
#' It performs Type III ANOVA and automatically selects post-hoc analysis strategies:
#' \itemize{
#'   \item \strong{Significant Interaction (P < 0.05)}: Performs simple effects analysis (slicing).
#'   \item \strong{Non-significant Interaction}: Performs main effects analysis.
#' }
#'
#' @param data A data frame containing the response variables and factors.
#' @param fixed_factors A character vector of length 2. Specifies the column names of the two fixed effects.
#' @param random_factor A character string. Specifies the column name of the random effect.
#' @param response_cols (Optional) A character vector. Specifies the column names of response variables to analyze. If NULL, the function automatically selects all numeric columns excluding the factors.
#' @param adjust_method A character string. The adjustment method for p-values in emmeans (default is "sidak"). Options include "tukey", "bonferroni", "none", etc.
#'
#' @return A list containing three data frames:
#' \itemize{
#'   \item \code{ANOVA_Results}: Summary of Type III ANOVA for all variables.
#'   \item \code{Pairwise_Comparisons}: P-values for pairwise comparisons (adjusted based on interaction).
#'   \item \code{ABC_Letters}: Compact Letter Display (CLD) for significant differences.
#' }
#'
#' @importFrom lmerTest lmer
#' @importFrom emmeans emmeans
#' @importFrom multcomp cld
#' @importFrom multcompView multcompLetters
#' @importFrom dplyr select mutate case_when bind_rows everything
#' @importFrom stats anova as.formula setNames
#' @importFrom utils globalVariables
#' @importFrom graphics pairs
#'
#' @export
analyze_mixed_model <- function(data,
                                fixed_factors,
                                random_factor,
                                response_cols = NULL,
                                adjust_method = "sidak") {

  # --- 0. Argument Checks ---
  if (length(fixed_factors) != 2) {
    stop("Argument 'fixed_factors' must contain exactly two variable names.")
  }

  # Ensure columns exist and convert to factors
  for (col in c(fixed_factors, random_factor)) {
    if (!col %in% colnames(data)) {
      stop(paste("Column not found in data:", col))
    }
    data[[col]] <- as.factor(data[[col]])
  }

  # Determine response variables
  if (is.null(response_cols)) {
    # Auto-select numeric columns that are not predictors
    response_cols <- colnames(data)[sapply(data, is.numeric)]
    response_cols <- setdiff(response_cols, c(fixed_factors, random_factor))
  }

  f1 <- fixed_factors[1]
  f2 <- fixed_factors[2]
  rnd <- random_factor

  # --- Internal Helper: Extract Pairs and CLD ---
  get_emm_res <- function(emm_obj, adj_meth, comparison_lbl) {
    # A. Pairs (Fixed: Use graphics::pairs generic, not emmeans::pairs)
    pairs_obj <- pairs(emm_obj, adjust = adj_meth)
    pairs_df <- as.data.frame(pairs_obj)
    pairs_df$Comparison_Type <- comparison_lbl

    # B. CLD (Compact Letter Display)
    # Note: multcomp::cld relies on multcompView
    cld_obj <- multcomp::cld(emm_obj, alpha = 0.05, Letters = letters, adjust = adj_meth)
    cld_df <- as.data.frame(cld_obj)
    cld_df$Comparison_Type <- comparison_lbl

    # Clean .group column if it exists
    if (".group" %in% colnames(cld_df)) {
      cld_df$Letters <- trimws(cld_df$.group)
      cld_df <- dplyr::select(cld_df, -c(.group))
    }

    return(list(pairs = pairs_df, cld = cld_df))
  }

  # --- Loop Analysis ---
  results_list <- lapply(response_cols, function(var_name) {
    tryCatch({
      # === A. Modeling ===
      # Construct formula: y ~ factor(A) * factor(B) + (1|Random)
      formula_str <- paste0("`", var_name, "` ~ ",
                            "factor(`", f1, "`) * factor(`", f2, "`) + ",
                            "(1|`", rnd, "`)")

      fit1 <- lmerTest::lmer(stats::as.formula(formula_str), data = data)

      # === B. ANOVA ===
      fit_aov <- stats::anova(fit1, type = 3)

      # Check Interaction (rows containing ":")
      interaction_row <- grep(":", rownames(fit_aov))
      if (length(interaction_row) > 0) {
        p_interaction <- fit_aov$`Pr(>F)`[interaction_row]
        is_interaction_sig <- p_interaction < 0.05
      } else {
        is_interaction_sig <- FALSE
      }

      status_label <- ifelse(is_interaction_sig, "Significant Interaction", "Non-Sig Interaction")

      # Tidy ANOVA table
      fit_aov_df <- as.data.frame(fit_aov)
      fit_aov_df$Variable <- var_name
      fit_aov_df$Term <- rownames(fit_aov_df)
      fit_aov_df$Interaction_Status <- status_label
      fit_aov_df$Sig <- dplyr::case_when(
        fit_aov_df$`Pr(>F)` < 0.001 ~ "***",
        fit_aov_df$`Pr(>F)` < 0.01  ~ "**",
        fit_aov_df$`Pr(>F)` < 0.05  ~ "*",
        TRUE ~ "NS"
      )
      fit_aov_df <- dplyr::select(fit_aov_df, Variable, Term, Interaction_Status, `Pr(>F)`, Sig, dplyr::everything())

      # === C. Post-hoc Analysis ===
      pairs_list <- list()
      cld_list <- list()

      if (is_interaction_sig) {
        # >>> Path 1: Significant Interaction -> Simple Effects <<<

        # 1.1 Fix F2, compare F1
        spec_formula_1 <- stats::as.formula(paste0("~ `", f1, "` | `", f2, "`"))
        emm_1 <- emmeans::emmeans(fit1, specs = spec_formula_1)
        res_1 <- get_emm_res(emm_1, adjust_method, paste0("Simple Effect: ", f1, " within ", f2))

        pairs_list[[1]] <- res_1$pairs
        cld_list[[1]]   <- res_1$cld

        # 1.2 Fix F1, compare F2
        spec_formula_2 <- stats::as.formula(paste0("~ `", f2, "` | `", f1, "`"))
        emm_2 <- emmeans::emmeans(fit1, specs = spec_formula_2)
        res_2 <- get_emm_res(emm_2, adjust_method, paste0("Simple Effect: ", f2, " within ", f1))

        pairs_list[[2]] <- res_2$pairs
        cld_list[[2]]   <- res_2$cld

      } else {
        # >>> Path 2: Non-sig Interaction -> Main Effects <<<

        # 2.1 F1 Main Effect
        spec_formula_main_1 <- stats::as.formula(paste0("~ `", f1, "`"))
        emm_main_1 <- emmeans::emmeans(fit1, specs = spec_formula_main_1)
        res_main_1 <- get_emm_res(emm_main_1, adjust_method, paste0("Main Effect: ", f1))

        # Fill missing column for merging
        try({ res_main_1$cld[[f2]] <- "All" }, silent = TRUE)

        pairs_list[[1]] <- res_main_1$pairs
        cld_list[[1]]   <- res_main_1$cld

        # 2.2 F2 Main Effect
        spec_formula_main_2 <- stats::as.formula(paste0("~ `", f2, "`"))
        emm_main_2 <- emmeans::emmeans(fit1, specs = spec_formula_main_2)
        res_main_2 <- get_emm_res(emm_main_2, adjust_method, paste0("Main Effect: ", f2))

        try({ res_main_2$cld[[f1]] <- "All" }, silent = TRUE)

        pairs_list[[2]] <- res_main_2$pairs
        cld_list[[2]]   <- res_main_2$cld
      }

      # === D. Merge Single Variable Results ===
      # Merge Pairs
      all_pairs_var <- dplyr::bind_rows(pairs_list)
      all_pairs_var$Variable <- var_name
      all_pairs_var$Interaction_Status <- status_label
      all_pairs_var$Sig <- ifelse(all_pairs_var$p.value < 0.05, "*", "NS")
      all_pairs_var <- dplyr::select(all_pairs_var, Variable, Interaction_Status, Comparison_Type, dplyr::everything())

      # Merge CLD
      all_cld_var <- dplyr::bind_rows(cld_list)
      all_cld_var$Variable <- var_name
      all_cld_var$Interaction_Status <- status_label
      all_cld_var <- dplyr::select(all_cld_var, Variable, Interaction_Status, Comparison_Type, dplyr::everything())

      return(list(anova = fit_aov_df, pairs = all_pairs_var, cld = all_cld_var))

    }, error = function(e) {
      message(paste("Error processing variable:", var_name, "-", e$message))
      return(NULL)
    })
  })

  # --- Final Merge ---
  results_list <- results_list[!sapply(results_list, is.null)]

  if (length(results_list) == 0) {
    warning("No variables were successfully analyzed.")
    return(NULL)
  }

  final_anova <- dplyr::bind_rows(lapply(results_list, `[[`, "anova"))
  final_anova$Term <- gsub("factor\\((.*?)\\)", "\\1", final_anova$Term)

  final_pairs <- dplyr::bind_rows(lapply(results_list, `[[`, "pairs"))
  final_cld   <- dplyr::bind_rows(lapply(results_list, `[[`, "cld"))

  return(list(
    "ANOVA_Results" = final_anova,
    "Pairwise_Comparisons" = final_pairs,
    "ABC_Letters" = final_cld
  ))
}
