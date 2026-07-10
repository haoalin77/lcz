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
#' It performs Type III ANOVA and automatically selects post-hoc analysis strategies.
#'
#' @param data A data frame containing the response variables and factors.
#' @param fixed_factors A character vector of length 2. Specifies the column names of the two fixed effects.
#' @param random_factor A character string. Specifies the column name of the random effect.
#' @param response_cols (Optional) A character vector. Specifies the column names of response variables to analyze.
#' @param adjust_method A character string. The adjustment method for p-values in emmeans (default is "sidak").
#'
#' @return A list containing three data frames: ANOVA_Results, Pairwise_Comparisons, and ABC_Letters.
#'
#' @importFrom lmerTest lmer
#' @importFrom emmeans emmeans
#' @importFrom multcomp cld
#' @importFrom multcompView multcompLetters
#' @importFrom dplyr bind_rows
#' @importFrom stats anova as.formula
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
  # Ensure columns exist and convert to factors
  for (col in c(fixed_factors, random_factor)) {
    if (!col %in% colnames(data)) {
      stop(paste("Column not found in data:", col))
    }
    data[[col]] <- as.factor(data[[col]]) #
  }

  # Determine response variables
  if (is.null(response_cols)) {
    all_numeric <- colnames(data)[vapply(data, is.numeric, logical(1))]
    response_cols <- setdiff(all_numeric, c(fixed_factors, random_factor))
  }

  f1  <- fixed_factors[1]
  f2  <- fixed_factors[2]
  rnd <- random_factor

  # --- Internal Helper: Extract Pairs and CLD ---
  get_emm_res <- function(emm_obj, adj_meth, comparison_lbl) {
    # A. Pairs
    pairs_obj <- suppressMessages(graphics::pairs(emm_obj, adjust = adj_meth))
    pairs_df  <- as.data.frame(pairs_obj)
    pairs_df[["Comparison_Type"]] <- comparison_lbl

    # B. CLD (Compact Letter Display)
    cld_obj <- suppressMessages(
      multcomp::cld(emm_obj, alpha = 0.05, Letters = letters, adjust = adj_meth)
    )
    cld_df <- as.data.frame(cld_obj)
    cld_df[["Comparison_Type"]] <- comparison_lbl

    # Clean .group column safely via Base R matrix indexing
    if (".group" %in% colnames(cld_df)) {
      cld_df[["Letters"]] <- trimws(cld_df[[".group"]])
      cld_df <- cld_df[, setdiff(names(cld_df), ".group"), drop = FALSE]
    }

    return(list(pairs = pairs_df, cld = cld_df))
  }

  # --- Loop Analysis ---
  results_list <- lapply(response_cols, function(var_name) {
    tryCatch({
      # === A. Modeling ===
      formula_str <- paste0("`", var_name, "` ~ `", f1, "` * `", f2, "` + (1 | `", rnd, "`)")

      # Suppress any internal convergence warnings or text messages from lme4
      fit1 <- suppressMessages(lmerTest::lmer(stats::as.formula(formula_str), data = data))

      # === B. ANOVA ===
      fit_aov <- stats::anova(fit1, type = 3)

      # Check Interaction safely
      interaction_row <- grep(":", rownames(fit_aov))
      if (length(interaction_row) > 0) {
        p_interaction <- fit_aov$`Pr(>F)`[interaction_row]
        is_interaction_sig <- (!is.na(p_interaction) && p_interaction < 0.05)
      } else {
        is_interaction_sig <- FALSE
      }

      status_label <- ifelse(is_interaction_sig, "Significant Interaction", "Non-Sig Interaction")

      # Tidy ANOVA table
      fit_aov_df <- as.data.frame(fit_aov)
      fit_aov_df[["Variable"]] <- var_name
      fit_aov_df[["Term"]]     <- rownames(fit_aov_df)
      fit_aov_df[["Interaction_Status"]] <- status_label

      # Vectorized sign assignments
      sig_vector <- rep("NS", nrow(fit_aov_df))
      p_vals <- fit_aov_df$`Pr(>F)`
      sig_vector[!is.na(p_vals) & p_vals < 0.05]  <- "*"
      sig_vector[!is.na(p_vals) & p_vals < 0.01]  <- "**"
      sig_vector[!is.na(p_vals) & p_vals < 0.001] <- "***"
      fit_aov_df[["Sig"]] <- sig_vector

      # Enforce explicit column order via dynamic string subsetting
      anova_front <- c("Variable", "Term", "Interaction_Status", "Pr(>F)", "Sig")
      anova_others <- setdiff(names(fit_aov_df), anova_front)
      fit_aov_df <- fit_aov_df[, c(anova_front, anova_others), drop = FALSE]

      # === C. Post-hoc Analysis ===
      pairs_list <- list()
      cld_list   <- list()

      if (is_interaction_sig) {
        # >>> Path 1: Significant Interaction -> Simple Effects <<<
        spec_formula_1 <- stats::as.formula(paste0("~ `", f1, "` | `", f2, "`"))
        emm_1 <- suppressMessages(emmeans::emmeans(fit1, specs = spec_formula_1))
        res_1 <- get_emm_res(emm_1, adjust_method, paste0("Simple Effect: ", f1, " within ", f2))

        pairs_list[[1]] <- res_1$pairs
        cld_list[[1]]   <- res_1$cld

        spec_formula_2 <- stats::as.formula(paste0("~ `", f2, "` | `", f1, "`"))
        emm_2 <- suppressMessages(emmeans::emmeans(fit1, specs = spec_formula_2))

        # FIXED: Added the missing comma here
        res_2 <- get_emm_res(emm_2, adjust_method, paste0("Simple Effect: ", f2, " within ", f1))

        pairs_list[[2]] <- res_2$pairs
        cld_list[[2]]   <- res_2$cld

      } else {
        # >>> Path 2: Non-sig Interaction -> Main Effects <<<
        spec_formula_main_1 <- stats::as.formula(paste0("~ `", f1, "`"))
        emm_main_1 <- suppressMessages(emmeans::emmeans(fit1, specs = spec_formula_main_1))
        res_main_1 <- get_emm_res(emm_main_1, adjust_method, paste0("Main Effect: ", f1))

        if (nrow(res_main_1$cld) > 0) res_main_1$cld[[f2]] <- "All"

        pairs_list[[1]] <- res_main_1$pairs
        cld_list[[1]]   <- res_main_1$cld

        spec_formula_main_2 <- stats::as.formula(paste0("~ `", f2, "`"))
        emm_main_2 <- suppressMessages(emmeans::emmeans(fit1, specs = spec_formula_main_2))
        res_main_2 <- get_emm_res(emm_main_2, adjust_method, paste0("Main Effect: ", f2))

        if (nrow(res_main_2$cld) > 0) res_main_2$cld[[f1]] <- "All"

        pairs_list[[2]] <- res_main_2$pairs
        cld_list[[2]]   <- res_main_2$cld
      }

      # === D. Merge Single Variable Results ===
      all_pairs_var <- dplyr::bind_rows(pairs_list)
      all_pairs_var[["Variable"]]           <- var_name
      all_pairs_var[["Interaction_Status"]] <- status_label
      all_pairs_var[["Sig"]]                <- ifelse(!is.na(all_pairs_var$p.value) & all_pairs_var$p.value < 0.05, "*", "NS")

      pairs_front  <- c("Variable", "Interaction_Status", "Comparison_Type")
      pairs_others <- setdiff(names(all_pairs_var), pairs_front)
      all_pairs_var <- all_pairs_var[, c(pairs_front, pairs_others), drop = FALSE]

      all_cld_var <- dplyr::bind_rows(cld_list)
      all_cld_var[["Variable"]]           <- var_name
      all_cld_var[["Interaction_Status"]] <- status_label

      cld_front  <- c("Variable", "Interaction_Status", "Comparison_Type")
      cld_others <- setdiff(names(all_cld_var), cld_front)
      all_cld_var <- all_cld_var[, c(cld_front, cld_others), drop = FALSE]

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

  rownames(final_anova) <- NULL
  rownames(final_pairs) <- NULL
  rownames(final_cld)   <- NULL

  return(list(
    "ANOVA_Results"        = final_anova,
    "Pairwise_Comparisons" = final_pairs,
    "ABC_Letters"          = final_cld
  ))
}
