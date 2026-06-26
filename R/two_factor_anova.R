#' Two-Factor ANOVA Analysis with Merged Output
#'
#' This function performs a comprehensive two-factor ANOVA analysis.
#' Results for Factor 1 and Factor 2 are merged into unified sheets for easier reading.
#'
#' @description
#' The function returns a flat list of data frames optimized for export to Excel.
#' Interaction and Main effect results are combined into single sheets respectively.
#' All redundant console warning and notice streams from emmeans are suppressed.
#'
#' @param data A data frame containing the variables to be analyzed
#' @param numeric_cols Character vector of numeric column names to analyze.
#'     If NULL, all numeric columns in the data will be used
#' @param factor1 Character string specifying the first factor variable name. Default is "pla"
#' @param factor2 Character string specifying the second factor variable name. Default is "clone"
#' @param factor1_levels Character vector specifying the order of levels for factor1.
#'     If NULL, uses unique values from data. Default is NULL
#' @param factor2_levels Character vector specifying the order of levels for factor2.
#'     If NULL, uses unique values from data. Default is NULL
#' @param alpha Numeric value specifying the significance level. Default is 0.05
#'
#' @return A named list of data frames, suitable for writing directly to Excel.
#'
#' @importFrom car leveneTest
#' @importFrom emmeans emmeans contrast
#' @importFrom broom tidy
#' @importFrom dplyr bind_rows
#' @importFrom stats aov as.formula
#' @importFrom multcomp cld
#'
#' @export
two_factor_anova_analysis <- function(data,
                                      numeric_cols = NULL,
                                      factor1 = "pla",
                                      factor2 = "clone",
                                      factor1_levels = NULL,
                                      factor2_levels = NULL,
                                      alpha = 0.05) {

  # --- Parameter Validation ---
  if (!is.data.frame(data)) stop("data must be a data frame")
  if (nrow(data) == 0) stop("data is empty")

  # Convert factors locally via dynamic primitives
  data[[factor1]] <- as.factor(data[[factor1]])
  data[[factor2]] <- as.factor(data[[factor2]])

  # Set levels safely
  if (is.null(factor1_levels)) factor1_levels <- as.character(unique(data[[factor1]]))
  if (is.null(factor2_levels)) factor2_levels <- as.character(unique(data[[factor2]]))

  # Set numeric_cols
  if (is.null(numeric_cols)) {
    numeric_cols <- names(data)[vapply(data, is.numeric, logical(1))]
  }

  if (length(numeric_cols) == 0) {
    warning("No numeric columns found for analysis")
    return(NULL)
  }

  # --- Namespace Guard Check ---
  for (pkg in c("emmeans", "broom", "multcomp", "dplyr")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste(pkg, "package is required but not installed"))
    }
  }

  # --- 1. Main ANOVA Analysis ---
  anova_results <- lapply(numeric_cols, function(col) {
    formula_obj <- stats::as.formula(paste(col, "~", factor1, "*", factor2))
    anova_model <- stats::aov(formula_obj, data = data)
    result <- broom::tidy(anova_model)

    res_df <- data.frame(
      Variable     = col,
      term         = result$term,
      df           = result$df,
      sumsq        = result$sumsq,
      meansq       = result$meansq,
      statistic    = result$statistic,
      p_value      = round(result$p.value, 10),
      Significance = "NS",
      stringsAsFactors = FALSE
    )

    # Vectorized safe assignment avoiding NSE mutation
    res_df$Significance[result$p.value < alpha] <- "Significant"
    return(res_df)
  })

  anova_summary <- do.call(rbind, anova_results)

  # Extract tracking tags cleanly using standard indexing
  interaction_term_string <- paste0(factor1, ":", factor2)
  interaction_vars <- anova_summary$Variable[anova_summary$term == interaction_term_string & anova_summary$p_value < alpha]
  main_vars        <- anova_summary$Variable[anova_summary$term == interaction_term_string & anova_summary$p_value >= alpha]

  final_output <- list()
  final_output$anova_summary <- anova_summary

  # --- 2. Interaction Effects Analysis (Merged) ---
  if (length(interaction_vars) > 0) {

    # --- 2.1 Pairwise Comparisons (Merged) ---
    f1_res <- lapply(interaction_vars, function(col) {
      mod <- stats::aov(stats::as.formula(paste(col, "~", factor1, "*", factor2)), data = data)

      # Wrap in suppressMessages to kill both "misleading" and "sidak" notes
      emm <- suppressMessages(emmeans::emmeans(mod, stats::as.formula(paste("~", factor1, "|", factor2))))
      comp <- suppressMessages(emmeans::contrast(emm, method = "pairwise", adjust = "tukey"))

      df <- as.data.frame(comp)
      df$Variable <- col
      df$Test_Factor <- factor1
      return(df)
    })

    f2_res <- lapply(interaction_vars, function(col) {
      mod <- stats::aov(stats::as.formula(paste(col, "~", factor1, "*", factor2)), data = data)
      emm <- suppressMessages(emmeans::emmeans(mod, stats::as.formula(paste("~", factor2, "|", factor1))))
      comp <- suppressMessages(emmeans::contrast(emm, method = "pairwise", adjust = "tukey"))

      df <- as.data.frame(comp)
      df$Variable <- col
      df$Test_Factor <- factor2
      return(df)
    })

    # Safely merge using data frame primitives
    raw_combined_pairwise <- dplyr::bind_rows(do.call(rbind, f1_res), do.call(rbind, f2_res))

    # Enforce standard column ordering via dynamic vector sub-setting
    pairwise_front <- c("Variable", "Test_Factor")
    pairwise_others <- setdiff(names(raw_combined_pairwise), pairwise_front)
    combined_pairwise <- raw_combined_pairwise[, c(pairwise_front, pairwise_others)]

    final_output$interaction_pairwise <- combined_pairwise

    # --- 2.2 Significance Letters (Merged) ---
    f1_let <- lapply(interaction_vars, function(col) {
      mod <- stats::aov(stats::as.formula(paste(col, "~", factor1, "*", factor2)), data = data)
      emm <- suppressMessages(emmeans::emmeans(mod, stats::as.formula(paste("~", factor1, "|", factor2))))
      cld_res <- suppressMessages(multcomp::cld(emm, Letters = letters, reversed = TRUE))

      df <- as.data.frame(cld_res)
      df$Variable <- col
      df$Test_Factor <- factor1

      df[[factor1]] <- factor(df[[factor1]], levels = factor1_levels)
      df[[factor2]] <- factor(df[[factor2]], levels = factor2_levels)
      df <- df[order(df[[factor2]], df[[factor1]]), ]

      names(df)[names(df) == ".group"] <- "letters"

      letter_cols_f1 <- c("Variable", "Test_Factor", factor1, factor2, "emmean", "letters")
      return(df[, letter_cols_f1])
    })

    f2_let <- lapply(interaction_vars, function(col) {
      mod <- stats::aov(stats::as.formula(paste(col, "~", factor1, "*", factor2)), data = data)
      emm <- suppressMessages(emmeans::emmeans(mod, stats::as.formula(paste("~", factor2, "|", factor1))))
      cld_res <- suppressMessages(multcomp::cld(emm, Letters = letters, reversed = TRUE))

      df <- as.data.frame(cld_res)
      df$Variable <- col
      df$Test_Factor <- factor2

      df[[factor1]] <- factor(df[[factor1]], levels = factor1_levels)
      df[[factor2]] <- factor(df[[factor2]], levels = factor2_levels)
      df <- df[order(df[[factor1]], df[[factor2]]), ]

      names(df)[names(df) == ".group"] <- "letters"

      letter_cols_f2 <- c("Variable", "Test_Factor", factor1, factor2, "emmean", "letters")
      return(df[, letter_cols_f2])
    })

    final_output$interaction_letters <- dplyr::bind_rows(do.call(rbind, f1_let), do.call(rbind, f2_let))
  }

  # --- 3. Main Effects Analysis (Merged) ---
  if (length(main_vars) > 0) {

    all_pairs_list <- list()
    all_cld_list <- list()

    for (col in main_vars) {
      mod <- stats::aov(stats::as.formula(paste(col, "~", factor1, "*", factor2)), data = data)

      # Factor 1 Analysis
      emm1 <- suppressMessages(emmeans::emmeans(mod, stats::as.formula(paste("~", factor1))))
      p1 <- suppressMessages(as.data.frame(emmeans::contrast(emm1, method = "pairwise", adjust = "tukey")))
      c1 <- suppressMessages(as.data.frame(multcomp::cld(emm1, Letters = letters, adjust = "tukey")))

      p1$Variable <- col; p1$Test_Factor <- factor1
      c1$Variable <- col; c1$Test_Factor <- factor1

      # Factor 2 Analysis
      emm2 <- suppressMessages(emmeans::emmeans(mod, stats::as.formula(paste("~", factor2))))
      p2 <- suppressMessages(as.data.frame(emmeans::contrast(emm2, method = "pairwise", adjust = "tukey")))
      c2 <- suppressMessages(as.data.frame(multcomp::cld(emm2, Letters = letters, adjust = "tukey")))

      p2$Variable <- col; p2$Test_Factor <- factor2
      c2$Variable <- col; c2$Test_Factor <- factor2

      all_pairs_list[[paste0(col, "_f1")]] <- p1
      all_pairs_list[[paste0(col, "_f2")]] <- p2
      all_cld_list[[paste0(col, "_f1")]]  <- c1
      all_cld_list[[paste0(col, "_f2")]]  <- c2
    }

    # Bind elements securely
    main_pairwise_df <- dplyr::bind_rows(all_pairs_list)
    rownames(main_pairwise_df) <- NULL

    # Re-order columns standard indexing
    pairwise_main_front <- c("Variable", "Test_Factor")
    pairwise_main_others <- setdiff(names(main_pairwise_df), pairwise_main_front)
    final_output$main_pairwise <- main_pairwise_df[, c(pairwise_main_front, pairwise_main_others)]

    # Merge and unify letter headers without using mutable dplyr pipelines
    main_letters_df <- dplyr::bind_rows(all_cld_list)
    rownames(main_letters_df) <- NULL

    if (".group" %in% names(main_letters_df)) {
      names(main_letters_df)[names(main_letters_df) == ".group"] <- "letters"
    }

    letters_main_front <- c("Variable", "Test_Factor")
    # FIXED HERE: Changed `:=` to standard standard R `<-`
    letters_main_others <- setdiff(names(main_letters_df), letters_main_front)
    final_output$main_letters <- main_letters_df[, c(letters_main_front, letters_main_others)]
  }

  return(final_output)
}
