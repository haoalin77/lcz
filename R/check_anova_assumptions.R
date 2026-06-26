#' Comprehensive ANOVA Assumptions Check
#'
#' @description
#' Performs Shapiro-Wilk normality test and Levene's homoscedasticity test on model residuals.
#' Can auto-detect numeric variables and save diagnostic plots to a PDF.
#'
#' @param data A data frame.
#' @param vars Character vector of response variables. If NULL, auto-detects numeric columns.
#' @param factor1 Character, first grouping factor.
#' @param factor2 Character, second grouping factor (optional).
#' @param save_plot Logical. If TRUE, saves all diagnostic plots to a single PDF file.
#' @param pdf_name Character, name of the output PDF file if save_plot is TRUE.
#' @return A data frame containing test results, decisions, and next-step recommendations.
#'
#' @importFrom stats as.formula aov resid shapiro.test
#' @importFrom graphics par plot
#' @importFrom grDevices pdf dev.off
#' @export
check_anova_assumptions <- function(data, vars = NULL, factor1, factor2 = NULL,
                                    save_plot = FALSE, pdf_name = "Assumptions_Diagnostic_Plots.pdf") {

  if (!requireNamespace("car", quietly = TRUE)) {
    stop("Please install the 'car' package: install.packages('car')")
  }

  # 1. Auto-detect numeric variables
  if (is.null(vars)) {
    numeric_cols <- names(data)[sapply(data, is.numeric)]
    vars <- setdiff(numeric_cols, c(factor1, factor2))
    if (length(vars) == 0) stop("Error: No numeric variables found.")
    cat(">>> Auto-detected", length(vars), "numeric variables.\n")
  }

  # 2. Setup PDF export if requested
  if (save_plot) {
    grDevices::pdf(file = pdf_name, width = 10, height = 5)
    cat(">>> Plots will be saved to:", pdf_name, "\n")
  }

  old_par <- graphics::par(mfrow = c(1, 2))
  on.exit({
    graphics::par(old_par)
    if (save_plot) grDevices::dev.off()
  })

  # 3. Main Loop with Error Handling (tryCatch)
  results_list <- lapply(vars, function(var) {

    tryCatch({
      if (is.null(factor2)) {
        f_str <- paste0(var, " ~ ", factor1)
      } else {
        f_str <- paste0(var, " ~ ", factor1, " * ", factor2)
      }

      test_formula <- stats::as.formula(f_str)
      fit <- stats::aov(test_formula, data = data)
      model_residuals <- stats::resid(fit)

      # Tests
      normal_test <- stats::shapiro.test(model_residuals)
      leve_test <- car::leveneTest(test_formula, data = data)

      p_norm <- normal_test$p.value
      p_leve <- leve_test$`Pr(>F)`[1]

      # Plots
      graphics::plot(fit, 1, main = paste(var, "- Residuals vs Fitted"))
      graphics::plot(fit, 2, main = paste(var, "- Q-Q Plot"))

      # Smart Recommendation Logic
      if (p_norm > 0.05 && p_leve > 0.05) {
        recommend <- "Parametric ANOVA"
      } else {
        recommend <- "Non-parametric / GLM / Transform"
      }

      data.frame(
        Variable  = var,
        Shapiro_P = round(p_norm, 4),
        Normality = ifelse(p_norm > 0.05, "Pass", "Fail"),
        Levene_P  = round(p_leve, 4),
        Homoscedasticity = ifelse(p_leve > 0.05, "Pass", "Fail"),
        Recommendation = recommend,
        row.names = NULL
      )

    }, error = function(e) {
      # Return a row with NA if a specific variable fails (e.g., constant values)
      warning(paste("Variable", var, "failed:", e$message))
      data.frame(
        Variable = var, Shapiro_P = NA, Normality = "Error",
        Levene_P = NA, Homoscedasticity = "Error", Recommendation = "Check Data",
        row.names = NULL
      )
    })
  })

  final_results <- do.call(rbind, results_list)
  return(final_results)
}
