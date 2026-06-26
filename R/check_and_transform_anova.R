#' Comprehensive ANOVA Assumptions Check and Automatic Transformation
#'
#' @description
#' A robust pipeline that checks ANOVA assumptions. If violations are detected,
#' it attempts multiple transformations and re-tests until an optimal fit is found.
#' Real-time logs are printed to the console to track the progress.
#'
#' @param data A data frame containing the variables.
#' @param vars Character vector of response variables. If NULL, all numeric columns are used.
#' @param factor1 Character. The name of the first grouping factor.
#' @param factor2 Character. The name of the second grouping factor (optional).
#' @param save_plot Logical. If TRUE, saves diagnostic plots to a PDF.
#' @param pdf_name Character. Name of the PDF file if save_plot is TRUE.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{data}: The updated data frame with new transformation columns.
#'   \item \code{summary}: A summary table of all tests and transformations.
#' }
#'
#' @importFrom stats aov resid shapiro.test as.formula setNames
#' @importFrom graphics par plot
#' @importFrom grDevices pdf dev.off
#' @export
check_and_transform_anova <- function(data, vars = NULL, factor1, factor2 = NULL,
                                      save_plot = FALSE, pdf_name = "ANOVA_Diagnostics.pdf") {

  if (!requireNamespace("car", quietly = TRUE)) {
    stop("Package 'car' is required. Please install it: install.packages('car')")
  }

  if (is.null(vars)) {
    numeric_cols <- names(data)[sapply(data, is.numeric)]
    vars <- setdiff(numeric_cols, c(factor1, factor2))
    if (length(vars) == 0) stop("No numeric response variables detected.")
    cat(">>> Auto-detected", length(vars), "numeric variables for analysis.\n")
  }

  if (save_plot) {
    grDevices::pdf(file = pdf_name, width = 10, height = 5)
    cat(">>> Diagnostic plots will be saved to:", pdf_name, "\n")
  }

  old_par <- graphics::par(mfrow = c(1, 2))
  on.exit({
    graphics::par(old_par)
    if (save_plot) grDevices::dev.off()
  })

  # Transformation List
  trans_list <- list(
    ln = function(x) log(x),
    log10 = function(x) log10(x),
    sqrt = function(x) sqrt(x),
    log1p = function(x) log(x + 1),
    boxcox = function(x) {
      if (any(x <= 0, na.rm = TRUE)) x <- x - min(x, na.rm = TRUE) + 0.001
      pt <- car::powerTransform(x)
      car::bcPower(x, pt$lambda)
    }
  )

  results_list <- list()
  updated_data <- data

  cat("\n", rep("=", 50), "\n", "STARTING ANOVA ASSUMPTION CHECK PIPELINE", "\n", rep("=", 50), "\n", sep = "")

  for (var in vars) {
    cat("\n[Variable:", var, "]\n")

    f_str <- if (is.null(factor2)) paste0(var, " ~ ", factor1) else paste0(var, " ~ ", factor1, " * ", factor2)

    # Baseline check
    fit <- stats::aov(stats::as.formula(f_str), data = updated_data)
    p_norm_orig <- stats::shapiro.test(stats::resid(fit))$p.value
    p_leve_orig <- car::leveneTest(stats::as.formula(f_str), data = updated_data)$`Pr(>F)`[1]

    status <- "Original"
    is_passed <- (p_norm_orig > 0.05 && p_leve_orig > 0.05)
    current_var_name <- var
    final_p_norm <- p_norm_orig
    final_p_leve <- p_leve_orig

    if (is_passed) {
      cat("  - Baseline assumptions PASSED (Normality P =", round(p_norm_orig, 4),
          ", Homogeneity P =", round(p_leve_orig, 4), ")\n")
    } else {
      cat("  - Baseline FAILED. Attempting transformations...\n")

      for (t_name in names(trans_list)) {
        tryCatch({
          temp_val <- trans_list[[t_name]](updated_data[[var]])
          if (any(is.nan(temp_val) | is.infinite(temp_val))) stop("Math Error")

          temp_col_name <- paste0(var, "_", t_name)
          test_df <- updated_data
          test_df[[temp_col_name]] <- temp_val

          t_f_str <- if (is.null(factor2)) paste0(temp_col_name, " ~ ", factor1) else paste0(temp_col_name, " ~ ", factor1, " * ", factor2)
          t_fit <- stats::aov(stats::as.formula(t_f_str), data = test_df)

          tp_norm <- stats::shapiro.test(stats::resid(t_fit))$p.value
          tp_leve <- car::leveneTest(stats::as.formula(t_f_str), data = test_df)$`Pr(>F)`[1]

          if (tp_norm > 0.05 && tp_leve > 0.05) {
            final_p_norm <- tp_norm
            final_p_leve <- tp_leve
            status <- t_name
            is_passed <- TRUE
            fit <- t_fit
            current_var_name <- temp_col_name

            cat("  ✔ SUCCESS:", t_name, "transformation passed (Normality P =",
                round(tp_norm, 4), ", Homogeneity P =", round(tp_leve, 4), ")\n")

            # Insert logic
            idx <- which(names(updated_data) == var)
            new_col_df <- stats::setNames(data.frame(temp_val), temp_col_name)

            if (idx == ncol(updated_data)) {
              updated_data <- cbind(updated_data, new_col_df)
            } else {
              updated_data <- cbind(
                updated_data[, 1:idx, drop = FALSE],
                new_col_df,
                updated_data[, (idx + 1):ncol(updated_data), drop = FALSE]
              )
            }
            break
          }
        }, error = function(e) NULL)
        if (is_passed) break
      }
      if (!is_passed) cat("  ✖ ALL transformations failed for", var, "\n")
    }

    # Plotting
    graphics::plot(fit, 1, main = paste(current_var_name, "Resid vs Fit"))
    graphics::plot(fit, 2, main = paste(current_var_name, "Q-Q Plot"))

    results_list[[var]] <- data.frame(
      Variable = var,
      Transformation = status,
      Final_Column = current_var_name,
      Shapiro_P = round(final_p_norm, 4),
      Levene_P = round(final_p_leve, 4),
      Status = ifelse(is_passed, "Passed", "Failed"),
      stringsAsFactors = FALSE
    )
  }

  cat("\n", rep("=", 50), "\n", "PIPELINE COMPLETED SUCCESSFULLY", "\n", rep("=", 50), "\n", sep = "")

  return(list(
    data = updated_data,
    summary = do.call(rbind, results_list)
  ))
}
