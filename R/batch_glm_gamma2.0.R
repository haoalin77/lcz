#' Batch GLM Analysis with Verified CLD Ordering
#'
#' @description
#' Performs GLM analysis (Gamma family), Type III ANOVA, and generates
#' Compact Letter Displays (CLD) for significance grouping.
#'
#' Key features:
#' \itemize{
#'   \item Fits Gamma GLM (log link) for multiple variables.
#'   \item Generates post-hoc grouping letters (e.g., "a", "b").
#'   \item Ensures output is strictly sorted by original factor levels.
#' }
#'
#' @param data A data frame containing the variables to be analyzed.
#' @param response_cols A character vector (optional). Names of the numeric response columns.
#'        If \code{NULL} (default), all numeric columns excluding the factor columns are used.
#' @param factor1 A string. The name of the first factor (e.g., "clone").
#' @param factor2 A string. The name of the second factor (e.g., "temp").
#' @param decreasing_letters Logical. If \code{TRUE} (default), the letter 'a' is
#'        assigned to the group with the largest mean.
#'
#' @return A list containing four data frames: ANOVA results, Letter groupings (F1|F2),
#'        Letter groupings (F2|F1), and detailed pairwise contrasts.
#'
#' @importFrom car Anova
#' @importFrom emmeans emmeans contrast
#' @importFrom multcomp cld
#' @importFrom tibble rownames_to_column
#' @importFrom dplyr rename mutate arrange select bind_rows
#' @importFrom stats as.formula glm Gamma
#'
#' @export
batch_glm_gamma_cld <- function(data,
                                response_cols = NULL,
                                factor1 = "clone",
                                factor2 = "temp",
                                decreasing_letters = TRUE) {

  # --- 1. Enforce Factor Level Order ---
  if (!is.factor(data[[factor1]])) {
    data[[factor1]] <- factor(data[[factor1]], levels = unique(data[[factor1]]))
  }
  if (!is.factor(data[[factor2]])) {
    data[[factor2]] <- factor(data[[factor2]], levels = unique(data[[factor2]]))
  }

  if (is.null(response_cols)) {
    all_numeric <- names(data)[sapply(data, is.numeric)]
    response_cols <- setdiff(all_numeric, c(factor1, factor2))
  }

  # --- 2. Core Analysis Loop ---
  results_list <- lapply(response_cols, function(variable) {

    f_str <- paste0(variable, " ~ ", factor1, " * ", factor2)
    test_formula <- stats::as.formula(f_str)

    tryCatch({
      # A. Fit Model
      fit_glm <- stats::glm(test_formula, data = data, family = stats::Gamma(link = "log"))

      # B. ANOVA (Type III)
      raw_anova <- car::Anova(fit_glm, type = "III")
      df_anova <- as.data.frame(raw_anova)
      df_anova <- tibble::rownames_to_column(df_anova, var = "Effect")

      # Clean names and columns to avoid dynamic non-standard evaluation (NSE) notes
      names(df_anova)[names(df_anova) == "Pr(>Chisq)"] <- "P_Value"
      df_anova[["indice"]] <- variable

      # C. Post-hoc & CLD Internal Helper Function
      get_cld_table <- function(model, formula_spec, group_col, sort_col) {

        # Use suppressMessages to completely silence the "tukey changed to sidak" note
        em_obj <- suppressMessages(
          emmeans::emmeans(model, formula_spec, type = "response")
        )

        # Generate Compact Letter Display quietly
        cld_out <- suppressMessages(
          multcomp::cld(em_obj,
                        Letters = letters,
                        adjust = "tukey",
                        sort = TRUE,
                        decreasing = decreasing_letters)
        )

        cld_df <- as.data.frame(cld_out)
        cld_df[["indice"]] <- variable
        cld_df[[".group"]] <- trimws(cld_df[[".group"]])

        names(cld_df)[names(cld_df) == ".group"] <- "Significance_Letter"

        # Safe arrange using standard evaluation via dynamic data indexing
        cld_df <- cld_df[order(cld_df[[sort_col]], cld_df[[group_col]]), ]

        # Safe column ordering using dynamic character subsetting
        front_cols <- c("indice", group_col, sort_col, "Significance_Letter")
        other_cols <- setdiff(names(cld_df), front_cols)
        cld_df <- cld_df[, c(front_cols, other_cols)]

        return(cld_df)
      }

      # C1. Clone within Temp (Slice by Temp)
      spec1 <- stats::as.formula(paste("~", factor1, "|", factor2))
      cld_1 <- get_cld_table(fit_glm, spec1, group_col = factor1, sort_col = factor2)

      # C2. Temp within Clone (Slice by Clone)
      spec2 <- stats::as.formula(paste("~", factor2, "|", factor1))
      cld_2 <- get_cld_table(fit_glm, spec2, group_col = factor2, sort_col = factor1)

      # D. Contrasts (Silenced as well)
      em_spec1 <- suppressMessages(emmeans::emmeans(fit_glm, spec1))
      em_spec2 <- suppressMessages(emmeans::emmeans(fit_glm, spec2))

      contr_1 <- suppressMessages(as.data.frame(emmeans::contrast(em_spec1, method = "pairwise", adjust = "tukey")))
      contr_2 <- suppressMessages(as.data.frame(emmeans::contrast(em_spec2, method = "pairwise", adjust = "tukey")))

      df_contrasts <- rbind(contr_1, contr_2)
      df_contrasts[["indice"]] <- variable

      # Move index to front safely
      df_contrasts <- df_contrasts[, c("indice", setdiff(names(df_contrasts), "indice"))]

      list(
        anova = df_anova,
        letters_1 = cld_1,
        letters_2 = cld_2,
        contrasts = df_contrasts
      )

    }, error = function(e) {
      message(paste("Error in evaluation of:", variable, "-", e$message))
      return(NULL)
    })
  })

  # --- 3. Bind and Combine Results Safely ---
  results_list <- results_list[!sapply(results_list, is.null)]
  if (length(results_list) == 0) {
    stop("All model fits and post-hoc analyses failed.")
  }

  final_list <- list(
    "ANOVA_All"           = dplyr::bind_rows(lapply(results_list, `[[`, "anova")),
    "Letters_F1_in_F2"    = dplyr::bind_rows(lapply(results_list, `[[`, "letters_1")),
    "Letters_F2_in_F1"    = dplyr::bind_rows(lapply(results_list, `[[`, "letters_2")),
    "PostHoc_Comparisons" = dplyr::bind_rows(lapply(results_list, `[[`, "contrasts"))
  )

  return(final_list)
}
