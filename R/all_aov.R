#' 全面的方差分析及统计检验函数 (展示优化版)
#'
#' @param factor 字符向量，指定数据框中作为分组变量的列名
#' @param var_name 输入数据，可以是文件路径、数据框或对象名称
#' @export
all_aov <- function(factor, var_name) {

  # --- 1. 数据读取与预处理 (保持不变) ---
  file_path <- var_name
  if (is.character(file_path) && file.exists(file_path)) {
    file_ext <- tolower(tools::file_ext(file_path))
    if (file_ext == "csv") {
      data <- utils::read.csv(file_path)
    } else if (file_ext == "xlsx") {
      data <- openxlsx::read.xlsx(file_path)
    } else {
      stop("Unsupported format")
    }
  } else if (is.data.frame(file_path)) {
    data <- file_path
  } else if (exists(var_name) && is.data.frame(get(var_name))) {
    data <- get(var_name)
  } else {
    stop("can not deal")
  }

  if (!factor %in% colnames(data)) stop(paste("Column", factor, "not found in data"))
  colnames(data)[colnames(data) == factor] <- "pla"
  data$pla <- as.factor(data$pla)

  numeric_cols <- names(data)[sapply(data, is.numeric)]
  numeric_cols <- setdiff(numeric_cols, "pla")
  if(length(numeric_cols) == 0) stop("No numeric columns found")

  # --- 2. 基础检验 (保持不变) ---
  shapiro_results <- do.call(rbind, lapply(numeric_cols, function(col) {
    res <- stats::shapiro.test(data[[col]])
    data.frame(Column = col, Statistic = res$statistic, P_value = round(res$p.value, 6),
               sig = ifelse(res$p.value > 0.05, "Yes", "NS"), stringsAsFactors = FALSE)
  }))
  rownames(shapiro_results) <- NULL

  levene_results <- do.call(rbind, lapply(numeric_cols, function(col) {
    formula <- stats::as.formula(paste(col, "~ pla"))
    test <- car::leveneTest(formula, data = data)
    data.frame(Variable = col, F_value = test$`F value`[1], p_value = test$`Pr(>F)`[1],
               sig = ifelse(test$`Pr(>F)`[1] > 0.05, "Yes", "NS"))
  }))

  anova_summary <- do.call(rbind, lapply(numeric_cols, function(col) {
    formula <- stats::as.formula(paste(col, "~ pla"))
    res <- broom::tidy(stats::aov(formula, data = data))
    data.frame(Variable = col, F_value = res$statistic[1], p_value = round(res$p.value[1], 4),
               Anova_sig = ifelse(res$p.value[1] < 0.05, "Yes", "NS"))
  }))

  tukey_summary <- do.call(rbind, lapply(numeric_cols, function(col) {
    mod <- stats::aov(stats::as.formula(paste(col, "~ pla")), data = data)
    res <- as.data.frame(stats::TukeyHSD(mod)$pla)
    res$Variable <- col; res$Comparison <- rownames(res); rownames(res) <- NULL
    colnames(res)[colnames(res) == "p adj"] <- "p_adj"
    res$Tukey_sig <- ifelse(res$p_adj < 0.05, "Yes", "NS")
    res
  }))

  kruskal_summary <- do.call(rbind, lapply(numeric_cols, function(col) {
    test <- stats::kruskal.test(stats::as.formula(paste(col, "~ pla")), data = data)
    data.frame(Variable = col, H_statistic = test$statistic, df = test$parameter,
               p_value = round(test$p.value, 6), kruskal_sig = ifelse(test$p.value < 0.05, "Yes", "NS"))
  }))
  rownames(kruskal_summary) <- NULL

  dunn_summary <- do.call(rbind, lapply(numeric_cols, function(col) {
    utils::capture.output(test <- dunn.test::dunn.test(x = data[[col]], g = data$pla, method = "bh"))
    data.frame(Variable = col, Comparison = test$comparisons, Z = test$Z,
               P_adj = round(test$P.adjusted, 6), Dunn_sig = ifelse(test$P.adjusted < 0.05, "Yes", "NS"))
  }))

  # --- 3. 核心辅助函数 (保持不变) ---
  extract_groups_from_dunn <- function(dunn_result, medians, alpha = 0.05) {
    all_comparisons <- unique(unlist(strsplit(dunn_result$comparisons, " - ")))
    p_matrix <- matrix(1, nrow = length(all_comparisons), ncol = length(all_comparisons),
                       dimnames = list(all_comparisons, all_comparisons))
    for(i in 1:length(dunn_result$comparisons)) {
      parts <- strsplit(dunn_result$comparisons[i], " - ")[[1]]
      if(length(parts) == 2) {
        p_val <- dunn_result$P.adjusted[i]
        p_matrix[parts[1], parts[2]] <- p_val
        p_matrix[parts[2], parts[1]] <- p_val
      }
    }
    diag(p_matrix) <- 1
    ordered_groups <- names(sort(medians, decreasing = TRUE))
    valid_groups <- ordered_groups[ordered_groups %in% rownames(p_matrix)]
    p_matrix <- p_matrix[valid_groups, valid_groups]
    letter_groups <- multcompView::multcompLetters(p_matrix, threshold = alpha)
    return(letter_groups)
  }

  get_tukey_letters <- function(tukey, means) {
    p_values <- tukey$pla[, "p adj"]
    names(p_values) <- rownames(tukey$pla)
    groups <- names(means)
    p_mat <- matrix(1, nrow = length(groups), ncol = length(groups), dimnames = list(groups, groups))
    comparisons <- names(p_values)
    for(comp in comparisons) {
      parts <- unlist(strsplit(comp, "-"))
      if(length(parts)==2 && all(parts %in% groups)) {
        p_mat[parts[1], parts[2]] <- p_values[comp]
        p_mat[parts[2], parts[1]] <- p_values[comp]
      }
    }
    ord <- names(sort(means, decreasing = TRUE))
    p_mat <- p_mat[ord, ord]
    return(list(pla = multcompView::multcompLetters(p_mat)))
  }

  # --- 4. 逐变量分析 (保持不变) ---
  analyze_variable <- function(data, var_name) {
    result <- list(variable = var_name)
    stats_df <- data %>%
      dplyr::group_by(pla) %>%
      dplyr::summarise(
        mean_val = mean(.data[[var_name]], na.rm = TRUE),
        se_val = stats::sd(.data[[var_name]], na.rm = TRUE) / sqrt(dplyr::n()),
        median_val = stats::median(.data[[var_name]], na.rm = TRUE),
        q1 = stats::quantile(.data[[var_name]], 0.25, na.rm = TRUE),
        q3 = stats::quantile(.data[[var_name]], 0.75, na.rm = TRUE),
        .groups = 'drop'
      )
    means_vec <- stats::setNames(stats_df$mean_val, stats_df$pla)
    medians_vec <- stats::setNames(stats_df$median_val, stats_df$pla)

    # 4.1 参数检验
    tryCatch({
      mod <- stats::aov(stats::as.formula(paste(var_name, "~ pla")), data = data)
      tukey_res <- stats::TukeyHSD(mod)
      tukey_letters <- get_tukey_letters(tukey_res, means_vec)
      letters_df <- as.data.frame.list(tukey_letters$pla)
      letters_df$pla <- rownames(letters_df)
      colnames(letters_df)[1] <- "Letters"
      data_new_param <- dplyr::left_join(stats_df[, c("pla", "mean_val", "se_val", "median_val")],
                                         letters_df, by = "pla")
      data_new_param$variable <- var_name
      result$parametric <- dplyr::select(data_new_param, variable, dplyr::everything())
    }, error = function(e) { result$parametric <- NULL })

    # 4.2 非参数检验
    tryCatch({
      utils::capture.output(dunn_res <- dunn.test::dunn.test(x = data[[var_name]], g = data$pla, method = "bh"))
      letter_groups <- extract_groups_from_dunn(dunn_res, medians_vec)
      letters_nonparam <- data.frame(pla = names(letter_groups$Letters),
                                     Letters = letter_groups$Letters, stringsAsFactors = FALSE)
      data_new_nonparam <- dplyr::left_join(stats_df[, c("pla", "median_val", "q1", "q3")],
                                            letters_nonparam, by = "pla")
      data_new_nonparam$variable <- var_name
      result$nonparametric <- dplyr::select(data_new_nonparam, variable, dplyr::everything())
    }, error = function(e) { result$nonparametric <- NULL })

    return(result)
  }

  # --- 5. 执行 ---
  all_results_list <- lapply(numeric_cols, function(var) analyze_variable(data, var))
  parametric_all <- dplyr::bind_rows(lapply(all_results_list, function(x) x$parametric))
  nonparametric_all <- dplyr::bind_rows(lapply(all_results_list, function(x) x$nonparametric))

  # --- 6. [优化] 结果整合与清洗 ---

  integrated_list <- lapply(numeric_cols, function(var) {
    p_norm <- shapiro_results$P_value[shapiro_results$Column == var]

    # 判断正态性
    if(length(p_norm) > 0 && !is.na(p_norm) && p_norm > 0.05) {
      # === 符合正态分布 (参数检验) ===
      df <- parametric_all[parametric_all$variable == var, ]

      # 创建简化列
      df$Method <- "Parametric"
      # 格式化数值: "Mean ± SE"
      df$Stats_Value <- sprintf("%.2f \u00B1 %.2f", df$mean_val, df$se_val) # \u00B1 是加减号

      # 选列
      df_final <- df[, c("variable", "pla", "Method", "Stats_Value", "Letters")]

    } else {
      # === 不符合正态分布 (非参数检验) ===
      df <- nonparametric_all[nonparametric_all$variable == var, ]

      # 创建简化列
      df$Method <- "Non-Parametric"
      # 格式化数值: "Median (Q1, Q3)"
      df$Stats_Value <- sprintf("%.2f (%.2f, %.2f)", df$median_val, df$q1, df$q3)

      # 选列
      df_final <- df[, c("variable", "pla", "Method", "Stats_Value", "Letters")]
    }
    return(df_final)
  })

  integrated_df <- dplyr::bind_rows(integrated_list)

  # --- 7. 返回结果 ---
  list(
    shapiro = shapiro_results,
    levene = levene_results,
    anova = anova_summary,
    tukey = tukey_summary,
    kruskal = kruskal_summary,
    dunn = dunn_summary,
    para = parametric_all,
    non_para = nonparametric_all,
    integrated = integrated_df # 优化后的表格
  )
}
