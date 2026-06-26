# 全局变量声明，避免 R CMD check 警告
utils::globalVariables(c(
  # 列名和变量名
  "pla", "variable", "p_adj",
  "Statistic", "P_value", "F_value", "p_value",
  "H_statistic", "df", "Comparison", "Z", "P_adj",
  "mean_val", "se_val", "median_val", "q1", "q3", "Letters",
  "Tukey_sig", "Anova_sig", "kruskal_sig", "Dunn_sig",
   "sig", "sheet_name",
  # dplyr 相关
  ".", ".data"
))
