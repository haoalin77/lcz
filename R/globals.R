# R/globals.R

utils::globalVariables(c(
  # 之前可能已经添加的
  "Pr(>Chisq)",
  "P_Value",
  ".group",
  "Significance_Letter",

  # 本次报错缺少的
  "indice",
  "Value",
  "Variable" # 之前 check_normality 里可能用到的
))
