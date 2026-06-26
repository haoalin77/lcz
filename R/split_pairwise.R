#' Generate Pairwise Subsets from a Data Frame
#'
#' @description
#' Creates a list of data frames, where each data frame contains data for
#' only two specific levels of the grouping variable. It generates all possible
#' pairwise combinations.
#'
#' @param data A data frame.
#' @param group_col Character string. The name of the grouping column.
#'
#' @return A named list of data frames. Names are formatted as "Group1_vs_Group2".
#' @importFrom utils combn
#' @export
split_pairwise <- function(data, group_col) {

  # 1. 检查列是否存在
  if (!group_col %in% names(data)) {
    stop(paste("Column", group_col, "not found."))
  }

  # 2. 获取唯一分组级别
  # 转换为字符以确保安全，移除 NA
  levels_vec <- unique(as.character(data[[group_col]]))
  levels_vec <- levels_vec[!is.na(levels_vec)]

  if (length(levels_vec) < 2) {
    stop("Need at least 2 groups to make pairs.")
  }

  # 3. 生成组合 (List形式)
  comb_list <- combn(levels_vec, 2, simplify = FALSE)

  # 4. 构建结果 List
  result_list <- list()

  for (pair in comb_list) {
    # 构建名称: "A_vs_B"
    pair_name <- paste(pair[1], pair[2], sep = "_vs_")

    # 筛选数据
    # 使用 base R 的 subset 逻辑，不依赖 dplyr，方便移植
    sub_df <- data[data[[group_col]] %in% pair, ]

    # 存入 List
    result_list[[pair_name]] <- sub_df
  }

  return(result_list)
}

# --- 使用示例 ---
# list_of_dfs <- split_pairwise(data_all, "treat")
#
# # 访问 D 和 Z 的组合数据
# df_d_z <- list_of_dfs[["D_vs_Z"]]
