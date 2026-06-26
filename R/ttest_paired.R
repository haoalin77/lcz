#' 批量配对t检验分析
#'
#' 该函数将数据框按指定列数分割成多个子数据集，对每个子数据集中的所有变量
#' 进行两两配对t检验，并汇总所有检验结果。
#'
#' @param data_all 数据框，包含所有需要分析的数据
#' @param col_number 数值，指定每个子数据集的列数
#'
#' @return 返回一个数据框，包含以下列：
#' \describe{
#'   \item{Pair}{字符，配对变量名称，格式为"变量1 - 变量2"}
#'   \item{Mean_Diff}{数值，配对差值的均值}
#'   \item{Std_Error}{数值，配对差值的标准误}
#'   \item{DF}{数值，自由度}
#'   \item{mean1}{数值，第一个变量的均值}
#'   \item{mean2}{数值，第二个变量的均值}
#'   \item{t_Value}{数值，t检验统计量}
#'   \item{p_Value}{数值，检验的p值}
#'   \item{Sig}{字符，显著性标记，p < 0.05为"*"，否则为"ns"}
#' }
#'
#' @details
#' 函数执行流程：
#' \enumerate{
#'   \item 计算需要分割的子数据集数量
#'   \item 按指定列数将原始数据分割成多个子数据集
#'   \item 对每个子数据集中的所有变量进行两两组合
#'   \item 对每对变量执行配对t检验
#'   \item 提取检验结果并汇总
#' }
#'
#' @note
#' 重要注意事项：
#' \itemize{
#'   \item 假设数据框中的所有列都是数值型变量
#'   \item 使用配对t检验，要求两个变量的观测值是对应的
#'   \item 显著性水平设为0.05
#'   \item 如果列数不能被col_number整除，最后一个子数据集可能包含较少列
#' }
#'
#' @examples
#' \dontrun{
#' # 创建示例数据
#' set.seed(123)
#' data_all <- data.frame(
#'   var1 = rnorm(30),
#'   var2 = rnorm(30),
#'   var3 = rnorm(30),
#'   var4 = rnorm(30),
#'   var5 = rnorm(30),
#'   var6 = rnorm(30)
#' )
#'
#' # 将数据分成每2列一组进行配对t检验
#' results <- ttest_paired(data_all, 2)
#' print(results)
#'
#' # 将数据分成每3列一组
#' results <- ttest_paired(data_all, 3)
#' print(results)
#' }
#'
#' @importFrom dplyr bind_rows
#' @importFrom utils combn
#' @export
ttest_paired <- function(data_all, col_number) {
  # 初始化结果汇总数据框
  result_summary <- data.frame()

  # 计算子数据集数量
  data_number <- ceiling(ncol(data_all) / col_number)

  for (i in 1:data_number) {
    start_col <- (i - 1) * col_number + 1
    end_col <- min(i * col_number, ncol(data_all))

    # 提取当前子数据集
    data <- data_all[, start_col:end_col]

    # 生成所有变量配对
    pairs <- utils::combn(names(data), 2, simplify = FALSE)

    # 对每对变量执行配对t检验
    results <- lapply(pairs, function(pair) {
      x <- data[[pair[1]]]
      y <- data[[pair[2]]]

      # 执行配对t检验
      test_result <- stats::t.test(x, y, paired = TRUE)

      # 计算均值
      m1 <- mean(x)
      m2 <- mean(y)

      # 整理结果
      data.frame(
        Pair = paste(pair[1], "-", pair[2]),
        Mean_Diff = test_result$estimate,
        Std_Error = test_result$stderr,
        DF = test_result$parameter,
        mean1 = m1,
        mean2 = m2,
        t_Value = test_result$statistic,
        p_Value = test_result$p.value,
        Sig = ifelse(test_result$p.value < 0.05, "*", "ns")
      )
    }) %>% dplyr::bind_rows()

    # 合并结果
    result_summary <- rbind(result_summary, results)
  }

  return(result_summary)
}
