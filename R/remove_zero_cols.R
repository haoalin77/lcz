#' 移除包含 0 的列
#'
#' 根据指定条件移除数据框中包含 0 的列。
#'
#' @param data 输入的数据框 (data.frame)。
#' @param type 移除模式。
#'   \itemize{
#'     \item \code{"all"}: (默认) 仅当整列的值**全为 0** 时才移除该列。
#'     \item \code{"any"}: 只要列中包含**任意一个 0**，就移除该列。
#'   }
#' @param na.rm 逻辑值。在判断 0 时是否忽略 NA 值？默认为 TRUE。
#'
#' @return 清洗后的数据框。
#' @export
#' @examples
#' df <- data.frame(
#'   A = c(1, 2, 3),      # 正常
#'   B = c(0, 0, 0),      # 全是 0
#'   C = c(1, 0, 3),      # 包含部分 0
#'   D = c(1, 2, NA)      # 包含 NA
#' )
#'
#' # 模式 1：只删 B 列 (全 0)
#' remove_zero_cols(df, type = "all")
#'
#' # 模式 2：删掉 B 和 C 列 (包含 0)
#' remove_zero_cols(df, type = "any")
remove_zero_cols <- function(data, type = c("all", "any"), na.rm = TRUE) {

  # 1. 参数检查
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data frame.")
  }

  # 匹配 type 参数，默认为 "all"
  type <- match.arg(type)

  # 2. 定义判断逻辑函数
  # x == 0 在 R 中如果是字符型比较会比较安全（"a" == 0 为 FALSE），
  # 但为了严谨，我们先判断是否为数值型，或者直接利用 R 的自动转换

  # 逻辑向量：TRUE 表示保留该列，FALSE 表示移除
  keep_cols <- sapply(data, function(x) {

    # 如果列不是数值型（例如分组的字符列），通常直接保留，不参与删除逻辑
    # 如果你也想删掉全是 "0" 字符的列，可以去掉下面这行 if
    if (!is.numeric(x)) return(TRUE)

    # 核心判断
    if (type == "all") {
      # 情况 1：如果 "不是" 全是 0，则保留 ( !all(...) )
      return(!all(x == 0, na.rm = na.rm))
    } else {
      # 情况 2：如果 "不是" 存在任意 0，则保留 ( !any(...) )
      return(!any(x == 0, na.rm = na.rm))
    }
  })

  # 3. 筛选数据
  # drop = FALSE 保证如果只剩一列，依然保持为数据框格式
  data_clean <- data[, keep_cols, drop = FALSE]

  # 4. (可选) 输出提示信息
  removed_count <- ncol(data) - ncol(data_clean)
  if (removed_count > 0) {
    message(paste("Removed", removed_count, "column(s) based on type =", type))
  }

  return(data_clean)
}
