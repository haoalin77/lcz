#' 收集当前环境中符合特定命名规则的对象
#'
#' @description
#' 该函数用于查找当前环境中以指定字符串“开头”或“结尾”的所有对象，
#' 并将它们的值收集到一个命名列表中返回。
#'
#' @param pattern 字符型。用于匹配的关键词（例如 "data" 或 "model"）。
#' @param position 字符型。指定匹配位置：
#' \itemize{
#'   \item \code{"start"}: 匹配以 pattern 开头的对象（默认）。
#'   \item \code{"end"}: 匹配以 pattern 结尾的对象。
#'   \item \code{"any"}: 只要名字中包含 pattern 即可。
#' }
#' @param envir 环境。指定在哪个环境中搜索，默认为调用该函数的环境 (parent.frame)。
#'
#' @return 一个命名列表 (Named List)，包含所有匹配到的对象。如果没有匹配到对象，返回空列表。
#'
#'
#' @examples
#' \dontrun{
#' # 1. 创建一些测试数据
#' data_2020 <- 1:10
#' data_2021 <- 11:20
#' model_lm <- "Linear Model"
#' result_lm <- "Result"
#' test_data <- "Test"
#'
#' # 2. 获取所有以 "data" 开头的对象
#' list_start <- collect_objects("data", position = "start")
#' # 结果包含: data_2020, data_2021
#'
#' # 3. 获取所有以 "lm" 结尾的对象
#' list_end <- collect_objects("lm", position = "end")
#' # 结果包含: model_lm, result_lm
#' }
#'
#' @export
collect_objects <- function(pattern, position = c("start", "end", "any"), envir = parent.frame()) {

  # 1. 检查参数，确保 position 输入正确，默认为 start
  position <- match.arg(position)

  # 2. 根据位置构建正则表达式
  # ^ 代表字符串开头
  # $ 代表字符串结尾
  regex_pattern <- switch(position,
                          "start" = paste0("^", pattern),
                          "end"   = paste0(pattern, "$"),
                          "any"   = pattern)

  # 3. 获取所有匹配的变量名
  target_names <- ls(pattern = regex_pattern, envir = envir)

  # 4. 检查是否找到了对象
  if (length(target_names) == 0) {
    warning(paste("No objects found matching pattern:", pattern, "at position:", position))
    return(list())
  }

  # 5. 批量获取对象的值并返回列表
  # mget 会返回一个列表，列表名就是变量名
  result_list <- mget(target_names, envir = envir)

  return(result_list)
}
