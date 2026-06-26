#' 读取并合并 Excel 文件中的所有工作表read_and_combine_sheets("data.xlsx)
#'
#' 该函数读取一个 Excel 文件中的所有工作表，将它们合并为一个数据框，
#' 并添加一个标识原始工作表的列。
#'
#' @param file_path 字符向量，Excel 文件的路径（支持 .xlsx 和 .xls 格式）
#'
#' @return 返回一个合并后的数据框，包含以下列：
#' \describe{
#'   \item{sheet_name}{字符向量，标识数据来源的工作表名称}
#'   \item{其他列}{原始工作表中的所有列}
#' }
#' 数据框会按照工作表名称和原始列的顺序排列。
#'
#' @details
#' 函数执行流程：
#' \enumerate{
#'   \item 读取 Excel 文件中的所有工作表名称
#'   \item 逐个读取每个工作表，并添加工作表名称列
#'   \item 将所有工作表数据合并为一个数据框
#'   \item 调整列顺序，使工作表名称列位于第一列
#' }
#'
#' @note
#' 重要注意事项：
#' \itemize{
#'   \item 要求 Excel 文件中的所有工作表具有相同的列结构
#'   \item 如果工作表列名不一致，合并时可能会产生意外结果
#'   \item 函数会自动处理不同工作表中的数据类型差异
#' }
#'
#' @examples
#' \dontrun{
#' # 读取并合并 Excel 文件中的所有工作表
#' combined_data <- read_and_rbind_sheets("data.xlsx")
#'
#' # 查看合并后的数据结构
#' str(combined_data)
#'
#' # 查看不同工作表的记录数
#' table(combined_data$sheet_name)
#' }
#'
#' @importFrom readxl excel_sheets read_excel
#' @importFrom dplyr select everything
#' @export
read_and_rbind_sheets <- function(file_path) {
  # 获取所有sheet名称
  sheet_names <- readxl::excel_sheets(file_path)
  # 读取所有sheet并添加sheet名称列
  combined_data <- lapply(sheet_names, function(sheet) {
    data <- readxl::read_excel(file_path, sheet = sheet)
    data$sheet_name <- sheet  # 添加sheet名称列
    return(data)
  })
  result <- do.call(rbind, combined_data)
  result <- result %>%
    dplyr::select(sheet_name, dplyr::everything())
  return(result)
}
