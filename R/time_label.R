#' Generate a timestamp string
#'
#' Creates a timestamp in the format "mmdd_HH_MM_SS" (e.g., "0615_14_30_45")
#' based on the current system time. All components are numeric and ASCII‑safe.
#'
#' @return A character string of length 1 representing the current timestamp.
#' @export
#'
#' @examples
#' time_label()
time_label <- function(){
  nowtime <- format(Sys.time(), "%m%d_%H_%M_%S")
  return(nowtime)
}
