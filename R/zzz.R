.onLoad <- function(libname, pkgname) {
  cat("\n=============================================\n")
  cat("Our story began in summer, a summer that knows no end.\n")
  cat("best love\n")
  cat("=============================================\n")
}

.onAttach <- function(libname, pkgname) {
  # 如果用户装了 crayon 包，就用彩色，没装就用普通文本
  if (requireNamespace("crayon", quietly = TRUE)) {
    msg <- paste0(
      crayon::green("Our story began in summer, a summer that knows no end.！\n"),
      crayon::silver("Best love for best you"),
      crayon::blue("dcz")
    )
    packageStartupMessage(msg)
  } else {
    packageStartupMessage("✅ lcz 包加载成功！")
  }
}
