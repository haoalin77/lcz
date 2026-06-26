#' Calculate Rotifer Life Table and Population Parameters
#'
#' @description
#' Calculates life table parameters for rotifers based on observational data.
#' Core functions include calculating age-specific survival rate (lx), mortality rate (qx),
#' and fecundity (mx). It also solves the Euler-Lotka equation to determine the
#' exact intrinsic rate of increase (rm).
#' The output 'Summary' table is in wide format, suitable for binding multiple groups.
#'
#' @param data A data frame containing the life table data.
#' @param col_time Character string. The column name for time (e.g., hours), default is "x".
#' @param col_survivors Character string. The column name for the number of survivors, default is "nx".
#' @param col_amictic Character string. The column name for amictic (asexual) offspring, default is "amictic".
#' @param col_mictic Character string. The column name for mictic (sexual) offspring, default is "mictic".
#'
#' @return A list containing two elements:
#' \item{Summary}{A data frame with one row containing core parameters: R0, T, Mixis_Rate, rm (hour/day), and Euler_Check.}
#' \item{Detail}{A data frame containing the detailed life table (lx, qx, mx) for each time point.}
#'
#' @importFrom stats uniroot
#' @importFrom utils head tail
#' @export
calc_rotifer_params <- function(data,
                                col_time = "x",
                                col_survivors = "nx",
                                col_amictic = "amatic",
                                col_mictic = "matic") {

  # =========================================================
  # 1. Data Extraction
  # =========================================================
  # Note: The default column names above contain Chinese characters as per your original data.
  # If you change your column names to English in your CSV, update the defaults above.

  req_cols <- c(col_time, col_survivors, col_amictic, col_mictic)
  if (!all(req_cols %in% names(data))) {
    stop("Specified columns not found in the data frame.")
  }

  time_obs      <- data[[col_time]]
  survivors_obs <- data[[col_survivors]]
  off_total     <- data[[col_amictic]] + data[[col_mictic]]

  # =========================================================
  # 2. Vector Calculations (lx, qx, mx)
  # =========================================================

  # --- lx (Survivorship) ---
  # lx = nx / n0 (max initial population)
  lx <- survivors_obs / max(survivors_obs, na.rm = TRUE)

  # --- qx (Mortality Rate) ---
  # Logic: (current - next) / current
  nx_next <- c(survivors_obs[-1], 0)

  # Avoid division by zero
  safe_surv <- survivors_obs
  safe_surv[safe_surv == 0] <- 1

  qx <- (survivors_obs - nx_next) / safe_surv

  # Clean up qx
  qx[survivors_obs == 0] <- 0
  qx[qx < 0] <- 0 # Fix negative values due to recording errors
  if (utils::tail(survivors_obs, 1) > 0) {
    qx[length(qx)] <- 1 # Assume death at the next step if survivors remain
  }

  # --- mx (Fecundity) ---
  # Logic: Current Total Offspring / Previous Survivors (Lagged)
  survivors_prev <- c(NA, utils::head(survivors_obs, -1))
  mx <- off_total / survivors_prev

  # Clean up mx
  # Set to 0 if previous survivors were 0, or if value is NA/Inf
  mx[is.na(mx) | survivors_prev == 0 | is.infinite(mx)] <- 0

  # =========================================================
  # 3. Core Parameter Calculation
  # =========================================================

  # Net Reproductive Rate (R0)
  R0 <- sum(lx * mx, na.rm = TRUE)

  # Mixis Rate (Total Mictic / Total Offspring)
  mictic_rate <- 0
  sum_off_total <- sum(off_total, na.rm = TRUE)
  if (sum_off_total > 0) {
    mictic_rate <- sum(data[[col_mictic]], na.rm = TRUE) / sum_off_total
  }

  # Mean Generation Time (T)
  # Approx formula: sum(lx * mx * x) / R0
  T_gen <- 0
  if (R0 > 0) {
    T_gen <- sum(lx * mx * time_obs, na.rm = TRUE) / R0
  }

  # Intrinsic Rate of Increase (rm) - Approximation
  rm_origin <- 0
  if (R0 > 0 && T_gen > 0) {
    rm_origin <- log(R0) / T_gen
  }

  # Exact rm solving (Euler-Lotka Equation)
  final_trm <- 0
  euler_check <- 0

  if (R0 > 0) {
    # Target value slightly less than 1 to ensure convergence
    target_val <- 0.9999995

    # Define function to zero
    euler_func <- function(r) {
      sum(exp(-r * time_obs) * lx * mx, na.rm = TRUE) - target_val
    }

    # Solve using uniroot
    try({
      sol <- stats::uniroot(euler_func, interval = c(-10, 10), extendInt = "yes", tol = 1e-9)
      final_trm <- sol$root
      # Verification
      euler_check <- sum(exp(-final_trm * time_obs) * lx * mx, na.rm = TRUE)
    }, silent = TRUE)
  }

  # =========================================================
  # 4. Result Summarization (Wide Format)
  # =========================================================

  # Detailed Table
  detail_table <- data.frame(
    Time = time_obs,
    nx = survivors_obs,
    lx = round(lx, 4),
    qx = round(qx, 4),
    mx = round(mx, 4)
  )

  # Summary Table
  summary_table <- data.frame(
    R0 = R0,
    T_hours = T_gen,
    Mixis_Rate = mictic_rate,
    rm_approx_h = rm_origin,
    rm_exact_h = final_trm,
    rm_exact_day = final_trm * 24,
    Euler_Check = euler_check
  )

  return(list(Summary = summary_table, Detail = detail_table))
}
