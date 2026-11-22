#' Estimate the mean and standard deviation from the median and range
#'
#' Estimates the mean, standard deviation and variance of a sample from its
#' median, minimum, maximum and size, with the methods of Hozo, Djulbegovic
#' and Hozo (2005). For each study, the function uses the estimator that the
#' authors recommend for its sample size. The inputs are vectorised, so one
#' call can convert every study in a meta-analysis.
#'
#' @param median Numeric vector of sample medians.
#' @param minimum Numeric vector of sample minimums.
#' @param maximum Numeric vector of sample maximums.
#' @param sample_size Numeric vector of sample sizes. Each value must be a
#'   whole number of at least 2.
#' @param mean_formula The formula for the mean when the sample size is 25 or
#'   less. `"5"` (the default) uses formula (5) of Hozo et al., as their
#'   Table 3 recommends. `"4"` uses formula (4), which adds a small
#'   correction for sample size.
#'
#' @details
#' Let \eqn{a}, \eqn{m}, \eqn{b} and \eqn{n} be the minimum, median, maximum
#' and sample size.
#'
#' When \eqn{n \le 25}{n <= 25}, the mean is estimated by formula (5),
#' \deqn{\bar{x} \approx \frac{a + 2m + b}{4},}{mean = (a + 2m + b) / 4,}
#' or by formula (4) if `mean_formula = "4"`,
#' \deqn{\bar{x} \approx \frac{a + 2m + b}{4} + \frac{a - 2m + b}{4n}.}{mean = (a + 2m + b) / 4 + (a - 2m + b) / (4n).}
#' When \eqn{n > 25}, the median is the estimate of the mean.
#'
#' When \eqn{n \le 15}{n <= 15}, the standard deviation is the square root of
#' formula (16),
#' \deqn{S^2 \approx \frac{1}{12} \left( \frac{(a - 2m + b)^2}{4} + (b - a)^2 \right).}{S^2 = ((a - 2m + b)^2 / 4 + (b - a)^2) / 12.}
#' When \eqn{15 < n \le 70}{15 < n <= 70}, it is the range divided by 4. When
#' \eqn{n > 70}, it is the range divided by 6.
#'
#' These sample-size bands follow Table 3 of Hozo et al. (2005). The authors
#' tested the estimators by simulation on samples of 8 to 100, so the
#' function warns when a sample size is below 8. It also warns when the
#' minimum equals the maximum. The estimated standard deviation is then 0,
#' and an inverse-variance meta-analysis cannot weight that study.
#'
#' A missing value (`NA`) in any input gives `NA` in the matching row of the
#' result.
#'
#' @return A data frame with one row per study and these columns:
#' \describe{
#'   \item{mean}{The estimated mean.}
#'   \item{sd}{The estimated standard deviation.}
#'   \item{variance}{The estimated variance, equal to `sd^2`.}
#'   \item{mean_method}{The estimator used for the mean.}
#'   \item{sd_method}{The estimator used for the standard deviation.}
#' }
#'
#' @references
#' Hozo SP, Djulbegovic B, Hozo I (2005). Estimating the mean and variance
#' from the median, range, and the size of a sample. *BMC Medical Research
#' Methodology*, 5, 13. \doi{10.1186/1471-2288-5-13}
#'
#' @examples
#' # One study: median 10, range 5 to 15, 30 participants
#' estimate_stats(median = 10, minimum = 5, maximum = 15, sample_size = 30)
#'
#' # Several studies in one call
#' studies <- data.frame(
#'   study   = c("A", "B", "C"),
#'   median  = c(12, 30, 7.5),
#'   minimum = c(4, 18, 2),
#'   maximum = c(25, 52, 16),
#'   n       = c(12, 40, 120)
#' )
#' est <- estimate_stats(studies$median, studies$minimum,
#'                       studies$maximum, studies$n)
#' cbind(studies, est)
#'
#' # Formula (4) for the mean of a small sample
#' estimate_stats(10, 5, 15, 12, mean_formula = "4")
#' @export
estimate_stats <- function(median, minimum, maximum, sample_size,
                           mean_formula = c("5", "4")) {
  mean_formula <- match.arg(as.character(mean_formula), c("5", "4"))

  inputs <- list(median = median, minimum = minimum, maximum = maximum,
                 sample_size = sample_size)
  for (name in names(inputs)) {
    x <- inputs[[name]]
    if (!is.numeric(x) && !(is.logical(x) && all(is.na(x)))) {
      stop("`", name, "` must be numeric.", call. = FALSE)
    }
  }

  lens <- lengths(inputs)
  if (any(lens == 0L)) {
    stop("Each input must have at least one value.", call. = FALSE)
  }
  k <- max(lens)
  if (any(lens != 1L & lens != k)) {
    stop("`median`, `minimum`, `maximum` and `sample_size` must have the ",
         "same length, or length 1.", call. = FALSE)
  }

  m <- rep_len(as.numeric(median), k)
  a <- rep_len(as.numeric(minimum), k)
  b <- rep_len(as.numeric(maximum), k)
  n <- rep_len(as.numeric(sample_size), k)

  complete <- !(is.na(m) | is.na(a) | is.na(b) | is.na(n))

  stop_rows(complete & !(is.finite(m) & is.finite(a) &
                           is.finite(b) & is.finite(n)),
            "Inputs must be finite")
  stop_rows(complete & (n < 2 | abs(n - round(n)) > sqrt(.Machine$double.eps)),
            "`sample_size` must be a whole number of at least 2")
  stop_rows(complete & a > m, "`minimum` is greater than `median`")
  stop_rows(complete & m > b, "`median` is greater than `maximum`")

  # Mean: formula (5), or formula (4) on request, up to n = 25; then the median
  mean_small <- (a + 2 * m + b) / 4
  if (mean_formula == "4") {
    mean_small <- mean_small + (a - 2 * m + b) / (4 * n)
  }
  est_mean <- ifelse(n <= 25, mean_small, m)
  mean_method <- ifelse(n <= 25, paste0("Hozo formula (", mean_formula, ")"),
                        "median")

  # SD: square root of formula (16) up to n = 15, range/4 up to n = 70, then range/6
  sd_small <- sqrt(((a - 2 * m + b)^2 / 4 + (b - a)^2) / 12)
  est_sd <- ifelse(n <= 15, sd_small,
                   ifelse(n <= 70, (b - a) / 4, (b - a) / 6))
  sd_method <- ifelse(n <= 15, "Hozo formula (16)",
                      ifelse(n <= 70, "range/4", "range/6"))

  est_mean <- as.numeric(est_mean)
  est_sd <- as.numeric(est_sd)
  mean_method <- as.character(mean_method)
  sd_method <- as.character(sd_method)
  est_mean[!complete] <- NA_real_
  est_sd[!complete] <- NA_real_
  mean_method[!complete] <- NA_character_
  sd_method[!complete] <- NA_character_

  small_n <- which(complete & n < 8)
  if (length(small_n) > 0L) {
    warning("Sample size below 8 in ", format_rows(small_n), ". ",
            "Hozo et al. (2005) tested their estimators on samples of ",
            "8 to 100.", call. = FALSE)
  }
  zero_range <- which(complete & a == b)
  if (length(zero_range) > 0L) {
    warning("Minimum equals maximum in ", format_rows(zero_range), ", so ",
            "the estimated standard deviation is 0.", call. = FALSE)
  }

  data.frame(mean = est_mean, sd = est_sd, variance = est_sd^2,
             mean_method = mean_method, sd_method = sd_method,
             stringsAsFactors = FALSE)
}

# Describe row numbers for messages, for example "rows 2, 5".
format_rows <- function(rows) {
  shown <- paste(rows[seq_len(min(5L, length(rows)))], collapse = ", ")
  if (length(rows) > 5L) {
    shown <- paste0(shown, ", ...")
  }
  paste0(if (length(rows) == 1L) "row " else "rows ", shown)
}

# Stop with a message that names the rows where `bad` is TRUE.
stop_rows <- function(bad, message) {
  rows <- which(bad)
  if (length(rows) > 0L) {
    stop(message, " (", format_rows(rows), ").", call. = FALSE)
  }
  invisible(NULL)
}
