#' Estimate bedtimes and compare to reported bedtimes
#'
#' @param timebins circadian fingerpints of users
#' @param model an idealized model of circadian posting
#' @param bedtimes reported bedtimes of users
#' @param threshold how good a fit required to output result
#'
#' @return a data frame containing both reported and inferred bedtimes
#' @export
#'
#' @examples
#' test_beddit(validation_binned_local, default_model, validation_reportedbedtimes)
test_beddit <- function(timebins, model, bedtimes, threshold=0.25) {
  estimated_times <- apply_beddit(timebins, model)
  compared_inferred_to_reported_bedtimes(estimated_times, bedtimes, threshold)
}
