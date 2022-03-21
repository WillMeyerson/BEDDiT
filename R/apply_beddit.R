#' Estimate bedtimes of users from circadian fingerpints and model
#'
#' @param bins circadian fingerpints of users
#' @param model a unified idealized model
#'
#' @return inferred bedtimes of users
#' @export
#'
#' @examples
#' apply_beddit(validation_binned_local, default_model)
apply_beddit <- function(bins, model=default_model) {
return(estimate_bedtimes(bins, model))
}
