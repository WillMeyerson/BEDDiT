#' Convert timestamps data frame into circadian footprint
#'
#' @param timestamps a data frame with columns author and created_utc
#' @param timezones a data frame with columns author and timezone
#'
#' @return time bins object that gives the circadian footprint of each user
#' @export
#'
#' @examples
#' my_timestamps <- data.frame(author=c(1,1,2,2), created_utc=1.6*10^9+c(0,1000,2000,3000))
#' bin_beddit(my_timestamps, timezones=FALSE)
bin_beddit <- function(timestamps, timezones) {
  if(!timezones) {
    timezones <- create_dummy_timezones(timestamps)
  }
  return(time_of_day_binner(assign_time_of_day(timestamps, timezones)))
}
