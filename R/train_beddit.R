#' Train a model from a collection of authors' circadian footprints and their known bedtimes
#'
#' @param timebins circadian footprint of each author in authors' local time
#' @param bedtimes data frame with columns author and nightbed where 0=midnight
#' @param necessary_authors in training model only include bedtimes represented by at least this many authors
#'
#' @return idealized unified model of Reddit posting frequency at each time bin relative to bedtime
#' @export
#'
#' @examples
#' my_timestamps <- data.frame(author=c(1,1,2,2), created_utc=1.6*10^9+c(0,1000,2000,3000))
#' my_bedtimes <- data.frame(author=c(1,2), nightbed=c(-1,1))
#' my_timezones <- create_dummy_timezones(my_timestamps)
#' my_bins <- time_of_day_binner(assign_time_of_day(my_timestamps, my_timezones))
#' train_beddit(my_timebins, my_bedtimes)
train_beddit <- function(timebins, bedtimes, necessary_authors=10) {
  aggregate_by_bedtime(timebins, bedtimes) %>%
    multi_relativizer %>%
    filter_sufficient(necessary_authors) %>%
    return(find_best_parabola)
}
