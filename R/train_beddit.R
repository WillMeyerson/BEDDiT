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
#' core_bins <- bin_beddit(timestamps = core_timestamps, timezones = core_timezones)
#' train_beddit(timebins = core_bins, bedtimes = core_reportedbedtimes)
train_beddit <- function(timebins, bedtimes, necessary_authors=10) {
  aggregate_by_bedtime(timebins, bedtimes) %>%
    multi_relativizer %>%
    filter_sufficient(necessary_authors) %>%
    find_best_parabola
}
