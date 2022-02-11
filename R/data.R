#' Timestamps of 128 users with public bedtimes and public time zones
#'
#' A dataset containing  2,036,843 timestamps of posts of
#' 128 Reddit users with public bedtimes and public time zones.
#'
#' @format A data frame with 2,036,843 rows and 2 variables:
#' \describe{
#'   \item{author}{random identifier for a unique Reddit user}
#'   \item{created_utc}{timestamps, in Unix epoch time format}
#'   ...
#' }
#' @source \url{https://psyarxiv.com/9mpbw/}
"core_timestamps"


#' Time zones of 128 users with public bedtimes and public time zones
#'
#' A dataset containing the time zones of 128 Reddit users with publicly implied timezones and
#' publicly reproted bedtimes.
#'
#' @format A data frame with 128 rows and 2 variables:
#' \describe{
#'   \item{author}{random identifier for a unique Reddit user}
#'   \item{timezone}{time zone, in Time Zone Database format}
#'   ...
#' }
#' @source \url{https://psyarxiv.com/9mpbw/}
"core_timezones"
