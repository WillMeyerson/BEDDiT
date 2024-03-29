% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_bedtimes.R
\name{aggregate_mean}
\alias{aggregate_mean}
\title{calculate bedtime-relativized mean posting frequency across all bedtimes per 15-min interval}
\usage{
aggregate_mean(mag)
}
\arguments{
\item{mag}{bedtime-relativized aggregated-by-bedtime frequency data frame}
}
\value{
bedtime-relativized mean posting frequency across all bedtimes per 15-min interval
}
\description{
calculate bedtime-relativized mean posting frequency across all bedtimes per 15-min interval
}
\examples{
my_timestamps <- data.frame(author=c(1,1,2,2), created_utc=1.6*10^9+c(0,1000,2000,3000))
my_bedtimes <- data.frame(author=c(1,2), nightbed=c(-1,1))
my_timezones <- create_dummy_timezones(my_timestamps)
my_bins <- time_of_day_binner(assign_time_of_day(my_timestamps, my_timezones))
my_binsb <- aggregate_by_bedtime(my_bins, my_bedtimes)
my_mag <- multi_relativizer(my_binsb)
my_mag <- filter_sufficient(my_mag, 1)
aggregate_mean(my_mag)
}
