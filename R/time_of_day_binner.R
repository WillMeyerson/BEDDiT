#' Creates a default (UTC) timezones object for a set of timestamps
#'
#' @param timestamps a dataframe with columns author and created_UTC
#'
#' @return a timezones object: a data frame with 1 row for each author, with timezone listed as "UTC"
#' @export
#'
#' @examples
#' my_timestamps <- data.frame(author=c(1,1,2,2), created_utc=1.6*10^9+c(0,1000,2000,3000))
#' create_dummy_timezones(my_timestamps)
create_dummy_timezones <- function(timestamps) {
  uauth = unique(sort(timestamps$author))
  return(data.frame(author=uauth, timezone="UTC"))
}

#' Add human readable columns to a timestamps object
#'
#' @param timestamps a dataframe with columns author and created_UTC
#' @param timezones a dataframe with columns author and timezone
#'
#' @return a timestamps object that includes extra columns for hour, 15-minute-interval within the hour, date, and day of week
#' @export
#'
#' @examples
#' my_timestamps <- data.frame(author=c(1,1,2,2), created_utc=1.6*10^9+c(0,1000,2000,3000))
#' my_timezones <- create_dummy_timezones(my_timestamps)
#' assign_time_of_day(my_timestamps, my_timezones)
assign_time_of_day <- function(timestamps, timezones) {
  timestamps$timezone <- timezones$timezone[match(timestamps$author, timezones$author)]
  utz = sort(unique(timestamps$timezone))
  timestamps$hour <- NA
  timestamps$m15 <- NA
  timestamps$date <- NA
  timestamps$weekday <-  NA
  timestamps$created_utc <- as.numeric(timestamps$created_utc)
  timestamps <- timestamps[!is.na(timestamps$created_utc),]
  for(uu in utz) {
    timestamps$hour[timestamps$timezone==uu] <- lubridate::hour(lubridate::with_tz(as.POSIXct(timestamps$created_utc[timestamps$timezone==uu], origin="1970-01-01"), tz=uu))
    timestamps$m15[timestamps$timezone==uu] <- floor(lubridate::minute(lubridate::with_tz(as.POSIXct(timestamps$created_utc[timestamps$timezone==uu], origin="1970-01-01"), tz=uu))/15)/4
    timestamps$date[timestamps$timezone==uu] <- lubridate::date(lubridate::with_tz(as.POSIXct(timestamps$created_utc[timestamps$timezone==uu], origin="1970-01-01"), tz=uu))
    timestamps$weekday[timestamps$timezone==uu] <- lubridate::wday(lubridate::with_tz(as.POSIXct(timestamps$created_utc[timestamps$timezone==uu], origin="1970-01-01"), tz=uu)) - 1
  }
  timestamps$date <- as.Date(timestamps$date, origin="1970-01-01")
  return(timestamps)
}


#' Creates circadian fingerprint
#'
#' @param timestamps a timestamps object that includes human-readable columns
#'
#' @return time bins object that gives the circadian footprint of each user
#' @export
#'
#' @examples
#' my_timestamps <- data.frame(author=c(1,1,2,2), created_utc=1.6*10^9+c(0,1000,2000,3000))
#' my_timezones <- create_dummy_timezones(my_timestamps)
#' time_of_day_binner(assign_time_of_day(my_timestamps, my_timezones))
time_of_day_binner <- function(timestamps) {
  timestamps$q15 <- 4*timestamps$hour + 4*timestamps$m15
  hx <- timestamps[!duplicated(timestamps[,c("author", "q15", "date")]),]
  af <- stats::aggregate(q15 ~ author, FUN=counterQ15, hx)
  af <- as.data.frame(cbind(af[,1], af[,-1]))
  names(af)[1] <- "author"
  names(af)[-1] <- paste("q", 0:95, sep="")
  for(j in 2:ncol(af)) {
    af[,j] <- as.numeric(af[,j])
  }
  af$tot <- rowSums(af[,2:97])
  return(af)
}


#' Find average circadian fingerprint for each (rounded) bedtime
#'
#' @param bins a time bins object that lists the circadian footprint of each user
#' @param bedtimes a bedtimes object with columns author and nightbed; nightbed is bedtime where 0=midngiht, -12=noon
#'
#' @return circadian meta-footprint for each rounded bedtime
#' @export
#'
#' @examples
#' my_timestamps <- data.frame(author=c(1,1,2,2), created_utc=1.6*10^9+c(0,1000,2000,3000))
#' my_bedtimes <- data.frame(author=c(1,1,2,2), nightbed=c(-1,1))
#' my_timezones <- create_dummy_timezones(my_timestamps)
#' my_bins <- time_of_day_binner(assign_time_of_day(my_timestamps, my_timezones))
#' aggregate_by_bedtime(my_bins, my_bedtimes)
aggregate_by_bedtime <- function(bins, bedtimes) {
  binsb <- merge(bins, bedtimes)
  binsb$bedtime <- round(binsb$nightbed)
  binsb[,2:97] <- binsb[,2:97]/binsb$tot
  mag <- stats::aggregate(stats::as.formula(paste("author ~ bedtime", sep="")), FUN=length, data=binsb)
  for(i in 0:95) {
    mag <- merge(mag, stats::aggregate(stats::as.formula(paste("q", i, " ~ bedtime", sep="")), FUN=mean, data=binsb))
  }
  return(mag)
}



#' Counts the number of instances of values 0 through 95
#'
#' @param Z integer vector of what 15-minute intervals of the day a set of posts occurred at
#'
#'
#' @return the counts of posts at each 15-minute interval of the day
#' @export
#'
#' @examples
#' my_timestamps <- data.frame(author=c(1,1,2,2), created_utc=1.6*10^9+c(0,1000,2000,3000))
#' my_timezones <- create_dummy_timezones(my_timestamps)
#' my_timestamps <- assign_time_of_day(my_timestamps, my_timezones)
#' my_timestamps$q15 <- 4*my_timestamps$hour + 4*my_timestamps$m15
#'  hx <- my_timestamps[!duplicated(my_timestamps[,c("author", "q15", "date")]),]
#'  af <- stats::aggregate(q15 ~ author, FUN=counterQ15, hx)
counterQ15 <- function(Z) {
  return(c(
    sum(Z==0), sum(Z==1), sum(Z==2), sum(Z==3),
    sum(Z==4), sum(Z==5), sum(Z==6), sum(Z==7),
    sum(Z==8), sum(Z==9), sum(Z==10), sum(Z==11),
    sum(Z==12), sum(Z==13), sum(Z==14), sum(Z==15),
    sum(Z==16), sum(Z==17), sum(Z==18), sum(Z==19),
    sum(Z==20), sum(Z==21), sum(Z==22), sum(Z==23),

    sum(Z==24), sum(Z==25), sum(Z==26), sum(Z==27),
    sum(Z==28), sum(Z==29), sum(Z==30), sum(Z==31),
    sum(Z==32), sum(Z==33), sum(Z==34), sum(Z==35),
    sum(Z==36), sum(Z==37), sum(Z==38), sum(Z==39),
    sum(Z==40), sum(Z==41), sum(Z==42), sum(Z==43),
    sum(Z==44), sum(Z==45), sum(Z==46), sum(Z==47),

    sum(Z==48), sum(Z==49), sum(Z==50), sum(Z==51),
    sum(Z==52), sum(Z==53), sum(Z==54), sum(Z==55),
    sum(Z==56), sum(Z==57), sum(Z==58), sum(Z==59),
    sum(Z==60), sum(Z==61), sum(Z==62), sum(Z==63),
    sum(Z==64), sum(Z==65), sum(Z==66), sum(Z==67),
    sum(Z==68), sum(Z==69), sum(Z==70), sum(Z==71),

    sum(Z==72), sum(Z==73), sum(Z==74), sum(Z==75),
    sum(Z==76), sum(Z==77), sum(Z==78), sum(Z==79),
    sum(Z==80), sum(Z==81), sum(Z==82), sum(Z==83),
    sum(Z==84), sum(Z==85), sum(Z==86), sum(Z==87),
    sum(Z==88), sum(Z==89), sum(Z==90), sum(Z==91),
    sum(Z==92), sum(Z==93), sum(Z==94), sum(Z==95)
  ))
}



#' Finds the circular mean of a set of clock times
#'
#' @param x clock times (in hours)
#'
#' @return circular mean of the clock times
#' @export
#'
#' @examples
#' clock.mean(c(-11,11))
clock.mean <- function (x)
{
  sinr <- sum(sin(2*pi*x/24))
  cosr <- sum(cos(2*pi*x/24))
  circmean <- atan2(sinr, cosr)
  return(circmean*24/2/pi)
}
