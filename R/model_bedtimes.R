


#' cyclically permutes a vector
#'
#' @param x vector to be permuted
#' @param n how far to permute
#'
#' @return pertmuted vector
#' @export
#'
#' @examples
#' x <- 0:95
#' shifter(x, 4)
shifter <- function(x, n = 1) {
  if (n == 0) x else c(utils::tail(x, -n), utils::head(x, n))
}


#' permutes circadian fingerprint from 1st column as midnight to 1st col as 8 hrs before bed
#'
#' @param bins_matrix for each user, 96 columns represent posting frequency in each 15 min interval
#' @param bedtime a single numerical bedtime in hours after midnight
#'
#' @return bedtime-relativezed bins_matrix, where column 1 is 8 hours before bedtime
#' @export
#'
#' @examples
#' my_timestamps <- data.frame(author=c(1,1,2,2), created_utc=1.6*10^9+c(0,1000,2000,3000))
#' my_bedtimes <- data.frame(author=c(1,2), nightbed=c(-1,-1))
#' my_timezones <- create_dummy_timezones(my_timestamps)
#' my_bins <- time_of_day_binner(assign_time_of_day(my_timestamps, my_timezones))
#' relativizer(my_bins[,2:97], -1)
relativizer <- function(bins_matrix,
                       bedtime) {
  return(bins_matrix[, shifter(1:96, 64 + bedtime*4)])

}



#' Relativizes the bins of each bedtime within a bins_bedtime data frame
#'
#' @param binsb a data frame with cols 3:98 as midnight-relativized freqs and there is bedtime col
#'
#' @return a data frame with cols 3:98 as bedtime-relativized freqs and there is bedtime col
#' @export
#'
#' @examples
#' my_timestamps <- data.frame(author=c(1,1,2,2), created_utc=1.6*10^9+c(0,1000,2000,3000))
#' my_bedtimes <- data.frame(author=c(1,2), nightbed=c(-1,1))
#' my_timezones <- create_dummy_timezones(my_timestamps)
#' my_bins <- time_of_day_binner(assign_time_of_day(my_timestamps, my_timezones))
#' my_binsb <- aggregate_by_bedtime(my_bins, my_bedtimes)
#' multi_relativizer(my_binsb)
multi_relativizer <- function(binsb) {
  out <- binsb
  ubedtime <- unique(binsb$bedtime)
  for(i in 1:length(ubedtime)) {
    out[which(out$bedtime==ubedtime[i]),3:98] <- relativizer(binsb[which(out$bedtime==ubedtime[i]),3:98], ubedtime[i])
  }
  names(out)[3:98] <- paste("r", seq(-8,15.75, 0.25), sep="")
  return(out)
}



#' filters aggregated by bedtime object to include rows representing at least threshhold authors
#'
#' @param mag aggregated-by-bedtime data frame
#' @param threshhold the minimum number of authors required to include a bedtime
#'
#' @return filtered aggregated-by-bedtime data frame
#' @export
#'
#' @examples
#' my_timestamps <- data.frame(author=c(1,1,2,2), created_utc=1.6*10^9+c(0,1000,2000,3000))
#' my_bedtimes <- data.frame(author=c(1,2), nightbed=c(-1,1))
#' my_timezones <- create_dummy_timezones(my_timestamps)
#' my_bins <- time_of_day_binner(assign_time_of_day(my_timestamps, my_timezones))
#' my_binsb <- aggregate_by_bedtime(my_bins, my_bedtimes)
#' my_mag <- multi_relativizer(my_binsb)
#' filter_sufficient(my_mag, 1)
filter_sufficient <- function(mag, threshhold=10) {
  return(mag[which(mag$author >= threshhold),])
}



#' calculate bedtime-relativized mean posting frequency across all bedtimes per 15-min interval
#'
#' @param mag bedtime-relativized aggregated-by-bedtime frequency data frame
#'
#' @return bedtime-relativized mean posting frequency across all bedtimes per 15-min interval
#' @export
#'
#' @examples
#' my_timestamps <- data.frame(author=c(1,1,2,2), created_utc=1.6*10^9+c(0,1000,2000,3000))
#' my_bedtimes <- data.frame(author=c(1,2), nightbed=c(-1,1))
#' my_timezones <- create_dummy_timezones(my_timestamps)
#' my_bins <- time_of_day_binner(assign_time_of_day(my_timestamps, my_timezones))
#' my_binsb <- aggregate_by_bedtime(my_bins, my_bedtimes)
#' my_mag <- multi_relativizer(my_binsb)
#' my_mag <- filter_sufficient(my_mag, 1)
#' aggregate_mean(my_mag)
aggregate_mean <- function(mag) {
  return(data.frame(relhour=seq(-8,15.75,0.25),  obsFreq=as.numeric(colMeans(mag[,3:98]))))
}



#' Fits unified idealized model
#'
#' @param mag bedtime-relativized posting frequencies aggregated by bedtime
#' @param out_prefix filename prefix
#' @param write set to TRUE if you wish to write the model to disk
#'
#' @return unified idealized model
#' @export
#'
#' @examples
#' my_timestamps <- data.frame(author=c(1,1,2,2), created_utc=1.6*10^9+c(0,1000,2000,3000))
#' my_bedtimes <- data.frame(author=c(1,2), nightbed=c(-1,1))
#' my_timezones <- create_dummy_timezones(my_timestamps)
#' my_bins <- time_of_day_binner(assign_time_of_day(my_timestamps, my_timezones))
#' my_binsb <- aggregate_by_bedtime(my_bins, my_bedtimes)
#' my_mag <- multi_relativizer(my_binsb)
#' my_mag <- filter_sufficient(my_mag, 1)
#' find_best_parabola(my_mag)
find_best_parabola <- function(mag, out_prefix="my_prefix", write=F) {
  mag_long <- tidyr::gather(mag, relhour, freq, `r-8`:r15.75, factor_key=TRUE)
  mag_long$relhour <- as.numeric(gsub("r", "", mag_long$relhour))
  best_mse <- Inf
  best_parabola_start_ix <- 0
  best_parabola_end_ix <- 0
  best_ideal_vector <- rep(NA, 96)
  for(parabola_start_ix in 1:94) {
    for(parabola_end_ix in (parabola_start_ix+2):96) {
      parabola_xs <- seq(-8,15.75, 0.25)[parabola_start_ix:parabola_end_ix]
      fit2 <- stats::lm(freq~poly(relhour,2,raw=TRUE), data =
                  mag_long[which(mag_long$relhour %in% parabola_xs),]  )
      quadratic <- fit2$coefficient[3]*(  parabola_xs  )^2 +
        fit2$coefficient[2]*(parabola_xs) +
        fit2$coefficient[1]
      nightFreq <- sum(quadratic)
      dayFreqPerHour <- (1-nightFreq)/(96-length(quadratic))
      ideal_vector <- c(rep(dayFreqPerHour,parabola_start_ix-1), quadratic, rep(dayFreqPerHour,96-parabola_end_ix)  )
      my_mse <- sum((matrix(rep(ideal_vector, nrow(mag)), nrow=nrow(mag), byrow = T) - mag[,-c(1,2)])^2)
      if(my_mse < best_mse) {
        best_mse <- my_mse
        best_parabola_start_ix <- parabola_start_ix
        best_parabola_end_ix <- parabola_end_ix
        best_ideal_vector <- ideal_vector
      }
    }
  }
  df.ideal_vector <- data.frame(relhour=seq(-8,15.75, 0.25), idealFreq=best_ideal_vector)
  if(write) {

    utils::write.table(df.ideal_vector, sep="\t", quote = F, row.names = F,
                file=paste("data/processed/ideal_posting_distribution/", out_prefix,"_ideal_posting_distribution.tsv", sep=""))}
  return(df.ideal_vector)
}



#' Plots unified model vs observed posting frequencies
#'
#' @param obsFreq data frame with columns relhour and obsFreq, which gives Reddit posting frequency in observed data
#' @param idealFreq data frame with columns relhour and idealFreq, which gives Reddit posting frequency in unified idealized model

#'
#' @return plotting function
#' @export
#'
#' @examples
#' rh <- seq(-8,15.75,0.25)
#' obsFreq <- data.frame(relhour=rh, obsFreq=(rh/15)^2+runif(96)/10)
#' idealFreq <- data.frame(relhour=rh, idealFreq=(rh/15)^2)
#' idealXreal_compare(obsFreq, idealFreq)
idealXreal_compare <- function(obsFreq, idealFreq) {
  maf <- merge(obsFreq, idealFreq)
  print(stats::cor(maf$obsFreq, maf$idealFreq))
  graphics::plot(maf$relhour, maf$obsFreq, xlab="hour (relative to bedtime)", ylab="posting frequency")
  graphics::lines(maf$relhour, maf$obsFreq)
  graphics::points(maf$relhour, maf$idealFreq, col="red")
  graphics::lines(maf$relhour, maf$idealFreq, col="red")
  # legend(-6, 0.01, legend = c("observed", "ideal"), col=c("black", "red"), pch=1, lwd = 1)

}

