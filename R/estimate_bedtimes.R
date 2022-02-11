
#' helper function to make which.max unbiased and single-valued
#'
#' @param vec vector
#'
#' @return index of maximum element of vector; if multiple, select one at random
#' @export
#'
#' @examples
#' x <- rep(1:5,3)
#' which.maximum(x)
which.maximum <- function(vec) {
  maxima <- which(vec == max(vec))
  if(length(maxima) > 1){
    maxima <- sample(maxima, 1)
  }
  return(maxima)
}


#' Estimate bedtimes of users from circadian fingerpints and model
#'
#' @param bins circadian fingerpints of users
#' @param model a unified idealized model
#'
#' @return inferred bedtimes of users
#' @export
#'
#' @examples
#' my_timestamps <- data.frame(author=c(1,1,2,2), created_utc=1.6*10^9+c(0,1000,2000,3000))
#' my_bedtimes <- data.frame(author=c(1,1,2,2), nightbed=c(-1,-1))
#' my_timezones <- create_dummy_timezones(my_timestamps)
#' my_bins <- time_of_day_binner(assign_time_of_day(my_timestamps, my_timezones))
#' rh <- seq(-8,15.75,0.25)
#' my_model <- data.frame(relhour=rh, idealFreq=(rh/15)^2)
#' estimate_bedtimes(my_bins, my_model)
estimate_bedtimes <- function(bins, model) {
  span <- seq(-12,11.75,0.25)
  cor_with_ideal <- matrix(NA, nrow=nrow(bins), ncol=length(span))
  j <- 1
  for(guessed_bedtime in span ) {
    my_rel <- relativizer(bins_matrix = bins[,-1] , bedtime = guessed_bedtime)
    cor_with_ideal[,j] <- as.numeric(stats::cor(t(my_rel), model$idealFreq))
    j <- j + 1
  }
  est_bed <- data.frame(author=bins[,1], inferred_bedtime=span[apply(cor_with_ideal,1,which.maximum)], cor=apply(cor_with_ideal,1,max))
  # fwrite(est_bed, paste("data/processed/inferred_bedtimes/", input_prefix, "_from_", model_prefix, "_inferred_bedtimes.tsv" , sep=""), sep="\t")
  return(est_bed)
}





#' compare inferred to reported bedtimes
#'
#' @param reported_bedtimes a data frame where each row contains a username and a reported bedtime
#' @param inferred_bedtimes a data frame where each row contains a username and an inferred bedtime
#' @param threshhold the minimum correlation between circadian fingerprint and best-matching ideal model, deault 0.25
#'
#' @return a data frame that includes reported_bedtimes and inferred_bedtimes for each user
#' @export
#'
#' @examples
#'
#' my_bedtimes <- data.frame(author=c(1,2), nightbed=c(-1, 1))
#' inferred_bedtimes <- data.frame(author=c(1,2), inferred_bedtime=c(-1.5, 0.75), cor=c(0.5,0.5))
#' compared_inferred_to_reported_bedtimes(my_bedtimes, inferred_bedtimes)
compared_inferred_to_reported_bedtimes <- function(reported_bedtimes,
                                                  inferred_bedtimes,
                                                  threshhold = 0.25) {
  combination <- merge(reported_bedtimes, inferred_bedtimes)
  print(sum(combination$cor >= threshhold))
  print(mean(combination$cor >= threshhold))

  ### for plotting purposes, if reported_bedtime is positive, then recast inferred bedtime as positive
  ### for plotting purposes, if reported_bedtime is negative, then recast inferred bedtime as negative

  filtered <- combination[which(combination$cor >= threshhold),]
  filtered$inferred_bedtime[which(filtered$nightbed < -6 & filtered$inferred_bedtime >= 6)] <- filtered$inferred_bedtime[which(filtered$nightbed < -6 & filtered$inferred_bedtime >= 6)] - 24
  filtered$inferred_bedtime[which(filtered$nightbed >= 6 & filtered$inferred_bedtime < -6)] <- filtered$inferred_bedtime[which(filtered$nightbed >= 6 & filtered$inferred_bedtime < -6)] + 24

  graphics::plot(jitter(filtered$nightbed),
       jitter(filtered$inferred_bedtime), pch=16,
       xlab="reported bedtime", ylab="inferred bedtime")

  alpha <- combination$nightbed[which(combination$cor >= threshhold & combination$inferred_bedtime >= -6 & combination$inferred_bedtime <= 6)]
  beta <- combination$inferred_bedtime[which(combination$cor >= threshhold & combination$inferred_bedtime >= -6 & combination$inferred_bedtime <= 6)]
  print(stats::cor.test(alpha, beta, method="spearman"))
  print(length(alpha))

  return(combination)

}


