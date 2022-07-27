`This R package trains, tests, and applies models for inferring Reddit user bedtimes in the manner of https://psyarxiv.com/9mpbw/

### Installation:
To install, first ensure devtools is installed  
install.packages("devtools")  
library(devtools)  

Then install the BEDDiT package  
install_github("WillMeyerson/BEDDiT")  
library(BEDDiT)  

### Use:
Most users will only be interested in two functions: bin_beddit() and apply_beddit()

bin_beddit() is a pre-processing helper script that converts raw tab-delimited Reddit data into the "circadian fingerprint" format expected of core pipeline functions. 

apply_beddit() is the main function for casual users of the R package. It predicts a set of Reddit users' bedtimes from their circardian fingerprints

There are an additional functions to make it convenient for users who want to train and test their own Reddit bedtime estimation models, or more deeply understand the mechanics of the package.

### Main workflow of BEDDiT paper:

options(stringsAsFactors = F)

if(!require(devtools)){
  install.packages("devtools")
}  
if(!require(BEDDiT)){
  install_github("WillMeyerson/BEDDiT")
}  

library(BEDDiT)  

core_bins <- bin_beddit(timestamps = core_timestamps, timezones = core_timezones)  

core_model <- train_beddit(timebins = core_bins, bedtimes = core_reportedbedtimes) 


core_test <- test_beddit(timebins=core_bins, 
                               model=core_model, 
                               bedtimes=core_reportedbedtimes)  

validation_test <- test_beddit(timebins=validation_binned_local, 
                               model=core_model, 
                               bedtimes=validation_reportedbedtimes)  

extended_estimated <- apply_beddit(bins=extended_binned_local, 
                                   model=core_model)  

hist(extended_estimated$inferred_bedtime)  

#### Tips
You can analyze a user's bedtime separately in different contexts such as across years or
days of the week by splitting the timestamps file as a pre-processing step.
For instance, 
library(lubridate)\
clock.mean_inferred_bedtime = function(DF) {
  return(clock.mean(DF$inferred_bedtime))
}\
core_timestamps$year = year(as.POSIXct(core_timestamps$created_utc, origin="1970-01-01"))\
core_timestamps$month = month(as.POSIXct(core_timestamps$created_utc, origin="1970-01-01"))\
core_timestamps$year_month = core_timestamps$year + (core_timestamps$month-1)/12\
cty = split(core_timestamps[which(core_timestamps$year >= 2015),], core_timestamps$year_month[which(core_timestamps$year >= 2015)])\
bins_year = vector("list", length=length(cty))\
for(i in 1:length(cty)) {
  print(i)\
  bins_year[[i]] = bin_beddit(timestamps = cty[[i]], timezones = core_timezones)
}\
bed_year = vector("list", length(cty))\
for(i in 1:length(bed_year)) {
  bed_year[[i]] = apply_beddit(bins_year[[i]], model = default_model)\
  bed_year[[i]]$year = names(cty)[i]\
}


df_bed_year = data.frame(year_month = as.numeric(names(cty)), bedtime=sapply(bed_year, clock.mean_inferred_bedtime))\

