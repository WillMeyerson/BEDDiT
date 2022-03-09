This R package trains, tests, and applies models for inferring Reddit user bedtimes in the manner of https://psyarxiv.com/9mpbw/

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
