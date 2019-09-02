###########################################
##Forecasting Preliminaries
##
###########################################

####################
##Load libraries
##Load data
####################

library(xts)
library(glmnet)

obj.list <- c("ab3_do", 
              "ab3_3.5",
              "ab3_4.0",
              "ab3_4.0_300")

load("data/ab3_objects.RData")

####################
##Add missing timestamps
##if needed
####################

for(x in obj.list) {
  time.intervals <- sapply(2:nrow(get(x)), function(i) difftime(index(get(x))[i], index(get(x))[i-1], units="mins"))
  if(length(which(time.intervals != 5)) > 0) {
    for(i in 1:length(which(time.intervals != 5))){
      start.time <- index(get(x))[which(time.intervals != 5)[i]]
      end.time <- index(get(x))[which(time.intervals != 5)[i]+1]
      new.times <- seq(start.time, end.time, by=5*60)
      new.x <- merge(get(x), new.times[2:(length(new.times)-1)], fill=NA)
      assign(x,new.x)
    }
  }
}

####################
##Checking size of 
##datasets
####################
dim(ab3_do)			#25920
dim(ab3_4.0)			#3889
dim(ab3_3.5)			#3169
dim(ab3_4.0_300)	#2137

####################
##Creating new variables
##for double Fourier pair
##diurnal model
####################
## Convert timestamps to runtime and project onto a unit circle 
## Based on 5 minute increments over the course of the day
for(x in obj.list) {
  time.stamps <- difftime(index(get(x)), index(get(x))[1], units = "mins")
  time.stamps <- as.numeric(time.stamps) %% 1440 # Cycles of 1 day are constructed (1440 min/day)
  time.stamps <- (time.stamps*360/1440)*pi/180 # Cycles of minutes are converted to radians
  assign(x, cbind(time.stamps,get(x)))
}

# # ## Convert timestamps to runtime and project onto a unit circle 
# ## Based on hourly increments over the course of the day
# for(x in obj.list) {
  # time.stamps <- floor(difftime(index(get(x)), index(get(x))[1], units = "hours"))
  # time.stamps <- as.numeric(time.stamps) %% 24 # Cycles of 1 day are constructed (24 hours/day)
  # time.stamps <- (time.stamps*360/24)*pi/180 # Cycles of hours are converted to radians
  # assign(x, cbind(time.stamps,get(x)))
# }


## Add diurnal model predictors to dataset
for(x in obj.list) {
  cos.x <- cos(get(x)[,1])
  names(cos.x) <- "cos.x"
  sin.x <- sin(get(x)[,1])
  names(sin.x) <- "sin.x"
  cos.2x <- cos(2*get(x)[,1])
  names(cos.2x) <- "cos.2x"
  sin.2x <- sin(2*get(x)[,1])
  names(sin.2x) <- "sin.2x"
  assign(x, cbind(get(x), cos.x, sin.x, cos.2x, sin.2x), envir = .GlobalEnv)
}


#Subsetting each dataset to just the most recent part needed for training and testing
for(x in obj.list) {
	nn=dim(get(x))[1]
	assign(x,get(x)[(nn-total.obs+1):(nn),])
}






