rm(list=ls())

# Load required libraries
library(readr)
library(xts)
library(glmnet)

### USE FOR TESTING ###
test.1.var <- function(all.data){
  all.data <- cbind(all.data, xts(seq(1,nrow(all.data)), order.by = index(all.data)))
  colnames(all.data)[ncol(all.data)] <- predictor.variable
  return(all.data)
}

##### Variables #####
testing1var <- TRUE
historian.export.path <- "data/Historian Export/"
historian.import.path <- "data/Historian Import/"
predictor.variable <- "Z7 Ammonia"
forecast.horizon <- 50

##### Compile data #####

# Import CSVs
files.to.import <- list.files(path=historian.export.path, ".CSV")
file <- files.to.import[1]
obj.list <- list()
for(file in files.to.import) {
  imported.file <- read_csv(paste0(historian.export.path,file), 
                            col_types = cols(TimeStamp = col_datetime(format = "%m/%d/%Y %H:%M:%S")))
  assign(strsplit(as.character(imported.file[1,1]),"[.]")[[1]][2], 
         xts(imported.file[,3], 
             order.by=data.frame(imported.file)[,2]))
  obj.list[[length(obj.list)+1]] <- strsplit(as.character(imported.file[1,1]),"[.]")[[1]][2]
}

# Merge all process data
all.data <- xts()
for(obj in obj.list) {
  all.data <- cbind(all.data, get(obj))
  colnames(all.data)[ncol(all.data)] <- obj
}

### RUN TESTING CODE ABOVE NOW IF ONLY 1 VARIABLE ###
if(testing1var) all.data <- test.1.var(all.data)

##### Forecast Ammonia #####
# Convert timestamps to runtime and project onto a unit circle 
time.stamps <- difftime(index(all.data), index(all.data)[1], units = "mins")
time.stamps <- as.numeric(time.stamps) %% 1440 # Cycles of 1 day are constructed (1440 min/day)
time.stamps <- (time.stamps*360/1440)*pi/180 # Cycles of minutes are converted to radians




# Create training and testing datasets
rows <- which(round(difftime(index(all.data), index(all.data)[1], units = "mins"),1) == forecast.horizon)
predictor.col <- which(colnames(all.data)==predictor.variable)
training.data <- cbind(data.frame(all.data[1:(nrow(all.data)-rows+1),]), data.frame(all.data[rows:nrow(all.data),predictor.col]))
training.data <- xts(training.data, order.by=as.POSIXct(rownames(training.data)))
colnames(training.data)[ncol(training.data)] <- paste("Forecasted",predictor.variable)
training.data <- xts(cbind(time.stamps[1:nrow(training.data)], 
                           apply(training.data, 2, scale),
                           cos(time.stamps[1:nrow(training.data)]),
                           sin(time.stamps[1:nrow(training.data)]),
                           cos(2*time.stamps[1:nrow(training.data)]),
                           sin(2*time.stamps[1:nrow(training.data)])), order.by = index(training.data))

testing.data <- all.data[(nrow(training.data)+1):nrow(all.data),]
testing.data <- xts(cbind(time.stamps[(nrow(training.data)+1):nrow(all.data)], 
                           apply(testing.data, 2, scale),
                           cos(time.stamps[(nrow(training.data)+1):nrow(all.data)]),
                           sin(time.stamps[(nrow(training.data)+1):nrow(all.data)]),
                           cos(2*time.stamps[(nrow(training.data)+1):nrow(all.data)]),
                           sin(2*time.stamps[(nrow(training.data)+1):nrow(all.data)])), order.by = index(testing.data))

# Set x and y model inputs
predictor.col <- which(colnames(training.data) == paste("Forecasted",predictor.variable))
yy <- as.matrix(training.data[,predictor.col])
xx <- as.matrix(training.data[,-predictor.col])

# Ridge regression
mod.ridge <- cv.glmnet(xx,yy,alpha=0)

# Define weights
w3 <- 1/abs(matrix(coef(mod.ridge, s=mod.ridge$lambda.min)[, 1][-1]))^1

# Adaptive Lasso
mod.adaptive <- cv.glmnet(xx, yy,  alpha=1, penalty.factor=w3)

# Set x for model forecast
xx.f <- as.matrix(testing.data)

# Forecast
s <- as.numeric(mod.adaptive$lambda.1se)
forecast.data <- predict(mod.adaptive,newx=xx.f, s=s)
forecast.data <- xts(forecast.data, order.by = as.POSIXct(dimnames(forecast.data)[[1]]))
colnames(forecast.data)[ncol(forecast.data)] <- paste("Forecasted",predictor.variable)


##### Write data #####
final.data <- forecast.data[nrow(forecast.data),]
write.data <- data.frame(rep(paste("Forecasted",predictor.variable), nrow(final.data)))
write.data <- cbind(write.data, as.character(index(final.data)))
write.data <- cbind(write.data, data.frame(final.data))
colnames(write.data) <- c("Tagname","TimeStamp","Value")
write.csv(write.data, file=paste0(historian.import.path,write.data[1,1],".csv"),
          row.names = FALSE)
