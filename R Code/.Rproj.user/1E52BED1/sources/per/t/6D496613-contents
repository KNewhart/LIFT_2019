rm(list=ls())

##### Variables #####
historian.export.path <- "../Historian Data Export CSV/"
historian.import.path <- "../Historian Data Import CSV/"
predictor.variable <- "NIF11010_EFFL_FLOW"
forecast.horizon <- 50 # In minutes


##### Load libraries #####
# Function to install and load libraries
packageLoad <- function(packName){ #packName - package name as a character string, e.g. "quantmod"
  if(!require(packName,character.only = TRUE)){ #If the package is not available, install it
    install.packages(packName,dependencies=TRUE,repos="http://cran.r-project.org")
  }
  library(packName, character.only = TRUE) # load package
}

# Load required libraries
sapply(c("readr",
         "xts",
         "glmnet"), packageLoad)

##### Compile data #####
# Import CSVs
files.to.import <- list.files(path=historian.export.path, "[.]CSV")
files.to.import <- c(files.to.import, files.to.import <- list.files(path=historian.export.path, "[.]csv"))
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


##### Forecast Ammonia #####
# Convert timestamps to runtime and project onto a unit circle 
time.stamps <- difftime(index(all.data), index(all.data)[1], units = "mins")
time.stamps <- as.numeric(time.stamps) %% 1440 # Cycles of 1 day are constructed (1440 min/day)
time.stamps <- (time.stamps*360/1440)*pi/180 # Cycles of minutes are converted to radians

# Create training and testing datasets
rows <- which(round(difftime(index(all.data), index(all.data)[1], units = "mins"),1) == forecast.horizon)
predictor.col <- which(colnames(all.data)==predictor.variable)
if(length(predictor.col) == 0) quit(save="no")
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

# Calculate error
s <- as.numeric(mod.adaptive$lambda.1se)
predictions <- predict(mod.adaptive,newx=xx, s=s)
SSE <- mean((yy-predictions)^2)
SST <- mean((yy-mean(yy))^2)
Rsqu <- 1-SSE/SST; print(Rsqu)

# Forecast
s <- as.numeric(mod.adaptive$lambda.1se)
xx.f <- as.matrix(testing.data)
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
