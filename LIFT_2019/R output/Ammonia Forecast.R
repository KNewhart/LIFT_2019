start <- Sys.time()
##### Variables #####
setwd("..") # Move out of the "R output" folder
historian.export.path <- paste0(getwd(),"/Raw data/")
historian.import.path <- paste0(getwd(),"/R output/")
predictor.variable <- "NIA33391_AB3_AMMONIA"
predictor.tag <- "NIA33391_AB3_PREDICTED_AMMONIA.F_CV"
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
         "glmnet",
         "taRifx",
         "doParallel",
         "foreach"), packageLoad)

##### Compile data #####
# Import CSVs
files.to.import <- list.files(path=historian.export.path, "[.]CSV")
files.to.import <- c(files.to.import, list.files(path=historian.export.path, "[.]csv"))
if(length(files.to.import) == 0) print("No files to import. Check working directory.")
if("HistorianDataImport.csv" %in% files.to.import) files.to.import <- files.to.import[-which("HistorianDataImport.csv" == files.to.import)]
obj.list <- list()
for(file in files.to.import) {
  print(paste("Importing",file))
  imported.file <- read_csv(paste0(historian.export.path,"/",file), 
                            col_types = cols(TimeStamp = col_datetime(format = "%m/%d/%Y %H:%M:%S")))
  assign(strsplit(as.character(imported.file[1,1]),"[.]")[[1]][2], 
         xts(imported.file[,3], 
             order.by=data.frame(imported.file)[,2]))
  obj.list[[length(obj.list)+1]] <- strsplit(as.character(imported.file[1,1]),"[.]")[[1]][2]
}

# Merge all process data
print("Merging files...")
all.data <- xts()
for(obj in obj.list) {
  all.data <- cbind(all.data, get(obj))
  colnames(all.data)[ncol(all.data)] <- obj
}
save(all.data, file = paste0(historian.import.path,"data_save_01.RData"))
load(file = paste0(historian.import.path,"data_save_01.RData"))

##### FOR TESTING  #####
# all.data <- all.data[paste0("/",last(index(all.data))-5*60)] # First round of testing
# all.data <- all.data[paste0(index(all.data)[1]+5*60,"/")]



five.min.timestamps <- seq(as.numeric(index(all.data)[1]), as.numeric(last(index(all.data))), 5*60)
five.min.timestamps <- round(as.POSIXct(five.min.timestamps, origin="1970-01-01"), "mins")
all.data <- na.locf(all.data)
all.timestamps <- round(index(all.data),"mins")

registerDoParallel(detectCores())
if("data_save_02.RData" %in% list.files(path=historian.import.path)) {
  load(paste0(historian.import.path,"data_save_02.RData"))
  # start.time <- all.timestamps[1]
  # end.time <- last(all.timestamps)
  merge.data <- all.data[paste0(last(index(five.min.data)),"/")]
  grab.timestamps <- which(five.min.timestamps %in% round(index(merge.data), "mins"))
  
  five.merge.data <- foreach(i=grab.timestamps,
                              .combine=rbind,
                              .packages = "xts") %dopar% {
                              j <- which(round(index(merge.data), "mins") == five.min.timestamps[i])[1]
                             
                             if((is.na(j)) || (length(j)==0)) l <- 1
                             while((is.na(j)) || (length(j)==0)) {
                               j <- last(which(round(index(merge.data), "mins") == (five.min.timestamps[i]-l*60)))
                               l <- l+1
                             }
                             
                             # if(!exists("five.min.data")) {
                             #   five.min.data <- all.data[j,]
                             # } else {
                             #   five.min.data <- rbind(five.min.data, all.data[j,])
                             # }
                             return(merge.data[j,])
                           }
  
  five.min.data <- rbind(five.min.data[which(round(index(five.min.data), "mins") == five.min.timestamps[1]):nrow(five.min.data),],
                         five.merge.data)
  save(five.min.data, file = paste0(historian.import.path,"data_save_02.RData"))
} else {
  five.min.data <- foreach(i=1:length(five.min.timestamps),
                           .combine=rbind,
                           .packages = "xts") %dopar% {
    j <- which(five.min.timestamps[i] == all.timestamps)[1]
    
    if((is.na(j)) || (length(j)==0)) l <- 1
    while((is.na(j)) || (length(j)==0)) {
      j <- last(which((five.min.timestamps[i]-l*60) == all.timestamps))
      l <- l+1
    }
    
    # if(!exists("five.min.data")) {
    #   five.min.data <- all.data[j,]
    # } else {
    #   five.min.data <- rbind(five.min.data, all.data[j,])
    # }
    return(all.data[j,])
  }
  save(five.min.data, file = paste0(historian.import.path,"data_save_02.RData"))
}

load(file=paste0(historian.import.path,"data_save_02.RData"))


# Remove nitrite?
all.data <- all.data[,-which(colnames(all.data) == "NIR33391A_AB3_Z3_NITRITE")]
all.data <- all.data[,-which(colnames(all.data) == "NP4232_CA")]

# Clean up timestamps
row.index <- index(five.min.data)[!is.na(five.min.data[,predictor.variable])]
five.min.data <- xts(na.omit(five.min.data))[row.index]
if(length(as.numeric(which(apply(five.min.data,2,function(x) anyNA(scale(x)))))) > 0) five.min.data <- five.min.data[,-as.numeric(which(apply(five.min.data,2,function(x) anyNA(scale(x)))))]


##### Forecast Ammonia #####
# Convert timestamps to runtime and project onto a unit circle 
time.stamps <- difftime(index(five.min.data), index(five.min.data)[1], units = "mins")
time.stamps <- as.numeric(time.stamps) %% 1440 # Cycles of 1 day are constructed (1440 min/day)
time.stamps <- (time.stamps*360/1440)*pi/180 # Cycles of minutes are converted to radians

# Create training and testing datasets
print("Dividing training and testing data...")
rows <- which(round(difftime(index(five.min.data), index(five.min.data)[1], units = "mins"),0) == forecast.horizon)
rows <- rows[1]
predictor.col <- which(colnames(five.min.data)==predictor.variable)
if(length(predictor.col) == 0) {
  print(paste("Predictor",predictor.variable, "not found"))
  quit(save="no")
}

five.min.data.scaled <- apply(five.min.data, 2, scale)
five.min.data.scaled <- xts(five.min.data.scaled, order.by = index(five.min.data))

training.data <- cbind(data.frame(five.min.data.scaled[1:(nrow(five.min.data.scaled)-rows+1),]), data.frame(five.min.data.scaled[rows:nrow(five.min.data.scaled),predictor.col]))
colnames(training.data)[ncol(training.data)] <- paste("Forecasted",predictor.variable)
training.data <- cbind(time.stamps[1:nrow(training.data)], 
                           training.data,
                           cos(time.stamps[1:nrow(training.data)]),
                           sin(time.stamps[1:nrow(training.data)]),
                           cos(2*time.stamps[1:nrow(training.data)]),
                           sin(2*time.stamps[1:nrow(training.data)]))
colnames(training.data)[(ncol(training.data)-3):ncol(training.data)] <- c("COS", "SIN", "COS^2", "SIN^2")

testing.data <- five.min.data.scaled[(nrow(training.data)+1):nrow(five.min.data.scaled),]
testing.data <- cbind(time.stamps[(nrow(training.data)+1):nrow(five.min.data.scaled)], 
                          testing.data,
                          cos(time.stamps[(nrow(training.data)+1):nrow(five.min.data.scaled)]),
                          sin(time.stamps[(nrow(training.data)+1):nrow(five.min.data.scaled)]),
                          cos(2*time.stamps[(nrow(training.data)+1):nrow(five.min.data.scaled)]),
                          sin(2*time.stamps[(nrow(training.data)+1):nrow(five.min.data.scaled)]))
colnames(testing.data)[(ncol(testing.data)-3):ncol(testing.data)] <- c("COS", "SIN", "COS^2", "SIN^2")



# Set x and y model inputs
print("Fitting model...")
predictor.col <- which(colnames(training.data) == paste("Forecasted",predictor.variable))
yy <- as.matrix(training.data[,predictor.col])
xx <- as.matrix(training.data[,-predictor.col])

# Ridge regression
mod.ridge <- cv.glmnet(xx,yy,alpha=0)

# Define weights
w3 <- 1/abs(matrix(coef(mod.ridge, s=mod.ridge$lambda.1se)[, 1][-1]))^1

# Adaptive Lasso
mod.adaptive <- cv.glmnet(xx, yy,  alpha=1, penalty.factor=w3)

# Calculate error
s <- as.numeric(mod.adaptive$lambda.1se)
predictions <- predict(mod.adaptive,newx=xx, s=s)
SSE <- mean((yy-predictions)^2)
SST <- mean((yy-mean(yy))^2)
Rsqu <- 1-SSE/SST; print(paste("R-squared",Rsqu))

# Forecast
s <- as.numeric(mod.adaptive$lambda.1se)
xx.f <- as.matrix(testing.data)
forecast.data <- predict(mod.adaptive,newx=xx.f, s=s)
forecast.data <- xts(forecast.data, order.by = as.POSIXct(dimnames(forecast.data)[[1]]))
colnames(forecast.data)[ncol(forecast.data)] <- paste("Forecasted",predictor.variable)



##### Write data #####
final.data <- forecast.data
write.data <- data.frame(rep(predictor.tag, nrow(final.data)))
write.data <- cbind(write.data, as.character(index(final.data)))
write.data <- cbind(write.data, data.frame(final.data))
colnames(write.data) <- c("Tagname","TimeStamp","Value")
write.csv(write.data, file=paste0(historian.import.path,"HistorianDataImport.csv"),
          row.names = FALSE)

# Save results
model.fit <- data.frame("Time" = as.character(index(final.data)),"SSE" = SSE, "SST" = SST, "Rsqu" = Rsqu, "Value" = as.numeric(final.data), stringsAsFactors = FALSE)
if(!("Model_Results.csv" %in% list.files(path=historian.import.path))) {
  write.csv(model.fit, file=paste0(historian.import.path,"Model_Results.csv"), row.names = FALSE)
} else {
  all.model.fit <- read.csv(file=paste0(historian.import.path,"Model_Results.csv"))
  all.model.fit <- taRifx::remove.factors(all.model.fit)
  all.model.fit <- rbind(data.frame(all.model.fit, stringsAsFactors = FALSE), model.fit)
  write.csv(all.model.fit, file=paste0(historian.import.path,"Model_Results.csv"), row.names = FALSE)
}
end <- Sys.time()
end-start