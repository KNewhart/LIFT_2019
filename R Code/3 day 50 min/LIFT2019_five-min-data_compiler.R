# Import CSVs
library(readr)
all.files<- list.files(path="C:\\Users\\Kate Newhart\\Desktop\\LIFT_2019\\Raw data", "[.]CSV")
all.variables <- unique(sapply(all.files, function(x) substr(x,1,nchar(x)-nchar("20190927164810_20190930164810")-5)))
obj.list <- list()
for(variable in all.variables) {
  files.to.import <- all.files[sapply(all.files, function(x) substr(x,1,nchar(x)-nchar("20190927164810_20190930164810")-5)) %in% variable]
  merged.file <- xts()
  for(file in files.to.import) {
    imported.file <- read_csv(paste0("C:\\Users\\Kate Newhart\\Desktop\\LIFT_2019\\Raw data\\",file), 
                              col_types = cols(TimeStamp = col_datetime(format = "%m/%d/%Y %H:%M:%S")))
    time <- as.POSIXlt(data.frame(imported.file)[,2])
    attr(time, "tzone") <- Sys.timezone()
    if(length(merged.file) > 0) merged.file <- rbind(merged.file, xts(data.frame(imported.file)[,3], 
                                                                      order.by=time))
    if(length(merged.file) == 0) merged.file <- xts(data.frame(imported.file)[,3], 
                                                    order.by=time)
  }
  assign(strsplit(as.character(imported.file[1,1]),"[.]")[[1]][2], merged.file)
  obj.list[[length(obj.list)+1]] <- strsplit(as.character(imported.file[1,1]),"[.]")[[1]][2]
}

# Merge all process data
print("Merging files...")
all.data <- xts()
for(obj in obj.list) {
  all.data <- cbind(all.data, get(obj))
  colnames(all.data)[ncol(all.data)] <- obj
}


# Unprocessed data
all.data <- na.locf(all.data)
all.data <- na.omit(all.data)

# Set timestamps
data.interval <- 5*60 # In minutes
five.min.timestamps <- seq(as.numeric(index(all.data)[1]), as.numeric(last(index(all.data))), data.interval)
five.min.timestamps <- round(as.POSIXct(five.min.timestamps, origin="1970-01-01"), "mins")

# Initialize parallel processing
library(doParallel)
library(foreach)
registerDoParallel(detectCores())

# Reduce to 5 min
all.timestamps <- round(index(all.data),"mins")
five.min.data <- foreach(i=1:length(five.min.timestamps),
                         .combine=rbind,
                         .packages = "xts") %dopar% {
                           j <- which(five.min.timestamps[i] == all.timestamps)[1]
                           
                           if((is.na(j)) || (length(j)==0)) l <- 1
                           while((is.na(j)) || (length(j)==0)) {
                             j <- last(which((five.min.timestamps[i]-l*60) == all.timestamps))
                             l <- l+1
                           }
                           
                           return(xts(all.data[j,], order.by = five.min.timestamps[i]))
                         }
save(five.min.data, file="five-min-data.RData")







library(xts)
load(file="five-min-data.RData")

predictor.variable <- "NIA33391_AB3_AMMONIA"
# Remove NAs
row.index <- index(five.min.data)[!is.na(five.min.data[,predictor.variable])]
five.min.data <- xts(na.omit(five.min.data))[row.index]

# If NA's appear during scaling, it's most likely constant. 
# Therefore, remove process variable
print(paste("Removing...", colnames(five.min.data[as.numeric(which(apply(five.min.data,2,function(x) anyNA(scale(x)))))])))
if(length(as.numeric(which(apply(five.min.data,2,function(x) anyNA(scale(x)))))) > 0) five.min.data <- five.min.data[,-as.numeric(which(apply(five.min.data,2,function(x) anyNA(scale(x)))))]

##### Forecast Ammonia #####
print("Dividing training and testing data...")
forecast.horizon <- 50 # In minutes
rows <- which(round(difftime(index(five.min.data), index(five.min.data)[1], units = "mins"),0) == forecast.horizon)
rows <- rows[1]
predictor.col <- which(colnames(five.min.data)==predictor.variable)
predictor.tag <- "NIA33391_AB3_PREDICTED_AMMONIA.F_CV"
if(length(predictor.col) == 0) {
  print(paste("Predictor",predictor.variable, "not found"))
  quit(save="no")
}
all.data <- five.min.data
predictor.col <- which(colnames(all.data)==predictor.variable)

# Test different training windows

# Initialize parallel processing
library(doParallel)
library(foreach)
registerDoParallel(detectCores())


all.training.windows <- list()
training.windows <- seq(1*24*60/5,10*24*60/5,by=24*60/5)
for(window in training.windows) {
  rolling.window.results <- foreach(i=1:(nrow(all.data)-window-forecast.horizon/5-1),
                                    .combine=rbind,
                                     .packages = "xts") %dopar% {
                                       
     
     # Start loop
     five.min.data <- all.data[i:(window-1+i+forecast.horizon/5),]
     predictor.col <- which(colnames(five.min.data)==predictor.variable)
     
     # Convert timestamps to runtime and project onto a unit circle 
     time.stamps <- difftime(index(five.min.data), index(five.min.data)[1], units = "mins")
     time.stamps <- as.numeric(time.stamps) %% 1440 # Cycles of 1 day are constructed (1440 min/day)
     time.stamps <- (time.stamps*360/1440)*pi/180 # Cycles of minutes are converted to radians
     
     # Save means and standard deviations
     training.mean <- mean(unlist(five.min.data[,predictor.variable]))
     training.sd <- sd(unlist(five.min.data[,predictor.variable]))
     
     # Scale data
     five.min.data.scaled <- apply(five.min.data, 2, scale)
     five.min.data.scaled <- xts(five.min.data.scaled, order.by = index(five.min.data))
     
     # Subset training data
     training.data <- cbind(data.frame(five.min.data.scaled[1:(nrow(five.min.data.scaled)-rows+1),]), data.frame(five.min.data.scaled[(rows:nrow(five.min.data.scaled)),predictor.col]))
     colnames(training.data)[ncol(training.data)] <- paste("Forecasted",predictor.variable)
     
     training.data <- cbind(time.stamps[1:nrow(training.data)], 
                            training.data,
                            cos(time.stamps[1:nrow(training.data)]),
                            sin(time.stamps[1:nrow(training.data)]),
                            cos(2*time.stamps[1:nrow(training.data)]),
                            sin(2*time.stamps[1:nrow(training.data)]),
                            cos(3*time.stamps[1:nrow(training.data)]),
                            sin(3*time.stamps[1:nrow(training.data)]),
                            cos(4*time.stamps[1:nrow(training.data)]),
                            sin(4*time.stamps[1:nrow(training.data)]),
                            cos(5*time.stamps[1:nrow(training.data)]),
                            sin(5*time.stamps[1:nrow(training.data)]))
     colnames(training.data)[(ncol(training.data)-9):ncol(training.data)] <- c("COS", "SIN", "COS^2", "SIN^2",
                                                                               "COS^3", "SIN^3","COS^4", "SIN^4",
                                                                               "COS^5", "SIN^5")
     
     # Subset testing data
     testing.data <- five.min.data.scaled[(nrow(training.data)+1):nrow(five.min.data.scaled),]
     testing.data <- cbind(time.stamps[(nrow(training.data)+1):nrow(five.min.data.scaled)], 
                           testing.data,
                           cos(time.stamps[(nrow(training.data)+1):nrow(five.min.data.scaled)]),
                           sin(time.stamps[(nrow(training.data)+1):nrow(five.min.data.scaled)]),
                           cos(2*time.stamps[(nrow(training.data)+1):nrow(five.min.data.scaled)]),
                           sin(2*time.stamps[(nrow(training.data)+1):nrow(five.min.data.scaled)]),
                           cos(3*time.stamps[(nrow(training.data)+1):nrow(five.min.data.scaled)]),
                           sin(3*time.stamps[(nrow(training.data)+1):nrow(five.min.data.scaled)]),
                           cos(4*time.stamps[(nrow(training.data)+1):nrow(five.min.data.scaled)]),
                           sin(4*time.stamps[(nrow(training.data)+1):nrow(five.min.data.scaled)]),
                           cos(5*time.stamps[(nrow(training.data)+1):nrow(five.min.data.scaled)]),
                           sin(5*time.stamps[(nrow(training.data)+1):nrow(five.min.data.scaled)]))
     colnames(testing.data)[(ncol(testing.data)-9):ncol(testing.data)] <- c("COS", "SIN", "COS^2", "SIN^2",
                                                                            "COS^3", "SIN^3","COS^4", "SIN^4",
                                                                            "COS^5", "SIN^5")
     
     
     
     # Set x and y model inputs
     predictor.col <- which(colnames(training.data) == paste("Forecasted",predictor.variable))
     yy <- as.matrix(training.data[,predictor.col])
     xx <- as.matrix(training.data[,-predictor.col])
     
     model.lm <- lm(yy~xx[,17]+xx[,18]+xx[,19]+xx[,20]+xx[,21]+xx[,22]+xx[,23]+xx[,24]+xx[,25]+xx[,26])
     # plot(yy)
     # points(predict.lm(model.lm, data.frame(xx[,17:26])),type="l")
     # text(x=0,y=max(yy), labels=round(summary(model.lm)$r.squared,3), pos=4)
     return(summary(model.lm)$r.squared)
                                     }
  all.training.windows[[length(all.training.windows)+1]] <- rolling.window.results
}
as.numeric(lapply(all.training.windows, mean)) # Shortest window is the best for diurnal fit



start <- Sys.time()
all.training.windows <- list()
training.windows <- seq(1*24*60/5,10*24*60/5,by=24*60/5)
for(window in training.windows) {
  rolling.window.results <- foreach(i=1:(nrow(all.data)-window-forecast.horizon/5-1),
                                    .combine=rbind,
                                    .packages = c("xts","glmnet")) %dopar% {
                                      
      
      # Start loop
      five.min.data <- all.data[i:(window-1+i+forecast.horizon/5),]
      predictor.col <- which(colnames(five.min.data)==predictor.variable)
      
      # Convert timestamps to runtime and project onto a unit circle 
      time.stamps <- difftime(index(five.min.data), index(five.min.data)[1], units = "mins")
      time.stamps <- as.numeric(time.stamps) %% 1440 # Cycles of 1 day are constructed (1440 min/day)
      time.stamps <- (time.stamps*360/1440)*pi/180 # Cycles of minutes are converted to radians
      
      # Save means and standard deviations
      training.mean <- mean(unlist(five.min.data[,predictor.variable]))
      training.sd <- sd(unlist(five.min.data[,predictor.variable]))
      
      # Scale data
      five.min.data.scaled <- apply(five.min.data, 2, scale)
      five.min.data.scaled <- xts(five.min.data.scaled, order.by = index(five.min.data))
      
      # Subset training data
      training.data <- cbind(data.frame(five.min.data.scaled[1:(nrow(five.min.data.scaled)-rows+1),]), data.frame(five.min.data.scaled[(rows:nrow(five.min.data.scaled)),predictor.col]))
      colnames(training.data)[ncol(training.data)] <- paste("Forecasted",predictor.variable)
      
      training.data <- cbind(time.stamps[1:nrow(training.data)], 
                             training.data,
                             cos(time.stamps[1:nrow(training.data)]),
                             sin(time.stamps[1:nrow(training.data)]),
                             cos(2*time.stamps[1:nrow(training.data)]),
                             sin(2*time.stamps[1:nrow(training.data)]),
                             cos(3*time.stamps[1:nrow(training.data)]),
                             sin(3*time.stamps[1:nrow(training.data)]),
                             cos(4*time.stamps[1:nrow(training.data)]),
                             sin(4*time.stamps[1:nrow(training.data)]),
                             cos(5*time.stamps[1:nrow(training.data)]),
                             sin(5*time.stamps[1:nrow(training.data)]))
      colnames(training.data)[(ncol(training.data)-9):ncol(training.data)] <- c("COS", "SIN", "COS^2", "SIN^2",
                                                                                "COS^3", "SIN^3","COS^4", "SIN^4",
                                                                                "COS^5", "SIN^5")
      
      # Subset testing data
      testing.data <- five.min.data.scaled[(nrow(training.data)+1):nrow(five.min.data.scaled),]
      testing.data <- cbind(time.stamps[(nrow(training.data)+1):nrow(five.min.data.scaled)], 
                            testing.data,
                            cos(time.stamps[(nrow(training.data)+1):nrow(five.min.data.scaled)]),
                            sin(time.stamps[(nrow(training.data)+1):nrow(five.min.data.scaled)]),
                            cos(2*time.stamps[(nrow(training.data)+1):nrow(five.min.data.scaled)]),
                            sin(2*time.stamps[(nrow(training.data)+1):nrow(five.min.data.scaled)]),
                            cos(3*time.stamps[(nrow(training.data)+1):nrow(five.min.data.scaled)]),
                            sin(3*time.stamps[(nrow(training.data)+1):nrow(five.min.data.scaled)]),
                            cos(4*time.stamps[(nrow(training.data)+1):nrow(five.min.data.scaled)]),
                            sin(4*time.stamps[(nrow(training.data)+1):nrow(five.min.data.scaled)]),
                            cos(5*time.stamps[(nrow(training.data)+1):nrow(five.min.data.scaled)]),
                            sin(5*time.stamps[(nrow(training.data)+1):nrow(five.min.data.scaled)]))
      colnames(testing.data)[(ncol(testing.data)-9):ncol(testing.data)] <- c("COS", "SIN", "COS^2", "SIN^2",
                                                                             "COS^3", "SIN^3","COS^4", "SIN^4",
                                                                             "COS^5", "SIN^5")
      
      
      
      # Set x and y model inputs
      predictor.col <- which(colnames(training.data) == paste("Forecasted",predictor.variable))
      yy <- as.matrix(training.data[,predictor.col])
      xx <- as.matrix(training.data[,-predictor.col])
      
      
      # Ridge regression
      mod.ridge <- cv.glmnet(xx,yy,alpha=0)
      
      # Define weights
      w3 <- abs(matrix(coef(mod.ridge, s=mod.ridge$lambda.min)[, 1][-1]))^1
      
      # Adaptive Lasso
      mod.adaptive <- cv.glmnet(xx, yy,  alpha=1, penalty.factor=w3)
      
      # Calculate error
      s <- as.numeric(mod.adaptive$lambda.min)
      predictions <- predict(mod.adaptive,newx=xx, s=s)
      SSE <- mean((yy-predictions)^2)
      SST <- mean((yy-mean(yy))^2)
      Rsqu <- 1-SSE/SST; 
      

      return(Rsqu)
                                    }
  all.training.windows[[length(all.training.windows)+1]] <- rolling.window.results
  print(paste(window/(24*60/5),"day window complete."))
}
end <- Sys.time()
print(end-start)
as.numeric(lapply(all.training.windows,mean))







#### Train and test a diurnal-linear model
start <- Sys.time()
all.training.windows <- list()
training.windows <- seq(1*24*60/5,10*24*60/5,by=24*60/5)
training.windows <- training.windows[1]
for(window in training.windows) {
  rolling.window.results <- foreach(i=1:(nrow(all.data)-window-2*forecast.horizon/5-1),
                                    .combine=rbind,
                                    .packages = c("xts","glmnet")) %dopar% {
                                      
                                      # Start loop
                                      five.min.data <- all.data[i:(window-1+i+forecast.horizon/5),]
                                      predictor.col <- which(colnames(five.min.data)==predictor.variable)
                                      
                                      # Convert timestamps to runtime and project onto a unit circle 
                                      time.stamps <- difftime(index(five.min.data), index(five.min.data)[1], units = "mins")
                                      time.stamps <- as.numeric(time.stamps) %% 1440 # Cycles of 1 day are constructed (1440 min/day)
                                      time.stamps <- (time.stamps*360/1440)*pi/180 # Cycles of minutes are converted to radians
                                      
                                      # Save means and standard deviations
                                      training.mean <- mean(unlist(five.min.data[1:(nrow(five.min.data)-forecast.horizon/5),predictor.variable]))
                                      training.sd <- sd(unlist(five.min.data[1:(nrow(five.min.data)-forecast.horizon/5),predictor.variable]))
                                      
                                      # Scale data
                                      five.min.data.scaled <- apply(five.min.data, 2, scale)
                                      five.min.data.scaled <- xts(five.min.data.scaled, order.by = index(five.min.data))
                                      
                                      # Subset training data
                                      training.data <- cbind(data.frame(five.min.data.scaled[1:(nrow(five.min.data.scaled)-forecast.horizon/5),]), data.frame(five.min.data.scaled[((forecast.horizon/5+1):nrow(five.min.data.scaled)),predictor.col]))
                                      colnames(training.data)[ncol(training.data)] <- paste("Forecasted",predictor.variable)
                                      
                                      training.data <- cbind(time.stamps[1:nrow(training.data)], 
                                                             training.data,
                                                             cos(time.stamps[1:nrow(training.data)]),
                                                             sin(time.stamps[1:nrow(training.data)]),
                                                             cos(2*time.stamps[1:nrow(training.data)]),
                                                             sin(2*time.stamps[1:nrow(training.data)]),
                                                             cos(3*time.stamps[1:nrow(training.data)]),
                                                             sin(3*time.stamps[1:nrow(training.data)]),
                                                             cos(4*time.stamps[1:nrow(training.data)]),
                                                             sin(4*time.stamps[1:nrow(training.data)]),
                                                             cos(5*time.stamps[1:nrow(training.data)]),
                                                             sin(5*time.stamps[1:nrow(training.data)]))
                                      colnames(training.data)[(ncol(training.data)-9):ncol(training.data)] <- c("COS", "SIN", "COS^2", "SIN^2",
                                                                                                                "COS^3", "SIN^3","COS^4", "SIN^4",
                                                                                                                "COS^5", "SIN^5")
                                      
                                      # Subset testing data
                                      testing.data <- five.min.data.scaled[(nrow(training.data)+1):nrow(five.min.data.scaled),]
                                      testing.data <- cbind(time.stamps[(nrow(training.data)+1):nrow(five.min.data.scaled)], 
                                                            testing.data,
                                                            cos(time.stamps[(nrow(training.data)+1):nrow(five.min.data.scaled)]),
                                                            sin(time.stamps[(nrow(training.data)+1):nrow(five.min.data.scaled)]),
                                                            cos(2*time.stamps[(nrow(training.data)+1):nrow(five.min.data.scaled)]),
                                                            sin(2*time.stamps[(nrow(training.data)+1):nrow(five.min.data.scaled)]),
                                                            cos(3*time.stamps[(nrow(training.data)+1):nrow(five.min.data.scaled)]),
                                                            sin(3*time.stamps[(nrow(training.data)+1):nrow(five.min.data.scaled)]),
                                                            cos(4*time.stamps[(nrow(training.data)+1):nrow(five.min.data.scaled)]),
                                                            sin(4*time.stamps[(nrow(training.data)+1):nrow(five.min.data.scaled)]),
                                                            cos(5*time.stamps[(nrow(training.data)+1):nrow(five.min.data.scaled)]),
                                                            sin(5*time.stamps[(nrow(training.data)+1):nrow(five.min.data.scaled)]))
                                      colnames(testing.data)[(ncol(testing.data)-9):ncol(testing.data)] <- c("COS", "SIN", "COS^2", "SIN^2",
                                                                                                             "COS^3", "SIN^3","COS^4", "SIN^4",
                                                                                                             "COS^5", "SIN^5")
                                      
                                      
                                      
                                      # Set x and y model inputs
                                      predictor.col <- which(colnames(training.data) == paste("Forecasted",predictor.variable))
                                      yy <- as.matrix(training.data[,predictor.col])
                                      xx <- as.matrix(training.data[,-predictor.col])
                                      
                                      
                                      # Ridge regression
                                      mod.ridge <- cv.glmnet(xx,yy,alpha=0)
                                      
                                      # Define weights
                                      w3 <- abs(matrix(coef(mod.ridge, s=mod.ridge$lambda.min)[, 1][-1]))^1
                                      
                                      # Adaptive Lasso
                                      mod.adaptive <- cv.glmnet(xx, yy,  alpha=1, penalty.factor=w3)
                                      
                                      # Calculate error
                                      s <- as.numeric(mod.adaptive$lambda.min)
                                      predictions <- predict(mod.adaptive,newx=xx, s=s)
                                      SSE <- mean((yy-predictions)^2)
                                      SST <- mean((yy-mean(yy))^2)
                                      Rsqu <- 1-SSE/SST; 
                                      
                                      
                                      # Forecast
                                      xx.f <- as.matrix(testing.data)
                                      forecast.data <- predict(mod.adaptive,newx=xx.f, s=s)
                                      forecast.data <- xts(forecast.data, order.by = as.POSIXct(dimnames(forecast.data)[[1]]))
                                      colnames(forecast.data)[ncol(forecast.data)] <- paste("Forecasted",predictor.variable)
                                      
                                      ##### Write data #####
                                      final.data <- last(forecast.data*training.sd+training.mean)
                                      actual.data <- all.data[(window-1+i+2*forecast.horizon/5),which(colnames(five.min.data)==predictor.variable)]
                                      sensor.data <- all.data[(window-1+i+forecast.horizon/5),which(colnames(five.min.data)==predictor.variable)]
                                      model.fit <- data.frame("Time" = as.character(index(final.data)), "Training R2" = Rsqu, "Forecast" = as.numeric(final.data), "Actual" = as.numeric(actual.data), "Sensor" = as.numeric(sensor.data), stringsAsFactors = FALSE)
                                      model.fit <- xts(model.fit[,-1], order.by=as.POSIXct(model.fit[,1]))
                                    
                                      return(model.fit)
                                    }
  all.training.windows[[length(all.training.windows)+1]] <- rolling.window.results
  print(paste(window/(24*60/5),"day window complete."))
}
end <- Sys.time()
print(end-start)

par(mar=c(5.1,4.1,4.1,4.1))
plot.data <- data.frame(all.training.windows[[1]])
plot(plot.data[,2], type="l", ylab="Z7 Ammonia (mg/L)")
points(x=index(plot.data), y=plot.data[,3], col="red", type="l")
points(x=index(plot.data), y=plot.data[,4], col="blue", type="l")
legend(x=0,y=max(plot.data[,2]),legend=c("Forecast", "Actual", "Sensor"), col=c("black", "red", "blue"), lty=1)
persistence.rmse <- (plot.data[,4] - plot.data[,3])^2
forecast.rmse <- (plot.data[,2] - plot.data[,3])^2
par(new=T)
plot(x=index(plot.data), y=forecast.rmse, col="red", type="l", lty=2, yaxt="n", xaxt="n", ylab="", xlab="")
points(x=index(plot.data), y=persistence.rmse, col="blue", type="l", lty=2, yaxt="n", xaxt="n", ylab="", xlab="")
axis(side=4)
mtext(side=4, text="RMSE", line=2.5)



### Train and test a neural network model

start <- Sys.time()
# all.training.windows <- list()
training.windows <- seq(1*24*60/5,10*24*60/5,by=24*60/5)
training.windows <- training.windows[2:length(training.windows)]
for(window in training.windows) {
  rolling.window.results <- foreach(i=1:(nrow(all.data)-window-2*forecast.horizon/5-1),
                                    .combine=rbind,
                                    .packages = c("xts","neuralnet")) %dopar% {
  # rolling.window.results <- xts()
  # for(i in 1:(nrow(all.data)-window-2*forecast.horizon/5-1)) {
    
                                      # Start loop
                                      five.min.data <- all.data[i:(window-1+i+forecast.horizon/5),]
                                      predictor.col <- which(colnames(five.min.data)==predictor.variable)
                                      
                                      # Convert timestamps to runtime and project onto a unit circle 
                                      time.stamps <- difftime(index(five.min.data), index(five.min.data)[1], units = "mins")
                                      time.stamps <- as.numeric(time.stamps) %% 1440 # Cycles of 1 day are constructed (1440 min/day)
                                      time.stamps <- (time.stamps*360/1440)*pi/180 # Cycles of minutes are converted to radians
                                      
                                      # Save means and standard deviations
                                      training.mean <- mean(unlist(five.min.data[1:(nrow(five.min.data)-forecast.horizon/5),predictor.variable]))
                                      training.sd <- sd(unlist(five.min.data[1:(nrow(five.min.data)-forecast.horizon/5),predictor.variable]))
                                      
                                      # Scale data
                                      five.min.data.scaled <- apply(five.min.data, 2, scale)
                                      five.min.data.scaled <- xts(five.min.data.scaled, order.by = index(five.min.data))

                                      # Subset training data
                                      training.data <- cbind(data.frame(five.min.data.scaled[1:(nrow(five.min.data.scaled)-forecast.horizon/5),]), data.frame(five.min.data.scaled[((forecast.horizon/5+1):nrow(five.min.data.scaled)),predictor.col]))
                                      
                                      colnames(training.data)[ncol(training.data)] <- paste0("Forecasted.",predictor.variable)
                                      
                                      training.data <- cbind(time.stamps[1:nrow(training.data)], 
                                                             training.data,
                                                             cos(time.stamps[1:nrow(training.data)]),
                                                             sin(time.stamps[1:nrow(training.data)]),
                                                             cos(2*time.stamps[1:nrow(training.data)]),
                                                             sin(2*time.stamps[1:nrow(training.data)]),
                                                             cos(3*time.stamps[1:nrow(training.data)]),
                                                             sin(3*time.stamps[1:nrow(training.data)]),
                                                             cos(4*time.stamps[1:nrow(training.data)]),
                                                             sin(4*time.stamps[1:nrow(training.data)]),
                                                             cos(5*time.stamps[1:nrow(training.data)]),
                                                             sin(5*time.stamps[1:nrow(training.data)]))
                                      colnames(training.data)[(ncol(training.data)-9):ncol(training.data)] <- c("COS", "SIN", "COS_2", "SIN_2",
                                                                                                                "COS_3", "SIN_3","COS_4", "SIN_4",
                                                                                                                "COS_5", "SIN_5")
                                      colnames(training.data)[1] <- "time.stamps"
                                      
                                      # Subset testing data
                                      testing.data <- five.min.data.scaled[(nrow(training.data)+1):nrow(five.min.data.scaled),]
                                      testing.data <- cbind(time.stamps[(nrow(training.data)+1):nrow(five.min.data.scaled)], 
                                                            testing.data,
                                                            cos(time.stamps[(nrow(training.data)+1):nrow(five.min.data.scaled)]),
                                                            sin(time.stamps[(nrow(training.data)+1):nrow(five.min.data.scaled)]),
                                                            cos(2*time.stamps[(nrow(training.data)+1):nrow(five.min.data.scaled)]),
                                                            sin(2*time.stamps[(nrow(training.data)+1):nrow(five.min.data.scaled)]),
                                                            cos(3*time.stamps[(nrow(training.data)+1):nrow(five.min.data.scaled)]),
                                                            sin(3*time.stamps[(nrow(training.data)+1):nrow(five.min.data.scaled)]),
                                                            cos(4*time.stamps[(nrow(training.data)+1):nrow(five.min.data.scaled)]),
                                                            sin(4*time.stamps[(nrow(training.data)+1):nrow(five.min.data.scaled)]),
                                                            cos(5*time.stamps[(nrow(training.data)+1):nrow(five.min.data.scaled)]),
                                                            sin(5*time.stamps[(nrow(training.data)+1):nrow(five.min.data.scaled)]))
                                      colnames(testing.data)[1] <- "time.stamps"
                                      colnames(testing.data)[(ncol(testing.data)-9):ncol(testing.data)] <- c("COS", "SIN", "COS_2", "SIN_2",
                                                                                                             "COS_3", "SIN_3","COS_4", "SIN_4",
                                                                                                             "COS_5", "SIN_5")
                                      
                                      
                                      
                                    
                                      # Set x and y model inputs
                                      response.var <- paste0("Forecasted.",predictor.variable)
                                      predictor.var <- colnames(testing.data)
                                      fmla <- as.formula(paste0(response.var," ~ ",paste(c(predictor.var),collapse=" + ")))
                                      
                                      # Build model
                                      nn <- neuralnet(fmla,data=training.data, hidden=3, 
                                                      act.fct = "logistic",
                                                      linear.output = TRUE,
                                                      stepmax=1e+06)
                                      
                                      # Calculate error
                                      
                                      predictions <- compute(nn,training.data[,-which(colnames(training.data)==response.var)])
                                      predictions <- as.numeric(predictions$net.result)
                                      yy <- as.numeric(training.data[,response.var])
                                      SSE <- mean((yy-predictions)^2)
                                      SST <- mean((yy-mean(yy))^2)
                                      Rsqu <- 1-SSE/SST; Rsqu
                                      plot(yy);points(predictions, col="red")
                                      
                                      # Forecast
                                      xx.f <- as.matrix(testing.data)
                                      forecast.data <- compute(nn,xx.f)
                                      forecast.data <- forecast.data$net.result
                                      forecast.data <- xts(as.numeric(forecast.data), order.by = index(testing.data))
                                      colnames(forecast.data)[ncol(forecast.data)] <- paste("Forecasted",predictor.variable)
                                      

                                      ##### Write data #####
                                      final.data <- last(forecast.data*training.sd+training.mean)
                                      actual.data <- all.data[(window-1+i+2*forecast.horizon/5),which(colnames(five.min.data)==predictor.variable)]
                                      sensor.data <- all.data[(window-1+i+forecast.horizon/5),which(colnames(five.min.data)==predictor.variable)]
                                      model.fit <- data.frame("Time" = as.character(index(final.data)), "Training R2" = Rsqu, "Forecast" = as.numeric(final.data), "Actual" = as.numeric(actual.data), "Sensor" = as.numeric(sensor.data), stringsAsFactors = FALSE)
                                      model.fit <- xts(model.fit[,-1], order.by=as.POSIXct(model.fit[,1]))
                                      
                                      # if(length(rolling.window.results) == 0) rolling.window.results <- model.fit
                                      # rolling.window.results <- rbind(rolling.window.results, model.fit)
                                      return(model.fit)
                                    }
  all.training.windows[[length(all.training.windows)+1]] <- rolling.window.results
  print(paste(window/(24*60/5),"day window complete."))
}
end <- Sys.time()
print(end-start)
# 29 min for 1 day window

# Train on ammonia & process variables 50-min prior
# Data[1:n.train]+NH4[51:n.train+50] or ([1:n.train]+50)

# Test on process variables through the end of the data set
# Data[51:end-51]

# Calculate error on testing set with ammonia through the end of the data set
# NH4[n.train+51:end]



par(mar=c(5.1,4.1,4.1,4.1))
plot.data <- data.frame(all.training.windows[[2]])
plot(plot.data[,2], type="l", ylab="Z7 Ammonia (mg/L)")
points(x=index(plot.data), y=plot.data[,3], col="red", type="l")
points(x=index(plot.data), y=plot.data[,4], col="blue", type="l")
legend(x=0,y=max(plot.data[,2]),legend=c("Forecast", "Actual", "Sensor"), col=c("black", "red", "blue"), lty=1)

# legend(x=max(index(plot.data)), y=max(plot.data[,2]), legend=c("Model error", "Persistance error"), col=c("red","blue"), lty=2)
# persistence.rmse <- (plot.data[,4] - plot.data[,3])^2
# forecast.rmse <- (plot.data[,2] - plot.data[,3])^2
# par(new=T)
# plot(x=index(plot.data), y=forecast.rmse, col="red", type="l", lty=2, yaxt="n", xaxt="n", ylab="", xlab="")
# points(x=index(plot.data), y=persistence.rmse, col="blue", type="l", lty=2, yaxt="n", xaxt="n", ylab="", xlab="")
# axis(side=4)
# mtext(side=4, text="RMSE", line=2.5)









### OLD
training.window <- 10*24*60/5

for(i in 1:4280) {
  # Start loop
  five.min.data <- all.data[i:(training.window-1+i+forecast.horizon),]
  predictor.col <- which(colnames(five.min.data)==predictor.variable)
  
  # Convert timestamps to runtime and project onto a unit circle 
  time.stamps <- difftime(index(five.min.data), index(five.min.data)[1], units = "mins")
  time.stamps <- as.numeric(time.stamps) %% 1440 # Cycles of 1 day are constructed (1440 min/day)
  time.stamps <- (time.stamps*360/1440)*pi/180 # Cycles of minutes are converted to radians
  
  # Save means and standard deviations
  training.mean <- mean(unlist(five.min.data[,predictor.variable]))
  training.sd <- sd(unlist(five.min.data[,predictor.variable]))
  
  # Scale data
  five.min.data.scaled <- apply(five.min.data, 2, scale)
  five.min.data.scaled <- xts(five.min.data.scaled, order.by = index(five.min.data))
  
  # Subset training data
  training.data <- cbind(data.frame(five.min.data.scaled[1:(nrow(five.min.data.scaled)-rows+1),]), data.frame(five.min.data.scaled[(rows:nrow(five.min.data.scaled)),predictor.col]))
  colnames(training.data)[ncol(training.data)] <- paste("Forecasted",predictor.variable)
  
  training.data <- cbind(time.stamps[1:nrow(training.data)], 
                         training.data,
                         cos(time.stamps[1:nrow(training.data)]),
                         sin(time.stamps[1:nrow(training.data)]),
                         cos(2*time.stamps[1:nrow(training.data)]),
                         sin(2*time.stamps[1:nrow(training.data)]),
                         cos(3*time.stamps[1:nrow(training.data)]),
                         sin(3*time.stamps[1:nrow(training.data)]),
                         cos(4*time.stamps[1:nrow(training.data)]),
                         sin(4*time.stamps[1:nrow(training.data)]),
                         cos(5*time.stamps[1:nrow(training.data)]),
                         sin(5*time.stamps[1:nrow(training.data)]))
  colnames(training.data)[(ncol(training.data)-9):ncol(training.data)] <- c("COS", "SIN", "COS^2", "SIN^2",
                                                                            "COS^3", "SIN^3","COS^4", "SIN^4",
                                                                            "COS^5", "SIN^5")
  
  # Subset testing data
  testing.data <- five.min.data.scaled[(nrow(training.data)+1):nrow(five.min.data.scaled),]
  testing.data <- cbind(time.stamps[(nrow(training.data)+1):nrow(five.min.data.scaled)], 
                        testing.data,
                        cos(time.stamps[(nrow(training.data)+1):nrow(five.min.data.scaled)]),
                        sin(time.stamps[(nrow(training.data)+1):nrow(five.min.data.scaled)]),
                        cos(2*time.stamps[(nrow(training.data)+1):nrow(five.min.data.scaled)]),
                        sin(2*time.stamps[(nrow(training.data)+1):nrow(five.min.data.scaled)]),
                        cos(3*time.stamps[(nrow(training.data)+1):nrow(five.min.data.scaled)]),
                        sin(3*time.stamps[(nrow(training.data)+1):nrow(five.min.data.scaled)]),
                        cos(4*time.stamps[(nrow(training.data)+1):nrow(five.min.data.scaled)]),
                        sin(4*time.stamps[(nrow(training.data)+1):nrow(five.min.data.scaled)]),
                        cos(5*time.stamps[(nrow(training.data)+1):nrow(five.min.data.scaled)]),
                        sin(5*time.stamps[(nrow(training.data)+1):nrow(five.min.data.scaled)]))
  colnames(testing.data)[(ncol(testing.data)-9):ncol(testing.data)] <- c("COS", "SIN", "COS^2", "SIN^2",
                                                                         "COS^3", "SIN^3","COS^4", "SIN^4",
                                                                         "COS^5", "SIN^5")
  
  
  
  # Set x and y model inputs
  predictor.col <- which(colnames(training.data) == paste("Forecasted",predictor.variable))
  yy <- as.matrix(training.data[,predictor.col])
  xx <- as.matrix(training.data[,-predictor.col])
  
  model.lm <- lm(yy~xx[,17]+xx[,18]+xx[,19]+xx[,20]+xx[,21]+xx[,22]+xx[,23]+xx[,24]+xx[,25]+xx[,26])
  plot(yy)
  points(predict.lm(model.lm, data.frame(xx[,17:26])),type="l")
  text(x=0,y=max(yy), labels=round(summary(model.lm)$r.squared,3), pos=4)
  
  # Ridge regression
  mod.ridge <- cv.glmnet(xx,yy,alpha=0)
  
  # Define weights
  w3 <- abs(matrix(coef(mod.ridge, s=mod.ridge$lambda.min)[, 1][-1]))^1
  
  # Adaptive Lasso
  mod.adaptive <- cv.glmnet(xx, yy,  alpha=1, penalty.factor=w3)
  
  # Calculate error
  s <- as.numeric(mod.adaptive$lambda.min)
  predictions <- predict(mod.adaptive,newx=xx, s=s)
  SSE <- mean((yy-predictions)^2)
  SST <- mean((yy-mean(yy))^2)
  Rsqu <- 1-SSE/SST; 
  
  # Forecast
  xx.f <- as.matrix(testing.data)
  forecast.data <- predict(mod.adaptive,newx=xx.f, s=s)
  forecast.data <- xts(forecast.data, order.by = as.POSIXct(dimnames(forecast.data)[[1]]))
  colnames(forecast.data)[ncol(forecast.data)] <- paste("Forecasted",predictor.variable)
  
  ##### Write data #####
  final.data <- forecast.data*training.sd+training.mean
  model.fit <- data.frame("Time" = as.character(index(final.data)),"SSE" = SSE, "SST" = SST, "Rsqu" = Rsqu, "Value" = as.numeric(final.data), stringsAsFactors = FALSE)
  model.fit <- last(model.fit)
  if(!("Model_Results_min.csv" %in% list.files())) {
    write.csv(model.fit, file=paste0("Model_Results_min.csv"), row.names = FALSE)
  } else {
    all.model.fit <- read.csv(file=paste0("Model_Results_min.csv"))
    all.model.fit <- taRifx::remove.factors(all.model.fit)
    all.model.fit <- rbind(data.frame(all.model.fit, stringsAsFactors = FALSE), model.fit)
    write.csv(all.model.fit, file=paste0("Model_Results_min.csv"), row.names = FALSE)
  }
  
  print(paste(round(i/4280,2)*100,"percent complete"))
}

# Compare to actual value
predictor.col <- which(colnames(all.data)==predictor.variable)
write.csv(data.frame("Time" = as.character(index(all.data)), "Z7 Ammonia Sensor" = as.numeric(all.data[,predictor.col])), file="Actual_NH4.csv")

# Let's do this with a NN
install.packages("neuralnet")
library(neuralnet)
for(i in 1:4280) {
  # Start loop
  five.min.data <- all.data[i:(training.window-1+i+forecast.horizon),]
  predictor.col <- which(colnames(five.min.data)==predictor.variable)
  
  # Save means and standard deviations
  training.mean <- mean(unlist(five.min.data[,predictor.variable]))
  training.sd <- sd(unlist(five.min.data[,predictor.variable]))
  
  # Scale data
  five.min.data.scaled <- apply(five.min.data, 2, scale)
  five.min.data.scaled <- xts(five.min.data.scaled, order.by = index(five.min.data))
  
  # Subset training data
  training.data <- cbind(data.frame(five.min.data.scaled[1:(nrow(five.min.data.scaled)-rows+1),]), data.frame(five.min.data.scaled[(rows:nrow(five.min.data.scaled)),predictor.col]))
  colnames(training.data)[ncol(training.data)] <- paste("Forecasted",predictor.variable)
  
  training.data <- cbind(time.stamps[1:nrow(training.data)], 
                         training.data,
                         cos(time.stamps[1:nrow(training.data)]),
                         sin(time.stamps[1:nrow(training.data)]),
                         cos(2*time.stamps[1:nrow(training.data)]),
                         sin(2*time.stamps[1:nrow(training.data)]),
                         cos(3*time.stamps[1:nrow(training.data)]),
                         sin(3*time.stamps[1:nrow(training.data)]),
                         cos(4*time.stamps[1:nrow(training.data)]),
                         sin(4*time.stamps[1:nrow(training.data)]),
                         cos(5*time.stamps[1:nrow(training.data)]),
                         sin(5*time.stamps[1:nrow(training.data)]))
  colnames(training.data)[(ncol(training.data)-9):ncol(training.data)] <- c("COS", "SIN", "COS^2", "SIN^2",
                                                                            "COS^3", "SIN^3","COS^4", "SIN^4",
                                                                            "COS^5", "SIN^5")
  colnames(training.data) <- make.names(colnames(training.data))
  colnames(training.data)[1] <- c("time.stamps")
  
  # Subset testing data
  testing.data <- five.min.data.scaled[(nrow(training.data)+1):nrow(five.min.data.scaled),]
  testing.data <- cbind(time.stamps[(nrow(training.data)+1):nrow(five.min.data.scaled)], 
                        testing.data,
                        cos(time.stamps[(nrow(training.data)+1):nrow(five.min.data.scaled)]),
                        sin(time.stamps[(nrow(training.data)+1):nrow(five.min.data.scaled)]),
                        cos(2*time.stamps[(nrow(training.data)+1):nrow(five.min.data.scaled)]),
                        sin(2*time.stamps[(nrow(training.data)+1):nrow(five.min.data.scaled)]),
                        cos(3*time.stamps[(nrow(training.data)+1):nrow(five.min.data.scaled)]),
                        sin(3*time.stamps[(nrow(training.data)+1):nrow(five.min.data.scaled)]),
                        cos(4*time.stamps[(nrow(training.data)+1):nrow(five.min.data.scaled)]),
                        sin(4*time.stamps[(nrow(training.data)+1):nrow(five.min.data.scaled)]),
                        cos(5*time.stamps[(nrow(training.data)+1):nrow(five.min.data.scaled)]),
                        sin(5*time.stamps[(nrow(training.data)+1):nrow(five.min.data.scaled)]))
  colnames(testing.data)[(ncol(testing.data)-9):ncol(testing.data)] <- c("COS", "SIN", "COS^2", "SIN^2",
                                                                         "COS^3", "SIN^3","COS^4", "SIN^4",
                                                                         "COS^5", "SIN^5")
  colnames(testing.data) <- make.names(colnames(testing.data))
  colnames(testing.data)[1] <- c("time.stamps")
  
  
  
  # Set x and y model inputs
  response.var <- paste0("Forecasted.",predictor.variable)
  predictor.var <- colnames(testing.data)
  fmla <- as.formula(paste0(response.var," ~ ",paste(c(predictor.var),collapse=" + ")))
  
  # Build model
  nn <- neuralnet(fmla,data=training.data, hidden=3,act.fct = "logistic",
                  linear.output = FALSE)
  
  # Calculate error
  # s <- as.numeric(mod.adaptive$lambda.min)
  # predictions <- predict(mod.adaptive,newx=xx, s=s)
  # SSE <- mean((yy-predictions)^2)
  # SST <- mean((yy-mean(yy))^2)
  # Rsqu <- 1-SSE/SST; 
  
  # Forecast
  xx.f <- as.matrix(testing.data)
  forecast.data <- compute(nn,xx.f)
  forecast.data <- forecast.data$net.result
  forecast.data <- xts(as.numeric(forecast.data), order.by = index(testing.data))
  colnames(forecast.data)[ncol(forecast.data)] <- paste("Forecasted",predictor.variable)
  
  ##### Write data #####
  final.data <- forecast.data*training.sd+training.mean
  model.fit <- data.frame("Time" = as.character(index(final.data)),
                          # "SSE" = SSE, "SST" = SST, "Rsqu" = Rsqu, 
                          "Value" = as.numeric(final.data), stringsAsFactors = FALSE)
  model.fit <- last(model.fit)
  if(!("Model_Results_NN.csv" %in% list.files())) {
    write.csv(model.fit, file=paste0("Model_Results_NN.csv"), row.names = FALSE)
  } else {
    all.model.fit <- read.csv(file=paste0("Model_Results_NN.csv"))
    all.model.fit <- taRifx::remove.factors(all.model.fit)
    all.model.fit <- rbind(data.frame(all.model.fit, stringsAsFactors = FALSE), model.fit)
    write.csv(all.model.fit, file=paste0("Model_Results_NN.csv"), row.names = FALSE)
  }
  
  print(paste(round(i/4280,2)*100,"percent complete"))
  
}