# Import data
{
  library(xts)
  library(readxl)
  
  # This chunk takes upwards of an hour to execute, load "raw_data.RData" if avalible
  if("raw_data.RData" %in% list.files(path="data")) {
    
    load(file="data/raw_data.RData")
    obj.list <- c("ab3_do", 
                  "ab3_3.5",
                  "ab3_4.0",
                  "ab3_4.0_300",
                  "ab3_4.0_300_plus")
    
  } else {
    
    import_data <- "do"
    source("src/import_data.R") # ab3_do
    import_data <- "3.5 mg/L 90"
    source("src/import_data.R") # ab3_3.5
    import_data <- "4.0 mg/L 90"
    source("src/import_data.R") # ab3_4.0
    import_data <- "4.0 mg/L 300"
    source("src/import_data.R") # ab3_4.0_300
    source("src/import_historian_data.R") # ab3_4.0_300_plus
    
    obj.list <- c("ab3_do", 
                  "ab3_3.5",
                  "ab3_4.0",
                  "ab3_4.0_300",
                  "ab3_4.0_300_plus")
    
    save(list=obj.list, file="data/raw_data.RData")
  }
  
  # Homogenize and shorten column names
  source("src/col_name_fix.R")
  
  # Preserve the original 
  real.data <- lapply(obj.list, function(x) get(x))
  names(real.data) <- c("DO", "ABAC 3.5", "ABAC 4.0, 90s", "ABAC 4.0, 300s", "ABAC 4.0, 300s plus")
  
  # Make column names, variable names
  for(x in obj.list) {
    data <- get(x)
    colnames(data) <- make.names(colnames(data))
    assign(x, data)
  }
  
  # Add missing timestamps
  for(x in obj.list) {
    time.intervals <- sapply(2:nrow(get(x)), function(i) difftime(index(get(x))[i], index(get(x))[i-1], units="mins"))
    if(length(which(time.intervals != 5)) > 0) {
      for(i in 1:length(which(time.intervals != 5))){
        start.time <- index(get(x))[which(time.intervals != 5)[i]]
        end.time <- index(get(x))[which(time.intervals != 5)[i]+1]
        new.times <- seq(start.time, end.time, by=5*60)
        new.x <- merge(get(x), new.times[2:(length(new.times)-1)], fill=NA)
        assign(x, new.x)
      }
    }
  }
  
  # Equalize number of observations in each dataset
  lapply(obj.list, 
         function(x) assign(x, get(x)[(nrow(get(x)) - min(sapply(obj.list, function(x) nrow(get(x))))):nrow(get(x)),], 
                            envir = .GlobalEnv))
  
  fixed.data <- lapply(obj.list, function(x) get(x))
}



library(dplyr)
# install.packages("keras")
library(keras)
library(doSNOW)
library(parallel)

# detect cores with parallel() package
nCores <- detectCores(logical = FALSE)
# detect threads with parallel()
nThreads<- detectCores(logical = TRUE)
# Create doSNOW compute cluster
cluster = makeCluster(nThreads, type = "SOCK", outfile="")
# register the cluster
registerDoSNOW(cluster)

obj.data <- fixed.data[[1]] # DO dataset
all.data <- data.matrix(obj.data)
all.data <- all.data[,-which(colnames(all.data) == "Z9.DO")]
predict.col <- which(colnames(all.data) == "Z7.NH4")

all.days.ann <- list()
all.days.rnn <- list()
days <- 1
for(days in 1:6) {
  lookback <- days*24*60/5 # Observations will go back 'days' at the 5 min interval
  forecast.horizon <- seq(1,15) # 5-75 min forecast intervals
  
  
  all.horizons.ann <- list()
  all.horizons.rnn <- list()
  delay <- forecast.horizon[1] # Targets will be some forecast horizon (in observations) into the future
  for(delay in forecast.horizon) {
    # ANN
    results <- foreach(i=seq(1,(nrow(all.data)-(lookback+delay)),by=10), .combine="rbind", .packages=c("keras")) %dopar% {
      # results <- foreach(i=1:5, .combine="rbind", .packages=c("keras")) %dopar% {
      # Create model, add layers, and compile the model
      model <- keras_model_sequential() %>%
        layer_dense(units=round((ncol(all.data))*2/3), input_shape=c(NULL, ncol(all.data))) %>%
        # layer_gru(units=round((ncol(all.data)-1)*2/3),
        #           # dropout = 0.2, recurrent_dropout = 0.2,
        #           # return_sequences = TRUE,
        #           input_shape=list(ncol(all.data),1)) %>%
        # layer_gru(units=round((ncol(all.data)-1)*2/3)) %>%
        layer_dense(units=ncol(all.data)) %>%
        layer_dense(units = 1)
      model %>% compile(
        optimizer = optimizer_rmsprop(),
        loss = "mae"
      )
      train.mean <- apply(all.data[i:(lookback+i-1),],2,mean)
      train.sd <- apply(all.data[i:(lookback+i-1),],2,sd)
      # train.x <- simplify2array(list(scale(all.data[i:(lookback+i-1),], center=train.mean, scale=train.sd)))
      train.x <- scale(all.data[i:(lookback+i-1),], center=train.mean, scale=train.sd)
      # train.y <- simplify2array(list(scale(all.data[(i+delay):(lookback+delay+i-1),], center=train.mean, scale=train.sd)[,predict.col]))
      train.y <- scale(all.data[(i+delay):(lookback+delay+i-1),], center=train.mean, scale=train.sd)[,predict.col]
      history <- model %>% fit(
        x=train.x,
        y=train.y,
        batch_size=1,
        epochs=20
      )
      validation <- model %>% predict(
        x=train.x,
        batch_size=1
      )
      r2 <- cor(validation, train.y)^2;r2
      # test.x <- simplify2array(list(scale(t(all.data[(lookback+i),]), center=train.mean, scale=train.sd)))
      test.x <- scale(t(all.data[(lookback+i),]), center=train.mean, scale=train.sd)
      prediction <- model %>% predict(
        x=test.x,
        batch_size=1
      )
      prediction <- prediction*train.sd[predict.col]+train.mean[predict.col]
      actual <- all.data[(lookback+i+delay),predict.col]
      e <- abs(actual-prediction);e

      return(data.frame(rownames(all.data)[(lookback+i)], actual, prediction, r2, e))
    }
    all.horizons.ann[[length(all.horizons.ann)+1]] <- results
    # print(paste("Completed",days,"days and", delay*5,"min forecast at", Sys.time()))
    rm(results)
    #RNN
    results <- foreach(i=seq(1,(nrow(all.data)-(lookback+delay)),by=10), .combine="rbind", .packages=c("keras")) %dopar% {
      # results <- foreach(i=1:5, .combine="rbind", .packages=c("keras")) %dopar% {
      # Create model, add layers, and compile the model
      model <- keras_model_sequential() %>%
        # layer_dense(units=round((ncol(all.data)-1)*2/3), input_shape=c(NULL, ncol(all.data))) %>%
        layer_gru(units=round(ncol(all.data)*2/3),
                  # dropout = 0.1, # worse rmse
                  # recurrent_dropout = 0.1, # worse rmse
                  return_sequences = TRUE,
                  input_shape=list(ncol(all.data),1)) %>%
        layer_gru(units=ncol(all.data)) %>% # attemped to use dropout, worse r2 and rmse
        layer_dense(units = 1)
      model %>% compile(
        optimizer = optimizer_rmsprop(),
        loss = "mae"
      )
      train.mean <- apply(all.data[i:(lookback+i-1),],2,mean)
      train.sd <- apply(all.data[i:(lookback+i-1),],2,sd)
      train.x <- simplify2array(list(scale(all.data[i:(lookback+i-1),], center=train.mean, scale=train.sd)))
      train.y <- simplify2array(list(scale(all.data[(i+delay):(lookback+delay+i-1),], center=train.mean, scale=train.sd)[,predict.col]))
      history <- model %>% fit(
        x=train.x,
        y=train.y,
        batch_size=1,
        epochs=20
      )
      validation <- model %>% predict(
        x=train.x,
        batch_size=1
      )
      r2 <- cor(validation, train.y)^2;r2
      
      prediction <- model %>% predict(
        x=simplify2array(list(scale(t(all.data[(lookback+i),]), center=train.mean, scale=train.sd))),
        batch_size=1
      )
      prediction <- prediction*train.sd[predict.col]+train.mean[predict.col]
      actual <- all.data[(lookback+i+delay),predict.col]
      e <- abs(actual-prediction);e
      
      # r2;e;past.test
      
      return(data.frame(rownames(all.data)[(lookback+i)], actual, prediction, r2, e))
    }
    all.horizons.rnn[[length(all.horizons.rnn)+1]] <- results
    rm(results)
  }
  all.days.ann[[length(all.days.ann)+1]] <- all.horizons.ann
  all.days.rnn[[length(all.days.rnn)+1]] <- all.horizons.rnn
}
save(all.days.ann, file="results/ann_2layer_do.RData")
save(all.days.rnn, file="results/rnn_2layer_do.RData")
# stop cluster and remove clients
stopCluster(cluster)
# insert serial backend, otherwise error in repetetive tasks
registerDoSEQ()
# clean up a bit.
invisible(gc); remove(nCores); remove(nThreads); remove(cluster);

# load(file="results/ann_2layer_do.RData")
# load(file="results/rnn_2layer_do.RData")