setwd("C:\\Users\\R-Compiler\\Documents\\KNewhart\\LIFT_2019\\R Code")

##### Only NN
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
library(parallel)
library(doSNOW)
# detect threads with parallel()
nThreads<- detectCores(logical = TRUE)

obj.data <- fixed.data[[4]] # DO dataset
load("results/all-days-ab3_4.0_300.RData")
all.data <- data.matrix(obj.data)
all.data <- all.data[,-which(colnames(all.data) == "Z9.DO")]
predict.col <- which(colnames(all.data) == "Z7.NH4")

#### JUST NN ####
{
all.days.ann <- list()
all.days.rnn <- list()
days <- 1

for(days in 1:6) {
# for(days in 4:6) {

  lookback <- days*24*60/5 # Observations will go back 'days' at the 5 min interval
  forecast.horizon <- seq(1,15) # 5-75 min forecast intervals

  all.horizons.ann <- list()
  all.horizons.rnn <- list()
  delay <- forecast.horizon[1] # Targets will be some forecast horizon (in observations) into the future

  for(delay in forecast.horizon) {

    # Create doSNOW compute cluster
    cluster = makeCluster(nThreads, type = "SOCK", outfile="")
    # register the cluster
    registerDoSNOW(cluster)
    # iterations <- seq(1,(nrow(all.data)-(lookback+delay)))
    iterations <- seq((6*24*60/5-lookback)+1, nrow(all.data)-(lookback+delay),by=1)
    pb <- txtProgressBar(min=0, max = length(iterations), style = 3)
    progress <- function(n) setTxtProgressBar(pb, n)
    opts <- list(progress = progress)
    print(paste("Starting ANN:", days, delay))

    # ANN
    results <- foreach(i=iterations, .combine="rbind", .packages=c("keras"), .options.snow = opts) %dopar% {
      # results <- foreach(i=1:5, .combine="rbind", .packages=c("keras")) %dopar% {
      # Create model, add layers, and compile the model
      model <- keras_model_sequential() %>%
        layer_dense(units=round((ncol(all.data))*2/3), input_shape=c(NULL, ncol(all.data))) %>%
        layer_dense(units=ncol(all.data)) %>%
        layer_dense(units = 1)
      model %>% compile(
        optimizer = optimizer_rmsprop(),
        loss = "mean_squared_error"
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
    close(pb)
    stopCluster(cluster)

    all.horizons.ann[[length(all.horizons.ann)+1]] <- results

    # print(paste("Completed",days,"days and", delay*5,"min forecast at", Sys.time()))
    rm(results)




    # # Create doSNOW compute cluster
    # cluster = makeCluster(nThreads, type = "SOCK", outfile="")
    # # register the cluster
    # registerDoSNOW(cluster)
    # 
    # pb <- txtProgressBar(min=0, max = length(iterations), style = 3)
    # progress <- function(n) setTxtProgressBar(pb, n)
    # opts <- list(progress = progress)
    # 
    # #RNN
    # print(paste("Starting RNN:", days, delay))
    # # results <- foreach(i=seq(1,(nrow(all.data)-(lookback+delay)),by=10), .combine="rbind", .packages=c("keras")) %dopar% {
    #   results <- foreach(i=iterations, .combine="rbind", .packages=c("keras"), .options.snow = opts) %dopar% {
    #   # Create model, add layers, and compile the model
    #   model <- keras_model_sequential() %>%
    #     # layer_dense(units=round((ncol(all.data)-1)*2/3), input_shape=c(NULL, ncol(all.data))) %>%
    #     layer_gru(units=round(ncol(all.data)*2/3),
    #               # dropout = 0.1, # worse rmse
    #               # recurrent_dropout = 0.1, # worse rmse
    #               return_sequences = TRUE,
    #               input_shape=list(ncol(all.data),1)) %>%
    #     layer_gru(units=ncol(all.data)) %>% # attemped to use dropout, worse r2 and rmse
    #     layer_dense(units = 1)
    #   model %>% compile(
    #     optimizer = optimizer_rmsprop(),
    #     loss = "mae"
    #   )
    #   train.mean <- apply(all.data[i:(lookback+i-1),],2,mean)
    #   train.sd <- apply(all.data[i:(lookback+i-1),],2,sd)
    #   train.x <- simplify2array(list(scale(all.data[i:(lookback+i-1),], center=train.mean, scale=train.sd)))
    #   train.y <- simplify2array(list(scale(all.data[(i+delay):(lookback+delay+i-1),], center=train.mean, scale=train.sd)[,predict.col]))
    #   history <- model %>% fit(
    #     x=train.x,
    #     y=train.y,
    #     batch_size=1,
    #     epochs=20
    #   )
    #   validation <- model %>% predict(
    #     x=train.x,
    #     batch_size=1
    #   )
    #   r2 <- cor(validation, train.y)^2;r2
    # 
    #   prediction <- model %>% predict(
    #     x=simplify2array(list(scale(t(all.data[(lookback+i),]), center=train.mean, scale=train.sd))),
    #     batch_size=1
    #   )
    #   prediction <- prediction*train.sd[predict.col]+train.mean[predict.col]
    #   actual <- all.data[(lookback+i+delay),predict.col]
    #   e <- abs(actual-prediction);e
    # 
    #   # r2;e;past.test
    # 
    #   return(data.frame(rownames(all.data)[(lookback+i)], actual, prediction, r2, e))
    #   }
    #   close(pb)
    #   stopCluster(cluster)
    # 
    # all.horizons.rnn[[length(all.horizons.rnn)+1]] <- results
    # rm(results)

  }
  all.days.ann[[length(all.days.ann)+1]] <- all.horizons.ann
  # all.days.rnn[[length(all.days.rnn)+1]] <- all.horizons.rnn
  save(all.days.ann, file="results/nn/ann_2layer_abac4_300.RData")
  # save(all.days.rnn, file="results/nn/rnn_2layer_do.RData")
}
save(all.days.ann, file="results/ann_2layer_abac4_300.RData")
# save(all.days.rnn, file="results/rnn_2layer_do.RData")
#clean up a bit.
invisible(gc); remove(nThreads); remove(cluster)
}

#### Residual NN ####
all.days.ann.r <- list()
# all.days.rnn.r <- list()
days <- 1
nThreads<- detectCores(logical = TRUE)

# Convert timestamps to runtime and project onto a unit circle
t <- 1440
time.stamps <- difftime(rownames(all.data), rownames(all.data)[1], units = "mins")
time.stamps <- as.numeric(time.stamps) %% t # Cycles are constructed
time.stamps <- (time.stamps*360/t)*pi/180 # Cycles of minutes are converted to radians
names(time.stamps) <- "time.stamps"

cos.x <- cos(time.stamps)
names(cos.x) <- "cos.x"
sin.x <- sin(time.stamps)
names(sin.x) <- "sin.x"
cos.2x <- cos(2*time.stamps)
names(cos.2x) <- "cos.2x"
sin.2x <- sin(2*time.stamps)
names(sin.2x) <- "sin.2x"
cos.3x <- cos(3*time.stamps)
names(cos.3x) <- "cos.3x"
sin.3x <- sin(3*time.stamps)
names(sin.3x) <- "sin.3x"
cos.4x <- cos(4*time.stamps)
names(cos.4x) <- "cos.4x"
sin.4x <- sin(4*time.stamps)
names(sin.4x) <- "sin.4x"
cos.5x <- cos(5*time.stamps)
names(cos.5x) <- "cos.5x"
sin.5x <- sin(5*time.stamps)
names(sin.5x) <- "sin.5x"
cos.6x <- cos(6*time.stamps)
names(cos.6x) <- "cos.6x"
sin.6x <- sin(6*time.stamps)
names(sin.6x) <- "sin.6x"
all.data <- cbind(all.data, cos.x, sin.x, cos.2x, sin.2x,
                cos.3x, sin.3x, cos.4x, sin.4x, cos.5x, sin.5x, cos.6x, sin.6x)

for(days in 1:6) {


  lookback <- days*24*60/5 # Observations will go back 'days' at the 5 min interval
  forecast.horizon <- seq(1,15) # 5-75 min forecast intervals

  all.horizons.ann.r <- list()
  # all.horizons.rnn.r <- list()
  delay <- forecast.horizon[1] # Targets will be some forecast horizon (in observations) into the future

  for(delay in forecast.horizon) {

    # Create doSNOW compute cluster
    cluster = makeCluster(nThreads, type = "SOCK", outfile="")
    # register the cluster
    registerDoSNOW(cluster)
    # iterations <- seq(1,(nrow(all.data)-(lookback+delay)))
    iterations <- seq((6*24*60/5-lookback)+1, nrow(all.data)-(lookback+delay),by=1)
    pb <- txtProgressBar(min=0, max = length(iterations), style = 3)
    progress <- function(n) setTxtProgressBar(pb, n)
    opts <- list(progress = progress)
    print(paste("Starting ANN:", days, delay))

    # ANN
    results <- foreach(i=iterations, .combine="rbind", .packages=c("keras", "glmnet"), .options.snow = opts) %dopar% {
      # results <- foreach(i=1:5, .combine="rbind", .packages=c("keras")) %dopar% {
      # Create model, add layers, and compile the model
      model <- keras_model_sequential() %>%
        layer_dense(units=round((ncol(all.data))*2/3), input_shape=c(NULL, ncol(all.data)+1)) %>%
        # layer_gru(units=round((ncol(all.data)-1)*2/3),
        #           # dropout = 0.2, recurrent_dropout = 0.2,
        #           # return_sequences = TRUE,
        #           input_shape=list(ncol(all.data)+1,1)) %>%
        # layer_gru(units=round((ncol(all.data)-1)*2/3)) %>%
        layer_dense(units=ncol(all.data)+1) %>%
        layer_dense(units = 1)
      model %>% compile(
        # optimizer = 'sgd',
        optimizer = optimizer_rmsprop(),
        loss = "mean_squared_error"
      )
      train.mean <- apply(all.data[i:(lookback+i-1),],2,mean)
      train.sd <- apply(all.data[i:(lookback+i-1),],2,sd)
      train.x <- scale(all.data[i:(lookback+i-1),], center=train.mean, scale=train.sd)
      train.y <- scale(all.data[(i+delay):(lookback+delay+i-1),], center=train.mean, scale=train.sd)[,predict.col]

      # Train model when lambda=0 (initial parameter estimate)
      mod.ridge <- cv.glmnet(train.x,train.y,alpha=0)
      predict.mod.ridge <- predict(mod.ridge, newx=train.x)
      # Adaptive lasso
      w3 <- 1/abs(matrix(coef(mod.ridge, s=mod.ridge$lambda.min)[, 1][-1]))^1
      set.seed(Sys.time())
      mod.adaptive <- cv.glmnet(train.x,train.y,alpha=1,penalty.factor=w3)
      pred.adapt <- predict(mod.adaptive,newx=train.x, s='lambda.1se')
      pred.mean <- mean(pred.adapt)
      pred.sd <- sd(pred.adapt)
      pred.adapt <- scale(pred.adapt, center=pred.mean, scale=pred.sd)
      
      history <- model %>% fit(
        x=cbind(train.x, pred.adapt),
        y=train.y,
        batch_size=1,
        epochs=20
      )
      validation <- model %>% predict(
        x=cbind(train.x, pred.adapt),
        batch_size=1
      )
      r2 <- cor(validation, train.y)^2;r2
      # test.x <- simplify2array(list(scale(t(all.data[(lookback+i),]), center=train.mean, scale=train.sd)))
      test.x <- scale(t(all.data[(lookback+i),]), center=train.mean, scale=train.sd)

      # Forecast
      pred.adapt <- predict(mod.adaptive,newx=test.x, s='lambda.1se')
      pred.adapt <- scale(pred.adapt, center=pred.mean, scale = pred.sd)
      
      prediction <- model %>% predict(
        x=cbind(test.x, pred.adapt),
        batch_size=1
      )
      prediction <- prediction*train.sd[predict.col]+train.mean[predict.col]
      actual <- all.data[(lookback+i+delay),predict.col]
      e <- abs(actual-prediction);e

      return(data.frame(rownames(all.data)[(lookback+i)], actual, prediction, r2, e))
    }
    close(pb)
    stopCluster(cluster)

    all.horizons.ann.r[[length(all.horizons.ann.r)+1]] <- results

    # print(paste("Completed",days,"days and", delay*5,"min forecast at", Sys.time()))
    rm(results)




    # # Create doSNOW compute cluster
    # cluster = makeCluster(nThreads, type = "SOCK", outfile="")
    # # register the cluster
    # registerDoSNOW(cluster)
    # # iterations <- seq(1,(nrow(all.data)-(lookback+delay)))
    # iterations <- seq((6*24*60/5-lookback)+1, nrow(all.data)-(lookback+delay),by=1)
    # pb <- txtProgressBar(min=0, max = length(iterations), style = 3)
    # progress <- function(n) setTxtProgressBar(pb, n)
    # opts <- list(progress = progress)
    # print(paste("Starting RNN:", days, delay))
    # #RNN
    # print(paste("Starting RNN:", days, delay))
    # # results <- foreach(i=seq(1,(nrow(all.data)-(lookback+delay)),by=10), .combine="rbind", .packages=c("keras")) %dopar% {
    # results <- foreach(i=iterations, .combine="rbind", .packages=c("keras", "glmnet"), .options.snow = opts) %dopar% {
    #   # Create model, add layers, and compile the model
    #   model <- keras_model_sequential() %>%
    #     # layer_dense(units=round((ncol(all.data)-1)*2/3), input_shape=c(NULL, ncol(all.data)+1)) %>%
    #     layer_gru(units=round(ncol(all.data)*2/3),
    #               # dropout = 0.1, # worse rmse
    #               # recurrent_dropout = 0.1, # worse rmse
    #               return_sequences = TRUE,
    #               input_shape=list(ncol(all.data)+1,1)) %>%
    #     layer_gru(units=ncol(all.data)+1) %>% # attemped to use dropout, worse r2 and rmse
    #     layer_dense(units = 1)
    #   model %>% compile(
    #     optimizer = optimizer_rmsprop(),
    #     loss = "mae"
    #   )
    #   
    #   train.mean <- apply(all.data[i:(lookback+i-1),],2,mean)
    #   train.sd <- apply(all.data[i:(lookback+i-1),],2,sd)
    #   train.x <- scale(all.data[i:(lookback+i-1),], center=train.mean, scale=train.sd)
    #   train.y <- scale(all.data[(i+delay):(lookback+delay+i-1),], center=train.mean, scale=train.sd)[,predict.col]
    #   
    #   # Train model when lambda=0 (initial parameter estimate)
    #   mod.ridge <- cv.glmnet(train.x,train.y,alpha=0)
    #   predict.mod.ridge <- predict(mod.ridge, newx=train.x)
    #   # Adaptive lasso
    #   w3 <- 1/abs(matrix(coef(mod.ridge, s=mod.ridge$lambda.min)[, 1][-1]))^1
    #   set.seed(Sys.time())
    #   mod.adaptive <- cv.glmnet(train.x,train.y,alpha=1,penalty.factor=w3)
    #   pred.adapt <- predict(mod.adaptive,newx=train.x, s='lambda.1se')
    # 
    #   train.mean <- apply(all.data[i:(lookback+i-1),],2,mean)
    #   train.sd <- apply(all.data[i:(lookback+i-1),],2,sd)
    #   pred.mean <- mean(pred.adapt)
    #   pred.sd <- sd(pred.adapt)
    #   train.x <- simplify2array(list(cbind(scale(all.data[i:(lookback+i-1),], center=train.mean, scale=train.sd), 
    #                                        scale(pred.adapt, center=pred.mean, scale = pred.sd), deparse.level = 0)))
    #   train.y <- simplify2array(list(scale(all.data[(i+delay):(lookback+delay+i-1),], center=train.mean, scale=train.sd)[,predict.col]))
    #   
    #   
    #   history <- model %>% fit(
    #     x=train.x,
    #     y=train.y,
    #     batch_size=1,
    #     epochs=20
    #   )
    #   validation <- model %>% predict(
    #     x=train.x,
    #     batch_size=1
    #   )
    #   r2 <- cor(validation, train.y)^2
    #   # test.x <- simplify2array(list(scale(t(all.data[(lookback+i),]), center=train.mean, scale=train.sd)))
    #   test.x <- scale(t(all.data[(lookback+i),]), center=train.mean, scale=train.sd)
    # 
    #   # Forecast
    #   pred.adapt <- predict(mod.adaptive,newx=test.x, s='lambda.1se')
    #   pred.adapt <- scale(pred.adapt, center=pred.mean, scale = pred.sd)
    # 
    #   prediction <- model %>% predict(
    #     x=simplify2array(list(cbind(test.x, pred.adapt))),
    #     batch_size=1
    #   )
    #   prediction <- prediction*train.sd[predict.col]+train.mean[predict.col]
    #   actual <- all.data[(lookback+i+delay),predict.col]
    #   e <- abs(actual-prediction)
    # 
    #   # r2;e;past.test
    # 
    #   return(data.frame(rownames(all.data)[(lookback+i)], actual, prediction, r2, e))
    # }
    # close(pb)
    # stopCluster(cluster)
    # 
    # all.horizons.rnn.r[[length(all.horizons.rnn.r)+1]] <- results
    # rm(results)

  }
  all.days.ann.r[[length(all.days.ann.r)+1]] <- all.horizons.ann.r
  # all.days.rnn.r[[length(all.days.rnn.r)+1]] <- all.horizons.rnn.r
  save(all.days.ann.r, file="results/nn/ann_r_2layer_abac4_300.RData")
  # save(all.days.rnn.r, file="results/nn/rnn_r_2layer_do.RData")
}
save(all.days.ann.r, file="results/ann_r_2layer_abac4_300.RData")
# save(all.days.rnn.r, file="results/rnn_r_2layer_do.RData")


