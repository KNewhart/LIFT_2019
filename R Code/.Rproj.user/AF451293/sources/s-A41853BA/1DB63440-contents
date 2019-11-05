setwd("C:\\Users\\Kate Newhart\\Dropbox\\Code\\LIFT_2019\\R Code")

library(xts)
library(readxl)
library(glmnet)

obj.list <- c("ab3_do", 
              "ab3_3.5",
              "ab3_4.0",
              "ab3_4.0_300")

## Import data
# import_data <- "do"
# source("src/import_data.R")
# import_data <- "3.5 mg/L 90"
# source("src/import_data.R")
# import_data <- "4.0 mg/L 90"
# source("src/import_data.R")
# import_data <- "4.0 mg/L 300"
# source("src/import_data.R")
# save(list=obj.list,file="data/ab3_objects.RData")
load("data/ab3_objects.RData")

## Add missing timestamps
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

## Equalize number of observations in each dataset
rows2train <- min(sapply(obj.list, function(x) nrow(get(x))))
lapply(obj.list, function(x) assign(x, get(x)[(nrow(get(x)) - rows2train):nrow(get(x)),], envir = .GlobalEnv))

## Scale data
# lapply(obj.list, function(x) assign(x, scale(get(x)), envir = .GlobalEnv))

# ## Find best diurnal fit
{
# # OLD
# {
# # diurnal.test.results <- list()
# # degrees <- 200
# #   for(x in obj.list) {
# #     # Given that we have limited data and want at least 3 data points for model fitting.
# #     # Test different cycle sizes from 1 hour to 59 hours in intervals of 1 hour
# #     library(doParallel)
# #     library(foreach)
# #     numCores <- detectCores()
# #     registerDoParallel(numCores)
# #     obj.x <- get(x)
# #     diurnal.test.results[[length(diurnal.test.results)+1]] <-
# #       foreach(t=seq(60, floor(nrow(get(x)/3)), by=60),
# #               .combine = 'c', .packages = c("xts")) %dopar% {
# #                 ## Convert timestamps to runtime and project onto a unit circle 
# #                 time.stamps <- difftime(index(obj.x), index(obj.x)[1], units = "mins")
# #                 time.stamps <- as.numeric(time.stamps) %% t # Cycles are constructed
# #                 time.stamps <- (time.stamps*360/t)*pi/180 # Cycles of minutes are converted to radians
# #                 obj.x.t <- cbind(time.stamps,obj.x)
# #                 
# #                 ## Diurnal model
# #                 predict.col <- which(colnames(obj.x.t) == "AB3.Z7.Ammonia.mg.N.L")
# #                 yy <- as.matrix(obj.x.t[,predict.col])
# #                 xx <- as.matrix(obj.x.t[,-predict.col])
# #                 r.sq.vals <- vector()
# #                 for(i in 1:degrees) {
# #                   fmla <- as.formula(paste("yy ~",paste(paste0("cos(",1:i," * xx[,1]) + sin(",1:i," * xx[,1])"),collapse=" + ")))
# #                   diurnal.mod <- lm(fmla)
# #                   r.sq.vals <- c(r.sq.vals, summary(diurnal.mod)$r.squared)
# #                 }
# #                 
# #                 ## Return r sq values for each degree for each cycle
# #                 return.results <- list(r.sq.vals)
# #                 names(return.results) <- paste(x, t)
# #                 return(return.results)
# #               }
# #     stopImplicitCluster()
# #     names(diurnal.test.results)[length(diurnal.test.results)] <- paste(x)
# #   } 
# }
# 
# degrees <- 20
# diurnal.test.results <- list()
# library(doParallel)
# library(foreach)
# numCores <- detectCores()
# registerDoParallel(numCores)
# for(x in obj.list) {
#   # Given that we have limited data and want at least 3 data points for model fitting.
#   # Test different cycle sizes from 1 hour to 59 hours in intervals of 1 hour
#   obj.x <- get(x)
# 
#   ## Convert timestamps to runtime and project onto a unit circle 
#   t <- 1440
#   time.stamps <- difftime(index(obj.x), index(obj.x)[1], units = "mins")
#   time.stamps <- as.numeric(time.stamps) %% t # Cycles are constructed
#   time.stamps <- (time.stamps*360/t)*pi/180 # Cycles of minutes are converted to radians
#   obj.x.t <- cbind(time.stamps,obj.x)
#   
#   ## Diurnal model
#   predict.col <- which(colnames(obj.x.t) == "AB3.Z7.Ammonia.mg.N.L")
#   yy <- as.matrix(obj.x.t[,predict.col])
#   xx <- as.matrix(obj.x.t[,-predict.col])
#   
#   r.sq.vals <- foreach(i=1:degrees,
#                        .combine = 'c', .packages = c("xts","stats"), .export = c("yy","xx")) %dopar% {
#               fmla <- as.formula(paste("yy ~",paste(paste0("cos(",1:i," * xx[,1]) + sin(",1:i," * xx[,1])"),collapse=" + ")))
#               diurnal.mod <- lm(fmla)
#               return(summary(diurnal.mod)$r.squared)
#             }
#               
#   ## Return r sq values for each degree for each cycle
#   diurnal.test.results[[length(diurnal.test.results)+1]] <- r.sq.vals
#   names(diurnal.test.results)[length(diurnal.test.results)] <- paste(x)
# }
# stopImplicitCluster()
# 
# save(diurnal.test.results,file="diurnal-test-results.RData")
# load("diurnal-test-results.RData")
# 
# # Plot results
# # OLD
# {
# #   # Plot r2 as a function of cycle time
# #   png(filename="diurnal-model-rsq-time.png", width = 1000, height = 1000/4)
# #   par(mfrow=c(1,4))
# #   # Do I need to average the cycle time?
# #   lapply(diurnal.test.results, function(x) {
# #     plot(y=as.numeric(sapply(x, max)), x=seq(1, length(x), by=1),
# #          pch=20, xlab = "Time (hours)", ylab="Rsq", 
# #          main=strsplit(names(x)[1], " ")[[1]][1], ylim=c(0,0.75))
# #   })
# #   mtext("Diurnal Model as Function of Cycle Length", side = 3, line = 1, outer=TRUE, adj=0.5)
# #   dev.off()
# #   
# #   # Plot r2 as a function of degree of diurnal model
# #   png(filename="diurnal-model-rsq-degree.png", width = 1000, height = 1000/4)
# #   par(mfrow=c(1,4))
# #   lapply(diurnal.test.results, function(x) {
# #     # Average cycles!!!!!
# #     best.cycle <- which(sapply(x, max) == max(sapply(x, max)))[1]
# #     min.degree <- which(x[[best.cycle]] == max(x[[best.cycle]]))[1]
# #     interval <- 1:degrees
# #     plot(y=x[[best.cycle]], x=interval, 
# #          pch=20, xlab="Degree of Diurnal Model", ylab="Rsq",
# #          main=strsplit(names(x)[1], " ")[[1]][1], ylim=c(0,0.75))
# #   })
# #   mtext("Diurnal Model as Function of Degree", side = 3, line = 1, outer=TRUE, adj=0.5)
# #   dev.off()
# }
# 
# # Plot r2 as a function of degree of diurnal model
# # png(filename="diurnal-model-rsq-degree.png", width = 800, height = 800/4)
# par(mfrow=c(1,4), mar=c(5, 4, 3, 1.5) + 0.1, oma=c(0,0,2,0))
# lapply(diurnal.test.results, function(x) {
#   interval <- 1:degrees
#   plot(y=x, x=interval,
#        # pch=20,
#        type = "l",
#        xlab="Degree of Diurnal Model", ylab="Rsq",
#        ylim=c(0,0.6),
#        main= names(which(unlist(lapply(diurnal.test.results, sum)) == sum(unlist(x)))))
#   points(y=x, x=interval,
#          pch=20, cex=1.5)
#   abline(h=max(x), col="red", cex=1.25)
#   text(x=degrees-1,y=max(x),labels=round(max(x),2),pos=3,col="red")
# })
# mtext("Diurnal Model as Function of Degree", side = 3, line = 0, outer=TRUE, adj=0.5)
# # dev.off()
}

diurnally.adj.ammonia.all <- list()
i <- 5 # 5 degree diurnal model captures diurnal variation
library(doParallel)
library(foreach)
for(x in obj.list) {
  # Given that we have limited data and want at least 3 data points for model fitting.
  # Test different cycle sizes from 1 hour to 59 hours in intervals of 1 hour
  obj.x <- get(x)
  
  ## Convert timestamps to runtime and project onto a unit circle 
  t <- 1440
  time.stamps <- difftime(index(obj.x), index(obj.x)[1], units = "mins")
  time.stamps <- as.numeric(time.stamps) %% t # Cycles are constructed
  time.stamps <- (time.stamps*360/t)*pi/180 # Cycles of minutes are converted to radians
  obj.x.t <- cbind(time.stamps,obj.x)
  if(length(which(apply(obj.x.t,2,function(x) length(unique(x))) < 10)) > 0) obj.x.t <- obj.x.t[,-which(apply(obj.x.t,2,function(x) length(unique(x))) < 10)]
  numCores <- detectCores()
  registerDoParallel(numCores)
  
  ## Fit model using n day training window
  diurnally.adj.ammonia <- foreach(rows=(1440/5*3+1):(nrow(obj.x.t)-20),
                                    .combine = 'rbind', .packages = c("xts","stats","glmnet")) %dopar% {
                                  
                                   # Set initial training data set
                                   predict.col <- which(colnames(obj.x.t) == "AB3.Z7.Ammonia.mg.N.L")
                                   obs <- seq(rows-(1440/5*3), rows-1, by = 1) # 3 day training window
                                   xx <- as.matrix(obj.x.t[obs,-predict.col])
                                   xx.mean <- apply(na.omit(xx), 2, mean)
                                   xx.sd <- apply(na.omit(xx),2,sd)
                                   xx <- scale(xx)
                                   
                                   # Diurnal model
                                   xx <- cbind(xx, sapply(paste0("cos(",1:i," * xx[,1])"), function(x) eval(parse(text=x))))
                                   xx <- cbind(xx, sapply(paste0("sin(",1:i," * xx[,1])"), function(x) eval(parse(text=x))))
                                   
                                   # Simulate future value of ammonia
                                   yy.f <- as.matrix(obj.x.t[obs+10,predict.col])
                                   yy.f.mean <- apply(yy.f, 2, mean)
                                   yy.f.sd <- apply(yy.f,2,sd)
                                   yy.f <- scale(yy.f)
                                   
                                   # Fit linear model
                                   mod.ridge <- cv.glmnet(x=xx,y=yy.f,alpha=0)
                                   w3 <- 1/abs(matrix(coef(mod.ridge, s=mod.ridge$lambda.min)[, 1][-1]))^1
                                   mod.adaptive <- cv.glmnet(x=xx, y=yy.f,  alpha=1, penalty.factor=w3)
                                   
                                   # Training Rsq
                                   predictions <- predict(mod.adaptive,newx=xx, s='lambda.1se')
                                   SSE <- mean((yy.f-predictions)^2)
                                   SST <- mean((yy.f-mean(yy.f))^2)
                                   Rsqu <- 1-SSE/SST
                                   Rsqu
                                   
                                   # Testing data
                                   xx <- data.frame(obj.x.t[(last(obs)+10),-predict.col])
                                   xx <- apply(xx, 2, )
                                   xx <- cbind(xx, t(as.matrix(sapply(paste0("cos(",1:i," * xx[,1])"), function(x) eval(parse(text=x))))))
                                   xx <- cbind(xx, t(as.matrix(sapply(paste0("sin(",1:i," * xx[,1])"), function(x) eval(parse(text=x))))))
                                   xx <- as.matrix(xx)
                                   
                                   # Make prediction
                                   yy.p <- predict(mod.adaptive,newx=xx, s='lambda.1se')
                                   dimnames(yy.p)[[2]] <- "Predicted Future Value"
                                   
                                   # Compare to actual future value
                                   yy.a <- obj.x.t[last(obs)+20,predict.col]
                                   dimnames(yy.a)[[2]] <- "Actual Future Value"
                                   
                                   return.results <- merge(yy.a, yy.p)
                                   return(return.results)
                                  }
  
  
  stopImplicitCluster()
  rownames(diurnally.adj.ammonia) <- as.character(index(obj.x.t)[(1440/5*3+1+10):(nrow(obj.x.t)-20+10)])
  # Rowname is the time that the prediction was made
  
  
  ## Return r sq values for each degree for each cycle
  diurnally.adj.ammonia.all[[length(diurnally.adj.ammonia.all)+1]] <- diurnally.adj.ammonia
  names(diurnally.adj.ammonia.all)[length(diurnally.adj.ammonia.all)] <- paste(x)
}
