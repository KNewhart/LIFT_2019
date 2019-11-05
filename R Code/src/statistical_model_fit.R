# library(doParallel)
# library(foreach)
# numCores <- detectCores()
# registerDoParallel(numCores)
# for(x in obj.list) {
# lm.results <- foreach(obj.data=lapply(obj.list, get), 
#                       .combine = 'c', .packages = c("xts","stats","glmnet")) %dopar% {
#                         
obj.data <- get(obj.list[1])
predict.col <- which(colnames(obj.data) == "Z7.NH4")

# Days to train on
training.days <- 7
training.obs <- training.days*24*60/5
total.obs <- nrow(obj.data)
testing.obs <- total.obs-training.obs
testing.days <- testing.obs/(24*60/5)

# What is the forecast horizon? (given in number of steps & number of minutes )
horizon.min <- seq(5,75,by=5)
horizon.steps <- horizon.min/5

# Set of things to save through the loop
model_obj.data <- matrix(NA, nrow=testing.obs, ncol=length(horizon.min))
persistence_obj.data <- matrix(NA, nrow=testing.obs, ncol=length(horizon.min))
true_obj.data <- matrix(NA, nrow=testing.obs, ncol=length(horizon.min))

RsqDir_obj.data <- matrix(NA, nrow=testing.obs, ncol=length(horizon.min))
RsqAdp_obj.data <- matrix(NA, nrow=testing.obs, ncol=length(horizon.min))
                        
################################################################
#Loop over the forecast horizon, index goes i=1,2,3,...,14,15
################################################################
for(i in 1:length(horizon.steps)){
  print(paste("i, horizon step is", i, sep=" "))
  
  # Setup training and testing data for diurnal model
  current.obs <- training.obs+i
  train.xx <- matrix(as.numeric(obj.data[1:(current.obs-i),]), ncol=ncol(obj.data), byrow=FALSE)
  train.yy <- matrix(as.numeric(obj.data[(1+i):current.obs, predict.col]), ncol=1)
  test.xx <- matrix(as.numeric(obj.data[current.obs,]), ncol=ncol(obj.data), byrow=FALSE)
  test.yy <- matrix(as.numeric(obj.data[(current.obs+i), predict.col]), ncol=1)
  
  # Train diurnal model
  terms <- c(grep("cos", colnames(obj.data)), grep("sin", colnames(obj.data)))
  fmla <- paste0("train.yy ~ ", paste0("train.xx[,",terms,"]", collapse=" + "))
  mod.lm <- lm(fmla)
  
  # Update response variables with diurnal forecast
  train.yy.diurnal <- apply(train.xx[,terms],1,function(x) mod.lm$coef%*%c(1,x))
  
  par(mar=c(2.5,3.5,5.5,.5),xpd=TRUE)
  plot(train.yy,ylab="",xlab="")
  mtext(side=2, line=2.5, "Scaled Ammonia")
  mtext(side=1, line=2.5, "Observations (5 min interval)")
  points(train.yy.diurnal, col="blue")
  
  # Train model when lambda=0 (initial parameter estimate)
  mod.ridge=cv.glmnet(train.xx,train.yy,alpha=0)
  points(predict(mod.ridge, newx=train.xx), col="red")
  mod.ridge=cv.glmnet(train.xx,train.yy.diurnal,alpha=0)
  points(predict(mod.ridge, newx=train.xx), col="orange")
  legend("topright",inset=c(0,-.2),legend=c("Actual NH4","Diurnal Forecast","Linear Forecast w/ Sine Cosine", "Diurnal + Linear Forecast"),
         col=c("black", "blue", "red", "orange"), pch=1)
  
  pred.ridge <- predict(mod.ridge, newx=train.xx)
  
  weight = 1/abs(matrix(coef(mod.ridge, s=mod.ridge$lambda.min)[, 1][-1]))^1
  mod.adaptive = cv.glmnet(train.xx, train.yy,  alpha=1, penalty.factor=weight)
  adapt.coef=coef(mod.adaptive, s=mod.adaptive$lambda.1se)
  
  pred.adapt=predict(mod.adaptive,newx=train.xx, s='lambda.min')
  
  
  # Obtaining the persistence forecast for each horizon
  # slots says where to put the persistence forecasts in the matrix to save
  slots=1:(testing.obs-i+1)
  persistence_obj.data[slots,i]=obj.data[(training.obs):(total.obs-i),predict.col]
  true_obj.data[slots,i]=obj.data[(training.obs+i):total.obs,predict.col]
  
  ################################################################
  #Loop over the observations to make predictions, index goes j=1:#testing obs-horizon+1
  ################################################################
  for(j in 1:(testing.obs-i+1)){
    if(j%%100==0){print(paste("j, forecast observation is", j, sep=" "))}
    
    train.data=obj.data[(j):(j+training.obs-2),]
    print(paste("Training data:", paste(range(index(train.data)), collapse=" ")))
    
    #Index of current observation
    curr.index=j+training.obs-1
    print(paste("Current obs:", paste(index(obj.data[curr.index,]))))
    
    #Index of observation to forecast
    fore.index=j+training.obs-1+i
    print(paste("Forecast:", paste(index(obj.data[fore.index,]))))
    
    ###########################
    ##MODELING STEP #1
    ##Estimate diurnal trend of these 7 days
    ###########################
    #Grabbing the columns of the response and cosine/sine terms
    yy=train.data[,predict.col]
    x1=train.data[,diurnal.1]
    x2=train.data[,diurnal.2]
    x3=train.data[,diurnal.3]
    x4=train.data[,diurnal.4]
    x5=train.data[,diurnal.5]
    x6=train.data[,diurnal.6]
    x7=train.data[,diurnal.7]
    x8=train.data[,diurnal.8]
    x9=train.data[,diurnal.9]
    x10=train.data[,diurnal.10]
    x11=train.data[,diurnal.11]
    x12=train.data[,diurnal.12]
    
    #sum(complete.cases(yy))
    diurnal.train=lm(yy~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12, na.action=na.exclude)
    Rsq.diurnal=summary(diurnal.train)$r.squared  #to save R^2 of diurnal model on training set
    RsqDir_obj.data[j,i]=Rsq.diurnal
    
    #Forecast from diurnal model for step ahead
    diurn.fore=diurnal.train$coef%*%c(1,obj.data[fore.index,diurnal.1:diurnal.12])
    
    #Level 1 residuals
    train.res1=yy-predict(diurnal.train)
    
    ###########################
    ##MODELING STEP #2
    ##Fit residuals of diurnal model with lagged values of covariates and lagged value of residual of response
    ###########################
    
    #Creating the predictor matrix, removing timestamp and cosine/sine terms
    # XX=cbind(train.res1,train.data[,-c(time.col, diurnal.1:diurnal.12)])
    # colnames(XX)[1]="Z9.NH4.residual"
    # nn.XX=dim(XX)[1]
    XX <- train.data
    
    #Lag covariates
    # train.yy=train.res1[(i+1):nn.XX]
    # colnames(train.yy)[1]="Z9.NH4.residual"
    train.yy <- yy[(i+1):nn.XX]
    train.xx=XX[1:(nn.XX-i),]
    
    #Remove missing values 
    # all.vars=cbind(train.yy,train.xx)
    # index.present=complete.cases(all.vars)
    index.present <- intersect(which(!is.na(train.yy)), which(!apply(train.xx, 1, anyNA)))
    train.yy=as.numeric(train.yy[index.present])
    train.xx=as.matrix(train.xx[index.present,])	
    
    set.seed(i*j)
    mod.ridge=cv.glmnet(train.xx,train.yy,alpha=0)
    weight = 1/abs(matrix(coef(mod.ridge, s=mod.ridge$lambda.min)[, 1][-1]))^1
    mod.adaptive = cv.glmnet(train.xx, train.yy,  alpha=1, penalty.factor=weight)
    adapt.coef=coef(mod.adaptive, s=mod.adaptive$lambda.1se)
    
    pred.adapt=predict(mod.adaptive,newx=train.xx, s='lambda.min')
    SSE=mean((train.yy-pred.adapt)^2);SSE
    SST=mean((train.yy-mean(train.yy))^2);SST
    Rsqu=1-SSE/SST
    
    RsqAdp_obj.data[j,i]=Rsqu
    
    explain=(1-Rsq.diurnal)*Rsqu
    Rsq.diurnal+explain
    
    train.res2=train.yy-pred.adapt  
    
    #Forecast the residual piece
    current.diurnal.residual=obj.data[curr.index,predict.col]-diurnal.train$coef%*%c(1,obj.data[curr.index,diurnal.1:diurnal.12])
    
    
    # current.xx=t(as.matrix(c(as.numeric(current.diurnal.residual),as.numeric(obj.data[curr.index,-c(time.col, diurnal.1:diurnal.12)]))))
    # colnames(current.xx)
    current.xx<-as.matrix(obj.data[curr.index,])
    
    XX.fore=predict(mod.adaptive,newx=current.xx, s='lambda.1se')
    
    # model_obj.data[j,i]=diurn.fore+XX.fore
    model_obj.data[j,i]=XX.fore
  }
}
                        return(list(list(model_obj.data,
                                         persistence_obj.data,
                                         true_obj.data,
                                         RsqDir_obj.data, 
                                         RsqAdp_obj.data)))


names(lm.results) <- obj.list

for(model in lm.results) {
  rmse.mod=array()
  rmse.per=array()
  
  for(i in 1:15){
    replace=which(model[[1]][,i]<0);print(replace)
    model[[1]][replace,i]=rep(0,length(replace))
    rmse.mod[i]=sqrt(mean((model[[1]][,i]-model[[3]][,i])^2,na.rm=TRUE))
    rmse.per[i]=sqrt(mean((model[[2]][,i]-model[[3]][,i])^2,na.rm=TRUE))
  }
  
  plot(horizon.min, rmse.mod, type="b",pch=19,col=1, xlab="",ylab="", ylim=c(0,1.1))
  lines(horizon.min, rmse.per, type="b",pch=17, col=2)
  title("Model Comparison", xlab="Forecast Horizon (Minutes)", ylab="RMSE", cex.main=1.75, cex.lab=1.4)
  legend(5,1.1, c("Linear Model", "Persistence"), pch=c(19,17), col=c(1,2), lty=c(1,1), bty="n",cex=1.5)
  
  avg.dir=array()
  avg.lin=array()
  avg.tot=array()
  
  for(i in 1:15) {
    avg.dir[i]=mean(model[[4]][,i], na.rm=TRUE)
    
    explain=(1-model[[4]][,i])*model[[5]][,i]
    tot.Rsq=model[[4]][,i]+explain
    
    avg.lin[i]=mean(explain,na.rm=TRUE)
    avg.tot[i]=mean(tot.Rsq,na.rm=TRUE)
  }
  
  plot(horizon.min, avg.dir, type="b",pch=19,col=3, xlab="",ylab="", ylim=c(0,1.1))
  lines(horizon.min, avg.lin, type="b",pch=17,col=4)
  lines(horizon.min, avg.tot, type="b",pch=18,col=2)
  title("Model Component R^2", xlab="Forecast Horizon (Minutes)", ylab="Rsqu", cex.main=1.75, cex.lab=1.4)
  legend(5,0.3, c("Diurnal", "Adaptive", "Total"), pch=c(19,17,18), col=c(3,4,2), lty=c(1,1,1), bty="n",cex=1.5)
}
