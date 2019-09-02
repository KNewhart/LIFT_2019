###########################################
##Forecasting Model Settings
##
###########################################
rm(list=ls())
load("data/ab3_objects.RData")
# #What are the sizes of the training/testing datasets?

#7 days will be used for training, with a rolling window
#This makes 2016 observations for training
training.days=3
training.obs=training.days*24*60/5 #288*7 = 2016

testing.days=4
testing.obs=testing.days*24*60/5 #288*4 = 1152

total.obs=training.obs+testing.obs


# #######################################################################################
# ##Labels for saving window size and data type being run in file names at the end
data.type="AB3-4-300"
window.size="3day"
#######################################################################################

##Sourcing in the data
source("01_preliminaries.R")

# ## HRT Correction
# source("src/hrt_correction.R")
# 
# #Should hrt adjustment be applied to the data?
# hrt_adjust=FALSE
# if(hrt_adjust==TRUE){sapply(obj.list, function(x) assign(paste0("hrt.",x), hrt_correction(get(x)), envir = .GlobalEnv))}

#What is the forecast horizon? (given in number of steps & number of minutes )
horizon.min=seq(5,75,by=5)
horizon.steps= horizon.min/5


#Testing ab3_4.0 with 300 second delay
obj.data=ab3_4.0_300[,-14]
predict.col <- which(colnames(obj.data) == "AB3.Z7.Ammonia.mg.N.L")
diurnal.1=which(colnames(obj.data) == "cos.x")
diurnal.2=which(colnames(obj.data) == "sin.x")
diurnal.3=which(colnames(obj.data) == "cos.2x")
diurnal.4=which(colnames(obj.data) == "sin.2x")


#Set of things to save through the loop
model_obj.data=matrix(NA,nrow=testing.obs,ncol=length(horizon.min))
persistence_obj.data=matrix(NA,nrow=testing.obs,ncol=length(horizon.min))
true_obj.data=matrix(NA,nrow=testing.obs,ncol=length(horizon.min))

RsqDir_obj.data=matrix(NA,nrow=testing.obs,ncol=length(horizon.min))
RsqAdp_obj.data=matrix(NA,nrow=testing.obs,ncol=length(horizon.min))


################################################################
#Loop over the forecast horizon, index goes i=1,2,3,...,14,15
################################################################
for(i in 1:length(horizon.steps)){
	print(paste("i, horizon step is", i, sep=" "))
	
	#Obtaining the persistence forecast for each horizon
	#slots says where to put the persistence forecasts in the matrix to save
	slots=1:(testing.obs-i+1)
	persistence_obj.data[slots,i]=obj.data[(training.obs):(total.obs-i),predict.col]
	true_obj.data[slots,i]=obj.data[(training.obs+i):total.obs,predict.col]
	
	################################################################
	#Loop over the observations to make predictions, index goes j=1:#testing obs-horizon+1
	################################################################
	for(j in 1:(testing.obs-i+1)){
		if(j%%100==0){print(paste("j, forecast observation is", j, sep=" "))}

		train.data=obj.data[(j):(j+training.obs-2),]
		
		#Index of current observation
		curr.index=j+training.obs-1
		
		#Index of observation to forecast
		fore.index=j+training.obs-1+i
		
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
		
		#sum(complete.cases(yy))
		diurnal.train=lm(yy~x1+x2+x3+x4, na.action=na.exclude)
		Rsq.diurnal=summary(diurnal.train)$r.squared  #to save R^2 of diurnal model on training set
		RsqDir_obj.data[j,i]=Rsq.diurnal
		
		#Forecast from diurnal model for step ahead
		diurn.fore=diurnal.train$coef%*%c(1,obj.data[fore.index,diurnal.1:diurnal.4])
	
		#Level 1 residuals
		train.res1=yy-predict(diurnal.train)
	
		###########################
		##MODELING STEP #2
		##Fit residuals of diurnal model with lagged values of covariates and lagged value of residual of response
		###########################
		
		#Creating the predictor matrix, removing timestamp and cosine/sine terms
		XX=cbind(train.res1,train.data[,-c(1,diurnal.1,diurnal.2,diurnal.3,diurnal.4)])
		colnames(XX)[1]="AB3.Z7.Ammonia.mg.N.L-residual"
		nn.XX=dim(XX)[1]
		
		#Lag covariates
		train.yy=train.res1[(i+1):nn.XX] # Use in NN model
		colnames(train.yy)[1]="AB3.Z7.Ammonia.mg.N.L-residual"
		train.xx=XX[1:(nn.XX-i),] # Use in NN model or just XX, linear relationship is already accounted for
		
		#Remove missing values 
		all.vars=cbind(train.yy,train.xx)
		index.present=complete.cases(all.vars)
		train.yy=as.numeric(train.yy[index.present])
		train.xx=as.matrix(train.xx[index.present,])	
	
		set.seed(i*j)
		mod.ridge=cv.glmnet(train.xx,train.yy,alpha=0) # Strips time info glmnet
		weight = 1/abs(matrix(coef(mod.ridge, s=mod.ridge$lambda.min)[, 1][-1]))^1
		mod.adaptive = cv.glmnet(train.xx, train.yy,  alpha=1, penalty.factor=weight)
		
		# plot(mod.adaptive)
		# rain=rainbow(ncol(train.xx))
		# plot(mod.adaptive$glmnet.fit, xvar="lambda", label=TRUE,col=rain)
		# abline(v=mod.adaptive$lambda.1se)
		
		adapt.coef=coef(mod.adaptive, s=mod.adaptive$lambda.1se)
		#adapt.coef
		#selected_attributes = (adapt.coef@i[-1]+1) 
		#selected_attributes
		#length(selected_attributes)
		
		pred.adapt=predict(mod.adaptive,newx=train.xx, s='lambda.1se')
		SSE=mean((train.yy-pred.adapt)^2);SSE
		SST=mean((train.yy-mean(train.yy))^2);SST
		Rsqu=1-SSE/SST
		
		RsqAdp_obj.data[j,i]=Rsqu
		
		explain=(1-Rsq.diurnal)*Rsqu
		Rsq.diurnal+explain
		
		train.res2=train.yy-pred.adapt
		#plot(train.yy,train.res2)
		#acf(train.res2, lag.max=1440)
		#pacf(train.res2, lag.max=1440)
		
		
		#Forecast the residual piece
		current.diurnal.residual=obj.data[curr.index,predict.col]-diurnal.train$coef%*%c(1,obj.data[curr.index,diurnal.1:diurnal.4])
		
		
		current.xx=t(as.matrix(c(as.numeric(current.diurnal.residual),as.numeric(obj.data[curr.index,-c(1,diurnal.1,diurnal.2,diurnal.3,diurnal.4)]))))
		colnames(current.xx)
		
		XX.fore=predict(mod.adaptive,newx=current.xx, s='lambda.1se')
		
		model_obj.data[j,i]=diurn.fore+XX.fore

		###########################
		##MODELING STEP #3
		###########################
		#Fit the residuals of the adaptive lasso with a neural network
		#This may not be necessary if the predictions are very good.
		#Come back later after testing the other parts of the model
		
				
	}
}


rmse.mod=array()
rmse.per=array()

for(i in 1:15){
	replace=which(model_obj.data[,i]<0);print(replace)
	model_obj.data[replace,i]=rep(0,length(replace))
	rmse.mod[i]=sqrt(mean((model_obj.data[,i]-true_obj.data[,i])^2,na.rm=TRUE))
	rmse.per[i]=sqrt(mean((persistence_obj.data[,i]-true_obj.data[,i])^2,na.rm=TRUE))
	
}


pdf(file=paste("figures/", data.type,"_dataset_RMSEs_",window.size,".pdf", sep=""), width=8, height=5)
plot(horizon.min, rmse.mod, type="b",pch=19,col=1, xlab="",ylab="", ylim=c(0,1.1))
lines(horizon.min, rmse.per, type="b",pch=17, col=2)
title(paste(data.type,": ", window.size, " Window ", sep=""), xlab="Forecast Horizon (Minutes)", ylab="RMSE", cex.main=1.75, cex.lab=1.4)
legend(5,1.1, c("Linear Model", "Persistence"), pch=c(19,17), col=c(1,2), lty=c(1,1), bty="n",cex=1.5)
dev.off()

print("Model RMSEs")
print(round(rmse.mod,2))

print("Persistence RMSEs")
print(round(rmse.per,2))

avg.dir=array()
avg.lin=array()
avg.tot=array()

for(i in 1:15){

	avg.dir[i]=mean(RsqDir_obj.data[,i], na.rm=TRUE)
	
	explain=(1-RsqDir_obj.data[,i])*RsqAdp_obj.data[,i]
	tot.Rsq=RsqDir_obj.data[,i]+explain
		
	avg.lin[i]=mean(explain,na.rm=TRUE)
	avg.tot[i]=mean(tot.Rsq,na.rm=TRUE)
	
}

pdf(file=paste("figures/", data.type,"_dataset_avg_R2_",window.size,".pdf", sep=""), width=8, height=5)
plot(horizon.min, avg.dir, type="b",pch=19,col=3, xlab="",ylab="", ylim=c(0,1.1))
lines(horizon.min, avg.lin, type="b",pch=17,col=4)
lines(horizon.min, avg.tot, type="b",pch=18,col=2)
title(paste(data.type,": ", window.size, " Window, ", "Model Component R^2", sep=""), xlab="Forecast Horizon (Minutes)", ylab="Rsqu", cex.main=1.75, cex.lab=1.4)
legend(5,0.35, c("Diurnal", "Adaptive", "Total"), pch=c(19,17,18), col=c(3,4,2), lty=c(1,1,1), bty="n",cex=1.5)
dev.off()



pdf(file=paste("figures/", data.type,"_dataset_indiv_R2_",window.size,".pdf", sep=""), width=11, height=5)
rain=rainbow(15)
plot(1:1152, (1-RsqDir_obj.data[,1])*RsqAdp_obj.data[,1], type="l",col=rain[1], ylim=c(0.05, .80), xlab="", ylab="")
for(i in 2:15){
	lines(1:1152, (1-RsqDir_obj.data[,i])*RsqAdp_obj.data[,i], type="l", col=rain[i])
}
title(paste(data.type,": ", window.size, " Window, ", "R^2 by Horizon", sep=""), cex.main="1.75", xlab="Time", ylab="Rsqu", cex.lab=1.4)
legend(0,0.625, c("5 min", "10 min", "15 min", "20 min", "25 min", "30 min", "35 min", "40 min", "45 min", "50 min", "55 min", "60 min", "65 min", "70 min", "75 min", "Diurnal"), col=c(rain,1), lty=rep(1,16), bty="n", cex=.5)

lines(1:1152, RsqDir_obj.data[,1], type="l", col=1, lwd=2)

dev.off()

plotting.objects <- c("true_obj.data",
                      "model_obj.data",
                      "persistence_obj.data")
save(plotting.objects, file="data/plotting_objects.RData")
pdf(file=paste("figures/", data.type,"_dataset_75min_forecasts_",window.size,".pdf", sep=""), width=11, height=5)
plot(1:1152, true_obj.data[,15],type="l",col=1, ylim=c(0,max(true_obj.data[,15],na.rm=T)), xlab="", ylab="",lwd=2)
lines(1:1152, model_obj.data[,15],type="l",col=2,lwd=2)
lines(1:1152, persistence_obj.data[,15],type="l",col=4,lwd=2)
title(paste(data.type,": ", window.size, " Window, ", "75 Minute Forecasts", sep=""), cex.main="1.75", xlab="Time", ylab="Ammonia Zone 7 (mg/L)", cex.lab=1.4)
legend(100,5, c("True", "Model", "Persistence"),  col=c(1,2,4), lty=c(1,1,1), lwd=c(2,2,2),bty="n",cex=1.5)
dev.off()



pdf(file=paste("figures/", data.type,"_dataset_75min_forecast_errors_",window.size,".pdf", sep=""), width=11, height=5)
plot(1:1152, true_obj.data[,15]-model_obj.data[,15],type="l", xlab="", ylab="",lwd=2, col=2, ylim=c(-2.1,2.1))
lines(1:1152, true_obj.data[,15]-persistence_obj.data[,15],type="l",col=4,lwd=2)
abline(h=0, col="darkgray", lty=2,lwd=2)
title(paste(data.type,": ", window.size, " Window, ", "75 Minute Errors", sep=""), cex.main="1.75", xlab="Time", ylab="Ammonia Errors (mg/L)", cex.lab=1.4)
legend(-50,2.5, c("Model Errors", "Persistence Errors"),  col=c(2,4), lty=c(1,1), lwd=c(2,2),bty="n",cex=1.5)
dev.off()

pdf(file=paste("figures/", data.type,"_dataset_75min_error_histogram_",window.size,".pdf", sep=""), width=11, height=5)
par(mfrow=c(1,2))
hist(true_obj.data[,15]-model_obj.data[,15],breaks=15, xlab="", freq=F, main="",xlim=c(-2.1,2.1), col=2)
title("Model Errors",cex.main=1.75)
hist(true_obj.data[,15]-persistence_obj.data[,15],breaks=15, xlab="", freq=F,main="",xlim=c(-2.1,2.1), col=4)
title("Persistence Errors",cex.main=1.75)
dev.off()



pdf(file=paste("figures/", data.type,"_dataset_50min_forecasts_",window.size,".pdf", sep=""), width=11, height=5)
plot(1:1152, true_obj.data[,10],type="l",col=1, ylim=c(0,max(true_obj.data[,15],na.rm=T)), xlab="", ylab="",lwd=2)
lines(1:1152, model_obj.data[,10],type="l",col=2,lwd=2)
lines(1:1152, persistence_obj.data[,10],type="l",col=4,lwd=2)
title(paste(data.type,": ", window.size, " Window, ", "50 Minute Forecasts", sep=""), cex.main="1.75", xlab="Time", ylab="Ammonia Zone 7 (mg/L)", cex.lab=1.4)
legend(100,5, c("True", "Model", "Persistence"),  col=c(1,2,4), lty=c(1,1,1), lwd=c(2,2,2),bty="n",cex=1.5)
dev.off()



pdf(file=paste("figures/", data.type,"_dataset_50min_forecast_errors_",window.size,".pdf", sep=""), width=11, height=5)
plot(1:1152, true_obj.data[,10]-model_obj.data[,10],type="l", xlab="", ylab="",lwd=2, col=2, ylim=c(-1.6,1.6))
lines(1:1152, true_obj.data[,10]-persistence_obj.data[,10],type="l",col=4,lwd=2)
abline(h=0, col="darkgray", lty=2,lwd=2)
title(paste(data.type,": ", window.size, " Window, ", "50 Minute Errors", sep=""), cex.main="1.75", xlab="Time", ylab="Ammonia Errors (mg/L)", cex.lab=1.4)
legend(-50,1.5, c("Model Errors", "Persistence Errors"),  col=c(2,4), lty=c(1,1), lwd=c(2,2),bty="n",cex=1.5)
dev.off()

pdf(file=paste("figures/", data.type,"_dataset_75min_error_histogram_",window.size,".pdf", sep=""), width=11, height=5)
par(mfrow=c(1,2))
hist(true_obj.data[,15]-model_obj.data[,15],breaks=15, xlab="", freq=F, main="",xlim=c(-1.6,1.6), col=2)
title("Model Errors",cex.main=1.75)
hist(true_obj.data[,15]-persistence_obj.data[,15],breaks=15, xlab="", freq=F,main="",xlim=c(-1.6,1.6), col=4)
title("Persistence Errors",cex.main=1.75)
dev.off()




to.save=list()
to.save[[1]]=model_obj.data
to.save[[2]]=persistence_obj.data
to.save[[3]]=true_obj.data
to.save[[4]]=RsqDir_obj.data
to.save[[5]]=RsqAdp_obj.data

names(to.save)=c("Model-Forecasts", "Persistence-Forecasts","True-Values","R2-diurnal", "R2-linear")

save(to.save, file=paste("forecasts/", data.type, "_window_", window.size, ".Rdata", sep=""))


