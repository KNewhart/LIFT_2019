###########################################
##Forecasting Model Settings
##
###########################################
rm(list=ls())
setwd("C:\\Users\\Kathr\\Dropbox\\Code\\LIFT_2019\\R Code")
# #What are the sizes of the training/testing datasets?

#3 days will be used for training, with a rolling window
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

## HRT Correction
source("src/hrt_correction.R")

#Should hrt adjustment be applied to the data?
hrt_adjust=FALSE
if(hrt_adjust==TRUE){sapply(obj.list, function(x) assign(paste0("hrt.",x), hrt_correction(get(x)), envir = .GlobalEnv))}

#What is the forecast horizon? (given in number of steps & number of minutes )
horizon.min=seq(5,75,by=5)
horizon.steps= horizon.min/5


#Testing ab3_4.0 with 300 second delay
obj.data=ab3_4.0_300

predict.col <- which(colnames(obj.data) == "AB3.Z7.Ammonia.mg.N.L")
diurnal.1=which(colnames(obj.data) == "cos.x")
diurnal.2=which(colnames(obj.data) == "sin.x")
diurnal.3=which(colnames(obj.data) == "cos.2x")
diurnal.4=which(colnames(obj.data) == "sin.2x")


################################################################
#Figures for WEFTEC presentation
################################################################
# for(i in 1:length(horizon.steps)){

i=10

	
	#Obtaining the persistence forecast for each horizon
	#slots says where to put the persistence forecasts in the matrix to save
	slots=1:(testing.obs-i+1)
	# persistence_obj.data[slots,i]=obj.data[(training.obs):(total.obs-i),predict.col]
	# true_obj.data[slots,i]=obj.data[(training.obs+i):total.obs,predict.col]
	
	################################################################
	#Loop over the observations to make predictions, index goes j=1:#testing obs-horizon+1
	################################################################
	# for(j in 1:(testing.obs-i+1)){
		j=1032
		if(j%%100==0){print(paste("j, forecast observation is", j, sep=" "))}

		train.data=obj.data[(j):(j+training.obs-2),]
		
		###############################
		##Diurnal plot of training data
		###############################
		head(train.data)
		tail(train.data)

		min.of.day=rep(seq(0,1435,by=5),3)[-864]
		hr.of.day <- min.of.day/60
		
		# pdf("weftec_figures/diurnal_trend.pdf", width=10, height=5)
		printToPowerPoint(code.block={
		  # plot(min.of.day, as.numeric(train.data[,predict.col]), pch=19, col=rgb(0,0,0,.15),xlab="",ylab="", ylim=c(-1,5))
		  # title("Ammonia Zone 7 by Minute",ylab="Ammonia (mg/L N)", xlab="Minute of the Day",cex.main=2, cex.lab=1.4)
		  plot(hr.of.day, as.numeric(train.data[,predict.col]), pch=19, col=rgb(0,0,0,.15),xlab="",ylab="", ylim=c(0,5), xlim=c(0,24), xaxt="n")
		  title("Ammonia Zone 7",ylab="Ammonia (mg/L N)", xlab="Hour of the Day",cex.main=2, cex.lab=1.4)
		  axis(side=1, at=c(0,4,8,12,16,20,24))
		  # 
		  # text(min.of.day[12]-15,-0.85, "0:00",cex=.9)
		  # text(min.of.day[36]-15,-0.85, "2:00",cex=.9)
		  # text(min.of.day[60]-15,-0.85, "4:00",cex=.9)
		  # text(min.of.day[84]-15,-0.85, "6:00",cex=.9)
		  # text(min.of.day[108]-15,-0.85, "8:00",cex=.9)
		  # text(min.of.day[132]-15,-0.85, "10:00",cex=.9)
		  # text(min.of.day[156]-15,-0.85, "12:00",cex=.9)
		  # 
		  # text(min.of.day[180]-15,-0.85, "14:00",cex=.9)
		  # text(min.of.day[204]-15,-0.85, "16:00",cex=.9)
		  # text(min.of.day[228]-15,-0.85, "18:00",cex=.9)
		  # text(min.of.day[252]-15,-0.85, "20:00",cex=.9)
		  # text(min.of.day[276]-15,-0.85, "22:00",cex=.9)
		  # 
		  # text(750,-.25, "Hour")
		}, presentation.path = "C:\\Users\\Kathr\\Dropbox\\Code\\LIFT_2019\\R Code")
		
		# dev.off()
		
		###############################
		##
		###############################
		
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
		
		
		###############################
		##Fit of diurnal model on training data
		###############################
		# pdf("weftec_figures/diurnal_fit.pdf", width=10, height=5)
		
		printToPowerPoint(code.block={
		  plot(1:863,yy,pch=19,col=rgb(0,0,0,.15),xlab="", ylab="",ylim=c(0,5), xaxt="n", xlim=c(1,875))
		  
		  lines(1:863, fitted(diurnal.train),lwd=2,col=4)
		  title("Fitted Diurnal Model",ylab="Ammonia (mg/L N)", xlab="Date",cex.main=2, cex.lab=1.4)
		  text(75,4.5, "R-squ = 69%", cex=1.25, col=4)
		  
		  position=1:863
		  # text(position[12]-15,-0.85, "0:00",cex=.9)
		  # text(position[84]-15,-0.85, "6:00",cex=.9)
		  # text(position[156]-15,-0.85, "12:00",cex=.9)
		  # text(position[228]-15,-0.85, "18:00",cex=.9)
		  # 
		  # text(position[228 +72]-15,-0.85, "0:00",cex=.9)
		  # text(position[228 +72*2]-15,-0.85, "6:00",cex=.9)
		  # text(position[228 +72*3]-15,-0.85, "12:00",cex=.9)
		  # text(position[228 +72*4]-15,-0.85, "18:00",cex=.9)
		  # 
		  # 
		  # text(position[516 +72]-15,-0.85, "0:00",cex=.9)
		  # text(position[516 +72*2]-15,-0.85, "6:00",cex=.9)
		  # text(position[516 +72*3]-15,-0.85, "12:00",cex=.9)
		  # text(position[516 +72*4]-15,-0.85, "18:00",cex=.9)
		  # 
		  # text(870,-0.85, "0:00",cex=.9)
		  
		  axis(side=1, at=c(12, 228+72, 516+72, 804+72), labels=c("Sat, 18-May-2019", "Sun, 19-May-2019", "Mon, 20-May-2019","Tues, 21-May-2019"))
		  
		  site=240
		  fit.val=as.numeric(fitted(diurnal.train)[site])
		  tru.val=as.numeric(yy[site])
		  lines(c(site, site), c(fit.val,tru.val),lwd=3, col=2)
		  text(150,0,"Model Residual")
		  library(shape)
		  
		  Arrows(x0=150, y0=0.25, x1=225, y1=1, code=2,arr.type = "triangle",col=1)
		}, presentation.path = "C:\\Users\\Kathr\\Dropbox\\Code\\LIFT_2019\\R Code")
		
		
		
		# dev.off()
		
		
		

		###############################
		##Plot of residuals of diurnal model
		###############################
		# pdf("weftec_figures/diurnal_residuals.pdf", width=10, height=5)
  		printToPowerPoint(code.block={
  		plot(1:863, residuals(diurnal.train), type="l",lwd=2, col=2,xlab="", ylab="",ylim=c(-2.5,2.5), xaxt="n", xlim=c(1,875))
  		title("Residuals of Diurnal Model",ylab="Model Residual (mg/L N)", xlab="Date",cex.main=2, cex.lab=1.4)
  		position=1:863
  		text(position[12]-15,-2.5, "0:00",cex=.9)
  		text(position[84]-15,-2.5, "6:00",cex=.9)
  		text(position[156]-15,-2.5, "12:00",cex=.9)
  		text(position[228]-15,-2.5, "18:00",cex=.9)
  	
  		text(position[228 +72]-15,-2.5, "0:00",cex=.9)
  		text(position[228 +72*2]-15,-2.5, "6:00",cex=.9)
  		text(position[228 +72*3]-15,-2.5, "12:00",cex=.9)
  		text(position[228 +72*4]-15,-2.5, "18:00",cex=.9)
  	
  		
  		text(position[516 +72]-15,-2.5, "0:00",cex=.9)
  		text(position[516 +72*2]-15,-2.5, "6:00",cex=.9)
  		text(position[516 +72*3]-15,-2.5, "12:00",cex=.9)
  		text(position[516 +72*4]-15,-2.5, "18:00",cex=.9)
  		
  		text(870,-2.5, "0:00",cex=.9)
  		
  		axis(side=1, at=c(12, 228+72, 516+72, 804+72), labels=c("Sat, 18-May-2019", "Sun, 19-May-2019", "Mon, 20-May-2019","Tues, 21-May-2019"))
  		}, presentation.path = "C:\\Users\\Kathr\\Dropbox\\Code\\LIFT_2019\\R Code")
  		# dev.off()
		
		###############################
		##
		###############################
		
		Rsq.diurnal=summary(diurnal.train)$r.squared  #to save R^2 of diurnal model on training set
		
		#Forecast from diurnal model for step ahead
		diurn.fore=diurnal.train$coef%*%c(1,obj.data[fore.index,diurnal.1:diurnal.4])
	
		#Level 1 residuals
		train.res1=yy-predict(diurnal.train)
	

# }



ALL.COEFS=matrix(0,ncol=18,nrow=1143)


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
		train.yy=train.res1[(i+1):nn.XX]
		colnames(train.yy)[1]="AB3.Z7.Ammonia.mg.N.L-residual"
		train.xx=XX[1:(nn.XX-i),]
		
		#Remove missing values 
		all.vars=cbind(train.yy,train.xx)
		index.present=complete.cases(all.vars)
		train.yy=as.numeric(train.yy[index.present])
		train.xx=as.matrix(train.xx[index.present,])	
	
		set.seed(i*j)
		mod.ridge=cv.glmnet(train.xx,train.yy,alpha=0)
		weight = 1/abs(matrix(coef(mod.ridge, s=mod.ridge$lambda.min)[, 1][-1]))^1
		mod.adaptive = cv.glmnet(train.xx, train.yy,  alpha=1, penalty.factor=weight)
		
		adapt.coef=coef(mod.adaptive, s=mod.adaptive$lambda.1se)
		ALL.COEFS[j,]=abs(as.vector(adapt.coef))
		
		pred.adapt=predict(mod.adaptive,newx=train.xx, s='lambda.1se')
		SSE=mean((train.yy-pred.adapt)^2);SSE
		SST=mean((train.yy-mean(train.yy))^2);SST
		Rsqu=1-SSE/SST
		
		RsqAdp_obj.data[j,i]=Rsqu
		
		explain=(1-Rsq.diurnal)*Rsqu
		Rsq.diurnal+explain
		
		train.res2=train.yy-pred.adapt
	
		#Forecast the residual piece
		current.diurnal.residual=obj.data[curr.index,predict.col]-diurnal.train$coef%*%c(1,obj.data[curr.index,diurnal.1:diurnal.4])
			
		current.xx=t(as.matrix(c(as.numeric(current.diurnal.residual),as.numeric(obj.data[curr.index,-c(1,diurnal.1,diurnal.2,diurnal.3,diurnal.4)]))))
		colnames(current.xx)
		
		XX.fore=predict(mod.adaptive,newx=current.xx, s='lambda.1se')
		
		model_obj.data[j,i]=diurn.fore+XX.fore		
}



med.coef=apply(ALL.COEFS,2,median)
cbind(rownames(adapt.coef),round(med.coef,2))

#Median Coefficient of all fitted linear models for 50-min ahead forecasts 
# "(Intercept)"                      "0.44"
# "Z7 Ammonia mg.N.L-residual"   "0.85"
# "Z6 DO (mg/L)"                   "0.28"
# "Z7 DO (mg/L)"                   "0"   
# "Z8 DO (mg/L)"                   "0.08"
# "Z9 DO (mg/L)"                   "0.16"
# "Z6 Header Flow SCFM"          "0"   
# "Z7 Header Flow SCFM"          "0"   
# "Z8 Header Flow SCFM"          "0"   
# "Z6 Valve Position" "0"   
# "Z7 Valve Position" "0.01"
# "Z8 Valve Position" "0"   
# "Z9 Valve Position" "0.29"
# "Z7 Ammonia (mg/L N)"          "0.01"
# "Z3 Nitrate (mg/L N)"            "0"   
# "Z3 NO2 (mg/L N)"                "0.04"
# "Z9 Nitrate (mg/L N)"            "0"   
# "Z9 NO2 (mg/L N)"                "0.71"



# "Z7 Ammonia Residual (mg/L N)"   	"0.85"
# "Z9 NO2 (mg/L N)"                "0.71"
# "(Intercept)"                      "0.44"
# "Z9 Valve Position" 			"0.29"
# "Z6 DO (mg/L)"                   "0.28"
# "Z9 DO (mg/L)"                   "0.16" 
# "Z8 DO (mg/L)"                   "0.08"
# "Z3 NO2 (mg/L N)"                "0.04" 
# "Z7 Ammonia (mg/L N)"          "0.01"

avg.coefs= c(0.85, 0.71, 0.44, 0.29, 0.28, 0.16, 0.08, 0.04, 0.01)
bar.names=c("Z7 Ammonia Residual (mg/L N)" , "Z9 NO2 (mg/L N)"  ,"(Intercept)"  ,
"Z9 Valve Position","Z6 DO (mg/L)" ,"Z9 DO (mg/L)" ,"Z8 DO (mg/L)" ,"Z3 NO2 (mg/L N)", "Z7 Ammonia (mg/L N)")


barplot(avg.coefs, main="", horiz=TRUE,names.arg=bar.names)


# # Z7 Ammonia Residual (mg/L N)   	0.85
# Z9 NO2 (mg/L N)               0.71
# Intercept                      0.44
# Z9 Valve Position 			0.29
# Z6 DO (mg/L)                   0.28
# Z9 DO (mg/L)                   0.16 
# Z8 DO (mg/L)                  0.08
# Z3 NO2 (mg/L N)               0.04 
# Z7 Ammonia (mg/L N)          0.01

printToPowerPoint(code.block={
  par(mar=c(3,20,2,2))
  barplot(abs(avg.coefs)[order(abs(avg.coefs))], 
          main = "Predictor Importance in Linear Model", 
          col=1:length(avg.coefs),
          cex.names = 1.5,
          cex.main = 1.5,
          cex.axis = 1.25,
          horiz=TRUE,
          las=2, names.arg = rev(bar.names))
}, presentation.path = "C:\\Users\\Kathr\\Dropbox\\Code\\LIFT_2019\\R Code")

