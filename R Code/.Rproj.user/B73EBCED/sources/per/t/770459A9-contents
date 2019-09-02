#### ACF PLOT ###
#What is the forecast horizon? (given in number of steps & number of minutes )
horizon.min=seq(5,75,by=5)
horizon.steps= horizon.min/5
i <- 50/5

#Testing ab3_4.0 with 300 second delay
obj.data=ab3_4.0_300
predict.col <- which(colnames(obj.data) == "AB3.Z7.Ammonia.mg.N.L")
diurnal.1=which(colnames(obj.data) == "cos.x")
diurnal.2=which(colnames(obj.data) == "sin.x")
diurnal.3=which(colnames(obj.data) == "cos.2x")
diurnal.4=which(colnames(obj.data) == "sin.2x")

#Set of things to save through the loop
model_obj.data=matrix(NA,nrow=testing.obs,ncol=length(horizon.min))
persistence_obj.data=matrix(NA,nrow=testing.obs,ncol=length(horizon.min))
true_obj.data=matrix(NA,nrow=testing.obs,ncol=length(horizon.min))

#Obtaining the persistence forecast for horizon
#slots says where to put the persistence forecasts in the matrix to save
slots=1:(testing.obs-i+1)
persistence_obj.data[slots,i]=obj.data[(training.obs):(total.obs-i),predict.col]
true_obj.data[slots,i]=obj.data[(training.obs+i):total.obs,predict.col]

# for(j in 1:(testing.obs-i+1)){
j <- 1000
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

  
  my_pres <- read_pptx(path = "my_pres.pptx")
  my_pres <- my_pres %>%
    add_slide(layout="Only Content", master = "CSM_16x9")
  my_pres <- ph_with_vg(my_pres, code = {
    par(mar=c(6,6,4,1.6), mgp=c(4, 1.5,0))
    acf(as.data.frame(yy), lag.max=1440, xaxt="n", xlab="Days", main="", cex.axis=2, cex.lab=2)
    axis(side=1, at=c(0,1,2,3)*24*60/5, labels = c(0,1,2,3), cex.axis=2)
    title("AB3 Z7 Ammonia", cex.main=2)
  }
  , type="body")
print(my_pres, "my_pres.pptx")


#sum(complete.cases(yy))
diurnal.train=lm(yy~x1+x2+x3+x4, na.action=na.exclude)
#Forecast from diurnal model for step ahead
diurn.fore=diurnal.train$coef%*%c(1,obj.data[fore.index,diurnal.1:diurnal.4])
#Level 1 residuals
train.res1=yy-predict(diurnal.train)


  my_pres <- read_pptx(path = "my_pres.pptx")
  my_pres <- my_pres %>%
    add_slide(layout="Only Content", master = "CSM_16x9")
  my_pres <- ph_with_vg(my_pres, code = {
    par(mar=c(6,6,4,1.6), mgp=c(4, 1.5,0))
    acf(as.data.frame(train.res1), lag.max=1440, xaxt="n", xlab="Days", main="", cex.axis=2, cex.lab=2)
    axis(side=1, at=c(0,1,2,3)*24*60/5, labels = c(0,1,2,3), cex.axis=2)
    title("AB3 Z7 Ammonia", cex.main=2)
  }
  , type="body")
print(my_pres, "my_pres.pptx")

#### R2 PLOT ###

load("RsqDir_obj.data")
load("RsqAdp_obj.data")

for(i in 1:15){
  avg.dir[i]=mean(RsqDir_obj.data[,i], na.rm=TRUE)
  explain=(1-RsqDir_obj.data[,i])*RsqAdp_obj.data[,i]
  tot.Rsq=RsqDir_obj.data[,i]+explain
  
  avg.lin[i]=mean(explain,na.rm=TRUE)
  avg.tot[i]=mean(tot.Rsq,na.rm=TRUE)
}

my_pres <- read_pptx(path = "my_pres.pptx")
my_pres <- my_pres %>% 
  add_slide(layout="Only Content", master = "CSM_16x9")

my_pres <- ph_with_vg(my_pres, code = {
  par(mar=c(6/2,6/2,4/2,1.6/2), mgp=c(4/2, 1.5/2,0), cex=2)
  plot(horizon.min, avg.dir, type="b",pch=19,col=3, xlab="",ylab="", ylim=c(0,1.1), cex.axis=2/2)
  lines(horizon.min, avg.lin, type="b",pch=17,col=4)
  lines(horizon.min, avg.tot, type="b",pch=18,col=2)
  title("Model Component R-squared for 4.0 mg/L - 300 s", xlab="Forecast Horizon (minutes)", ylab="R-squared")
  legend(5,0.4, c("Diurnal", "Adaptive", "Total"), pch=c(19,17,18), col=c(3,4,2), lty=c(1,1,1), bty="n", cex=.9)
}, type="body")
print(my_pres, "my_pres.pptx")

{
  obj.data=ab3_4.0_300[,-14]
  predict.col <- which(colnames(obj.data) == "AB3.Z7.Ammonia.mg.N.L")
  diurnal.1=which(colnames(obj.data) == "cos.x")
  diurnal.2=which(colnames(obj.data) == "sin.x")
  diurnal.3=which(colnames(obj.data) == "cos.2x")
  diurnal.4=which(colnames(obj.data) == "sin.2x")
  i <- 50/5
  j <- 1000
  
  train.data=obj.data[(j):(j+training.obs-2),]
  #Grabbing the columns of the response and cosine/sine terms
  yy=train.data[,predict.col]
  x1=train.data[,diurnal.1]
  x2=train.data[,diurnal.2]
  x3=train.data[,diurnal.3]
  x4=train.data[,diurnal.4]
  
  #sum(complete.cases(yy))
  diurnal.train=lm(yy~x1+x2+x3+x4, na.action=na.exclude)
  
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
  
  coefplot::coefplot(mod.adaptive)
}




my_pres <- read_pptx(path = "my_pres.pptx")
my_pres <- my_pres %>%
  add_slide(layout="Only Content", master = "CSM_16x9")
# ggplot.n <- coefplot(mod.adaptive, lambda=mod.ridge$lambda.min, sort='magnitude')
# my_pres <- ph_with(x=my_pres, value = ggplot.n,location = ph_location_fullsize())
data.values <- as.numeric(coef(mod.adaptive, s=mod.adaptive$lambda.1se)@x)
data.labels <- coef(mod.adaptive)@Dimnames[[1]][coef(mod.adaptive)@i+1]
data.labels <- gsub("...Open", "", data.labels)
data.labels <- gsub("[.]", " ", data.labels)
data.labels <- gsub("AB3 Z7 Ammonia mg N L-residual", "Residual Z7 Ammonia", data.labels)
data.labels <- gsub("AB3 ", "", data.labels)
data.labels <- gsub(" mg N L", "", data.labels)
data.labels <- gsub(" mg L", "", data.labels)
data.labels <- gsub("A BASIN INFLUENT CHANNEL AMMONIUM","Influent Ammonia", data.labels)
data.labels <- gsub("AB INFLUENT FLOW MGD", "Influent Flow", data.labels)
names(data.values) <- data.labels
my_pres <- ph_with_vg(my_pres, code={
par(mar=c(3,20,2,2))
barplot(abs(data.values)[order(abs(data.values))], 
        main = "GLM Coefficient Absolute Value", 
        col=1:length(data.values),
        cex.names = 1.5,
        cex.main = 1.5,
        cex.axis = 1.25,
        horiz=TRUE,
        las=2)
}, type="body")
print(my_pres, "my_pres.pptx")



rain=rainbow(15)
i <- 1
j <- 1:(testing.obs-i+1)
fore.index <- j+training.obs-1+i
time.stamps <- index(obj.data[fore.index])

my_pres <- read_pptx(path = "my_pres.pptx")
my_pres <- my_pres %>% 
  add_slide(layout="Only Content", master = "CSM_16x9")
my_pres <- ph_with_vg(my_pres, code={
par(mar=c(2.5,10,2,4), mgp=c(2.5,1,0))
plot.it <- cbind(1:1152, (1-RsqDir_obj.data[,1])*RsqAdp_obj.data[,1])
plot.it <- plot.it[-which((1-RsqDir_obj.data[,1])*RsqAdp_obj.data[,1] <.01),]
plot(plot.it, type="l",col=rain[1], ylim=c(0.05, .80), xlab="", ylab="", xaxt="n")

labels.at <- which(format(time.stamps, "%H:%M") == "00:00")
labels.are <- sapply(labels.at, function(x) weekdays(time.stamps[x+1], abbreviate=T))
axis(side=1, at = labels.at, labels = labels.are)

for(i in 2:15){
  plot.it <- cbind(1:1152, (1-RsqDir_obj.data[,i])*RsqAdp_obj.data[,i])
  plot.it <- plot.it[-which(plot.it[,2] <.01),]
  lines(plot.it, type="l", col=rain[i])
}

lines(1:1152, RsqDir_obj.data[,1], type="l", col=1, lwd=2)
legend("left", inset = c(-.375,0), c("5 min", "10 min", "15 min", "20 min", "25 min", "30 min", "35 min", "40 min", "45 min", "50 min", "55 min", "60 min", "65 min", "70 min", "75 min", "Diurnal", "Ammonia"), col=c(rain,1,1), lty=c(rep(1,16),2), lwd=2, bty="n", cex=.75)

par(new=T)
plot(x=1:1152, y=as.numeric(obj.data[fore.index, 3]), 
     xaxt="n", yaxt="n", xlab="", ylab="",
     type="l", lty=2, lwd=2)
axis(side=4)
mtext(side=4, line=2.5, "Influent Ammonia")

title(paste(data.type,": ", window.size, " Window, ", "R^2 by Horizon", sep=""), ylab="R squared")
}, type="body")

print(my_pres, "my_pres.pptx")




my_pres <- read_pptx(path = "my_pres.pptx")
my_pres <- my_pres %>% 
  add_slide(layout="Only Content", master = "CSM_16x9")
my_pres <- ph_with_vg(my_pres, code={
  
  par(mar=c(2,4,3,1), mgp=c(2.5,1,0))
  plot(1:1152, true_obj.data[,10],type="l",col=1, ylim=c(0,max(true_obj.data[,15],na.rm=T)), xaxt="n", xlab="", ylab="",lwd=2)
  
  labels.at <- which(format(time.stamps, "%H:%M") == "00:00")
  labels.are <- sapply(labels.at, function(x) weekdays(time.stamps[x+1], abbreviate=T))
  axis(side=1, at = labels.at, labels = labels.are)
  
  lines(1:1152, model_obj.data[,10],type="l",col=3,lwd=2)
  # lines(1:1152, persistence_obj.data[,10],type="l",col=4,lwd=2)
  title(paste(data.type,": ", window.size, " Window, ", "50 Minute Forecasts", sep=""), xlab="Time", ylab="Ammonia Zone 7 (mg/L)")
  # legend("topleft", c("True", "Model", "Persistence"),  col=c(1,2,4), lty=c(1,1,1), lwd=c(2,2,2),bty="n",cex=1.5)
  legend("topleft", c("Actual", "Model"),  col=c(1,3), lty=c(1,1), lwd=c(2,2),bty="n")

}, type="body")

print(my_pres, "my_pres.pptx")



my_pres <- read_pptx(path = "my_pres.pptx")
my_pres <- my_pres %>% 
  add_slide(layout="Only Content", master = "CSM_16x9")
my_pres <- ph_with_vg(my_pres, code={
  plot(1:1152, true_obj.data[,10]-model_obj.data[,10],type="l", xlab="", ylab="",lwd=2, col=2, ylim=c(-1.6,1.6), xaxt="n")
  
  labels.at <- which(format(time.stamps, "%H:%M") == "00:00")
  labels.are <- sapply(labels.at, function(x) weekdays(time.stamps[x+1], abbreviate=T))
  axis(side=1, at = labels.at, labels = labels.are)
  
  lines(1:1152, true_obj.data[,10]-persistence_obj.data[,10],type="l",col=4,lwd=2)
  abline(h=0, col="darkgray", lty=2,lwd=2)
  title(paste(data.type,": ", window.size, " Window, ", "50 Minute Errors", sep=""),  xlab="Time", ylab="Ammonia Errors (mg/L)")
  legend("topleft", c("Model", "Persistence"),  col=c(2,4), lty=c(1,1), lwd=c(2,2),bty="n")
}, type="body")

print(my_pres, "my_pres.pptx")