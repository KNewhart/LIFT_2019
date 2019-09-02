setwd("C:\\Users\\Kate Newhart\\odrive\\Mines\\LIFT\\LIFT 2019 Challenge\\R Code")


rm(list=ls())

library(xts)
library(readxl)

# Load Excel data into R

## Plant Influent & Effluent
LIFT_2019_Inf_Eff <- read_excel("data/LIFT 2019_Process Data_5m Jan - Apr.xlsx", 
                                sheet = "Plant Inf_Eff", col_types = c("date", 
                                                                       "text", "text", "text", "text", "text", 
                                                                       "text", "text", "text", "text"), skip = 3)
colnames(LIFT_2019_Inf_Eff) <- paste(colnames(LIFT_2019_Inf_Eff), LIFT_2019_Inf_Eff[1,])

LIFT_2019_Inf_Eff <- xts(data.frame(LIFT_2019_Inf_Eff[-c(1:2),-1]), order.by = data.frame(LIFT_2019_Inf_Eff[-c(1:2),1])[,1])
# LIFT_2019_Inf_Eff <- LIFT_2019_Inf_Eff[,-which(LIFT_2019_Inf_Eff[1,] == "Bad")] # No bad observations
LIFT_2019_Inf_Eff <- xts(sapply(LIFT_2019_Inf_Eff,as.numeric), order.by = index(LIFT_2019_Inf_Eff))


## Aeration Basin Influnet
LIFT_2019_AB_Inf <- read_excel("data/LIFT 2019_Process Data_5m Jan - Apr.xlsx", 
                               sheet = "AB Inf", col_types = c("date", 
                                                               "text", "text", "text", "text", "text", 
                                                               "text", "text", "text", "text", "text", 
                                                               "text", "text", "text", "text"), skip = 3)
colnames(LIFT_2019_AB_Inf) <- paste(colnames(LIFT_2019_AB_Inf), LIFT_2019_AB_Inf[1,])

LIFT_2019_AB_Inf <- xts(data.frame(LIFT_2019_AB_Inf[-c(1:2),-1]), order.by = data.frame(LIFT_2019_AB_Inf[-c(1:2),1])[,1])
# LIFT_2019_AB_Inf <- LIFT_2019_AB_Inf[,-which(LIFT_2019_AB_Inf[1,] == "Bad")]
LIFT_2019_AB_Inf <- xts(sapply(LIFT_2019_AB_Inf,as.numeric), order.by = index(LIFT_2019_AB_Inf))


## Aeration Basin 1
LIFT_2019_AB1 <- read_excel("data/LIFT 2019_Process Data_5m Jan - Apr.xlsx", 
                            sheet = "A_Basin 1", col_types = c("date", 
                                                               "text", "text", "text", "text", "text", 
                                                               "text", "text", "text", "text", "text", 
                                                               "text", "text", "text", "text", "text", 
                                                               "text", "text", "text", "text", "text", 
                                                               "text", "text", "text", "text", "text", 
                                                               "text", "text", "text", "text", "text", 
                                                               "text", "text", "text", "text", "text", 
                                                               "text", "text", "text", "text", "text", 
                                                               "text", "text", "text"), skip = 3)
colnames(LIFT_2019_AB1) <- paste(colnames(LIFT_2019_AB1), LIFT_2019_AB1[1,])

LIFT_2019_AB1 <- xts(data.frame(LIFT_2019_AB1[-c(1:2),-1]), order.by = data.frame(LIFT_2019_AB1[-c(1:2),1])[,1])
LIFT_2019_AB1 <- LIFT_2019_AB1[,-which(LIFT_2019_AB1[1,] == "Bad")]
LIFT_2019_AB1 <- xts(sapply(LIFT_2019_AB1,as.numeric), order.by = index(LIFT_2019_AB1))


## Aeration Basin 2
LIFT_2019_AB2 <- read_excel("data/LIFT 2019_Process Data_5m Jan - Apr.xlsx", 
                            sheet = "A_Basin 2", col_types = c("date", 
                                                               "text", "text", "text", "text", "text", 
                                                               "text", "text", "text", "text", "text", 
                                                               "text", "text", "text", "text", "text", 
                                                               "text", "text", "text", "text", "text", 
                                                               "text", "text", "text", "text", "text", 
                                                               "text", "text", "text", "text", "text", 
                                                               "text", "text", "text", "text", "text", 
                                                               "text", "text", "text"), skip = 3)
colnames(LIFT_2019_AB2) <- paste(colnames(LIFT_2019_AB2), LIFT_2019_AB2[1,])

LIFT_2019_AB2 <- xts(data.frame(LIFT_2019_AB2[-c(1:2),-1]), order.by = data.frame(LIFT_2019_AB2[-c(1:2),1])[,1])
LIFT_2019_AB2 <- LIFT_2019_AB2[,-which(LIFT_2019_AB2[1,] == "Bad")]
LIFT_2019_AB2 <- xts(sapply(LIFT_2019_AB2,as.numeric), order.by = index(LIFT_2019_AB2))


## Aeration Basin 3
LIFT_2019_AB3 <- read_excel("data/LIFT 2019_Process Data_5m Jan - Apr.xlsx", 
                            sheet = "A_Basin 3", col_types = c("date", 
                                                               "text", "text", "text", "text", "text", 
                                                               "text", "text", "text", "text", "text", 
                                                               "text", "text", "text", "text", "text", 
                                                               "text", "text", "text", "text", "text", 
                                                               "text", "text", "text", "text", "text", 
                                                               "text", "text", "text", "text", "text", 
                                                               "text", "text", "text", "text", "text", 
                                                               "text", "text", "text"), skip = 3)
colnames(LIFT_2019_AB3) <- paste(colnames(LIFT_2019_AB3), LIFT_2019_AB3[1,])

LIFT_2019_AB3 <- xts(data.frame(LIFT_2019_AB3[-c(1:2),-1]), order.by = data.frame(LIFT_2019_AB3[-c(1:2),1])[,1])
LIFT_2019_AB3 <- LIFT_2019_AB3[,-which(LIFT_2019_AB3[1,] == "Bad")]
LIFT_2019_AB3 <- xts(sapply(LIFT_2019_AB3,as.numeric), order.by = index(LIFT_2019_AB3))


## Solids Contact Basin
LIFT_2019_Solids_Contact <- read_excel("data/LIFT 2019_Process Data_5m Jan - Apr.xlsx", 
                                       sheet = "Solids Contact", col_types = c("date", 
                                                                               "text", "text", "text", "text", "text", 
                                                                               "text", "text", "text", "text", "text", 
                                                                               "text", "text", "text", "text", "text", 
                                                                               "text", "text", "text", "text", "text", 
                                                                               "text", "text", "text", "text", "text", 
                                                                               "text", "text", "text", "text"), skip = 3)
colnames(LIFT_2019_Solids_Contact) <- paste(colnames(LIFT_2019_Solids_Contact), LIFT_2019_Solids_Contact[1,])
LIFT_2019_Solids_Contact <- na.omit(LIFT_2019_Solids_Contact[-c(1:2),])

LIFT_2019_Solids_Contact <- xts(data.frame(LIFT_2019_Solids_Contact[,-1]), order.by = na.omit(data.frame(LIFT_2019_Solids_Contact[,1])[,1]))
LIFT_2019_Solids_Contact <- LIFT_2019_Solids_Contact[,-which(LIFT_2019_Solids_Contact[1,] == "Bad")]
LIFT_2019_Solids_Contact <- xts(sapply(LIFT_2019_Solids_Contact,as.numeric), order.by = index(LIFT_2019_Solids_Contact))


## PAD Centrate
LIFT_2019_PAD_Centrate <- read_excel("data/LIFT 2019_Process Data_5m Jan - Apr.xlsx", 
                                     sheet = "PAD_Centrate", col_types = c("date", 
                                                                           "text", "text", "text", "text", "text", 
                                                                           "text", "text"), skip = 3)
colnames(LIFT_2019_PAD_Centrate) <- paste(colnames(LIFT_2019_PAD_Centrate), LIFT_2019_PAD_Centrate[1,])

LIFT_2019_PAD_Centrate <- xts(data.frame(LIFT_2019_PAD_Centrate[-c(1:2),-1]), order.by = data.frame(LIFT_2019_PAD_Centrate[-c(1:2),1])[,1])
# LIFT_2019_PAD_Centrate <- LIFT_2019_PAD_Centrate[,-which(LIFT_2019_PAD_Centrate[1,] == "Bad")]
LIFT_2019_PAD_Centrate <- xts(sapply(LIFT_2019_PAD_Centrate,as.numeric), order.by = index(LIFT_2019_PAD_Centrate))


# Modeling
## Linear model
all.data <- merge(LIFT_2019_AB3, LIFT_2019_AB_Inf)
predict.cols <- "AB3.Z7.Ammonia.mg.N.L"
predict.cols <- which(colnames(all.data) == "AB3.Z7.Ammonia.mg.N.L")
cols2remove <- c(#"AB3.Z8.Header.Flow.SCFM", # Pr(>t) = 0.82  
                 #"AB3.Z8.Calculated.Header.Flow.SCFM",# Pr(>t) = 0.99
                 #"AB3.Z9.Header.Flow.SCFM", # Pr(>t) = .45
                 #"AB3.Z9.Nitrate.mg.N.L", # Pr(>t) = NA MOSTLY ZERO VALUES
                 #"AB3.IMLR.PUMP.3.RUNNING..F_CV..on.off", # 0.61
                 #"AB3.MIXER.4.RUNNING..F_CV..on.off", # .25
                 "A.BASIN.INFLUENT.TOC.SIGNAL.FROM.SPS.NA", # NA
                 "ABASIN.INFLUENT.CHANNEL.MIXER.2.RUNNING..F_CV..on.off" # NA
) 
cols2remove <- sapply(cols2remove, function(x) which(colnames(all.data) == x))
cols2remove <- c(cols2remove, grep("MIXER", colnames(all.data)))
cols2remove <- c(cols2remove, grep("IMLR", colnames(all.data)))

fmla <- as.formula(paste0(colnames(all.data)[predict.cols],"~", paste(colnames(all.data)[-c(predict.cols, cols2remove)], collapse= "+")))
lm.1 <- lm(fmla, all.data)
lm.predict <- predict(lm.1, all.data, interval="confidence") 

#Scaling data to have mean zero and unit variance
complete.index=complete.cases(all.data)
z.data=scale(all.data[complete.index,])
cov(z.data)[1:5,1:5]
round(apply(z.data,2, mean,na.rm=T),3)


library(glmnet)
yy=as.matrix(z.data[,predict.cols])
xx=as.matrix(z.data[,-c(predict.cols,as.numeric(cols2remove))])

#Building cosine and sine predictors of timestamp

dates=index(all.data)[complete.index]
min=as.numeric(substr(dates, 15,16))
hour=as.numeric(substr(dates,12,13))
head(min)
tail(min)
head(hour)
tail(hour)
unique(hour)

an.min=(min*360/60)*pi/180
an.hour=(hour*360/24)*pi/180

diurnal.mod=lm(yy~cos(an.hour)+sin(an.hour)+cos(2*an.hour)+sin(2*an.hour))
summary(diurnal.mod)

plot(cos(an.rad),yy)
acf(yy, lag.max=1000)
acf(resid(diurnal.mod),lag.max=1440)
pacf(resid(diurnal.mod),lag.max=1440)




xx=cbind(xx,cos(an.hour),sin(an.hour), cos(2*an.hour), sin(2*an.hour))
colnames(xx)
colnames(xx)[46]="cosine.term"
colnames(xx)[47]="sine.term"
colnames(xx)[48]="cosine2.term"
colnames(xx)[49]="sine2.term"


set.seed(77)
mod.ridge=cv.glmnet(xx,yy,alpha=0)

w3 = 1/abs(matrix(coef(mod.ridge, s=mod.ridge$lambda.min)[, 1][-1]))^1

set.seed(999)
mod.adaptive = cv.glmnet(xx, yy,  alpha=1, penalty.factor=w3)
plot(mod.adaptive)

rain=rainbow(ncol(xx))
plot(mod.adaptive$glmnet.fit, xvar="lambda", label=TRUE,col=rain)
abline(v = log(mod.adaptive$lambda.min))
abline(v = log(mod.adaptive$lambda.1se))
abline(v = log(6*mod.adaptive$lambda.1se),col=2,lwd=2)
abline(v = 0,col=2,lwd=2)



coef(mod.adaptive, s=mod.adaptive$lambda.1se)
coef(mod.adaptive, s=6*mod.adaptive$lambda.1se)
coef(mod.adaptive, s=1)


coef = coef(mod.adaptive, s='lambda.1se')
coef=coef(mod.adaptive, s=6*mod.adaptive$lambda.1se)
coef=coef(mod.adaptive, s=1)
selected_attributes = (coef@i[-1]) 
colnames(xx)[selected_attributes]



predictions=predict(mod.adaptive,newx=xx, s=6*mod.adaptive$lambda.1se)
predictions=predict(mod.adaptive,newx=xx, s='lambda.1se')
predictions=predict(mod.adaptive,newx=xx, s=1)


SSE=mean((yy-predictions)^2);SSE
SST=mean((yy-mean(yy))^2);SST
Rsqu=1-SSE/SST
Rsqu

residuals=yy-predictions
plot(yy,residuals)
acf(residuals, lag.max=1440)
pacf(residuals, lag.max=1440)
plot(yy,xx[,(selected_attributes-1)[1]])
plot(yy,xx[,(selected_attributes-1)[2]])
plot(yy,xx[,(selected_attributes-1)[3]]) #nonlinear
plot(yy,xx[,(selected_attributes-1)[4]])




set.seed(99)
random.subset=sample(1:nrow(yy), 1000)
pairs(cbind(yy[random.subset],xx[random.subset,selected_attributes-1]))







## Plot prediction
data1 <- as.numeric(na.omit(all.data)[,predict.cols])
data2 <- as.numeric(lm.1$fitted.values)
max.val <- max(c(data1, data2))
min.val <- min(c(data1, data2))
data2plot <- cbind(data1, data2)

# png(paste0("images/",colnames(all.data)[predict.cols],"_lm.png"), units="in", res=2000, width = 5, height = 4)
par(mar=c(3,3,2,8), mgp=c(1.75,.5,0))
plot(x = data2plot[,1], y = data2plot[,2]
     , xlim=c(min.val,max.val), ylim=c(min.val,max.val)
     , xlab="Actal", ylab="Predicted"
     , pch=20
     , main = stringr::str_replace_all(colnames(all.data)[predict.cols], c("[.]" = " "))
)
abline(a=0,b=1,col="blue", lwd=2)

legend("bottomright", 
       # inset = c(-.55,-.23),
       legend = c("Observation", "Perfect Fit"
                  , paste0("R-sq = ",round(summary.lm(lm.1)$r.squared,2))
                  ),
       col = c("black", "blue", NA),
       pch = c(20,NA, NA),
       lwd = c(NA,2, NA),
       bty = "n",
       xpd=NA,
       xjust = 1,
       text.font = 2)

text(do.call(paste0, as.list(paste0(stringr::str_replace_all(colnames(all.data)[-c(predict.cols,cols2remove)], c("[.][.]" = " ", "[.]" = " ")), "\n"))), x = max.val+max.val*.05, y = max.val-min.val-max.val*.75, xpd=NA, pos=4, cex=.7)
# dev.off()


#Some additional plotting of AB3.Z7.Ammonia.mg.N.L
dim(all.data)
head(all.data)


hour.col=NULL
ammonia.by.hour=NULL
ammonia.mean=NULL
for(i in 1:24){
	test1=all.data[.indexhour(all.data) %in% (i-1) & .indexmin(all.data) %in% c(0:60)]
	nhours=length(test1[,14])
	hour.col=c(hour.col,rep(i,nhours))
	ammonia.by.hour=c(ammonia.by.hour,test1[,14])
	ammonia.mean[i]=mean(test1[,14],na.rm=TRUE)
}


boxplot(ammonia.by.hour~(hour.col))
title("AB3 Z7 Ammonia (mg N/L)", xlab="Hour of the Day", cex.main=1.75,cex.lab=1.5)
points(1:24, ammonia.mean, pch=17, col=2)

acf(all.data[,14], na.action=na.pass, lag.max=1440, main="AB3 Z7 Ammonia (mg N/L)")


