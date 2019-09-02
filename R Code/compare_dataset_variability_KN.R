rm(list=ls())

library(xts)
library(readxl)
# 
# import_data <- "do"
# source("src/import_data.R")
# 
# import_data <- "3.5 mg/L 90"
# source("src/import_data.R")
# 
# import_data <- "4.0 mg/L 90"
# source("src/import_data.R")
# 
# import_data <- "4.0 mg/L 300"
# source("src/import_data.R")
# 
# save("all_do", "all_3.5", "all_4.0", "all_4.0_300",
#    "ab3_do", "ab3_3.5", "ab3_4.0", "ab3_4.0_300", file ="data/ab3_objects.RData")
load("data/ab3_objects.RData")
obj.list <- c("ab3_do", "ab3_3.5", "ab3_4.0", "ab3_4.0_300")

# Homogenize length of datasets
n.min <- min(sapply(obj.list, function(x) nrow(na.omit(get(x)))))
for (data in obj.list) {
  shorter <- get(data)
  shorter <- na.omit(shorter)
  assign(data, shorter[(nrow(shorter)-n.min):nrow(shorter),])
}

# # Boxplots
# for(data in obj.list) {
#   par(mar=c(12,3,2,3))
#   boxplot(data.frame(get(data)), 
#           main = paste(data),
#           xlab = "",
#           xaxt = "n", 
#           space=1)
#   end_point = ncol(get(data))
#   text(seq(1,end_point,by=1), par("usr")[3], 
#        srt = 60, adj= 1, xpd = TRUE,
#        labels = paste(colnames(get(data))), cex=0.65)
#   
#   
#   par(mar=c(12,3,2,3))
#   boxplot(scale(data.frame(get(data))), 
#           main = paste("Scaled",data),
#           xlab = "",
#           xaxt = "n", 
#           space=1)
#   end_point = ncol(get(data))
#   text(seq(1,end_point,by=1), par("usr")[3], 
#        srt = 60, adj= 1, xpd = TRUE,
#        labels = paste(colnames(get(data))), cex=0.65)
# }
# 


# Calculate total sample variation
TSV.results <- vector()
GSV.results <- vector()
for(data in obj.list) {
  raw.data <- get(data)
  # if(length(which(apply(raw.data,2,function(x) length(unique(x))) == 1)) > 0) raw.data <- raw.data[,-which(apply(raw.data,2,function(x) length(unique(x))) == 1)]

  TSV <- sum(diag(cov(raw.data)))
  print(paste("TSV of raw",data,round(TSV,2)))
  
  GSV <- det(cov(raw.data))
  print(paste("GSV of raw",data,GSV))
  
  TSV.results <- c(TSV.results, TSV)
  GSV.results <- c(GSV.results, GSV)
}
names(TSV.results) <- c("DO", "3.5 mg/l - 90 s", "4.0 mg/L - 90 s", "4.0 mg/L - 300 s")

compare.set.variability=function(dataset1, dataset2, type, BB, title = NULL){
  #type can be one of "greater" or "not.equal"
  
  #Observed statistics
  diff.TSV.obs=sum(diag(cov(dataset1)))-sum(diag(cov(dataset2)))	
  ratio.TSV.obs=sum(diag(cov(dataset1)))/sum(diag(cov(dataset2)))	
  
  #Combinining all of the data into one set
  all.data=rbind(dataset1,dataset2)
  nn1=dim(dataset1)[1]
  nn2=dim(dataset2)[1]
  nn.total=nn1+nn2
  
  
  #Resampling
  diff.TSV.permute=array()
  ratio.TSV.permute=array()
  
  for(i in 1:BB){
    first.set=sample(1:nn.total, nn1, replace=FALSE)
    second.set=setdiff(1:nn.total, first.set)
    permute.set1=all.data[first.set,]
    permute.set2=all.data[second.set,]
    
    diff.TSV.permute[i]=sum(diag(cov(permute.set1)))-sum(diag(cov(permute.set2)))			
    ratio.TSV.permute[i]=sum(diag(cov(permute.set1)))/sum(diag(cov(permute.set2)))	
    
  }
  
  if(type=="greater"){
    
    pval.TSV.diff=length(which(diff.TSV.permute>(diff.TSV.obs)))/BB
    pval.TSV.ratio=length(which(ratio.TSV.permute>(ratio.TSV.obs)))/BB
    
  }
  # if(type=="not.equal"){
  # pval.GSV=length(which(diff.GSV.permute>abs(diff.GSV.obs)) && which(diff.GSV.permute < -abs(diff.GSV.obs)))/BB
  # pval.TSV=length(which(diff.TSV.permute>abs(diff.TSV.obs)) && which(diff.TSV.permute < -abs(diff.TSV.obs)))/BB
  # }
  
  par(mfrow=c(1,2), mar=c(2.5,2.5,2.5,0.5))
  if(!is.null(title)) par(oma=c(0,0,2,0))
  
  diff.range <- range(diff.TSV.permute)
  if(diff.TSV.obs > diff.range[2]) diff.range[2] <- diff.TSV.obs
  if(diff.TSV.obs < diff.range[1]) diff.range[1] <- diff.TSV.obs
  hist(diff.TSV.permute, breaks="FD", xlab="", ylab="", freq=F, 
       main=paste0("Difference of TSV\nObserved difference ",round(diff.TSV.obs,0)),
       xlim=diff.range)
  points(x=diff.TSV.obs, y = 0, col=2, pch=20)
  # abline(v=diff.TSV.obs, col=2, lwd=2)
  print(c(diff.TSV.obs))
  
  ratio.range <- range(ratio.TSV.permute)
  if(ratio.TSV.obs > ratio.range[2]) ratio.range[2] <- ratio.TSV.obs
  if(ratio.TSV.obs < ratio.range[1]) ratio.range[1] <- ratio.TSV.obs
  hist(ratio.TSV.permute, breaks="FD", xlab="", ylab="", freq=F, 
       main=paste0("Ratio of TSV\nObserved ratio ",round(ratio.TSV.obs,2)),
       xlim=ratio.range)
  points(x=ratio.TSV.obs, y = 0, col=2, pch=20)
  # abline(v=ratio.TSV.obs, col=2, lwd=2)
  print(c(ratio.TSV.obs))
  
  if(!is.null(title)) mtext(title, side = 3, outer = TRUE, font = 2, cex = 1.5)
  
  
  
  return(c(pval.TSV.diff, pval.TSV.ratio))
  
  
}


source("scrap1.R")
# 
# 
# 
# 
# png("compare_variability_TSV.png", width = 800, height = 600)
# barplot(TSV.results, names.arg = obj.list, main = "Total Sample Variance", col=2:5)
# dev.off()
# barplot(GSV.results, names.arg = obj.list, main = "Generalized Sample Variance") 
# # GSV results don't make sense. Stick with TSV. 

# 
# png("compare_variability_01a.png", width = 800, height = 600)
compare.set.variability(dataset1 = ab3_do, dataset2 = ab3_3.5, type="greater", BB=1000, title = "DO vs 3.5") # Want to reject
# dev.off()
# png("compare_variability_01a-2.png", width = 800, height = 600)
compare.set.variability(dataset1 = ab3_3.5, dataset2 = ab3_do, type="greater", BB=1000, title = "3.5 vs DO") # Want to reject
# dev.off()
# png("compare_variability_01b.png", width = 800, height = 600)
# compare.set.variability(ab3_do, ab3_4.0, type="greater", BB=1000, title = "DO vs 4.0 - 90") # Want to reject
# dev.off()
# png("compare_variability_01c.png", width = 800, height = 600)
# compare.set.variability(ab3_do, ab3_4.0_300, type="greater", BB=1000, title = "DO vs 4.0 - 300") # Want to reject
# dev.off()
# png("compare_variability_02.png", width = 800, height = 600)
# compare.set.variability(ab3_3.5, ab3_4.0, type="greater", BB=1000, title = "3.5 - 90 vs 4.0 - 90") # Want to reject
# dev.off()
# png("compare_variability_03.png", width = 800, height = 600)
compare.set.variability(dataset1=ab3_4.0, dataset2=ab3_4.0_300, type="greater", BB=1000, title = "4.0 - 90 vs 4.0 - 300") # Want to reject
# dev.off()
# 

# What's happening with 3.5?
dev.off()
par(mfrow=c(4,1), mar=c(2,2,2.5,1), oma=c(2,2,0,0))
for(i in c(1,3,4,2)) {
  plot.data <- get(obj.list[i])[,14]
  run.time <- as.numeric(difftime(index(plot.data), index(plot.data)[1], units="days"))
  plot.data <- as.numeric(plot.data)
  plot(x = run.time, y = plot.data, 
       main=obj.list[i], 
       xlab="", ylab="", ylim=c(0,10),
       pch=20, cex.axis = 1.25)
}
mtext("Days", side = 1, line = 0.5, outer = TRUE)
mtext("Ammonia (mg/L)", side = 2, line = 0.5, outer = TRUE)

timeseries <- function(data, title) {
  run.time <- as.numeric(difftime(index(data), index(data)[1], units="days"))
  plot.data <- as.numeric(data)
  plot(x = run.time, y = plot.data, 
       main=title, 
       xlab="", ylab="", ylim=c(0,10),
       pch=20, cex.axis = 1.25)
}
timeseries(get(obj.list[1])[,14], "DO")
