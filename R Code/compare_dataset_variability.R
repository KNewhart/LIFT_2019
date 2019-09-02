

######################################
##Just generating a couple of dummy
##datasets to work with
######################################

library(mnormt)
set.seed(77)

n1=500
n2=1000

#These two sets have different means and different covariances, different sample sizes
mean1=seq(1, 100, by=2)
mean2=seq(2, 101, by=2)

Sig1=diag(sqrt(1:50), nrow=length(mean1), ncol=length(mean1))
Sig2=4*exp(-3*as.matrix(dist(1:50)/10))
#round((distance)[1:5,1:5],2)


#These two sets have different means and different variances, different sample sizes
#For this set, we want to reject the null hypothesis.
set.a=rmnorm(n1, mean=mean1, varcov=Sig1)
set.b=rmnorm(n2, mean=mean2, varcov=Sig2)


#These two sets have different means but the same variances, different sample sizes
#For this set, we want to fail to reject the null hypothesis.
set.c=rmnorm(n1, mean=mean1, varcov=Sig2)
set.d=rmnorm(n2, mean=mean2, varcov=Sig2)


#These two sets have the same means and the same variances, different sample sizes
#For this set, we want to fail to reject the null hypothesis.
set.e=rmnorm(n1, mean=mean2, varcov=Sig2)
set.f=rmnorm(n2, mean=mean2, varcov=Sig2)



###############################################
#Calculating the observed (sample) statistics
###############################################

#Generalized sample variance (GSV)

GSV.a=det(cov(set.a))
GSV.b=det(cov(set.b))
GSV.a
GSV.b

#Total sample variance (TSV)

TSV.a=sum(diag(cov(set.a)))
TSV.b=sum(diag(cov(set.b)))
TSV.a
TSV.b


#Generalized sample variance (GSV)

GSV.c=det(cov(set.c))
GSV.d=det(cov(set.d))
GSV.c
GSV.d

#Total sample variance (TSV)

TSV.c=sum(diag(cov(set.c)))
TSV.d=sum(diag(cov(set.d)))
TSV.c
TSV.d



#Generalized sample variance (GSV)

GSV.e=det(cov(set.e))
GSV.f=det(cov(set.f))
GSV.e
GSV.f
GSV.e-GSV.f
GSV.e/GSV.f

#Total sample variance (TSV)

TSV.e=sum(diag(cov(set.e)))
TSV.f=sum(diag(cov(set.f)))
TSV.e
TSV.f

TSV.e-TSV.f
TSV.e/TSV.f



#Be sure to put the dataset in the first slot that has the larger observed TSV.

compare.set.variability=function(dataset1, dataset2, type, BB){
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
	
	par(mfrow=c(1,2))
	hist(diff.TSV.permute, breaks="FD", xlab="", ylab="", freq=F, main="Difference of TSV")
	abline(v=diff.TSV.obs, col=2, lwd=2)
	print(c(diff.TSV.obs))
	
	hist(ratio.TSV.permute, breaks="FD", xlab="", ylab="", freq=F, main="Ratio of TSV")
	abline(v=ratio.TSV.obs, col=2, lwd=2)
	print(c(ratio.TSV.obs))
	
	
	if(type=="greater"){

		pval.TSV.diff=length(which(diff.TSV.permute>(diff.TSV.obs)))/BB
		pval.TSV.ratio=length(which(ratio.TSV.permute>(ratio.TSV.obs)))/BB
		
		}
	 # if(type=="not.equal"){
		 # pval.GSV=length(which(diff.GSV.permute>abs(diff.GSV.obs)) && which(diff.GSV.permute < -abs(diff.GSV.obs)))/BB
		 # pval.TSV=length(which(diff.TSV.permute>abs(diff.TSV.obs)) && which(diff.TSV.permute < -abs(diff.TSV.obs)))/BB
		 # }
		
	return(c(pval.TSV.diff, pval.TSV.ratio))
	
	
}




compare.set.variability(set.a, set.b, type="greater", BB=1000)  #Want to reject
compare.set.variability(set.c, set.d, type="greater", BB=1000)	#Want to FTR
compare.set.variability(set.e, set.f, type="greater", BB=1000)	#Want to FTR












###############################################
#The difference and ratio in GSV's are not centered at 
#zero and one under the null, respectively, and are unstable.
#So the code below this line is OLD. It includes GSV results.
###############################################


compare.set.variability=function(dataset1, dataset2, type, BB){
	#type can be one of "greater" or "not.equal"
	
	#Observed statistics
	diff.GSV.obs=det(cov(dataset1))-det(cov(dataset2))
	diff.TSV.obs=sum(diag(cov(dataset1)))-sum(diag(cov(dataset2)))	
	
	ratio.GSV.obs=det(cov(dataset1))/det(cov(dataset2))
	ratio.TSV.obs=sum(diag(cov(dataset1)))/sum(diag(cov(dataset2)))	
	
	#Combinining all of the data into one set
	all.data=rbind(dataset1,dataset2)
	nn1=dim(dataset1)[1]
	nn2=dim(dataset2)[1]
	nn.total=nn1+nn2
	
	
	#Resampling
	diff.GSV.permute=array()
	diff.TSV.permute=array()
	
	ratio.GSV.permute=array()
	ratio.TSV.permute=array()
	
	for(i in 1:BB){
		first.set=sample(1:nn.total, nn1, replace=FALSE)
		second.set=setdiff(1:nn.total, first.set)
		permute.set1=all.data[first.set,]
		permute.set2=all.data[second.set,]
		
		diff.GSV.permute[i]=det(cov(permute.set1))-det(cov(permute.set2))
		diff.TSV.permute[i]=sum(diag(cov(permute.set1)))-sum(diag(cov(permute.set2)))	
		
		ratio.GSV.permute[i]=det(cov(permute.set1))/det(cov(permute.set2))
		ratio.TSV.permute[i]=sum(diag(cov(permute.set1)))/sum(diag(cov(permute.set2)))	
			
	}
	
	par(mfrow=c(2,2))
	hist(diff.GSV.permute, breaks="FD", xlab="", ylab="", freq=F, main="Difference of GSV")
	abline(v=diff.GSV.obs, col=2, lwd=2)
	hist(diff.TSV.permute, breaks="FD", xlab="", ylab="", freq=F, main="Difference of TSV")
	abline(v=diff.TSV.obs, col=2, lwd=2)
	print(c(diff.GSV.obs, diff.TSV.obs))
	
	hist(ratio.GSV.permute, breaks="FD", xlab="", ylab="", freq=F, main="Ratio of GSV")
	abline(v=ratio.GSV.obs, col=2, lwd=2)
	hist(ratio.TSV.permute, breaks="FD", xlab="", ylab="", freq=F, main="Ratio of TSV")
	abline(v=ratio.TSV.obs, col=2, lwd=2)
	print(c(ratio.GSV.obs, ratio.TSV.obs))
	
	
	
	
	if(type=="greater"){
		pval.GSV=length(which(diff.GSV.permute>(diff.GSV.obs)))/BB
		pval.TSV=length(which(diff.TSV.permute>(diff.TSV.obs)))/BB
		
		pval.GSV2=length(which(ratio.GSV.permute>(ratio.GSV.obs)))/BB
		pval.TSV2=length(which(ratio.TSV.permute>(ratio.TSV.obs)))/BB
		
		}
	 # if(type=="not.equal"){
		 # pval.GSV=length(which(diff.GSV.permute>abs(diff.GSV.obs)) && which(diff.GSV.permute < -abs(diff.GSV.obs)))/BB
		 # pval.TSV=length(which(diff.TSV.permute>abs(diff.TSV.obs)) && which(diff.TSV.permute < -abs(diff.TSV.obs)))/BB
		 # }
		
	return(c(pval.GSV, pval.TSV, pval.GSV2, pval.TSV2))
	
	
}



compare.set.variability(set.e, set.f, type="greater", BB=1000)
compare.set.variability(set.a, set.b, type="greater", BB=1000)
compare.set.variability(set.d, set.c, type="greater", BB=1000)











