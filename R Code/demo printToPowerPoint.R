for(i in obj.list) {
  data <- data.frame(get(i))
  
  printToPowerPoint(code.block = {
    xrange <- 1:1440 # 5 days
    xinterval <- seq(min(xrange),max(xrange),max(xrange)/5)
    par(mar=c(2,4,2,5))
    size <- 1.5
    
    # Ammonia (L)
    plot(data[xrange,grep("Ammonia", colnames(data))], 
         type="l",ylab="", col="blue", main=i, xaxt="n",xlab="", cex.axis=size, cex=size,lwd=2*size)
    axis(1,at=xinterval,labels = format(as.Date(rownames(data[xinterval,])), "%m/%d"), cex.axis=size)
    mtext(side=2,line=3, "Z7 Ammonia (mg/L N)", col="blue", cex=size)
    if(i==obj.list[3]) abline(a=4, b=0,lwd=2*size)
    if(i==obj.list[4]) abline(a=4, b=0,lwd=2*size)
    if(i==obj.list[2]) abline(a=3.5, b=0,lwd=2*size)
    
    # Z6 DO (R)
    par(new=T)
    plot(data[xrange,grep("Z6.DO", colnames(data))], 
         type="l",axes=FALSE, xlab="", ylab="", col="red",cex.axis=size, cex=size,lwd=2*size)
    axis(4,cex.axis=size)
    mtext(side=4,line=3, "Z6 DO (mg/L)", col="red", cex=size)
    if(i==obj.list[1]) abline(a=2.5, b=0,lwd=2*size)
  }, presentation.path = "C:\\Users\\Kathr\\Dropbox\\Code\\LIFT_2019\\R Code")
}