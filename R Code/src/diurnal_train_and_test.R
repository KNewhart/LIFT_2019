diurnal.train <- function(data, predict.col) {
  terms <- c(grep("cos", colnames(data)), grep("sin", colnames(data)))
  fmla <- paste0("data.m[,predict.col] ~ ", paste0("data.m[,",terms,"]", collapse=" + "))
  data.m <- matrix(as.numeric(data), ncol=ncol(data), byrow=FALSE)
  mod.lm <- lm(fmla)
  return(mod.lm)
}

calc.residuals <- function(data, predict.col, model) {
  terms <- c(grep("cos", colnames(data)), grep("sin", colnames(data)))
  yy.diurnal <- apply(data[,terms],1,function(x) model$coef%*%c(1,x)) 
  yy.residual <- data[,predict.col] - yy.diurnal
  return(yy.residual)
}
