hrt_correction <- function(data) {
  data <- na.omit(data)
  flow <- data$AB.INFLUENT.FLOW.MGD
  colnames(flow) <- "HRT"
  Z3.hrt <- 0.0913*flow^2-4.1399*flow+78.869
  Z5.hrt <- 0.1519*flow^2-6.8954*flow+131.42
  Z7.nh3.hrt <- 0.2289*flow^2-10.387*flow+197.97
  # Z7.hrt <- 0.3065*flow^2-13.908*flow+265.09
  # Z8.hrt <- 0.3848*flow^2-17.458*flow+332.7
  # Z9.hrt <- 0.463*flow^2-21.005*flow+400.29
  
  new.date <- lubridate::round_date(index(data)[1]+as.numeric(Z7.nh3.hrt[1])*60, "5 minutes") 
  start.time <- which(index(data) == new.date) # First useable timestamp
  
  for(i in start.time:(nrow(data)-1)) {
    new.data <- data[i,grep("Z7.Ammonia", colnames(data))]
    
    delta.time <- as.numeric(Z7.nh3.hrt[i]-Z5.hrt[i])*60
    j <- which(index(data) == lubridate::round_date(index(data)[i]-delta.time, "5 minutes"))
    if(length(j) == 0) next
    new.columns <- data[j, grep("Z6", colnames(data))]
    index(new.columns) <- index(data)[i]
    new.data <- cbind(new.data, new.columns)
    
    
    delta.time <- as.numeric(Z7.nh3.hrt[i]-Z3.hrt[i])*60
    j <- which(index(data) == lubridate::round_date(index(data)[i]-delta.time, "5 minutes"))
    if(length(j) == 0) next
    new.columns <- data[j, grep("Z3", colnames(data))]
    index(new.columns) <- index(data)[i]
    new.data <- cbind(new.data, new.columns)
    
    delta.time <- as.numeric(Z7.nh3.hrt[i])*60
    j <- which(index(data) == lubridate::round_date(index(data)[i]-delta.time, "5 minutes"))
    if(length(j) == 0) next
    new.columns <- data[j, grep("INFLUENT", colnames(data))]
    index(new.columns) <- index(data)[i]
    new.data <- cbind(new.data, new.columns)
    
    # delta.time <- as.numeric(Z9.hrt[i]-Z7.hrt[i])*60
    # j <- which(index(data) ==lubridate::round_date(index(data)[i]-delta.time, "5 minutes"))
    # if(length(j) == 0) next
    # new.columns <- data[j, grep("Z7", colnames(data))]
    # if(length(grep("Ammonia", colnames(new.columns))) > 0) new.columns <- new.columns[,-grep("Ammonia", colnames(new.columns))]
    # index(new.columns) <- index(data)[i]
    # new.data <- cbind(new.data, new.columns)
      
    if(!exists("save.data")) save.data <- new.data
    if(exists("save.data")) save.data <- rbind(save.data, new.data)
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  return(save.data)
}