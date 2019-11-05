all.files.in.folder <- list.files(path="C:\\Users\\Kate Newhart\\Desktop\\LIFT_2019\\Raw data", pattern="[.]CSV")
variables <- sapply(all.files.in.folder, function(x) strsplit(x, split="[.]")[[1]][2])
rm("timestamps")
for(var in unique(variables)) {
  files.to.import <- names(variables[which(variables==var)])
  all.data <- data.frame()
  for(file in files.to.import) {
    data <- read.csv(paste0("C:/Users/Kate Newhart/Desktop/LIFT_2019/Raw data/",file), stringsAsFactors=FALSE)
    all.data <- rbind(all.data, data)
  }
  if(!exists("timestamps")) {
    first.timestamp <- lubridate::ceiling_date(as.POSIXct(all.data[1,2], format="%m/%d/%Y %H:%M:%S"), "5 min")
    last.timestamp <- lubridate::floor_date(as.POSIXct(all.data[nrow(all.data),2], format="%m/%d/%Y %H:%M:%S"), "5 min")
    timestamps <- seq(first.timestamp, last.timestamp, by=5*60)
  } 
  library(foreach)
  library(parallel)
  library(doParallel)
  detectCores()
  cl <- parallel::makeCluster(detectCores())
  doParallel::registerDoParallel(cl)
  timestamp.matches <- vector()
  # for(t in 1:length(timestamps)) {
  timestamp.matches <- foreach(t = timestamps,
                               .combine = 'c') %dopar% {
    v <- which(as.character(round(as.POSIXct(all.data[,2], format="%m/%d/%Y %H:%M:%S"), "min")) %in% as.character(t))
    return(v[length(v)])
  }
  parallel::stopCluster(cl)
  results <- all.data[timestamp.matches,]
  results[,2] <- round(as.POSIXct(results[,2], format="%m/%d/%Y %H:%M:%S"), "min")
  assign(x=var, value=results)
}
