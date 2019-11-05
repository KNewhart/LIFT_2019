# Import CSVs
library(readr)
all.files<- list.files(path="data/Historian Export", "[.]CSV")
all.variables <- unique(sapply(all.files, function(x) substr(x,1,nchar(x)-nchar("20190927164810_20190930164810")-5)))
obj.list <- list()
for(variable in all.variables) {
  files.to.import <- all.files[sapply(all.files, function(x) substr(x,1,nchar(x)-nchar("20190927164810_20190930164810")-5)) %in% variable]
  merged.file <- xts()
  for(file in files.to.import) {
    imported.file <- read_csv(paste0("data/Historian Export/",file), 
                              col_types = cols(TimeStamp = col_datetime(format = "%m/%d/%Y %H:%M:%S")))
    time <- as.POSIXlt(data.frame(imported.file)[,2])
    attr(time, "tzone") <- Sys.timezone()
    if(length(merged.file) > 0) merged.file <- rbind(merged.file, xts(data.frame(imported.file)[,3], 
                                                                      order.by=time))
    if(length(merged.file) == 0) merged.file <- xts(data.frame(imported.file)[,3], 
                                                    order.by=time)
  }
  assign(strsplit(as.character(imported.file[1,1]),"[.]")[[1]][2], merged.file)
  obj.list[[length(obj.list)+1]] <- strsplit(as.character(imported.file[1,1]),"[.]")[[1]][2]
}

# Merge all process data
print("Merging files...")
all.data <- xts()
for(obj in obj.list) {
  all.data <- cbind(all.data, get(obj))
  colnames(all.data)[ncol(all.data)] <- obj
}


# Unprocessed data
all.data <- na.locf(all.data)
all.data <- na.omit(all.data)

# Set timestamps
data.interval <- 5*60 # In minutes
five.min.timestamps <- seq(as.numeric(index(all.data)[1]), as.numeric(last(index(all.data))), data.interval)
five.min.timestamps <- round(as.POSIXct(five.min.timestamps, origin="1970-01-01"), "mins")

# Initialize parallel processing
library(doParallel)
library(foreach)
registerDoParallel(detectCores())

# Reduce to 5 min
all.timestamps <- round(index(all.data),"mins")
ab3_4.0_300_plus <- foreach(i=1:length(five.min.timestamps),
                             .combine=rbind,
                             .packages = "xts") %dopar% {
                               j <- which(five.min.timestamps[i] == all.timestamps)[1]
                               
                               if((is.na(j)) || (length(j)==0)) l <- 1
                               while((is.na(j)) || (length(j)==0)) {
                                 j <- last(which((five.min.timestamps[i]-l*60) == all.timestamps))
                                 l <- l+1
                               }
                               
                               return(xts(all.data[j,], order.by = five.min.timestamps[i]))
                             }
stopImplicitCluster()

col2remove <- "BASIN3_8_DO_PVENG"
ab3_4.0_300_plus <- ab3_4.0_300_plus[,-which(colnames(ab3_4.0_300_plus)==col2remove)] # Bad data
