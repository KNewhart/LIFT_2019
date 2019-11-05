if(import_data == "do") {
# Load Excel data into R
filename <- "data/LIFT 2019_Process Data_5m Jan - Apr.xlsx"
}

if(import_data == "3.5 mg/L 90") {
  filename <- "data/Ammonia SP 3-5 mg per L and 90 sec time delay.xlsx"
}

if(import_data == "4.0 mg/L 90") {
  filename <- "data/LIFT Process Data_Ammonia SP 4 mg per L and 90 sec time delay.xlsx"
}

if(import_data == "4.0 mg/L 300") {
  filename <- "data/LIFT Process Data_Ammonia SP 4 mg per L and 300 sec time delay.xlsx"
}


obj.names <- c("LIFT_2019_Inf_Eff", "LIFT_2019_AB_Inf", 
               "LIFT_2019_AB1", "LIFT_2019_AB2", "LIFT_2019_AB3",
               "LIFT_2019_Solids_Contact", "LIFT_2019_PAD_Centrate")
obj.sheets <- c("Plant Inf_Eff", "AB Inf", 
                "A_Basin 1", "A_Basin 2", "A_Basin 3",
                "Solids Contact", "PAD_Centrate")
require(naniar)
require(readxl)

for(i in 1:length(obj.names)) {
  data <- read_excel(filename, sheet = obj.sheets[i])
  if(!is.numeric(data[1,2][[1]])) colnames(data) <- paste(colnames(data), data[1,])
  bad.timestamps <- which(is.na(data.frame(data[,1])[,1]))
  if(length(bad.timestamps) > 0) data <- xts(replace_with_na_all(data = data[-bad.timestamps,-1], condition = ~.x == "Bad"), order.by = data.frame(data[-bad.timestamps,1])[,1])
  if(length(bad.timestamps) == 0) data <- xts(replace_with_na_all(data = data[,-1], condition = ~.x == "Bad"), order.by = data.frame(data[,1])[,1])
  assign(obj.names[i], xts(sapply(data,as.numeric), order.by = index(data)))
  rm(data)
  rm(bad.timestamps)
}


all.data <- cbind(LIFT_2019_Inf_Eff, LIFT_2019_AB_Inf,
                  LIFT_2019_AB1, LIFT_2019_AB2, 
                  LIFT_2019_AB3, LIFT_2019_Solids_Contact,
                  LIFT_2019_PAD_Centrate)

cols2remove <- unique(c(grep("NO2.mg.N.L", colnames(LIFT_2019_AB3)),
                        grep("MIXER", colnames(LIFT_2019_AB3)),
                        grep("IMLR", colnames(LIFT_2019_AB3)),
                        grep("Setpoint", colnames(LIFT_2019_AB3)),
                        grep("Calculated.Header.Flow", colnames(LIFT_2019_AB3)),
                        grep("AB3.Z9.Header.Flow.SCFM", colnames(LIFT_2019_AB3)))) # Mostly 0
ab3.data <- cbind(LIFT_2019_AB_Inf$`AB INFLUENT FLOW MGD`, 
                  LIFT_2019_AB_Inf$`A-BASIN INFLUENT CHANNEL AMMONIUM mg-N/L`, 
                  LIFT_2019_AB3[,-cols2remove])

if(import_data == "do") {
  all_do <- all.data
  ab3_do <- ab3.data
}

if(import_data == "3.5 mg/L 90") {
  all_3.5 <- all.data
  ab3_3.5 <- ab3.data
}

if(import_data == "4.0 mg/L 90") {
  all_4.0 <- all.data
  ab3_4.0 <- ab3.data
}

if(import_data == "4.0 mg/L 300") {
  all_4.0_300 <- all.data
  ab3_4.0_300 <- ab3.data
}

rm(all.data)
rm(ab3.data)
rm(LIFT_2019_Inf_Eff, LIFT_2019_AB_Inf,
   LIFT_2019_AB1, LIFT_2019_AB2, 
   LIFT_2019_AB3, LIFT_2019_Solids_Contact,
   LIFT_2019_PAD_Centrate)
