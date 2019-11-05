col.name.replacements <- matrix(data=c("AB.INFLUENT.FLOW.MGD", "Inf Flow",
                                       "NIF31053_PE_TO_ABAS_FLOW", "Inf Flow",
                                      "A.BASIN.INFLUENT.CHANNEL.AMMONIUM.mg.N.L", "Inf NH4",
                                      "AB3.Z6.DO.mg.L", "Z6 DO",
                                      "BASIN3_16_DO_PVENG", "Z6 DO",
                                      "AB3.Z7.DO.mg.L", "Z7 DO",
                                      "BASIN3_7_DO_PVENG", "Z7 DO",
                                      "AB3.Z8.DO.mg.L", "Z8 DO", 
                                      "NIX33384_AB3_Z8_DO", "Z8 DO",
                                      "AB3.Z9.DO.mg.L", "Z9 DO",
                                      "AB3.Z6.Header.Flow.SCFM", "Z6 Air Flow",
                                      "BASIN3_16_FLOW_PVENG" , "Z6 Air Flow",
                                      "AB3.Z7.Header.Flow.SCFM", "Z7 Air Flow",
                                      "BASIN3_7_FLOW_PVENG", "Z7 Air Flow",
                                      "AB3.Z8.Header.Flow.SCFM", "Z8 Air Flow",
                                      "BASIN3_8_FLOW_PVENG", "Z8 Air Flow",
                                      "AB3.Zone.6.Valve.Position...Open", "Z6 Air Valve",
                                      "BASIN3_16_VALVE_POS_PVENG", "Z6 Air Valve",
                                      "AB3.Zone.7.Valve.Position...Open", "Z7 Air Valve",
                                      "BASIN3_7_VALVE_POS_PVENG", "Z7 Air Valve",
                                      "AB3.Zone.8.Valve.Position...Open", "Z8 Air Valve",
                                      "BASIN3_8_VALVE_POS_PVENG", "Z8 Air Valve",
                                      "AB3.Zone.9.Valve.Position...Open", "Z9 Air Valve",
                                      "AB3.Z7.Ammonia.mg.N.L", "Z7 NH4",
                                      "NIA33391_AB3_AMMONIA", "Z7 NH4",
                                      "AB3.Z3.Nitrate.mg.N.L", "Z3 NO3",
                                      "NIR33391_AB3_Z3_NITRATE", "Z3 NO3",
                                      "AB3.Z9.Nitrate.mg.N.L", "Z9 NO3",
                                      "NIR33392_AB3_Z9_NITRATE", "Z9 NO3",
                                      "NR4242", "Final NO3"), nrow=30, ncol=2, byrow=TRUE)

for(x in obj.list){
  data <- get(x)
  if(anyNA(col.name.replacements[as.numeric(sapply(colnames(data), function(y) which(col.name.replacements[,1] == y))),2])) {
    print(paste("Unknown variable name. Check",x,"object and src/col_name_fix.R"))
    next()
  }
  colnames(data) <- col.name.replacements[as.numeric(sapply(colnames(data), function(y) which(col.name.replacements[,1] == y))),2]
  assign(x=x,value=data)
}
rm(data)
