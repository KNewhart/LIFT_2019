library(officer)
library(magrittr)
library(rvg)
library(xts)

# my_pres <- read_pptx()
my_pres <- read_pptx(path = "my_pres.pptx")
# layout_summary(my_pres)

my_pres <- my_pres %>% 
  add_slide(layout="Only Content", master = "CSM_16x9")
my_pres <- ph_with_vg(my_pres, code = {
  par(mar=c(3,2,2,2))
  barplot(TSV.results, 
          main = "Total Sample Variance", 
          col=2:5,
          cex.names = 1.5,
          cex.main = 1.5,
          cex.axis = 1.25)
}
                      , type="body")

my_pres <- my_pres %>% 
  add_slide(layout="Only Content", master = "CSM_16x9")
my_pres <- ph_with_vg(my_pres, code = {
  compare.set.variability(dataset1 = ab3_3.5, dataset2 = ab3_do, type="greater", BB=1000, title = "3.5 vs DO")
                      }, type="body")
my_pres <- my_pres %>% 
  add_slide(layout="Only Content", master = "CSM_16x9")
my_pres <- ph_with_vg(my_pres, 
                      code = compare.set.variability(ab3_do, ab3_4.0, type="greater", BB=1000, title = "DO vs 4.0 - 90")
                      , type="body")

my_pres <- my_pres %>% 
  add_slide(layout="Only Content", master = "CSM_16x9")
my_pres <- ph_with_vg(my_pres, 
                      code = compare.set.variability(ab3_4.0, ab3_4.0_300, type="greater", BB=1000, title = "4.0 - 90 vs 4.0 - 300")

                      , type="body")

timeseries <- function(data, title) {
  par(mar=c(2.6, 4.1,3.6, 4.1), mgp=c(2,1.5,0))
  run.time <- as.numeric(difftime(index(data), index(data)[1], units="days"))
  plot.data <- as.numeric(data)
  plot(x = run.time, y = plot.data, 
       main=title,
       xlab="", ylab="",
       pch=20, cex.axis = 2, cex.main = 2,
       xaxt="n")
  labels.at <- axTicks(side=1)
  labels.are <- sapply(labels.at, function(x) weekdays(index(data[which(run.time == x)]), abbreviate=T))
  axis(side=1, at = labels.at, labels = labels.are, cex.axis=2)
}

my_pres <- my_pres %>% 
  add_slide(layout="Only Content", master = "CSM_16x9")
my_pres <- ph_with_vg(my_pres, 
                      code = timeseries(get(obj.list[1])[,14], "DO")
                      
                      , type="body")

my_pres <- my_pres %>% 
  add_slide(layout="Only Content", master = "CSM_16x9")
my_pres <- ph_with_vg(my_pres, 
                      code = timeseries(get(obj.list[3])[,14], "4.0 mg/L - 90")
                      
                      , type="body")

my_pres <- my_pres %>% 
  add_slide(layout="Only Content", master = "CSM_16x9")
my_pres <- ph_with_vg(my_pres, 
                      code = timeseries(get(obj.list[4])[,14], "4.0 mg/L - 300")
                      
                      , type="body")

my_pres <- my_pres %>% 
  add_slide(layout="Only Content", master = "CSM_16x9")
my_pres <- ph_with_vg(my_pres, 
                      code = {
                        par(mfrow=c(3,1));
                        timeseries(get(obj.list[1])[,14], "DO");
                        timeseries(get(obj.list[3])[,14], "4.0 mg/L - 90");
                        timeseries(get(obj.list[4])[,14], "4.0 mg/L - 300")
                      }
                      , type="body")




my_pres <- my_pres %>% 
  add_slide(layout="Only Content", master = "CSM_16x9")
my_pres <- ph_with_vg(my_pres, 
                      code = timeseries(get(obj.list[2])[,14], "3.5 mg/L - 90")
                      
                      , type="body")


my_pres <- my_pres %>% 
  add_slide(layout="Only Content", master = "CSM_16x9")
my_pres <- ph_with_vg(my_pres, 
                      code = {
                        par(mfrow=c(3,1));
                        timeseries(get(obj.list[1])[,14], "DO");
                        timeseries(get(obj.list[2])[,14], "3.5 mg/L - 90");
                        timeseries(get(obj.list[3])[,14], "4.0 mg/L - 90")
                      }
                      , type="body")

print(my_pres, "my_pres.pptx")


all.data <- ab3_do
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

my_pres <- read_pptx(path = "my_pres.pptx")
my_pres <- my_pres %>% 
  add_slide(layout="Only Content", master = "CSM_16x9")
my_pres <- ph_with_vg(my_pres, 
                      code = {
  par(mar=c(4,4,3,1), mgp=c(2.5,1,0))
  boxplot(ammonia.by.hour~(hour.col), pch=19, 
          xlab="Hour of the Day", ylab="Ammonia (mg/L N)")
  title("Diurnal Trend in Ammonia in Zone 7")
  points(1:24, ammonia.mean, pch=17, col=2)
}
, type="body")

print(my_pres, "my_pres.pptx")


all.data <- train.res1
hour.col=NULL
ammonia.by.hour=NULL
ammonia.mean=NULL
for(i in 1:24){
  test1=all.data[.indexhour(all.data) %in% (i-1) & .indexmin(all.data) %in% c(0:60)]
  nhours=length(test1[,1])
  hour.col=c(hour.col,rep(i,nhours))
  ammonia.by.hour=c(ammonia.by.hour,test1[,1])
  ammonia.mean[i]=mean(test1[,1],na.rm=TRUE)
}
my_pres <- read_pptx(path = "my_pres.pptx")
my_pres <- my_pres %>% 
  add_slide(layout="Only Content", master = "CSM_16x9")
my_pres <- ph_with_vg(my_pres, 
                      code = {
                        par(mar=c(4.1,4.1,3,1), mgp=c(3,1,0))
                        boxplot(ammonia.by.hour~(hour.col), pch=19, 
                                xlab="Hour of the Day", ylab="Adjusted Ammonia")
                        title("Diurnal Trend in Ammonia in Zone 7")
                        points(1:24, ammonia.mean, pch=17, col=2)
                      }
                      , type="body")

print(my_pres, "my_pres.pptx")
