
#plot(names(data1),data1) 
#print(names(data1))
#names(data1) <- gsub(".01", ".416", names(data1))
# convert data from string to float
#head(data1)
#data1<-as.numeric(data1,na.rm=TRUE)
#names(data1) <- str_replace(".01", "0.416")
# data subset
#data1_z1 <-subset(data1,data1$RegionName == 94112)
#data1<-as.numeric(data1,na.rm=TRUE)
#data1_z2 <-subset(data1,data1$RegionName == 94110)

#plot(data1_z1,data1_z2)
#f2 = read.csv("Births_by_Race_by_ZIP_Code__2000-2013.csv",head=TRUE)
#data2<-subset(f2,f2$ZIP.CODE==94112)
#data2<-subset(f2,f2$ZIP.CODE==94112 & RACE=="Hispanic")
#data2<-tapply(data2$RACE.COUNT, data2$YEAR, FUN=sum)
#plot(data2$YEAR,data2$RACE.COUNT)