library(corrplot)
library(ggplot2)
library(dplyr)
library(reshape2)

# 1) Does the Stock Market Affect the Housing Market?
# 2) If there is any effect, is this the same for different cities?
# 3) Which index is more connected to the Housing Market?
# 4) Does the consumer sentiment affect the Housing Market?


# set working Directory
setwd('~/Dropbox/DS/DataIncubator2/ch3/') 

################# Property price
# read the file with Medial value of Houses per square feat (04/1996 to 04/2015)
data = read.table("./data/Zip_MedianValuePerSqft2.csv", sep=",",head=TRUE)                            
# Take variables of interest & Remove irrelevant information 
# (e.g. Zip (1), City (2), State (3), Metro (4), etc. )
data<-data[c(2,7:235)]
# remove X from columns. Create the x axis values (1996-2015)
x <- as.numeric(sub("X", "\\2", colnames(data[2:230])))
 
# Aggregate by City (complete cases)
by_city <- aggregate(x = data[2:230], by = list(data$City), FUN = "mean",na.rm = TRUE)
#by_state <- aggregate(x = data[2:230], by = list(data$State), FUN = "mean",na.rm = TRUE)
#by_metro <- aggregate(x = data[2:230], by = list(data$Metro), FUN = "mean",na.rm = TRUE)
by_city <-by_city[complete.cases(by_city),]

# put the cities as row names and remove 1st col to create matrix
rownames(by_city)<-by_city[,1]
by_city[1]<-list(NULL)

################# Markets
#http://finance.yahoo.com/q/hp?a=03&b=1&c=2006&d=03&e=1&f=2015&g=m&s=NYA%2C+&ql=1

# read the stock markets from Apr06 -Apr2015

# Read NYSE (Dow Jones). Make Dataframe with close value (old to new)
nyse_dj <-read.table("./data/nyse_dj.csv", sep=",",head=TRUE) 
nyse_dj <-nyse_dj[order(-1:-229),c(5)]

# Read NASDAQ (Tech). Make Dataframe with close value (old to new)
nasdaq <-read.table("./data/nasdaq_comp.csv", sep=",",head=TRUE) 
nasdaq <-nasdaq[order(-1:-229),c(5)]


######################### Relation between data.

# calculate the correlation matrix
set.seed(246)
cormat <- apply(df, MARGIN=1, FUN=function(z) apply(df, MARGIN=1, FUN=function(y) cor(z, y)))

# print the first five elements of the correlation matrix
print(cormat[1:5,1:5])

########################################################## PLOT 1
# 1/2) is there relationship with The stock market price?

nyc<-as.numeric(cor(t(by_city["New York",]),nyse_dj))
sfc<-as.numeric(cor(t(by_city["San Francisco",]),nyse_dj))

par(mfrow = c(1, 2))
par(mar=c(5.1,4.1,4.1,2.1))
 

# plot for NY
plot(x,by_city["New York",], main="New York",type="l",col="black", lwd=2.5,
     xlab="years", ylab = "Mean property value ($/sq. ft.)")
par(new=TRUE)
plot(x,predict(loess(nyse_dj~x)), type = "l", col="blue", axes = FALSE, bty = "n", 
     xlab = "", ylab = "",lwd=2.5)
legend("topleft", inset=0.01,c("Housing","NYSE"), col=c("black","blue"),lwd=c(2.5,2.5),cex=0.7)
axis(side=4, at = pretty(range(nyse_dj),col="blue",cx=0.4))
legend("bottomright", inset=0.01,c(paste("R=",round(nyc, digits=2))), col=c("black"),cex=0.7)

# plot for SF
plot(x,by_city["San Francisco",],main="San Francisco",type="l",col="black", lwd=2.5,
     xlab="years", ylab = "Mean property value ($/sq. ft.)")
par(new=TRUE)
plot(x,predict(loess(nyse_dj~x)), type = "l", col="blue", axes = FALSE, bty = "n", 
     xlab = "", ylab = "",lwd=2.5)
legend("topleft", inset=0.01, c("Housing","NYSE"), col=c("black","blue"),lwd=c(2.5,2.5),cex=0.7)
axis(side=4, at = pretty(range(nyse_dj),col="blue",cx=0.4))
legend("bottomright", inset=0.01,c(paste("R=",round(sfc, digits=2))), col=c("black"),cex=0.7)


# The housing and stock markets are cleare interconnected. 
# but not exactly in the same way for different cities. 
# The Correlation for SF is higher than in NY (0.84 vs 0.79)

################################################# PLOT 2

# 3) if yes, which index is more connected to the Housing Market?
# We have compared with Nasdaq index

nyc2<-as.numeric(cor(t(by_city["New York",]),nasdaq))
sfc2<-as.numeric(cor(t(by_city["San Francisco",]),nasdaq))

par(mfrow = c(1, 2))
par(mar=c(5.1,4.1,4.1,2.1))


# plot for NY
plot(x,by_city["New York",], main="New York",type="l",col="black", lwd=2.5,
     xlab="years", ylab = "Mean property value ($/sq. ft.)")
par(new=TRUE)
plot(x,predict(loess(nasdaq_100~x)), type = "l", col="red", axes = FALSE, bty = "n", 
     xlab = "", ylab = "",lwd=2.5)
legend("topleft", inset=0.01,c("Housing","NASDAQ"), col=c("black","red"),lwd=c(2.5,2.5),cex=0.7)
axis(side=4, at = pretty(range(nasdaq),col="red",cx=0.4))
legend("bottomright", inset=0.01,c(paste("R=",round(nyc2, digits=2))), col=c("black"),cex=0.7)

# plot for SF
plot(x,by_city["San Francisco",],main="San Francisco",type="l",col="black", lwd=2.5,
     xlab="years", ylab = "Mean property value ($/sq. ft.)")
par(new=TRUE)
plot(x,predict(loess(nasdaq~x)), type = "l", col="red", axes = FALSE, bty = "n", 
     xlab = "", ylab = "",lwd=2.5)
legend("topleft", inset=0.01, c("Housing","NASDAQ"), col=c("black","red"),lwd=c(2.5,2.5),cex=0.7)
axis(side=4, at = pretty(range(nasdaq),col="red",cx=0.4))
legend("bottomright", inset=0.01,c(paste("R=",round(sfc2, digits=2))), col=c("black"),cex=0.7)



## Clearly Dow Jones had a higher correlation than NASDAQ with the housing price
# in SF but is it the same for the rest of the country?

########################################################### Plot 3

# 4) which cities are more correlated to Dow Jones, which ones to NASDAQ?




# now let's select the 5 most/least expensive cities in US
most <- by_city[order(-by_city[,229],rownames(by_city)),] 
less <- by_city[order(by_city[,229],rownames(by_city)),]

# 5 most expensive cities:
rownames(most)[1:5]
# "Palo Alto"  "Los Altos"  "Menlo Park" "Malibu"     "Stanford" 

# 5 less expensive cities:
rownames(less)[1:5]
#"Shenandoah"        "Town of Salamanca" "Cahokia"        
# "East Saint Louis"  "Youngstown"       


# cities correlated with DJ
mcmdj<-c()
for (i in 1:3672) {mcmdj[i]<-as.numeric(cor(t(by_city[i,]),nyse_dj))}
names(mcmdj)<-rownames(by_city)[1:3672]
mcmdj<-mcmdj[order(-mcmdj)]
mcmdj[1:5]
# Mountain View    Eagle Pass     Cupertino    San Carlos     Sunnyvale 
# 0.8840694     0.8834987     0.8795367     0.8784956     0.8770255 

mcmdj2<-mcmdj[order(mcmdj)]
mcmdj2[1:5]
# Detroit   Town of Beloit      Center Line          Redford Garfield Heights 
# -0.4113483       -0.3460930       -0.3104341       -0.2955093       -0.2914177

# cities correlated with NA
mcmna<-c()
for (i in 1:3672) {mcmna[i]<-as.numeric(cor(t(by_city[i,]),nasdaq))}
names(mcmna)<-rownames(by_city)[1:3672]
mcmna<-mcmna[order(-mcmna)]
mcmna[1:5]

# Palo Alto     Hohenwald       Purcell Mountain View     Cupertino 
# 0.7236257     0.7134041     0.7011296     0.6991091     0.6922888 


# As a curiosity, Here is the matrix of the Housing price correlation for 10 cities 

# print corr-plot of the correlation matrix using clustering
corrplot(M[1:50,1:50], order = "hclust", addrect = 10)


 


 
