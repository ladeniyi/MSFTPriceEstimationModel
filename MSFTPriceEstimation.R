library("YRmisc")           # each session
library("readxl")           # each session

# 1. import spInfox excel file
# spInfox <- File|Import|..navigate|.....spInfox
spInfox <- read_excel("Desktop/BUA 633/Class 3 June 1/spInfox.xlsx")
View(spInfox)
spInfoSave<-spInfox
spInfox<-as.data.frame(spInfox)
dim(spInfox)
names(spInfox)


# 2. import spDatax  excel file
#spDatax <- File|Import|..navigate|.....spDatax
spDatax <- read_excel("Desktop/BUA 633/Class 3 June 1/spDatax.xlsx")
View(spDatax)
spDataSave<-spDatax
data.class(spDatax)
spDatax<-as.data.frame(spDatax)
dim(spDatax)
names(spDatax)

# Merge 2 dataframe by "tkr" overlap
names(spInfox)
names(spDatax)
spmdf<-merge(spInfox,spDatax,by="tkr")
names(spmdf)
dim(spmdf)

# create new numeric variable date extracted from date
spmdf$date
spmdf$year<-as.numeric(substring(spmdf$date,7,10))
names(spmdf)


###############################################################################    
# Time Series Data Analysis
# extract time series data for 1 company MSFT (Microsoft)
tsdf<-spmdf[spmdf$tkr=="MSFT",c("tkr","price","eps","bvps","name","year")]
tsdf
#sorting in ascending order
tsdf<-df.sortcol(tsdf,6,desc = F)
tsdf
#creating obs variable
tsdf$obs<-1:nrow(tsdf)
tsdf
                                      ?    +   +
# function specification   price = f(obs, eps,bvps)

dim(tsdf)
names(tsdf)
tsdf

#Graphical Analysis

par(mfrow=c(3,3))  # par - partion
hist(tsdf$price,xlab = "Price",ylab="Freq",main="Fig. 1 Hist of Price")
hist(tsdf$eps,xlab = "EPS",ylab="Freq",main="Fig. 2 Hist of EPS")
hist(tsdf$bvps,xlab = "BVPS",ylab="Freq",main="Fig. 3 Hist of BVPS")
  
par(mfrow=c(3,3))  # par - partion
ts.plot(tsdf$price,xlab = "Time (year)",ylab="Price",main="Fig. 4 TS of Price")
ts.plot(tsdf$eps,xlab = "Time (year)",ylab="EPS",main="Fig. 5 TS of EPS")
ts.plot(tsdf$bvps,xlab = "Time (year)",ylab="BVPS",main="Fig. 6 TS of BVPS")

par(mfrow=c(2,2))
scatter.smooth(tsdf$bvps,tsdf$price,xlab="bvps",ylab="price",main="Fig. 7 Price v BVPS")
scatter.smooth(tsdf$eps,tsdf$price,xlab="eps",ylab="price",main="Fig. 8 Price v EPS",
     pch="*",cex=2)

#Statistical Analysis
# des stats
ds.summ(tsdf[,c("price","eps","bvps")],2)[,-c(7,8)]

# cor
tsdf
round(cor(tsdf[,c("price","obs","eps","bvps")]),2)

#lm 
~  - "modeled as a function of obs, eps, bvps."
fit<-lm(price~obs+eps+bvps,data=tsdf,na.action=na.omit)
reg.dw(fit)  #durbin-watson statistic  measures forecast bias
                  # bias - serial correlation
                  # dw in YRmisc
                  # 0 - 4.0, 2.0 perfect,   1.7-2.3 is OK - no bias

	summary(fit)
    names(fit)
    coefficients(fit)
    names(summary(fit))
    predValues<-predict(fit,tsdf)
    residValues<-residuals(fit)

  # Model Validation
    vdf<-data.frame(tsdf,predValues,residValues)
    par(mfrow=c(2,2))
      hist(vdf$residValues)
      plot(vdf$predValues,vdf$price,type="n")
         text(vdf$predValues,vdf$price,vdf$tkr)
      scatter.smooth(vdf$predValues,vdf$price,type="n")
         text(vdf$predValues,vdf$price,vdf$tkr)
      pl.2ts(vdf$price,vdf$predValues,"TSPlot Act v Pred")
     
