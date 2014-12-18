# sample code to create graphs using bigvis

library("bigvis")
library("ggplot2")
library("plyr")
library("bigmemory")
require("data.table")

#did a merge of all five .csvs using cat*.csv > all_PLUTO_data.csv
#figured i could leave all the data together in one file and didn't need to filter like example
#as.data.frame(pData)
pData <- read.csv("/users/bcarancibia/CUNY_IS608/lecture2/data/all_PLUTO_data.csv", header = TRUE, sep =",", na.strings=c("0", "NA",""), stringsAsFactors=FALSE)
pData[, c(1,2,3,4)] <- sapply(pData[, c(1,2,3,4)], as.numeric)
pData[is.na(pData)] <- 0


# does lot area change with year of construction?
lotArea  <- pData$LotArea[pData$YearBuilt > 1850 & pData$LotArea > 100 & pData$AssessTot < 10000000 & pData$NumFloors != 0 ]
yrBuilt  <- pData$YearBuilt[pData$YearBuilt > 1850 & pData$LotArea > 100 & pData$AssessTot < 10000000 & pData$NumFloors != 0]
builtFar <- pData$BuiltFAR[pData$YearBuilt > 1850 & pData$NumFloors != 0]
numFloor <- pData$NumFloors[pData$YearBuilt > 1850 & pData$NumFloors != 0]
assessTot <- pData$AssessTot[pData$YearBuilt > 1850 & pData$NumFloors != 0]
valPerFloor <- assessTot/numFloor

#question 1
summary(yrBuilt)
yr <- condense(bin(yrBuilt, 1))
autoplot(yr)
ggsave("/users/bcarancibia/CUNY_IS608/lecture2/yrBuilt.png", height=3, width=4)
#there is a lot in the results that make me question the data. 
#It looks like there are just estimates when the data is less than or equal to 1980. 
#You can tell by the large peaks on certain years and afer 1980 the more normal distribution.

#five year averages are better
yr <- condense(bin(yrBuilt, 5), z=lotArea)
autoplot(yr) + xlim(1900, 2014) + ylim(0, 10000) + ylab('Lot Area')
ggsave("/users/bcarancibia/CUNY_IS608/lecture2/Question1.png", height=3, width=5)



#question 2
#Number of buildings that were built unusually tall
#keep getting error with the number of observations being different
yr2<-condense(bin(yrBuilt,1))
total<-sum(yr2$.count) 
yr2$perc_built<-cumsum(yr2$.count)/total 
ggplot(yr2,aes(x=yrBuilt,y=perc_built))+geom_line()+ylab('') #need to add logs

flrVsYr <- condense(bin(yrBuilt,5),bin(numFloor,1)) 
p <- autoplot(flrVsYr)+theme(panel.background=element_rect(fill='white'))+ylim(0,60) 
p+scale_fill_gradient(limits=c(1,100000),
                      low='grey', 
                      high='blue', 
                      trans="log",
                      breaks=myBreaks)

flrVsYr$stories<-10*trunc(flrVsYr$numFloor/10) 
flrVsYr$stories<-sapply(flrVsYr$stories,min,50) 
flrVsYr$num_built<-ave(flrVsYr$.count,flrVsYr$stories,FUN=cumsum) 
flrVsYr$perc_built<-flrVsYr$num_built/ave(flrVsYr$.count,flrVsYr$stories,FUN=sum) 
ggplot(flrVsYr,aes(x=yrBuilt,y=perc_built,group=stories))+
  geom_line()+ 
  facet_wrap(~stories)+
  theme(axis.text=element_text(size=5))

#question 3
#war time housing
flrVal<-condense(bin(yrBuilt[assessTot<5000000],5),z=valPerFloor[assessTot<5000000]) 
autoplot(flrVal)+xlab('')+ylab('$perfloor') 
ggsave("/users/bcarancibia/CUNY_IS608/lecture2/Question3.png", height=3, width=5)



