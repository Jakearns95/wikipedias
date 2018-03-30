############################################
# Analyizing bedrooms vs price over time   #
# 3/25/18                                  #
# Data Set: Census housing microdata 2016  #
############################################

#small multiples
#import packages
install.packages(c("lubridate","ggplot2"))

#load packages
library(lubridate)
library(ggplot2)

#import data sets (Pulling in raw housing data and col names)
mydata = read.csv("soc16.csv", header = T)
names = read.csv("socmicro_variables.csv", header = T)

#Change over col headings
names(mydata) = names[,3]

#modify the date column
mydata$`Permit Authorization Date` = sapply(mydata$`Permit Authorization Date`, as.character)
mydata$`Permit Authorization Date` = ymd(paste(mydata$`Permit Authorization Date`,"01", sep = ""))

#checking for NAs
mydata[mydata == 0] = NA 
sum(is.na(mydata$`Laundry Location`))

#Location vs size
qplot(`Square Foot Area of House`, data = mydata, facets =mydata$Division~. ) 

qplot(Division, data = mydata, facets =mydata$`Metropolitan Area`~. ) 



#https://www.inman.com/2011/10/27/10-states-with-biggest-houses/
#http://www.jstor.org/stable/pdf/1924995.pdf?refreqid=excelsior%3Acd9a39b4b3f36dc6092e345962cedd7f 
#https://www.nytimes.com/2017/08/24/realestate/homes-built-united-states.html
#https://www.usnews.com/opinion/economic-intelligence/2014/08/05/which-states-are-building-the-most-homes-and-why












#laundry location
with(mydata, hist(`Laundry Location`, axes = F))
axis(side = 1, at=seq(1,6,1), labels = c("Basement","First","Second+","Garage","None","Multiple"))
qplot(mydata$`Square Foot Area of House`, data = mydata, facets =mydata$`Laundry Location`~. )
qplot(mydata$`Laundry Location`, data = mydata, facets =mydata$Stories~. )

#Ploting bedroom data
with(mydata, hist(Bedrooms))

with(mydata$BEDR, plot(mydata$`Permit Authorization Date`,mydata$Bedrooms))

#Data vs value broken up by bedroom size
qplot(`Lot Value`, `Permit Authorization Date`, data = mydata, facets = Bedrooms~. ) 

#GGPLOT
with(mydata, plot(Bedrooms, mydata$`Sales Price`, xlab= "Bedrooms", ylab= "Price in USD",
                    main = "House shit"))

qplot(Bedrooms, mydata$`Lot Value`, data = mydata, xlab= "Bedrooms", ylab= "Price in USD",
      main = "House Shit", color = "Blue")

