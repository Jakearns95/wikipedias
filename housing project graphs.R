install.packages("ggplot2", type = "source", dependencies = TRUE)

library(ggplot2)
library(plyr)
install.packages("lubridate")
library(lubridate)


#import data sets (Pulling in raw housing data and col names)
housingData = read.csv(file.choose(), header = T)
names = read.csv(file.choose(), header = T)

#Change over col headings
names(housingData) = names[,3]

#modify the date column
housingData$`Permit Authorization Date` = sapply(housingData$`Permit Authorization Date`, as.character)
housingData$`Permit Authorization Date` = ymd(paste(housingData$`Permit Authorization Date`,"01", sep = ""))

#housing data script 

head(housingData)

#chart for displaying dates
with(housingData, hist(housingData$'Permit Authorization Date'))
startDataHist <- qplot(STRT, data= housingData, color= "red", xlim = c(201100, 201700))
startDataHist

#chart for seeing most common location
qplot(Division, data = housingData, col = "blue", binwidth = 1, bins = 10)
with(housingData, hist(housingData$Division))
h <- hist(housingData$Division, breaks = 10, col= "blue", xlab = "Division number", main = "Histogram of Division/location of houses",
          ylim = c(0, 6000))
count(housingData, 'Division')
count(housingData, housingData$'Division')

barplot_div <- barplot(table(housingData$Division), col = "blue", ylim = c(0, 6000), xlab = "Division", ylab = "Count")
#barplot('Division', housingData$'Division'.count, data = housingData, ylim = c(0, 6000))


#try to create a table that shows the count 

 #count()
divCountTable <- count(housingData, 'Division' )
divCountTable


#look at square feet vs. number of fireplaces
qplot(housingData$Fireplace, housingData$'Square Foot Area of House', data = housingData, color = "blue")
qplot(housingData$'Square Foot Area of House', housingData$Fireplace, data = housingData, color = "red")
plotFirePlacesSizeLocation <- qplot(Fireplace, 'Square Foot Area of House', data = housingData, facets = Division~.)
plotFirePlacesSizeLocation

#heating method vs square feet, divided by division 
qplot(HEAT, SQFS, data = housingData, color = "red", facets = DIV~.)

#look at start date versus location
qplot(housingData$'Permit Authorization Date', housingData$Division, data = housingData, color = "blue")
qplot(housingData$'Permit Authorization Date', Division, data = housingData, color = "blue")

qplot(housingData$Division, data= housingData, facets = housingData$`Metropolitan Area`~.)
qplot(Division, data= housingData, facets = housingData$`Metropolitan Area`~.)

barplot(table(housingData$`Primary Space Heating System`), col = "blue", xlab = "Heating Method", ylab = "Count")

