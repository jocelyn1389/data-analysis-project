library("readxl")
library(ggplot2)
library(data.table)
library(dplyr)
library(tidyr)
library(maps)
library(rworldmap)


#Import CPI data
CPI <- read_excel("CPI2019.xlsx", sheet = "CPI Timeseries 2012 - 2019")
head(CPI)

#Change the CPI Score column names to indicate years only
names(CPI)[names(CPI) == "CPI Score 2012"] <- "2012"
names(CPI)[names(CPI) == "CPI Score 2013"] <- "2013"
names(CPI)[names(CPI) == "CPI score 2014"] <- "2014"
names(CPI)[names(CPI) == "CPI score 2015"] <- "2015"
names(CPI)[names(CPI) == "CPI score 2016"] <- "2016"
names(CPI)[names(CPI) == "CPI score 2017"] <- "2017"
names(CPI)[names(CPI) == "CPI score 2018"] <- "2018"
head(CPI)

#Update the Region column with full region names
CPI$Region[CPI$Region=='AP'] <- "Asia-Pacific"
CPI$Region[CPI$Region=='WE/EU'] <- "West-Europe"
CPI$Region[CPI$Region=='MENA'] <- "Middle-East&North-Africa"
CPI$Region[CPI$Region=='AME'] <- "Americas"
CPI$Region[CPI$Region=='ECA'] <- "Europe&Central-Asia"
CPI$Region[CPI$Region=='SSA'] <- "South-Africa"

#Select CPI data from 2018
CPI <- CPI %>% select(1,2,3,8)
head(CPI)


#Import HDI data and select data from 2018
HDI <- fread('HDI.csv',header = TRUE)
HDI <- HDI %>% select(1,2,31)
head(HDI)
summary(HDI)

#Merge CPI and HDI data tables together
data2018 <- left_join(CPI,HDI,by="Country")
head(data2018)

#Update column names of CPI index and HDI index
names(data2018)[names(data2018) == "2018.x"] <- "CPI"
names(data2018)[names(data2018) == "2018.y"] <- "HDI"
head(data2018)
summary(data2018)

#Check Null Values in the join table
CPI_null_df <- data2018[is.na(data2018$CPI),]
CPI_null_df
HDI_null_df <- data2018[is.na(data2018$HDI),]
HDI_null_df

#Manually add the null HDI for some countries
data2018$HDI[data2018$Country=='United States of America'] <- "0.92"
data2018$HDI[data2018$Country=='Korea, South'] <- "0.906"
data2018$HDI[data2018$Country=='Czech Republic'] <- "0.891"
data2018$HDI[data2018$Country=='Tanzania'] <- "0.528"
data2018$HDI[data2018$Country=='Vietnam'] <- "0.693"
data2018$HDI[data2018$Country=="Cote d'Ivoire"] <- "0.516"
data2018$HDI[data2018$Country=='Eswatini'] <- "0.608"
data2018$HDI[data2018$Country=='Moldova'] <- "0.711"
data2018$HDI[data2018$Country=='Bolivia'] <- "0.703"
data2018$HDI[data2018$Country=='Laos'] <- "0.604"
data2018$HDI[data2018$Country=='Russia'] <- "0.824"
data2018$HDI[data2018$Country=='Iran'] <- "0.797"
data2018$HDI[data2018$Country=='Democratic Republic of the Congo'] <- "0.459"
data2018$HDI[data2018$Country=='Guinea Bissau'] <- "0.461"
data2018$HDI[data2018$Country=='Venezuela'] <- "0.726"
data2018$HDI[data2018$Country=='Syria'] <- "0.549"

HDI_null_df <- data2018[is.na(data2018$HDI),]
HDI_null_df

#Convert HDI column to numberic type
data2018$HDI <-as.numeric(data2018$HDI)
#data2018 <- na.omit(data2018) 
summary(data2018)
data2018

#Draw World Map for CPI 2018
worldmap <- getMap(resolution = "coarse")
plot(worldmap, xlim = c(-80, 160), ylim = c(-50, 100), 
     asp = 1, bg = "white", col = "black", fill = T)

# combine data frame with map
CPIMap <- joinCountryData2Map(data2018, joinCode = "ISO3",nameJoinColumn = "ISO3")

mapParams <- mapCountryData(CPIMap, 
                            nameColumnToPlot="CPI",
                            oceanCol = "white",
                            catMethod = "pretty",
                            missingCountryCol = gray(.8),
                            colourPalette = "heat",
                            addLegend = TRUE,
                            mapTitle = "Corruption Perceptions Index 2018",
                            borderCol = "White")


#Draw World Map for HDI 2018
worldmap <- getMap(resolution = "coarse")
plot(worldmap, xlim = c(-80, 160), ylim = c(-50, 100), 
     asp = 1, bg = "white", col = "black", fill = T)

# combine data frame with map
HDIMap <- joinCountryData2Map(data2018, joinCode = "ISO3",nameJoinColumn = "ISO3")

mapParams <- mapCountryData(HDIMap, 
                            nameColumnToPlot="HDI",
                            oceanCol = "white",
                            catMethod = "pretty",
                            missingCountryCol = gray(.8),
                            colourPalette = "heat",
                            addLegend = TRUE,
                            mapTitle = "Human Development Index 2018",
                            borderCol = "White")



#Create Plots
pl <- ggplot(data2018,aes(x=CPI,y=HDI,color=Region)) + geom_point()
pl

#Change the shape of the points
pl2 <- ggplot(data2018,aes(x=CPI,y=HDI,color=Region)) + geom_point(size=4,shape=1)
pl2

#Add a trend line
pl2 + geom_smooth(aes(group=1))

#Add a line for linear regression
pl3 <- pl2 + geom_smooth(aes(group=1),method ='lm',formula = y~log(x),se=FALSE,color='red')
pl3

#Highlight labels for selected countries
pointsToLabel <- c("Russia", "Venezuela", "Iraq", "Myanmar", "Sudan",
                   "Afghanistan", "Congo", "Greece", "Argentina", "Brazil",
                   "India", "Italy", "China", "South Africa", "Spane",
                   "Botswana", "Cape Verde", "Bhutan", "Rwanda", "France",
                   "United States", "Germany", "Britain", "Barbados", "Norway", "Japan",
                   "New Zealand", "Singapore")

pl4 <- pl3 + geom_text(aes(label = Country), color = "gray20", data = subset(data2018, Country %in% pointsToLabel),check_overlap = TRUE)
pl4

#Chnage background to white color
pl5 <- pl4 + theme_bw() 
pl5

#Add y axis label and x axis label
pl6 <- pl5 + scale_x_continuous(name = "Corruption Perceptions Index, 2018 (100=least corrupt)",limits = c(10, 95))
pl6

pl7 <- pl6 + scale_y_continuous(name = "Human Development Index, 2018 (1=Best)",limits = c(0.35, 1.0))
pl7

#Add title for the plot
pl7 + ggtitle("Corruption and Human development")

#Change the theme of the picture
library(ggthemes)
pl7 + theme_economist_white()
