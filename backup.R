
####UNCCOMMENT IF NEEDED TO INSTALL
#install.packages('rworldmap',dependencies=TRUE) 
#install.packages(dplyr)
#install.packages(tidyverse)
#install.packages(maps)
#install.packages(mapproj)
#install.packages(rworldmap)
#install.packages(reshape)
#install.packages("hrbrthemes")

#library(plyr)
#library(dplyr) #used for dropping columns
#library(tidyverse)
#library(maps)
#library(mapproj)
#library(rworldmap)
#library(ggplot2)
#library(scales)
#library(reshape)
#library(hrbrthemes)


#########################################################
####### GETTING THE DATA  AND VIEWING THE DATA  ##########
##########################################################

#dataset 
#if having problem type getwd() to locate ur file directory and then copy and paste the file there
data <- read.csv('data/Africa_DatasetACLED.csv', header=TRUE)


#view the file
data


#view in clear tabular form
view(data)

#GETTING ONLY THE DATA FOR WEST AFRICA
WesternAfricaData <- data[which(data$region=="Western Africa"),] 


view(WesternAfricaData)


#########################################################
############## TIDYING ALL THE DATASETS   ###############
##########################################################

###GETTING INFO ABOUT THE DATASET ######

dim(WesternAfricaData) #19570 31
data.frame(colnames(WesternAfricaData)) #to get the column names
str(WesternAfricaData)
summary(WesternAfricaData)

#to show how many missing values
sum(is.na(WesternAfricaData)) 

##REMOVING MISSING VALUES
data <- na.omit(WesternAfricaData)


####TO CHANGE THE DATA POINTS TO NUMERIC
WesternAfricaData$longitude <- as.numeric(WesternAfricaData$longitude)
WesternAfricaData$latitude = as.numeric(WesternAfricaData$latitude)




#####DROPPING COLUMNS NOT NEEDED BECAUSE THEY ARE NOT AS IMPORTANT IN OBSERVATION FOR DATA
####DROPPING IRRELEVANT OBSERVATIONS 
WestAfricadroppedData <- select(WesternAfricaData, -c("data_id", "event_id_no_cnty" , "timestamp","notes","source_scale","source","geo_precision","inter1", "inter2", "interaction", "event_id_cnty", "assoc_actor_1","assoc_actor_2","event_date","time_precision", "admin3"))
WestAfricadroppedData 

colnames(WestAfricadroppedData)

# Remove duplicated rows
distinct(WestAfricadroppedData, iso, year, event_type, sub_event_type, actor1, actor2, region, country, admin1, admin2, location, latitude, longitude, fatalities, iso3)



######RENAMING COLUMNS TO MAKE IT EASIER TO KNOW WHAT ITS ABOUT######
WestAfricadroppedData<- WestAfricadroppedData %>% rename(belligerentOne = actor1)
WestAfricadroppedData <- WestAfricadroppedData %>% rename(belligerentTwo = actor2)
WestAfricadroppedData <- WestAfricadroppedData %>% rename(MainPlaceWarTook = admin1)
WestAfricadroppedData <- WestAfricadroppedData %>% rename(SecondaryPlaceTheWarTook = admin2)
WestAfricadroppedData


###GETTING THE UNIQUE VALUES 
unique(WestAfricadroppedData$event_type)
unique(WestAfricadroppedData$year)
unique(WestAfricadroppedData$region)
unique(WestAfricadroppedData$country)
unique(WestAfricadroppedData$fatalities)
unique(WestAfricadroppedData$sub_event_type)
unique(WestAfricadroppedData$belligerentOne)
unique(WestAfricadroppedData$belligerentTwo)



#####################################################################################################
############## THIS IS THE FINAL DATASET WITH AT LEAST 8 COLUMNS AND 200 OBSERVATIONS  ##############
#####################################################################################################

FinalData <- WestAfricadroppedData[row.names(WestAfricadroppedData) %in% 1:200, ]
dim(FinalData) #200 observations 15 columns




#########################################################
################### EXPLORING FINAL DATA   #############
##########################################################
view(FinalData)
dim(FinalData)
colnames(FinalData)
summary(FinalData)



#########################################################
###### Explore the data using charts and code   #########
##########################################################




#########################################################
########## MAKING CHARTS/ VISUALISATIONS   ###############
##########################################################






#################### TO CREATE A barplot ####################################

ggplot(data = WestAfricadroppedData, aes(x = country, y = year, fill = event_type)) +
  geom_bar(stat="identity")+
  geom_text(aes(y = year, label = year), vjust=1.6, 
            color="white", size=3.5)+
  scale_fill_brewer(palette="RdGy")+
  theme_minimal() + labs(title = "Highest occurance of events?")




#################### TO CREATE A WORLD MAP ####################################

world <- map_data("world")
world


#####REMOVED THE SUBREGION AS IT CONTAINED MISSING VALUES AND WAS NOT NEEDED
world <- select(world, -c("subregion"))
world

###CHECKING HOW MANY MISSING DATA
sum(is.na(world)) #0



####JOINed (BY Shorten version of countries name) MY AFRICA DATASET AND THIS WORLD DATASET SO I WOULD BE ABLE TO USE THE MAP Visualisation
sPDF <- joinCountryData2Map( WestAfricadroppedData,
                             joinCode = "ISO3"
                             ,nameJoinColumn = "iso3" )


####TO PLOT THE POINTS AND SHOW WORLD MAP
sPDF2 <-sPDF[which(sPDF$continent=="Africa"),]

ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),color = "black", fill = "lightgrey", size = 0.1
  )  + geom_point(data = sPDF2, aes(x = long, y = lat), color = "yellow", alpha = 0.1) +
  theme_void()  + labs(title="FOCUSING ON AFRICA") +  theme(text = element_text(color = "#FFFFFF")
                                                            ,plot.background = element_rect(fill = "#444444")
                                                            ,plot.title = element_text(size = 30)
  )


#SHOWS MAP OF AFRICA
sPDF <-sPDF[which(sPDF$continent=="Africa"),]
MapOfAfrica <- mapCountryData( sPDF
                , nameColumnToPlot="country"
                , colourPalette="rainbow"
                , mapTitle="Countries in West Africa"
) 


####get the country details
countryDetails <- WesternAfricaData %>% 
  select(event_type, region, longitude, 
         latitude, country, sub_event_type)
countryDetails

####MAKE THE POINTS WHERE EACH TIME OF WAR OCCURS ON THE MAP
ggplot() + geom_map(
  data = world, map = world,
  aes(long, lat, map_id = region),color = "black", fill = "black", size = 0.1
  )  + geom_point(
    data = countryDetails,
    aes(longitude, latitude, color =sub_event_type), alpha = 0.7
  )  + theme_void()  + labs(title="Highlighting Specific Events") +  theme(text = element_text(color = "white")
                                                            ,plot.background = element_rect(fill = "#444444")
                                                            ,plot.title = element_text(size = 30))





#################### TO CREATE A PIE CHART ####################################

####TO GET THE FREQUENCY 
####USED FINAL DATA TO GET FREQUENCY OF EVENT TYPE
count_event_type <-  count(FinalData['event_type'])


####TO CREATE A PIE CHART SHOWING THE FREQUENCY OF EVENT TYPE OCCURANCE
ggplot(count_event_type, aes(x="", y = freq, fill = event_type)) +
  geom_bar(stat = "identity", width = 1, color="black") +
  coord_polar("y", start=0) + theme_void() + guides(fill = guide_legend(title = "Violence Grouping")) + 
  labs(title="Frequency Representation of Violence in West Africa",size = 52, face = "bold") +  
  geom_text(aes(label = paste0(freq, "%")), position = position_stack(vjust=0.5)) + 
  coord_polar(theta = "y") +
  scale_fill_manual(values=c("#873600", "#CD961F", "#CD5E1F", "#E67E22", "#E59866", "#FF5733"))







############################ HEATMAP TO GET THE SIMILARITIES ###################
#################### TO CREATE A HEATMAP ####################################


######TO GET THE A SUBSET OF THE DATA WHERE THE COUNTRY IS NIGERIA
###TO CAOMPARE THE CONSEQUENCES OF THE SUBEVENTS  BASED ON THE MAIN LOCATION THE EVENT TOOK PLACE
NigeriaData <- FinalData[which(FinalData$country=="Nigeria"),]
view(NigeriaData)

ggplot(NigeriaData, aes(MainPlaceWarTook, event_type, fill= fatalities), scale="column") + 
  geom_tile(color = "black")+
  scale_fill_gradient(low="grey", high="darkred") +coord_fixed() +
  guides(fill = guide_colourbar(title = "Fatality Number of event")) + labs(title="CAUSUALITY OF EVENTS IN DIFFERENT CITIES IN NIGERIA",size = 52, face = "bold") 


####TO COMPARE THE VIOLENCE AGAINST CIVIIANS IN EACH OF THE CITIES IN NIGERIA IN 2022
heatMapData <- NigeriaData[which(NigeriaData$event_type=="Violence against civilians")] 
heatMapData
ggplot(heatMapData, aes(MainPlaceWarTook,event_type, fill= fatalities), scale="column") + 
  geom_tile()+
  scale_fill_gradient(low="white", high="red") +coord_fixed() +
  guides(fill = guide_colourbar(title = "Fatalities")) + labs(title="CONSEQUENCES OF VIOLENCE AGAINST CITIZENS",size = 52, face = "bold")  
