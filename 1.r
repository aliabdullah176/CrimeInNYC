setwd("D:/Study/OneDrive - The University of Texas at Dallas/Winter break 1/MIS R")
devtools::install_github("tidyverse/purrr")
library(purrr)
library(caret); library(MASS)
library(data.table); library(caret); library(ROCR); library(ggplot2); library(car); library(rpart) 
library(parallel); library(doParallel)
library(sqldf); library(dplyr)
#install.packages("date")
library(date)
library(lubridate)
library(tidyr)

#install.packages("")

#caret::train()

rawdata <- fread("Dataset.csv", stringsAsFactors = TRUE, na.strings='')
rawdata <- rawdata %>% drop_na(CMPLNT_FR_TM)
rawdata <- rawdata %>% drop_na(CMPLNT_FR_DT)
t1 <- strptime(rawdata$CMPLNT_FR_TM, format = "%H:%M:%S")
summary(as.factor(rawdata$hours))
rawdata$hours <- hour(t1)
t2 <- strptime(rawdata$CMPLNT_FR_DT, format = "%m/%d/%Y")
rawdata$date <- lubridate::day(t2)
rawdata$month <- lubridate::month(t2)
rawdata$year <- lubridate::year(t2)
head(rawdata,1)

set.seed(176)
indx <- createDataPartition(rawdata$CMPLNT_NUM, p = .01, list = F)
rawdata1 <- ( rawdata[indx, ] )
sort(summary(rawdata1$OFNS_DESC))

rawdata2 <- sqldf(
"SELECT * FROM rawdata WHERE OFNS_DESC IN 
('PETIT LARCENY','HARRASSMENT 2', 'ASSAULT 3 & RELATED OFFENSES', 
'CRIMINAL MISCHIEF & RELATED OF','GRAND LARCENY', 'DANGEROUS DRUGS')")

rawdata2 <- droplevels(rawdata2)
rawdata2 <- as.data.table(rawdata2)
summary(rawdata2$OFNS_DESC)

# write.csv(rawdata2,file="filtered.csv")
# rawdata2 <- fread("filtered.csv")
library(ggmap)
map_str <- get_map(location = "staten island", zoom = 12, maptype = "toner")
ggmap(map_str, extent = "device") + 
  geom_density2d(data = rawdata1[rawdata1$BORO_NM=="STATEN ISLAND",], aes(x = Longitude, y = Latitude), size = 0.3) + 
  stat_density2d(data = rawdata1[rawdata1$BORO_NM=="STATEN ISLAND",], aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..), size = 0.01, bins = 25, geom = "polygon") + 
  scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0, 0.3), guide = FALSE) 

map_str1 <- get_map(location = "Bronx", zoom = 12, maptype = "toner")
ggmap(map_str1, extent = "device") + 
  geom_density2d(data = rawdata1[rawdata1$BORO_NM=="BRONX",], aes(x = Longitude, y = Latitude), size = 0.3) + 
  stat_density2d(data = rawdata1[rawdata1$BORO_NM=="BRONX",], aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..), size = 0.01, bins = 25, geom = "polygon") + 
  scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0, 0.3), guide = FALSE) 

map_str2 <- get_map(location = "Manhattan", zoom = 12, maptype = "toner")
ggmap(map_str2, extent = "device") + 
  geom_density2d(data = rawdata1[rawdata1$BORO_NM=="MANHATTAN",], aes(x = Longitude, y = Latitude), size = 0.3) + 
  stat_density2d(data = rawdata1[rawdata1$BORO_NM=="MANHATTAN",], aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..), size = 0.01, bins = 25, geom = "polygon") + 
  scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0, 0.3), guide = FALSE) 

map_str3 <- get_map(location = "brooklyn", zoom = 12, maptype = "toner")
ggmap(map_str3, extent = "device") + 
  geom_density2d(data = rawdata1[rawdata1$BORO_NM=="BROOKLYN",], aes(x = Longitude, y = Latitude), size = 0.3) + 
  stat_density2d(data = rawdata1[rawdata1$BORO_NM=="BROOKLYN",], aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..), size = 0.01, bins = 25, geom = "polygon") + 
  scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0, 0.3), guide = FALSE)

map_str4 <- get_map(location = "Queens", zoom = 12, maptype = "toner")
ggmap(map_str4, extent = "device") + 
  geom_density2d(data = rawdata1[rawdata1$BORO_NM=="QUEENS",], aes(x = Longitude, y = Latitude), size = 0.3) + 
  stat_density2d(data = rawdata1[rawdata1$BORO_NM=="QUEENS",], aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..), size = 0.01, bins = 25, geom = "polygon") + 
  scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0, 0.3), guide = FALSE)

ggplot(rawdata1, aes(x = BORO_NM, fill = LAW_CAT_CD)) +
  geom_bar(stat='count', position="dodge") +
  labs(x = 'Location', y = 'count of crime')+theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

ggplot(rawdata2, aes(x = BORO_NM, fill = OFNS_DESC)) +
  geom_bar(stat='count', position="dodge") +
  labs(x = 'Location', y = 'count of crime')+theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(OFNS_DESC~.) + theme(legend.position="bottom")

ggplot(rawdata1[rawdata1$year >= 2006,], aes(x = year)) +
  geom_line(stat='count', position="dodge", color="darkred") +
  labs(x = 'Year', y = 'count of crime')

ggplot(rawdata1, aes(x = date, color=LAW_CAT_CD, fill = LAW_CAT_CD)) +
  geom_line(stat='count', position="dodge", size =1) +
  labs(x = 'Date Index', y = 'count of crime')

ggplot(rawdata1, aes(x = (hours), fill= LAW_CAT_CD)) +
  geom_bar(stat='count', position = "dodge") +
  labs(x = 'hour of day', y = 'crime count')

ggplot(rawdata1, aes(x = (hours))) +
  geom_bar(stat='count', position = "dodge", fill="darkred") +
  labs(x = 'time', y = ' ')

ggplot(rawdata2, aes(x = LAW_CAT_CD, fill =BORO_NM)) +
  geom_bar(stat='count', position="fill") +
  labs(x = 'level of offense', y = ' ')

ggplot(rawdata2, aes(x = reorder(OFNS_DESC))) +
  geom_bar(stat='count') +
  labs(x = 'level of offense', y = ' ') +theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(rawdata2, aes(x = (hours), fill= OFNS_DESC)) +
  geom_bar(stat='count', position = "dodge") +
  labs(x = 'hour of day', y = 'crime count') + facet_grid(BORO_NM~.)

ggplot(rawdata2[rawdata2$BORO_NM != "NA",], aes(x = (hours), fill= BORO_NM)) +
  geom_bar(stat='count', position = "dodge") +
  labs(x = 'hour of day', y = 'crime count') + facet_grid(OFNS_DESC~.)

deptvslaw <- sqldf("select JURIS_DESC, LAW_CAT_CD, count(*) from rawdata2 group by JURIS_DESC, LAW_CAT_CD ")
write.csv(deptvslaw, file = "123.csv",row.names=F)
