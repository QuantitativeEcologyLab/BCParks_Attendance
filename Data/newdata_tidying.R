library(tidyr)
library(readr)
library(ggplot2)

#Source in historical climate data
source("~/Desktop/bio/440/BCParks_Attendance/bind-historical-climate-data.R")


#Import Camping and Day-use data 
setwd("~/Desktop/bio/440/BCParks_Attendance/Data")
camping <- read.csv("camping.csv", na = "0", check.names = FALSE)
dayuse <- read.csv("dayuse.csv", na = "0", check.names = FALSE)
#Convert from wide to long format
camping <- gather(camping, date, visitortotal, "2010-01-01":"2019-12-01", factor_key = TRUE)
dayuse <- gather(dayuse, date, visitortotal, "2010-01-01":"2019-12-01", factor_key = TRUE)
#Remove months with no values (NAs)
camping <- na.omit(camping)
dayuse <- na.omit(dayuse)
#Classify type of attendance
camping$attendancetype <- "camping"
dayuse$attendancetype <- "dayuse"
#Merge day use and camping information into one dataset
bcparks <- rbind(camping,dayuse) 


#ADD NEW COLUMNS: MONTH AND YEAR
bcparks <- bcparks %>%
  separate(date, sep="-", into = c("year", "month", "day"))
#remove day column (it's an arbitrary number)
bcparks = subset(bcparks, select = -c(day))
#bring back the date column (maybe won't need it?)
bcparks$date <- paste(paste(bcparks$year, bcparks$month, sep = "-"), 15, sep = "-")
bcparks$date <- as.POSIXct(bcparks$date, format = "%Y-%m-%d")
bcparks$month <- as.numeric(bcparks$month)

#ADD NEW COLUMN: BC POPULATION
#import historic population data
population_records <- read_csv("population_records.csv")
#remove unnecessary growth rate column (maybe?)
population_records = subset(population_records, select =-c(3))
#add historic population as a column
bcparks <- merge(bcparks,population_records,by=c("year"))


#ADD NEW COLUMN: ATTENDANCE CORRECTED FOR POPULATION SIZE
bcparks$attendance <- bcparks$visitortotal/bcparks$BCpop*1000


#ADD NEW COLUMNS: LATITUDE AND LONGITUDE COORDINATES
#import park coordinate data
park_coordinates <- read_csv("park_coordinates.csv")
#add latitude and longitude as columns
bcparks <- merge(bcparks,park_coordinates, by=c("park"))


#ADD NEW COLUMNS: AVG MONTHLY TEMPERATURES/DAILY PRECIPITATION NEAR PARK
#Get rid of unnecessary columns in historical climate dataset
historicaldata = select(historicaldata, -c(dec_date, elevation))
#Change avgprecip from meters to mm
historicaldata$avgprecip <- historicaldata$avgprecip*1000
#Merge the datasets
bcparks <- merge(x=bcparks,y=historicaldata, 
             by=c("latitude","longitude", "year", "month"), all.x=TRUE)


#ADD NEW ROWS: OKANAGAN DATA FROM 2019-2022
#Source in Okanagan data and classify region/type of attendance
source("~/Desktop/bio/440/BCParks_Attendance/Data/modelfinal.R")
ok2019_2022$region <- "ok"
ok2019_2022$attendancetype <- "dayuse"
#Add in coordinates for each park 
ok2019_2022 <- ok2019_2022 %>%
  mutate(latitude = case_when(
    park == "Fintry Park" ~ 50.138676,
    park == "Skaha Bluffs Park" ~ 49.433939,
    park == "E.C. Manning Park" ~ 49.119421
  ))
ok2019_2022 <- ok2019_2022 %>%
  mutate(longitude = case_when(
    park == "Fintry Park" ~ -119.501595,
    park == "Skaha Bluffs Park" ~ -119.546110,
    park == "E.C. Manning Park" ~ -120.856781
  ))
#Bind the datasets
bcparks <- rbind(bcparks,ok2019_2022) 


#Clean up environment
rm(camping,dayuse,ok2019_2022,park_coordinates,M,population_records,historicaldata)
