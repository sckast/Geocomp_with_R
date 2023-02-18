#Geocomputation with R
#Chapter 3 - Exercises
##############

#load packages
library(sf)
library(dplyr)
library(terra)
library(spData)
data("us_states")
data("us_states_df")

names(us_states)

#exercise 1
us_states_name <- us_states %>% 
  select("NAME")
class(us_states_name) #class is sf and data.frame
#it is geographic because there is the sticky geometry column

#exercise 2
#subset 3 ways
us_states_pop <- us_states %>% 
  select(total_pop_10, total_pop_15)

us_states_pop2 <- us_states[, c("total_pop_10", "total_pop_15")]

us_states_pop3 <- us_states[, grep("pop", names(us_states))]

#exercise 3
#find all states that belong to the midwest region
unique(us_states$REGION)#figure out what the midwest region is called
midwest_states <- us_states %>% 
  filter(REGION == "Midwest")
midwest_states

midwest_states2 <- subset(us_states, REGION == "Midwest")

#find all states that belong to the West region, have an area below 250,000 km^2, 
#and in 2015 a population great 5,000,000 residents
us_states$AREA <- as.numeric(us_states$AREA)#remove units from AREA column
class(us_states$AREA)#check that it's numeric now instead of 'units' class
#subset base R method 1
west_states <- subset(us_states, REGION == "West" & total_pop_15 >5000000 &
                        AREA < 250000)
west_states#only Washington state fits these parameters

#subset base R method 2
west_states <- us_states[us_states$REGION == "West" & us_states$AREA < 250000 &
                            us_states$total_pop_15 > 5000000, ]
west_states#still just WA
#subset tidy method
west_states <- us_states %>% 
  filter(REGION == "West" &
           AREA <250000 &
           total_pop_15 > 5000000)
west_states#still...just...WA
#set units of AREA column back to km^2
west_states$AREA <- units::set_units(west_states3$AREA, km^2)
west_states$AREA#check column to see if units are there (they should be)
west_states#check whole df because it's small anyway 
#set units back to the AREA column of the whole us_states data frame
us_states$AREA <- units::set_units(us_states$AREA, km^2)
us_states$AREA#make sure that it worked as intended
class(us_states$AREA)#class is back to 'units'
