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

#find all states that belong to the south region, have an area larger than 
#150000 km^2 OR a total population in 2015 larger than 7,000,000
us_states$AREA <- as.numeric(us_states$AREA)#remove units from AREA column
#subset
south_states <- us_states %>% 
  filter(REGION == "South" &
           (AREA > 150000 | total_pop_15 > 7000000))
#set units back for the subset and the full US df
south_states$AREA <- units::set_units(south_states$AREA, km^2)
us_states$AREA <- units::set_units(us_states$AREA, km^2)

#exercise 4
#what was the total population in 2015 in the us_states dataset?
total_pop <- sum(us_states$total_pop_15)
total_pop#314,375,347 people

#what was the max and min total pop in 2015?
min(us_states$total_pop_15)#579,679 people
max(us_states$total_pop_15)#38,421,464 people

#exercise 5
#how many states are there in each region
num_states <- us_states %>% 
  select(REGION, NAME) %>% #subset the columns
  group_by(REGION) %>% #group by region
  summarize(NUM_STATES = n()) %>% #get the number of states in each region
  mutate(NUM_STATES) #add a column with the number of states in each region
num_states 

#exercise 6
#What was the minimum and maximum total population in 2015 in each region? 
pop_region <- us_states %>% 
  select(REGION, total_pop_15) %>% #subset the columns I need
  group_by(REGION) %>% #group by region
  #get the min and max pops
  summarize(MIN_POP = min(total_pop_15), MAX_POP = max(total_pop_15)) %>%
  mutate(MIN_POP = MIN_POP, MAX_POP = MAX_POP)
pop_region

#What was the total population in 2015 in each region?
total_pop_region <- us_states %>% 
  select(REGION, total_pop_15) %>% #subset the columns I need
  group_by(REGION) %>% #group by region
  summarize(pop = sum(total_pop_15)) %>% #get the sum of the pops in each region
  mutate(TOTAL_POP = pop) #add a column with the total pop for each region
total_pop_region              

#exercise 7
#Add variables from us_states_df to us_states, 
#and create a new object called us_states_stats. 
names(us_states_df)
names(us_states)
us_states_stats <- left_join(us_states_df, us_states, 
                             by = c(state = "NAME"))
us_states_stats
#What function did you use and why? 
#I used left join and put the us_states_df first in order to preserve all 51 
#observations even though 2 are missing from us_states

#Which variable is the key in both datasets? 
#'NAME' in us_states and 'state' in us_states_df

#What is the class of the new object?
class(us_states_stats) #tibble and dataframe. no longer an sf object. 

#exercise 8
#us_states_df has two more rows than us_states. 
#How can you find them? (hint: try to use the dplyr::anti_join() function)
us_states_df <- rename(us_states_df, NAME = state)
anti_join(us_states, us_states_df, 
          by = "NAME")
