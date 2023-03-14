#Geocomputation with R
#Chapter 4 - Exercises
##############

#load packages
library(sf)
library(dplyr)
library(spData)
library(terra)

#exercise 1
#number of high points in the canterbury region
canterbury <- nz %>% #using dplyr
  filter(Name == "Canterbury")#pull out records from nz with Name of Canterbury
canterbury_height <- nz_height[canterbury, ]#pull out the highest points in Canterbury
other_height <- nz_height[canterbury, , op = st_disjoint]#pull out the highest points not in Canterbury
nrow(canterbury_height)#number of observations in canterbury subset 
nrow(unique(canterbury_height))#are any duplicates?

#plot the result
#make canterbury yellow
#make high points in canterbury red crosses
#make the high points not in canterbury blue c
{plot(st_geometry(canterbury), col = "yellow",
      expandBB = c(0.6, 0.2, 2.5, 0.2))
  plot(st_geometry(nz), add = TRUE)
  plot(st_geometry(canterbury_height), 
       col = "red", pch = 3, add = TRUE)
  plot(st_geometry(other_height), col = "blue", add = TRUE)
}

#exercise 2
#which region has the second highest number of nz_height points?
nz_highs_n <- st_join(x = nz, y = nz_height) %>% 
  group_by(Name) %>% 
  summarize(n = n()) %>% 
  arrange(desc(n))
nz_highs_n#region with 2nd highest is West Coast
#how many does it have? 22 peaks
#use slice()

#Exercise 3
nz_regions <- nz %>% 
  group_by(Name)

#Exercise 4

