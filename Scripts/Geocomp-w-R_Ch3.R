##GEOCOMPUTATION WITH R
##Chapter 3: Attribute data operations

#load packages
library(sf)
library(terra)
library(dplyr)
library(spData)
library(tidyr)

#########
#3.2 VECTOR ATTRIBUTE MANIPULATION

methods(class = "sf")#methods for sf objects
#'`rbind()` binds rows of dataframes together
#'`$ <- ` creates new columns
#'create a new sf object
test_sf_obj <- st_sf(data.frame(n = world$name_long), g = world$geom)

#recap on understanding basic properties of vector data objects
class(world)#show the class of the sf object. its sf, tbl and data.frame
dim(world)#2D object with 177 rows and 11 columns
#world sf object has 10 non-geographic columns and one geom list column

world_df <- st_drop_geometry(world)#keep only the attributes and drop the geom
class(world_df)#no longer and sf object
ncol(world_df)#only 10 columns now

#sf incorporates tidyverse compatability but sp does not

#########
#3.2.1 VECTOR ATTRIBUTE SUBSETTING

#in Base R
world[1:6, ] #subset rows by position
world[, 1:3] #subset columns by position
world[1:6, 1:3]#subset rows and columns by position
world[, c("name_long", "pop")]#subset columns by name
world[, c(T, T, F, F, F, F, F, T, T, F, F)]#subset by logical indices
world[, 888]#index representing a non-existent column

#subset countries that are smaller than 10,000km^2
i_small <- world$area_km2 < 10000 #creates vector of logical operators
summary(i_small)#find out how many are TRUE or FALSE
small_countries <- world[i_small, ]#Subset based on this vector
#could also do this more concisely: 
small_countries <- world[world$area_km2 < 10000, ]
#can also do this with `subset()` which is base R
small_countries <- subset(world, area_km2 < 10000)

#now try subsetting the tidy way with dplyr
world1 <- dplyr::select(world, name_long, pop)
names(world1)#sticky geom column stays put
#subset all columns btwn name_long and pop (inclusive)
world2 <- dplyr::select(world, name_long:pop)
names(world2)
#all columns expept subregion and area_km2 (inclusive)
world3 <- dplyr::select(world, -subregion, -area_km2)#remove cols with `-`
names(world3)
#subset and rename columns at the same time with new_name = old_name syntax
world4 <- dplyr::select(world, name_long, population = pop)
names(world4)
#do the same thing in base R (less concise)
world5 <- world[, c("name_long", "pop")]#subset columns by name
names(world5)[names(world5) == "pop"] <- "population"
names(world5)
#select() also has some more helper functions for advanced subsetting (see help)

#extract a single column as a vector
pull(world, pop)#using dplyr
world$pop#base R way
world[["pop"]]#base R way also

#subset by row using dplyr
slice(world, 1:6)#subset rows 1 thru 6

#filter() in dplyr  = subset() in base R
#only keeps rows matching given criteria
#uses Boolean operators
world7 <- filter(world, area_km2 < 10000)#countries with a small area
world7 <- filter(world, lifeExp > 82)#countries with high life expectancy

######
#CHAINING COMMANDS WITH PIPES

world7 <- world |> 
  filter(continent == "Asia") |> #filter to only Asian countries
  dplyr::select(name_long, continent) |> #subset two columns
  slice(1:5) #get 1st 5 rows
#using pipes is in contrast to nested function calls
#harder to read but does the same thing
world8 <- slice(
  dplyr::select(
    filter(world, continent == "Asia"), 
    name_long, continent), 
  1:5)
#also in contrast to splitting the operations into multiple self-contained lines
#better for debugging 
#but verbose and cluttering global environment
world9_filtered = filter(world, continent == "Asia")
world9_selected = dplyr::select(world9_filtered, continent)
world9 = slice(world9_selected, 1:5)

######
#3.2.3 VECTOR ATTRIBUTE AGGREGATION

#aggregation is a form of data reduction
#useful first step when working with large datasets

#get populations of continents
world_agg1 <- aggregate(pop ~ continent, FUN = sum, data = world, 
                        na.rm = TRUE)#aggregate() is base R
class(world_agg1)# non-spatial data frame with one row for each continent
nrow(world_agg1)#6 rows
world_agg1#no antarctica and no open ocean 

world_agg2 <- aggregate(world["pop"], list(world$continent), FUN = sum, 
                        na.rm = TRUE)
class(world_agg2)#now it is sf and data frame
nrow(world_agg2)#8 rows (adds antarctica and open ocean)

#now do this the dplyr way
world_agg3 <- world |> 
  group_by(continent) |> 
  summarize(pop = sum(pop, na.rm = TRUE))
class(world_agg3)#sf, tibble, and dataframe
nrow(world_agg3)#8 rows
#calculate population but also area and # of countries in each continent
world_agg4 <- world |> 
  group_by(continent) |> 
  summarize(pop = sum(pop, na.rm = TRUE), 'area_sqkm' = sum(area_km2), n = n())
names(world_agg4)
world_agg4

#now really play w dplyr capability
world_agg5 <- world |> 
  st_drop_geometry() |> #drop the geometry to speed up operation
  dplyr::select(pop, continent, area_km2) |> #subset by these columns
  group_by(continent) |> #group by continent and summarize:
  summarize(Pop = sum(pop, na.rm = TRUE), Area = sum(area_km2), N = n()) |> 
  mutate(Density = round(Pop / Area)) |> #calculate pop density, add as new col
  slice_max(Pop, n = 3) |> #keep only the top 3 
  arrange(desc(N)) #arrance in order of largest to smallest N
world_agg5

############
#3.2.4 VECTOR ATTRIBUTE JOINING

#use the coffee_data example to learn about joins
#join coffee_data to world
#these datasets share a 'key variable', in thise case "name_long"
world_coffee <- left_join(world, coffee_data)#this function is dplyr
class(world_coffee)#still an sf, tibble, and DF
names(world_coffee)
#plot as a map
plot(world_coffee["coffee_production_2017"])

#must have a 'key variable' for joinging to work
#if key variable does not have same name, you have to either
#rename one of them
#or use the `by = ` argument to specify joining variables
coffee_renamed <- rename(coffee_data, nm = name_long)
world_coffee2 <- left_join(world, coffee_renamed, by = c(name_long = "nm"))
names(world_coffee2)
nrow(world_coffee2)#same number of rows as world, NA values for the coffee cols
nrow(coffee_data)#only 47 rows

#if you only want to keep the rows that have a match in the key variable
#use inner_join()
world_coffee_inner <- inner_join(world, coffee_data)
nrow(world_coffee_inner)#only 45 rows
#which rows did not match?
setdiff(coffee_data$name_long, world$name_long)
#"Others" and "Congo, Dem. Rep. of" are not names in the world dataset
#to fix, find the name that is in the world dataset
(drc <-  stringr::str_subset(world$name_long, "Dem*.+Congo"))
#create new version of coffee_data and update name
coffee_data$name_long[grepl("Congo,", coffee_data$name_long)] <- drc
world_coffee_match <- inner_join(world, coffee_data)
nrow(world_coffee_match)#now 46 rows

#can also join a simple features object to a non-spatial object
#but won't be an sf object
coffee_world <- left_join(coffee_data, world)
class(coffee_world)#tibble and df
names(coffee_world)#still has geom column but isn't ID'd as spatial by R
#can later coerce non-spatial data frames with a geom column into sf object
coffee_world <- st_as_sf(coffee_world)
class(coffee_world)#now is sf object oo

#################
#3.2.5 CREATING ATTRIBUTES AND REMOVING SPATIAL INFORMATION

#create new column based on already existing columns in base R
world_new <- world #don't overwrite OG data
world_new$pop_dens <- world_new$pop / world_new$area_km2
names(world_new)

#do this using dplyr
world_new2 <- world |> 
  mutate(pop_dens  = pop / area_km2)#add col and keep all the others
names(world_new2)

world_new2 <- world |> 
  transmute(pop_dens = pop / area_km2)#create new col and drop all the others
names(world_new2)#but keeps geom col

#can use the unite() function to combine cols 
world_unite <- world |> 
  tidyr::unite("con_reg", continent:region_un, sep = ":", remove = TRUE)
#the argument remove = TRUE removes the OG columns that were combined
names(world_unite)
world_unite

#can also separate columns
world_separate <- world_unite |> 
  tidyr::separate(con_reg, c("continent", "region_un"), sep = ":")
names(world_separate)

#rename columns dplyr way
world |> rename(name = name_long)
#now the base R way
new_names <-  c("i", "n", "c", "r", "s", "t", "a", "p", "l", "gP", "geom")
world_new_names <-  world |>
  setNames(new_names)
names(world_new_names)

#'to remove geometry to speed up aggregation, ONLY USE `st_drop_geometry()`
