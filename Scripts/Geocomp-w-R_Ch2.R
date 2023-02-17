#load packages
library(sf) #classes and functions for vector data
library(terra) #classes and functions for raster data
library(spData) #geographic data
library(spDataLarge) #larger geographic data

#Vector data models: represent the world via points, lines, and polygons
#raster data models: represent the world by dividing earth surface into equally 
#sized cells - common in enviro sciences bc of dependence on remote sensing

#check out the vignettes 
vignette(package = "sf")
vignette("sf1")

#view the world dataset provided by spData
#world is an sf data frame (an sf object)
class(world)
names(world)

#world$geom is a list column of all of the coords for the country polygons
#plot the sf object 
plot(world)# ploting results in one map for each variable in the dataset

#treat sf objects as normal data frames with spatial powers
summary(world)
#geom feature is sticky
#stays with every variable unless user deliberatetly removes it
summary(world["lifeExp"])

#subset the world dataframe
#keeps the spatial features in the subset
#even when only taking the 1st 3 columns
world_mini <- world[1:2, 1:3]
world_mini

#sf package interfaces well with tidyverse
#import data as base R dataframe using `st_read()`
world_dfr <- st_read(system.file("shapes/world.shp", package = "spData"))
#import data as tibble using `read_sf()`
world_tbl <- read_sf(system.file("shapes/world.shp", package = "spData"))
class(world_dfr)#check class of each
class(world_tbl)

#create basic maps in sf using plot()
plot(world[3:6])#plot variables 3 thru 6
plot(world["pop"])#plot the population variable

#plots can be layered on top of each other with add = TRUE
#filter out Asia and set as single feature 
world_asia <- world[world$continent == "Asia", ]
asia <- st_union(world_asia)
#then add to world population plot
plot(world["pop"], reset = FALSE)
plot(asia, add = TRUE, col = "red")

#can add layers to the map
#e.g. circles representing populations of countries
plot(world["continent"], reset = FALSE)
cex <- sqrt(world$pop) / 10000
world_cents <- st_centroid(world, of_largest = TRUE)
plot(st_geometry(world_cents), add = TRUE, cex = cex)

#can also change the bounding box to highlight a country
india = world[world$name_long == "India", ]
plot(st_geometry(india), expandBB = c(0, 0.2, 0.1, 1), col = "gray", lwd = 3)
plot(st_geometry(world_asia), add = TRUE)

#create an sf object
lnd_point <- st_point(c(0.1, 51.5))                 # sfg object
lnd_geom <- st_sfc(lnd_point, crs = "EPSG:4326")    # sfc object
lnd_attrib <- data.frame(                           # data.frame object
  name = "London",
  temperature = 25,
  date = as.Date("2017-06-21")
)
lnd_sf <- st_sf(lnd_attrib, geometry = lnd_geom)    # sf object
lnd_sf
class(lnd_sf)

#play with s2
india_buffer_with_s2 <- st_buffer(india, 1)
sf_use_s2(FALSE) #turn s@ off
india_buffer_without_s2 <- st_buffer(india, 1)
#compare maps of both
plot(india_buffer_with_s2)
plot(india_buffer_without_s2)
#turn s2 back on 
sf_use_s2(TRUE)

###################################
##RASTER DATA

#raster data is cells (also called pixels) of constant size. 
#each cell has a value, but can only hold a single value. 
#usually used to represent continuous data (e.g. elevation)
#can also represent categorical data but vector models often better for this. 
#terra and stars packages both work w raster data.
#terra evolved from its predecessor raster and is faster

#get to know terra
#srtm.tig is a DEM of Zion Nat'l Park
raster_filepath <- system.file("raster/srtm.tif", package = "spDataLarge")
my_rast <- rast(raster_filepath)
class(my_rast) #class is SpatRaster
#SpatRaster is the class that represents raster objects in terra

#get to know a few terra functions 
dim(my_rast)#returns the # of rows, columns, and layers
ncell(my_rast)#returns the number of cells (pixels)
res(my_rast)#returns the spatial resolution
ext(my_rast)#returns the spatial extent
crs(my_rast)#returns the coordinate reference system 
inMemory(my_rast)#reports whether raster data is stored in memory or on disk
help("terra-package")#full list of all available terra functions

#map-making with terra
plot(my_rast)

#simplest way to read in raster file from disk or server: 
single_raster_file <- system.file("raster/srtm.tif", package = "spDataLarge")
single_rast <- rast(raster_filepath)

#can also create rasters from scratch with `rast()`
#fits cells row-wise (unlike matrix())
#centered around Prime Meridian and Equator - that is what xmin/max and 
#ymin/max refer to
new_raster <- rast(nrows = 6, ncols = 6, 
                   xmin = -1.5, xmax = 1.5, ymin = -1.5, ymax = 1.5, 
                   vals = 1:36)
#resolution determined by # rows/columns and extent
#must be 0.5 because 6 rows/cols and -1.5 to 1.5 extent for x and y
#unit of resolution is that of the underlying CRS
#default CRS of raster objects is WGS84
#can specify with `crs` argument
plot(new_raster)#view it

#SpatRaster class can also handle multiple laters
multi_raster_file <- system.file("raster/landsat.tif", package = "spDataLarge")
multi_rast <- rast(multi_raster_file)
multi_rast# 4 layers instead of 1 as with the Zion raster
nlyr(multi_rast)#get the number of layers
plot(multi_rast)#makes for plots, one of each landsat layer
#can subset layers from multi-layer raster objects
multi_rast3 <- subset(multi_rast, 3)#using layer number
multi_rast4 <- subset(multi_rast, "landsat_4")#using layer name
#can also combine several SpatRaster objects into one using `c()`
multi_rast34 <- c(multi_rast3, multi_rast4)
nlyr(multi_rast34)#now 2 layers

#important to know that most SpatRaster objects do not actually store 
#raster values
#rather they point to the file itself
#so you cannot directly save to ".rds." or ".rda" 
#have to use the `wrap()` function to save as temp objects that can be 
#saved as R objects or used in cluster computing (whatever that means)
#or use the `writeRaster()` function to save object as regular raster

########################################
#GEOGRAPHIC AND PROJECTED COORDINATE REFERENCE SYSTEMS

#ellipsoidal models use equatorial radius and polar radius 
#equatorial radius is ~11.5 km longer than polar (earth is not a sphere)

#geocentric datum is centered around center of gravity
#therefore is not optimized for any specific location 

#local datum (e.g. NAD83) means ellipsoidal surface is shifted to align with
#surface at a particular location
#but then is not accurate for other locations using that exact datum
#see figure 2.16 

#projected CRS translates 3D into 2D (planar) units
#lose some accuracy in some dimensions
#each projection tries to preserve some dimension at the cost of others
#conic, cylindrical, planar (azimuthal) are the types
sf_proj_info(type = "proj")#view available projections in PROJ library
#also check out www.geo-projections.com/ to see them! very helpful
#must know now which CRS my data is in
#whether it is geographic (lat/long) or projected (usually meters)
#' `st_crs()` to query CRS of sf objects
#' `crs()` to query CRS of terra objects

########################################
#UNITS

#must know units of your map
#good practice to put scale bar somewhere
#sf objects have native support for units
luxembourg <- world[world$name_long == "Luxembourg", ]#subset Luxembourg
st_area(luxembourg)#get the area of luxembourg. It shows units
#divide by 1 million to get area in km^2
st_area(luxembourg) / 1000000#but units wrong bc still m^2
#so specify units 
units::set_units(st_area(luxembourg), km^2)#now units show in km^2

#terra package does not have native support for units
#so need to know the units of your projection
#because that is what the units of your resolution are
#e.g. WGS84 is in decimal degrees but UTM is in meters - just need to know this


