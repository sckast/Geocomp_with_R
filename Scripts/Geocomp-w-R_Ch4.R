#load packages
library(sf)
library(terra)
library(dplyr)
library(spData)
library(ggplot2)

#read in datasets for later use
elev <- rast(system.file("raster/elev.tif", package = "spData"))
grain <- rast(system.file("raster/grain.tif", package = "spData"))

#spatial subsetting 
canterbury <- nz %>% #using dplyr
  filter(Name == "Canterbury")#pull out records from nz with Name of Canterbury
canterbury_height <- nz_height[canterbury, ]#pull out the highest points in Canterbury
canterbury_height
#second method is base subsetting technique (x[y, ])
#subsets target x using the contents of source object y

nz_height[canterbury, , op = st_disjoint]#return the highest points that are NOT in Canterbury
#st_disjoint returns features that do not touch the source object (y)
#st_intersects returns features that touch, cross, or are within the subsetting object

#spatial subsetting using objects returned by topological operators
sel_sgbp <- st_intersects(x = nz_height, y = canterbury)
class(sel_sgbp)#sgpb object and also list
#sgpb stands for sparse geometry binary predicate, a list of length x in the 
#spatial operation
#basically it says if that row has a value or not for the source subsetting object
#e.g. if not, then it says (empty). If yes it returns 1
#can also use logicals
sel_logical <- lengths(sel_sgbp) > 0#get a list of logicals
#true means there is a value there, false means there is not
canterbury_height2 <- nz_height[sel_logical, ]#subset to only the TRUE values
#lengths() identifies which features in the object x intersect with any objects in y
#could also do the following to get the same thing as sel_logical
#sparse = false means return a dense matrix not a sparse one
st_intersects(x = nz_height, y = canterbury, sparse = FALSE)[, 1]
#can also subset using the st_filter() function
canterbury_height3 <- nz_height %>% 
  st_filter(y = canterbury, .predicate = st_intersects)


#Topical Relations
polygon_matrix <- cbind(#create a matrix
  x = c(0, 0, 1, 1, 0), 
  y = c(0, 1, 1, 0.5, 0)
)
#turn matrix into an sfc object
polygon_sfc <- st_sfc(st_polygon(list(polygon_matrix)))
line_sfc <- st_sfc(st_linestring(cbind(#create a line object
  x = c(0.4, 1), 
  y = c(0.2, 0.5)
)))
point_df <- data.frame(#create points
  x = c(0.2, 0.7, 0.4), 
  y = c(0.1, 0.2, 0.8)
)
point_sf <- st_as_sf(point_df, coords = c("x", "y"))#turn points into sf object
#plot the polygon, line, and points
plot(st_geometry(polygon_sfc))
plot(st_geometry(line_sfc), lwd = 3, add = TRUE)
plot(st_geometry(point_sf), add = TRUE)
text(point_df$x, point_df$y, labels = c(1, 2, 3), pos = 1)#label the points
#which points intersect in some way with polygon_sfc?
st_intersects(point_sf, polygon_sfc)#call the points then the polygon
#output is a sparse matrix
#points 1 and 3 intersect, point 2 does not. 
st_intersects(point_sf, polygon_sfc, sparse = FALSE)#same thing, but dense matrix
#dense matrix means there is a value in every position
#sparse matrix has empty cells

#st_intersects returns TRUE whenever the two objects touch in any way
#other operators are more specific
st_within(point_sf, polygon_sfc, sparse = FALSE)#which points are within polygon
#only point 3 is within
st_touches(point_sf, polygon_sfc, sparse = FALSE)#which touch boundary of polygon
#only 1 touches boundary. point 3 is entirely within not touching a boundary
st_disjoint(point_sf, polygon_sfc, sparse = FALSE)#which do not intersect in any way
#only point 2
#can also get points within a specified distance
#adds dist argument
st_is_within_distance(point_sf, polygon_sfc, dist = 0.2, sparse = FALSE)
#all 3 points are within 0.2 of the polygon
#verify the distance of each point from the polygon
#measured to nearest edge
st_distance(point_sf, polygon_sfc)

########
#SPATIAL JOINING
#create random points spread across the world and 
#find out what countries they're in
set.seed(2018)#set same seed as textbook
(bb = st_bbox(world))#the world's bounds
random_df <- data.frame(
  x = runif(n = 10, min = bb[1], max = bb[3]), 
  y = runif(n = 10, min = bb[2], max = bb[4])
)
random_points <- random_df %>% 
  st_as_sf(coords = c("x", "y")) %>% #set coordinates
  st_set_crs("EPSG:4326")#set geographic CRS
#subset world to only the countries that contain random points
world_random <- world[random_points, ]
nrow(world_random)#there are only 4 countries, the other points not in countries
#now join world and random points
random_joined <- st_join(random_points, world["name_long"])
random_joined#includes those points in the ocean
#but the spatial join is important if we want to plot them
plot(st_geometry(random_joined), pch = 4)
plot(st_geometry(world), add = TRUE)

#default join is a left join
#add the argument `left = FALSE` to do an inner join

########
#NON-OVERLAPPING JOINS

#geographic datasets can still have strong geographic relationships even if
#they do not touch
plot(st_geometry(cycle_hire), col = "blue", pch = 16)
plot(st_geometry(cycle_hire_osm), col = "red", pch = 16, add = TRUE)
#check if any of the points intersect
any(st_touches(cycle_hire, cycle_hire_osm, sparse = FALSE))#none touch
#join the `capacity` variable in cycle_hire_osm to cycle_hire
#simplest method to join non-overlapping datasets is `st_is_within_distance()`
sel <- st_is_within_distance(cycle_hire, cycle_hire_osm, 
                             dist = units::set_units(20, "m"))#set the units to m
summary(lengths(sel)>0)#438 points in the cycle_hire are within 20 m of cycle_hire_osm
#to obtain the values of these 438 points, need to join
z <- st_join(cycle_hire, cycle_hire_osm, st_is_within_distance,
             dist = units::set_units(20, "m"))
nrow(cycle_hire)#742 rows
nrow(z)#762 rows bc some cycle hire stations in cycle_hire have multiple matches
#in cycle_hire_osm (multiple pts in cycle_hire_osm are within 20 m of a point in
#cycle_hire - I THINK - textbook isn't clear)
#fix this by aggregating
z <- z %>% 
  group_by(id) %>% #group by column id
  summarize(capacity = mean(capacity))#set capacity to mean of capacity to collapse column
nrow(z) == nrow(cycle_hire)#check to see if this worked. Now same # of rows
#compare plot of capacity of source data with results of new object z
plot(cycle_hire_osm["capacity"])
plot(z["capacity"])

#########
#SPATIAL AGGREGATION

#get the average height of the high points in each region of new zeoland
nz_agg <- aggregate(x = nz_height, by = nz, FUN = mean)
#check that the geometry of nz is the same as nz_agg
identical(st_geometry(nz), st_geometry(nz_agg))#TRUE

plot(nz_agg["elevation"])#view the plot of the mean elevation of each region
#try the plot using ggplot...
ggplot(nz_agg) +
  geom_sf(mapping = aes(fill = elevation)) +
  coord_sf()

#can do the same thing using dplyr and st_join
nz_agg2 <- st_join(x = nz, y = nz_height) %>% 
  group_by(Name) %>% 
  summarize(elevation = mean(elevation, na.rm = TRUE))
plot(nz_agg2["elevation"])#same plot as above

#########
#DISTANCE RELATIONS

#find the distance between the highest point in New Zealand and the geographic
#centroid of the Canterbury region
nz_highest <- nz_height %>% 
  slice_max(n = 1, order_by = elevation)#get the highest point in NZ
canterbury_centroid <- st_centroid(canterbury)#find centroid of canterbury region
#st_distance provides the distance between 2 objecst
st_distance(nz_highest, canterbury_centroid)#returns a matrix, though only 1 value
#answer is also returned in meters

#now find the distances between the first 3 features in nz_height 
#and the Otago and Canterbury regions 
co <- filter(nz, grepl("Canter|Otag", Name))
st_distance(nz_height[1:3, ], co)
#distance between Otago (column 2) and second 2 features is zero bc they're in Otago
#verify by plotting
plot(st_geometry(co)[2])#plot outline of Otago region
plot(st_geometry(nz_height)[2:3], add = TRUE)#add the points

###################
#SPATIAL OPERATIONS ON RASTER DATA

elev <- rast(system.file("raster/elev.tif", package = "spData"))
grain <- rast(system.file("raster/grain.tif", package = "spData"))

#SPATIAL SUBSETTING

#extract values from rasters using coordinates
id <- cellFromXY(elev, xy = matrix(c(0.1, 0.1), ncol = 2))
elev[id]
#this works the same as 
terra::extract(elev, matrix(c(0.1, 0.1), ncol = 2))

#Can also subset raster objects with another raster object
#retrieve the values of the first raster that fall within the extent 
#of the second raster
clip <- rast(xmin = 0.9, xmax = 1.8, ymin = -0.45, ymax = 0.45, 
             resolution = 0.3, vals = rep(1, 9))
elev[clip]

#keep the spatial output
#this returns a spatRaster rather than just the values. 
elev[1:2, drop = FALSE]#drop argument refers to dropping the geometry

#raster masking
rmask <- elev #set rmask to the same thing as elev
#now replace values randomly with logical values of NA and TRUE
values(rmask) <- sample(c(NA, TRUE), 36, replace = TRUE)
#keep values of elev which are TRUE in rmask
elev[rmask, drop = FALSE] #creates a new spatRaster full of TRUEs
mask(elev, rmask)
#can also use this approach to replace some values with NAs

#replace all values less than 20 in the elev raster with NAs
elev[elev < 20] <-  NA

###
#MAP ALGEBRA
#like matrix algebra but the cells cannot move
#4 types:
#local: per-cell operations
#focal: neighborhood operations - output usually result of 3x3 input cell block
#Zonal operations - like focal but surrounding pixel grid can have 
#irregular sizes/shapes
#Global: per-raster operations. Output cell potentially derives its value from 
#one or several entire rasters

#local operations 
#comprise all cell-by-cell operations in one or several layers
#changes values of cells in raster (note min and max values)
elev + elev
elev^2
log(elev)
elev > 5#spatRaster returns a raster of logicals 

#reclassify into different groups
#e.g. changing DEM into low, middle, and high elevations
rcl <- matrix(c(0, 12, 1, 12, 24, 2, 24, 36, 3), ncol = 3, byrow = TRUE)
rcl#matrix of 3 cols
#reclassify with values in 3rd column
recl <- classify(elev, rcl = rcl)
recl

#' can also use the functions `app()`, `lapp()`, `tapp()`
#' calculate NDVI for the multispectral satellite file of Zion NP
multi_raster_file = system.file("raster/landsat.tif", package = "spDataLarge")
multi_rast = rast(multi_raster_file)
#make a function to implement NDVI: 
ndvi_fun <- function(nir, red){#accepts 2 values: nir and red
  (nir - red) / (nir + red)#formula for calculating NDVI
}
#use this function as the function for lapp()
#need to subset the input raster because we only need two bands and input has 4
#' `lapp()` applies a function to each cell using layers as arguments
ndvi_rast <- lapp(multi_rast[[c(4, 3)]], fun = ndvi_fun)
plot(ndvi_rast)

#focal operations
#applies an aggregation function to mult raster cells
#central (focal) cell and its neighbors
#neighborhood also called kernel, filter, moving window
#usually 3 x 3 (focal cell + 8 surrounding)
#can also be any shape as defined by the user
r_focal <- focal(elev, #input raster
                 w = matrix(1, nrow = 3, ncol = 3),#define the kernel w/matrix
                 fun = min)#get minimum, but could be sum, mean, var, etc.
plot(r_focal)
plot(elev)

#' can use the `terrain()` function to calculate topo characteristics
#' e.g. slop, aspect, flow directions
#' related the the `focal()` function

#Zonal operations
#similar to focal operations
#but uses a second raster (usually w categorical values) to define the zonal filters
#also called zones
#they don't have to be neighbors
#result is a summary table grouped by zone, not a raster
#This operation is also known as zonal statistics in the GIS world

z <- zonal(elev, #input raster
           grain, #raster to zone by
           fun = "mean") #function to apply
z#returns mean altitude for each grain size
#' can also get a raster with calculated stats for each zone by setting 
#' `as.raster` argument to true
z_rast <- zonal(elev, grain, fun = "mean", as.raster = TRUE)
z_rast#raster with same dimensions as grain and elev but only 3 values, mean elev

#MERGING RASTERS
#put rasters together that have different scenes (put them side by side)
#demo by getting elev data for Austria and Switzerland
#use `country_codes()` to get the country codes
aut <- geodata::elevation_30s(country = "AUT", path = tempdir())
ch <- geodata::elevation_30s(country = "CHE", path = tempdir())
aut_ch <- merge(aut, ch)#merge the two DEMs togethers
plot(aut_ch)#view them
#if the two scenes overlap, it will use the value from the first raster

