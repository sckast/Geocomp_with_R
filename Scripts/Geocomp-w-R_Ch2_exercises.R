#Exercise 1
#' use `summary()` on the geometry columb of hte `world` data object that is 
#' included in the `spData` package.
names(world)
summary(world["geom"])
#the geometry type is multipolygon
#it is showing 177 multipolygons, which I think means 177 countries
#projection is EPSG: 4326
#looking this up on the internet shows that this is WGS84
st_crs(world)#check projection another way

#Exercise 2
#Run the code that 'generated' the map of the world in Section 2.2.3
#Find 2 similarites between the image on your computer and that in the book.
plot(world["continent"], reset = FALSE)
cex = sqrt(world$pop) / 10000
world_cents = st_centroid(world, of_largest = TRUE)
plot(st_geometry(world_cents), add = TRUE, cex = cex)
#differences: projection is different, width of lines of circles
#similarities: colors are the same, circles are present

#'what does the `cex` argument do (see ?plot)?
#it changes the symbol size

#'why was `cex` set to the `sqrt(world$pop) / 10000`? 
#'because not reducing the symbol size would have made massive circles
#'
#'experiment with ways to visualize the global population
plot(world["continent"], reset = FALSE)
cex = world$pop / 100000000
world_cents = st_centroid(world, of_largest = TRUE)
plot(st_geometry(world_cents), add = TRUE, cex = cex)

#Exercise 3
#' use `plot()` to create maps of Nigeria in context
#' adjust the `lwd`, `col`, and `expandBB` arguments of `plot()`
nigeria <- world[world$name_long == "Nigeria", ]
plot(st_geometry(nigeria), expandBB = c(1, 1, 1, 1), col = "red", lwd = 2)
world_africa <- world[world$continent == "Africa", ]#'make sure to add `, ]` 
plot(st_geometry(world_africa), add = TRUE)
text(nigeria, label = nigeria$name_long, pos = 4:1) 
#' challenge: read the documentation of `text()` and annotate the map

#' Exercise 4 
#' Create an empty SpatRaster object called my_raster with 10 columns and 10 rows. 
#' Assign random values between 0 and 10 to the new raster and plot it.
raster_4 <- rast(nrows = 10, ncols = 10, 
                 xmin = -5, xmax = 5, ymin = -5, ymax = 5,
                 vals = 0:10)
plot(raster_4)

#Exercise 5
#Read-in the raster/nlcd.tif file from the spDataLarge package. 
#What kind of information can you get about the properties of this file?
import_raster_5 <- system.file("raster/nlcd.tif", package = "spDataLarge")
raster_5 <- rast(import_raster_5)
raster_5#get a bunch of info
#can also use variety of functions to obtain info
nlyr(raster_5)#1 layer
dim(raster_5)#dimemsions - rows, cols, layers
res(raster_5)#resolutioni is 31.5 x 31.5 (meters I think)

#Exercise 6
#check the CRS of the `raster/nlcd.tif` file
#what kind of info can you learn from it? 
crs(raster_5)#crs is NAD83 UTM Zone 12N
#length unit is meter
#angle unit is degree
#area is North America between 114 deg W and 108 deg W which includes interior 
# west Canada and US

