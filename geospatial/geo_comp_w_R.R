

# classes and functions for vector data
if (!require(sf)) { 
  install.packages("sf")
  library(sf)
}

# classes and functions for raster data
if (!require(raster)) {
  install.packages("raster")
  library(raster)
}

# load geographic data - see nowosad.github.io/spData
if (!require(spData)) {
  install.packages("spData")
  library(spData)
}

# load larger geographic data
if (!require(spDataLarge)) {
  devtools::install_github("Nowosad/spDataLarge")
  library(spDataLarge)
}

if (!require(RQGIS)) {
  devtools::install_github("r-spatial/RQGIS")
  library(RQGIS)
}

library(tidyverse)



# 2 - GEOGRAPHIC DATA IN R ------------------------------------------------

# 2.2 - Vector data -------------------------------------------------------

# sf = Simple Features

names(world)
world
plot(world)
summary(world)
summary(world["lifeExp"])

class(world$geom) # Simple Feature Columns
world$geom
class(world$geom[[1]]) # Simple Feature Geometries
world$geom[[1]]
world %>% dplyr::select(lifeExp)

library(sp)
world_sp = as(world, Class = "Spatial")

world_sf = st_as_sf(world_sp, "sf")

plot(world["pop"])
plot(world[3:6])

world_asia = world[world$continent == "Asia", ]
asia = st_union(world_asia)

plot(world["pop"], reset = FALSE)
plot(asia, add = TRUE, col = "red")

plot(world["continent"], reset = FALSE)
cex = sqrt(world$pop) / 10000
world_cents = st_centroid(world, of_largest = TRUE)
plot(st_geometry(world_cents), add = TRUE, cex = cex)


india = world[world$name_long == "India", ]
plot(st_geometry(india), expandBB = c(0, 0.2, 0.1, 1), col = "gray", lwd = 3)
plot(world_asia[0], add = TRUE)


# 2.2.7 - Simple Feature Columns (sfc) ------------------------------------

# To combine two simple features into one object with two features, we can
# use the st_sfc() function. This is important since sfc represents the geometry
# column in sf data frames
point1 = st_point(c(5, 2))
point2 = st_point(c(1, 3))
points_sfc = st_sfc(point1, point2)
points_sfc

st_geometry_type(point1)
st_geometry_type(points_sfc)

# sfc POLYGON
polygon_list1 = list(rbind(c(1, 5), c(2, 2), c(4, 1), c(4, 4), c(1, 5)))
polygon1 = st_polygon(polygon_list1)
polygon_list2 = list(rbind(c(0, 2), c(1, 2), c(1, 3), c(0, 3), c(0, 2)))
polygon2 = st_polygon(polygon_list2)
polygon_sfc = st_sfc(polygon1, polygon2)
st_geometry_type(polygon_sfc)

# sfc objects can additionally store information on the
# coordinate reference systems (CRS). To specify a certain CRS, we can use the
# epsg (SRID) or proj4string attributes of an sfc object. The default value of epsg
# (SRID) and proj4string is NA (Not Available), as can be verified with st_crs():
st_crs(points_sfc)

# We can add coordinate
# reference system as a crs argument of st_sfc(). This argument accepts an integer
# with the epsg code such as 4326, which automatically adds the ‘proj4string’ 

# EPSG definition
points_sfc_wgs = st_sfc(point1, point2, crs = 4326)
points_sfc_wgs
st_crs(points_sfc_wgs)

# It also accepts a raw proj4string:
points_sfc_proj4string <- st_sfc(point1, point2, crs = "+proj=longlat +datum=WGS84 +no_defs")
points_sfc_proj4string
st_crs(points_sfc_proj4string)


# 2.2.8 - The sf (Simple Features) class ----------------------------------

lnd_point = st_point(c(0.1, 51.5)) # sfg object
lnd_geom = st_sfc(lnd_point, crs = 4326) # sfc object
lnd_attrib = data.frame( # data.frame object
  name = "London",
  temperature = 25,
  date = as.Date("2017-06-21")
)

lnd_sf = st_sf(lnd_attrib, geometry = lnd_geom) # sf object

class(lnd_sf)


# 2.3 - Raster data -------------------------------------------------------

# Raster Data Model:
#
# - A MATRIX representing equally spaced cells (oftenalso called pixels)
# - A HEADER
#            - The Coordinate Reference System (CRS)
#            - The Origin (or starting point) is frequently the coordinate of 
#              the lower-left corner of the matrix (the raster package, however, 
#              uses the upper left corner, by default)
#            - The Extent: the number of columns, the number of rows and
#              the cell size resolution

raster_filepath = system.file("raster/srtm.tif", package = "spDataLarge")
new_raster = raster(raster_filepath)
class(new_raster)

dim(new_raster)
ncell(new_raster)
extent(new_raster)
crs(new_raster)
inMemory(new_raster)

plot(new_raster)


# 2.3.3 - Raster classes --------------------------------------------------

# RasterLayer

# The raster package supports numerous drivers with the help of rgdal
raster::writeFormats()
rgdal::gdalDrivers()


# Rasters can also be created from scratch using the raster() function
# raster() fills cells row-wise (unlike matrix()) starting at the
# upper left corner 
new_raster2 = raster(nrows = 6, ncols = 6, res = 0.5,
                     xmn = -1.5, xmx = 1.5, ymn = -1.5, ymx = 1.5,
                     vals = 1:36)

# RasterBrick 
multi_raster_file = system.file("raster/landsat.tif", package = "spDataLarge")
r_brick = brick(multi_raster_file)
r_brick
nlayers(r_brick)

plot(r_brick)

# Raster-Stack

raster_on_disk = raster(r_brick, layer = 1)
raster_in_memory = raster(xmn = 301905, xmx = 335745,
                          ymn = 4111245, ymx = 4154085,
                          res = 30)
values(raster_in_memory) = sample(seq_len(ncell(raster_in_memory)))
crs(raster_in_memory) = crs(raster_on_disk)

r_stack = stack(raster_in_memory, raster_on_disk)
r_stack

nlayers(r_stack)

plot(r_stack)


# 2.4 - Coordinate Reference Systems --------------------------------------

# Geographic coordinate systems identify any location on the Earth’s surface using
# two values — longitude and latitude
# Distances in geographic CRSs are therefore not measured in meters

sf_proj_info(type = "datum")
sf_proj_info(type = "have_datum_files")

# Projected coordinate reference systems are based on Cartesian coordinates on 
# an implicitly flat surface.
# They have an origin, x and y axes, and a linear unit of measurement such as
# meters. All projected CRSs are based on a geographic CRS, described in the
# previous section, and rely on map projections to convert the three-dimensional
# surface of the Earth into Easting and Northing (x and y) values in a projected
# CRS.
# There are three main groups of projection types - conic, cylindrical, and planar
sf_proj_info(type = "proj")

# CRSs in R
# Two main ways to describe CRS in R are an epsg code or a proj4string definition

crs_data = rgdal::make_EPSG()
View(crs_data)

# Vector data
vector_filepath = system.file("vector/zion.gpkg", package = "spDataLarge")
new_vector = st_read(vector_filepath)

st_crs(new_vector)

# Raster data
projection(new_raster) # get CRS
# The main difference, compared to vector data, is that raster objects only accept
# proj4 definitions

# 2.5 - Units -------------------------------------------------------------

# Vector data
luxembourg = world[world$name_long == "Luxembourg", ]
st_area(luxembourg)

st_area(luxembourg) %>% units::set_units("km^2")

# Raster data

# Units are of equal importance in the case of raster data. However, so far sf is
# the only spatial package that supports units, meaning that people working on
# raster data should approach changes in the units of analysis (for example, converting
# pixel widths from imperial to decimal units) with care.
# 
# The new_raster object (see above) uses a WGS84 projection with decimal degrees as units.
# 
# Consequently, its resolution is also given in decimal degrees but you have to
# know it, since the res() function simply returns a numeric vector.
res(new_raster)

# If we used the UTM projection, the units would change.
repr = projectRaster(new_raster, crs = "+init=epsg:26912")
res(repr)

# Again, the res() command gives back a numeric vector without any unit,
# forcing us to know that the unit of the UTM projection is meters.


# 3 - ATTRIBUTE DATA OPERATIONS -------------------------------------------


# 3.3 - Manipulating raster objects ---------------------------------------

elev = raster(nrows = 6, ncols = 6, res = 0.5,
              xmn = -1.5, xmx = 1.5, ymn = -1.5, ymx = 1.5,
              vals = 1:36)

grain_order = c("clay", "silt", "sand")
grain_char = sample(grain_order, 36, replace = TRUE)
grain_fact = factor(grain_char, levels = grain_order)
grain = raster(nrows = 6, ncols = 6, res = 0.5,
               xmn = -1.5, xmx = 1.5, ymn = -1.5, ymx = 1.5,
               vals = grain_fact)

# raster objects can contain values of class numeric, integer, logical or factor,
# but not character.

levels(grain)

levels(grain)[[1]] = cbind(levels(grain)[[1]], wetness = c("wet", "moist", "dry"))
levels(grain)

factorValues(grain, grain[c(1, 11, 35)])


# 3.3.1 - Raster subsetting -----------------------------------------------

# row 1, column 1
elev[1, 1]
# cell ID 1
elev[1]

values(elev)
getValues(elev, row = 2)
getValues(elev, row = 2, nrows = 3)


r_stack = stack(elev, grain)
names(r_stack) = c("elev", "grain")
# three ways to extract a layer of a stack
raster::subset(r_stack, "elev")
r_stack[["elev"]]
r_stack$elev


elev[1, 1] = 0
elev[]

elev[1, 1:2] = 0



# 3.3.2 - Summarizing raster objects --------------------------------------

summary(elev)

cellStats(elev, sd)

summary(brick(elev, grain))

# Specific functions such as boxplot(), density(), hist() and pairs() work also 
# with raster objects, as demonstrated in the histogram created with the command 
# below:
hist(elev)


# 4 - SPATIAL DATA OPERATIONS ---------------------------------------------


# 4.2 - Spatial operations on vector data ---------------------------------

# 4.2.1 - Spatial subsetting ----------------------------------------------

# Spatial subsetting is the process of selecting features of a spatial object based
# on whether or not they in some way relate in space to another object.

plot(nz %>% select(geom))
plot(nz_height %>% select(geometry), add = TRUE, pch = 2)

canterbury = nz %>% filter(Name == "Canterbury")
canterbury_height = nz_height[canterbury, ]
canterbury_height

# Topological relations - Ver ?geos_binary_pred 
nz_height[canterbury, , op = st_intersects] # Lo mismo que sin "op"
nz_height[canterbury, , op = st_disjoint]

# Topological operators - ver ?geos_binary_ops
sel_sgbp = st_intersects(x = nz_height, y = canterbury)
class(sel_sgbp)

sel_logical = lengths(sel_sgbp) > 0
canterbury_height2 = nz_height[sel_logical, ]

canterbury_height3 = nz_height %>%
  filter(st_intersects(x = ., y = canterbury, sparse = FALSE))


# 4.2.2 - Topological relations -------------------------------------------

# Topological relations describe the spatial relationships between objects. To
# understand them, it helps to have some simple test data to work with
# create a polygon
a_poly = st_polygon(list(rbind(c(-1, -1), c(1, -1), c(1, 1), c(-1, -1))))
a = st_sfc(a_poly)
# create a line
l_line = st_linestring(x = matrix(c(-1, -1, -0.5, 1), ncol = 2))
l = st_sfc(l_line)
# create points
p_matrix = matrix(c(0.5, 1, -1, 0, 0, 1, 0.5, 1), ncol = 2)
p_multi = st_multipoint(x = p_matrix)
p = st_cast(st_sfc(p_multi), "POINT")

plot(a)
plot(l, add = TRUE)
plot(p, add = TRUE)

# A simple query is: which of the points in p intersect in some way with polygon
# a?
st_intersects(p, a)
st_intersects(p, a, sparse = FALSE)
p[st_intersects(p, a, sparse = FALSE)]

# The opposite of st_intersects() is st_disjoint()
st_disjoint(p, a, sparse = FALSE)
p[st_disjoint(p, a, sparse = FALSE)]

# st_within() returns TRUE only for objects that are completely within the selecting
# object.
st_within(p, a, sparse = FALSE)
p[st_within(p, a, sparse = FALSE)]

# st_touches() only returns TRUE for the second
# point:
st_touches(p, a, sparse = FALSE)
p[st_touches(p, a, sparse = FALSE)]

# What about features that do not touch, but almost touch the selection object?
sel = st_is_within_distance(p, a, dist = 0.9) # can only return a sparse matrix
lengths(sel) > 0
p[lengths(sel) > 0]

# 4.2.3 - Spatial joining -------------------------------------------------

# Imagine you have ten points
# randomly distributed across the Earth’s surface. Of the points that are on land,
# which countries are they in?
set.seed(2018) # set seed for reproducibility
(bb_world = st_bbox(world)) # the world’s bounds

random_df = tibble(x = runif(n = 10, min = bb_world[1], max = bb_world[3]),
                   y = runif(n = 10, min = bb_world[2], max = bb_world[4])
)
random_points = random_df %>%
  st_as_sf(coords = c("x", "y")) %>% # set coordinates
  st_set_crs(4326) # set geographic CRS

plot(world %>% select(geom))
plot(random_points, add = TRUE, pch = 3, col = "blue")

world_random = world[random_points, ]
nrow(world_random)

random_joined = st_join(random_points, world["name_long"])

plot(world %>% select(geom))
plot(random_joined, add = TRUE, pch = 3, lwd = 10)


# 4.2.4 - Non-overlapping joins -------------------------------------------

# Sometimes two geographic datasets do not touch but still have a strong geographic
# relationship enabling joins.
plot(st_geometry(cycle_hire), col = "blue")
plot(st_geometry(cycle_hire_osm), add = TRUE, pch = 3, col = "red")

# We can check if any points are the same st_intersects() as shown below:
any(st_touches(cycle_hire, cycle_hire_osm, sparse = FALSE))

# Imagine that we need to join the capacity variable in cycle_hire_osm onto the official
# ‘target’ data contained in cycle_hire. This is when a non-overlapping join is
# needed. before performing the relation, both objects are transformed into a projected
# CRS.
cycle_hire_P = st_transform(cycle_hire, 27700)
cycle_hire_osm_P = st_transform(cycle_hire_osm, 27700)
sel = st_is_within_distance(cycle_hire_P, cycle_hire_osm_P, dist = 20)
summary(lengths(sel) > 0)

# How to retrieve the values associated
# with the respective cycle_hire_osm_P points? The solution is again with st_join(),
# but with an addition dist argument
z = st_join(cycle_hire_P, cycle_hire_osm_P, st_is_within_distance, dist = 20)
nrow(cycle_hire)
nrow(z)

# # Note that the number of rows in the joined result is greater than the target.
# This is because some cycle hire stations in cycle_hire_P have multiple matches
# in cycle_hire_osm_P. To aggregate the values for the overlapping points and
# return the mean, we can use the aggregation methods learned in Chapter 3,
# resulting in an object with the same number of rows as the target
z = z %>%
  group_by(id) %>%
  summarize(capacity = mean(capacity))
nrow(z) == nrow(cycle_hire)

plot(cycle_hire_osm["capacity"])
plot(z["capacity"])


# 4.2.5 - Spatial data aggregation ----------------------------------------

# Returning to the example of New Zealand, imagine you want to find out the
# average height of high points in each region.
nz_avheight = aggregate(x = nz_height, by = nz, FUN = mean)

nz_avheight2 = nz %>%
  st_join(nz_height) %>%
  group_by(Name) %>%
  summarize(elevation = mean(elevation, na.rm = TRUE))

# Spatial congruence is an important concept related to spatial aggregation. An
# aggregating object (which we will refer to as y) is congruent with the target
# object (x) if the two objects have shared borders.

plot(incongruent %>% select(value, geometry))
plot(st_geometry(aggregating_zones))

# How can we transfer
# the values of the underlying nine spatial polygons into the two polygons of
# aggregating_zones?

# The simplest useful method for this is area weighted spatial interpolation. In this
# case values from the incongruent object are allocated to the aggregating_zones in
# proportion to area; the larger the spatial intersection between input and output
# features, the larger the corresponding value.
agg_aw = st_interpolate_aw(incongruent[, "value"], aggregating_zones,
                           extensive = TRUE)
agg_aw$value

# In our case it is meaningful to sum up the values of the intersections falling
# into the aggregating zones since total income is a so-called spatially extensive
# variable. This would be different for spatially intensive variables, which are
# independent of the spatial units used, such as income per head or percentages5.
# In this case it is more meaningful to apply an average function when doing
# the aggregation instead of a sum function. To do so, one would only have to
# set the extensive parameter to FALSE.


# 4.2.6 - Distance relations ----------------------------------------------

nz_heighest = nz_height %>% top_n(n = 1, wt = elevation)
canterbury_centroid = st_centroid(canterbury)
st_distance(nz_heighest, canterbury_centroid)


co = filter(nz, grepl("Canter|Otag", Name))
st_distance(nz_height[1:3, ], co)

# distances
# between points and polygons refer to the distance to any part of the polygon:
#   The second and third points in nz_height are in Otago, which can be verified
# by plotting them (result not shown):

plot(st_geometry(co)[2])
plot(st_geometry(nz_height)[2:3], add = TRUE)


# 4.3 - Spatial operations on raster data ---------------------------------


# 4.3.1 - Spatial subsetting ----------------------------------------------

# Raster objects can
# also be extracted by location (coordinates) and other spatial objects. To use
# coordinates for subsetting, one can ‘translate’ the coordinates into a cell ID
# with the raster function cellFromXY(). An alternative is to use raster::extract()
# (be careful, there is also a function called extract() in the tidyverse) to extract
# values.
id = cellFromXY(elev, xy = c(0.1, 0.1))
elev[id]
# the same as
raster::extract(elev, data.frame(x = 0.1, y = 0.1))

# It is convenient that both functions also accept objects of class Spatial* Objects.
# Raster objects can also be subset with another raster object
clip = raster(xmn = 0.9, xmx = 1.8, ymn = -0.45, ymx = 0.45,
              res = 0.3, vals = rep(1, 9))
elev[clip]
# the same as
extract(elev, extent(clip))

# Basically, this amounts to retrieving the values of the first raster (here: elev)
# falling within the extent of a second raster (here: clip).

# # So far, the subsetting returned the values of specific cells, however, when doing
# spatial subsetting, one often also expects a spatial object as an output. To do
# this, we can use again the [ when we additionally set the drop parameter to
# FALSE.
elev[1:2, drop = FALSE] # spatial subsetting with cell IDs
elev[1, 1:2, drop = FALSE] # spatial subsetting by row,column indices

# Another common use case of spatial subsetting is when a raster with logical (or
# NA) values is used to mask another raster with the same extent and resolution
# create raster mask
rmask = elev
values(rmask) = sample(c(NA, TRUE), 36, replace = TRUE)
plot(rmask)
# spatial subsetting
elev[rmask, drop = FALSE] # with [ operator
mask(elev, rmask) # with mask()
overlay(elev, rmask, fun = "max") # with overlay


# 4.3.2 - Map algebra -----------------------------------------------------


# 4.3.3 - Local operations ------------------------------------------------

# A good example is the classification of intervals of numeric values into groups
# such as grouping a digital elevation model into low (class 1), middle (class 2)
# and high elevations (class 3). Using the reclassify() command, we need first to
# construct a reclassification matrix where the first column corresponds to the
# lower and the second column to the upper end of the class. The third column
# represents the new value for the specified ranges in column one and two
rcl = matrix(c(0, 12, 1, 12, 24, 2, 24, 36, 3), ncol = 3, byrow = TRUE)
recl = reclassify(elev, rcl = rcl)

# Raster algebra is another classical use case of local operations. This includes
# adding, subtracting and squaring two rasters. Raster algebra also allows logical
# operations such as finding all raster cells that are greater than a specific value
# (5 in our example below). The raster package supports all these operations
# and more, as described in vignette("Raster") and demonstrated below:
elev + elev
elev^2
log(elev)
elev > 5

# Instead of arithmetic operators, one can also use the calc() and overlay()
# functions. These functions are more efficient, hence, they are preferable in the
# presence of large raster datasets. Additionally, they allow you to directly store
# an output file.

# Predictive mapping is another interesting application of local raster operations.


# 4.3.4 - Focal operations ------------------------------------------------

# While local functions operate on one cell, though possibly from multiple
# layers, focal operations take into account a central cell and its neighbors. The
# neighborhood (also named kernel, filter or moving window) under consideration
# is typically of size 3-by-3 cells (that is the central cell and its eight surrounding
# neighbors), but can take on any other (not necessarily rectangular) shape as
# defined by the user. A focal operation applies an aggregation function to all cells
# within the specified neighborhood, uses the corresponding output as the new
# value for the the central cell, and moves on to the next central cell (Figure 4.8).
# Other names for this operation are spatial filtering and convolution

# # In R, we can use the focal() function to perform spatial filtering. We define
# the shape of the moving window with a matrix whose values correspond to
# weights (see w parameter in the code chunk below). Secondly, the fun parameter
# lets us specify the function we wish to apply to this neighborhood
r_focal = focal(elev, w = matrix(1, nrow = 3, ncol = 3), fun = min)

# Focal functions or filters play a dominant role in image processing. Low-pass
# or smoothing filters use the mean function to remove extremes. In the case of
# categorical data, we can replace the mean with the mode, which is the most
# common value. By contrast, high-pass filters accentuate features. The line
# detection Laplace and Sobel filters might serve as an example here. Check
# the focal() help page for how to use them

# Terrain processing, the calculation of topographic characteristics such as slope,
# aspect and flow directions, relies on focal functions. terrain() can be used
# to calculate these metrics, although some terrain algorithms, including the
# Zevenbergen and Thorne method to compute slope, are not implemented in this
# raster function. Many other algorithms — including curvatures, contributing
# areas and wetness indices — are implemented in open source desktop geographic
# information system (GIS) software. Chapter 9 shows how to access such GIS
# functionality from within R.


# 4.3.5 - Zonal operations ------------------------------------------------

# Zonal operations are similar to focal operations. The difference is that zonal
# filters can take on any shape instead of a predefined rectangular window

z = zonal(elev, grain, fun = "mean") %>%
  as.data.frame()
z

# This returns the statistics for each category, here the mean altitude for each
# grain size class


# 4.3.6 - Global operations and distances ---------------------------------

# Global operations are a special case of zonal operations with the entire raster
# dataset representing a single zone.


# 4.3.7 - Merging rasters -------------------------------------------------

aut = getData("alt", country = "AUT", mask = TRUE)
ch = getData("alt", country = "CHE", mask = TRUE)
aut_ch = merge(aut, ch)

# Raster’s merge() command combines two images, and in case they overlap,
# it uses the value of the first raster. You can do exactly the same with gdalUtils::
#   mosaic_rasters() which is faster, and therefore recommended if you have
# to merge a multitude of large rasters stored on disk

# 5 - GEOMETRY OPERATIONS -------------------------------------------------

# The previous three chapters have demonstrated how geographic datasets are
# structured in R (Chapter 2) and how to manipulate them based on their
# non-geographic attributes (Chapter 3) and spatial properties (Chapter 4). This
# chapter extends these skills. After reading it — and attempting the exercises at
# the end — you should understand and have control over the geometry column
# in sf objects and the geographic location of pixels represented in rasters.


# 5.2 - Geometric operations on vector data -------------------------------

# operations that in some way change the geometry of vector
# (sf) objects. It is more advanced than the spatial data operations presented
# in the previous chapter (in Section 4.2), because here we drill down into the
# geometry: the functions discussed in this section work on objects of class sfc
# in addition to objects of class sf.


# 5.2.1 - Simplification --------------------------------------------------

plot(seine)

seine_simp = st_simplify(seine, dTolerance = 2000) # 2000 m

plot(seine_simp)

#
plot(st_geometry(us_states))

us_states2163 = st_transform(us_states, 2163)
plot(st_geometry(us_states2163))

# st_simplify() works equally well with projected polygons:
us_states_simp1 = st_simplify(us_states2163, dTolerance = 100000) # 100 km
plot(st_geometry(us_states_simp1))

#
# proportion of points to retain (0-1; default 0.05)
us_states2163$AREA = as.numeric(us_states2163$AREA)
us_states_simp2 = rmapshaper::ms_simplify(us_states2163, keep = 0.01,
                                          keep_shapes = TRUE)
plot(st_geometry(us_states_simp2))


# 5.2.2 - Centroids -------------------------------------------------------

nz_centroid = st_centroid(nz)
plot(st_geometry(nz))
plot(st_geometry(nz_centroid), col = "red", pch = 4, add = TRUE)

seine_centroid = st_centroid(seine)
plot(seine)
plot(seine_centroid, col = "red", pch = 4, add = TRUE)

sp = world %>% filter(name_long == "Spain") %>% st_geometry()
plot(sp)

sp_centroid = st_centroid(sp)
plot(sp_centroid, add = TRUE, col = "blue", pch = 3)

#
nz_pos = st_point_on_surface(nz)
plot(st_geometry(nz))
plot(st_geometry(nz_centroid), col = "red", pch = 4, add = TRUE)
plot(st_geometry(nz_pos), col = "blue", pch = 3, add = TRUE)

seine_pos = st_point_on_surface(seine)
plot(seine)
plot(seine_centroid, col = "red", pch = 4, add = TRUE)
plot(seine_pos, col = "blue", pch = 3, add = TRUE)


# 5.2.3 - Buffers ---------------------------------------------------------

seine_buff_5km = st_buffer(seine, dist = 5000)
seine_buff_50km = st_buffer(seine, dist = 50000)

plot(seine)
plot(seine_buff_5km, add = TRUE)

plot(seine_buff_50km, add = TRUE)


ggplot(seine, aes(color = name, fill = name)) + 
  geom_sf() + 
  geom_sf(alpha = 0.2, data = seine_buff_5km) +
  geom_sf(alpha = 0.2, data = seine_buff_50km)


# 5.2.4 - Affine transformations ------------------------------------------

# Shifting
nz_sfc = st_geometry(nz)
nz_shift = nz_sfc + c(0, 100000)

plot(nz_sfc)
plot(nz_shift, color = "red", add = TRUE)

# Global scaling
global_scal_nz_sfc_1_5 <- nz_sfc * 1.5
st_crs(global_scal_nz_sfc_1_5) <- st_crs(nz_sfc)
ggplot() + 
  geom_sf(data = nz_sfc) +
  geom_sf(data = global_scal_nz_sfc_1_5, color = "blue")

# Local scaling
nz_centroid_sfc = st_centroid(nz_sfc)
nz_scale = (nz_sfc - nz_centroid_sfc) * 0.5 + nz_centroid_sfc
st_crs(nz_scale) <- st_crs(nz_sfc)
ggplot() + 
  geom_sf(data = nz_sfc) +
  geom_sf(data = nz_scale, color = "blue")

# Rotation of two-dimensional coordinates requires a rotation matrix
rotation = function(a){
  r = a * pi / 180 #degrees to radians
  matrix(c(cos(r), sin(r), -sin(r), cos(r)), nrow = 2, ncol = 2)
}

nz_rotate = (nz_sfc - nz_centroid_sfc) * rotation(30) + nz_centroid_sfc
st_crs(nz_rotate) <- st_crs(nz_sfc)
ggplot() + 
  geom_sf(data = nz_sfc) +
  geom_sf(data = nz_rotate, color = "blue")

# the newly created geometries can replace the old ones with the
# st_set_geometry() function
nz_scale_sf = st_set_geometry(nz, nz_scale)
ggplot(nz_scale) + geom_sf()


# 5.2.5 - Clipping --------------------------------------------------------

b = st_sfc(st_point(c(0, 1)), st_point(c(1, 1))) # create 2 points
b = st_buffer(b, dist = 1) # convert points to circles
plot(b)
text(x = c(-0.5, 1.5), y = 1, labels = c("x", "y")) # add text

x = b[1]
y = b[2]
x_and_y = st_intersection(x, y)
plot(b)
plot(x_and_y, col = "lightgrey", add = TRUE) # color intersecting area

plot(b)
plot(st_difference(y, x), col = "lightgrey", add = TRUE)

plot(b)
plot(st_difference(x, y), col = "lightgrey", add = TRUE)

plot(b)
plot(st_union(x, y), col = "lightgrey", add = TRUE)

plot(b)
plot(st_sym_difference(x, y), col = "lightgrey", add = TRUE)

#
bb = st_bbox(st_union(x, y))
box = st_as_sfc(bb)
set.seed(2017)
p = st_sample(x = box, size = 10)

plot(box)
plot(x, add = TRUE)
plot(y, add = TRUE)
plot(p, add = TRUE)
text(x = c(-0.5, 1.5), y = 1, labels = c("x", "y"))

# The logical operator way would find the points inside both x and y using
# a spatial predicate such as st_intersects(), whereas the intersection method
# simply finds the points inside the intersecting region created above as x_and_y.
# As demonstrated below the results are identical, but the method that uses the
# clipped polygon is more concise:

sel_p_xy = st_intersects(p, x, sparse = FALSE)[, 1] &
  st_intersects(p, y, sparse = FALSE)[, 1]
p_xy1 = p[sel_p_xy]
p_xy2 = p[x_and_y]
identical(p_xy1, p_xy2)


# 5.2.6 - Geometry unions -------------------------------------------------

regions = aggregate(x = us_states[, "total_pop_15"], by = list(us_states$REGION),
                    FUN = sum, na.rm = TRUE)

regions2 = us_states %>% group_by(REGION) %>%
  summarize(pop = sum(total_pop_15, na.rm = TRUE))

us_west = us_states[us_states$REGION == "West", ]
us_west_union = st_union(us_west)

texas = us_states[us_states$NAME == "Texas", ]
texas_union = st_union(us_west_union, texas)


# 5.2.7 - Type transformations --------------------------------------------

# Geometry casting is a powerful operation that enables transformation of the
# geometry type. It is implemented in the st_cast function from the sf package.
# Importantly, st_cast behaves differently on single simple feature geometry (sfg)
# objects, simple feature geometry column (sfc) and simple features objects

# sfg
multipoint = st_multipoint(matrix(c(1, 3, 5, 1, 3, 1), ncol = 2))

linestring = st_cast(multipoint, "LINESTRING")
polyg = st_cast(multipoint, "POLYGON")

plot(multipoint)
plot(linestring)
plot(polyg)

multipoint_2 = st_cast(linestring, "MULTIPOINT")
multipoint_3 = st_cast(polyg, "MULTIPOINT")
all.equal(multipoint, multipoint_2, multipoint_3)

# 
multilinestring_list = list(matrix(c(1, 4, 5, 3), ncol = 2),
                            matrix(c(4, 4, 4, 1), ncol = 2),
                            matrix(c(2, 4, 2, 2), ncol = 2))
multilinestring = st_multilinestring((multilinestring_list))
multilinestring_sf = st_sf(geom = st_sfc(multilinestring))
multilinestring_sf

linestring_sf2 = st_cast(multilinestring_sf, "LINESTRING")
linestring_sf2

linestring_sf2$name = c("Riddle Rd", "Marshall Ave", "Foulke St")
linestring_sf2$length = st_length(linestring_sf2)
linestring_sf2

plot(multilinestring_sf)
plot(linestring_sf2)

ggplot(data = multilinestring_sf) + geom_sf()
ggplot(data = linestring_sf2, mapping = aes(color = name)) + geom_sf()


# 5.3 - Geometric operations on raster data -------------------------------


# 5.3.1 - Geometric intersections -----------------------------------------

data("elev", package = "spData")
clip = raster(xmn = 0.9, xmx = 1.8, ymn = -0.45, ymx = 0.45,
              res = 0.3, vals = rep(1, 9))
elev[clip, drop = FALSE]


plot(elev)
plot(clip, add = TRUE)

plot(elev[clip, drop = FALSE])

# For the same operation we can also use the intersect() and crop() command.


# 5.3.2 - Extent and origin -----------------------------------------------

data(elev, package = "spData")
elev_2 = extend(elev, c(1, 2), value = 1000)
plot(elev_2)

# Performing an algebraic operation on two objects with differing extents in R,
# the raster package returns the result for the intersection, and says so in a
# warning.
elev_3 = elev + elev_2
plot(elev_3)

# However, we can also align the extent of two rasters with extend(). Instead
# of telling the function how many rows or columns should be added (as done
# before), we allow it to figure it out by using another raster object.
elev_4 = extend(elev, elev_2)

extent(elev)
origin(elev)

extent(elev_2)
origin(elev_2)

extent(elev_4)
origin(elev_4)

# change the origin
origin(elev_4) = c(0.25, 0.25)
plot(elev_4)
# and add the original raster
plot(elev, add = TRUE)


# 5.3.3 - Aggregation and disaggregation ----------------------------------

data("dem", package = "RQGIS")
plot(dem)
dem

dem_agg = aggregate(dem, fact = 5, fun = mean)
plot(dem_agg)
dem_agg

dem_2 = disaggregate(dem_agg, fact = 5, method = "")
plot(dem_2)
dem_2

dem_3 = disaggregate(dem_agg, fact = 5, method = "bilinear")
plot(dem_3)
dem_3

identical(dem, dem_3)

compareRaster(dem, dem_3)

# The process of computing values for new pixel locations is also called resampling.
# In fact, the raster package provides a resample() function. It lets you align
# several raster properties in one go, namely origin, extent and resolution. By
# default, it uses the bilinear-interpolation.

# add 2 rows and columns, i.e. change the extent
dem_agg = extend(dem_agg, 2)
dem_disagg_2 = resample(dem_agg, dem)

# 5.4 - Raster-vector interactions ----------------------------------------


# 5.4.1 - Raster cropping -------------------------------------------------

# Raster object
srtm = raster(system.file("raster/srtm.tif", package = "spDataLarge"))

# sf object
zion = st_read(system.file("vector/zion.gpkg", package = "spDataLarge"))
zion = st_transform(zion, projection(srtm))

# Both target and cropping objects must have the same projection.
plot(srtm)
plot(st_geometry(zion), add = TRUE)
ggplot(data = zion) + geom_sf()

# We will use crop() from the raster package to crop the srtm raster. crop()
# reduces the rectangular extent of the object passed to its first argument based
# on the extent of the object passed to its second argument
srtm_cropped = crop(srtm, zion)
class(srtm_cropped)
plot(srtm_cropped)
plot(st_geometry(zion), add = TRUE)

# Related to crop() is the raster function mask(), which sets values outside of the
# bounds of the object passed to its second argument to NA.
srtm_masked = mask(srtm, zion)
class(srtm_masked)
plot(srtm_masked)
plot(st_geometry(zion), add = TRUE)

# Changing the settings of mask() yields different results. Setting maskvalue = 0,
# for example, will set all pixels outside the national park to 0. Setting inverse =
# TRUE will mask everything inside the bounds of the park
srtm_inv_masked = mask(srtm, zion, inverse = TRUE)
class(srtm_inv_masked)
plot(srtm_inv_masked)
plot(st_geometry(zion), add = TRUE)


# 5.4.2 - Raster extraction -----------------------------------------------

# Raster extraction is the process of identifying and returning the values associated
# with a ‘target’ raster at specific locations, based on a (typically vector)
# geographic ‘selector’ object.

data("zion_points", package = "spDataLarge")
zion_points = st_transform(zion_points, projection(srtm))

plot(st_geometry(zion))
plot(zion_points, add = TRUE)

zion_points$elevation = raster::extract(srtm, zion_points)

plot(zion_points)

plot(srtm)
plot(st_geometry(zion), add = TRUE)
plot(zion_points, add = TRUE)

# The buffer argument can be used to specify a buffer radius (in meters) around
# each point. The result of raster::extract(srtm, zion_points, buffer = 1000), for
# example, is a list of vectors, each of which representing the values of cells inside
# the buffer associated with each point. In practice, this example is a special
# case of extraction with a polygon selector, described below.

# Raster extraction also works with line selectors
zion_transect = cbind(c(-113.2, -112.9), c(37.45, 37.2)) %>%
  st_linestring() %>%
  st_sfc(crs = projection(srtm)) %>%
  st_sf()

# The utility of extracting heights from a linear selector is illustrated by imagining
# that you are planning a hike. The method demonstrated below provides an
# ‘elevation profile’ of the route 
transect = raster::extract(srtm, zion_transect,
                           along = TRUE, cellnumbers = TRUE)
head(transect[[1]])
# Note the use of along = TRUE and cellnumbers = TRUE arguments to return cell IDs
# along the path. The result is a list containing a matrix of cell IDs in the first
# column and elevation values in the second. The number of list elements is equal
# to the number of lines or polygons from which we are extracting values.

# The subsequent code chunk first converts this tricky matrix-in-a-list object into a
# simple data frame, returns the coordinates associated with each extracted cell,
# and finds the associated distances along the transect (see ?geosphere::distGeo()
# or details):
transect_df = purrr::map_dfr(transect, as_tibble, .id = "ID")
transect_coords = xyFromCell(srtm, transect_df$cell)
transect_df$dist = c(0, cumsum(geosphere::distGeo(transect_coords))) %>% head(-1)

plot(srtm_masked)
plot(zion_transect, add = TRUE)

transect_df %>% ggplot(aes(x = dist, y = srtm)) + 
  geom_line() +
  labs(x = "Distance (m)", 
       y = "Elevation (m a.s.l.)",
       title = "Elevation along the line")

# The final type of geographic vector object for raster extraction is polygons.
# Like lines and buffers, polygons tend to return many raster values per polygon.
zion_srtm_values = raster::extract(x = srtm, y = zion, df = TRUE)

# Such results can be used to generate summary statistics for raster values
# per polygon, for example to characterize a single region or to compare many
# regions. The generation of summary statistics is demonstrated in the code
# below, which creates the object zion_srtm_df containing summary statistics for
# elevation values in Zion National Park
group_by(zion_srtm_values, ID) %>%
  summarize_at(vars(srtm), list(~min(.), ~mean(.), ~max(.)))
# The preceding code chunk used the tidyverse to provide summary statistics
# for cell values per polygon ID, as described in Chapter 3. The results provide
# useful summaries, for example that the maximum height in the park is around
# 2,661 meters (other summary statistics, such as standard deviation, can also
# be calculated in this way). Because there is only one polygon in the example a
# data frame with a single row is returned; however, the method works when
# multiple selector polygons are used.

# The same approach works for counting occurrences of categorical raster values
# within polygons. This is illustrated with a land cover dataset (nlcd) from the
# spDataLarge package in Figure 5.20(B), and demonstrated in the code below:

plot(nlcd)
plot(st_geometry(st_transform(zion, projection(nlcd))), add = TRUE)

zion_nlcd = raster::extract(nlcd, zion, df = TRUE, factors = TRUE)

dplyr::select(zion_nlcd, ID, levels) %>%
  tidyr::gather(key, value, -ID) %>%
  group_by(ID, key, value) %>%
  tally() %>%
  tidyr::spread(value, n, fill = 0)