library(bmstdr)
library(ggsn)

nymap <- map_data(database = "state", regions = "new york")

p <-  ggplot() +
  geom_polygon(data = nymap, 
               aes(x = long, y = lat, group = group),
               color="black", 
               size = 0.6, fill = NA) +
  geom_point(data = nyspatial, aes(x = Longitude, y = Latitude)) +
  labs(title = "28 airpollution monitoring sites in NewYork",
  x = "Longitude", y = "Latitude") +
  scalebar(data = nymap, dist = 100, location = "bottomleft", 
           transform = T, dist_unit = "km",
           st.dist = .05, st.size = 5, height = .06, st.bottom = T,
           model = "WGS84") +
  north(data = nymap, location = "topleft", symbol = 12)

p

# The map_data command extracts the boundary polygons.
#
# The ggplot command draws the figure where:
#
# (i) geom_polygon draws the map boundary,
# (ii) geom_point adds the points,
# (iii) scalebar draws the scale of the map and
# (iv) north puts the north arrow.
# 
# In the polygon drawing the option fill = NA allows the
# map to be drawn with a blank background without any color filling.
# 
# The functions scalebar and north are provided by the ggsn library
# which must be linked before calling the plotting command.

# ?nysptime
# ??nyspatial

p <-  ggplot(nyspatial,aes(x=yo3))+
  geom_histogram(binwidth=4.5,color="black",fill="white")+
  labs(x="Average dailyozoneconcentration",y="count")
p

library(GGally)
p <- ggpairs(nyspatial,columns=6:9)
p


# Traditionally,spatialvariationisexploredbydrawinganempiricalvariogram.
# Toobtainanempiricalvariogramwefirstobtainavariogramcloud.Avar-
#   iogram cloudisasimplescatterplotofthehalfofthesquareddifference
# betweenthedatavaluesatanytwolocationsagainstthedistancebetween
# the twolocations.WiththespatialcoordinatessuppliedinUniversalTrans-
#   verseMercator(UTM)co-ordinatesystem,thedistancebetweentwolocations
# is simplytheEuclideandistance.Usually,theUTM-XandYcoordinatesare
# givenintheunitofmeter.

a <-  bmstdr_variogram(data=nyspatial, formula = yo3 ~ utmx + utmy,
                       coordtype="utm",nb=50)

# ?bmstdr_variogram.

library(fields)

ny_fit <- Krig(x = nyspatial %>% select(Longitude, Latitude),
               Y = nyspatial$yo3)

predict(ny_fit, x = gridnyspatial %>% select(Longitude, Latitude))


library(akima)

akima.smooth <- interp(gridnyspatial$Longitude, 
                       gridnyspatial$Latitude, 
                       predict(ny_fit, x = gridnyspatial %>% select(Longitude, Latitude)), 
                       nx=200, ny=200
                       )
si.zmin <- min(akima.smooth$z,na.rm=TRUE)
si.zmax <- max(akima.smooth$z,na.rm=TRUE)
breaks <- pretty(c(si.zmin,si.zmax),10)
colors <- heat.colors(length(breaks)-1)

image  (akima.smooth, main = "interp(<akima data>, *) on finer grid",
        breaks=breaks, col=colors)
contour(akima.smooth, add = TRUE, levels=breaks, col = "thistle")
points(akima, pch = 3, cex = 2, col = "blue")

akima.smooth <- fnc.delete.map.XYZ(akima.smooth)

aux <- tibble(Longitude = NULL, Latitude = NULL, pred_yo3 = NULL)
for (x in seq_along(akima.smooth$x)) {
  for(y in seq_along(akima.smooth$y)) {
    aux <- aux %>% 
      bind_rows(tibble(Longitude = akima.smooth$x[x], 
                       Latitude = akima.smooth$y[y], 
                       pred_yo3 = akima.smooth$z[x, y]))
  }
}


p <-  ggplot() +
  geom_polygon(data = nymap, 
               aes(x = long, y = lat, group = group),
               color="black", 
               size = 0.6, fill = NA) +
  geom_point(data = nyspatial, aes(x = Longitude, y = Latitude)) +
  labs(title = "28 airpollution monitoring sites in NewYork",
       x = "Longitude", y = "Latitude") +
  scalebar(data = nymap, dist = 100, location = "bottomleft", 
           transform = T, dist_unit = "km",
           st.dist = .05, st.size = 5, height = .06, st.bottom = T,
           model = "WGS84") +
  north(data = nymap, location = "topleft", symbol = 12)

p
p + geom_contour_filled(data = aux, 
                        mapping = aes(x = Longitude, 
                                      y = Latitude, 
                                      z = pred_yo3), 
                        alpha = .4,
                        bins = 12)

p + geom_contour_filled(data = aux, 
                        mapping = aes(x = Longitude, 
                                      y = Latitude, 
                                      z = pred_yo3), 
                        alpha = .4,
                        binwidth = 3) 

p + stat_contour_filled(data = aux, 
                        mapping = aes(x = Longitude, 
                                      y = Latitude, 
                                      z = pred_yo3), 
                        alpha = .4,
                        # binwidth = 3,
                        breaks = breaks)

# https://cran.r-project.org/web/packages/bmstdr/vignettes/bmstdr-vig_bookdown.html
coord <- nyspatial[, c("Longitude","Latitude")]
library(interp)
xo <- seq(from=min(coord$Longitude)-0.5, to = max(coord$Longitude)+0.8, length=200)
yo <- seq(from=min(coord$Latitude)-0.25, to = max(coord$Latitude)+0.8, length=200)
# surf <- interp(b$Longitude, b$Latitude, b$mean,  xo=xo, yo=yo)
surf <- interp(gridnyspatial$Longitude, 
               gridnyspatial$Latitude, 
               predict(ny_fit, x = gridnyspatial %>% select(Longitude, Latitude)),  
               xo=xo, yo=yo)
v <- fnc.delete.map.XYZ(xyz=surf)

interp1 <- data.frame(long = v$x, v$z )
names(interp1)[1:length(v$y)+1] <- v$y
library(tidyr)
interp1 <- gather(interp1,key = lat,value =Predicted,-long,convert = TRUE)
library(ggplot2)
nymap <- map_data(database="state",regions="new york")
mappath <- cbind(nymap$long, nymap$lat)
zr <- range(interp1$Predicted, na.rm=T)
P <- ggplot() +  
  geom_raster(data=interp1, aes(x = long, y = lat,fill = Predicted)) +
  geom_polygon(data=nymap, aes(x=long, y=lat, group=group), color="black", size = 0.6, fill=NA) + 
  geom_point(data=coord, aes(x=Longitude,y=Latitude))  +
  stat_contour(data=na.omit(interp1), aes(x = long, y = lat,z = Predicted), colour = "black", binwidth =2) +
  scale_fill_gradientn(colours=colpalette, na.value="gray95", limits=zr) +
  theme(axis.text = element_blank(), axis.ticks = element_blank()) +
  ggsn::scalebar(data =interp1, dist = 100, location = "bottomleft", transform=T, dist_unit = "km", st.dist = .05, st.size = 5, height = .06, st.bottom=T, model="WGS84") +
  ggsn::north(data=interp1, location="topleft", symbol=12) +
  labs(x="Longitude", y = "Latitude", size=2.5) 
P
