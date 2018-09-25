#Load libraries
library("maps")
library("mapdata")
library("rgdal")
library("maptools")
library("ggplot2")
library("plyr")
library("rgeos")
library("foreign")
library("ggmap")
library("RgoogleMaps")

#Set working directory for this file
setwd("/Users/jeromemayaud/Documents/University/BritishColumbia/Modelling/RStudioProjects/PlottingRoutesStopsOntoMaps/Translink_Trips_Routes_Stops_shp")

#Set what the map you want to use to layer on is: #This is from script "PlottingHexagonalMapsOntoGoogleMaps"
CityMap <- combined_map 

#Get shapefile from your working folder
Routes <- readOGR(".","Trips_Routes")
Stops <- readOGR(".","Stops")

#Transform to the right projection format
#Routes <- spTransform(Routes, CRS("+proj=utm +zone=10 +ellps=GRS80 +units=m +no_defs")) #This is to be compatible with hexagon map
Routes <- spTransform(Routes, CRS("+proj=longlat +datum=WGS84")) #This is to convert to GoogleMapsFriendly
Stops <- spTransform(Stops, CRS("+proj=longlat +datum=WGS84")) #This is to convert to GoogleMapsFriendly

#'Fortify' means converting polygon spatial data into a data frame that R understands
Routes <- fortify(Routes)
#Bus stops are point data, not polygons, have to convert them to a dataframe that R understands
Stops <- data.frame(Stops)

#Layer up the different maps (make sure the x and y parameter names (ie long and lat) are correct)
CityMapWithRoutes <- CityMap + geom_polygon(aes(x=long, y=lat, group=group), fill='grey', size=.2,color='green', data=Routes, alpha=0)
CityMapWithStops <- CityMap + geom_point(aes(x=coords.x1, y=coords.x2), data = Stops, alpha = .3, size = 1, color = 'red')
CityMapWithRoutesAndStops <- CityMap + geom_polygon(aes(x=long, y=lat, group=group), fill='grey', size=.2,color='green', data=Routes, alpha=0) + geom_point(aes(x=coords.x1, y=coords.x2), data = Stops, alpha = .3, size = 1, color = 'red')


print(CityMapWithRoutes)
print(CityMapWithStops)
print(CityMapWithRoutesAndStops)
