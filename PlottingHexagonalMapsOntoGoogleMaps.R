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
library("raster")

#******PARAMETERS NEEDED*****: 
#'hex_shapes.df' from the 'IntersectHexagonMapsWithAccessibility' script
#****************************

#Define centre of map using Google location
CenterOfMap <- geocode("Vancouver") 
#Can also try specifying the cenre of the map via lat/long: 
#CenterOfMap <- geocode("49.118944,-122.785694")

#Get google maps background in correct location; set zoom (10-11 is usually OK)
#For terrain version
City <- get_map(c(lon=CenterOfMap$lon, lat=CenterOfMap$lat),zoom = 11, maptype = "terrain", source = "google", color="bw")
#For B&W trippy version
#City <- get_map(c(lon=CenterOfMap$lon, lat=CenterOfMap$lat),zoom = 11, maptype = "toner", source = "stamen")
#For OSM version
#City <- get_map(c(lon=CenterOfMap$lon, lat=CenterOfMap$lat),zoom = 11,source = "osm")

#CityMap <- ggmap(City) #Plots the map WITH long/lat axes
CityMap <- ggmap(City, extent = "device") #Plots the map, but without the long/lat axes (margins)
#This removes all the 'Na' in Access column from the map plotting, so they don't appear on the map
hex_shapes.dfmodified = na.omit(hex_shapes.df)

#Create combined map (set the transparency value in 'alpha')
combined_map <- CityMap + geom_polygon(aes(x=long, y=lat, fill=Access, group=group), data=hex_shapes.dfmodified, alpha =1.0) 

#Change gradient of colourbar and add title
combined_map <- combined_map + scale_fill_gradientn(colours = c("navyblue", "seagreen4", "yellow"), limits=c(0,50), oob=squish, space = "Lab", na.value = "transparent", guide = "colourbar") + theme(legend.position="bottom")
#combined_map <- combined_map + scale_fill_gradientn(colours = c("red", "orange", "white", "deepskyblue1", "navyblue"), limits=c(-80, 80), space = "Lab", na.value = "transparent", guide = "colourbar")

combined_map <- combined_map + scale_alpha(range = c(0,1), limits = c(0, 100), na.value = 0)

#Add title
#combined_map <- combined_map + labs(title="Time: 23:00")
#If you want to crop the map...
#combined_map <- combined_map + scale_x_continuous(limits = c(-123.29, -123.0), expand = c(0, 0))
#combined_map <- combined_map + scale_y_continuous(limits = c(49.19, 49.31), expand = c(0, 0))
#If you want to change colorbar position and title
combined_map <- combined_map + theme(legend.position="bottom")
combined_map <- combined_map + labs(fill = "Title") 

#Display map
print(combined_map)

#ggsave("Surrey_Difference_Car_WalkTransit_30mins_19Sep2017.png", combined_map, width = 3.25, height = 3.15, dpi = 1200)

