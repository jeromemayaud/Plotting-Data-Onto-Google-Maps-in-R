library("spatialEco")
library("sp")
library("rgdal")
library("maptools")
library("ggplot2")
library("plyr")
library("rgeos")
library("foreign")
library("ggmap")

#******PARAMETERS NEEDED*****: 
#'hex_got' from the 'MakingHexagonalMaps' script
#****************************

####### FOR ORIGINS ##########
setwd("/Users/jeromemayaud/Documents/University/BritishColumbia/Modelling/OpenTripPlanner/CatchmentsAnalysis/Surrey")
origin_access_to_facilities_data <- read.csv(file = "BLAH_catchments_from_origin_Surrey_hospitals_30mins_19Sep2017_10_00.csv", header=TRUE, sep=",", strip.white=TRUE)
#############################

####### FOR HYBRID ###########
#setwd("/Users/jeromemayaud/Documents/University/BritishColumbia/Modelling/OpenTripPlanner/CatchmentsAnalysis/Surrey/Hybrid/")
#origin_access_to_facilities_data <- read.csv(file = "hybrid_measure_Surrey_hospitals_30mins_DIFFERENCE.csv", header=TRUE, sep=",", strip.white=TRUE)
#############################

origin_access_to_facilities_data[origin_access_to_facilities_data == NA] <- NA #Turn all zero values to NaNs (as it means there is no access to that part of the map)
colnames(origin_access_to_facilities_data) <- c("idhex","TotalFacilities") #Rename columns
origin_access_to_facilities_data$idhex <- sub("^", "ID", origin_access_to_facilities_data$idhex) #Add 'ID' to the GEOID numbers, to help with merging later

#Assign hexagonal grid from 'MakingHexagonMaps' script
hex_grid <- spTransform(hex_got, CRS("+proj=longlat +datum=WGS84")) # Convert hex_grid from UTM to WGS84
hex_shapes_1 <- hex_grid #hex_shapes_1 is to differentiate it from hex_shapes that is outputted from IntersectHexagonMapsWithAccessibility script

#Fortify and join the hex_shapes_1 file
hex_shapes_1@data$id <- rownames(hex_shapes_1@data)
hex_shapes_1.df <- fortify(hex_shapes_1)
hex_shapes_1.df <- join(hex_shapes_1.df, hex_shapes_1@data, by="id") #This 'rejoins' the fortified file

#Create a datafile by merging the hexagon polygons with the spatial_key that contains the census data
hex_and_facilities_merged <- merge(hex_shapes_1.df, origin_access_to_facilities_data, by.x="idhex", by.y="idhex", all.x=TRUE, all.y=TRUE)
hex_and_facilities_merged.modified = na.omit(hex_and_facilities_merged)


####### PLOTTING ####### 
#Plot simple choropleth map
aaa <- ggplot(data=hex_and_facilities_merged.modified, aes(x=long, y=lat, group=group)) 
aaa <- aaa + geom_polygon(aes(x=long, y=lat, fill=TotalFacilities, group=group), data=hex_and_facilities_merged.modified, alpha =0.7) 
aaa <- aaa + geom_polygon(aes(fill=TotalFacilities)) #This is where you choose which parameter to show as 'fill'
aaa <- aaa + scale_fill_gradientn(colours = c("lightpink", "darkblue"), space = "Lab", na.value = "transparent", guide = "colourbar")
aaa <- aaa + labs(title="Number of Accessible Boulangeries")
print(aaa)

#Plot choropleth map on Google Maps
CenterOfMap <- geocode("Surrey, BC")
City <- get_map(c(lon=CenterOfMap$lon, lat=CenterOfMap$lat),zoom = 11, maptype = "terrain", source = "google", color="bw")
CityMap <- ggmap(City, extent = "device")
CityMap <- CityMap + geom_polygon(aes(x=long, y=lat, fill=TotalFacilities, group=group), data=hex_and_facilities_merged.modified, alpha =0.7) 
CityMap <- CityMap + scale_fill_gradientn(colours = c("moccasin", "violetred2", "midnightblue"), space = "Lab", na.value = "transparent", guide = "colourbar")
CityMap <- CityMap + scale_fill_gradientn(colours = c("lavenderblush", "springgreen2", "springgreen4", "darkgreen"), space = "Lab", na.value = "transparent", guide = "colourbar")
CityMap <- CityMap + scale_fill_gradientn(colours = c("black", "purple", "yellow"), space = "Lab", na.value = "transparent", guide = "colourbar")
#CityMap <- CityMap + scale_fill_gradientn(colours = c("darkblue", "hotpink", "lightyellow"), space = "Lab", na.value = "transparent", guide = "colourbar")
#CityMap <- CityMap + scale_fill_gradientn(colours = c("lightgoldenrod1", "palegreen3", "dodgerblue4"), space = "Lab", na.value = "transparent", guide = "colourbar")
#CityMap <- CityMap + scale_fill_gradientn(colours = c("lightpink", "darkblue"), space = "Lab", na.value = "transparent", guide = "colourbar")
#CityMap <- CityMap + scale_alpha(range = c(0,0.55), limits = c(0.01, 45), na.value = 0)
CityMap <- CityMap + theme(legend.position="bottom") + labs(fill = "Nombre de boulangeries") 
#CityMap <- CityMap + labs(title="Nombre de boulangeries")
print(CityMap)

