library("plyr")
library("rgeos")
library("foreign")
library("ggmap")
library("RgoogleMaps")
library("raster")
library("maps")
library("mapdata")
library("rgdal")
library("maptools")
library("ggplot2")


#******PARAMETERS NEEDED*****: 
#'hex_got' from the 'MakingHexagonalMaps' script
#****************************

# Read accessibility data from the analysed traveltimematrix (done in Python for now)
setwd("/Users/jeromemayaud/Documents/University/BritishColumbia/Modelling/CensusData/")
BMU_data <- read.csv(file = "Census_by_Hexagon_2022_Surrey.csv", header=TRUE, sep=",", strip.white=TRUE)
BMU_data[BMU_data == NA] <- NA #Turn all zero values to NaNs (as it means there is no access to that part of the map)

# Read hexagonal spatialpolygonsdataframe from the 'MakingHexagonMaps' script
hex_shapes <- hex_got

#Convert from UTM to WGS84
hex_shapes <- spTransform(hex_shapes, CRS("+proj=longlat +datum=WGS84"))

#Fortify and join the hex_shapes file
hex_shapes@data$id <- rownames(hex_shapes@data)
hex_shapes.df <- fortify(hex_shapes)
hex_shapes.df <- join(hex_shapes.df, hex_shapes@data, by="id") #This 'rejoins' the fortified file

#Create a datafile by merging the hexagon polygons with the accessibility data (make sure the accessibility data has an 'ID' column)
hex_shapes.df <- merge(hex_shapes.df, BMU_data, by.x="id", by.y="idhex", all.x=T, all.y=T)

#Define centre of map using Google location
CenterOfMap <- geocode("Northview Golf & Country Club, BC") 
#Get google maps background in correct location; set zoom (10-11 is usually OK)
#For terrain version
City <- get_map(c(lon=CenterOfMap$lon, lat=CenterOfMap$lat),zoom = 11, maptype = "terrain", source = "google", color="bw")
CityMap <- ggmap(City, extent = "device") #Plots the map, but without the long/lat axes (margins)

#****** RAW CLUSTERS *******
CityMap <- CityMap + geom_polygon(aes(x=long, y=lat, fill=SOM_BMU_basedon2016_richandpoor, group=group), data=hex_shapes.df, alpha =0.7) 
CityMap <- CityMap + scale_fill_gradientn(colours = c("red3","orangered","orange","yellow","darkorchid4","mediumblue","skyblue","darkolivegreen1"),limits=c(1, 8), space = "Lab", na.value = "transparent", guide = "colourbar")
CityMap <- CityMap + scale_fill_gradientn(colours = c("red3","orangered","orange","darkorchid4","mediumblue","skyblue"),limits=c(1, 6), space = "Lab", na.value = "transparent", guide = "colourbar")
CityMap <- CityMap + scale_fill_gradientn(colours = c("red3","orangered","darkorchid4","mediumblue"),limits=c(1, 4), space = "Lab", na.value = "transparent", guide = "colourbar")
CityMap <- CityMap + scale_fill_gradientn(colours = c("hotpink","mediumblue"),limits=c(1, 2), space = "Lab", na.value = "transparent", guide = "colourbar")

#******CLUSTER DISTANCE******
#CityMap <- CityMap + geom_polygon(aes(x=long, y=lat, fill=Euclidean_distance_between_2016_2022_clusters, group=group), data=hex_shapes.df, alpha =0.7) 
#CityMap <- CityMap + scale_fill_gradientn(colours = c("mistyrose","palevioletred1", "deeppink1","deeppink4"),limits=c(0.01,1), space = "Lab", na.value = "transparent", guide = "colourbar")

#******PCs******
#CityMap <- CityMap + geom_polygon(aes(x=long, y=lat, fill=PCA_PC1, group=group), data=hex_shapes.df, alpha =0.7) 
#CityMap <- CityMap + scale_fill_gradientn(colours = c("hotpink", "lightpink", "lightyellow", "springgreen3", "darkslategray"), limits=c(-0.05,0.05), space = "Lab", na.value = "transparent", guide = "colourbar")

CityMap <- CityMap + theme(legend.position="bottom") + labs(fill = "Cluster number") 

print(CityMap)


