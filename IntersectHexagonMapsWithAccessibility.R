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
#'hex_got' from the 'MakingHexagonalMaps' script
#****************************

# Read accessibility data from the analysed traveltimematrix 
setwd("/Users/jeromemayaud/Documents/University/BritishColumbia/Modelling/RStudioProjects/MakingHexagonalMaps/ANALYSED_traveltime_matrices/Vancouver")
accessibility_data <- read.csv(file = "Analysed_traveltimematrix_Vancouver_30mins_19Sep2017.csv", header=TRUE, sep=",", strip.white=TRUE)
accessibility_data[accessibility_data == 0] <- NA #Turn all zero values to NaNs (as it means there is no access to that part of the map)

# Read hexagonal spatialpolygonsdataframe from the 'MakingHexagonMaps' script
hex_shapes <- hex_got

#Convert from UTM to WGS84
hex_shapes <- spTransform(hex_shapes, CRS("+proj=longlat +datum=WGS84"))

#Fortify and join the hex_shapes file
hex_shapes@data$id <- rownames(hex_shapes@data)
hex_shapes.df <- fortify(hex_shapes)
hex_shapes.df <- join(hex_shapes.df, hex_shapes@data, by="id") #This 'rejoins' the fortified file

#Create a datafile by merging the hexagon polygons with the accessibility data (make sure the accessibility data has an 'ID' column)
hex_shapes.df <- merge(hex_shapes.df, accessibility_data, by.x="id", by.y="ID", all.x=T, all.y=T)

#Create the map layers
ggp <- ggplot(data=hex_shapes.df, aes(x=long, y=lat, group=group)) 
ggp <- ggp + geom_polygon(aes(fill=Access)) #Access is the name of the data column
#ggp <- ggp + geom_path(color="black", linestyle=4)  # Draw boundaries
ggp <- ggp + scale_fill_gradientn(colours = c("navyblue", "seagreen4", "yellow"), limits=c(0,100), oob=squish, space = "Lab", na.value = "transparent", guide = "colourbar") + theme(legend.position="bottom")
ggp <- ggp + labs(title="Accessibility in Surrey")
print(ggp) #Render the map

