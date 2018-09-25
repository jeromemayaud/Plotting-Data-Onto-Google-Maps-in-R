library("spatialEco")
library("sp")
library("rgdal")
library("maptools")
library("ggplot2")
library("plyr")
library("rgeos")
library("foreign")
library("ggmap")
library("tigris")
library("RColorBrewer")
library("RgoogleMaps")
library("raster")
library("dplyr")
library("scales")

setwd("/Users/jeromemayaud/Documents/University/BritishColumbia/Modelling/RStudioProjects/MakingHexagonalMaps")
#Read shape file
urbanmetro_mal <- readOGR(dsn="/Users/jeromemayaud/Documents/University/BritishColumbia/Modelling/RStudioProjects/MakingHexagonalMaps/SeattleShapeFile", layer="SeattleShape")    
#Assign projection in UTM, because HexGrid works in metres (zone 10 aligns with Vancouver area)
urbanmetro_mal <- spTransform(urbanmetro_mal, CRS("+proj=utm +zone=10 +ellps=GRS80 +units=m +no_defs"))
#* * * * * HEXAGONAL GRID FUNCTION * * * * 
HexGrid <- function(mycellsize, originlpolygon) { 
  #Define size of hexagon bins in meters to create points
  HexPts <- spsample(originlpolygon, type="hexagonal", offset=c(0,0), cellsize=mycellsize)
  #Create Grid - transform into spatial polygons
  HexPols <- HexPoints2SpatialPolygons(HexPts)
  # convert to spatial polygon data frame
  df <- data.frame(idhex = getSpPPolygonsIDSlots(HexPols))
  row.names(df) <- getSpPPolygonsIDSlots(HexPols)
  hexgrid <- SpatialPolygonsDataFrame(HexPols, data =df)
  return(hexgrid)
}
#* * * * * CREATE HEXAGONAL GRID * * * * 
#This makes the hexagons 500 meters in diameter
hex_got <- HexGrid(500, urbanmetro_mal)
#**********************************************
hexagon_size = 500 #The diameter of an individual hexagon
setwd("/Users/jeromemayaud/Documents/University/BritishColumbia/Modelling/RStudioProjects/MakingHexagonalMaps/CENSUS_DATA/")
census_data <- read.csv(file = "All_2018_Seattle.csv", header=TRUE, sep=",", strip.white=TRUE)
census_data[census_data == NA] <- NA #Turn all zero values to NaNs (as it means there is no access to that part of the map)
#Load the Dissemination Area shapefile
DA_map <- readOGR(dsn="/Users/jeromemayaud/Documents/University/BritishColumbia/Modelling/CensusData/CensusBlockGroups_Washington/", layer="cb_2016_53_bg_500k")   
DA_map <- spTransform(DA_map, CRS("+proj=longlat +datum=WGS84"))
#Crop the DA shapefile to just the area you want
x_coord <- c(-122.5,  -122.5,  -122.2, -122.2, -122.5)
y_coord <- c(47.4, 47.8, 47.8, 47.4, 47.4)
xym <- cbind(x_coord, y_coord)
p = Polygon(xym)
ps = Polygons(list(p),1)
sps = SpatialPolygons(list(ps))
proj4string(sps) <- CRS("+proj=longlat +datum=WGS84") #Convert to same lat/long as census data
DA_map_cropped <- crop(DA_map, sps) #Crop the map
#Create a SpatialPolygonsDataFrame
DA_map.spdf <- geo_join(DA_map_cropped, census_data, "GEOID", "BGID")
#####################################################################

#Assign the census tract info from hexagonal grid from 'MakingHexagonMaps' script
census_tract <- DA_map.spdf
census_tract$Area_sqm <- area(census_tract) #ADD A NEW COLUMN WITH THE AREA OF EACH CENSUS TRACT (in metres squared) - VERY IMPORTANT FOR DIVIDING UP THE CENSUS INTO HEXAGONS LATER
hex_grid <- spTransform(hex_got, CRS("+proj=longlat +datum=WGS84")) # Convert hex_grid from UTM to WGS84
hex_shapes_1 <- hex_grid #hex_shapes_1 is to differentiate it from hex_shapes that is outputted from IntersectHexagonMapsWithAccessibility script

#Fortify and join the hex_shapes_1 file
hex_shapes_1@data$id <- rownames(hex_shapes_1@data)
hex_shapes_1.df <- fortify(hex_shapes_1)
hex_shapes_1.df <- join(hex_shapes_1.df, hex_shapes_1@data, by="id") #This 'rejoins' the fortified file

#Calculate centroids of all hexagonal cells in hex_grid
hexgrid_centroids <- SpatialPointsDataFrame(gCentroid(hex_grid, byid=TRUE), hex_grid@data, match.ID=FALSE)

#The spatial_key contains info on where the hexagonal cell centroids overlay onto the census tract
spatial_key <- over(hexgrid_centroids, census_tract)
hexgrid_centroids <- as.data.frame(hexgrid_centroids) #Need to convert to data table before merging
spatial_key <- as.data.frame(merge(hexgrid_centroids, spatial_key, by.x="idhex", by.y="row.names", all.x=TRUE))

#Identify number of hexagons in each BlockGroup
BGID_column <-  data.frame(spatial_key$BGID)
shared_DAs.df <- as.data.frame(table(BGID_column))

#Merge the info of number of shared DAs with the main spatial key
spatial_key <- as.data.frame(merge(spatial_key, shared_DAs.df, by.x="BGID", by.y="BGID_column", all.x=TRUE))

#Assign the total population of each census tract to its constituent hexagons, by finding the population density per squared metre and multiplying up by hexagon size
spatial_key$CorrectedHexPopulation <- round((spatial_key$Total.Population / spatial_key$Freq))

#Create a datafile by merging the hexagon polygons with the spatial_key that contains the census data
spatial_key_merged <- merge(hex_shapes_1.df, spatial_key, by.x="idhex", by.y="idhex", all.x=TRUE, all.y=TRUE)

########## ONLY IF YOUR COLUMN IS MADE UP OF DECIMALS (e.g. for % change) #############################
spatial_key_merged$PercentChangeHighIncome=as.numeric(levels(spatial_key_merged$PercentChangeHighIncome))[spatial_key_merged$PercentChangeHighIncome]
###################################################################################

####### PLOTTING ####### 
#Plot choropleth map on Google Maps
CenterOfMap <- geocode("Seattle")
City <- get_map(c(lon=CenterOfMap$lon, lat=CenterOfMap$lat),zoom = 11, maptype = "terrain", source = "google", color="bw")
CityMap <- ggmap(City, extent = "device")
CityMap <- CityMap + geom_polygon(aes(x=long, y=lat, fill=Median.Age.Of.Total.Population, group=group), data=spatial_key_merged, alpha =0.7) 
CityMap <- CityMap + scale_fill_gradientn(colours = c("beige", "violetred2", "midnightblue"),limits=c(30,60), oob=squish, space = "Lab", na.value = "transparent", guide = "colourbar") + theme(legend.position="bottom")
CityMap <- CityMap + scale_fill_gradientn(colours = c("hotpink", "lightpink", "lightyellow", "springgreen3", "darkslategray"), limits=c(-400, 400), oob=squish, space = "Lab", na.value = "transparent", guide = "colourbar")
CityMap <- CityMap + theme(legend.position="bottom") + labs(fill = "Total Population") 

print(CityMap)

#Save file with the collocated accessibility and census data
write.csv(spatial_key, "Census_by_Hexagon2.csv", row.names=FALSE) #Don't want row names, otherwise you get the IDs outputted as a separate column
