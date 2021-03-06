# Plotting-Data-Onto-Google-Maps-in-R
A series of functions to plot data (e.g. census) onto Google Maps in R. 

These functions were used to analyse and visualise urban and population data in the following publications:

Mayaud, J. R., Tran, M., Pereira, R. H. M. & Nuttall, R. (2018). Future access to essential services in a growing smart city: The case of Surrey, British Columbia. Computers, Environment and Urban Systems.

# Running the code
The functions are as follows, and should be run in this order:

MakingHexagonalMaps.R --> Creates a grid of hexagonal cells that cover your study area.

IntersectHexagonMapsWithCensus.R --> Combines census data with the hexagon grid. Standalone script that does not require output from another file. 

IntersectHexagonMapsWithAccessibility.R --> Combines data (in this case, the accessibility of each grid cell to specific facilities in the study area) with the hexagon grid. Requires output from MakingHexagonalMaps.R file. 

IntersectHexagonMapsWithBMUs.R --> Combines data (in this case, the BMU (best matching unit) and/or PC (principal component) of each grid cell) with the hexagon grid. Requires output from MakingHexagonalMaps.R file. 

PlottingHexagonMapsOntoGoogleMaps.R --> Plots hexagonal grid onto an underlying Google map of the study area. Requires output from IntersectHexagonMapsWithAccessibility.R file.

PlottingRoutesStopsOntoMaps.R --> Plots the transit lines and transit stops for the study area. Requires a shapefile of the transportation network. Requires output from PlottingHexagonMapsOntoGoogleMaps.R file.

PlottingOriginAccessToFacilitiesOntoGoogleMaps.R --> Plots the accessibility of each grid cell to certain facilities on a Google Map. Requires output from MakingHexagonalMaps.R file. 
