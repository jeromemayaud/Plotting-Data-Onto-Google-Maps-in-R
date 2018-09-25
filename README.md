# Plotting-Data-Onto-Google-Maps-in-R
A series of functions to plot data (e.g. census) onto Google Maps in R. 

These functions were used to analyse and visualise urban and population data in the following publications:

Mayaud, J. R., Tran, M., Pereira, R. H. M. & Nuttall, R. (2018). Future access to essential services in a growing smart city: The case of Surrey, British Columbia. Computers, Environment and Urban Systems.

# Running the code
The functions are as follows, and should be run in this order:

MakingHexagonalMaps.R --> Creates a grid of hexagonal cells that cover your study area.

IntersectHexagonMapsWithAccessibility.R --> Combines data (in this case, the accessibility of each grid cell to specific facilities in the study area) with hexagon maps built in the previous function. Requires output from MakingHexagonalMaps.R file. 

PlottingHexagonMapsOntoGoogleMaps.R --> Plots hexagonal grid onto an underlying Google map of the study area. Requires output from IntersectHexagonMapsWithAccessibility.R file.

PlottingRoutesStopsOntoMaps.R --> Plots the transit lines and transit stops for the study area. Requires a shapefile of the transportation network. Requires output from PlottingHexagonMapsOntoGoogleMaps.R file.
