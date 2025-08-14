# this script is for cleaning the clarkia occurence data
library("terra")
library("maps")
library("sf")
library("rnaturalearth")
library("geodata")

###---- general outline
# 1.Remove NA’s
# 2.Check X and Y coordinates (visually)
# 3.Make sure all points are within study area, if they are not move to nearest valid pixel
# 4.Eliminate those that are “too close”, within 0.1 longitude and latitude of each other
#       Use similar to elis code (idrk how this works bc apparently “pointDistance” gets the shorter length of the arc between two points then those closer than 0.1 degrees are eliminated and 0.1 degrees near the equator is about 11.1 km, seems a bit big)
# 5.try to eliminate sampling biases (optional ???)
#   a. Reduce species clustering
#       By: removing data until it looks homogenous or by using spatial analytics (nearest neighbor index) ( do not remove to many points, deciding which to delete are subjective)
#       Use a greedy algorithm to get it as close to one as possible

#importing clakria data ----
#if it says its not showing up run these two lines: 
  # getwd()
  # setwd('..')
  # getwd() //by here it should be under sdm project
clarkia_data <- read.csv(file = "data/ClarkiaUnguiculata/calflora-Clarkia.csv")

data <- subset(clarkia_data, select = c(ID, Latitude, Longitude, Location.Description)) #creating a clakria data frame with onlu ID, Latitude, Longitude, Location.Descriptio

data_filtered <- data[(!is.na(data$Latitude))|(!is.na(data$Longitude)),] #removing from data only if the Latitude OR Longitude is NA

#Checking the X and Y coordinates visually also makeing sure all points are within the study area ----

# Get California boundary
usa <- ne_states(country = "United States of America", returnclass = "sf")
california <- usa[usa$name == "California", ]

#getting world map
world <- ne_countries(scale = "medium", returnclass = "sf")

# Convert to SpatVector
california_vect <- vect(california)
world_vect <- vect(world)

# Determine geographic extent of data, from elis code and a tutorial online
max_lat <- ceiling(max(data_filtered$Latitude))
min_lat <- floor(min(data_filtered$Latitude))
max_lon <- ceiling(max(data_filtered$Longitude))
min_lon <- floor(min(data_filtered$Longitude))
# Store boundaries in a single extent object
geographic_extent <- ext(x = c(min_lon, max_lon, min_lat, max_lat))

# Download data with geodata's world function to use for our base map
world_map <- world(resolution = 3,
                   path = "data/")

# Crop the map to our area of interest then zooming out a little
my_map <- crop(x = world_map, y = 1.25*geographic_extent)

#plotting occurrence4 points on world map
plot(world_vect, axes = TRUE, col = "grey95")
plot(california_vect, add = TRUE, border = "red", lwd = 1.5) #adding CA outline
points(x = data_filtered$Longitude,
       y = data_filtered$Latitude,
       col = "olivedrab",
       pch = 20,
       cex = 0.75)

# Plot occurence points on map in with areas surrounding CA 
plot(my_map,
     axes = TRUE, 
     col = "grey95")
# Add California outline
plot(california_vect, add = TRUE, border = "black", lwd = 1.5)
# Add the points for individual observations
points(x = data_filtered$Longitude, 
       y = data_filtered$Latitude, 
       col = "olivedrab", 
       pch = 20, 
       cex = 0.75)

#plotting the data points on only onto CA map, seems that everything is in order!
plot(california_vect, axes = TRUE, col = "grey95")
points(x = data_filtered$Longitude,
       y = data_filtered$Latitude,
       col = "olivedrab",
       pch = 20,
       cex = 0.75)

# eliminating points that are "too close" withing 0.1 degrees (i think that eli meant meters) of each other. 
# when talking about the distane i'm talking about the geodisic distance (shortest arc on the surface of an ellipsoid)
#using the distance function

# x in distance() needs to be a matrix
temp_data <- data_filtered[,c("Longitude","Latitude")] # just selecting which columns to use
matrix_filtered <- data.matrix(temp_data) # converting into a matrix

point_distance <- distance(x = matrix_filtered, unit = "m", lonlat = TRUE) # computing the distance between every pair of points

