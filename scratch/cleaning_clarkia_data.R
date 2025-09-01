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
options(warn = 2)
clarkia_data <- read.csv(file = "data/ClarkiaUnguiculata/calflora-Clarkia.csv")

data <- subset(clarkia_data, select = c(ID, Latitude, Longitude, Location.Description)) #creating a clakria data frame with onlu ID, Latitude, Longitude, Location.Descriptio

data_na <- data[(!is.na(data$Latitude))&(!is.na(data$Longitude)),] #removing from data only if the Latitude OR Longitude is NA
data_filtered <- data_na[!duplicated(data_na[,c("Latitude","Longitude")]),] #removing duplicates, after removing dupluicates the rows arent numbered coorectly so the distance matrix looks like nonsense
rownames(data_filtered) <- 1:nrow(data_filtered)
#checking which rows were duplicated, compare them with data_na, from just sampling a few of them it seems that this is right
duplicated_rows <- duplicated(data_na[,c("Latitude","Longitude")]) # gets after first occurence
removed_rows <- data_na[duplicated_rows,]

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

# eliminating points that are "too close" withing 0.1 degrees (i think that eli meant meters) of each other. ----
# when talking about the distane i'm talking about the geodisic distance (shortest arc on the surface of an ellipsoid)
#using the distance function

distance_threshold <- 0.1

# x in distance() needs to be a matrix
temp_data <- data_filtered[,c("Longitude","Latitude")] # just selecting which columns to use
matrix_filtered <- as.matrix(temp_data) # converting into a matrix

point_distance <- distance(x = matrix_filtered, unit = "m", lonlat = TRUE) # computing the distance between every pair of points. on the ones ive checked this has give the exact same results as eli's code so im gonna take this as right
dist_matrix <- as.matrix(point_distance) 
remove_points <- which((dist_matrix < distance_threshold)&(row(dist_matrix)!=col(dist_matrix)), arr.ind = TRUE) #finding which points to remove while excluding the diagonal (all zeroes)

for (x in 1:nrow(remove_points)){ # removes one of the to close points\
  delete_rows <- remove_points[x,1]
  data_filtered_fin <- data_filtered[-delete_rows,]
}
rownames(data_filtered_fin) <- 1:nrow(data_filtered_fin) 

# getting nearest neighbor index as close to one as possible ----
#Get initial nearest neighbor index.
  #Eliminate 1 point. 
  #Check NNI
  #If nearest neighbor index is closer to 1 than previous NNI
    #Set to new NNI
    #Create new set of neighbors
  #If not closer to 1 than previous NNI 
    #Reinstate eliminated point
    #Go to next point
  #If at end of points
    #Return list of kept points

library("spatialEco")
library("spatstat.geom")
nni_goal <- 1
df_temp <- st_as_sf(data_filtered_fin, coords = c("Longitude","Latitude"), crs = 4326) #converting to a sf object
nni_temp <- nni(x = df_temp) #getting initial nni
df_main <- df_temp #setting inital main df
nni_main <- nni_temp  #setting inital main nni


id_main <- df_main$ID
x <- 0
num_removed <- 0

for(i in id_main){
  #print(df_temp$ID[x])
  #message(x)
  df_temp <- df_main[!(df_main$ID %in% i),]
  nni_temp <- nni(df_temp)
  if (abs(nni_goal - nni_temp$NNI) < abs(nni_goal - nni_main$NNI)){
    df_main <- df_temp
    nni_main <-nni_temp
    #message(nni_main$NNI)
    #message(num_removed)
    num_removed <- num_removed + 1
  }else{
    df_temp <- df_main
    nni_temp <- nni_main
  }
  #x<-x+1
  if (abs(nni_goal-nni_main$NNI) < 0.25){
    break
  }
}

message(nni_main$NNI)




