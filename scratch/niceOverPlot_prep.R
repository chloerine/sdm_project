#the prupose of this script is to prepare the data for the niceOverPlot fcn

library("terra")
library("maps")
library("sf")
library("rnaturalearth")
library("geodata")

# this is the cleaned Clarkia Ungi occurence data, this data has removed NA's and has been spatially filtered (removed points that were closer than 1000 meters and removed further points until an NNI close to 1 was achieved )
ungu_data <- read.csv(file = "output/clarkia_occurence_fin.csv")

#getting the bioclim data to the shape of CA----
input_dir <- "data/wc2.1_30s_bio/"
files <- list.files(input_dir, pattern = "\\.tif$", full.names = TRUE)
bioclim_stack <- rast(files)  # Creates a 19-layer SpatRaster

# Get California boundary
usa <- ne_states(country = "United States of America", returnclass = "sf")
california <- usa[usa$name == "California", ]

# Convert to SpatVector
california_vect <- vect(california)

#cropping the bioclim stack
biocrop <- crop(x = bioclim_stack, y = california_vect)

#masking the biocrop stack
bioclim_ca <- mask(x = biocrop, mask = california_vect)

#reordering the bioclim variables so that it in order 1,2,...19
in_order <- order(as.numeric(gsub("\\D", "", names(bioclim_ca))))
bioclim_ca <- bioclim_ca[[in_order]]
names(bioclim_ca) <- paste0("bio", 1:19) #renaming bioclim_ca to bioX where X=1,2,...,19, 
plot(bioclim_ca[["bio19"]]) # its actually cropped

#replacing id column with presence, changing all values to 1----
colnames(ungu_data)[1] <- "presence"
ungu_data[1] <- 1

#creating pseudo-absences, 10000 randomly selected points----
set.seed(123) # setting seed for reproducability

background <- spatSample(x = bioclim_ca,
                         size = 10000, # generate 10,000 pseudo-absence points
                         values = FALSE, # don't need values
                         na.rm = TRUE, # don't sample from ocean
                         xy = TRUE) # just need coordinates

# Look at first few rows of background
head(background)

# Plot the base map
plot(california_vect,
     axes = TRUE,
     col = "grey95")
#title("rng pseudo absence points") # looks weird if i add a title (poitns get squished together)
points(background,
       col = "red",
       pch = 20,
       cex = 0.01)

absence_data <- data.frame(background)
absence_data["presence"]<-0
colnames(absence_data)[colnames(absence_data) %in% c("x","y")]<- c("longitude","latitude")

all_data <- rbind(absence_data,ungu_data)

#extracting bioclim data at the points
bioclim_extract <- extract(bioclim_ca, all_data[,c("longitude","latitude")],ID = FALSE)

point_climate <- cbind(all_data,bioclim_extract)

#removing NA, and keeping track of how many rows removed (eli did it like this only checking removed rows at the end of the process )
rows_before <- nrow(point_climate)
point_climate <- na.omit(point_climate)
rows_after <- nrow(point_climate)
num_rows_deleted <- rows_before - rows_after

message(sprintf("Number of rows removed %f", num_rows_deleted))


#for ecospat----
# pa column is from highest to lowest, ungi[1], pseudo-absence last [0]
#count how many sample of each species
point_climate_order <- order(point_climate$presence,decreasing = TRUE)
pc_sorted <- point_climate[point_climate_order,] #sorted point_climate points, this is the final prodcuct for eco_spat
num_of_samples <- table(pc_sorted$presence) 
num_pseudo <- num_of_samples["0"] #number of pseudo-absence
num_ungu <- num_of_samples["1"] #number of ungu occurences

#for niceOverPlot----
#make copy of above data
#kept track of all the pa column numbers and ensured that the species/pseudo-abscence points are in the proper order
#delete pa column and decimalLat and DecimalLong columns, add new col that is an ascending 'count' for each row so that row 2(after the headers) will be 1 and for row 3 it will be 2, row four will be 3
# now the data should be ready
pc_sorted_nice <- pc_sorted
pc_sorted_nice$longitude <- NULL
pc_sorted_nice$latitude <- NULL
pc_sorted_nice$presence <- NULL

 
write.csv(pc_sorted,file = "C:/Users/joazr/School/RESEARCH/sdm_project/output/ecospat_ungu_fin.csv",row.names = FALSE)
write.csv(pc_sorted_nice,file = "C:/Users/joazr/School/RESEARCH/sdm_project/output/niceOver_ungu_fin.csv",row.names = FALSE)

#data for ecospat and niceOverPlot Should be ready



