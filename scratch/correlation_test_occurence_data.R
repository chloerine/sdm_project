#READ me this gets the pearson correlation coefficient with the selected bioclim variables and occurence data

library(terra)
library(ggplot2)
library(sf)
library(maps)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)


raw_clarkia_data <- read.csv(file = "data/ClarkiaUnguiculata/calflora-Clarkia.csv")

# Get California boundary
usa <- ne_states(country = "United States of America", returnclass = "sf")
california <- usa[usa$name == "California", ]

# Convert to SpatVector
california_vect <- vect(california)

#cropping biolcim variables to CA
input_dir <- "data/wc2.1_30s_bio/"
files <- list.files(input_dir, pattern = "\\.tif$", full.names = TRUE)
bioclim_stack <- rast(files)  # Creates a 19-layer SpatRaster

#cropping the bioclim stack
biocrop <- crop(x = bioclim_stack, y = california_vect)

#masking the biocrop stack
bioclim_ca <- mask(x = biocrop, mask = california_vect)

#reordering the bioclim variables so that it in order 1,2,...19
in_order <- order(as.numeric(gsub("\\D", "", names(bioclim_ca))))
bioclim_ca <- bioclim_ca[[in_order]]
names(bioclim_ca) <- paste0("bio", 1:19) #renaming bioclim_ca to bioX where X=1,2,...,19, 
plot(bioclim_ca[["bio19"]]) # its actually cropped

#removing all the NA location values from clarkia_data and low location quality (low loc quality an have radious of <798meters)
clarkia_na <- na.omit(raw_clarkia_data)
clarkia_data <- clarkia_na[clarkia_na$Location.Quality != "low", ]

#plotting the data points
plot(california_vect, axes = TRUE, col = "grey95")
points(x = clarkia_data$Longitude,
       y = clarkia_data$Latitude,
       col = "olivedrab",
       pch = 20,
       cex = 0.75)

#coordinates of presence data 
clarkia_presence <- clarkia_data[, c("Longitude","Latitude")]


#idk if this works or not===
#presence data
clarkia_points <- vect(clarkia_presence,
                       geom = c("Longitude", "Latitude"),
                       crs = "EPSG:4326")

#creating and getting values of absence points, can restrict iot to places where 
background_points <- spatSample(x = bioclim_ca, size = 10000,method = "random", na.rm = TRUE, as.points = TRUE, xy = TRUE)

#plotting absence points
points(x = background_points$x,
       y = background_points$y,
       col = "red",
       pch = 1,
       cex = 0.1)

#data frame containing the data
presence_vals <- extract(bioclim_ca, clarkia_points) # getting bioclim vals at these points
presence_frame <- data.frame(presence_vals[, -1], presence = 1) # setting up frame with values at points and presence
presence_frame <- presence_frame[complete.cases(presence_frame), ] #getting rid of NA

background_vals <- extract(bioclim_ca, background_points)
background_frame <- data.frame(background_vals[, -1], presence = 0)
background_frame <- background_frame[complete.cases(background_frame), ]

#getting combined data frame
combined_frame <- rbind(presence_frame,background_frame)

cor_with_presence <- numeric(19)

for (i in 1:19){
  varnames<- paste0("bio",i)
  cor_with_presence[i] <- cor(combined_frame[[varname]],combined_frame$"presence",use = "complete.obs")
}

# Combine results into a data.frame
cor_results <- data.frame(
  variable = paste0("bio", 1:19),
  correlation_with_presence = cor_with_presence
)












