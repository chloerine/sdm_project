###README
#this file contains some scratch code use to test terra and doing pearson correlation coefficieints 
#also messing around with what you can do in R 

# installing packages needed to work
# installing terra which is needed to read the raster data from bioclimn

#this function just downloads and loads the package libraries, takes a string of package name as an input
needpackage <- function(package){
  if(!requireNamespace(package)){
    install.packages(package)
    message("Installing package: ", package)
  }
  library(package, character.only = TRUE)
}

#this funcion takes 

#downloading libraries if needed and setting up libraries
needpackage("terra")
needpackage("maps")
needpackage("sf")
needpackage("rnaturalearth")

#getting the state of california 

# this is reading the bioX variable, 2.1 worldclim values is just raw data (1.4 is scaled by 10)
#maybe this can be better found using geodata

# Read all rasters in at once
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






