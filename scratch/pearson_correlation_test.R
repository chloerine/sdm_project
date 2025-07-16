###README
#this file contains some scratch code use to test terra and doing pearson correlation coefficieints 

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

#downloading libraries if needed and setting up libraries
needpackage("terra")
needpackage("rnaturalearth")
needpackage("rnaturalearthdata")
needpackage("sf")

#getting the state of california 

# this is reading the bioX variable, 2.1 worldclim values is just raw data (1.4 is scaled by 10)
bio1 <- rast("data\\wc2.1_30s_bio\\wc2.1_30s_bio_1.tif") #annual mean temp
bio2 <- rast("data\\wc2.1_30s_bio\\wc2.1_30s_bio_2.tif") #mean diurnal range



#these inspect the raster
print(bio1)
plot(bio1)
plot(bio2)





