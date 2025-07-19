###README
#this file contains some scratch code use to test terra and doing pearson correlation coefficieints 
# this is for eliminating some paris of bioclim variables 
#also messing around with what you can do in R 

# installing packages needed to work
# installing terra which is needed to read the raster data from bioclimn

#libraries
library("terra")
library("maps")
library("sf")
library("rnaturalearth")

#function identifying if the column is a specific number
id_cor <- function(matrix,num){
  temp_cor <- list()
  for (row in 1:nrow(matrix)){
    if ((matrix[[row,2]] == num)&(matrix[[row,1]]!=num)){
      temp_cor <- c(temp_cor, matrix[[row,1]])
    }
  }
  return(temp_cor)
}

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

#reordering the bioclim variables so that it in order 1,2,...19
in_order <- order(as.numeric(gsub("\\D", "", names(bioclim_ca))))
bioclim_ca <- bioclim_ca[[in_order]]
names(bioclim_ca) <- paste0("bio", 1:19) #renaming bioclim_ca to bioX where x=1,2,...,19

#extracting the values of the bioclim variables
bio_val <- values(bioclim_ca)
bio_matrix <- na.omit(as.matrix(bio_val)) #making into a matrix then deleting all rows where there was a NaN

#checking no na or nan in matrix
if (any(is.na(bio_matrix))) {
  message("There is at least one NA or NaN in bio_matrix")
} else {
  message("There are no NA or NaN values in bio_matrix")
}

#computing the pearson correalation, puts it into a matrix
p_cor_raw <- cor(x = bio_matrix, method = "pearson")

#finding which biolcim variables are highly correlated elimi nate higher than 0.75 
p_cor_abs <- abs(x = p_cor_raw)

#all_list- a list containt all the sublists 1-19 of high correlation variables
layer_names <- names(bioclim_ca)
cor_pairs <- expand.grid(var1 = layer_names,var2 = layer_names)
cor_pairs$correlation <- as.vector(p_cor_abs)
  

