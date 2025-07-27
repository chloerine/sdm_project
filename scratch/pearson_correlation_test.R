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
# Getting Bioclim Variables ====
# Read all rasters in at once
message("Starting Correlation Test")
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
message("Cropped to CA!")

#extracting the values of the bioclim variables
bio_val <- values(bioclim_ca)
bio_matrix <- na.omit(as.matrix(bio_val)) #making into a matrix then deleting all rows where there was a NaN

#checking no na or nan in matrix
if (any(is.na(bio_matrix))) {
  message("There is at least one NA or NaN in bio_matrix")
} else {
  message("There are no NA or NaN values in bio_matrix")
}

#Getting Pearson Correlatio Coefficient ====
#computing the pearson correalation, puts it into a matrix, the values are mostly mirrored with very small differences, I think this is because of how floating point numbers are computed
p_cor_raw <- cor(x = bio_matrix, method = "pearson")

#getting absolute values
p_cor_abs <- abs(x = p_cor_raw)

#all_list- a list containt all the sublists 1-19 of high correlation variables
cor_lvl <- 0.7
layer_names <- names(bioclim_ca) 
cor_pairs <- expand.grid(var1 = layer_names,var2 = layer_names)
cor_pairs$correlation <- as.vector(p_cor_abs)
cor_pairs_sub <- subset(x = cor_pairs, subset = (cor_pairs$correlation > cor_lvl) & (cor_pairs$var1 != cor_pairs$var2)) #only getting those with correaltion higher thatn 0.75 and those with same pair eg bio1 and bio1 wld have a correlation of 1
message("Found Correlation!")
#Getting Largest uncorrelated subset ====
#goal for this part. I want to create the largest subset where no varaible has a correlation of over 0.75 with each other, this makes everything before kinda useless
#for all (x,y) in a subset the |cor(x,y)| < 0.75

#creating a combination of a certain size from all the bioclim variables
all_subsets <- list()
for (i in 1:length(layer_names)) {
  subsets_i <- combn(x = layer_names, m = i, simplify = FALSE)  # returns a list of combinations of size i where i = 1,2,...,19
  all_subsets <- c(all_subsets, subsets_i)
}


#---- Logic for getting largest subset with non highly correlated variables
# loop through all subsets{
  #select first of pair, going through all elements in the subset increment when finished going with all other pairs)
    #select second of pair, going through all elemetnts in the subset (not the first pair)
      #get index of row 
      #get index of col
      #if pair has high correlation go to next subset
  #if new subset is larger than previous largest then set this as largest valid subset}
subset_six <- list()
largest_subset <- list()
largest_subset_list <- list()
for (subset_index in 1:length(all_subsets)){ #going through all the subsets
  current_subset <- all_subsets[[subset_index]]
  valid_subset <- TRUE # flag to know when to break loop


  
  for (member_one in 1:length(current_subset)){ # getting first member of the pair
    for (member_two in member_one:length(current_subset)){ #getting second member of the pair
      if(all_subsets[[subset_index]][member_one] != all_subsets[[subset_index]][member_two]){ # making sure the members are not the same eg bio1 and boi1 are the pairs
        row_index <- which(rownames(x = p_cor_abs) == current_subset[member_one]) #getting row index, first member
        col_index <- which(colnames(x = p_cor_abs) == current_subset[member_two]) # getting column index, second member
        if(p_cor_abs[row_index,col_index] > cor_lvl){ #if correlation value is to high set the flag to false and break out of loop 
          valid_subset <- FALSE
          break #breaks member_two loop
        }
      }
    }
    if(!valid_subset){ # breaks member_one loop
      break
    }
  }
  if(valid_subset){ #if a vlid subset
    if(length(current_subset) == 6)
      subset_six <- c(subset_six, list(current_subset)) 
    if(length(current_subset) > length(largest_subset)){ # if the current subset is larger than the largest subset set the largest subset to the current subset
      largest_subset <- current_subset 
      largest_subset_list <- list(current_subset) # delete the list of all previous largest subsets and just start with the new largest subset
    } else if(length(current_subset) == length(largest_subset)){ # if the current subset is the same length then just add it to the list of largest subsets
      largest_subset_list <- c(largest_subset_list, list(current_subset)) 
    }
  }
  if (subset_index %% 10000 == 0) { # keeping track, returing values every 10000 subsets iterated through
    message(sprintf("Currently on subset %d / 524287", subset_index))
  }else if (subset_index == 524287){
    message ("Found the largest subsets where the variables aren't highly correlated!")
  }
}
 


      


