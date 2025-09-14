#this script is for a niche statistical analysis
# this also does basic plotting of niches

library(ecospat)
library(missMDA)
library(FactoMineR)
library(factoextra)

#laoding dataset
data_ungu <- read.csv("output/ecospat_ungu_fin.csv", header = TRUE)

# Separate the climate variables and species occurrence data
climate_data <- data_ungu[, 4:22]  # Assuming columns 4 to 22 are climate variablesinstall
species_data <- data_ungu$presence  # this distinguishes from occurences and pseudo-absences

#pca on climate data
pca_result <- PCA(climate_data, graph = FALSE)

#visualize pca results
fviz_pca_ind(pca_result, geom.ind = "point", col.ind = as.factor(species_data), addEllipses = TRUE, legend.title = "Species")

# Extract the PCA scores for the first two principal components, contains both results for presences and pseudo-absences
scores <- pca_result$ind$coord[, 1:2]

# Create presence points for each species
presence_1 <- scores[species_data == 1, ]

# Create the background data (all points)
background <- scores

# Create niche objects for each species using ecospat.grid.clim.dyn
grid_clim_1 <- ecospat.grid.clim.dyn(glob = background, glob1 = presence_1, sp = presence_1, R = 100)

# plot niches
ecospat.plot.niche(grid_clim_1, title = "Species 1 Niche")