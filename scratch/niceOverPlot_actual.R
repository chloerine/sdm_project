#this script is for the niceOverPlot

library(ade4)
library(terra)
library(ecospat)
library(ggplot2)
library(grid)
library(gridExtra)
library(gtable)
library(RColorBrewer) 
library(ggplot2)
library(gridExtra)
library(grid)
library(readr)
library(corrplot)  
library(ggbiplot)
library(reshape2)

#niceOverPLot function ----
###IMPORTANT###
###If you want to play around with how the output environmental space looks, change the bandwidth (bw) and bins (b) in the first line of the of the function
#they are set to NULL by default, but I used bw = 2 and b = 14

###COLORS AND UPDATE FROM ORIGINAL FUNCTION###
# The original script used this color palette "" scale_fill_brewer(palette = "Set1") "", I changed it to specific colors, feel free to change them as you see fit
# i.e. "" scale_fill_manual(values = c("#ab82ff", "#00ffff")) "" to match the color scheme of my other figures. 
# This may cause problems if you use more than 2 species, if that is the case you can try the original niceOverPlot function:
#https://www.r-bloggers.com/2017/05/niceoverplot-or-when-the-number-of-dimensions-does-matter/
#If you do this you will have to update his script to work with more recent versions of R
#the biug change I made was changing the second 'if' from this:
#if (class(sc1)==c("pca","dudi") && class(sc2)==c("pca","dudi")) {
#to this:
# if (inherits(sc1, c("pca", "dudi")) && inherits(sc2, c("pca", "dudi"))) {

# In order to get this plot to work with one species I just excluded all mentions of sc2 and n2 - chloe

niceOverPlot <- function(sc1, sc2 = NULL, n1 = NULL, n2 = NULL, plot.axis = TRUE, bw = NULL, b = NULL, a1cont = NULL, a2cont = NULL, contour_colors = c("#a175ff", "#00bfc4")) {
  
  # Prepare the data, depending on the type of input ("pca"/"dudi" object or raw scores)
  # if (is.null(sc2)) {
  #   sc_1 <- sc1
  #   sc_2 <- sc1
  #   sc1 <- sc_1$li[1:n1, ]
  #   sc2 <- sc_1$li[(n1 + 1):(n1 + n2), ]
  # }
  
  if (inherits(sc1, c("pca", "dudi"))) {
    sc_1 <- sc1
    # sc_2 <- sc2
    sc1 <- sc1$li
    # sc2 <- sc2$li
  }
  
  # Recognize both species
  scores <- rbind(sc1)#, sc2)
  g <- factor(c(rep(0, nrow(sc1))))
  df <- data.frame(x = scores$Axis1, y = scores$Axis2, g = g)
  
  # Establish an empty plot to be placed at top-right corner (X)
  empty <- ggplot() +
    geom_point(aes(1, 1), colour = "white") +
    theme_void()
  
  # sp1
  p1 <- ggplot(data = df, aes(x, y)) +
    stat_density_2d(aes(fill = ..level.., color = factor(g)), alpha = 0.2, bins = b, geom = "polygon", h = c(bw, bw)) +
    scale_fill_gradient(low = "#bd9dff", high = "#bc9cff", space = "Lab", name = "sp1") +
    scale_color_manual(values = contour_colors) +
    theme(legend.position = "none")
  
  # # sp2
  # p2 <- ggplot(data = df, aes(x, y)) +
  #   stat_density_2d(aes(fill = ..level.., color = factor(g)), alpha = 0.2, bins = b, geom = "polygon", h = c(bw, bw)) +
  #   scale_fill_gradient(low = "#67d8ff", high = "#00ffff", space = "Lab", name = "sp2") +
  #   scale_color_manual(values = contour_colors) +
  #   theme(legend.position = "none")
  
  pp1 <- ggplot_build(p1)
  ppp1 <- ggplot_build(
    p1 + aes(alpha = 0.15) + 
      theme_classic() + 
      theme(legend.position = "none", text = element_text(size = 15)) + 
      xlab("axis1") + 
      ylab("axis2") + 
      xlim(c(min(pp1$data[[1]]$x) - 1, max(pp1$data[[1]]$x) + 1)) + 
      ylim(c(min(pp1$data[[1]]$y) - 1, max(pp1$data[[1]]$y) + 1))
  )
  # pp2 <- ggplot_build(
  #   p2 + aes(alpha = 0.15) +
  #     theme_classic() +
  #     theme(legend.position = "none") +
  #     xlab("axis1") +
  #     ylab("axis2") +
  #     xlim(c(min(pp1$data[[1]]$x) - 1, max(pp1$data[[1]]$x) + 1)) +
  #     ylim(c(min(pp1$data[[1]]$y) - 1, max(pp1$data[[1]]$y) + 1))
  # )$data[[1]]
  
  # ppp1$data[[1]]$fill[grep(pattern = "^2", pp2$group)] <- pp2$fill[grep(pattern = "^2", pp2$group)]
  
  grob1 <- ggplot_gtable(ppp1)
  # grob2 <- ggplotGrob(p2)
  grid.newpage()
  grid.draw(grob1)
  
  # Marginal density of x - plot on top
  if (inherits(sc_1, c("pca", "dudi"))) {
    plot_top <- ggplot(df, aes(x, y = ..scaled.., fill = g)) + 
      geom_density(position = "identity", alpha = 0.5) +
      scale_x_continuous(name = paste("Contribution ", round((sc_1$eig[1] * 100) / sum(sc_1$eig), 2), "%", sep = ""), limits = c(min(pp1$data[[1]]$x) - 0.5, max(pp1$data[[1]]$x) + 0.5)) +
      scale_fill_manual(values = c("#ab82ff", "#00ffff")) + 
      theme_classic() + 
      theme(legend.position = "none")
  } else {
    if (is.null(a1cont)) {
      plot_top <- ggplot(df, aes(x, y = ..scaled.., fill = g)) + 
        geom_density(position = "identity", alpha = 0.5) +
        scale_x_continuous(name = "axis1", limits = c(min(pp1$data[[1]]$x) - 0.5, max(pp1$data[[1]]$x) + 0.5)) +
        scale_fill_manual(values = c("#ab82ff")) + 
        theme_classic() + 
        theme(legend.position = "none")
    } else {
      plot_top <- ggplot(df, aes(x, y = ..scaled.., fill = g)) + 
        geom_density(position = "identity", alpha = 0.5) +
        scale_x_continuous(name = paste("Contribution ", a1cont, "%", sep = ""), limits = c(min(pp1$data[[1]]$x) - 0.5, max(pp1$data[[1]]$x) + 0.5)) +
        scale_fill_manual(values = c("#ab82ff", "#00ffff")) +
        theme_classic() + 
        theme(legend.position = "none")
    }
  }
  
  # Marginal density of y - plot on the right
  if (inherits(sc_1, c("pca", "dudi"))) {
    plot_right <- ggplot(df, aes(y, y = ..scaled.., fill = g)) + 
      geom_density(position = "identity", alpha = 0.5) + 
      scale_x_continuous(name = paste("Contribution ", round((sc_1$eig[2] * 100) / sum(sc_1$eig), 2), "%", sep = ""), limits = c(min(pp1$data[[1]]$y) - 0.5, max(pp1$data[[1]]$y) + 0.5)) +
      coord_flip() + 
      scale_fill_manual(values = c("#ab82ff", "#00ffff")) + 
      theme_classic() + 
      theme(legend.position = "none")
  } else {
    if (is.null(a2cont)) {
      plot_right <- ggplot(df, aes(y, y = ..scaled.., fill = g)) + 
        geom_density(position = "identity", alpha = 0.5) + 
        scale_x_continuous(name = "axis2", limits = c(min(pp1$data[[1]]$y) - 0.5, max(pp1$data[[1]]$y) + 0.5)) +
        coord_flip() + 
        scale_fill_manual(values = c("#ab82ff", "#00ffff")) + 
        theme_classic() + 
        theme(legend.position = "none")
    } else {
      plot_right <- ggplot(df, aes(y, y = ..scaled.., fill = g)) + 
        geom_density(position = "identity", alpha = 0.5) + 
        scale_x_continuous(name = paste("Contribution ", a2cont, "%", sep = ""), limits = c(min(pp1$data[[1]]$y) - 0.5, max(pp1$data[[1]]$y) + 0.5)) +
        coord_flip() + 
        scale_fill_manual(values = c("#ab82ff", "#00ffff")) +
        theme_classic() + 
        theme(legend.position = "none")
    }
  }
  
  if (plot.axis == TRUE) {
    grid.arrange(plot_top, empty, grob1, plot_right, ncol = 2, nrow = 2, widths = c(4, 1), heights = c(1, 4))
  } else {
    grid.arrange(empty, grob1, ncol = 2, nrow = 2, widths = c(0.01, 10), heights = c(0.01, 10))
  }
}

#test with all 19 bioclim varaibles----
data_ungu <- read.csv("output/niceOver_ungu_fin.csv", header = TRUE)
pca_ungu = dudi.pca(df = na.omit(data_ungu), center = TRUE, scale = TRUE, scannf = FALSE, nf = 2)
niceOverPlot(pca_ungu,n1=307, bw = 2, b = 14)

#plots with selected subsets of variables----
#previously excluded highly correlated variabes pearson correlation > 0.7
subset_one_7 <- c("bio2",  "bio3",  "bio5",  "bio11", "bio15", "bio16", "bio18" ) # this is one one were gonna opt for in the SDM
subset_two_7 <- c("bio2",  "bio3",  "bio6",  "bio10", "bio15", "bio16", "bio18" )
subset_three_6 <- c("bio3",  "bio5",  "bio8",  "bio15", "bio16", "bio18" ) # best one of length 6

#creating the data_ungu sets where only valid bioclim variables are held
data_ungu_one <- data_ungu[,subset_one_7]
data_ungu_two <- data_ungu[,subset_two_7]
data_ungu_three <- data_ungu[,subset_three_6]

#running dudi.pca() on data_ungu_x, x is 1,2,3. creating PCA subsets
pca_ungu_one = dudi.pca(df = na.omit(data_ungu_one), center = TRUE, scale = TRUE, scannf = FALSE, nf = 2)
pca_ungu_two = dudi.pca(df = na.omit(data_ungu_two), center = TRUE, scale = TRUE, scannf = FALSE, nf = 2)
pca_ungu_three = dudi.pca(df = na.omit(data_ungu_three), center = TRUE, scale = TRUE, scannf = FALSE, nf = 2)

#see which varioables are represented by each pca axis----

#perform pca
pca_ungu_one_sum = dudi.pca(df = na.omit(data_ungu_one), center = TRUE, scale = TRUE, scannf = FALSE, nf = 2)
pca_ungu_two_sum = dudi.pca(df = na.omit(data_ungu_two), center = TRUE, scale = TRUE, scannf = FALSE, nf = 2)
pca_ungu_three_sum = dudi.pca(df = na.omit(data_ungu_three), center = TRUE, scale = TRUE, scannf = FALSE, nf = 2)

#view summary of pca result
summary(pca_ungu_one_sum)
summary(pca_ungu_two_sum)
summary(pca_ungu_three_sum)

# Extract the loadings (contributions of each variable to each PC)
loadings_one = pca_ungu_one_sum$c1
loadings_two = pca_ungu_two_sum$c1
loadings_three = pca_ungu_three_sum$c1


#Print the loadings, THIS IS THE OUTPUT YOU WANT
# how to interpret maybe: CS1 is the contribution of each variable to axis 1 , cs2 is for axis 2
# the magnitude of these scores tells how important that variable is in shaping the axis
# the sign tells the relationship, positive -> high pc1 scores, negative -> low pc1 scores
# across the axis : big psotive on pc1 and big neg on pc2 it pulls data in opposite direction on those axis
print(loadings_one) 
print(loadings_two)
print(loadings_three)

#creating niceOverPLot with the pca subsets
niceOverPlot(pca_ungu_one,n1=307, bw = 2, b = 14)
niceOverPlot(pca_ungu_two,n1=307, bw = 2, b = 14)
niceOverPlot(pca_ungu_three,n1=307, bw = 2, b = 14)



