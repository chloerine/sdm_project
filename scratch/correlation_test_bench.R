##Test bench for checking that variables are not highly correlated

source("scratch/pearson_correlation_test.R")
message("Loading Correlation Test...")

max_cor <- 0.7
#function that tells if a subset of biolcim variables has a high correlation
high_correlation <- function(subset,show_rslt) {#subset: list with bioclim variables, show_rslt: boolean value that shows which pairs failed or passed
  subset_pairs <- combn(subset, 2, simplify = FALSE)  # list of all 2-element pairs
  
  for (pair in subset_pairs) {
    cor_val <- p_cor_abs[pair[1], pair[2]]
    if (cor_val > max_cor) {
      if(show_rslt == TRUE){
        message(sprintf("FAIL: the pair (%s, %s) has correlation %.2f (above %.2f)",pair[1], pair[2], cor_val, max_cor))
      }
      return(TRUE)  # Found a bad pair
    }
  }
  if(show_rslt == TRUE){
    message("PASS: There are no highly correlated pairs")
  }
  return(FALSE)  # All pairs passed
}

#Testing to make sure the function works ====
high_correlation_list <- c("bio2", "bio7", "bio8", "bio9", "bio10", "bio18", "bio19")
low_correlation_list <- c("bio2", "bio3", "bio6", "bio10", "bio15", "bio16", "bio18")

if((high_correlation(high_correlation_list,FALSE)) && (!high_correlation(low_correlation_list,FALSE))){
  message("The function works")
}else{
  message("The function does not work")
}

# Using function on subsets of length 7 and length 6 ====
largest_subset_correct <- 0
for (subset in largest_subset_list) {
  #print(subset)
  if (!high_correlation(subset,FALSE)){
    largest_subset_correct <- largest_subset_correct + 1
  }
}
if (largest_subset_correct == length(largest_subset_list)){
  message(sprintf("subset length 7: %.2f/%.2f correct", largest_subset_correct, length(largest_subset_list)))
}

subset_six_correct <- 0
for (subset in subset_six) {
  #print(subset)
  if (!high_correlation(subset,FALSE)){
    subset_six_correct <- subset_six_correct + 1
  }
}
if (subset_six_correct == length(subset_six)){
  message(sprintf("subset length 6: %.2f/%.2f correct", subset_six_correct, length(subset_six)))
}

