## Date: 07/25/2022
## Author: Bill Chew
## Purpose: load packages, setup functions


library(tidyverse)

dice_roll <- function(){
  sample(1:6, 1, FALSE)
}

number_combos <- function(int_vector){
  
  if (length(int_vector) > 0) {
    out_meta_list <- list()
    for (k in 1:length(int_vector)) {
      
      all_combn <- combn(int_vector, k)
      
      combo_list <- all_combn %>% as.data.frame() %>% as.list()
      
      int_meta_list <- list("name" = combo_list)
      names(int_meta_list) <- paste0(k, "tiles")
      out_meta_list <- c(out_meta_list, int_meta_list)
      
    }
  }else{
    return()
  }
    
  out_meta_list
}

tile_moves <- function(all_combos, sum_needed){
  
  combo_sums <- lapply(all_combos, sapply, sum)
  combo_ranges <- lapply(all_combos, sapply, function(x) diff(range(x)))
  status <- FALSE
  k <- 1
  
  while(status == FALSE){
    if (k > length(all_combos)) {
      status <- "FAIL"
    } else {
      
      possible_move <- combo_sums[[k]][which(combo_sums[[k]] == sum_needed)]
        
      if(length(possible_move) > 0){
        max_range <- names(which.max(combo_ranges[[k]][names(possible_move)]))
        make_move <- all_combos[[k]][max_range]
        status <- TRUE
      }
    
    }

    k <- k + 1
  }
  
  if(status == TRUE){
    return(unlist(make_move))
  }else{
    return(NULL)
  }
}
