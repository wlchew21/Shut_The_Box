## Date: 07/25/2022
## Author: Bill Chew
## Purpose: Play a game of Shut the Box

source("00_setup.R")

shut_the_box <- function(tile_range){
  available_tiles <- tile_range
  status <- "Playing"
  n_moves <- 0
  
  while(status == "Playing"){
    die_1 <- dice_roll()
    die_2 <- dice_roll()
    dice_total <- die_1 + die_2
    n_moves <- n_moves + 1
    
    possible_moves <- number_combos(available_tiles)
    preferred_move <- tile_moves(possible_moves, dice_total)
    
    if(length(preferred_move) == 0){
      status <- "Lose"
    }else{
      available_tiles <- available_tiles[-which(available_tiles %in% preferred_move)]
    }
    
    if(length(available_tiles) == 0){
      status <- "Win"
    }
    
  }
  
  tibble(n_moves = n_moves, status = status, remaining = list(available_tiles))
}
