## Date: 07/25/2022
## Author: Bill Chew
## Purpose: Simulation of Shut the Box



library(parallel)
library(tidyverse)

S <- 1000000

seeds <- sample(1:.Machine$integer.max, S, replace = FALSE)

cl <- makeCluster(detectCores() - 1)
clusterEvalQ(cl, source("01_shut_the_box.R"))

simulation_box <- parLapply(cl, seeds,
                            function(x){
                              set.seed(x)
                              shut_the_box(1:9) %>% add_column(seed = x)
                            } ) %>% 
  bind_rows()

stopCluster(cl)


simulation_box %>% count(status) %>% 
  mutate(n_total = sum(n),
         pct = n / n_total)
