# setting -----------------------------------------------------------------

library(dplyr)
# remotes::install_github("dgrtwo/adventdrob")
df <- adventdrob::advent_input(day = 1,year = 2021,parse = TRUE)

# part 1 -------------------------------------------------------------------

df %>% 
  count(diff_x = sign(c(NA,diff(x)))) 

# part 2 -------------------------------------------------------------------

df %>% 
  mutate(
    window_sum = x + lead(x,n = 1) + lead(x,n = 2)
  ) %>% 
  na.omit() %>% 
  count(diff_window_sum = sign(c(NA,diff(window_sum)))) 
