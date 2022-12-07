# remotes::install_github("dgrtwo/adventdrob")
# setting -----------------------------------------------------------------

library(tidyverse)

input <- adventdrob::advent_input(day = 7,year = 2022,parse = TRUE) 

# part1 - code ------------------------------------------------------------

#adapted from jrosell and drob
get_cd <- function(path,dir = NA){
  if (any(is.na(dir))) return(path)
  if (any(dir == "..")) return(head(path, -1))
  return(c(path, paste0(tail(path, 1), "/", dir)))
}

df <-
input %>% 
  extract(x,"cd_dir","cd (.*)",remove = FALSE) %>% 
  mutate(path = c(accumulate(cd_dir,get_cd))) %>% 
  unnest(path) %>%
  filter(str_detect(x,"\\d")) %>% 
  separate(x,c("size","file"),sep = " ", convert = TRUE) %>% 
  group_by(path) %>% 
  summarise(size = sum(size))

# part 1 - test ------------------------------------------------------------------

# part 1 - input ------------------------------------------------------------------
df %>% 
  filter(size < 100000) %>% 
  summarise(sum(size))
  
# part 2 - test ------------------------------------------------------------------

# part 2 - input ------------------------------------------------------------------

total_disk <- 70000000
required_to_update <- 30000000
used_disk <- df %>% slice(1) %>% pull(size)

condition <- required_to_update - (total_disk - used_disk)

df %>% 
  filter(size >= condition) %>% 
  slice_max(order_by = -size)
