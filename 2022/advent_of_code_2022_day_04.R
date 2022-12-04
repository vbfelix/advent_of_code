# remotes::install_github("dgrtwo/adventdrob")
# setting -----------------------------------------------------------------

library(tidyverse)

df <-
  adventdrob::advent_input(day = 4,year = 2022,parse = TRUE) %>% 
  rename(sections = x) %>% 
  separate(col = sections,into = c("elf1","elf2"),sep = ",") %>% 
  separate(col = elf1,into = c("elf1_min","elf1_max"),sep = "-") %>%
  separate(col = elf2,into = c("elf2_min","elf2_max"),sep = "-") 

# part 1 -------------------------------------------------------------------

df %>% 
  mutate(
    across(.fns = as.numeric),
    is_fully_contained = case_when(
      elf1_min <= elf2_min & elf1_max >= elf2_max ~ TRUE,
      elf2_min <= elf1_min & elf2_max >= elf1_max ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>% 
  count(is_fully_contained)
  

# part 2 ------------------------------------------------------------------

overlap <-
  function(x1,x2,y1,y2){
  length(intersect(x1:x2,y1:y2)) >= 1
}

df %>% 
  rowwise() %>% 
  mutate(
    across(.fns = as.numeric),
    overlap = overlap(elf1_min,elf1_max,elf2_min,elf2_max)
    ) %>% 
  count(overlap)
  
