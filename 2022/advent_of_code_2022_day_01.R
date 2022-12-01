# setting -----------------------------------------------------------------

library(dplyr)
# remotes::install_github("dgrtwo/adventdrob")
df <- adventdrob::advent_input(day = 1,year = 2022,parse = TRUE)


# part 1 -------------------------------------------------------------------

df %>% 
  mutate(elf = cumsum(is.na(x))) %>%
  group_by(elf) %>% 
  summarise(total_calories = sum(x,na.rm = TRUE)) %>% 
  slice_max(total_calories)

# part 2 -------------------------------------------------------------------

df %>% 
  mutate(elf = cumsum(is.na(x))) %>%
  group_by(elf) %>% 
  summarise(total_calories = sum(x,na.rm = TRUE)) %>% 
  ungroup() %>% 
  slice_max(total_calories,n = 3) %>% 
  summarise(top3_total_calories = sum(total_calories,na.rm = TRUE))
