# setting -----------------------------------------------------------------

library(tidyverse)
# remotes::install_github("dgrtwo/adventdrob")

df <-
  adventdrob::advent_input(day = 3,year = 2022,parse = TRUE) %>% 
  rename(rucksack = x)

# part 1 -------------------------------------------------------------------

common_item <- function(x,y){
  xsplit <- unlist(strsplit(x,""))
  ysplit <- unlist(strsplit(y,""))
  aux <- na.omit(pmatch(xsplit,ysplit))[1]
  out <- ysplit[aux]
  return(out)
}

item_priority <- function(x){
  
  if(x == str_to_upper(x)){
    out <- which(x == LETTERS) + 26
  } else{
    out <- which(x == letters)
  }
  return(out)
}

df %>% 
  rowwise() %>% 
  mutate(
    n_items = str_length(rucksack),
    compartment1 = substring(text = rucksack,first = 1,last = (n_items/2)),
    compartment2 = substring(text = rucksack,first = (n_items/2)+1,last =n_items),
    common_item = common_item(compartment1,compartment2),
    priority = item_priority(common_item)
  ) %>% 
  ungroup() %>% 
  summarise(total_priority = sum(priority,na.rm = TRUE))


# part 2 -------------------------------------------------------------------

find_bagde <- function(x,y,z){
  
  # x <- "vJrwpWtwJgWrhcsFMMfFFhFp"
  # y <- "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
  # z <- "PmmdzqPrVvPwwTWBwg"
  
  xsplit <- unlist(strsplit(x,""))
  ysplit <- unlist(strsplit(y,""))
  zsplit <- unlist(strsplit(z,""))
  
  aux_common_xy <- na.omit(pmatch(xsplit,ysplit))
  out_common_xy <- ysplit[aux_common_xy]

  aux_common_yz <- na.omit(pmatch(ysplit,zsplit))
  out_common_yz <- zsplit[aux_common_yz]
  
  out <- common_item(out_common_xy,out_common_yz)
  
  return(out)
}


df %>% 
  bind_cols(aux = rep(1:3,100),group = rep(1:100,each = 3)) %>% 
  pivot_wider(names_from = aux,values_from = rucksack,names_prefix = "rucksack") %>% 
  rowwise() %>% 
  mutate(
    item_badge = find_bagde(rucksack1,rucksack2,rucksack3),
    priority = item_priority(item_badge)
  ) %>% 
  ungroup() %>% 
  # View()
  summarise(total_priority = sum(priority,na.rm = TRUE))

