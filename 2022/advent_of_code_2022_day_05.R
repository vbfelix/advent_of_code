# remotes::install_github("dgrtwo/adventdrob")
# setting -----------------------------------------------------------------

library(tidyverse)

input <- adventdrob::advent_input(day = 5,year = 2022,parse = TRUE) 

moves_df <-
  input %>% 
  filter(str_detect(x,"move")) %>% 
  extract(
    col = x,
    into = c("move","from","to"),
    regex = "move (\\d+) from (\\d+) to (\\d+)",
    convert = TRUE
    )


#adapted from @drob
crates_df <-
  input %>% 
  filter(!str_detect(x,"move")) %>% 
  adventdrob::grid_tidy(x) %>% 
  filter(str_detect(value,"[A-Z]")) %>% 
  arrange(desc(row)) %>% 
  mutate(
    crate = match(col, unique(sort(col))),
    row = -(row-9)) %>% 
  select(-col) %>% 
  full_join(expand_grid(row = 1:72, crate = 1:9)) %>% 
  pivot_wider(names_from = crate,values_from = value,values_fill = "",names_prefix = "stack") %>% 
  arrange(-row) %>% 
  select(-row) %>% 
  mutate(across(.fns = ~if_else(is.na(.),"",.))) 



# test --------------------------------------------------------------------

 # crates_df <-
 #   bind_rows(
 #     data.frame(stack1 = rep("",4),stack2 = rep("",4),stack3 = rep("",4)),
 #   data.frame(
 #     stack1 = c("Z","N",""),
 #     stack2 = c("M","C","D"),
 #     stack3 = c("P","","")
 #   ) %>%
 #   mutate(across(.fns = rev)))
 # 
 # tests_df <- data.frame(
 #   move = c(1,3,2,1),
 #   from = c(2,1,2,1),
 #   to = c(1,3,1,2)
 # )

# part 1 -------------------------------------------------------------------



move_from_to <- 
  function(move,from,to,crane_mover = 9000){
    
    aux_from <- min(which(crates_df[,from] != ""))
    
    if(move > 1){
      aux_from <- aux_from:(aux_from+move-1) 
    }
    if(crane_mover == 9001){
      aux_from <- rev(aux_from)
    }
    
    aux_to <- max(which(crates_df[,to] == "")) 
    
    if(move > 1){
      aux_to <- aux_to:(aux_to-move+1) 
    }
    
    crates_df[aux_to,to] <<- crates_df[aux_from,from]
    crates_df[aux_from,from] <<- ""
    
    return(crates_df)
    
  }

# output <-
#   moves_df %>%
#   rowwise() %>%
#   mutate(crate = pmap(.l = list(move,from,to),.f = ~move_from_to(move,from,to))) 
# 
# output %>% 
#   ungroup() %>% 
#   slice(nrow(output)) %>% 
#   select(crate) %>% 
#   unnest() %>% 
#   summarise(across(.fns = ~.[min(which(. != ""))] )) 

# FWNSHLDNZ  
# part 2 ------------------------------------------------------------------


output <-
  moves_df %>%
  rowwise() %>%
  mutate(crate = pmap(.l = list(move,from,to),.f = ~move_from_to(move,from,to,crane_mover = 9001))) 

output %>% 
  ungroup() %>% 
  slice(nrow(output)) %>% 
  select(crate) %>% 
  unnest() %>% 
  summarise(across(.fns = ~.[min(which(. != ""))] )) %>% 
  unlist() %>% 
  str_squish() %>% 
  str_c(collapse = "")

# RNRGDNFQG