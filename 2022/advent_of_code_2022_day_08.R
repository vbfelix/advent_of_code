# remotes::install_github("dgrtwo/adventdrob")
# setting -----------------------------------------------------------------

library(tidyverse)

input <- adventdrob::advent_input(day = 8,year = 2022,parse = TRUE) 

# part 1 - code ------------------------------------------------------------

count_visible_trees <-
  function(grid){
    
    n_col <- ncol(grid)
    
    n_row <- nrow(grid)
    
    edge_trees <- 2*n_col+2*n_row - 4
    
    visible_trees <- 0
    
    for(i in 2:(n_row-1) ){
      
      for(j in 2:(n_row-1)){
        
        if(
          all(grid[i,j] > grid[i,(j+1):n_col])|
          all(grid[i,j] > grid[i,1:(j-1)])|
          all(grid[i,j] > grid[(i+1):n_row,j])|
          all(grid[i,j] > grid[1:(i-1),j])
        ){
          visible_trees <- visible_trees + 1
        }
      }
    }
    visible_trees <- visible_trees + edge_trees
    return(visible_trees) 
  }

# part 1 - test ------------------------------------------------------------------

test_part1 <- c(30373L, 25512L, 65332L, 33549L, 35390L)

test_part1 <- 
  strsplit(as.character(test_part1),split = "") %>% 
  unlist() %>% 
  as.numeric() %>% 
  matrix(ncol = 5,byrow = TRUE)

count_visible_trees(test_part1)

# part 1 - input ------------------------------------------------------------------

grid <-
  input$x %>% 
  as.character() %>% 
  strsplit(split = "") %>% 
  unlist() %>% 
  as.numeric() %>% 
  matrix(ncol = 99,byrow = TRUE)


count_visible_trees(grid)

# part 2 - code -----------------------------------------------------------
#adapted from ucla_posc
tree_house <-
  function(grid){
    
    n_col <- ncol(grid)
    
    n_row <- nrow(grid)
    
    tree_house <- matrix(data = 0L, nrow = n_row, ncol = n_col)

    for(i in 2:(n_row-1) ){
      
      for(j in 2:(n_col-1)){

        current_tree = grid[i, j]
        

        tree_house[i, j] = list(
          #up
          grid[(i-1):1, j],
          #down
          grid[(i+1):n_row, j],
          #right
          grid[i, (j-1):1],
          #left
          grid[i, (j+1):n_col]
        ) %>% map_int(function(x) { 
          num_greater = which(x >= current_tree)
          if(!length(num_greater)) length(x) else num_greater[1]
        }) %>%
          prod()
        
      }
    }
    
    max(tree_house)
  }

# part 2 - test ------------------------------------------------------------------

tree_house(test_part1)

# part 2 - input ------------------------------------------------------------------


tree_house(grid) 
  
