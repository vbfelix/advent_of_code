# remotes::install_github("dgrtwo/adventdrob")
# setting -----------------------------------------------------------------

library(tidyverse)

input <- adventdrob::advent_input(day = 6,year = 2022,parse = TRUE) 


# part1 - code ------------------------------------------------------------

get_marker <- function(string,distinct_char){
  
  distinct_char <- distinct_char - 1
  
  string <- str_split(string,pattern = "") %>% unlist()

  duplicated <- 1
  
  aux <- 1
  
  while(duplicated > 0){
    
    duplicated <- stringi::stri_duplicated_any(string[aux:(aux+distinct_char)])
    
    if(duplicated == 0){
      
      stop(aux + distinct_char)
      
    }
    
    aux <- aux + 1
  }
  
}

# part 1 - test ------------------------------------------------------------------

get_marker("mjqjpqmgbljsphdztnvjfqwrcgsmlb",4)
get_marker("bvwbjplbgvbhsrlpgdmjqwftvncz",4)
get_marker("nppdvjthqldpwncqszvftbrmjlhg",4)
get_marker("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg",4)
get_marker("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw",4)

# part 1 - input ------------------------------------------------------------------

get_marker(input$x,distinct_char = 4)

# part 2 - test ------------------------------------------------------------------

get_marker("mjqjpqmgbljsphdztnvjfqwrcgsmlb",14)
get_marker("bvwbjplbgvbhsrlpgdmjqwftvncz",14)
get_marker("nppdvjthqldpwncqszvftbrmjlhg",14)
get_marker("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg",14)
get_marker("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw",14)

# part 2 - input ------------------------------------------------------------------

get_marker(input$x,distinct_char = 14)
