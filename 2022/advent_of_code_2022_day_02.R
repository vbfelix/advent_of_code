# setting -----------------------------------------------------------------

library(tidyverse)
# remotes::install_github("dgrtwo/adventdrob")

df <-
  adventdrob::advent_input(day = 2,year = 2022,parse = TRUE) %>% 
  separate(col = x,into = c("opponent","myself"),sep = " ") %>% 
  mutate(
    opponent = case_when(
      opponent == "A" ~ "Rock",
      opponent == "B" ~ "Paper",
      opponent == "C" ~ "Scissors"
    ))

# part 1 -------------------------------------------------------------------

df %>%
  mutate(
    myself = case_when(
      myself == "X" ~ "Rock",
      myself == "Y" ~ "Paper",
      myself == "Z" ~ "Scissors"
    ),
    choice_score = case_when(
      myself == "Rock" ~ 1,
      myself == "Paper" ~ 2,
      myself == "Scissors" ~ 3
    ),
    result_score = case_when(
      opponent == myself ~ 3,
      myself == "Rock" & opponent == "Scissors" ~ 6,
      myself == "Scissors" & opponent == "Paper" ~ 6,
      myself == "Paper" & opponent == "Rock" ~ 6,
      TRUE ~ 0
    ),
    total_score = choice_score + result_score
  ) %>% 
  summarise(sum(total_score))

# part 2 -------------------------------------------------------------------

df %>%
  mutate(
    myself = case_when(
      myself == "X" & opponent == "Scissors"  ~ "Paper",
      myself == "X" & opponent == "Rock"  ~ "Scissors",
      myself == "X" & opponent == "Paper"  ~ "Rock",
      #Y
      myself == "Y" ~ opponent,
      #Z
      myself == "Z" & opponent == "Scissors"  ~ "Rock",
      myself == "Z" & opponent == "Rock"  ~ "Paper",
      myself == "Z" & opponent == "Paper"  ~ "Scissors"
    ),
    choice_score = case_when(
      myself == "Rock" ~ 1,
      myself == "Paper" ~ 2,
      myself == "Scissors" ~ 3
    ),
    result_score = case_when(
      opponent == myself ~ 3,
      myself == "Rock" & opponent == "Scissors" ~ 6,
      myself == "Scissors" & opponent == "Paper" ~ 6,
      myself == "Paper" & opponent == "Rock" ~ 6,
      TRUE ~ 0
    ),
    total_score = choice_score + result_score
  ) %>% 
  summarise(sum(total_score))
