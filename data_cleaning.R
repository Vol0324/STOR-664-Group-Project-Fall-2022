total_stats = read.csv("./Data/NBA_season_stats_from_basketball_references-2017-18_total.csv")
salary = read.csv("./Data/NBA_season1718_salary_from_Kaggle.csv")

#### Remove accent marks from players in total_stats ----
require(stringi)
players_noaccent = sapply(X = total_stats$Player, FUN = stri_trans_general, id = "Latin-ASCII")
total_stats$Player.noaccent = players_noaccent

length(setdiff(salary$Player, total_stats$Player.noaccent)) ## 75
length(setdiff(total_stats$Player.noaccent, salary$Player)) ## 80
length(which(duplicated(salary$Player))) ## 38
length(which(duplicated(total_stats$Player))) ## 124

#### Remove players that appear more than once from salary ----
salary = salary[-c(which(duplicated(salary$Player))), ]

#### Remove duplicate rows in total_stats ----
require(tidyverse)
total_stats.noDuplicates =  total_stats %>% distinct(Player.noaccent, .keep_all = TRUE) 

#### Merge salary and data and drop uselesss columns
final_dataset = merge(x = total_stats.noDuplicates, y = salary, by.x = "Player.noaccent", by.y = "Player") %>%
  select(-c("Player.noaccent", "Rk", "Player.additional", "X", "Tm.x", "Tm.y")) %>%
  select("Player", "season17_18", everything()) %>%
  rename(salery = "season17_18")
