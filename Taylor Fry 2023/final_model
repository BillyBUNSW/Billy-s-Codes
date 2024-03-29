library('tidyverse')
library(caret)
library(car)
library(neuralnet)
library(pROC)

m_matches_log <- read.csv("atp_matches_log.csv")
#m_matches_log <- read.csv("wta_matches_log.csv") - WOMANS


# RUN THE ENTIRE ELO CODE

firstDate <- as.Date("01/01/1900")
m_matches_log$tourney_date <- as.Date(as.character(m_matches_log$tourney_date),format='%d/%m/%Y', origin = "01/01/1900")

m_matches_log$player1_elo <- c(matches$eloA, matches$eloB)
m_matches_log$player2_elo <- c(matches$eloB, matches$eloA)
m_matches_log <- m_matches_log %>% mutate(elo_player1_winner =  1 / (1 + 10 ^ ((player2_elo - player1_elo)/400)))
m_matches_log <- m_matches_log %>% mutate(elo_difference = player1_elo - player2_elo)


add_variables <- function(m_matches_log) {
  m_matches_log <- m_matches_log %>% mutate(age_difference = player1_age - player2_age)
  m_matches_log <- m_matches_log %>% mutate(rank_difference = player1_rank - player2_rank)
  m_matches_log <- m_matches_log %>% mutate(height_difference = player1_ht - player2_ht)
  m_matches_log <- m_matches_log %>% mutate(point_difference = player1_rank_points - player2_rank_points)
  m_matches_log <- m_matches_log %>% mutate(different_hand = ifelse(player1_hand != player2_hand, 1, 0))
  
  player1_avg_aces <- aggregate(player1_aces ~ player1_id, data = m_matches_log, FUN = mean, na.rm = TRUE)
  m_matches_log <- merge(m_matches_log, player1_avg_aces, by = "player1_id", suffixes = c("", "_avg"))
  player2_avg_aces <- aggregate(player2_aces ~ player2_id, data = m_matches_log, FUN = mean)
  m_matches_log <- merge(m_matches_log, player2_avg_aces, by = "player2_id", suffixes = c("", "_avg"))
  m_matches_log <- m_matches_log %>% mutate(avg_ace_difference = player1_aces_avg - player2_aces_avg)
  
  avg_wins <- aggregate(player1_winner ~ player1_id, data = m_matches_log, FUN = mean)
  m_matches_log <- merge(m_matches_log, avg_wins, by = "player1_id", suffixes = c("", "_avg"))
  avg_wins <- aggregate(player2_winner ~ player2_id, data = m_matches_log, FUN = mean)
  m_matches_log <- merge(m_matches_log, avg_wins, by = "player2_id", suffixes = c("", "_avg"))
  m_matches_log <- m_matches_log %>% mutate(avg_difference = player1_winner_avg - player2_winner_avg)
  
  grass_avg_wins <- aggregate(player1_winner ~ player1_id, data = m_matches_log[m_matches_log$surface == "Grass", ], FUN = mean)
  m_matches_log <- merge(m_matches_log, grass_avg_wins, by = "player1_id", suffixes = c("", "_avg_grass"), all = TRUE)
  grass_avg_wins <- aggregate(player2_winner ~ player2_id, data = m_matches_log[m_matches_log$surface == "Grass", ], FUN = mean)
  m_matches_log <- merge(m_matches_log, grass_avg_wins, by = "player2_id", suffixes = c("", "_avg_grass"), all = TRUE)
  m_matches_log <- m_matches_log %>% mutate(avg_grass_difference = player1_winner_avg_grass - player2_winner_avg_grass)
  
  
  player1_avg_doublefaults <- aggregate(player1_double_faults ~ player1_id, data = m_matches_log, FUN = mean)
  m_matches_log <- merge(m_matches_log, player1_avg_doublefaults, by = "player1_id", suffixes = c("", "_avg"))
  player2_avg_doublefaults <- aggregate(player2_double_faults ~ player2_id, data = m_matches_log, FUN = mean)
  m_matches_log <- merge(m_matches_log, player2_avg_doublefaults, by = "player2_id", suffixes = c("", "_avg"))
  m_matches_log <- m_matches_log %>% mutate(avg_fault_difference =  player1_double_faults_avg - player2_double_faults_avg)
  
  
  
  m_matches_log <- m_matches_log %>% mutate(player1_save_rat = player1_breaks_saved/player1_breaks_faced)
  m_matches_log <- m_matches_log %>% mutate(player2_save_rat = player2_breaks_saved/player2_breaks_faced)
  player1_avg_save <- aggregate(player1_save_rat ~ player1_id, data = m_matches_log, FUN = mean)
  m_matches_log <- merge(m_matches_log, player1_avg_save, by = "player1_id", suffixes = c("", "_avg"))
  player2_avg_save <- aggregate(player2_save_rat ~ player2_id, data = m_matches_log, FUN = mean)
  m_matches_log <- merge(m_matches_log, player2_avg_save, by = "player2_id", suffixes = c("", "_avg"))
  m_matches_log <- m_matches_log %>% mutate(save_ratio_diff = player1_save_rat_avg - player2_save_rat_avg)
  
  m_matches_log <- m_matches_log %>% mutate(player1_breaks = player2_breaks_faced - player2_breaks_saved)
  m_matches_log <- m_matches_log %>% mutate(player2_breaks = player1_breaks_faced - player1_breaks_saved)
  player1_avg_break <- aggregate(player1_breaks ~ player1_id, data = m_matches_log, FUN = mean)
  m_matches_log <- merge(m_matches_log, player1_avg_break, by = "player1_id", suffixes = c("", "_avg"))
  player2_avg_break <- aggregate(player2_breaks ~ player2_id, data = m_matches_log, FUN = mean)
  m_matches_log <- merge(m_matches_log, player2_avg_break, by = "player2_id", suffixes = c("", "_avg"))
  m_matches_log <- m_matches_log %>% mutate(avg_break_difference = player1_breaks_avg - player2_breaks_avg)
}

m_matches_log <- add_variables(m_matches_log)

train_data <- m_matches_log[m_matches_log$tourney_date <= "2022-05-01",]
test_data <- m_matches_log[m_matches_log$tourney_date > "2022-05-01",]
train_data <- select(train_data, c("player1_winner", "rank_difference", "elo_difference", "avg_difference", "age_difference"))
test_data <- select(test_data, c("player1_winner", "rank_difference", "elo_difference", "avg_difference", "age_difference"))
train_data <- train_data %>% na.omit()
test_data <- test_data %>% na.omit()
model_formula <- player1_winner ~ rank_difference + elo_difference + avg_difference + age_difference
neural_network <- neuralnet(model_formula, train_data, hidden = 2, stepmax=1e+05)
predictions <- compute(neural_network, test_data[, -1])
predicted_classes <- predictions$net.result > 0.5  
accuracy <- mean(predicted_classes == test_data$player1_winner)
roc_curve <- roc(test_data$player1_winner, predictions$net.result)
auc(roc_curve)
plot(roc_curve)
