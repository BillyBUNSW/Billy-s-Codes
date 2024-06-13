library(markovchain)
library(tidyr)
library('tidyverse')
library(readr)
library(dplyr)
library(castor)
library(ggplot2)
library(gridExtra)
library(expm)
library(jrvFinance)
library('gbm')
library(caret)
library(car)
library(neuralnet)
library(pROC)
library(class)

#m_matches <- read.csv("atp_matches.csv")
m_matches_log <- read.csv("atp_matches_log.csv")
#m_players <- read.csv("atp_players.csv")
#m_rankings <- read.csv("atp_rankings.csv")

#w_matches <- read.csv("wta_matches.csv")
m_matches_log <- read.csv("wta_matches_log.csv")
#w_players <- read.csv("wta_players.csv")
#w_rankings <- read.csv("wta_rankings.csv")

## RUN ELO CODE

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

#indices <- sample(1:nrow(m_matches_log), size = floor(0.7 * nrow(m_matches_log)), replace = FALSE)

#train_data <- m_matches_log[indices, ]
#test_data <- m_matches_log[-indices, ]

# MENS

train_data <- m_matches_log[m_matches_log$tourney_date <= "2022-05-01",]
test_data <- m_matches_log[m_matches_log$tourney_date > "2022-05-01",]
model_train <- glm(player1_winner ~ age_difference + rank_difference + different_hand + height_difference + avg_ace_difference + point_difference + avg_fault_difference + save_ratio_diff + avg_break_difference + elo_difference + avg_difference + elo_player1_winner, data = train_data, family = binomial)
summary(model_train) #16228
train_predict <- predict(model_train, newdata = train_data, type = "response")
confusionMatrix(data = as.factor(ifelse(as.numeric(train_predict > 0.5),TRUE,FALSE)), reference = as.factor(train_data$player1_winner))
test_predict <- predict(model_train, newdata = test_data, type = "response")
confusionMatrix(data = as.factor(ifelse(as.numeric(test_predict > 0.5),TRUE,FALSE)), reference = as.factor(test_data$player1_winner)) #72%

model_train <- glm(player1_winner ~ age_difference + rank_difference +  height_difference + avg_ace_difference + point_difference + avg_fault_difference + save_ratio_diff + avg_break_difference + elo_difference + avg_difference + elo_player1_winner, data = train_data, family = binomial)
summary(model_train) #16226
train_predict <- predict(model_train, newdata = train_data, type = "response")
confusionMatrix(data = as.factor(ifelse(as.numeric(train_predict > 0.5),TRUE,FALSE)), reference = as.factor(train_data$player1_winner))
test_predict <- predict(model_train, newdata = test_data, type = "response")
confusionMatrix(data = as.factor(ifelse(as.numeric(test_predict > 0.5),TRUE,FALSE)), reference = as.factor(test_data$player1_winner)) #72.2%

model_train <- glm(player1_winner ~ age_difference + rank_difference + point_difference + avg_fault_difference + elo_difference + avg_difference + elo_player1_winner, data = train_data, family = binomial)
summary(model_train) #16271
train_predict <- predict(model_train, newdata = train_data, type = "response")
confusionMatrix(data = as.factor(ifelse(as.numeric(train_predict > 0.5),TRUE,FALSE)), reference = as.factor(train_data$player1_winner))
test_predict <- predict(model_train, newdata = test_data, type = "response")
confusionMatrix(data = as.factor(ifelse(as.numeric(test_predict > 0.5),TRUE,FALSE)), reference = as.factor(test_data$player1_winner)) #72.6%


model_train <- glm(player1_winner ~ age_difference + rank_difference +  elo_difference + avg_difference, data = train_data, family = binomial)
summary(model_train) #16268
train_predict <- predict(model_train, newdata = train_data, type = "response")
confusionMatrix(data = as.factor(ifelse(as.numeric(train_predict > 0.5),TRUE,FALSE)), reference = as.factor(train_data$player1_winner))
test_predict <- predict(model_train, newdata = test_data, type = "response")
confusionMatrix(data = as.factor(ifelse(as.numeric(test_predict > 0.5),TRUE,FALSE)), reference = as.factor(test_data$player1_winner)) #72.88%
roc_curve <- roc(test_data$player1_winner, test_predict)
plot(roc_curve)
auc(roc_curve)

#WOMENS

model_train <- glm(player1_winner ~ rank_difference + point_difference + elo_difference + avg_difference, data = train_data, family = binomial)
summary(model_train) #16228
train_predict <- predict(model_train, newdata = train_data, type = "response")
confusionMatrix(data = as.factor(ifelse(as.numeric(train_predict > 0.5),TRUE,FALSE)), reference = as.factor(train_data$player1_winner))
test_predict <- predict(model_train, newdata = test_data, type = "response")
confusionMatrix(data = as.factor(ifelse(as.numeric(test_predict > 0.5),TRUE,FALSE)), reference = as.factor(test_data$player1_winner)) #72%
roc_curve <- roc(test_data$player1_winner, test_predict)
plot(roc_curve)
auc(roc_curve)




train_grass_data <- m_matches_log[m_matches_log$surface == "Grass" & m_matches_log$tourney_date <= "2022-05-01",]
test_grass_data <- m_matches_log[m_matches_log$surface == "Grass" & m_matches_log$tourney_date > "2022-05-01",]
grass_train <- glm(player1_winner ~ age_difference + rank_difference + different_hand + height_difference + avg_ace_difference + point_difference + avg_fault_difference + save_ratio_diff + avg_break_difference + elo_difference + avg_grass_difference + elo_player1_winner, data = train_grass_data, family = binomial)
summary(grass_train) #2088.9
grass_train_predict <- predict(grass_train, newdata = train_grass_data, type = "response")
confusionMatrix(data = as.factor(ifelse(as.numeric(grass_train_predict > 0.5),TRUE,FALSE)), reference = as.factor(train_grass_data$player1_winner))
grass_test_predict <- predict(grass_train, newdata = test_grass_data, type = "response")
confusionMatrix(data = as.factor(ifelse(as.numeric(grass_test_predict > 0.5),TRUE,FALSE)), reference = as.factor(test_grass_data$player1_winner)) #79.68

grass_train <- glm(player1_winner ~ age_difference + rank_difference + height_difference + avg_ace_difference + point_difference + avg_fault_difference + save_ratio_diff + avg_break_difference + elo_difference + avg_grass_difference + elo_player1_winner, data = train_grass_data, family = binomial)
summary(grass_train) #2086.9
grass_train_predict <- predict(grass_train, newdata = train_grass_data, type = "response")
confusionMatrix(data = as.factor(ifelse(as.numeric(grass_train_predict > 0.5),TRUE,FALSE)), reference = as.factor(train_grass_data$player1_winner))
grass_test_predict <- predict(grass_train, newdata = test_grass_data, type = "response")
confusionMatrix(data = as.factor(ifelse(as.numeric(grass_test_predict > 0.5),TRUE,FALSE)), reference = as.factor(test_grass_data$player1_winner)) #79.68

grass_train <- glm(player1_winner ~  rank_difference + height_difference + point_difference + avg_break_difference + avg_grass_difference + elo_player1_winner, data = train_grass_data, family = binomial)
summary(grass_train) #2078.2
grass_train_predict <- predict(grass_train, newdata = train_grass_data, type = "response")
confusionMatrix(data = as.factor(ifelse(as.numeric(grass_train_predict > 0.5),TRUE,FALSE)), reference = as.factor(train_grass_data$player1_winner))
grass_test_predict <- predict(grass_train, newdata = test_grass_data, type = "response")
confusionMatrix(data = as.factor(ifelse(as.numeric(grass_test_predict > 0.5),TRUE,FALSE)), reference = as.factor(test_grass_data$player1_winner)) #78.71

grass_train <- glm(player1_winner ~  rank_difference + height_difference + avg_break_difference + avg_grass_difference + elo_player1_winner, data = train_grass_data, family = binomial)
summary(grass_train) #2082.2
grass_train_predict <- predict(grass_train, newdata = train_grass_data, type = "response")
confusionMatrix(data = as.factor(ifelse(as.numeric(grass_train_predict > 0.5),TRUE,FALSE)), reference = as.factor(train_grass_data$player1_winner))
grass_test_predict <- predict(grass_train, newdata = test_grass_data, type = "response")
confusionMatrix(data = as.factor(ifelse(as.numeric(grass_test_predict > 0.5),TRUE,FALSE)), reference = as.factor(test_grass_data$player1_winner)) #79.03

grass_train <- glm(player1_winner ~  rank_difference + avg_break_difference + avg_grass_difference + elo_player1_winner, data = train_grass_data, family = binomial)
summary(grass_train) #2086.8
grass_train_predict <- predict(grass_train, newdata = train_grass_data, type = "response")
confusionMatrix(data = as.factor(ifelse(as.numeric(grass_train_predict > 0.5),TRUE,FALSE)), reference = as.factor(train_grass_data$player1_winner))
grass_test_predict <- predict(grass_train, newdata = test_grass_data, type = "response")
confusionMatrix(data = as.factor(ifelse(as.numeric(grass_test_predict > 0.5),TRUE,FALSE)), reference = as.factor(test_grass_data$player1_winner)) #79.01

grass_train <- glm(player1_winner ~  rank_difference + avg_grass_difference + elo_player1_winner, data = train_grass_data, family = binomial)
summary(grass_train) #207.7
grass_train_predict <- predict(grass_train, newdata = train_grass_data, type = "response")
confusionMatrix(data = as.factor(ifelse(as.numeric(grass_train_predict > 0.5),TRUE,FALSE)), reference = as.factor(train_grass_data$player1_winner))
grass_test_predict <- predict(grass_train, newdata = test_grass_data, type = "response")
confusionMatrix(data = as.factor(ifelse(as.numeric(grass_test_predict > 0.5),TRUE,FALSE)), reference = as.factor(test_grass_data$player1_winner)) #78.09
roc_curve <- roc(test_grass_data$player1_winner, grass_test_predict)
plot(roc_curve)
auc(roc_curve)


vif(grass_train)
#wimbledon_m <- read.csv("Wimbldon_Males.csv")

wimbledon_m <- read.csv("Wimbledon_Females.csv")

#colnames(wimbledon_m)[colnames(wimbledon_m) == "Player.ID"] <- "ID"
colnames(wimbledon_m)[colnames(wimbledon_m) == "Player_id"] <- "ID"
colnames(m_matches_log)[colnames(m_matches_log) == "player1_id"] <- "ID"
w_ages <- m_matches_log %>% group_by(ID) %>% summarise(current_age = max(player1_age))
w_ages <- as.data.frame(w_ages)
wimbledon_m_new <- merge(wimbledon_m, w_ages, by = "ID", all = TRUE)
#wimbledon_m_new <- wimbledon_m_new[complete.cases(wimbledon_m_new$atp_plauers),]
wimbledon_m_new <- wimbledon_m_new[complete.cases(wimbledon_m_new$Grand.Slam),]
#wimbledon_m_new <- wimbledon_m_new[order(wimbledon_m_new$atp_plauers),]
wimbledon_m_new <- wimbledon_m_new[order(wimbledon_m_new$Full.Name),]
nrow(wimbledon_m_new)

elo_by_player <- summaryPlayers() %>% select(name, ranking)
#colnames(elo_by_player)[colnames(elo_by_player) == "name"] <- "Name"
colnames(elo_by_player)[colnames(elo_by_player) == "name"] <- "Full.Name"
#wimbledon_m_new <- merge(wimbledon_m_new, elo_by_player, by = "Name", all = TRUE)
wimbledon_m_new <- merge(wimbledon_m_new, elo_by_player, by = "Full.Name", all = TRUE)
#wimbledon_m_new <- wimbledon_m_new[complete.cases(wimbledon_m_new$atp_plauers),]
wimbledon_m_new <- wimbledon_m_new[complete.cases(wimbledon_m_new$Grand.Slam),]
#wimbledon_m_new <- wimbledon_m_new[order(wimbledon_m_new$atp_plauers),]
wimbledon_m_new <- wimbledon_m_new[order(wimbledon_m_new$Full.Name),]
nrow(wimbledon_m_new)

m_matches_log <- m_matches_log %>% arrange(desc(tourney_date), desc(match_num))
df1_latest <- m_matches_log %>% group_by(ID) %>% slice(1)
df1_latest <- df1_latest %>% select(ID, player1_winner_avg_grass, player1_winner_avg, player1_rank, player1_ht)
df1_latest <- as.data.frame(df1_latest)
wimbledon_m_new  <- merge(wimbledon_m_new, df1_latest, by = "ID", all = TRUE)
#wimbledon_m_new <- wimbledon_m_new[order(wimbledon_m_new$atp_plauers),]
wimbledon_m_new <- wimbledon_m_new[complete.cases(wimbledon_m_new$Grand.Slam),]
#wimbledon_m_new <- wimbledon_m_new[complete.cases(wimbledon_m_new$atp_plauers),]
nrow(wimbledon_m_new)

write.csv(wimbledon_m_new, "output_w_f_new.csv", row.names=FALSE)
#write.csv(wimbledon_m_new, "output_m.csv", row.names=FALSE)







#NN backpropagation


train_data <- m_matches_log[m_matches_log$tourney_date <= "2022-05-01",]
gbm <- gbm(player1_winner ~ age_difference + rank_difference + different_hand + height_difference + avg_ace_difference + point_difference + avg_fault_difference + save_ratio_diff + avg_break_difference + elo_difference + avg_difference + elo_player1_winner, data = train_data, n.trees = 1000)
summary(gbm)

test_data <- m_matches_log[m_matches_log$tourney_date > "2022-05-01",]
#indices <- sample(1:nrow(m_matches_log), size = floor(0.7 * nrow(m_matches_log)), replace = FALSE)

#train_data <- m_matches_log[indices, ]
#test_data <- m_matches_log[-indices, ]

#train_data <- select(train_data, c("player1_winner", "rank_difference", "elo_difference", "avg_difference", "age_difference", "point_difference","avg_ace_difference", "height_difference", "avg_fault_difference", "elo_player1_winner", "avg_break_difference"))
#test_data <- select(test_data, c("player1_winner", "rank_difference", "elo_difference", "avg_difference", "age_difference", "point_difference", "avg_ace_difference", "height_difference", "avg_fault_difference", "elo_player1_winner", "avg_break_difference"))
train_data <- select(train_data, c("player1_winner", "rank_difference", "elo_difference", "avg_difference", "age_difference", "elo_player1_winner"))
test_data <- select(test_data, c("player1_winner", "rank_difference", "elo_difference", "avg_difference", "age_difference", "elo_player1_winner"))
train_data <- train_data %>% na.omit()
test_data <- test_data %>% na.omit()
model_formula <- player1_winner ~ rank_difference + elo_difference + avg_difference + age_difference
#model_formula <- player1_winner ~ rank_difference + elo_difference + avg_difference + elo_player1_winner + height_difference
neural_network <- neuralnet(model_formula, train_data, hidden = 2, stepmax=1e+05)
plot(neural_network)
predictions <- compute(neural_network, test_data[, -1])
predicted_classes <- predictions$net.result > 0.5  
accuracy <- mean(predicted_classes == test_data$player1_winner)
roc_curve <- roc(test_data$player1_winner, predictions$net.result)
auc(roc_curve)
plot(roc_curve)


 gbm_grass <- gbm(player1_winner ~ age_difference + rank_difference + different_hand + height_difference + avg_ace_difference + point_difference + avg_fault_difference + save_ratio_diff + avg_break_difference + elo_difference + avg_grass_difference + elo_player1_winner, data = m_matches_log[m_matches_log$surface == "Grass",], n.trees = 500)
summary(gbm_grass)

train_data <- m_matches_log[m_matches_log$tourney_date <= "2022-05-01" & m_matches_log$surface == "Grass",]
test_data <- m_matches_log[m_matches_log$tourney_date > "2022-05-01" & m_matches_log$surface == "Grass",]
#train_data <- select(train_data, c("player1_winner", "age_difference", "rank_difference", "point_difference", "elo_difference", "avg_difference", "avg_ace_difference", "avg_fault_difference"))
#test_data <- select(test_data, c("player1_winner", "age_difference", "rank_difference", "point_difference", "elo_difference", "avg_difference", "avg_ace_difference", "avg_fault_difference"))
train_data <- select(train_data, c("player1_winner",  "avg_ace_difference", "elo_difference", "avg_grass_difference"))
test_data <- select(test_data, c("player1_winner",  "avg_ace_difference", "elo_difference", "avg_grass_difference"))
train_data <- train_data %>% na.omit()
test_data <- test_data %>% na.omit()
model_formula <- player1_winner ~ avg_ace_difference + elo_difference + avg_grass_difference
neural_network <- neuralnet(model_formula, train_data, hidden = 3, stepmax=1e+05)
predictions <- compute(neural_network, test_data[, -1])
predicted_classes <- predictions$net.result > 0.5  
accuracy <- mean(predicted_classes == test_data$player1_winner)
roc_curve <- roc(test_data$player1_winner, predictions$net.result)
auc(roc_curve)
plot(roc_curve)


# KNN
train_data <- m_matches_log[m_matches_log$tourney_date <= "2022-05-01",]
test_data <- m_matches_log[m_matches_log$tourney_date > "2022-05-01",]
#train_data <- select(train_data, c("age_difference", "rank_difference", "elo_difference", "avg_difference", "player1_winner"))
#test_data <- select(test_data, c("age_difference", "rank_difference", "elo_difference", "avg_difference", "player1_winner"))
train_data <- select(train_data, c("height_difference", "rank_difference", "point_difference", "elo_difference", "avg_difference", "player1_winner"))
test_data <- select(test_data, c("height_difference", "rank_difference", "point_difference", "elo_difference", "avg_difference", "player1_winner"))
train_data <- train_data %>% na.omit()
test_data <- test_data %>% na.omit()
knn_train <- train_data[, -ncol(train_data)]
knn_test <- test_data[, -ncol(train_data)] 
train_labels <- train_data[,ncol(train_data)]
test_labels <- test_data[,ncol(train_data)]


k <- 450  # Specify the number of neighbors (k)
knn_model <- knn(train = knn_train, test = knn_test, cl = train_labels, k = k)


accuracy <- sum(knn_model == test_labels) / length(test_labels)

k_values <- c()
accuracies <- c()
k <- 1
while (k < 450) {
  knn_model <- knn(train = knn_train, test = knn_test, cl = train_labels, k = k)
  accuracy <- sum(knn_model == test_labels) / length(test_labels)
  
  # Append k and accuracy values to vectors
  k_values <- c(k_values, k)
  accuracies <- c(accuracies, accuracy)
  k <- k + 5
}
plot(k_values, accuracies, type = "b", pch = 19, xlim = c(1, 400), ylim = c(0.5, 0.8),
     xlab = "k", ylab = "Accuracy", main = "Accuracy vs. k", cex.axis = 2, cex.lab = 2)
which.max(accuracies)
accuracies[which.max(accuracies)]











# HYPOTHETICAL

library('randomForest')

target_col <- 1  # Replace with the actual column index of your target variable

train_data <- m_matches_log[m_matches_log$tourney_date <= "2022-05-01",]
train_data <- select(train_data, c("player1_winner", "rank_difference", "elo_difference", "avg_difference", "age_difference", "point_difference","avg_ace_difference", "height_difference", "avg_fault_difference"))
train_data <- train_data %>% na.omit()
# Create the control parameters for the RFE
ctrl <- rfeControl(functions = rfFuncs,  # Use random forest for feature ranking
                   method = "cv",  # Cross-validation
                   number = 5)  # Number of folds for cross-validation

# Define the formula for the target variable and features
target <- names(train_data)[target_col]
features <- names(train_data)[-target_col]
formula <- as.formula(paste(target, "~", paste(features, collapse = "+")))

target_data <- as.vector(train_data[[target]])
target_data <- as.factor(unlist(target_data))
# Convert the subset of train_data to a data frame
train_subset <- as.data.frame(train_data[, features])
train_matrix <- as.matrix(train_subset)
# Perform RFE
rfe_result <- rfe(y = target_data, x = train_matrix, formula, sizes = c(1:length(features)),
                  rfeControl = ctrl)

# Print the selected features
selected_features <- predict(rfe_result, train_data)
print(selected_features)

# Get the ranking of all features
feature_ranking <- rfe_result$variables
print(feature_ranking)









train_data <- m_matches_log
train_data <- select(train_data, c("player1_winner", "age_difference", "rank_difference", "different_hand" ,"height_difference", "avg_ace_difference", "point_difference", "avg_fault_difference", "save_ratio_diff", "avg_break_difference",  "elo_difference",  "avg_difference", "elo_player1_winner"))
train_data <- train_data %>% na.omit()
# Specify the response variable column index
response_col <- 1

# Create a control object for RFE
ctrl <- rfeControl(functions = rfFuncs, method = "cv", number = 5)

# Perform RFE
rfe_result <- rfe(x = train_data[, -response_col],y = as.factor(unlist(train_data[,response_col])),
                  sizes = c(1:ncol(train_data)-1), rfeControl = ctrl)

# Print the results
print(rfe_result)

# Plot the results
plot(rfe_result, type = c("g", "o"))

# Get the selected features
selected_features <- predictors(rfe_result)

# Subset the training data with the selected features
selected_data <- train_data[, c(selected_features, response_col)]






































#HYPOTHETICAL MATCHES

hypo <- read.csv("hypothetical_m.csv")

n_predictions <- compute(neural_network, hypo[, -1])
l_predict <- predict(model_train, newdata = hypo, type = "response")

hypo <- hypo %>% mutate(n_predict = n_predictions$net.result)
hypo <- hypo %>% mutate(l_predict = l_predict)
hypo


write.csv(hypo, "output_hypo.csv")















