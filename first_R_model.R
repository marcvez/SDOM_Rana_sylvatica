# SDO MODEL

#----------------Initial parameters-------------------

growth <- 0 # Initial growth value

performance <- 0 # Initial performance value

energy_reserve <- 0 # Where food units go before being invested

metabolic_rate <- 1 
# Amount of food units that every time step is used on self-maintenance

prob_1_unit <- seq(from = 0.4, to = 0.2, length.out = 31) 
# Probability of finding 1 unit of food

prob_2_unit <-seq(from = 0.4, to = 0.75, length.out = 31)
# Probability of finding 2 unit of food

prob_predation <- seq(from = 0.2, to = 0.05, length.out = 31)
# Probability of being predated

energy_growth <- seq(from = 0, to = 30, length.out = 31)

energy_performance <- seq(from = 0, to = 30, length.out = 31)

matrix_food_predation <- cbind(energy_performance , prob_1_unit , prob_2_unit , prob_predation)
# Matrix with the probabilities of each event for every performance value

matrix_food_predation_sum <- cbind(energy_performance , prob_1_unit , prob_2_unit + prob_1_unit, prob_predation + prob_2_unit + prob_1_unit)
# Matrix with the probabilities of each event for every performance value, packed

matrix_food_predation_df <- as.data.frame(matrix_food_predation)
# Data frame of the probabilities

matrix_food_predation_sum_df <- as.data.frame(matrix_food_predation_sum)
names(matrix_food_predation_sum_df)[3] <- "prob_2_unit"
names(matrix_food_predation_sum_df)[4] <- "prob_predation"
# Data frame of the stacked probabilities

sigmoid = function(energy_growth) {
  10 / (1 + exp(0.5*(-energy_growth+15)))
}

fitness_values <- cbind(energy_growth, sigmoid(energy_growth))
fitness_values_df <- as.data.frame(fitness_values)
names(fitness_values_df)[2] <- "Fitness"
# Sigmoid function + Data frame with all its values



#------------------Graphs------------------------

library(ggplot2)
library(dplyr)
library(viridis) # I can't make it colorblind-friendly, but I would like to


# Probability graph

Probability_plot <- ggplot(data = matrix_food_predation_df, aes(x = energy_performance, group = 1)) +
  geom_line(aes(y = prob_1_unit, color = "1 food unit")) +
  geom_line(aes(y = prob_2_unit + prob_1_unit , color = "2 food units")) +
  geom_line(aes(y = prob_predation + prob_2_unit + prob_1_unit, color="Predation")) +
  xlab('Energy invested in performance') +
  ylab('Probability') +
  scale_colour_discrete("Legend") +
  geom_ribbon(aes(ymax = 1, ymin = prob_2_unit + prob_1_unit), fill = "blue", alpha = 0.2) +
  geom_ribbon(aes(ymax = prob_2_unit + prob_1_unit, ymin = prob_1_unit), fill = "green", alpha = 0.2) +
  geom_ribbon(aes(ymax = prob_1_unit, ymin = 0), fill="red", alpha = 0.2)

Probability_plot # Without stacked matrix

Probability_plot_2 <- ggplot(data = matrix_food_predation_sum_df, aes(x = energy_performance, group = 1)) +
  geom_line(aes(y = prob_1_unit, color = "1 food unit")) +
  geom_line(aes(y = prob_2_unit, color = "2 food units")) +
  geom_line(aes(y = prob_predation, color="Predation")) +
  xlab('Energy invested in performance') +
  ylab('Probability') +
  scale_colour_discrete("Legend") +
  geom_ribbon(aes(ymax = 1, ymin = prob_2_unit), fill = "blue", alpha = 0.2) +
  geom_ribbon(aes(ymax = prob_2_unit, ymin = prob_1_unit), fill = "green", alpha = 0.2) +
  geom_ribbon(aes(ymax = prob_1_unit, ymin = 0), fill="red", alpha = 0.2)

Probability_plot_2 # With stacked matrix


# Fitness graph

Fitness_plot <- ggplot(data = fitness_values_df, aes(x = energy_growth)) +
  geom_line(aes(energy_growth, Fitness), data = fitness_values_df, size = 0.6) +
  xlab("Energy invested in growth") + ylab("Fitness")

Fitness_plot



#-----------------Loop---------------------

x <- 1

Random_1 <- function(x){
  runif(x)
}
# Function that generates a random number from 0 to 1 every time it is called

Random_num <- Random_1(x)
# We fix a random number, on which the if loop is going to work

Random_num
# Fixed random number

if(Random_num < prob_1_unit[1]) {
  energy_reserve <- energy_reserve + 1 - metabolic_rate
  "I've found 1 piece of food!"
  } else if(Random_num > prob_1_unit[1] & Random_num <= (prob_1_unit[1] + prob_2_unit[1])) {
  energy_reserve <- energy_reserve + 2 - metabolic_rate
  "I've found 2 pieces of food!! WOW!"
  } else {
  energy_reserve <- 0
  "I'm dead :("
  }
# 3 possible outputs. At the moment we are only working with 0 energy invested on performance. 


time <- 0
while (time <= 10){
  Random_num <- Random_1(x)
  # We fix a random number, on which the if loop is going to work
  
  Random_num
  # Fixed random number
  
  if(Random_num < prob_1_unit[1]) {
    energy_reserve <- energy_reserve + 1 - metabolic_rate
    print("I've found 1 piece of food!")
  } else if(Random_num > prob_1_unit[1] & Random_num <= (prob_1_unit[1] + prob_2_unit[1])) {
    energy_reserve <- energy_reserve + 2 - metabolic_rate
    print("I've found 2 pieces of food!! WOW!")
  } else {
    energy_reserve <- 0
    print("I'm dead :(")
  }
  time <- time + 1
}

# While loop for the previous if sequence. Now we should try to 
# change the performance state at each time_step
