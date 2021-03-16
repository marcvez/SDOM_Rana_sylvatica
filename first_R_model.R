# SDO MODEL

#----------------Initial parameters-------------------

growth <- 0 # Initial growth value

performance <- 0 # Initial performance value

energy_reserve <- 0 # Where food units go before being invested

metabolic_rate <- 1 
# Amount of food units that every time step is used on self-maintenance

prob_1_unit <- seq(from = 0.5, to = 0.2, length.out = 31) 
# Probability of finding 1 unit of food
prob_1_unit_2.0 <- prob_1_unit + prob_predation #For backwards iteration

prob_2_unit <-seq(from = 0.4, to = 0.79, length.out = 31)
# Probability of finding 2 unit of food

prob_predation <- seq(from = 0.1, to = 0.01, length.out = 31)
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
round_fitness <- round(sigmoid(energy_growth), digits = 3)

fitness_values <- cbind(energy_growth, round_fitness)
fitness_values_df <- as.data.frame(fitness_values)
names(fitness_values_df)[2] <- "Fitness"
# Sigmoid function + Data frame with all its values



#------------------Graphs------------------------

library(ggplot2)
library(dplyr)
library(viridis)



# Probability graph

Probability_plot <- ggplot(data = matrix_food_predation_df, aes(x = energy_performance, group = 1)) +
  geom_line(aes(y = prob_1_unit, color = "1 food unit")) +
  geom_line(aes(y = prob_2_unit + prob_1_unit, color = "2 food units")) +
  geom_line(aes(y = prob_predation + prob_2_unit + prob_1_unit, color = "Predation")) +
  xlab('Energy invested in performance') +
  ylab('Probability') +
  scale_colour_discrete("Legend", labels = c("1 food unit", "2 food units", "Predation")) +
  geom_ribbon(aes(ymax = 1, ymin = prob_2_unit + prob_1_unit), fill = "blue", alpha = 0.2) +
  geom_ribbon(aes(ymax = prob_2_unit + prob_1_unit, ymin = prob_1_unit), fill = "green", alpha = 0.2) +
  geom_ribbon(aes(ymax = prob_1_unit, ymin = 0), fill="red", alpha = 0.2)

Probability_plot # Without stacked matrix

Probability_plot_2 <- ggplot(data = matrix_food_predation_sum_df, aes(x = energy_performance, group = 1)) +
  geom_line(aes(y = prob_1_unit, color = "1 food unit")) +
  geom_line(aes(y = prob_2_unit, color = "2 food unit")) +
  geom_line(aes(y = prob_predation, color="Predation")) +
  xlab('Energy invested in performance') +
  ylab('Probability') +
  geom_ribbon(aes(ymax = 1, ymin = prob_2_unit), fill = "#440154FF", alpha = 0.5) +
  geom_ribbon(aes(ymax = prob_2_unit, ymin = prob_1_unit), fill = "#21908CFF", alpha = 0.5) +
  geom_ribbon(aes(ymax = prob_1_unit, ymin = 0), fill="#FDE725FF", alpha = 0.5) +
  scale_color_manual(name = "Legend", values=c("#FDE725FF", "#21908CFF", "#440154FF"))

Probability_plot_2 # With stacked matrix

viridis(3) # This line gives the color code in the viridis scale with 3 factors

viridis(option = "inferno", 3)

# Fitness graph

Fitness_plot <- ggplot(data = fitness_values_df, aes(x = energy_growth)) +
  geom_line(aes(energy_growth, Fitness), data = fitness_values_df, size = 0.6) +
  xlab("Energy invested in growth") + ylab("Fitness")

Fitness_plot



#-----------------Loop---------------------

# Here we can see the evolution of the loop code

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



time <- 0
while (time <= 10){
  Random_num <- Random_1(x)
  # We fix a random number, on which the if loop is going to work
  
  Random_num
  # Fixed random number
  
  if(Random_num < prob_1_unit[energy_reserve + 1]) {
    # It is energy_reserve + 1 because we are selecting row "0" + 1 (There is no row 0)
    # and row 1 corresponds to energy 0.
    
    energy_reserve <- energy_reserve + 1 - metabolic_rate
    print("I've found 1 piece of food!")
  } else if(Random_num > prob_1_unit[energy_reserve + 1] & Random_num <= (prob_1_unit[energy_reserve + 1] + prob_2_unit[energy_reserve + 1])) {
    energy_reserve <- energy_reserve + 2 - metabolic_rate
    # Here the energy reserve incresases by 1, and we want to select row "1" + 1, corresponding
    # to row 2, energy 1
    
    print("I've found 2 pieces of food!! WOW!")
  } else {
    energy_reserve <- 0
    print("I'm dead :(")
  }
  
  time <- time + 1
}

# The same as before, but changing the state at every time step


time <- 0
while (time < 20){
  Random_num <- Random_1(x)
  # We fix a random number, on which the if loop is going to work
  
  Random_num
  # Fixed random number
  
  if(Random_num < prob_1_unit[energy_reserve + 1]) {
    energy_reserve <- energy_reserve + 1 - metabolic_rate
    print("I've found 1 piece of food!")
  } else if(Random_num > prob_1_unit[energy_reserve + 1] & Random_num <= (prob_1_unit[energy_reserve + 1] + prob_2_unit[energy_reserve + 1])) {
    energy_reserve <- energy_reserve + 2 - metabolic_rate
    print("I've found 2 pieces of food!! WOW!")
  } else {
    energy_reserve <- 0
    print("I'm dead :(")
    break
  }
  
  time <- time + 1
}

# The same, but exiting the loop when the tadpole dies
# There is too much mortality :S --> Changed probabilities 
# from: Start --> 0.4, 0.4, 0.2 to 0.5, 0.4, 0.1 
# and End --> 0.2, 0.75, 0.05 to 0.2, 0.79, 0.01



result <- data.frame(matrix(nrow = 20, ncol = 2))
colnames(result) <- c("iteration", "energy_reserve")

for (i in 1:100) {
  time <- 0
  energy_reserve <- 0 # Thais was important and I missed it!
  while (time < 20){
    Random_num <- Random_1(x)
    # We fix a random number, on which the if loop is going to work
    
    Random_num
    # Fixed random number
    
    if(Random_num < prob_1_unit[energy_reserve + 1]) {
      energy_reserve <- energy_reserve + 1 - metabolic_rate
      print("I've found 1 piece of food!")
      result[i, 1] <- i
      result[i, 2] <- energy_reserve
      
    } else if(Random_num > prob_1_unit[energy_reserve + 1] & Random_num <= (prob_1_unit[energy_reserve + 1] + prob_2_unit[energy_reserve + 1]) & energy_reserve == 30) {
      energy_reserve <- energy_reserve + 1 - metabolic_rate
      print("I've found 2 pieces of food, but my energy reserve can store up to 30 units")
      result[i, 1] <- i
      result[i, 2] <- energy_reserve
      #Here we solve the problem that I had when I used too many time steps. It cannot surpass 30 energy_reserve units.
      
    } else if(Random_num > prob_1_unit[energy_reserve + 1] & Random_num <= (prob_1_unit[energy_reserve + 1] + prob_2_unit[energy_reserve + 1])) {
      energy_reserve <- energy_reserve + 2 - metabolic_rate
      print("I've found 2 pieces of food!! WOW!")
      result[i, 1] <- i
      result[i, 2] <- energy_reserve
    
      
    } else {
      energy_reserve <- 0
      print("I'm dead :(")
      result[i, 1] <- i
      result[i, 2] <- energy_reserve
      break
    }
    
    time <- time + 1
  }
}
result

sum(result$energy_reserve > 0) 
# sum(result$energy_reserve == 30)

Survival <- ggplot(data = result, aes(x = iteration, y = energy_reserve)) +
  geom_point()
Survival 
# Loop with for function, saving the results in a matrix and in a plot


