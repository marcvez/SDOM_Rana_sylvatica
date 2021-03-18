

#---------------------Backwards model-----------------------

# In this model you either invest in either or you die, it's a simpler model. 
# To do this, we will use a matrix where the x-axis contains the different units 
# of energy invested in growth, and the y-axis contains the units of energy 
# invested in performance. The content of each cell will be the fitness value 
# of that combination of traits. 
# The function should see how fitness increases with respect to the previous 
# state by deciding whether to invest in performance (by moving down one cell 
# in the matrix) or in growth (by moving one cell to the right). It will then 
# calculate the fitness increase of the two decisions and choose the best option.

energy_growth <- seq(from = 0, to = 30, length.out = 31)
# Number of food units that can be invested in growth

energy_performance <- seq(from = 0, to = 30, length.out = 31)
# Number of food units that can be invested in performance

prob_predation <- seq(from = 0.2, to = 0.01, length.out = 31)
# Probability of being predated

prob_find_food <- seq(from = 0.3, to = 0.8, length.out = 31)


sigmoid_growth = function(energy_growth) {
  1 / (1 + exp(0.1*(-energy_growth+15)))
}



sigmoid_performance = function(energy_performance) {
  1 / (1 + exp(0.15*(-energy_growth-10)))
}

(1-prob_predation) * prob_find_food + prob_predation * 0 + 

fitness_matrix <- sigmoid_performance(energy_performance) %*% t(sigmoid_growth(energy_growth))
# Columns --> Different fitness values depending on growth
# Rows --> Different fitness values depending on performance

fitness_matrix <- as.data.frame(fitness_matrix)


plot(energy_growth, (1 - prob_predation)*0.5, type = "l")
lines(energy_growth, (sigmoid_growth(energy_growth)), type = "l")

x <- seq(1,30,1)

a <- 0.4^((0.1*x)+1) # Mortality
b <- 12^((0.07*x)-2) # Size


plot(x, b, type = "l")
lines(x, a, type = "l")

Reward_Matrix <- data.frame(matrix(nrow = 30, ncol = 30))

for (i in 1:30){ 
  z <- (1-a) + b[i]
  rRewardMatrix[i,] <- z
  # Columns --> Mortality
  # Rows --> Size
  
}

Reward_Matrix

maxc <- 30
maxt <- 20
for (t in maxt:1) {
  
  Reward_P <- matrix(nrow=maxc+1, ncol=maxt)
  Reward_G <- matrix(nrow=maxc+1, ncol=maxt)
  for (i in 2:maxc) {
    Reward_P[i,t] <-  Reward_Matrix[i,t+1] P_eat_same[i] * Reward[i,t+1] +
      P_eat_up[i] * Reward[i+1,t+1] + P_eat_dead[i] * 0
    Reward_G[i,t] <- P_rest_same * Reward[i,t+1] +
      P_rest_down * Reward[i-1,t+1]
  }
}
  
