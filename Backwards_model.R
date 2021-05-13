

#------------------------- Backwards model ---------------------------------

# This script contains the backward simulation of the model. 
# Different object values will be generated, which will contain different variables.
# Performance -> How fast you move.
# Size -> Your size.
# Fitness -> Terminal fitness values.
# Condition -> Combination of performance and size, and ranks your aptitude in 
# relation to the maximum possible.
# Survival -> Probability of survival depending on your current size and performance


# From this point, the model creates a loop in which at each time step the 
# organism can decide whether to invest in Performance or Growth. 
# It compares which of the two options is better and stores the decision in an array (ForageRule). 
# The fitness values obtained from each of the decisions are also stored (array Fitness).

# Finally, a plot of each time step is made and it is seen which decision is the 
# optimal one starting from the last time step, where the terminal fitness of each size is known. 

# In this script, temperature plays an important role, because if the temperature 
# is bad, investing in growth does not bring benefits and you stay the same, 
# while if you invest in moving better, it does not matter what temperature you are at.
# A good temperature brings benefits to both decisions, and influences the rate of 
# internal development of the organisms (the higher the temperature, the more accelerated, 
# and this is measured by the Inner_time factor and the probability of skipping 
# time_step due to accelerated development).

library(plot.matrix)


Decisions <- function (prob_good_temp, prob_bad_temp, days, end_season_percentage, end_season_intensity, death_rate_day, development_rate) {
  
  time_steps <- days + ((prob_bad_temp - 0.5)*30)
  # An environment with higher temperatures is also more likely to dry out
  # earlier and to have a shorter growing season.
  
  # prob_jump <- prob_good_temp
  # The probability of skipping a time_step (equivalent to development speeding 
  # up due to temperature) depends directly on the temperature.
  
  Performance <- seq(1.0, 7.5, 0.25) 
  max_Performance <- length(Performance)
  # Performance values (How fast you move cm/s)
  
  Size <- c(0, seq(1, 5.5, 0.2))
  max_Size <- length(Size)
  # Size values. All the values that tadpoles can archive. Also, this is the only 
  # trait that is relevant for the final Fitness (The bigger, the better)
  # Size 0 is equal to being dead.
  
  Stages <- c(1:5) # 1:10
  max_Stages <- length(Stages)
  # Number of Stages that the tadpole has to go through in order to metamorphose.
  
  
  Fitness_values <- Size
  max_Fitness <- length(Fitness_values)
  Fitness_values[Fitness_values < 4] <- 0
  Fitness_values[Fitness_values >=4] <- seq(2, 3, 1/(length(Fitness_values[Fitness_values >= 4]) - 1))
  
  
  Fitness_values
  # Sizes under 4 cm don't receive Fitness benefits. This is the benefit that you
  # receive for being in each Size at the final time step.
  
  Fitness <- array(NA, dim = c((max_Size), max_Performance, max_Stages, time_steps + 1 + max_Stages))
  Fitness[, , max_Stages, time_steps + 1] <- Fitness_values
  
  
  for (j in 2:max_Performance) {
    
    Fitness[, j, max_Stages, time_steps + 1] <- Fitness[, j - 1, max_Stages, time_steps + 1] + (1/(max_Performance - 1))
    
  }
  # This loop fills the cells of the last time step and the last Stage with fitness values
  
  
  for (k in 1:max_Stages - 1) {
    
    
    Fitness[, , k , time_steps + 1] <- 0
    
  }
  # This loop fills with 0 all the cells that are in stages lower than the maximum,
  # as only the last stage gives you fitness.
  
  for (t in (time_steps + 2):(time_steps + 1 + max_Stages)){
    
    Fitness[, , , t] <- Fitness[, , max_Stages, time_steps + 1]
    
  }
  # This loop fills with 0 all the cells that are after the last time step.
  # I had to add extra "time_steps" in order to make some functions work, but
  # their fitness values are 0. 
  
  Fitness[Fitness < 2] <- 0
  
  # Array that stores the Fitness values for every time step. In every time step
  # you multiply your current condition by the expected Fitness value that would get
  # if you invest on this or that trait.
  # Only on the last Metamorphosis Stage you receive final fitness. 
  
  prob_end_season <- c(rep(0, round(time_steps - (end_season_percentage * time_steps))),
                       seq(0, end_season_intensity, (end_season_intensity/ (round(end_season_percentage * time_steps) - 1))))
  prob_no_end_season <- 1 - prob_end_season
  # end_season_percentage is the percentage of days in the standard metamorphosis period that are 
  # likely to be the end of the season due to stochastic causes. 
  # end_season_intensity is the intensity of the event occurring at the end of the standard 
  # metamorphosis period.
  # At the moment, it is a linear function, but I would like it to be exponential.
  
  Condition <- matrix(nrow = max_Size, ncol = max_Performance)
  Condition[ , ] <- Size %*% t(Performance)
  Condition <- Condition / max(Condition)
  Condition <- t(t(Condition) + Performance)
  Condition <- Condition / max(Condition)
  
  Condition <- Condition / (max(Condition) * 10) + 0.8
  # Condition is the result of the interaction between Size and Performance 
  # and it's different for every combination of each trait.
  # We divide by the highest value to create a 0 to 1 Condition matrix.
  
  Survival <- matrix(nrow = max_Size, ncol = max_Performance)
  Survival[,1] <- c(0, seq(1 - 2*death_rate_day, 1 - death_rate_day, death_rate_day/(max_Size - 2)))
  for (j in 2:max_Performance) {
    
    Survival[, j] <- Survival[, j - 1] + (death_rate_day/(max_Performance - 1))
    
  }
  Survival[1,] <- 0
  # survival rate per size
  
  Metamorphosis <- matrix(nrow = max_Size, ncol = max_Performance)
  Metamorphosis[Size >= 4] <- 1
  Metamorphosis[Size < 4] <- 0
  # Matrix that has a 0 on those sizes that are not able to metamorphose and 1 for
  # those sizes that can metamorphose. 
  
  
  ForageRule <- array(NA, dim = c(max_Size, max_Performance, max_Stages, time_steps))
  ForageRule_B <- array(NA, dim = c(max_Size, max_Performance, max_Stages, time_steps))
  # Here, the ForageRule array is a 2 state variable matrix with time as the 3rd dimension,
  # and depends has the same dimensions as Performance and Size. 
  # We are going to store the TRUE/FALsE results here,
  # and see if it's better to invest in Performance or in Size.
  
  
  RewardIfPerformance <-  array(NA, dim = c(max_Size, max_Performance, max_Stages, time_steps))
  RewardIfGrowth <-  array(NA, dim = c(max_Size, max_Performance, max_Stages, time_steps))
  RewardIfMetamorphosis <-  array(NA, dim = c(max_Size, max_Performance, max_Stages, time_steps))
  # Arrays that are going to store the fitness values of each decision at 
  # every time step and state.
  
  
  # Loop
  
  t <- time_steps
  
  while (t >= 1) { 
    
    for (k in 1:max_Stages) {
      
      for (i in 1:max_Size) {
        
        for (j in 1:max_Performance) {
          
          # The rest of the cells will operate as follows: You multiply your 
          # current survival rate per your condition, and depending on the temperature,
          # you receive more or less benefits from your decision. Also there's the chance to 
          # jump one time step if the temperature is good. This is what happens if the season 
          # continues on the next time step. If it ends stochastically, you can't 
          # invest on anything, and you just have to survive until next time step.
          
          
          
          n <- max_Stages - k
          # We are always going to look for the fitness that we are going to 
          # receive in max_Stage time steps ahead. If you are in time_step 46, 
          # you are going to look for the fitness that is 4 time steps ahead 
          # (time_step 50), as there are no more time steps after that.
          
          if (Metamorphosis[i, j] == 1) {
            # First we are going to calculate the expected fitness for each decision
            # if you have a size that allows you to metamorphose.
            
            if (j == max_Performance & i == max_Size & k == max_Stages & t <= time_steps){
              
              RewardIfPerformance[i, j, k, t] <- (Condition[i, j] * Survival[i, j] * Fitness[i, j, k, t + 1] * prob_good_temp +
                                                    Condition[i, j] * Survival[i, j] * Fitness[i, j, k, t + 1] * prob_bad_temp +
                                                    (1 - Condition[i, j]) * Survival[i, j] * Fitness[i, j, k, t + 1] * prob_good_temp +
                                                    (1 - Condition[i, j]) * Survival[i, j] * Fitness[i, j, k, t + 1] * prob_bad_temp) * prob_no_end_season[t] + 
                Fitness[i, j, k, t + 1] * Survival[i, j] * prob_end_season[t] 
              
              
              RewardIfGrowth[i, j, k, t] <- (Condition[i, j] * Survival[i, j] * Fitness[i, j, k, t + 1] * prob_good_temp +
                                               Condition[i, j] * Survival[i, j] * Fitness[i, j, k, t + 1] * prob_bad_temp +
                                               (1 - Condition[i, j]) * Survival[i, j] * Fitness[i, j, k, t + 1] * prob_good_temp +
                                               (1 - Condition[i, j]) * Survival[i, j] * Fitness[i, j, k, t + 1] * prob_bad_temp) * prob_no_end_season[t] + 
                Fitness[i, j, k, t + 1] * Survival[i, j] * prob_end_season[t]
              
              
              RewardIfMetamorphosis[i, j, k, t] <- Fitness[i, j, k, t + n] * ((Survival[i, j])^n) + 
                ((max_Stages/max_Stages - k/max_Stages) * 0.1) + ((prob_end_season[t] / end_season_percentage) * 0.1)
              
              # You are going to look for the fitness that is 10 time steps ahead 
              # of you (if you are in k = 1), and this is going to be multiplied 
              # by survival, that is going to be multiplied also by the number 
              # of time steps that are left to reach that stage.
              # The last thing (t/time_steps) is an idea of how to make this decision
              # less important as you go backwards (and more important as you approach 
              # the final time step). Spoiler: it doesn't work properly. 
              
              # I HAVE TO CHANGE THIS: Now you are always looking 10 time_steps 
              # ahead, even if you are in k = 9, and you should look for the 
              # number of stages that are left to reach k = 10.
              # This is a problem in terms of time, as if you are in 
              # time step 47 (out of 50) and you are in k = 1, you are going to look for time
              # step 57, and it doesn't exist. I have to solve this...
                
              
            } else if (j == max_Performance & i == max_Size & k < max_Stages & t <= time_steps){
              
              RewardIfPerformance[i, j, k, t] <- (Condition[i, j] * Survival[i, j] * Fitness[i, j, max_Stages, t + 1] * prob_good_temp +
                                                    Condition[i, j] * Survival[i, j] * Fitness[i, j, max_Stages, t + 1] * prob_bad_temp +
                                                    (1 - Condition[i, j]) * Survival[i, j] * Fitness[i, j, max_Stages, t + 1] * prob_good_temp +
                                                    (1 - Condition[i, j]) * Survival[i, j] * Fitness[i, j, max_Stages, t + 1] * prob_bad_temp) * prob_no_end_season[t] + 
                Fitness[i, j, max_Stages, t + 1] * Survival[i, j] * prob_end_season[t] 
              
              
              RewardIfGrowth[i, j, k, t] <- (Condition[i, j] * Survival[i, j] * Fitness[i, j, max_Stages, t + 1] * prob_good_temp +
                                               Condition[i, j] * Survival[i, j] * Fitness[i, j, max_Stages, t + 1] * prob_bad_temp +
                                               (1 - Condition[i, j]) * Survival[i, j] * Fitness[i, j, max_Stages, t + 1] * prob_good_temp +
                                               (1 - Condition[i, j]) * Survival[i, j] * Fitness[i, j, max_Stages, t + 1] * prob_bad_temp) * prob_no_end_season[t] + 
                Fitness[i, j, max_Stages, t + 1] * Survival[i, j] * prob_end_season[t]
              
              
              RewardIfMetamorphosis[i, j, k, t] <-  Fitness[i, j, max_Stages, t + n] * ((Survival[i, j])^n) + 
                ((max_Stages/max_Stages - k/max_Stages) * 0.1) + ((prob_end_season[t] / end_season_percentage) * 0.1)
              
              
            } else if (j == max_Performance & i < max_Size & k == max_Stages & t <= time_steps){
              
              RewardIfPerformance[i, j, k, t] <- (Condition[i, j] * Survival[i, j] * Fitness[i, j, k, t + 1] * prob_good_temp +
                                                    Condition[i, j] * Survival[i, j] * Fitness[i, j, k, t + 1] * prob_bad_temp +
                                                    (1 - Condition[i, j]) * Survival[i, j] * Fitness[i, j, k, t + 1] * prob_good_temp +
                                                    (1 - Condition[i, j]) * Survival[i, j] * Fitness[i, j, k, t + 1] * prob_bad_temp) * prob_no_end_season[t] + 
                Fitness[i, j, k, t + 1] * Survival[i, j] * prob_end_season[t] 
              
              
              RewardIfGrowth[i, j, k, t] <- (Condition[i, j] * Survival[i, j] * Fitness[i + 1, j, k, t + 1] * prob_good_temp +
                                               Condition[i, j] * Survival[i, j] * Fitness[i, j, k, t + 1] * prob_bad_temp +
                                               (1 - Condition[i, j]) * Survival[i, j] * Fitness[i, j, k, t + 1] * prob_good_temp +
                                               (1 - Condition[i, j]) * Survival[i, j] * Fitness[i, j, k, t + 1] * prob_bad_temp) * prob_no_end_season[t] + 
                Fitness[i, j, k, t + 1] * Survival[i, j] * prob_end_season[t]
              
              
              RewardIfMetamorphosis[i, j, k, t] <-  Fitness[i, j, k, t + n] * ((Survival[i, j])^n) + 
                ((max_Stages/max_Stages - k/max_Stages) * 0.1) + ((prob_end_season[t] / end_season_percentage) * 0.1)
              
              
            } else if (j < max_Performance & i == max_Size & k == max_Stages & t <= time_steps){
              
              RewardIfPerformance[i, j, k, t] <- (Condition[i, j] * Survival[i, j] * Fitness[i, j + 1, k, t + 1] * prob_good_temp +
                                                    Condition[i, j] * Survival[i, j] * Fitness[i, j + 1, k, t + 1] * prob_bad_temp +
                                                    (1 - Condition[i, j]) * Survival[i, j] * Fitness[i, j, k, t + 1] * prob_good_temp +
                                                    (1 - Condition[i, j]) * Survival[i, j] * Fitness[i, j, k, t + 1] * prob_bad_temp) * prob_no_end_season[t] + 
                Fitness[i, j, k, t + 1] * Survival[i, j] * prob_end_season[t] 
              
              
              RewardIfGrowth[i, j, k, t] <- (Condition[i, j] * Survival[i, j] * Fitness[i, j, k, t + 1] * prob_good_temp +
                                               Condition[i, j] * Survival[i, j] * Fitness[i, j, k, t + 1] * prob_bad_temp +
                                               (1 - Condition[i, j]) * Survival[i, j] * Fitness[i, j, k, t + 1] * prob_good_temp +
                                               (1 - Condition[i, j]) * Survival[i, j] * Fitness[i, j, k, t + 1] * prob_bad_temp) * prob_no_end_season[t] + 
                Fitness[i, j, k, t + 1] * Survival[i, j] * prob_end_season[t]
              
              
              RewardIfMetamorphosis[i, j, k, t] <-  Fitness[i, j, k, t + n] * ((Survival[i, j])^n) + 
                ((max_Stages/max_Stages - k/max_Stages) * 0.1) + ((prob_end_season[t] / end_season_percentage) * 0.1)
              
              
            } else if (j == max_Performance & i < max_Size & k < max_Stages & t <= time_steps){
              
              RewardIfPerformance[i, j, k, t] <- (Condition[i, j] * Survival[i, j] * Fitness[i, j, max_Stages, t + 1] * prob_good_temp +
                                                    Condition[i, j] * Survival[i, j] * Fitness[i, j, max_Stages, t + 1] * prob_bad_temp +
                                                    (1 - Condition[i, j]) * Survival[i, j] * Fitness[i, j, max_Stages, t + 1] * prob_good_temp +
                                                    (1 - Condition[i, j]) * Survival[i, j] * Fitness[i, j, max_Stages, t + 1] * prob_bad_temp) * prob_no_end_season[t] + 
                Fitness[i, j, max_Stages, t + 1] * Survival[i, j] * prob_end_season[t] 
              
              
              RewardIfGrowth[i, j, k, t] <- (Condition[i, j] * Survival[i, j] * Fitness[i + 1, j, max_Stages, t + 1] * prob_good_temp +
                                               Condition[i, j] * Survival[i, j] * Fitness[i, j, max_Stages, t + 1] * prob_bad_temp +
                                               (1 - Condition[i, j]) * Survival[i, j] * Fitness[i, j, max_Stages, t + 1] * prob_good_temp +
                                               (1 - Condition[i, j]) * Survival[i, j] * Fitness[i, j, max_Stages, t + 1] * prob_bad_temp) * prob_no_end_season[t] + 
                Fitness[i, j, max_Stages, t + 1] * Survival[i, j] * prob_end_season[t]
              
              
              RewardIfMetamorphosis[i, j, k, t] <- Fitness[i, j, max_Stages, t + n] * ((Survival[i, j])^n) + 
                ((max_Stages/max_Stages - k/max_Stages) * 0.1) + ((prob_end_season[t] / end_season_percentage) * 0.1)
              
              
            } else if (j < max_Performance & i == max_Size & k < max_Stages & t <= time_steps){
              
              RewardIfPerformance[i, j, k, t] <- (Condition[i, j] * Survival[i, j] * Fitness[i, j + 1, max_Stages, t + 1] * prob_good_temp +
                                                    Condition[i, j] * Survival[i, j] * Fitness[i, j + 1, max_Stages, t + 1] * prob_bad_temp +
                                                    (1 - Condition[i, j]) * Survival[i, j] * Fitness[i, j, max_Stages, t + 1] * prob_good_temp +
                                                    (1 - Condition[i, j]) * Survival[i, j] * Fitness[i, j, max_Stages, t + 1] * prob_bad_temp) * prob_no_end_season[t] + 
                Fitness[i, j, max_Stages, t + 1] * Survival[i, j] * prob_end_season[t] 
              
              
              RewardIfGrowth[i, j, k, t] <- (Condition[i, j] * Survival[i, j] * Fitness[i, j, max_Stages, t + 1] * prob_good_temp +
                                               Condition[i, j] * Survival[i, j] * Fitness[i, j, max_Stages, t + 1] * prob_bad_temp +
                                               (1 - Condition[i, j]) * Survival[i, j] * Fitness[i, j, max_Stages, t + 1] * prob_good_temp +
                                               (1 - Condition[i, j]) * Survival[i, j] * Fitness[i, j, max_Stages, t + 1] * prob_bad_temp) * prob_no_end_season[t] + 
                Fitness[i, j, max_Stages, t + 1] * Survival[i, j] * prob_end_season[t]
              
              
              RewardIfMetamorphosis[i, j, k, t] <-  Fitness[i, j, max_Stages, t + n] * ((Survival[i, j])^n) + 
                ((max_Stages/max_Stages - k/max_Stages) * 0.1) + ((prob_end_season[t] / end_season_percentage) * 0.1)
              
              
            } else if (j < max_Performance & i < max_Size & k == max_Stages & t <= time_steps){
              
              RewardIfPerformance[i, j, k, t] <- (Condition[i, j] * Survival[i, j] * Fitness[i, j + 1, k, t + 1] * prob_good_temp +
                                                    Condition[i, j] * Survival[i, j] * Fitness[i, j + 1, k, t + 1] * prob_bad_temp +
                                                    (1 - Condition[i, j]) * Survival[i, j] * Fitness[i, j, k, t + 1] * prob_good_temp +
                                                    (1 - Condition[i, j]) * Survival[i, j] * Fitness[i, j, k, t + 1] * prob_bad_temp) * prob_no_end_season[t] + 
                Fitness[i, j, k, t + 1] * Survival[i, j] * prob_end_season[t] 
              
              
              RewardIfGrowth[i, j, k, t] <- (Condition[i, j] * Survival[i, j] * Fitness[i + 1, j, k, t + 1] * prob_good_temp +
                                               Condition[i, j] * Survival[i, j] * Fitness[i, j, k, t + 1] * prob_bad_temp +
                                               (1 - Condition[i, j]) * Survival[i, j] * Fitness[i, j, k, t + 1] * prob_good_temp +
                                               (1 - Condition[i, j]) * Survival[i, j] * Fitness[i, j, k, t + 1] * prob_bad_temp) * prob_no_end_season[t] + 
                Fitness[i, j, k, t + 1] * Survival[i, j] * prob_end_season[t]
              
              
              RewardIfMetamorphosis[i, j, k, t] <-  Fitness[i, j, k, t + n] * ((Survival[i, j])^n) + 
                ((max_Stages/max_Stages - k/max_Stages) * 0.1) + ((prob_end_season[t] / end_season_percentage) * 0.1)
              
              
            } else {
              
              RewardIfPerformance[i, j, k, t] <- (Condition[i, j] * Survival[i, j] * Fitness[i, j + 1, max_Stages, t + 1] * prob_good_temp +
                                                    Condition[i, j] * Survival[i, j] * Fitness[i, j + 1, max_Stages, t + 1] * prob_bad_temp +
                                                    (1 - Condition[i, j]) * Survival[i, j] * Fitness[i, j, max_Stages, t + 1] * prob_good_temp +
                                                    (1 - Condition[i, j]) * Survival[i, j] * Fitness[i, j, max_Stages, t + 1] * prob_bad_temp) * prob_no_end_season[t] + 
                Fitness[i, j, max_Stages, t + 1] * Survival[i, j] * prob_end_season[t] 
              
              
              RewardIfGrowth[i, j, k, t] <- (Condition[i, j] * Survival[i, j] * Fitness[i + 1, j, max_Stages, t + 1] * prob_good_temp +
                                               Condition[i, j] * Survival[i, j] * Fitness[i, j, max_Stages, t + 1] * prob_bad_temp +
                                               (1 - Condition[i, j]) * Survival[i, j] * Fitness[i, j, max_Stages, t + 1] * prob_good_temp +
                                               (1 - Condition[i, j]) * Survival[i, j] * Fitness[i, j, max_Stages, t + 1] * prob_bad_temp) * prob_no_end_season[t] + 
                Fitness[i, j, max_Stages, t + 1] * Survival[i, j] * prob_end_season[t]
              
              
              RewardIfMetamorphosis[i, j, k, t] <-  Fitness[i, j, max_Stages, t + n] * ((Survival[i, j])^n) + 
                ((max_Stages/max_Stages - k/max_Stages) * 0.1) + ((prob_end_season[t] / end_season_percentage) * 0.1)
              
            } # end if/else loop
            
            
            
          } else if (Metamorphosis[i, j] == 0) {
            
            # This part is for those sizes that can't metamorphose.
            # The reward they obtain by metamorphosing is 0.
            
            if (j == max_Performance & i == max_Size & k == max_Stages & t <= time_steps){
              
              RewardIfPerformance[i, j, k, t] <- (Condition[i, j] * Survival[i, j] * Fitness[i, j, k, t + 1] * prob_good_temp +
                                                    Condition[i, j] * Survival[i, j] * Fitness[i, j, k, t + 1] * prob_bad_temp +
                                                    (1 - Condition[i, j]) * Survival[i, j] * Fitness[i, j, k, t + 1] * prob_good_temp +
                                                    (1 - Condition[i, j]) * Survival[i, j] * Fitness[i, j, k, t + 1] * prob_bad_temp) * prob_no_end_season[t] + 
                Fitness[i, j, k, t + 1] * Survival[i, j] * prob_end_season[t] 
              
              
              RewardIfGrowth[i, j, k, t] <- (Condition[i, j] * Survival[i, j] * Fitness[i, j, k, t + 1] * prob_good_temp +
                                               Condition[i, j] * Survival[i, j] * Fitness[i, j, k, t + 1] * prob_bad_temp +
                                               (1 - Condition[i, j]) * Survival[i, j] * Fitness[i, j, k, t + 1] * prob_good_temp +
                                               (1 - Condition[i, j]) * Survival[i, j] * Fitness[i, j, k, t + 1] * prob_bad_temp) * prob_no_end_season[t] + 
                Fitness[i, j, k, t + 1] * Survival[i, j] * prob_end_season[t]
              
              
              RewardIfMetamorphosis[i, j, k, t] <-  0
              
              
            } else if (j == max_Performance & i == max_Size & k < max_Stages & t <= time_steps){
              
              RewardIfPerformance[i, j, k, t] <- (Condition[i, j] * Survival[i, j] * Fitness[i, j, max_Stages, t + 1] * prob_good_temp +
                                                    Condition[i, j] * Survival[i, j] * Fitness[i, j, max_Stages, t + 1] * prob_bad_temp +
                                                    (1 - Condition[i, j]) * Survival[i, j] * Fitness[i, j, max_Stages, t + 1] * prob_good_temp +
                                                    (1 - Condition[i, j]) * Survival[i, j] * Fitness[i, j, max_Stages, t + 1] * prob_bad_temp) * prob_no_end_season[t] + 
                Fitness[i, j, max_Stages, t + 1] * Survival[i, j] * prob_end_season[t] 
              
              
              RewardIfGrowth[i, j, k, t] <- (Condition[i, j] * Survival[i, j] * Fitness[i, j, max_Stages, t + 1] * prob_good_temp +
                                               Condition[i, j] * Survival[i, j] * Fitness[i, j, max_Stages, t + 1] * prob_bad_temp +
                                               (1 - Condition[i, j]) * Survival[i, j] * Fitness[i, j, max_Stages, t + 1] * prob_good_temp +
                                               (1 - Condition[i, j]) * Survival[i, j] * Fitness[i, j, max_Stages, t + 1] * prob_bad_temp) * prob_no_end_season[t] + 
                Fitness[i, j, max_Stages, t + 1] * Survival[i, j] * prob_end_season[t]
              
              
              RewardIfMetamorphosis[i, j, k, t] <-  0
              
              
            } else if (j == max_Performance & i < max_Size & k == max_Stages & t <= time_steps){
              
              RewardIfPerformance[i, j, k, t] <- (Condition[i, j] * Survival[i, j] * Fitness[i, j, k, t + 1] * prob_good_temp +
                                                    Condition[i, j] * Survival[i, j] * Fitness[i, j, k, t + 1] * prob_bad_temp +
                                                    (1 - Condition[i, j]) * Survival[i, j] * Fitness[i, j, k, t + 1] * prob_good_temp +
                                                    (1 - Condition[i, j]) * Survival[i, j] * Fitness[i, j, k, t + 1] * prob_bad_temp) * prob_no_end_season[t] + 
                Fitness[i, j, k, t + 1] * Survival[i, j] * prob_end_season[t] 
              
              
              RewardIfGrowth[i, j, k, t] <- (Condition[i, j] * Survival[i, j] * Fitness[i + 1, j, k, t + 1] * prob_good_temp +
                                               Condition[i, j] * Survival[i, j] * Fitness[i, j, k, t + 1] * prob_bad_temp +
                                               (1 - Condition[i, j]) * Survival[i, j] * Fitness[i, j, k, t + 1] * prob_good_temp +
                                               (1 - Condition[i, j]) * Survival[i, j] * Fitness[i, j, k, t + 1] * prob_bad_temp) * prob_no_end_season[t] + 
                Fitness[i, j, k, t + 1] * Survival[i, j] * prob_end_season[t]
              
              
              RewardIfMetamorphosis[i, j, k, t] <-  0
              
              
            } else if (j < max_Performance & i == max_Size & k == max_Stages & t <= time_steps){
              
              RewardIfPerformance[i, j, k, t] <- (Condition[i, j] * Survival[i, j] * Fitness[i, j + 1, k, t + 1] * prob_good_temp +
                                                    Condition[i, j] * Survival[i, j] * Fitness[i, j + 1, k, t + 1] * prob_bad_temp +
                                                    (1 - Condition[i, j]) * Survival[i, j] * Fitness[i, j, k, t + 1] * prob_good_temp +
                                                    (1 - Condition[i, j]) * Survival[i, j] * Fitness[i, j, k, t + 1] * prob_bad_temp) * prob_no_end_season[t] + 
                Fitness[i, j, k, t + 1] * Survival[i, j] * prob_end_season[t] 
              
              
              RewardIfGrowth[i, j, k, t] <- (Condition[i, j] * Survival[i, j] * Fitness[i, j, k, t + 1] * prob_good_temp +
                                               Condition[i, j] * Survival[i, j] * Fitness[i, j, k, t + 1] * prob_bad_temp +
                                               (1 - Condition[i, j]) * Survival[i, j] * Fitness[i, j, k, t + 1] * prob_good_temp +
                                               (1 - Condition[i, j]) * Survival[i, j] * Fitness[i, j, k, t + 1] * prob_bad_temp) * prob_no_end_season[t] + 
                Fitness[i, j, k, t + 1] * Survival[i, j] * prob_end_season[t]
              
              
              RewardIfMetamorphosis[i, j, k, t] <-  0
              
              
            } else if (j == max_Performance & i < max_Size & k < max_Stages & t <= time_steps){
              
              RewardIfPerformance[i, j, k, t] <- (Condition[i, j] * Survival[i, j] * Fitness[i, j, max_Stages, t + 1] * prob_good_temp +
                                                    Condition[i, j] * Survival[i, j] * Fitness[i, j, max_Stages, t + 1] * prob_bad_temp +
                                                    (1 - Condition[i, j]) * Survival[i, j] * Fitness[i, j, max_Stages, t + 1] * prob_good_temp +
                                                    (1 - Condition[i, j]) * Survival[i, j] * Fitness[i, j, max_Stages, t + 1] * prob_bad_temp) * prob_no_end_season[t] + 
                Fitness[i, j, max_Stages, t + 1] * Survival[i, j] * prob_end_season[t] 
              
              
              RewardIfGrowth[i, j, k, t] <- (Condition[i, j] * Survival[i, j] * Fitness[i + 1, j, max_Stages, t + 1] * prob_good_temp +
                                               Condition[i, j] * Survival[i, j] * Fitness[i, j, max_Stages, t + 1] * prob_bad_temp +
                                               (1 - Condition[i, j]) * Survival[i, j] * Fitness[i, j, max_Stages, t + 1] * prob_good_temp +
                                               (1 - Condition[i, j]) * Survival[i, j] * Fitness[i, j, max_Stages, t + 1] * prob_bad_temp) * prob_no_end_season[t] + 
                Fitness[i, j, max_Stages, t + 1] * Survival[i, j] * prob_end_season[t]
              
              
              RewardIfMetamorphosis[i, j, k, t] <-  0
              
              
            } else if (j < max_Performance & i == max_Size & k < max_Stages & t <= time_steps){
              
              RewardIfPerformance[i, j, k, t] <- (Condition[i, j] * Survival[i, j] * Fitness[i, j + 1, max_Stages, t + 1] * prob_good_temp +
                                                    Condition[i, j] * Survival[i, j] * Fitness[i, j + 1, max_Stages, t + 1] * prob_bad_temp +
                                                    (1 - Condition[i, j]) * Survival[i, j] * Fitness[i, j, max_Stages, t + 1] * prob_good_temp +
                                                    (1 - Condition[i, j]) * Survival[i, j] * Fitness[i, j, max_Stages, t + 1] * prob_bad_temp) * prob_no_end_season[t] + 
                Fitness[i, j, max_Stages, t + 1] * Survival[i, j] * prob_end_season[t] 
              
              
              RewardIfGrowth[i, j, k, t] <- (Condition[i, j] * Survival[i, j] * Fitness[i, j, max_Stages, t + 1] * prob_good_temp +
                                               Condition[i, j] * Survival[i, j] * Fitness[i, j, max_Stages, t + 1] * prob_bad_temp +
                                               (1 - Condition[i, j]) * Survival[i, j] * Fitness[i, j, max_Stages, t + 1] * prob_good_temp +
                                               (1 - Condition[i, j]) * Survival[i, j] * Fitness[i, j, max_Stages, t + 1] * prob_bad_temp) * prob_no_end_season[t] + 
                Fitness[i, j, max_Stages, t + 1] * Survival[i, j] * prob_end_season[t]
              
              
              RewardIfMetamorphosis[i, j, k, t] <-  0
              
              
            } else if (j < max_Performance & i < max_Size & k == max_Stages & t <= time_steps){
              
              RewardIfPerformance[i, j, k, t] <- (Condition[i, j] * Survival[i, j] * Fitness[i, j + 1, k, t + 1] * prob_good_temp +
                                                    Condition[i, j] * Survival[i, j] * Fitness[i, j + 1, k, t + 1] * prob_bad_temp +
                                                    (1 - Condition[i, j]) * Survival[i, j] * Fitness[i, j, k, t + 1] * prob_good_temp +
                                                    (1 - Condition[i, j]) * Survival[i, j] * Fitness[i, j, k, t + 1] * prob_bad_temp) * prob_no_end_season[t] + 
                Fitness[i, j, k, t + 1] * Survival[i, j] * prob_end_season[t] 
              
              
              RewardIfGrowth[i, j, k, t] <- (Condition[i, j] * Survival[i, j] * Fitness[i + 1, j, k, t + 1] * prob_good_temp +
                                               Condition[i, j] * Survival[i, j] * Fitness[i, j, k, t + 1] * prob_bad_temp +
                                               (1 - Condition[i, j]) * Survival[i, j] * Fitness[i, j, k, t + 1] * prob_good_temp +
                                               (1 - Condition[i, j]) * Survival[i, j] * Fitness[i, j, k, t + 1] * prob_bad_temp) * prob_no_end_season[t] + 
                Fitness[i, j, k, t + 1] * Survival[i, j] * prob_end_season[t]
              
              
              RewardIfMetamorphosis[i, j, k, t] <-  0
              
              
            } else {
              
              RewardIfPerformance[i, j, k, t] <- (Condition[i, j] * Survival[i, j] * Fitness[i, j + 1, max_Stages, t + 1] * prob_good_temp +
                                                    Condition[i, j] * Survival[i, j] * Fitness[i, j + 1, max_Stages, t + 1] * prob_bad_temp +
                                                    (1 - Condition[i, j]) * Survival[i, j] * Fitness[i, j, max_Stages, t + 1] * prob_good_temp +
                                                    (1 - Condition[i, j]) * Survival[i, j] * Fitness[i, j, max_Stages, t + 1] * prob_bad_temp) * prob_no_end_season[t] + 
                Fitness[i, j, max_Stages, t + 1] * Survival[i, j] * prob_end_season[t] 
              
              
              RewardIfGrowth[i, j, k, t] <- (Condition[i, j] * Survival[i, j] * Fitness[i + 1, j, max_Stages, t + 1] * prob_good_temp +
                                               Condition[i, j] * Survival[i, j] * Fitness[i, j, max_Stages, t + 1] * prob_bad_temp +
                                               (1 - Condition[i, j]) * Survival[i, j] * Fitness[i, j, max_Stages, t + 1] * prob_good_temp +
                                               (1 - Condition[i, j]) * Survival[i, j] * Fitness[i, j, max_Stages, t + 1] * prob_bad_temp) * prob_no_end_season[t] + 
                Fitness[i, j, max_Stages, t + 1] * Survival[i, j] * prob_end_season[t]
              
              
              RewardIfMetamorphosis[i, j, k, t] <-  0
              
              # The rest of the cells will operate as follows: You multiply your 
              # current survival rate per your condition, and depending on the temperature,
              # you receive more or less benefits from your decision. Also there's the chance to 
              # jump one time step if the temperature is good. This is what happens if the season 
              # continues on the next time step. If it ends stochastically, you can't 
              # invest on anything, and you just have to survive until next time step.
              
            } # end if/else loop
            
          } # if/else Mteamorphosis 1 or 0
          
        } # end j loop
        
      } # end i loop
      
      ForageRule[, , k, t] <- RewardIfPerformance[, , k, t] > RewardIfGrowth[, , k, t]
      # TRUE/False matrix depending on which decision is best, and the result
      # is stored.
      
      Fitness[, , k, t] <- ForageRule[, , k, t] * RewardIfPerformance[, , k, t] +
        as.numeric(!ForageRule[, , k, t]) * RewardIfGrowth[, , k, t]
      # This matrix stores the reward values obtained for the best decisions, 
      # and it is going to be used in the next time step.
      
      ForageRule_B[, , k, t] <- RewardIfMetamorphosis[, , k, t] > Fitness[, , k, t]
      # This ForageRule_B is useful to compare if making the decision of
      # starting the metamorphosis is better than the current fitness.
      
      
      # I should find a way to write the fitness they would obtain by starting 
      # the metamorphosis in the fitness matrix
  
      
    } # end k loop
        
        
    
    
    RewardIfPerformance[1, , , ] <- 0
    RewardIfGrowth[1, , , ] <- 0
    RewardIfMetamorphosis[1, , , ] <- 0
    
    # Fitness values if you are dead.
    
    
    
    t <- t - 1
    
  } # end of while loop
  
  assign("Condition", Condition, envir =  globalenv())
  assign("time_steps", time_steps, envir = globalenv())
  assign("Fitness", Fitness, envir = globalenv())
  assign("ForageRule", ForageRule, envir=globalenv())
  assign("max_Size", max_Size, envir=globalenv())
  assign("Size", Size, envir=globalenv())
  assign("max_Performance", max_Performance, envir=globalenv())
  assign("Performance", Performance, envir=globalenv())
  assign("Survival", Survival, envir=globalenv())
  assign("Fitness_values", Fitness_values, envir=globalenv())
  assign("ForageRule_B", ForageRule_B, envir = globalenv())
  assign("RewardIfPerformance", RewardIfPerformance, envir = globalenv())
  assign("RewardIfGrowth", RewardIfGrowth, envir = globalenv())
  assign("RewardIfMetamorphosis", RewardIfMetamorphosis, envir = globalenv())
  
  # This line extracts Fitness, ForageRule and other objects from inside the function to 
  # the global environment, so we can use it in the plot function or the Forward simulation.
  
  
} # end of Decision function


# Decision plot function

Backwards_Plot <- function(){
  
  par(mfrow=c(3,4))
  par(mar=c(5.1, 4.5, 4.1, 6.5))
  
  t <- time_steps
  
  while (t >= 1) {
    
    ForageRule_rev <- apply(ForageRule[, , 1, t], 2, rev) # At time step "t"
    ForageRule_B_rev <- apply(ForageRule_B[, , 1, t], 2, rev) # At time step "t"
    ForageRule_rev[ForageRule_rev == "FALSE"] <- "Growth"
    ForageRule_rev[ForageRule_rev == "TRUE"] <- "Performance"
    ForageRule_rev[ForageRule_B_rev == "TRUE"] <- "Metamorphosis"
    ForageRule_rev[max_Size, ] <- "Dead"
    
    plot(ForageRule_rev, col=c('#440154FF', '#31688EFF', '#35B779FF', '#FDE725FF'), 
         breaks=c("Dead", "Growth", "Performance", "Metamorphosis"), xlab = "Burst speed (mm/s)", ylab = "Size (cm)",
         main = paste('Decision at time step ', t ), axis.col = NULL, axis.row = NULL)
    axis(1, at = 1:max_Performance, labels = Performance * 10)
    axis(2, at = 1:(max_Size), labels = c(Size), las = 1)
    
    #This function takes the time step you are in from the ForageRule array and 
    #creates an matrix where it changes the names from TRUE/FALSE to Dead, 
    #Performance or Growth. A plot is made of this new matrix. 
    #This is done for each time step. Ideally it would be an array too, but I 
    #can't get it to keep the array structure after changing the names.
    
    t <- t - 1 
    
  } # End of while loop
  
} # End of Plot loop





# Initial Parameters


prob_good_temp <- 0.5
# Probability of having a good Temperature

prob_bad_temp <- 1 - prob_good_temp 
# Probability of having a bad Temperature

days <- 50
# How many days does the metamorphosis last (normal conditions)?

end_season_percentage <- 0.4 
# How many days (% of the normal growing season), 
# beginning from the back, are susceptible to be the end of season (due stochasticity)?

end_season_intensity <- 1 
# Increasing probability of ending the season in that 
# particular time step since the days start to be susceptible of being the end of season.

death_rate_day <- 0.012 
# Death rate per day (from 0 to 1)



# Plot

Decisions(prob_good_temp, prob_bad_temp, days, end_season_percentage, end_season_intensity, death_rate_day, development_rate)


Backwards_Plot()

# What actually happens with the model: If we look at k=10, the maximum stage of 
# metamorphosis and where the frogs obtain fitness, we see that it behaves more or 
# less normally, as in the normal model. It is not optimal to take the decision to 
# invest in metamorphosis because you are already in the last stage, but at the 
# same time, you see a series of decisions that should not be possible because in 
# principle once in this stage, you don't grow or anything, you stay the same. This 
# should be changed for k = 10 (no benefits of any kind). 

# When we look at k = 9 and all the rest of k, we see that the optimal decision is 
# always the same: If you are below the minimum size to metamorphose, you must grow, 
# and if you are at the minimum size to metamorphose or above, you must start the 
# metamorphosis, even from day 1. What we should see is that at the beginning it is 
# better to invest in growth and performance, and as you get closer to the end of 
# the season, you should expect to see the frogs start to take the option to 
# metamorphose, starting with the bigger ones, which would already have a good 
# optimal size, and the smaller ones that would keep growing hoping to get a 
# bigger size with more fitness before metamorphosing. We don't observe this,
# and I don't know how to solve it. Thinking in 3D is costing me a lot and I 
# can't find a way to benefit from investing in performance/growth at the beginning 
# and investing in metamorphosis towards the end, and in a staggered way according 
# to the size... I need some suggestions or corrections on what is currently 
# written in order to solve this problem.

# Last time Update! Now it looks better! What I've changed is that now the decision 
# to grow or invest in performance is based on the expected fitness in the last 
# developmental stage, and what would they earn in that stage. This changes
# results from the fact that staying in the same stage in the the decisions
# of grow vs performance provided 0 fitness, as they could not see what was 
# the fitness on the next stage. Now it looks better.

# Also, in the RewardIfMetamorphosis part, there are some blocks that are a little
# bit tricky to understand, but the point is that one is 
# a "diminishing returns" with time (as you go backwards in time, less benefits you receive from 
# investing in metamorphosis, but as you approach the end, you get more benefits.) 
# The second part is used to give an incentive to those frogs that are in a low 
# metamorphosis stages to invest in metamorphosis.The more Stage you have, the 
# less you receive from investing in metamorphosis, and the lower your stage is, 
# the best is for you to start investing in metamorphosis. 

# There is still a problem, and it is that the final grid doesn't make much sense...
# If you change the stage you are looking at (9 or 1), you can see that if the 
# stage is low, you start investing in metamorphosis before,
# but how the decisions are distributed along the grid is very strange... You 
# wouldn't expect to have specific sizes to grow, others to invest in metamorphosis... 
# The patterns are strange. 

RewardIfMetamorphosis[,,1,41]
RewardIfPerformance[,,1,41]
RewardIfGrowth[,,1,41]
Size

Fitness[,,max_Stages,time_steps + 1]
