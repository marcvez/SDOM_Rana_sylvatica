

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
  
  Stages <- c(1:10)
  max_Stages <- length(Stages)
  
  
  Fitness_values <- Size
  max_Fitness <- length(Fitness_values)
  Fitness_values[Fitness_values < 4] <- 0
  Fitness_values[Fitness_values >=4] <- seq(2, 3, 2/(length(Fitness_values[Fitness_values >= 4]) - 1))
  
  
  Fitness_values
  # Sizes under 4 cm don't receive Fitness benefits. This is the benefit that you
  # receive for being in each Size at the final time step.
  
  Fitness <- array(NA, dim = c((max_Size), max_Performance, max_Stages, time_steps + 1))
  Fitness[, , max_Stages, time_steps + 1] <- Fitness_values
  
  
  for (j in 2:max_Performance) {
    
    Fitness[, j, max_Stages, time_steps + 1] <- Fitness[, j - 1, max_Stages, time_steps + 1] + (1/(max_Performance - 1))
    
  }
  
  
  for (k in 1:max_Stages - 1) {
    
    
    Fitness[, , k , time_steps + 1] <- 0
    
  }
  

  Fitness[Fitness < 2] <- 0
  
  # Array that stores the Fitness values for every time step. In every time step
  # you multiply your current condition by the expected Fitness value that would get
  # if you invest on this or that trait.
  
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
    
    for (j in 1:max_Performance) {
      
      for (i in 1:max_Size) {
        
        for (k in 1:max_Stages) {
          
          # The rest of the cells will operate as follows: You multiply your 
          # current survival rate per your condition, and depending on the temperature,
          # you receive more or less benefits from your decision. Also there's the chance to 
          # jump one time step if the temperature is good. This is what happens if the season 
          # continues on the next time step. If it ends stochastically, you can't 
          # invest on anything, and you just have to survive until next time step.
          
          
          # This first part models the decision in the final time step, when no 
          # time jumps can be made due to matrix sizes. The next block is the 
          # same but with the possibility of jumping.
          
          
          n <- min((time_steps - (t - 1)), 10)
          
          
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
            
            
            RewardIfMetamorphosis[i, j, k, t] <-  Fitness[i, j, k, t + 1] * Survival[i, j]
            
            
          } else if (j == max_Performance & i == max_Size & k < max_Stages & t <= time_steps){
            
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
            
            
            RewardIfMetamorphosis[i, j, k, t] <-  Fitness[i, j, k + 1, t + 1] * Survival[i, j]
            
            
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
            
            
            RewardIfMetamorphosis[i, j, k, t] <-  Fitness[i, j, k, t + 1] * Survival[i, j]
            
            
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
            
            
            RewardIfMetamorphosis[i, j, k, t] <-  Fitness[i, j, k, t + 1] * Survival[i, j]
            
            
          } else if (j == max_Performance & i < max_Size & k < max_Stages & t <= time_steps){
            
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
            
            
            RewardIfMetamorphosis[i, j, k, t] <-  Fitness[i, j, k + 1, t + 1] * Survival[i, j]
            
            
          } else if (j < max_Performance & i == max_Size & k < max_Stages & t <= time_steps){
            
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
            
            
            RewardIfMetamorphosis[i, j, k, t] <-  Fitness[i, j, k + 1, t + 1] * Survival[i, j]
            
            
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
            
            
            RewardIfMetamorphosis[i, j, k, t] <-  Fitness[i, j, k, t + 1] * Survival[i, j]
            
            
          } else {
            
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
            
            
            RewardIfMetamorphosis[i, j, k, t] <-  Fitness[i, j, k + 1, t + 1] * Survival[i, j]
            
            # The rest of the cells will operate as follows: You multiply your 
            # current survival rate per your condition, and depending on the temperature,
            # you receive more or less benefits from your decision. Also there's the chance to 
            # jump one time step if the temperature is good. This is what happens if the season 
            # continues on the next time step. If it ends stochastically, you can't 
            # invest on anything, and you just have to survive until next time step.
            
          } # end if/else loop
          
        } # end k loop
        
      } # end i loop
      
    } # end j loop
        
        
    
    
    RewardIfPerformance[1, , , ] <- 0
    RewardIfGrowth[1, , , ] <- 0
    RewardIfMetamorphosis[1, , , ] <- 0
    # Fitness values if you are dead.
    
    ForageRule[, , , t] <- RewardIfPerformance[, , , t] > RewardIfGrowth[, , , t]
    # TRUE/False matrix depending on which decision is best, and the result
    # is stored.
    
    Fitness[, , , t] <- ForageRule[, , , t] * RewardIfPerformance[, , , t] +
      as.numeric(!ForageRule[, , , t]) * RewardIfGrowth[, , , t]
    # This matrix stores the reward values obtained for the best decisions, 
    # and it is going to be used in the next time step.
    
    ForageRule_B[, , , t] <- RewardIfMetamorphosis[, , , t] > Fitness[, , , t]
    
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

