

#---------------------------- Backwards model ----------------------------------

# This script contains the backward simulation of the model. 
# Different object values will be generated, which will contain different 
# variables.

# Performance -> How fast you move.
# Size -> Your size.
# Stages -> Developmental state.
# Fitness -> Terminal fitness values.
# Condition -> Combination of performance and size, and determines the Forage 
# success.
# Survival -> Probability of survival depending on your current Size and 
# Performance.


# From this point, the model creates a loop in which at each time step the 
# organism can decide whether to invest and increase its Performance, Growth 
# or Developmental stage. 
# It compares which of the options is better (in terms of fitness) and stores 
# the decision in an array (ForageRule). The fitness values obtained from each 
# of the decisions are also stored (array Fitness).

# Finally, a plot for each time step is made and in it, we can see which 
# decision is the optimal one starting from the last time step (t = t) towards
# the first one (t = 1). 

# In this script, temperature plays an important role: Good temperature brings 
# better benefits than bad temperature. 

# The maximum developmental state has to be reached in order to survive and 
# reproduce. 

library(plot.matrix)


Decisions <- function (prob_good_temp, prob_bad_temp, days, 
                       end_season_percentage, end_season_intensity, 
                       death_rate_day) {
  
  time_steps <- days + ((prob_bad_temp - 0.5)*50)
  # An environment with higher temperatures is also more likely to dry out
  # earlier and to have a shorter growing season.
  # It works as the developmental rate.
  
  
  Performance <- seq(4.0, 7.5, 0.1) 
  max_Performance <- length(Performance)
  # Performance values (How fast you move cm/s).
  
  
  Size <- c(0, seq(1, 5.5, 0.1))
  max_Size <- length(Size)
  # Size values. All the values that tadpoles can archive. Also, this is the 
  # only trait that is relevant for the final Fitness (The bigger, the better).
  # Size 0 is equal to being dead.
  
  
  Stages <- c(1:10) 
  max_Stages <- length(Stages)
  # Number of Stages that the tadpole has to go through in order to 
  # complete the metamorphosis.
  
  
  Fitness_values <- Size
  max_Fitness <- length(Fitness_values)
  Fitness_values[Fitness_values < 4] <- 0
  Fitness_values[Fitness_values >=4] <- 
    seq(2, 4, 2/(length(Fitness_values[Fitness_values >= 4]) - 1))
    Fitness_values
  
  Fitness <- array(NA, dim = c((max_Size), max_Performance, max_Stages, 
                               time_steps + 1 + max_Stages))
  Fitness[, , max_Stages, time_steps + 1] <- Fitness_values
 
  for (k in 1:max_Stages - 1) {
    
    
    Fitness[, , k , time_steps + 1] <- 0
    
  }
  # Sizes smaller than 4 cm do not have Fitness. Fitness is the reproductive 
  # success that a specific size has in the last time step.Only the maximum 
  # developmental state has Fitness. If you don't reach that state, you 
  # are dead.
  
  
  
  prob_end_season <- c(rep(0, round(time_steps - 
                                      (end_season_percentage * time_steps))),
                       seq(0, end_season_intensity, 
                           (end_season_intensity / (round(end_season_percentage 
                                                          * time_steps) - 1))))
  prob_no_end_season <- 1 - prob_end_season
  # end_season_percentage is the percentage of days in the standard 
  # metamorphosis period that are likely to be the end of the season due to
  # stochastic causes. 
  # end_season_intensity is the intensity of the event occurring at the end of 
  # the standard metamorphosis period.
  
  
  Condition <- matrix(nrow = max_Size, ncol = max_Performance)
  Condition[ , ] <- Size %*% t(Performance)
  Condition <- Condition / max(Condition)
  Condition <- t(t(Condition) + Performance)
  Condition <- Condition / max(Condition)
    Condition <- Condition / (max(Condition) * 10) + 0.8
  # Condition is the result of the interaction between Size and Performance 
  # and it's different for every combination of each trait.
  # We divide by the highest value to create a 0 to 1 Condition matrix.
  # The values mean the probability of finding food given a concrete
  # Size and speed.
  
  
  Survival <- matrix(nrow = max_Size, ncol = max_Performance)
  Survival[,1] <- c(0, seq(1 - (3 * death_rate_day), 1 - (2 * death_rate_day), 
                           death_rate_day/(max_Size - 2)))
  for (j in 2:max_Performance) {
    
    Survival[, j] <- Survival[, j - 1] + 
      (2 * (death_rate_day/(max_Performance - 1)))
    
  }
  Survival[1,] <- 0
  # survival rate per size.
  
  
  
  Metamorphosis <- matrix(nrow = max_Size, ncol = max_Performance)
  Metamorphosis[Size >= 4] <- 1
  Metamorphosis[Size < 4] <- 0
  # Matrix that has a 0 on those sizes that are not able to metamorphose 
  # and 1 for those sizes that can metamorphose. 
  
  
  ForageRule <- array(NA, dim = c(max_Size, max_Performance, 
                                  max_Stages, time_steps))
  ForageRule_B <- array(NA, dim = c(max_Size, max_Performance, 
                                    max_Stages, time_steps))
  # Here, the ForageRule array is a 2 state variable matrix with time as a 
  # 3rd dimension. 
  # It stores the optimal decision as TRUE/FALSE (Performance/Growth).
  # ForageRule_B is the same but comparing if investing in Stages 
  # (start metamorphosis) is better than the Fitness obtained due 
  # the decision made and stored in the ForageRule array.
  
  
  RewardIfPerformance <-  array(0, dim = c(max_Size, max_Performance, 
                                           max_Stages, time_steps))
  RewardIfGrowth <-  array(0, dim = c(max_Size, max_Performance, 
                                      max_Stages, time_steps))
  RewardIfMetamorphosis <-  array(0, dim = c(max_Size, max_Performance, 
                                             max_Stages, time_steps))
  # Arrays that are going to store the fitness values of each decision at 
  # every time step and state.
  
  
  # Loop
  
  t <- time_steps
  
  while (t >= 1) { 
    
    for (k in 1:max_Stages) {
      
      for (i in 1:max_Size) {
        
        for (j in 1:max_Performance) {
          
          # This is how it is going to work: Each time step (from t = t to 
          # t = 1), it is going to be compared if it's better to invest in 
          # Growth, in Performance or to start investing energy in the
          # metamorphosis process. 
          # The results are going to be stored in different arrays and they 
          # are going to be used in the Forward simulation.
            
          
          if(Metamorphosis[i, j] == 1){
            
            RewardIfPerformance[i, j, k, t] <-
              
              (Condition[i, j] * Survival[i, j] * 
                 Fitness[i, min(j + 2, max_Performance), k, t + 1] * 
                 prob_good_temp + Condition[i, j] * Survival[i, j] * 
                 Fitness[i, min(j + 1, max_Performance), k, t + 1] * 
                 prob_bad_temp + (1 - Condition[i, j]) * Survival[i, j] * 
                 Fitness[i, j, k, t + 1] * prob_good_temp + 
                 (1 - Condition[i, j]) * Survival[i, j] * 
                 Fitness[i, j, k, t + 1] * prob_bad_temp) * 
              prob_no_end_season[t] + 
              Fitness[i, j, k, t + 1] * Survival[i, j] * prob_end_season[t] 
            
            
            RewardIfGrowth[i, j, k, t] <- 
              
              (Condition[i, j] * Survival[i, j] * 
                 Fitness[min(i + 2, max_Size), j, k, t + 1] * prob_good_temp + 
                 Condition[i, j] * Survival[i, j] * 
                 Fitness[min(i + 1, max_Size), j, k, t + 1] * prob_bad_temp + 
                 (1 - Condition[i, j]) * Survival[i, j] * 
                 Fitness[i, j, k, t + 1] * prob_good_temp + 
                 (1 - Condition[i, j]) * Survival[i, j] * 
                 Fitness[i, j, k, t + 1] * prob_bad_temp) * 
              prob_no_end_season[t] + 
              Fitness[i, j, k, t + 1] * Survival[i, j] * prob_end_season[t]
            
            
            RewardIfMetamorphosis[i, j, k, t] <-  
              
              Fitness[i, j, min(k + 1, max_Stages), t + 1] * 
              (Survival[i, j]) * Condition[i, j] * prob_no_end_season[t] +
              Fitness[i, j, k,  t + 1] * Survival[i, j] * prob_end_season[t]
            
          } else {
            
            
            RewardIfPerformance[i, j, k, t] <- 
              
              (Condition[i, j] * Survival[i, j] * 
                 Fitness[i, min(j + 2, max_Performance), k, t + 1] * 
                 prob_good_temp + Condition[i, j] * Survival[i, j] *
                 Fitness[i, min(j + 1, max_Performance), k, t + 1] *
                 prob_bad_temp + (1 - Condition[i, j]) * Survival[i, j] * 
                 Fitness[i, j, k, t + 1] * prob_good_temp + 
                 (1 - Condition[i, j]) * Survival[i, j] * 
                 Fitness[i, j, k, t + 1] * prob_bad_temp) * 
              prob_no_end_season[t] + 
              Fitness[i, j, k, t + 1] * Survival[i, j] * prob_end_season[t] 
            
            
            RewardIfGrowth[i, j, k, t] <- 
              
              (Condition[i, j] * Survival[i, j] * 
                 Fitness[min(i + 2, max_Size), j, k, t + 1] * prob_good_temp +
                 Condition[i, j] * Survival[i, j] * 
                 Fitness[min(i + 1, max_Size), j, k, t + 1] * prob_bad_temp + 
                 (1 - Condition[i, j]) * Survival[i, j] * 
                 Fitness[i, j, k, t + 1] * prob_good_temp + 
                 (1 - Condition[i, j]) * Survival[i, j] * 
                 Fitness[i, j, k, t + 1] * prob_bad_temp) * 
              prob_no_end_season[t] + 
              Fitness[i, j, k, t + 1] * Survival[i, j] * prob_end_season[t]
            
            
            RewardIfMetamorphosis[i, j, k, t] <-  0
            
            
          } #if/else loop
           
        } # end j loop
        
      } # end i loop
      
    } # end k loop
    
    ForageRule[, , , t] <- 
      
      RewardIfPerformance[, , , t] > RewardIfGrowth[, , , t]
    # TRUE/False matrix that stores if investing in Performance is better 
    # than in Growth.
    
    Fitness[, , , t] <- 
      
      ForageRule[, , , t] * RewardIfPerformance[, , , t] +
      as.numeric(!ForageRule[, , , t]) * RewardIfGrowth[, , , t]
    # Matrix that stores the reward values of the best decision. 
    # It is going to be used in the next time step as a terminal Fitness
    # matrix. 
    
    ForageRule_B[, , , t] <- 
      
      RewardIfMetamorphosis[, , , t] > Fitness[, , , t]
    # This ForageRule_B is used to decide if the decision of
    # starting the metamorphosis is better than the current fitness.
    
      
    Fitness[, , , t] <- 
      
      ForageRule_B[, , , t] * RewardIfMetamorphosis[, , , t] + 
        as.numeric(!ForageRule_B[, , , t]) * Fitness[, , , t]
    # Update of the Fitness matrix with the ForageRule_B results.
    
    
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
  assign("max_Stages", max_Stages, envir = globalenv())
  
  # These lines extract Fitness, ForageRule and other objects from inside the 
  # function to the global environment, so they can be used it in other plots 
  # or in the Forward simulation.
  
  
} # end of Decision function


# Decision plot function

Backwards_Plot <- function(){
  
  par(mfrow=c(3,4))
  par(mar=c(5.1, 4.5, 4.1, 6.5))
  
  t <- time_steps
  
  k <- 1 
  # Which stage would you like to see in the plot. (Normally stage nº 1, 
  # that is when the decision to start metamorphosis is made).
  
  Dead_state <- time_steps - max_Stages + k
  # This is used to know if you are dead or not at the current time step and 
  # stage due that you are not going to be able to reach the minimum size 
  # to metamorphose. 
  
  while (t >= 1) {
    
    
    ForageRule_rev <- apply(ForageRule[, , k, t], 2, rev) # At time step "t"
    ForageRule_B_rev <- apply(ForageRule_B[, , k, t], 2, rev) # At time step "t"
    ForageRule_rev[ForageRule_rev == "FALSE"] <- "Growth"
    ForageRule_rev[ForageRule_rev == "TRUE"] <- "Performance"
    ForageRule_rev[ForageRule_B_rev == "TRUE"] <- "Metamorphosis"
    ForageRule_rev[max_Size, ] <- "Dead"
    
    if (t > Dead_state){
      
      ForageRule_rev[ForageRule_B_rev == "FALSE"] <- "Dead"
      
    }
    
    
    plot(ForageRule_rev, col=c('#440154FF', '#31688EFF', 
                               '#35B779FF', '#FDE725FF'), 
         breaks=c("Dead", "Growth", "Performance", "Metamorphosis"), 
         xlab = "Burst speed (mm/s)", ylab = "Size (cm)",
         main = paste('Decision at time step ', t ), 
         axis.col = NULL, axis.row = NULL)
    axis(1, at = 1:max_Performance, labels = Performance * 10)
    axis(2, at = 1:(max_Size), labels = c(Size), las = 1)
    # This function takes the time step you are in from the ForageRule array and 
    # creates a grid where TRUE/FALSE values from ForageRule and ForageRule_B 
    # are transformed into Dead, Performance, Growth or Metamorphosis labels. 
    
    
    t <- t - 1 
    
  } # End of while loop
  
} # End of Plot loop




# Initial Parameters


prob_good_temp <- 0.5
# Probability of having a good Temperature day.

prob_bad_temp <- 1 - prob_good_temp 
# Probability of having a bad Temperature day.

days <- 60
# How many days does the metamorphosis last in normal conditions.

end_season_percentage <- 0.2
# How many days (% of the normal growing season), 
# beginning from the back, are susceptible to be the end of season 
# due environmental stochasticity.

end_season_intensity <- 1 
# Increasing probability of ending the season in each particular time step 
# since the days start to be susceptible of being the end of the season.

death_rate_day <- 0.012 
# Death rate per day (from 0 to 1).


# Plot

Decisions(prob_good_temp, prob_bad_temp, days, end_season_percentage, 
          end_season_intensity, death_rate_day)


Backwards_Plot()
