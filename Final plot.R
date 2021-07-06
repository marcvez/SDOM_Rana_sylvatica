prob_good_temp <- 0.5
prob_good_temp
prob_bad_temp <- 1 - prob_good_temp
days <- 60
end_season_percentage <- 0.2
end_season_intensity <- 1 
death_rate_day <- 0.012 
N <- 100000




#------------------------- Backwards simulation --------------------------------

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
  
  time_steps <- days + ((prob_bad_temp - 0.5)*30)
  # An environment with higher temperatures is also more likely to dry out
  # earlier and to have a shorter growing season.
  # It works as the developmental rate.
  
  tradeoff_advantage <- prob_bad_temp
  # To simulate trade-off effect of development on performance.
  
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
                               time_steps + 1 + max_Stages + 20))
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
                                  max_Stages, time_steps + 20))
  ForageRule_B <- array(NA, dim = c(max_Size, max_Performance, 
                                    max_Stages, time_steps + 20))
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
          
          if (k == max_Stages) {
            
            RewardIfPerformance[i, j, k, t] <- 0
            
            
            
            RewardIfGrowth[i, j, k, t] <- 0
            
            
            
            RewardIfMetamorphosis[i, j, k, t] <-  
              
              Fitness[i, j, min(k + 1, max_Stages), t + 1]
            
          } # k = max_Stages -> tadpoles become frogs and can't die. 
          
          
          else if (Metamorphosis[i, j] == 1){
            
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
  assign("tradeoff_advantage", tradeoff_advantage, envir = globalenv())
  assign("prob_good_temp", prob_good_temp, envir = globalenv())
  assign("prob_bad_temp", prob_bad_temp, envir = globalenv())
  
  # These lines extract Fitness, ForageRule and other objects from inside the 
  # function to the global environment, so they can be used it in other plots 
  # or in the Forward simulation.
  
  
  
} # end of Decision function

Forward_1 <- function(N) {
  
  Population <- matrix(nrow = N, ncol = time_steps + 3)
  rownames(Population) <- c(1:N)
  colnames(Population) <- c((1:time_steps), "Size", "Performance", "Fitness") 
  # This matrix is going to store the state of each tadpole during the whole
  # period, being 1 the value of being alive and 0, being dead. 
  # It is going to be read by the "Alive" matrix, to determine how many 
  # individuals are still alive at each time step. The last columns store 
  # the Size, Performance and Fitness that tadpoles have at the last time step.
  
  
  Alive <- as.data.frame(matrix(nrow = time_steps + 1, ncol = 2))
  colnames(Alive) <- c("time_step", "N_alive")
  Alive[, 1] <- c(0:time_steps)
  Alive[1, 2] <- N
  # It sums the number of dead or alive individuals at each time step.
  
  
  Tadpole <- c(1:N)
  State <- c("Size", "Performance", "Fitness", "Stage")
  Time <- c(0:time_steps)
  Tadpole_state <- array(NA, dim = c(N, 4, time_steps + 1), dimnames = 
                           list (Tadpole, State, Time))
  # This array stores the Size, Performance and Expected Fitness values for 
  # each tadpole at each time step.
  
  
  Performance_forw <- Performance
  # This object is the same as Performance, going to be modified 
  # during the simulation instead of the original one.
  
  
  Adult <- array(NA, dim = c(N, 6, time_steps + 1))
  # Matrix that is going to store when do the tadpoles start the metamorphosis
  # (1, 2, 3), and when do they finish it (4, 5, 6).
  # It stores the Size (1, 4), the Performance (2, 5) and Fitness (3, 6).
  
  
  for (n in 1:N) {
    
    i <- sample(2:4,1)
    j <- 1  + 10 + ((tradeoff_advantage - 0.5) * 40) # sample(1:3,1)
    k <- 1
    t <- 1
    # Initial conditions for each tadpole
    
    Tadpole_state[n, 1, t] <- Size[i]
    Tadpole_state[n, 2, t] <- Performance[j]
    Tadpole_state[n, 3, t] <- Fitness[i,j,k,t]
    Tadpole_state[n, 4, t] <- k
    # We fix the initial condition in the Tadpole_state array.
    
    Performance_forw[j] <- Performance[j]
    # This object is the same as Performance, going to be modified 
    # during the simulation instead of the original one.
    
    while (t <= time_steps) {
      
      Prob_Survive <- runif(1)
      # Random value that determines if you live or you die.
      
      Temperature <- runif(1)
      # Random value that is going to determine if the day is good or bad, 
      # in terms of temperature.
      
      Forage <- runif(1)
      # Probability of finding food.
      
      
      if (Prob_Survive < Survival[i, j]) {
        
        if (ForageRule_B[i, j, k, t] == FALSE){
          
          if (Forage < Condition[i, j]){
            
            if (Temperature < prob_good_temp) {
              
              # Alive, no Metamorphosis, Food, Food Tº
              
              if (ForageRule[i, j, k, t] == FALSE) {
                
                # Invest in Growth
                
                i <- min(i + 2, max_Size)
                
              } else if (ForageRule[i, j, k, t] == TRUE) {
                
                # Invest in Performance
                
                j <- min(j + 2, max_Performance)
                
              } # if / else G/P
              
            } else {
              
              # Alive, no Metamorphosis, Food, Bad Tº
              
              if (ForageRule[i, j, k, t] == FALSE){
                
                # Invest in Growth
                
                i <- min(i + 1, max_Size)
                
              } else if (ForageRule[i, j, k, t] == TRUE){
                
                # Invest in Performance
                
                j <- min(j + 1, max_Performance)
                
              } # if / else G/P
              
            } # if / else Temperature
            
          } else {
            
            # Alive, no Metamorphosis, no Food
            
            i <- i
            
            j <- j
            
          } # if / else Food
          
          Population[n, t] <- 1
          
          Alive[t + 1, 2] <- sum(Population[, t])
          
          Tadpole_state[n, 1, t + 1] <- Size[i]
          Tadpole_state[n, 2, t + 1] <- Performance_forw[j]
          Tadpole_state[n, 3, t + 1] <- Fitness[i, j, k, t]
          Tadpole_state[n, 4, t + 1] <- k
          
          Population[n, time_steps + 1] <- Size[i]
          Population[n, time_steps + 2] <- Performance_forw[j]
          Population[n, time_steps + 3] <- Fitness[i, j, k, t]
          
          t <- t + 1
          
        } else {
          
          # Alive, Metamorphosis
          
          for (t in t:time_steps){
            
            if (Prob_Survive < Survival[i, j]){
              
              if (k < max_Stages) {
                
                if (Forage < Condition[i, j]) {
                  
                  # Food
                  
                  k <- min(k + 1, max_Stages)
                  
                } else {
                  
                  # No food, no investment
                  
                } # if/else k lower than max_Stages
                
                Prob_Survive <- runif(1)
                
                Forage <- runif(1)
                
              } else {
                
                # k = max_Stages -> frog, it doesn't die
                
                Prob_Survive <- 0
                
              }
              
              Population[n, t] <- 1
              
              Alive[t + 1, 2] <- sum(Population[, t])
              
              Tadpole_state[n, 1, t + 1] <- Size[i]
              Tadpole_state[n, 2, t + 1] <- Performance_forw[j]
              Tadpole_state[n, 3, t + 1] <- Fitness[i, j, k, t]
              Tadpole_state[n, 4, t + 1] <- k
              
              Population[n, time_steps + 1] <- Size[i]
              Population[n, time_steps + 2] <- Performance_forw[j]
              Population[n, time_steps + 3] <- Fitness[i, j, k, t]
              
              if (Tadpole_state[n, 4, t] == 1 & 
                  Tadpole_state[n, 4, t + 1] == 2) {
                
                Adult[n, 1, t] <- Size[i]
                Adult[n, 2, t] <- Performance_forw[j]
                Adult[n, 3, t] <- Fitness[i, j, k, t]
                
              } # Stores the state and time step where metamorphosis starts.
              
              if (Tadpole_state[n, 4, t] == 9 & 
                  Tadpole_state[n, 4, t + 1] == 10) {
                
                Adult[n, 4, t + 1] <- Size[i]
                Adult[n, 5, t + 1] <- Performance_forw[j]
                Adult[n, 6, t + 1] <- Fitness[i, j, k, t]
                
              } # Stores the state and time step where metamorphosis ends.
              
              t <- t + 1
              
            } else {
              
              # Dead while doing Metamorphosis
              
              i <- 1
              j <- 1
              Performance_forw[j] <- 0
              
              Population[n, t] <- 0
              
              Alive[t + 1, 2] <- sum(Population[, t])
              
              Tadpole_state[n, 1, t + 1] <- Size[i]
              Tadpole_state[n, 2, t + 1] <- Performance_forw[j]
              Tadpole_state[n, 3, t + 1] <- Fitness[i, j, k, t]
              Tadpole_state[n, 4, t + 1] <- k
              
              Population[n, time_steps + 1] <- Size[i]
              Population[n, time_steps + 2] <- Performance_forw[j]
              Population[n, time_steps + 3] <- Fitness[i, j, k, t]
              
              Prob_Survive <- 1
              
              t <- t + 1
              
            }
            
          } 
          
        } # if / else Metamorphosis
        
      } else {
        
        # Dead
        
        i <- 1
        j <- 1
        Performance_forw[j] <- 0
        
        Population[n, t] <- 0
        
        Alive[t + 1, 2] <- sum(Population[, t])
        
        Tadpole_state[n, 1, t + 1] <- Size[i]
        Tadpole_state[n, 2, t + 1] <- Performance_forw[j]
        Tadpole_state[n, 3, t + 1] <- Fitness[i, j, k, t]
        Tadpole_state[n, 4, t + 1] <- k
        
        Population[n, time_steps + 1] <- Size[i]
        Population[n, time_steps + 2] <- Performance_forw[j]
        Population[n, time_steps + 3] <- Fitness[i, j, k, t]
        
        Prob_Survive <- 1
        
        t <- t + 1
        
      } # if / else Survival
      
    } # while loop
    
  } # for loop
  
  
  print("How many tadpoles are still alive?")
  print(Alive[time_steps + 1, 2])
  
  assign("Alive", Alive, envir = globalenv())
  assign("Tadpole_state", Tadpole_state, envir = globalenv())
  assign("Population", Population, envir = globalenv())
  Final_Fitness <- (Tadpole_state[, 3, time_steps + 1])
  Final_Size <- (Tadpole_state[, 1, time_steps + 1])
  Final_Performance <- (Tadpole_state[, 2, time_steps + 1])
  Final_results <- cbind(Final_Size, Final_Performance, Final_Fitness)
  Max_Condition <- as.vector(which(Final_results[, 3] == 
                                     max(Final_results[, 3])))
  assign("Final_Fitness", Final_Fitness, envir = globalenv())
  assign("Final_Size", Final_Size, envir = globalenv())
  assign("Final_Performance", Final_Performance, envir = globalenv())
  assign("Final_results", Final_results, envir = globalenv())
  assign("Max_Condition", Max_Condition, envir = globalenv())
  assign("Adult", Adult, envir = globalenv())
  
} # End Forward simulation 1

Forward_2 <- function(N) {
  
  prob_good_temp_forw <- 0.5
  
  development <- prob_good_temp_forw - prob_good_temp
  
  time_steps_forw <- time_steps - ((development * 30) * 2)
  
  if (time_steps < time_steps_forw) {
    
    for (t in time_steps:time_steps_forw){
      
      ForageRule[, , , t] <- ForageRule[, , , time_steps]
      
      ForageRule_B[, , , t] <- ForageRule_B[, , , time_steps]
      
      Fitness[, , , t] <- Fitness[, , , time_steps]
      
    }
    
  }
  
  prob_good_temp <- prob_good_temp_forw
  
  prob_bad_temp <- 1- prob_good_temp
  
  time_steps <- time_steps_forw
  
  
  
  Population <- matrix(nrow = N, ncol = time_steps + 3)
  rownames(Population) <- c(1:N)
  colnames(Population) <- c((1:time_steps), "Size", "Performance", "Fitness") 
  # This matrix is going to store the state of each tadpole during the whole
  # period, being 1 the value of being alive and 0, being dead. 
  # It is going to be read by the "Alive" matrix, to determine how many 
  # individuals are still alive at each time step. The last columns store 
  # the Size, Performance and Fitness that tadpoles have at the last time step.
  
  
  Alive <- as.data.frame(matrix(nrow = time_steps + 1, ncol = 2))
  colnames(Alive) <- c("time_step", "N_alive")
  Alive[, 1] <- c(0:time_steps)
  Alive[1, 2] <- N
  # It sums the number of dead or alive individuals at each time step.
  
  
  Tadpole <- c(1:N)
  State <- c("Size", "Performance", "Fitness", "Stage")
  Time <- c(0:time_steps)
  Tadpole_state <- array(NA, dim = c(N, 4, time_steps + 1), dimnames = 
                           list (Tadpole, State, Time))
  # This array stores the Size, Performance and Expected Fitness values for 
  # each tadpole at each time step.
  
  
  Performance_forw <- Performance
  # This object is the same as Performance, going to be modified 
  # during the simulation instead of the original one.
  
  
  Adult <- array(NA, dim = c(N, 6, time_steps + 1))
  # Matrix that is going to store when do the tadpoles start the metamorphosis
  # (1, 2, 3), and when do they finish it (4, 5, 6).
  # It stores the Size (1, 4), the Performance (2, 5) and Fitness (3, 6).
  
  
  for (n in 1:N) {
    
    i <- sample(2:4,1)
    j <- 1 + 10 + ((tradeoff_advantage - 0.5) * 40)
    k <- 1
    t <- 1
    # Initial conditions for each tadpole
    
    Tadpole_state[n, 1, t] <- Size[i]
    Tadpole_state[n, 2, t] <- Performance[j]
    Tadpole_state[n, 3, t] <- Fitness[i,j,k,t]
    Tadpole_state[n, 4, t] <- k
    # We fix the initial condition in the Tadpole_state array.
    
    Performance_forw[j] <- Performance[j]
    # This object is the same as Performance, going to be modified 
    # during the simulation instead of the original one.
    
    while (t <= time_steps) {
      
      Prob_Survive <- runif(1)
      # Random value that determines if you live or you die.
      
      Temperature <- runif(1)
      # Random value that is going to determine if the day is good or bad, 
      # in terms of temperature.
      
      Forage <- runif(1)
      # Probability of finding food.
      
      
      if (Prob_Survive < Survival[i, j]) {
        
        if (ForageRule_B[i, j, k, t] == FALSE){
          
          if (Forage < Condition[i, j]){
            
            if (Temperature < prob_good_temp) {
              
              # Alive, no Metamorphosis, Food, Food Tº
              
              if (ForageRule[i, j, k, t] == FALSE) {
                
                # Invest in Growth
                
                i <- min(i + 2, max_Size)
                
              } else if (ForageRule[i, j, k, t] == TRUE) {
                
                # Invest in Performance
                
                j <- min(j + 2, max_Performance)
                
              } # if / else G/P
              
            } else {
              
              # Alive, no Metamorphosis, Food, Bad Tº
              
              if (ForageRule[i, j, k, t] == FALSE){
                
                # Invest in Growth
                
                i <- min(i + 1, max_Size)
                
              } else if (ForageRule[i, j, k, t] == TRUE){
                
                # Invest in Performance
                
                j <- min(j + 1, max_Performance)
                
              } # if / else G/P
              
            } # if / else Temperature
            
          } else {
            
            # Alive, no Metamorphosis, no Food
            
            i <- i
            
            j <- j
            
          } # if / else Food
          
          Population[n, t] <- 1
          
          Alive[t + 1, 2] <- sum(Population[, t])
          
          Tadpole_state[n, 1, t + 1] <- Size[i]
          Tadpole_state[n, 2, t + 1] <- Performance_forw[j]
          Tadpole_state[n, 3, t + 1] <- Fitness[i, j, k, t]
          Tadpole_state[n, 4, t + 1] <- k
          
          Population[n, time_steps + 1] <- Size[i]
          Population[n, time_steps + 2] <- Performance_forw[j]
          Population[n, time_steps + 3] <- Fitness[i, j, k, t]
          
          t <- t + 1
          
        } else {
          
          # Alive, Metamorphosis
          
          for (t in t:time_steps){
            
            if (Prob_Survive < Survival[i, j]){
              
              if (k < max_Stages) {
                
                if (Forage < Condition[i, j]) {
                  
                  # Food
                  
                  k <- min(k + 1, max_Stages)
                  
                } else {
                  
                  # No food, no investment
                  
                } # if/else k lower than max_Stages
                
                Prob_Survive <- runif(1)
                
                Forage <- runif(1)
                
              } else {
                
                # k = max_Stages -> frog, it doesn't die
                
                Prob_Survive <- 0
                
              }
              
              Population[n, t] <- 1
              
              Alive[t + 1, 2] <- sum(Population[, t])
              
              Tadpole_state[n, 1, t + 1] <- Size[i]
              Tadpole_state[n, 2, t + 1] <- Performance_forw[j]
              Tadpole_state[n, 3, t + 1] <- Fitness[i, j, k, t]
              Tadpole_state[n, 4, t + 1] <- k
              
              Population[n, time_steps + 1] <- Size[i]
              Population[n, time_steps + 2] <- Performance_forw[j]
              Population[n, time_steps + 3] <- Fitness[i, j, k, t]
              
              if (Tadpole_state[n, 4, t] == 1 & 
                  Tadpole_state[n, 4, t + 1] == 2) {
                
                Adult[n, 1, t] <- Size[i]
                Adult[n, 2, t] <- Performance_forw[j]
                Adult[n, 3, t] <- Fitness[i, j, k, t]
                
              } # Stores the state and time step where metamorphosis starts.
              
              if (Tadpole_state[n, 4, t] == 9 & 
                  Tadpole_state[n, 4, t + 1] == 10) {
                
                Adult[n, 4, t + 1] <- Size[i]
                Adult[n, 5, t + 1] <- Performance_forw[j]
                Adult[n, 6, t + 1] <- Fitness[i, j, k, t]
                
              } # Stores the state and time step where metamorphosis ends.
              
              t <- t + 1
              
            } else {
              
              # Dead while doing Metamorphosis
              
              i <- 1
              j <- 1
              Performance_forw[j] <- 0
              
              Population[n, t] <- 0
              
              Alive[t + 1, 2] <- sum(Population[, t])
              
              Tadpole_state[n, 1, t + 1] <- Size[i]
              Tadpole_state[n, 2, t + 1] <- Performance_forw[j]
              Tadpole_state[n, 3, t + 1] <- Fitness[i, j, k, t]
              Tadpole_state[n, 4, t + 1] <- k
              
              Population[n, time_steps + 1] <- Size[i]
              Population[n, time_steps + 2] <- Performance_forw[j]
              Population[n, time_steps + 3] <- Fitness[i, j, k, t]
              
              Prob_Survive <- 1
              
              t <- t + 1
              
            }
            
          } 
          
        } # if / else Metamorphosis
        
      } else {
        
        # Dead
        
        i <- 1
        j <- 1
        Performance_forw[j] <- 0
        
        Population[n, t] <- 0
        
        Alive[t + 1, 2] <- sum(Population[, t])
        
        Tadpole_state[n, 1, t + 1] <- Size[i]
        Tadpole_state[n, 2, t + 1] <- Performance_forw[j]
        Tadpole_state[n, 3, t + 1] <- Fitness[i, j, k, t]
        Tadpole_state[n, 4, t + 1] <- k
        
        Population[n, time_steps + 1] <- Size[i]
        Population[n, time_steps + 2] <- Performance_forw[j]
        Population[n, time_steps + 3] <- Fitness[i, j, k, t]
        
        Prob_Survive <- 1
        
        t <- t + 1
        
      } # if / else Survival
      
    } # while loop
    
  } # for loop
  
  
  print("How many tadpoles are still alive?")
  print(Alive[time_steps + 1, 2])
  
  assign("Alive", Alive, envir = globalenv())
  assign("Tadpole_state", Tadpole_state, envir = globalenv())
  assign("Population", Population, envir = globalenv())
  Final_Fitness <- (Tadpole_state[, 3, time_steps + 1])
  Final_Size <- (Tadpole_state[, 1, time_steps + 1])
  Final_Performance <- (Tadpole_state[, 2, time_steps + 1])
  Final_results <- cbind(Final_Size, Final_Performance, Final_Fitness)
  Max_Condition <- as.vector(which(Final_results[, 3] == 
                                     max(Final_results[, 3])))
  assign("Final_Fitness", Final_Fitness, envir = globalenv())
  assign("Final_Size", Final_Size, envir = globalenv())
  assign("Final_Performance", Final_Performance, envir = globalenv())
  assign("Final_results", Final_results, envir = globalenv())
  assign("Max_Condition", Max_Condition, envir = globalenv())
  assign("Adult", Adult, envir = globalenv())
  assign("time_steps", time_steps, envir = globalenv())
  
} # End Forward simulation 2

Forward_3 <- function(N) {
  
  prob_good_temp_forw <- prob_good_temp + 0.1
  
  development <- prob_good_temp_forw - prob_good_temp
  
  time_steps_forw <- time_steps - ((development * 30) * 2)
  
  if (time_steps < time_steps_forw) {
    
    for (t in time_steps:time_steps_forw){
      
      ForageRule[, , , t] <- ForageRule[, , , time_steps]
      
      ForageRule_B[, , , t] <- ForageRule_B[, , , time_steps]
      
      Fitness[, , , t] <- Fitness[, , , time_steps]
      
    }
    
  }
  
  prob_good_temp <- prob_good_temp_forw
  
  prob_bad_temp <- 1- prob_good_temp
  
  time_steps <- time_steps_forw
  
  Population <- matrix(nrow = N, ncol = time_steps + 3)
  rownames(Population) <- c(1:N)
  colnames(Population) <- c((1:time_steps), "Size", "Performance", "Fitness") 
  # This matrix is going to store the state of each tadpole during the whole
  # period, being 1 the value of being alive and 0, being dead. 
  # It is going to be read by the "Alive" matrix, to determine how many 
  # individuals are still alive at each time step. The last columns store 
  # the Size, Performance and Fitness that tadpoles have at the last time step.
  
  
  Alive <- as.data.frame(matrix(nrow = time_steps + 1, ncol = 2))
  colnames(Alive) <- c("time_step", "N_alive")
  Alive[, 1] <- c(0:time_steps)
  Alive[1, 2] <- N
  # It sums the number of dead or alive individuals at each time step.
  
  
  Tadpole <- c(1:N)
  State <- c("Size", "Performance", "Fitness", "Stage")
  Time <- c(0:time_steps)
  Tadpole_state <- array(NA, dim = c(N, 4, time_steps + 1), dimnames = 
                           list (Tadpole, State, Time))
  # This array stores the Size, Performance and Expected Fitness values for 
  # each tadpole at each time step.
  
  
  Performance_forw <- Performance
  # This object is the same as Performance, going to be modified 
  # during the simulation instead of the original one.
  
  
  Adult <- array(NA, dim = c(N, 6, time_steps + 1))
  # Matrix that is going to store when do the tadpoles start the metamorphosis
  # (1, 2, 3), and when do they finish it (4, 5, 6).
  # It stores the Size (1, 4), the Performance (2, 5) and Fitness (3, 6).
  
  
  for (n in 1:N) {
    
    i <- sample(2:4,1)
    j <- 1  + 10 + ((tradeoff_advantage - 0.5) * 40) # sample(1:3,1)
    k <- 1
    t <- 1
    # Initial conditions for each tadpole
    
    Tadpole_state[n, 1, t] <- Size[i]
    Tadpole_state[n, 2, t] <- Performance[j]
    Tadpole_state[n, 3, t] <- Fitness[i,j,k,t]
    Tadpole_state[n, 4, t] <- k
    # We fix the initial condition in the Tadpole_state array.
    
    Performance_forw[j] <- Performance[j]
    # This object is the same as Performance, going to be modified 
    # during the simulation instead of the original one.
    
    while (t <= time_steps) {
      
      Prob_Survive <- runif(1)
      # Random value that determines if you live or you die.
      
      Temperature <- runif(1)
      # Random value that is going to determine if the day is good or bad, 
      # in terms of temperature.
      
      Forage <- runif(1)
      # Probability of finding food.
      
      
      if (Prob_Survive < Survival[i, j]) {
        
        if (ForageRule_B[i, j, k, t] == FALSE){
          
          if (Forage < Condition[i, j]){
            
            if (Temperature < prob_good_temp) {
              
              # Alive, no Metamorphosis, Food, Food Tº
              
              if (ForageRule[i, j, k, t] == FALSE) {
                
                # Invest in Growth
                
                i <- min(i + 2, max_Size)
                
              } else if (ForageRule[i, j, k, t] == TRUE) {
                
                # Invest in Performance
                
                j <- min(j + 2, max_Performance)
                
              } # if / else G/P
              
            } else {
              
              # Alive, no Metamorphosis, Food, Bad Tº
              
              if (ForageRule[i, j, k, t] == FALSE){
                
                # Invest in Growth
                
                i <- min(i + 1, max_Size)
                
              } else if (ForageRule[i, j, k, t] == TRUE){
                
                # Invest in Performance
                
                j <- min(j + 1, max_Performance)
                
              } # if / else G/P
              
            } # if / else Temperature
            
          } else {
            
            # Alive, no Metamorphosis, no Food
            
            i <- i
            
            j <- j
            
          } # if / else Food
          
          Population[n, t] <- 1
          
          Alive[t + 1, 2] <- sum(Population[, t])
          
          Tadpole_state[n, 1, t + 1] <- Size[i]
          Tadpole_state[n, 2, t + 1] <- Performance_forw[j]
          Tadpole_state[n, 3, t + 1] <- Fitness[i, j, k, t]
          Tadpole_state[n, 4, t + 1] <- k
          
          Population[n, time_steps + 1] <- Size[i]
          Population[n, time_steps + 2] <- Performance_forw[j]
          Population[n, time_steps + 3] <- Fitness[i, j, k, t]
          
          t <- t + 1
          
        } else {
          
          # Alive, Metamorphosis
          
          for (t in t:time_steps){
            
            if (Prob_Survive < Survival[i, j]){
              
              if (k < max_Stages) {
                
                if (Forage < Condition[i, j]) {
                  
                  # Food
                  
                  k <- min(k + 1, max_Stages)
                  
                } else {
                  
                  # No food, no investment
                  
                } # if/else k lower than max_Stages
                
                Prob_Survive <- runif(1)
                
                Forage <- runif(1)
                
              } else {
                
                # k = max_Stages -> frog, it doesn't die
                
                Prob_Survive <- 0
                
              }
              
              Population[n, t] <- 1
              
              Alive[t + 1, 2] <- sum(Population[, t])
              
              Tadpole_state[n, 1, t + 1] <- Size[i]
              Tadpole_state[n, 2, t + 1] <- Performance_forw[j]
              Tadpole_state[n, 3, t + 1] <- Fitness[i, j, k, t]
              Tadpole_state[n, 4, t + 1] <- k
              
              Population[n, time_steps + 1] <- Size[i]
              Population[n, time_steps + 2] <- Performance_forw[j]
              Population[n, time_steps + 3] <- Fitness[i, j, k, t]
              
              if (Tadpole_state[n, 4, t] == 1 & 
                  Tadpole_state[n, 4, t + 1] == 2) {
                
                Adult[n, 1, t] <- Size[i]
                Adult[n, 2, t] <- Performance_forw[j]
                Adult[n, 3, t] <- Fitness[i, j, k, t]
                
              } # Stores the state and time step where metamorphosis starts.
              
              if (Tadpole_state[n, 4, t] == 9 & 
                  Tadpole_state[n, 4, t + 1] == 10) {
                
                Adult[n, 4, t + 1] <- Size[i]
                Adult[n, 5, t + 1] <- Performance_forw[j]
                Adult[n, 6, t + 1] <- Fitness[i, j, k, t]
                
              } # Stores the state and time step where metamorphosis ends.
              
              t <- t + 1
              
            } else {
              
              # Dead while doing Metamorphosis
              
              i <- 1
              j <- 1
              Performance_forw[j] <- 0
              
              Population[n, t] <- 0
              
              Alive[t + 1, 2] <- sum(Population[, t])
              
              Tadpole_state[n, 1, t + 1] <- Size[i]
              Tadpole_state[n, 2, t + 1] <- Performance_forw[j]
              Tadpole_state[n, 3, t + 1] <- Fitness[i, j, k, t]
              Tadpole_state[n, 4, t + 1] <- k
              
              Population[n, time_steps + 1] <- Size[i]
              Population[n, time_steps + 2] <- Performance_forw[j]
              Population[n, time_steps + 3] <- Fitness[i, j, k, t]
              
              Prob_Survive <- 1
              
              t <- t + 1
              
            }
            
          } 
          
        } # if / else Metamorphosis
        
      } else {
        
        # Dead
        
        i <- 1
        j <- 1
        Performance_forw[j] <- 0
        
        Population[n, t] <- 0
        
        Alive[t + 1, 2] <- sum(Population[, t])
        
        Tadpole_state[n, 1, t + 1] <- Size[i]
        Tadpole_state[n, 2, t + 1] <- Performance_forw[j]
        Tadpole_state[n, 3, t + 1] <- Fitness[i, j, k, t]
        Tadpole_state[n, 4, t + 1] <- k
        
        Population[n, time_steps + 1] <- Size[i]
        Population[n, time_steps + 2] <- Performance_forw[j]
        Population[n, time_steps + 3] <- Fitness[i, j, k, t]
        
        Prob_Survive <- 1
        
        t <- t + 1
        
      } # if / else Survival
      
    } # while loop
    
  } # for loop
  
  
  print("How many tadpoles are still alive?")
  print(Alive[time_steps + 1, 2])
  
  assign("Alive", Alive, envir = globalenv())
  assign("Tadpole_state", Tadpole_state, envir = globalenv())
  assign("Population", Population, envir = globalenv())
  Final_Fitness <- (Tadpole_state[, 3, time_steps + 1])
  Final_Size <- (Tadpole_state[, 1, time_steps + 1])
  Final_Performance <- (Tadpole_state[, 2, time_steps + 1])
  Final_results <- cbind(Final_Size, Final_Performance, Final_Fitness)
  Max_Condition <- as.vector(which(Final_results[, 3] == 
                                     max(Final_results[, 3])))
  assign("Final_Fitness", Final_Fitness, envir = globalenv())
  assign("Final_Size", Final_Size, envir = globalenv())
  assign("Final_Performance", Final_Performance, envir = globalenv())
  assign("Final_results", Final_results, envir = globalenv())
  assign("Max_Condition", Max_Condition, envir = globalenv())
  assign("Adult", Adult, envir = globalenv())
  assign("time_steps", time_steps, envir = globalenv())
  
} # End Forward simulation 3





Final_plot <- function(){
  
  
  par(mfrow=c(3, 3))
  par(mar=c(5.1, 4.5, 3, 4.5))
 
  # ---------------------------------------------------------------------
  
  
  
  
  
  # ----------------------------------------------------------------------
  
  prob_good_temp <- 0.6
  prob_good_temp
  prob_bad_temp <- 1 - prob_good_temp
  days <- 60
  end_season_percentage <- 0.2
  end_season_intensity <- 1 
  death_rate_day <- 0.012 
  N <- 10000
 
  
  Decisions(prob_good_temp, prob_bad_temp, days, end_season_percentage, 
              end_season_intensity, death_rate_day)
    
    
  Forward_1(N)
  
  Size_bigger_0 <- as.vector(Final_Size[Final_Size > 0])
  
  plot(density(Final_results[,1], bw = 0.1, from = -0.5, 
               to = max(Size) + 0.3), col = "red",
       main = "Final Size (cm)",  ylim = c(0, 3))
  abline(v = mean(Size_bigger_0), col = "red")
  
  minor.tick(nx=10, ny=1)
  
  rm(list=ls())
  
  
  
  
  
  
  
  
  
  prob_good_temp <- 0.4
  prob_good_temp
  prob_bad_temp <- 1 - prob_good_temp
  days <- 60
  end_season_percentage <- 0.2
  end_season_intensity <- 1 
  death_rate_day <- 0.012 
  N <- 10000
  
  
  Decisions(prob_good_temp, prob_bad_temp, days, end_season_percentage, 
              end_season_intensity, death_rate_day)
  
  
  Forward_1(N)
  
  Size_bigger_0 <- as.vector(Final_Size[Final_Size > 0])
  
  lines(density(Final_results[,1], bw = 0.1, from = -0.5, 
               to = max(Size) + 0.3), col = "blue")
  abline(v = mean(Size_bigger_0), col = "blue")
  
  rm(list=ls())
  
  
  
  
  
  
  prob_good_temp <- 0.5
  prob_good_temp
  prob_bad_temp <- 1 - prob_good_temp
  days <- 60
  end_season_percentage <- 0.2
  end_season_intensity <- 1 
  death_rate_day <- 0.012 
  N <- 10000
  
  
  Decisions(prob_good_temp, prob_bad_temp, days, end_season_percentage, 
              end_season_intensity, death_rate_day)
  
  
  Forward_1(N)
  
  Size_bigger_0 <- as.vector(Final_Size[Final_Size > 0])
  
  lines(density(Final_results[,1], bw = 0.1, from = -0.5, 
                to = max(Size) + 0.3), col = "black")
  abline(v = mean(Size_bigger_0), col = "black")
  
  rm(list=ls())
  
  # ---------------------------------------------------------------------
  
  
  prob_good_temp <- 0.6
  prob_good_temp
  prob_bad_temp <- 1 - prob_good_temp
  days <- 60
  end_season_percentage <- 0.2
  end_season_intensity <- 1 
  death_rate_day <- 0.012 
  N <- 10000
  
  
  Decisions(prob_good_temp, prob_bad_temp, days, end_season_percentage, 
              end_season_intensity, death_rate_day)
  
  
  Forward_1(N)
  
  Performance_bigger_0 <- as.vector(Final_Performance[Final_Performance > 0])
  
  plot(density(Final_results[,2], bw = 0.1, from = -0.5, 
               to = max(Performance) + 0.3), col = "red",
       main = "Final Burst Speed (cm/s)",  ylim = c(0, 3))
  abline(v = mean(Performance_bigger_0), col = "red")
  
  minor.tick(nx=10, ny=1)
  
  rm(list=ls())
  
  
  
  
  
  
  
  
  
  prob_good_temp <- 0.4
  prob_good_temp
  prob_bad_temp <- 1 - prob_good_temp
  days <- 60
  end_season_percentage <- 0.2
  end_season_intensity <- 1 
  death_rate_day <- 0.012 
  N <- 10000
  
  
  Decisions(prob_good_temp, prob_bad_temp, days, end_season_percentage, 
              end_season_intensity, death_rate_day)
  
  
  Forward_1(N)
  
  Performance_bigger_0 <- as.vector(Final_Performance[Final_Performance > 0])
  
  lines(density(Final_results[,2], bw = 0.1, from = -0.5, 
                to = max(Performance) + 0.3), col = "blue")
  abline(v = mean(Performance_bigger_0), col = "blue")
  
  rm(list=ls())
  
  
  
  
  
  
  
  
  
  prob_good_temp <- 0.5
  prob_good_temp
  prob_bad_temp <- 1 - prob_good_temp
  days <- 60
  end_season_percentage <- 0.2
  end_season_intensity <- 1 
  death_rate_day <- 0.012 
  N <- 10000
  
  
  Decisions(prob_good_temp, prob_bad_temp, days, end_season_percentage, 
              end_season_intensity, death_rate_day)
  
  
  Forward_1(N)
  
  Performance_bigger_0 <- as.vector(Final_Performance[Final_Performance > 0])
  
  lines(density(Final_results[,2], bw = 0.1, from = -0.5, 
                to = max(Performance) + 0.3), col = "black")
  abline(v = mean(Performance_bigger_0), col = "black")
  
  rm(list=ls())
    
    
  # ------------------------------------------------------------------
  
  prob_good_temp <- 0.6
  prob_good_temp
  prob_bad_temp <- 1 - prob_good_temp
  days <- 60
  end_season_percentage <- 0.2
  end_season_intensity <- 1 
  death_rate_day <- 0.012 
  N <- 10000
  
  
  Decisions(prob_good_temp, prob_bad_temp, days, end_season_percentage, 
              end_season_intensity, death_rate_day)
  
  
  Forward_1(N)
  
  Fitness_bigger_0 <- as.vector(Final_Fitness[Final_Fitness > 0])
  
  plot(density(Final_results[,3], bw = 0.1, from = -0.5, 
               to = max(Fitness_values) + 0.3), col = "red",
       main = "Final Fitness",  ylim = c(0, 3))
  abline(v = mean(Fitness_bigger_0), col = "red")
  
  minor.tick(nx=10, ny=1)
  
  rm(list=ls())
  
  
  
  
  
  
  
  
  
  prob_good_temp <- 0.4
  prob_good_temp
  prob_bad_temp <- 1 - prob_good_temp
  days <- 60
  end_season_percentage <- 0.2
  end_season_intensity <- 1 
  death_rate_day <- 0.012 
  N <- 10000
  
  
  Decisions(prob_good_temp, prob_bad_temp, days, end_season_percentage, 
              end_season_intensity, death_rate_day)
  
  
  Forward_1(N)
  
  Fitness_bigger_0 <- as.vector(Final_Fitness[Final_Fitness > 0])
  
  lines(density(Final_results[,3], bw = 0.1, from = -0.5, 
                to = max(Fitness_values) + 0.3), col = "blue")
  abline(v = mean(Fitness_bigger_0), col = "blue")
  
  rm(list=ls())
  
  
  
  
  
  
  prob_good_temp <- 0.5
  prob_good_temp
  prob_bad_temp <- 1 - prob_good_temp
  days <- 60
  end_season_percentage <- 0.2
  end_season_intensity <- 1 
  death_rate_day <- 0.012 
  N <- 10000
  
  
  Decisions(prob_good_temp, prob_bad_temp, days, end_season_percentage, 
              end_season_intensity, death_rate_day)
  
  
  Forward_1(N)
  
  Fitness_bigger_0 <- as.vector(Final_Fitness[Final_Fitness > 0])
  
  lines(density(Final_results[,3], bw = 0.1, from = -0.5, 
                to = max(Fitness_values) + 0.3), col = "black")
  abline(v = mean(Fitness_bigger_0), col = "black")
  
  rm(list=ls())
  
  
  # ----------------------------------------------------------------------
  
  
  
  
  
  
  
  
  
  
  # -----------------------------------------------------------------------
  
  
  prob_good_temp <- 0.6
  prob_good_temp
  prob_bad_temp <- 1 - prob_good_temp
  days <- 60
  end_season_percentage <- 0.2
  end_season_intensity <- 1 
  death_rate_day <- 0.012 
  N <- 10000
  
  
  Decisions(prob_good_temp, prob_bad_temp, days, end_season_percentage, 
              end_season_intensity, death_rate_day)
  
  
  Forward_2(N)
  
  Size_bigger_0 <- as.vector(Final_Size[Final_Size > 0])
  
  plot(density(Final_results[,1], bw = 0.1, from = -0.5, 
               to = max(Size) + 0.3), col = "red",
       main = "Final Size (cm)",  ylim = c(0, 3))
  abline(v = mean(Size_bigger_0), col = "red")
  
  minor.tick(nx=10, ny=1)
  
  rm(list=ls())
  
  
  
  
  
  
  
  prob_good_temp <- 0.4
  prob_good_temp
  prob_bad_temp <- 1 - prob_good_temp
  days <- 60
  end_season_percentage <- 0.2
  end_season_intensity <- 1 
  death_rate_day <- 0.012 
  N <- 10000
  
  
  Decisions(prob_good_temp, prob_bad_temp, days, end_season_percentage, 
              end_season_intensity, death_rate_day)
  
  
  Forward_2(N)
  
  Size_bigger_0 <- as.vector(Final_Size[Final_Size > 0])
  
  lines(density(Final_results[,1], bw = 0.1, from = -0.5, 
                to = max(Size) + 0.3), col = "blue")
  abline(v = mean(Size_bigger_0), col = "blue")
  
  rm(list=ls())
  
  
  
  
  
  
  
  prob_good_temp <- 0.5
  prob_good_temp
  prob_bad_temp <- 1 - prob_good_temp
  days <- 60
  end_season_percentage <- 0.2
  end_season_intensity <- 1 
  death_rate_day <- 0.012 
  N <- 10000
  
  
  Decisions(prob_good_temp, prob_bad_temp, days, end_season_percentage, 
              end_season_intensity, death_rate_day)
  
  
  Forward_2(N)
  
  Size_bigger_0 <- as.vector(Final_Size[Final_Size > 0])
  
  lines(density(Final_results[,1], bw = 0.1, from = -0.5, 
                to = max(Size) + 0.3), col = "black")
  abline(v = mean(Size_bigger_0), col = "black")
  
  rm(list=ls())
  
  
  # ---------------------------------------------------------------------
  
  
  prob_good_temp <- 0.6
  prob_good_temp
  prob_bad_temp <- 1 - prob_good_temp
  days <- 60
  end_season_percentage <- 0.2
  end_season_intensity <- 1 
  death_rate_day <- 0.012 
  N <- 10000
  
  
  Decisions(prob_good_temp, prob_bad_temp, days, end_season_percentage, 
              end_season_intensity, death_rate_day)
  
  
  Forward_2(N)
  
  Performance_bigger_0 <- as.vector(Final_Performance[Final_Performance > 0])
  
  plot(density(Final_results[,2], bw = 0.1, from = -0.5, 
               to = max(Performance) + 0.3), col = "red",
       main = "Final Burst Speed (cm/s)",  ylim = c(0, 3))
  abline(v = mean(Performance_bigger_0), col = "red")
  
  minor.tick(nx=10, ny=1)
  
  rm(list=ls())
  
  
  
  
  
  
  
  
  prob_good_temp <- 0.4
  prob_good_temp
  prob_bad_temp <- 1 - prob_good_temp
  days <- 60
  end_season_percentage <- 0.2
  end_season_intensity <- 1 
  death_rate_day <- 0.012 
  N <- 10000
  
 
  Decisions(prob_good_temp, prob_bad_temp, days, end_season_percentage, 
              end_season_intensity, death_rate_day)
  
  
  Forward_2(N)
  
  Performance_bigger_0 <- as.vector(Final_Performance[Final_Performance > 0])
  
  lines(density(Final_results[,2], bw = 0.1, from = -0.5, 
                to = max(Performance) + 0.3), col = "blue")
  abline(v = mean(Performance_bigger_0), col = "blue")
  
  rm(list=ls())
  
  
  
  
  
  
  
  
  
  prob_good_temp <- 0.5
  prob_good_temp
  prob_bad_temp <- 1 - prob_good_temp
  days <- 60
  end_season_percentage <- 0.2
  end_season_intensity <- 1 
  death_rate_day <- 0.012 
  N <- 10000
  
  
  Decisions(prob_good_temp, prob_bad_temp, days, end_season_percentage, 
              end_season_intensity, death_rate_day)
  
  
  Forward_2(N)
  
  Performance_bigger_0 <- as.vector(Final_Performance[Final_Performance > 0])
  
  lines(density(Final_results[,2], bw = 0.1, from = -0.5, 
                to = max(Performance) + 0.3), col = "black")
  abline(v = mean(Performance_bigger_0), col = "black")
  
  rm(list=ls())
  
  # ------------------------------------------------------------------
  
  prob_good_temp <- 0.6
  prob_good_temp
  prob_bad_temp <- 1 - prob_good_temp
  days <- 60
  end_season_percentage <- 0.2
  end_season_intensity <- 1 
  death_rate_day <- 0.012 
  N <- 10000
  
  
  Decisions(prob_good_temp, prob_bad_temp, days, end_season_percentage, 
              end_season_intensity, death_rate_day)
  
  
  Forward_2(N)
  
  Fitness_bigger_0 <- as.vector(Final_Fitness[Final_Fitness > 0])
  
  plot(density(Final_results[,3], bw = 0.1, from = -0.5, 
               to = max(Fitness_values) + 0.3), col = "red",
       main = "Final Fitness",  ylim = c(0, 3))
  abline(v = mean(Fitness_bigger_0), col = "red")
  
  minor.tick(nx=10, ny=1)
  
  rm(list=ls())
  
  
  
  
  
  
  
  
  prob_good_temp <- 0.4
  prob_good_temp
  prob_bad_temp <- 1 - prob_good_temp
  days <- 60
  end_season_percentage <- 0.2
  end_season_intensity <- 1 
  death_rate_day <- 0.012 
  N <- 10000
  
  
  Decisions(prob_good_temp, prob_bad_temp, days, end_season_percentage, 
              end_season_intensity, death_rate_day)
  
  
  Forward_2(N)
  
  Fitness_bigger_0 <- as.vector(Final_Fitness[Final_Fitness > 0])
  
  lines(density(Final_results[,3], bw = 0.1, from = -0.5, 
                to = max(Fitness_values) + 0.3), col = "blue")
  abline(v = mean(Fitness_bigger_0), col = "blue")
  
  rm(list=ls())
  
  
  
  
  
  
  
  
  
  prob_good_temp <- 0.5
  prob_good_temp
  prob_bad_temp <- 1 - prob_good_temp
  days <- 60
  end_season_percentage <- 0.2
  end_season_intensity <- 1 
  death_rate_day <- 0.012 
  N <- 10000
  
  
  Decisions(prob_good_temp, prob_bad_temp, days, end_season_percentage, 
              end_season_intensity, death_rate_day)
  
  
  Forward_2(N)
  
  Fitness_bigger_0 <- as.vector(Final_Fitness[Final_Fitness > 0])
  
  lines(density(Final_results[,3], bw = 0.1, from = -0.5, 
                to = max(Fitness_values) + 0.3), col = "black")
  abline(v = mean(Fitness_bigger_0), col = "black")
  
  rm(list=ls())
  
  
  # ----------------------------------------------------------------------
  
  
  
  
  
  
  
  
  
  
  # -----------------------------------------------------------------------
  
  
  prob_good_temp <- 0.6
  prob_good_temp
  prob_bad_temp <- 1 - prob_good_temp
  days <- 60
  end_season_percentage <- 0.4
  end_season_intensity <- 1 
  death_rate_day <- 0.012 
  N <- 10000
  
  
  Decisions(prob_good_temp, prob_bad_temp, days, end_season_percentage, 
              end_season_intensity, death_rate_day)
  
  
  Forward_3(N)
  
  Size_bigger_0 <- as.vector(Final_Size[Final_Size > 0])
  
  plot(density(Final_results[,1], bw = 0.1, from = -0.5, 
               to = max(Size) + 0.3), col = "red",
       main = "Final Size (cm)",  ylim = c(0, 3))
  abline(v = mean(Size_bigger_0), col = "red")
  
  minor.tick(nx=10, ny=1)
  
  rm(list=ls())
  
  
  
  
  
  
  
  
  prob_good_temp <- 0.4
  prob_good_temp
  prob_bad_temp <- 1 - prob_good_temp
  days <- 60
  end_season_percentage <- 0.4
  end_season_intensity <- 1 
  death_rate_day <- 0.012 
  N <- 10000
  
  
  Decisions(prob_good_temp, prob_bad_temp, days, end_season_percentage, 
              end_season_intensity, death_rate_day)
  
  
  Forward_3(N)
  
  Size_bigger_0 <- as.vector(Final_Size[Final_Size > 0])
  
  lines(density(Final_results[,1], bw = 0.1, from = -0.5, 
                to = max(Size) + 0.3), col = "blue")
  abline(v = mean(Size_bigger_0), col = "blue")
  
  rm(list=ls())
  
  
  
  
  
  
  
  
  
  prob_good_temp <- 0.5
  prob_good_temp
  prob_bad_temp <- 1 - prob_good_temp
  days <- 60
  end_season_percentage <- 0.4
  end_season_intensity <- 1 
  death_rate_day <- 0.012 
  N <- 10000
  
  
  Decisions(prob_good_temp, prob_bad_temp, days, end_season_percentage, 
              end_season_intensity, death_rate_day)
  
  
  Forward_3(N)
  
  Size_bigger_0 <- as.vector(Final_Size[Final_Size > 0])
  
  lines(density(Final_results[,1], bw = 0.1, from = -0.5, 
                to = max(Size) + 0.3), col = "black")
  abline(v = mean(Size_bigger_0), col = "black")
  
  rm(list=ls())
  
  
  # ---------------------------------------------------------------------
  
  
  prob_good_temp <- 0.6
  prob_good_temp
  prob_bad_temp <- 1 - prob_good_temp
  days <- 60
  end_season_percentage <- 0.4
  end_season_intensity <- 1 
  death_rate_day <- 0.012 
  N <- 10000
  

  Decisions(prob_good_temp, prob_bad_temp, days, end_season_percentage, 
              end_season_intensity, death_rate_day)
  
  
  Forward_3(N)
  
  Performance_bigger_0 <- as.vector(Final_Performance[Final_Performance > 0])
  
  plot(density(Final_results[,2], bw = 0.1, from = -0.5, 
               to = max(Performance) + 0.3), col = "red",
       main = "Final Burst Speed (cm/s)",  ylim = c(0, 3))
  abline(v = mean(Performance_bigger_0), col = "red")
  
  minor.tick(nx=10, ny=1)
  
  rm(list=ls())
  
  
  
  
  
  
  
  
  
  prob_good_temp <- 0.4
  prob_good_temp
  prob_bad_temp <- 1 - prob_good_temp
  days <- 60
  end_season_percentage <- 0.4
  end_season_intensity <- 1 
  death_rate_day <- 0.012 
  N <- 10000
  
  
  Decisions(prob_good_temp, prob_bad_temp, days, end_season_percentage, 
              end_season_intensity, death_rate_day)
  
  
  Forward_3(N)
  
  Performance_bigger_0 <- as.vector(Final_Performance[Final_Performance > 0])
  
  lines(density(Final_results[,2], bw = 0.1, from = -0.5, 
                to = max(Performance) + 0.3), col = "blue")
  abline(v = mean(Performance_bigger_0), col = "blue")
  
  rm(list=ls())
  
  
  
  
  
  
  
  
  prob_good_temp <- 0.5
  prob_good_temp
  prob_bad_temp <- 1 - prob_good_temp
  days <- 60
  end_season_percentage <- 0.4
  end_season_intensity <- 1 
  death_rate_day <- 0.012 
  N <- 10000
  
  
  Decisions(prob_good_temp, prob_bad_temp, days, end_season_percentage, 
              end_season_intensity, death_rate_day)
  
  
  Forward_3(N)
  
  Performance_bigger_0 <- as.vector(Final_Performance[Final_Performance > 0])
  
  lines(density(Final_results[,2], bw = 0.1, from = -0.5, 
                to = max(Performance) + 0.3), col = "black")
  abline(v = mean(Performance_bigger_0), col = "black")
  
  rm(list=ls())
  
  
  # ------------------------------------------------------------------
  
  prob_good_temp <- 0.6
  prob_good_temp
  prob_bad_temp <- 1 - prob_good_temp
  days <- 60
  end_season_percentage <- 0.4
  end_season_intensity <- 1 
  death_rate_day <- 0.012 
  N <- 10000
  
  
  Decisions(prob_good_temp, prob_bad_temp, days, end_season_percentage, 
              end_season_intensity, death_rate_day)
  
  
  Forward_3(N)
  
  Fitness_bigger_0 <- as.vector(Final_Fitness[Final_Fitness > 0])
  
  plot(density(Final_results[,3], bw = 0.1, from = -0.5, 
               to = max(Fitness_values) + 0.3), col = "red",
       main = "Final Fitness",  ylim = c(0, 3))
  abline(v = mean(Fitness_bigger_0), col = "red")
  
  minor.tick(nx=10, ny=1)
  
  rm(list=ls())
  
  
  
  
  
  
  
  
  
  prob_good_temp <- 0.4
  prob_good_temp
  prob_bad_temp <- 1 - prob_good_temp
  days <- 60
  end_season_percentage <- 0.4
  end_season_intensity <- 1 
  death_rate_day <- 0.012 
  N <- 10000
  
 
  Decisions(prob_good_temp, prob_bad_temp, days, end_season_percentage, 
              end_season_intensity, death_rate_day)
  
  
  Forward_3(N)
  
  Fitness_bigger_0 <- as.vector(Final_Fitness[Final_Fitness > 0])
  
  lines(density(Final_results[,3], bw = 0.1, from = -0.5, 
                to = max(Fitness_values) + 0.3), col = "blue")
  abline(v = mean(Fitness_bigger_0), col = "blue")
  
  rm(list=ls())
  
  
  
  
  
  
  
  
  
  prob_good_temp <- 0.5
  prob_good_temp
  prob_bad_temp <- 1 - prob_good_temp
  days <- 60
  end_season_percentage <- 0.4
  end_season_intensity <- 1 
  death_rate_day <- 0.012 
  N <- 10000
  
  
  Decisions(prob_good_temp, prob_bad_temp, days, end_season_percentage, 
              end_season_intensity, death_rate_day)
  
  
  Forward_3(N)
  
  Fitness_bigger_0 <- as.vector(Final_Fitness[Final_Fitness > 0])
  
  lines(density(Final_results[,3], bw = 0.1, from = -0.5, 
                to = max(Fitness_values) + 0.3), col = "black")
  abline(v = mean(Fitness_bigger_0), col = "black")
  
  rm(list=ls())
  
  
  # ----------------------------------------------------------------------
  
  
  
  

}




Final_plot()

