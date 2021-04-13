

#---------------------Backwards model-----------------------

# In this model you either invest in either or you die, it's a simpler model. 
# To do this, we will use a matrix where the y-axis contains the different units 
# of energy invested in growth, and the x-axis contains the units of energy 
# invested in performance. The content of each cell will be the Survival value 
# of that combination of traits. 
# The function should see how Fitness increases with respect to the previous 
# state by deciding whether to invest in growth (by moving down one cell 
# in the matrix) or in performance (by moving one cell to the right). It will then 
# calculate the Fitness increase of the two decisions and choose the best option.

library(plot.matrix)

# Idea 2: Jump one Fitness state if Tº is good.If Tº is bad, you don't receive 
# any benefit from investing in growth

Decisions <- function (prob_good_temp, prob_bad_temp, effect_good_temp, effect_bad_temp, time_steps) {
  
  
  # Life history values (from here to "Loop" can be removed from inside the function)
  
  Performance <- seq(0.7, 0.975, 0.025)
  max_Performance <- length(Performance)
  # Performance values, depends on performance (You have to invest in performance to increase this trait)
  
  Size <- c(0, seq(1, 5.5, 0.1))
  max_Size <- length(Size)
  # Size values, you have to invest in growth to increase this trait. Also, this is the only trait that
  # is relevant for the final Survival (The bigger, the better)
  
  
  Fitness_values <- c(0, seq(1, 5.5, 0.1))
  max_Fitness <- length(Fitness_values)
  Fitness_values[Fitness_values < 4] <- 0
  Fitness_values[Fitness_values >=4 & Fitness_values < 5] <- seq(2, 3.8, 2/(length(Fitness_values[Fitness_values >=4 & Fitness_values < 5])))
  Fitness_values[Fitness_values >= 5] <- c(4, 4.1, 4.15, 4.15, 4.15, 4.15)
  # Sizes under 4 cm don't receive Survival benefits.This is the benefit that you
  # receive for being in each Size at the final time step.
  
  
  Fitness <- array(NA, dim = c((max_Size), max_Performance, time_steps + 1))
  Fitness[,,time_steps + 1] <- Fitness_values
  # Fitness[,,12]
  
  
  
  Survival <- matrix(nrow = max_Size, ncol = max_Performance)
  Survival[ , ] <- Size %*% t(Performance)
  Survival <- Survival / 5.5 # We divide by the highest value to create a 0 to 1 Survival matrix
  # The Survival is the result of the interaction between size and Performance 
  # (performance), and it's different for every combination of each value.
  
  
  ForageRule <- array(NA, dim = c((max_Size), max_Performance, time_steps))
  # ForageRule <- matrix(nrow = max_Size, ncol = max_Performance)
  
  # Here, the ForageRule matrix is a 2 state variable matrix, and depends
  # on Performance and Size. We are going to store the TRUE/FALsE results here,
  # and see if it's better to invest in Performance or in Size.
  
  
  
  # Reward <- matrix(nrow = max_Size, ncol = max_Performance)
  # Reward[,] <- Fitness
  
  # The Reward matrix has to have the same dimensions as the ForageRule one. 
  # Each Size has the same Reward, so each row has the same Reward value. As you
  # only receive Survival from Size, you only increase the reward you get by 
  # increasing your Size. 
  
  
  RewardIfPerformance <-  array(NA, dim = c((max_Size), max_Performance, time_steps))
  RewardIfGrowth <-  array(NA, dim = c((max_Size), max_Performance, time_steps))
  
  
  
  # Loop
  
  t <- time_steps
  
  while (t >= 1) { 
    
    for (j in 1:max_Performance) {
      
      for (i in 1:max_Size) {
        
        if(j == max_Performance & i == max_Size){
          
          RewardIfPerformance[i,j,t] <- Survival[i, j] * Fitness[i,j,t+1] * prob_good_temp + 
            Survival[i, j] * Fitness[i,j,t+1] * prob_bad_temp
          
          RewardIfGrowth[i,j,t] <- Survival[i, j] * Fitness[i,j,t+1] * prob_good_temp * effect_good_temp +
            Survival[i, j] * Fitness[i,j,t+1] * prob_bad_temp * effect_bad_temp
          
          
        } else if (j == max_Performance & i == max_Size - 1) {
          
          RewardIfPerformance[i,j,t] <- Survival[i, j] * Fitness[i,j,t+1] * prob_good_temp +
            Survival[i, j] * Fitness[i,j,t+1] * prob_bad_temp
          
          RewardIfGrowth[i,j,t] <- Survival[i, j] * Fitness[i + 1,j,t+1] * prob_good_temp * effect_good_temp +
            Survival[i, j] * Fitness[i,j,t+1] * prob_bad_temp * effect_bad_temp
          
          
        } else if (j < max_Performance & i == max_Size) {
          
          RewardIfPerformance[i,j,t] <- Survival[i, j] * Fitness[i,j+1,t+1] * prob_good_temp +
            Survival[i, j] * Fitness[i,j+1,t+1] * prob_bad_temp
          
          RewardIfGrowth[i,j,t] <- Survival[i, j] * Fitness[i,j,t+1] * prob_good_temp * effect_good_temp +
            Survival[i, j] * Fitness[i,j,t+1] * prob_bad_temp * effect_bad_temp
          
          
        } else if (j < max_Performance & i == max_Size - 1) {
          
          RewardIfPerformance[i,j,t] <- Survival[i, j] * Fitness[i,j+1,t+1] * prob_good_temp +
            Survival[i, j] * Fitness[i,j+1,t+1] * prob_bad_temp
          
          RewardIfGrowth[i,j,t] <- Survival[i, j] * Fitness[i+1,j,t+1] * prob_good_temp * effect_good_temp +
            Survival[i, j] * Fitness[i,j,t+1] * prob_bad_temp * effect_bad_temp
          
          
        } else if (j == max_Performance & i < max_Size - 1) {
          
          RewardIfPerformance[i,j,t] <- Survival[i, j] * Fitness[i,j,t+1] * prob_good_temp +
            Survival[i, j] * Fitness[i,j,t+1] * prob_bad_temp
          
          RewardIfGrowth[i,j,t] <- Survival[i, j] * Fitness[i+2,j,t+1] * prob_good_temp * effect_good_temp +
            Survival[i, j] * Fitness[i,j,t+1] * prob_bad_temp * effect_bad_temp
          
          
        }else {
          
          RewardIfPerformance[i,j,t] <- Survival[i, j] * Fitness[i,j+1,t+1] * 
            prob_good_temp + Survival[i, j] * Fitness[i,j+1,t+1] * prob_bad_temp
          
          RewardIfGrowth[i,j,t] <- Survival[i, j] * Fitness[i+2,j,t+1] * prob_good_temp *
            effect_good_temp + Survival[i, j] * Fitness[i,j,t+1] * prob_bad_temp * effect_bad_temp
          # The rest of cells are going to work like this. If you invest in 
          # performance (Performance), you change your Survival value for the one that 
          # is on your right, that is calculated with a higher Performance value and 
          # the same size. If you invest in growth, then, you stay in the same Performance
          # column but you increase in Size, with its Rewards dependent on Size.
          
        } # end if/else loop
        
      } # end i loop
      
    } # end j loop
    
    
    RewardIfPerformance[1, , ] <- 0
    RewardIfGrowth[1, , ] <- 0
    # Survival and Reward values if you are dead
    
    ForageRule[,,t] <- RewardIfPerformance[,,t] > RewardIfGrowth[,,t]
    # TRUE/False matrix depending on which decision is best
    
    
    
    Fitness[, , t] <- ForageRule[,,t] * RewardIfPerformance[,,t] +
      as.numeric(!ForageRule[,,t]) * RewardIfGrowth[,,t]
    # This matrix stores the reward values obtained for the best decisions, 
    # and it is going to be used on the next time step.
    
    
    
    ForageRule_rev <- apply(ForageRule[,,t], 2, rev)
    ForageRule_rev[ForageRule_rev == "FALSE"] <- "Growth"
    ForageRule_rev[ForageRule_rev == "TRUE"] <- "Performance"
    ForageRule_rev[max_Size, ] <- "Dead"
    # ForageRule matrix inverted, so the lowest Fitness value is going to 
    # be displayed on the bottom layer
    
    # assign(paste("Decision", t, sep = ""), (ForageRule_rev)) #Not necessary now
    
    # We generate an object called Decision "t", that is going to store the Decision
    # matrix for each combination of states at each time step.
    # This is useful if we want to see one specific graph, as we can search for 
    # a matrix called "DecisionXX" in the environment. 
    
    plot(ForageRule_rev, col=c('#440154FF', '#21908CFF', '#FDE725FF'), 
         breaks=c("Dead", "Growth", "Performance"), xlab = "Survival", ylab = "Size",
         main = paste('Decision at time step ', t ), axis.col = NULL, axis.row = NULL)
    axis(1, at = 1:max_Performance, labels = Performance)
    axis(2, at = 1:(max_Size), labels = c(Size), las = 1)
    # At the end of each loop, we plot a matrix with the time step on the title
    
    t <- t - 1
    
  } # end of while loop
  
  
} # end of function



# Parameters

prob_good_temp <- 0.5
prob_bad_temp <- 1 - prob_good_temp
effect_good_temp <- 1
effect_bad_temp <- 1

time_steps <- 12

# par(mfrow=c(1,1))
par(mfrow=c(3,4))
par(mar=c(5.1, 4.5, 4.1, 6.5))


# Plot

Decisions(prob_good_temp, prob_bad_temp, effect_good_temp, effect_bad_temp, time_steps)
