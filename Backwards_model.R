

#---------------------Backwards model-----------------------

# In this model you either invest in either or you die, it's a simpler model. 
# To do this, we will use a matrix where the y-axis contains the different units 
# of energy invested in growth, and the x-axis contains the units of energy 
# invested in performance. The content of each cell will be the fitness value 
# of that combination of traits. 
# The function should see how fitness increases with respect to the previous 
# state by deciding whether to invest in growth (by moving down one cell 
# in the matrix) or in performance (by moving one cell to the right). It will then 
# calculate the fitness increase of the two decisions and choose the best option.


# Idea 1: Multiply by factor

Decisions <- function (prob_good_temp, prob_bad_temp, effect_good_temp, effect_bad_temp, time_steps) {
  
  
  # Life history values (from here to "Loop" can be removed from inside the function)
  
  Survival <- seq(0.7, 0.975, 0.025)
  max_Survival <- length(Survival)
  
  
  Size <- seq(1, 5.5, 0.1)
  max_Size <- length(Size)
  
  
  Condition <- seq(1, 5.5, 0.1)
  max_Condition <- length(Condition)
  Condition[Condition < 4] <- 0
  Condition[Condition >=4 & Condition < 5] <- seq(2, 3.8, 2/(length(Condition[Condition >=4 & Condition < 5])))
  Condition[Condition >= 5] <- c(4, 4.1, 4.15, 4.15, 4.15, 4.15)
  # Sizes under 4 cm don't receive Fitness benefits.
  
  
  Fitness <- matrix(nrow = max_Size + 1, ncol = max_Survival)
  Fitness[2:(max_Size + 1), ] <- Size %*% t(Survival)
  Fitness[1, ] <- 0
  
  
  ForageRule <- matrix(nrow = max_Size + 1, ncol = max_Survival)
  # Here, the ForageRule matrix is a 2 state variable matrix, and depends
  # on Survival and Size
  
  Reward <- matrix(nrow = max_Condition + 1, ncol = max_Survival)
  Reward[,] <- c(0, Condition)
  # The Reward matrix has to have the same dimensions as the ForageRule one. 
  # Each Size has the same Reward, so each row has the same Reward value
  
  
  RewardIfPerformance <- matrix(nrow = max_Size + 1, ncol = max_Survival)
  RewardIfGrowth <- matrix(nrow = max_Size + 1, ncol = max_Survival)
  
  
  
  # Loop
  
  t <- time_steps
  
  while (t >= 1) { 
    
    for (j in 1:(max_Survival)) {
      
      for (i in 1:max_Size + 1) {
        
        if(j == max_Survival & i < max_Size + 1){
          
          RewardIfPerformance[i,j] <- Fitness[i, j] * Reward[i, j] * prob_good_temp + 
            Fitness[i, j] * Reward[i,j] * prob_bad_temp
            
          RewardIfGrowth[i,j] <- Fitness[i, j] * Reward[i + 1, j] * prob_good_temp * effect_good_temp +
            Fitness[i, j] * Reward[i + 1, j] * prob_bad_temp * effect_bad_temp
          # This "if" condition is necessary so if you are on top performance, 
          # you stay with the same performance value (Survival)
          
        } else if (j == max_Survival & i == max_Size + 1) {
          
          RewardIfPerformance[i,j] <- Fitness[i, j] * Reward[i, j] * prob_good_temp +
            Fitness[i, j] * Reward[i,j] * prob_bad_temp
          
          RewardIfGrowth[i,j] <- Fitness[i, j] * Reward[i, j] * prob_good_temp * effect_good_temp +
            Fitness[i, j] * Reward[i, j] * prob_bad_temp * effect_bad_temp
          # The same but with max Condition and Survival
          
        } else if (j < max_Survival & i == max_Size + 1) {
          
          RewardIfPerformance[i,j] <- Fitness[i, j] * Reward[i, j + 1] * prob_good_temp +
            Fitness[i, j] * Reward[i, j + 1] * prob_bad_temp
          
          RewardIfGrowth[i,j] <- Fitness[i, j] * Reward[i, j] * prob_good_temp * effect_good_temp +
            Fitness[i, j] * Reward[i, j] * prob_bad_temp * effect_bad_temp
          # The same but for Condition
          
        } else {
          
          RewardIfPerformance[i,j] <- Fitness[i, j] * Reward[i, j + 1] * 
            prob_good_temp + Fitness[i, j] * Reward[i, j + 1] * prob_bad_temp
          
          RewardIfGrowth[i,j] <- Fitness[i, j] * Reward[i + 1, j] * prob_good_temp *
            effect_good_temp + Fitness[i, j] * Reward[i + 1, j] * prob_bad_temp * effect_bad_temp
          # The rest of cells are going to work like this. If you invest in 
          # performance (Survival), you change your Fitness value for the one that 
          # is on your right, that is calculated with a higher Survival value and 
          # the same size. If you invest in growth, then, you stay in the same Survival
          # column but you increase in Size, with its Rewards dependent on Size.
        
        } # end if/else loop
        
      } # end i loop
      
    } # end j loop
  
   
   RewardIfPerformance[1, ] <- 0
   RewardIfGrowth[1, ] <- 0
   # Fitness and REward values if you are dead
   
   ForageRule[,] <- RewardIfPerformance[,] > RewardIfGrowth[,]
   # TRUE/False matrix depending on which decision is best
   
   Reward[,] <- ForageRule[,] * RewardIfPerformance[,] +
     as.numeric(!ForageRule[,]) * RewardIfGrowth[,]
   
   # This matrix stores the reward values obtained for the best decisions, 
   # and it is going to be used on the next time step.
  
   ForageRule_rev <- apply(ForageRule, 2, rev)
   ForageRule_rev[ForageRule_rev == "FALSE"] <- "Growth"
   ForageRule_rev[ForageRule_rev == "TRUE"] <- "Performance"
   ForageRule_rev[max_Size + 1, ] <- "Dead"
   # ForageRule matrix inverted, so the lowest Condition value is going to 
   # be displayed on the bottom layer
   
   # assign(paste("Decision", t, sep = ""), (ForageRule_rev)) #Not necessary now
   
   # We generate an object called Decision "t", that is going to store the Decision
   # matrix for each combination of states at each time step.
   # This is useful if we want to see one specific graph, as we can search for 
   # a matrix called "DecisionXX" in the environment. 
   
   plot(ForageRule_rev, col=c('#440154FF', '#21908CFF', '#FDE725FF'), 
        breaks=c("Dead", "Growth", "Performance"), xlab = "Survival", ylab = "Size",
        main = paste('Decision at time step ', t ), axis.col = NULL, axis.row = NULL)
   axis(1, at = 1:max_Survival, labels = Survival)
   axis(2, at = 1:(max_Size + 1), labels = c(0, Size), las = 1)
   # At the end of each loop, we plot a matrix with the time step on the title
   
   t <- t - 1
   
  } # end of while loop
  

} # end of function



# Parameters

prob_good_temp <- 0
prob_bad_temp <- 1 - prob_good_temp
effect_good_temp <- 1.2
effect_bad_temp <- 0.8

time_steps <- 12

# par(mfrow=c(1,1))
par(mfrow=c(3,4))
par(mar=c(5.1, 4.5, 4.1, 6.5))


# Plot

Decisions(prob_good_temp, prob_bad_temp, effect_good_temp, effect_bad_temp, time_steps)













# Idea 2: Jump one condition state if Tº is good.

Decisions <- function (prob_good_temp, prob_bad_temp, effect_good_temp, effect_bad_temp, time_steps) {
  
  
  # Life history values (from here to "Loop" can be removed from inside the function)
  
  Survival <- seq(0.7, 0.975, 0.025)
  max_Survival <- length(Survival)
  
  
  Size <- seq(1, 5.5, 0.1)
  max_Size <- length(Size)
  
  
  Condition <- seq(1, 5.5, 0.1)
  max_Condition <- length(Condition)
  Condition[Condition < 4] <- 0
  Condition[Condition >=4 & Condition < 5] <- seq(2, 3.8, 2/(length(Condition[Condition >=4 & Condition < 5])))
  Condition[Condition >= 5] <- c(4, 4.1, 4.15, 4.15, 4.15, 4.15)
  # Sizes under 4 cm don't receive Fitness benefits.
  
  
  Fitness <- matrix(nrow = max_Size + 1, ncol = max_Survival)
  Fitness[2:(max_Size + 1), ] <- Size %*% t(Survival)
  Fitness[1, ] <- 0
  
  
  ForageRule <- matrix(nrow = max_Size + 1, ncol = max_Survival)
  # Here, the ForageRule matrix is a 2 state variable matrix, and depends
  # on Survival and Size
  
  Reward <- matrix(nrow = max_Condition + 1, ncol = max_Survival)
  Reward[,] <- c(0, Condition)
  # The Reward matrix has to have the same dimensions as the ForageRule one. 
  # Each Size has the same Reward, so each row has the same Reward value
  
  
  RewardIfPerformance <- matrix(nrow = max_Size + 1, ncol = max_Survival)
  RewardIfGrowth <- matrix(nrow = max_Size + 1, ncol = max_Survival)
  
  
  
  # Loop
  
  t <- time_steps
  
  while (t >= 1) { 
    
    for (j in 1:(max_Survival)) {
      
      for (i in 1:max_Size + 1) {
        
        if(j == max_Survival & i < max_Size){
          
          RewardIfPerformance[i,j] <- Fitness[i, j] * Reward[i, j] * prob_good_temp + 
            Fitness[i, j] * Reward[i,j] * prob_bad_temp
          
          RewardIfGrowth[i,j] <- Fitness[i, j] * Reward[i + 2, j] * prob_good_temp * effect_good_temp +
            Fitness[i, j] * Reward[i , j] * prob_bad_temp * effect_bad_temp
          # This "if" condition is necessary so if you are on top performance, 
          # you stay with the same performance value (Survival)
          
        } else if (j == max_Survival & i == max_Size) {
          
          RewardIfPerformance[i,j] <- Fitness[i, j] * Reward[i, j] * prob_good_temp +
            Fitness[i, j] * Reward[i,j] * prob_bad_temp
          
          RewardIfGrowth[i,j] <- Fitness[i, j] * Reward[i + 1, j] * prob_good_temp * effect_good_temp +
            Fitness[i, j] * Reward[i, j] * prob_bad_temp * effect_bad_temp
          # The same but with max Condition and Survival
          
        } else if (j < max_Survival & i == max_Size) {
          
          RewardIfPerformance[i,j] <- Fitness[i, j] * Reward[i, j + 1] * prob_good_temp +
            Fitness[i, j] * Reward[i, j + 1] * prob_bad_temp
          
          RewardIfGrowth[i,j] <- Fitness[i, j] * Reward[i + 1, j] * prob_good_temp * effect_good_temp +
            Fitness[i, j] * Reward[i, j] * prob_bad_temp * effect_bad_temp
          # The same but for Condition
          
        } else if (j < max_Survival & i == max_Size + 1) {
          
          RewardIfPerformance[i,j] <- Fitness[i, j] * Reward[i, j + 1] * prob_good_temp +
            Fitness[i, j] * Reward[i, j + 1] * prob_bad_temp
          
          RewardIfGrowth[i,j] <- Fitness[i, j] * Reward[i, j] * prob_good_temp * effect_good_temp +
            Fitness[i, j] * Reward[i, j] * prob_bad_temp * effect_bad_temp
          # The same but for Condition
          
        } else if (j == max_Survival & i == max_Size + 1) {
          
          RewardIfPerformance[i,j] <- Fitness[i, j] * Reward[i, j] * prob_good_temp +
            Fitness[i, j] * Reward[i, j] * prob_bad_temp
          
          RewardIfGrowth[i,j] <- Fitness[i, j] * Reward[i, j] * prob_good_temp * effect_good_temp +
            Fitness[i, j] * Reward[i, j] * prob_bad_temp * effect_bad_temp
          # The same but for Condition
          
        }else {
          
          RewardIfPerformance[i,j] <- Fitness[i, j] * Reward[i, j + 1] * 
            prob_good_temp + Fitness[i, j] * Reward[i, j + 1] * prob_bad_temp
          
          RewardIfGrowth[i,j] <- Fitness[i, j] * Reward[i + 2, j] * prob_good_temp *
            effect_good_temp + Fitness[i, j] * Reward[i , j] * prob_bad_temp * effect_bad_temp
          # The rest of cells are going to work like this. If you invest in 
          # performance (Survival), you change your Fitness value for the one that 
          # is on your right, that is calculated with a higher Survival value and 
          # the same size. If you invest in growth, then, you stay in the same Survival
          # column but you increase in Size, with its Rewards dependent on Size.
          
        } # end if/else loop
        
      } # end i loop
      
    } # end j loop
    
    
    RewardIfPerformance[1, ] <- 0
    RewardIfGrowth[1, ] <- 0
    # Fitness and REward values if you are dead
    
    ForageRule[,] <- RewardIfPerformance[,] > RewardIfGrowth[,]
    # TRUE/False matrix depending on which decision is best
    
    Reward[,] <- ForageRule[,] * RewardIfPerformance[,] +
      as.numeric(!ForageRule[,]) * RewardIfGrowth[,]
    
    # This matrix stores the reward values obtained for the best decisions, 
    # and it is going to be used on the next time step.
    
    ForageRule_rev <- apply(ForageRule, 2, rev)
    ForageRule_rev[ForageRule_rev == "FALSE"] <- "Growth"
    ForageRule_rev[ForageRule_rev == "TRUE"] <- "Performance"
    ForageRule_rev[max_Size + 1, ] <- "Dead"
    # ForageRule matrix inverted, so the lowest Condition value is going to 
    # be displayed on the bottom layer
    
    # assign(paste("Decision", t, sep = ""), (ForageRule_rev)) #Not necessary now
    
    # We generate an object called Decision "t", that is going to store the Decision
    # matrix for each combination of states at each time step.
    # This is useful if we want to see one specific graph, as we can search for 
    # a matrix called "DecisionXX" in the environment. 
    
    plot(ForageRule_rev, col=c('#440154FF', '#21908CFF', '#FDE725FF'), 
         breaks=c("Dead", "Growth", "Performance"), xlab = "Survival", ylab = "Size",
         main = paste('Decision at time step ', t ), axis.col = NULL, axis.row = NULL)
    axis(1, at = 1:max_Survival, labels = Survival)
    axis(2, at = 1:(max_Size + 1), labels = c(0, Size), las = 1)
    # At the end of each loop, we plot a matrix with the time step on the title
    
    t <- t - 1
    
  } # end of while loop
  
  
} # end of function



# Parameters

prob_good_temp <- 0.5
prob_bad_temp <- 1 - prob_good_temp
effect_good_temp <- 1
effect_bad_temp <- 1

time_steps <- 24

# par(mfrow=c(1,1))
par(mfrow=c(3,4))
par(mar=c(5.1, 4.5, 4.1, 6.5))


# Plot

Decisions(prob_good_temp, prob_bad_temp, effect_good_temp, effect_bad_temp, time_steps)








#--------------- Not necessary now --------------------------
  
par(mfrow=c(1,1))
par(mfrow=c(2,3))
par(mar=c(5.1, 4.5, 4.1, 6.5))

library('plot.matrix')

Plots <- function(x){

plot(Decision20, col=c('#440154FF', '#21908CFF', '#FDE725FF'), 
     breaks=c("Dead", "Growth", "Performance"), xlab = "Survival", ylab = "Size",
     main="Decision at time step 6", axis.col=NULL, axis.row=NULL)
axis(1, at = 1:max_Survival, labels = Survival)
axis(2, at = 1:(max_Size + 1), labels = c(0, Size), las = 1)

plot(Decision18, col=c('#440154FF', '#21908CFF', '#FDE725FF'), 
     breaks=c("Dead", "Growth", "Performance"), xlab = "Survival", ylab = "Size",
     main="Decision at time step 5", axis.col=NULL, axis.row=NULL)
axis(1, at = 1:max_Survival, labels = Survival)
axis(2, at = 1:(max_Size + 1), labels = c(0, Size), las = 1)

plot(Decision15, col=c('#440154FF', '#21908CFF', '#FDE725FF'), 
     breaks=c("Dead", "Growth", "Performance"), xlab = "Survival", ylab = "Size",
     main="Decision at time step 4", axis.col=NULL, axis.row=NULL)
axis(1, at = 1:max_Survival, labels = Survival)
axis(2, at = 1:(max_Size + 1), labels = c(0, Size), las = 1)

plot(Decision10, col=c('#440154FF', '#21908CFF', '#FDE725FF'), 
     breaks=c("Dead", "Growth", "Performance"), xlab = "Survival", ylab = "Size",
     main="Decision at time step 3", axis.col=NULL, axis.row=NULL)
axis(1, at = 1:max_Survival, labels = Survival)
axis(2, at = 1:(max_Size + 1), labels = c(0, Size), las = 1)

plot(Decision5, col=c('#440154FF', '#21908CFF', '#FDE725FF'), 
     breaks=c("Dead", "Growth", "Performance"), xlab = "Survival", ylab = "Size",
     main="Decision at time step 2", axis.col=NULL, axis.row=NULL)
axis(1, at = 1:max_Survival, labels = Survival)
axis(2, at = 1:(max_Size + 1), labels = c(0, Size), las = 1)

plot(Decision1, col=c('#440154FF', '#21908CFF', '#FDE725FF'), 
     breaks=c("Dead", "Growth", "Performance"), xlab = "Survival", ylab = "Size",
     main="Decision at time step 1", axis.col=NULL, axis.row=NULL)
axis(1, at = 1:max_Survival, labels = Survival)
axis(2, at = 1:(max_Size + 1), labels = c(0, Size), las = 1)

}

Plots(1)



