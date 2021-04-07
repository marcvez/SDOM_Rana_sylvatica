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
##needed libraries
library(plot.matrix)

Decisions <- function (prob_good_temp, time_steps) {
  
  # Parameters
  
  prob_good_temp <- 0.5
  prob_bad_temp <- 1 - prob_good_temp
  effect_good_temp <- 1.2
  effect_bad_temp <- 0.8
  
  time_steps <- 12
  
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
  
  
  Fitness <- array(NA, dim = c((max_Size + 1), max_Survival,time_steps))
  Fitness[2:(max_Size + 1),,time_steps] <- Condition
  Fitness[1, ,] <- 0
  
  
  ForageRule <- array(NA,dim = c((max_Size + 1), max_Survival,time_steps))
  # Here, the ForageRule matrix is a 2 state variable matrix, and depends
  # on Survival and Size
  
  growth<-c(1,2) #this is a vector that hold the possibilities of growth in 
  # different conditions (this can also be stochastic and due to luck alone)
  
  # Loop
  
  t <- time_steps-1
  
  while (t >= 1) { 
    
    for (j in 1:(max_Survival)) {
        
      for (i in 1:max_Size + 1) {
          
        if(j == max_Survival & i < max_Size + 1){
            
            RewardIfPerformance<- Fitness[i, j, t+1]*Survival[j] #No change in state since you already have max performance
            
            
            size_if_growth<- i+growth
            RewardIfGrowth<- Fitness[min(max_Size, size_if_growth[1]), j ,t+1] * (1-prob_good_temp) + Fitness[min(max_Size, size_if_growth[2]), j ,t+1] * (prob_good_temp)
            RewardIfGrowth<- RewardIfGrowth*Survival[j] #you only get fitness if you survive
            # This "if" condition is necessary so if you are on top performance, 
            # you stay with the same performance value (Survival)
            
          } else if (j == max_Survival & i == max_Size + 1) {
            
            RewardIfPerformance <- Fitness[i, j, t+1]*Survival[j] #No change in state since you already have max performance
            
            RewardIfGrowth<- Fitness[i, j, t+1]*Survival[j] #No change in state since you already have max size
            # The same but with max Condition and Survival
            
          } else if (j < max_Survival & i == max_Size + 1) {
            
            RewardIfPerformance <- Fitness[i, j+1, t+1]*Survival[j] #performance increased since invested in
            
            RewardIfGrowth<- Fitness[i, j, t+1]*Survival[j] #No change in state since you already have max size
            # The same but for Condition
            
          } else {
            
            RewardIfPerformance<- Fitness[i, j+1, t+1]*Survival[j] #performance increased since invested in
            
            size_if_growth<- i+growth
            RewardIfGrowth<- Fitness[min(max_Size, size_if_growth[1]), j ,t+1] * (1-prob_good_temp) + Fitness[min(max_Size, size_if_growth[2]), j ,t+1] * (prob_good_temp)
            RewardIfGrowth<- RewardIfGrowth*Survival[j] #you only get fitness if you survive
            
            # The rest of cells are going to work like this. If you invest in 
            # performance (Survival), you change your Fitness value for the one that 
            # is on your right, that is calculated with a higher Survival value and 
            # the same size. If you invest in growth, then, you stay in the same Survival
            # column but you increase in Size, with its Rewards dependent on Size.
            
          } # end if/else loop for making sure no exceeding size or performance (this is possible to make simpler)
        
        if (i==1){
          RewardIfPerformance<- 0
          RewardIfGrowth <- 0
          # Fitness and REward values if you are dead
        }
        
        ForageRule[i,j,t] <- RewardIfPerformance > RewardIfGrowth
        # TRUE/False matrix depending on which decision is best
        
        Fitness[i, j, t] <- max(RewardIfPerformance, RewardIfGrowth)
        
        # This matrix stores the reward values obtained for the best decisions, 
          
        } # end i loop
        
      } # end j loop
      
    t <- t - 1
    
  } # end of while loop
  
} # end of function

for (time in c(1,5,11)){ #we plot some selected time steps

ForageRule_rev <- apply(ForageRule[,,time], 2, rev) #at time step "time"
ForageRule_rev[ForageRule_rev == "FALSE"] <- "Growth"
ForageRule_rev[ForageRule_rev == "TRUE"] <- "Performance"
ForageRule_rev[max_Size + 1, ] <- "Dead"
# ForageRule matrix inverted, so the lowest Condition value is going to 
# be displayed on the bottom layer

plot(ForageRule_rev, col=c('#440154FF', '#21908CFF', '#FDE725FF'), 
     breaks=c("Dead", "Growth", "Performance"), xlab = "Survival", ylab = "Size",
     main = paste('Decision at time step ', time ), axis.col = NULL, axis.row = NULL)
axis(1, at = 1:max_Survival, labels = Survival)
axis(2, at = 1:(max_Size + 1), labels = c(0, Size), las = 1)

}



# par(mfrow=c(1,1))
par(mfrow=c(3,1))
par(mar=c(5.1, 4.5, 4.1, 6.5))


# Plot

Decisions(prob_good_temp, prob_bad_temp, effect_good_temp, effect_bad_temp, time_steps)












