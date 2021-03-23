

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



#---------------1st Model------------------------

# Back-up code for the main code

Survival <- c(0.7, 0.75, 0.8, 0.85, 0.9, 0.95) 
# Survival values that the tadpole can have if it invests in performance

Condition_values <- c (0.4, 0.6, 0.75, 0.85, 0.90, 0.925, 0.9375, 0.945, 0.95)
# Final condition values and growth sizes that the tadpole can obtain by
# investing in growth. These values (final condition and sizes) can change and 
# be different from each other.

number_Survival <- length(Survival) 
# Number of Survival values

time_steps <- 6
# Time steps I'll work with

max_condition <- length(Condition_values)
# Number of sizes and final condition values I'm working with


Fitness <- matrix(nrow=max_condition+1, ncol=number_Survival)
Fitness[2:(max_condition+1), ] <- Condition_values %*% t(Survival)
Fitness[1, ] <- rep(0,6)
# Fitness matrix. Each cell contains the actual fitness of each tadpole. 
# At the moment, it is measured as Size*Performance



ForageRule <- matrix(nrow=max_condition+1, ncol=time_steps)
# This example is very similar to Hanna Kokko's model. This attempt isn't good,
# so don't waste much time looking at it. 

Reward <- matrix(nrow=max_condition+1, ncol=time_steps+1)
Reward[,time_steps+1] <- c(0, Condition_values)

for (t in time_steps:1) {
  
  RewardIfPerformance <- matrix(nrow=max_condition+1, ncol=time_steps)
  RewardIfGrowth <- matrix(nrow=max_condition+1, ncol=time_steps)
  
  for (j in 1:(number_Survival-1)) {
    
    for (i in 2:max_condition) {
      
      RewardIfPerformance[i,t] <- Fitness[i, j+1] * Reward[i,t+1] 
      RewardIfGrowth[i,t] <- Fitness[i+1, j] * Reward[i+1,t+1]
      
    } # end i loop
    
  } # end j loop

  RewardIfPerformance[1,t] <- 0
  RewardIfGrowth[1,t] <- 0
  
  if(j < 6){
    
    RewardIfPerformance[max_condition+1,t] <- Fitness[i, j+1] * Reward[max_condition+1,t+1] 
    RewardIfGrowth[max_condition+1,t] <- Fitness[i, j] * Reward[max_condition+1,t+1]   
    
  } else {
    
    RewardIfPerformance[max_condition+1,t] <- Fitness[i, j] * Reward[max_condition+1,t+1] 
    RewardIfGrowth[max_condition+1,t] <- Fitness[i, j] * Reward[max_condition+1,t+1]
    
  } # end if loop
  
  ForageRule[,t] <- RewardIfPerformance[,t] > RewardIfGrowth[,t]
  
  Reward[,t] <- ForageRule[,t] * RewardIfPerformance[,t] +
    as.numeric(!ForageRule[,t]) * RewardIfGrowth[,t]

}

colour <- c("white", "blue")
require(lattice)
library(grid)
mypanel <- function(x, y, z, ...) {
  panel.levelplot(x, y, z, ...)
  grid.rect(x=x, y=y, width=1, height=1, default.units="native")
}
print(levelplot(t(ForageRule),
                scales=list(tck=0,
                            x=list(at=1:time_steps,labels=1:time_steps),
                            y=list(at=1:(max_condition+1),labels=c(0,Condition_values))),
                colorkey=FALSE, col.regions=colour, aspect="fill",
                xlab="Time", ylab="Condition", panel=mypanel))

list(ForageRule=ForageRule)


library(reshape2)
library(ggplot2)
ggplot(melt(ForageRule), aes(x=Var2, y=Var1, fill=value)) + geom_tile() +
  scale_fill_viridis_d(name = "Action", labels = c("Growth", "Performance"), alpha = 0.5) +
  scale_y_discrete(name = "Fitness", breaks = c(0,Condition_values), 
                   labels = c("0", "0.4", "0.6", "0.75", "0.85", "0.90", "0.925", "0.9375", "0.945", "0.95"), limit = c(1:10)) +
  scale_x_continuous(name="Time step", limits=c(0.5, 6.5))


# The model could be improved, there are many points that are wrong. 
# The problem with this model is that for each time step, it does not take all 
# possible combinations of fitness, but only takes the highest ones, so the 
# reward matrix does not show all the options. 






#--------------------- 2nd model --------------------------
#                This is the good one


Survival <- c(0.7, 0.75, 0.8, 0.85, 0.9, 0.95)
Condition_values <- c (0.4, 0.6, 0.75, 0.85, 0.90, 0.925, 0.9375, 0.945, 0.95)
number_Survival <- length(Survival)
time_steps <- 6
max_condition <- length(Condition_values)

Fitness <- matrix(nrow=max_condition+1, ncol=number_Survival)
Fitness[2:(max_condition+1), ] <- Condition_values %*% t(Survival)
Fitness[1, ] <- rep(0,6)


ForageRule <- matrix(nrow=max_condition+1, ncol=number_Survival)
# Here, the ForageRule matrix is a 2 state variable matrix, and depends
# on Survival and Size

Reward <- matrix(nrow=max_condition+1, ncol=number_Survival)
Reward[,] <- c(0, Condition_values)
# The Reward matrix has to have the same dimensions as the ForageRule one. 
# Each Size has the same Reward, so each row has the same Reward value

RewardIfPerformance <- matrix(nrow=max_condition+1, ncol=number_Survival)
RewardIfGrowth <- matrix(nrow=max_condition+1, ncol=number_Survival)

t <- time_steps

while (t >= 1) { 

  for (j in 1:(number_Survival)) {
    
    for (i in 1:max_condition+1) {
      
      if(j == 6 & i < max_condition + 1){
        
        RewardIfPerformance[i,j] <- Fitness[i, j] * Reward[i,j] 
        RewardIfGrowth[i,j] <- Fitness[i+1, j] * Reward[i+1,j]   
        # This "if" condition is necessary so if you are on top performance, 
        # you stay with the same performance value (Survival)
        
      } else if (j == 6 & i == max_condition + 1) {
        
        RewardIfPerformance[i,j] <- Fitness[i, j] * Reward[i,j]
        RewardIfGrowth[i,j] <- Fitness[i, j] * Reward[i,j]
        # The same but with max Condition and Survival
        
      } else if (j < 6 & i == max_condition + 1) {
        
        RewardIfPerformance[i,j] <- Fitness[i, j+1] * Reward[i,j+1] 
        RewardIfGrowth[i,j] <- Fitness[i, j] * Reward[i,j]
        # The same but for Condition
        
      } else {
        
        RewardIfPerformance[i,j] <- Fitness[i, j+1] * Reward[i,j+1] 
        RewardIfGrowth[i,j] <- Fitness[i+1, j] * Reward[i+1,j]
        # The rest of cells are going to work like this. If you invest in 
        # performance (Survival), you change your Fitness value for the one that 
        # is on your right, that is calculated with a higher Survival value and 
        # the same size. If you invest in growth, then, you stay in the same Survival
        # column but you increase in Size, with its Rewards dependent on Size.
      
      } # end if/else loop
      
    } # end i loop
    
  } # end j loop

 
 RewardIfPerformance[1,] <- 0
 RewardIfGrowth[1,] <- 0
 # Fitness and REward values if you are dead
 
 ForageRule[,] <- RewardIfPerformance[,] > RewardIfGrowth[,]
 # TRUE/False matrix depending on which decision is best
 
 Reward[,] <- ForageRule[,] * RewardIfPerformance[,] +
   as.numeric(!ForageRule[,]) * RewardIfGrowth[,]
 
 # This matrix stores the reward values obtained for the best decisions, 
 # and it is going to be used on the next time step.

 ForageRule_rev <- apply(ForageRule, 2, rev)
 ForageRule_rev[ForageRule_rev=="FALSE"] <- "Growth"
 ForageRule_rev[ForageRule_rev=="TRUE"] <- "Performance"
 ForageRule_rev[max_condition+1,] <- "Dead"
 # ForageRule matrix inverted, so the lowest Condition value is going to 
 # be displayed on the bottom layer
 
 assign(paste("Decision", t, sep = ""), (ForageRule_rev))
 # We generate an object called Decision "t", that is going to store the Decision
 # matrix for each convination of states at each time step.
 
 t <- t - 1
 
}
  
  
par(mfrow=c(1,1))
par(mfrow=c(2,3))
par(mar=c(5.1, 4.5, 4.1, 6.5))

library('plot.matrix')

Plots <- function(x){

plot(Decision6, col=c('#440154FF', '#21908CFF', '#FDE725FF'), 
     breaks=c("Dead", "Growth", "Performance"), xlab = "Survival", ylab = "Size",
     main="Decision at time step 6", axis.col=NULL, axis.row=NULL)
axis(1, at = 1:6, labels = Survival)
axis(2, at = 1:10, labels = c(0,Condition_values), las = 1)

plot(Decision5, col=c('#440154FF', '#21908CFF', '#FDE725FF'), 
     breaks=c("Dead", "Growth", "Performance"), xlab = "Survival", ylab = "Size",
     main="Decision at time step 5", axis.col=NULL, axis.row=NULL)
axis(1, at = 1:6, labels = Survival)
axis(2, at = 1:10, labels = c(0,Condition_values), las = 1)

plot(Decision4, col=c('#440154FF', '#21908CFF', '#FDE725FF'), 
     breaks=c("Dead", "Growth", "Performance"), xlab = "Survival", ylab = "Size",
     main="Decision at time step 4", axis.col=NULL, axis.row=NULL)
axis(1, at = 1:6, labels = Survival)
axis(2, at = 1:10, labels = c(0,Condition_values), las = 1)

plot(Decision3, col=c('#440154FF', '#21908CFF', '#FDE725FF'), 
     breaks=c("Dead", "Growth", "Performance"), xlab = "Survival", ylab = "Size",
     main="Decision at time step 3", axis.col=NULL, axis.row=NULL)
axis(1, at = 1:6, labels = Survival)
axis(2, at = 1:10, labels = c(0,Condition_values), las = 1)

plot(Decision2, col=c('#440154FF', '#21908CFF', '#FDE725FF'), 
     breaks=c("Dead", "Growth", "Performance"), xlab = "Survival", ylab = "Size",
     main="Decision at time step 2", axis.col=NULL, axis.row=NULL)
axis(1, at = 1:6, labels = Survival)
axis(2, at = 1:10, labels = c(0,Condition_values), las = 1)

plot(Decision1, col=c('#440154FF', '#21908CFF', '#FDE725FF'), 
     breaks=c("Dead", "Growth", "Performance"), xlab = "Survival", ylab = "Size",
     main="Decision at time step 1", axis.col=NULL, axis.row=NULL)
axis(1, at = 1:6, labels = Survival)
axis(2, at = 1:10, labels = c(0,Condition_values), las = 1)

}

Plots(1)


#----------------------- 3rd model --------------------------


# In this model, you have to change performance state manually, and there is a
# problem with decisions (decision at time t-1 should affect decision at t, and 
# it doesn't. Also the Reward value on which t-1 is based is wrong).


Survival <- c(0.7, 0.75, 0.8, 0.85, 0.9, 0.95)
Condition_values <- c (0.4, 0.6, 0.75, 0.85, 0.90, 0.925, 0.9375, 0.945, 0.95)
time_steps <- 6
max_condition <- length(Condition_values)


ForageRule <- matrix(nrow=max_condition+1, ncol=time_steps)

Reward <- matrix(nrow=max_condition+1, ncol=time_steps+1)
Reward[,time_steps+1] <- c(0, Condition_values)

# Invest <- Survival 

for (t in time_steps:1) {
  
  RewardIfPerformance <- matrix(nrow=max_condition+1, ncol=time_steps)
  RewardIfGrowth <- matrix(nrow=max_condition+1, ncol=time_steps)
 
  for (i in 2:max_condition) {
    RewardIfPerformance[i,t] <- Survival[2] * Reward[i,t+1] 
    RewardIfGrowth[i,t] <- Survival[1] * Reward[i+1,t+1]
  }
  
  RewardIfPerformance[1,t] <- 0
  RewardIfGrowth[1,t] <- 0
  
  RewardIfPerformance[max_condition+1,t] <- Survival[2] * Reward[max_condition+1,t+1] 
  RewardIfGrowth[max_condition+1,t] <- Survival[1] * Reward[max_condition+1,t+1] 
  
  
  ForageRule[,t] <- RewardIfPerformance[,t] > RewardIfGrowth[,t]
  
  Reward[,t] <- ForageRule[,t] * RewardIfPerformance[,t] +
    as.numeric(!ForageRule[,t]) * RewardIfGrowth[,t]
}


colour <- c("white", "blue")
require(lattice)
library(grid)
mypanel <- function(x, y, z, ...) {
  panel.levelplot(x, y, z, ...)
  grid.rect(x=x, y=y, width=1, height=1, default.units="native")
}
print(levelplot(t(ForageRule),
                scales=list(tck=0,
                            x=list(at=1:time_steps,labels=1:time_steps),
                            y=list(at=1:(max_condition+1),labels=c(0,Condition_values))),
                colorkey=FALSE, col.regions=colour, aspect="fill",
                xlab="Time", ylab="Condition", panel=mypanel))

list(ForageRule=ForageRule)


library(reshape2)
library(ggplot2)
ggplot(melt(ForageRule), aes(x=Var2, y=Var1, fill=value)) + geom_tile() +
  scale_fill_viridis_d(name = "Action", labels = c("Growth", "Performance"), alpha = 0.5) +
  scale_y_discrete(name = "Fitness", breaks = c(0,Condition_values), 
                   labels = c("0", "0.4", "0.6", "0.75", "0.85", "0.90", "0.925", "0.9375", "0.945", "0.95"), limit = c(1:10)) +
  scale_x_continuous(name="Time step", limits=c(0.5, 6.5))

