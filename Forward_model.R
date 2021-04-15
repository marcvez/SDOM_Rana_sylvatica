

#---------------------------- Forward model ----------------------------------

# In this model we will use the values obtained in the backward simulation to 
# simulate a tadpole population trying to achieve maximum fitness at the end of 
# the metamorphosis period. At each time step, the organism either lives or dies. 
# If it lives, it finds food (size and/or performance can influence how much 
# food it finds) and this is spent on growing or moving faster. 


N <- 100
# Number of Tadpoles

Population <- matrix(nrow = N, ncol = time_steps + 1)
rownames(Population) <- c(1:N) # Tadpole number xx
colnames(Population) <- c((1:time_steps), "Fitness") # Time step xx
# This matrix is going to store the state of each tadpole during the whole
# period. It is going to be read by the alive matrix, to determine how many 
# individuals are still alive at each time step. 


Alive <- matrix(nrow = time_steps, ncol = 2)
colnames(Alive) <- c("time step", "Nº alive")
Alive[, 1] <- c(1:time_steps)
# We are going to sum the number of dead or alive individuals at each time step.
# For ploting.


for (n in 1:N) {
  
  i <- 2
  j <- 1
  t <- 1
  # Initial condition, after hatching
  
  while (t <= time_steps) {
    
    Prob_Survive <- runif(1)
    # Random value that determines if you live or you die.
    
    if (Prob_Survive < Survival[i, j]) {
      # Prob_Survive where 0.7
      if (ForageRule[i, j, t] == TRUE) {
        
        j <- j + 1
        
        Population[n, t] <- 1
        
        print("Performance")
        
      } else if (ForageRule[i, j, t] == FALSE) {
        
        i <- i + 1
        
        Population[n, t] <- 1
        
        print("Growth")
        
      }
      
    } else {
      
      i <- 1
      
      Population[n, t] <- 0
      
      print("Dead")
    
      
    } # if/else loop
    
    Alive[t, 2] <- sum(Population[, t])
    
    Population[n, time_steps + 1] <- Fitness[i, j, t]
    
    t <- t + 1 
    
  } # while loop
  
  
} # for loop

print(Alive[time_steps, 2])




