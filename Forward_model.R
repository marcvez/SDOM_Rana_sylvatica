

#---------------------------- Forward model ----------------------------------

# In this model we will use the values obtained in the backward simulation to 
# simulate a tadpole population trying to achieve maximum fitness at the end of 
# the metamorphosis period. At each time step, the organism either lives or dies. 
# If it lives, it finds food (size and/or performance can influence how much 
# food it finds) and this is spent on growing or moving faster. 

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
      
      print("Performance")
      
    } else if (ForageRule[i, j, t] == FALSE) {
      
      i <- i + 1
      
      print("Growth")
      
    }
    
    t <- t + 1 
    
  } else {
    
    i <- 1
    
    print("Dead")
    
    break
    
  } # if/else loop
  
}

print(Fitness[i, j, t])




