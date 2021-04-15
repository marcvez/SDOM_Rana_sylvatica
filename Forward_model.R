

#---------------------------- Forward model ----------------------------------

# In this model we will use the values obtained in the backward simulation to 
# simulate a tadpole population trying to achieve maximum fitness at the end of 
# the metamorphosis period. At each time step, the organism either lives or dies. 
# If it lives, it finds food (size and/or performance can influence how much 
# food it finds) and this is spent on growing or moving faster. 


N <- 100
# Number of Tadpoles

Population <- matrix(nrow = N, ncol = time_steps + 3)
rownames(Population) <- c(1:N) # Tadpole number xx
colnames(Population) <- c((1:time_steps), "Size", "Performance", "Fitness") # Time step xx
# This matrix is going to store the state of each tadpole during the whole
# period. It is going to be read by the "Alive" matrix, to determine how many 
# individuals are still alive at each time step. The last columns store the Size,
# Performance and fitness that tadpoles have at the last time step.


Alive <- as.data.frame(matrix(nrow = time_steps + 1, ncol = 2))
colnames(Alive) <- c("time_step", "N_alive")
Alive[, 1] <- c(0:time_steps)
Alive[1, 2] <- N
# We are going to sum the number of dead or alive individuals at each time step.
# For ploting.

Tadpole <- c(1:N)
State <- c("Size", "Performance", "Fitness")
Time <- c(0:time_steps)
Tadpole_state <- array(NA, dim = c(N, 3, time_steps + 1), dimnames=list (Tadpole, State, Time))
Tadpole_state[, 1, 1] <- Size[2]
Tadpole_state[, 2, 1] <- Performance[1]
Tadpole_state[, 3, 1] <- Fitness[1,1,1]
# This array stores the Size, Performance and Expected Fitness values of each tadpole at each time step.

Performance_forw <- Performance

for (n in 1:N) {
  
  i <- 2
  j <- 1
  t <- 1
  Size[i] <- Size[2]
  Performance_forw[j] <- Performance[1]
  
  # Initial condition, after hatching (This could be a little bit stochastic)
  
  while (t <= time_steps) {
    
    Prob_Survive <- runif(1)
    # Random value that determines if you live or you die.
    
    if (Prob_Survive < Survival[i, j]) {
      # If you survive...
      
      # This is how it's going to Work: If at this time step and combination of Size and Performance, on your ForageRule array it says that you should invest in performance (TRUE), you add 1 to this state (j). If it says that you should invest in growth (FALSE), you add one to the growth state (i).
      # Then you write 1 on the Population matrix, in the correct time step and tadpole ID.
  
      if (ForageRule[i, j, t] == TRUE & i == max_Size & j == max_Performance) {
    
        j <- j
        
        Population[n, t] <- 1
        
      } else if (ForageRule[i, j, t] == FALSE & i == max_Size & j == max_Performance) {
        
        i <- i
        
        Population[n, t] <- 1
        
      } # MAx Performance and MAX Size -> You stay the same
      
      
      else if (ForageRule[i, j, t] == TRUE & i < max_Size & j == max_Performance) {
        
        j <- j
        
        Population[n, t] <- 1
        
      } else if (ForageRule[i, j, t] == FALSE & i < max_Size & j == max_Performance) {
        
        i <- i + 1
        
        Population[n, t] <- 1
        
      } # MAX Performance but Size < Max -> You can grow, but not increase your Performance
      
      
      else if (ForageRule[i, j, t] == TRUE & i == max_Size & j < max_Performance) {
        
        j <- j + 1
        
        Population[n, t] <- 1
        
      } else if (ForageRule[i, j, t] == FALSE & i == max_Size & j < max_Performance) {
        
        i <- i
        
        Population[n, t] <- 1
        
      } # MAX Size but Performance < Max -> You can't grow, but you can improve Performance
      
      
      else if (ForageRule[i, j, t] == TRUE & i < max_Size & j < max_Performance) {
        
        j <- j + 1
        
        Population[n, t] <- 1
        
      } else if (ForageRule[i, j, t] == FALSE & i < max_Size & j < max_Performance) {
       
        i <- i + 1
       
        Population[n, t] <- 1
       
      } # Normal situation, you can always Grow or improve Performance
      
      
      
    } else {
      # On the contrary, if you are dead,
      
      i <- 1
      j <- 1
      Performance_forw[j] <- 0
      # Your state is 1 for Survival and everything, and you are going to stay
      # this way forever.
      
      Population[n, t] <- 0
      # A 0 in the Population matrix symbolizes that you are dead
     
    } # if/else loop (dead/alive)
    
    Alive[t + 1, 2] <- sum(Population[, t])
    # Store the number of tadpoles that are alive at each time step.
    # There is a time step 0, and it's the initial population.
    
    
    Tadpole_state[n, 1, t + 1] <- Size[i]
    Tadpole_state[n, 2, t + 1] <- Performance_forw[j]
    Tadpole_state[n, 3, t + 1] <- Fitness[i, j, t]
    # We store the Size and Performance of each tadpole at each time Step
    
    
    Population[n, time_steps + 1] <- Size[i]
    Population[n, time_steps + 2] <- Performance_forw[j]
    Population[n, time_steps + 3] <- Fitness[i, j, t]
    # We write the final Size, Performance and Fitness of each Tadpole at the end 
    # of the Population matrix.
    
    t <- t + 1 
    
  } # while loop
  
  
} # for loop

print(Alive[time_steps, 2])

par(mfrow=c(1,1))

ggplot(data = Alive, aes(x = time_step, y = N_alive, group=1)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(name="Time Steps", limits=c(0, time_steps)) +
  scale_y_continuous(name="Nº tadpoles alive", limits=c(0, N))




par(mfrow=c(2,1))
par(mar=c(5.1, 4.5, 1, 6.5))

plot(1, type="l", xlab="Time Step", ylab="Size", xlim=c(1, time_steps), ylim=c(0, Size[max_Size]), xaxt = "n")
axis(1, at=1:(time_steps + 1), labels = c(0:time_steps))
for (n in 1:N) {
  lines(Tadpole_state[n, 1, ])
  
}
  
plot(1, type="l", xlab="Time Step", ylab="Performance", xlim=c(1, time_steps), ylim=c(0, Performance[max_Performance]), xaxt = "n")
axis(1, at=1:(time_steps + 1), labels = c(0:time_steps))

for (n in 1:N) {
  lines(Tadpole_state[n, 2, ])
  
}


