

#---------------------------- Forward model ----------------------------------

# In this model we will use the values obtained in the backward simulation to 
# simulate a tadpole population trying to achieve maximum fitness at the end of 
# the metamorphosis period. At each time step, the organism either lives or dies. 
# If it lives, it finds food (size and/or performance can influence how much 
# food it finds) and this is spent on growing or moving faster. 

library(ggplot2)

# Forward simulation

Forward <- function(N) {
  
  Population <- matrix(nrow = N, ncol = time_steps + 3)
  rownames(Population) <- c(1:N) # Tadpole number xx
  colnames(Population) <- c((1:time_steps), "Size", "Performance", "Fitness") # Time step xx
  # This matrix is going to store the state of each tadpole during the whole
  # period, being 1 the value of being alive and 0, being dead.. 
  # It is going to be read by the "Alive" matrix, to determine how many 
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
  # This array stores the Size, Performance and Expected Fitness values of each tadpole at each time step.
  
  Performance_forw <- Performance
  # This line exists because it was the only way I've found to not mess up the Performance array
  
  for (n in 1:N) {
    
    i <- sample(2:4,1)
    j <- sample(1:3,1)
    t <- 1
    # Initial condition, after hatching (This could be a little bit stochastic)
    
    Tadpole_state[n, 1, t] <- Size[i]
    Tadpole_state[n, 2, t] <- Performance[j]
    Tadpole_state[n, 3, t] <- Fitness[i,j,t]
    # We fix the initial condition in the Tadpole_state array
    
    Performance_forw[j] <- Performance[j]
    # This line exists because it was the only way I've found to not mess up the Performance array
    
    while (t <= time_steps) {
      
      Prob_Survive <- runif(1)
      # Random value that determines if you live or you die.
      
      Temperature <- runif(1)
      
      if (Prob_Survive < Survival[i, j]) {
        # If you survive...
        
        # This is how it's going to Work: If at this time step and combination of
        # Size and Performance, on your ForageRule array it says that you should 
        # invest in performance (TRUE), you add 1 to this state (j). If it says 
        # that you should invest in growth (FALSE), you add one to the growth state (i).
        # Then you write 1 on the Population matrix, in the correct time step and tadpole ID.
        
        # Bad Tº
        
        if (ForageRule[i, j, t] == TRUE & i == max_Size & j == max_Performance & Temperature > prob_good_temp) {
          
          j <- j
          
          Population[n, t] <- 1
          
        } else if (ForageRule[i, j, t] == FALSE & i == max_Size & j == max_Performance & Temperature > prob_good_temp) {
          
          i <- i
          
          Population[n, t] <- 1
          
        } # MAx Performance and MAX Size -> You stay the same
        
        
        else if (ForageRule[i, j, t] == TRUE & i < max_Size & j == max_Performance & i > 1 & Temperature > prob_good_temp) {
          
          j <- j
          
          Population[n, t] <- 1
          
        } else if (ForageRule[i, j, t] == FALSE & i < max_Size & j == max_Performance & i > 1 & Temperature > prob_good_temp) {
          
          i <- i + 1
          
          Population[n, t] <- 1
          
        } # MAX Performance but Size < Max -> You can grow, but not increase your Performance
        
        
        else if (ForageRule[i, j, t] == TRUE & i == max_Size & j < max_Performance & Temperature > prob_good_temp) {
          
          j <- j + 1
          
          Population[n, t] <- 1
          
        } else if (ForageRule[i, j, t] == FALSE & i == max_Size & j < max_Performance & Temperature > prob_good_temp) {
          
          i <- i
          
          Population[n, t] <- 1
          
        } # MAX Size but Performance < Max -> You can't grow, but you can improve Performance
        
        
        else if (ForageRule[i, j, t] == TRUE & i < max_Size & j < max_Performance & i > 1 & Temperature > prob_good_temp) {
          
          j <- j + 1
          
          Population[n, t] <- 1
          
        } else if (ForageRule[i, j, t] == FALSE & i < max_Size & j < max_Performance & i > 1 & Temperature > prob_good_temp) {
          
          i <- i + 1
          
          Population[n, t] <- 1
          
        } # Normal situation, you can always Grow or improve Performance
        
        
        
        # Good Tº
        
        
         else if (ForageRule[i, j, t] == TRUE & i == max_Size & j == max_Performance & Temperature < prob_good_temp) {
          
          j <- j
          
          Population[n, t] <- 1
          
        } else if (ForageRule[i, j, t] == FALSE & i == max_Size & j == max_Performance & Temperature < prob_good_temp) {
          
          i <- i
          
          Population[n, t] <- 1
          
        } # MAx Performance and MAX Size -> You stay the same
        
        
        else if (ForageRule[i, j, t] == TRUE & i < max_Size - 1 & j == max_Performance & i > 1 & Temperature < prob_good_temp) {
          
          j <- j
          
          Population[n, t] <- 1
          
        } else if (ForageRule[i, j, t] == FALSE & i < max_Size - 1 & j == max_Performance & i > 1 & Temperature < prob_good_temp) {
          
          i <- i + 2
          
          Population[n, t] <- 1
          
        } # MAX Performance but Size < Max -> You can grow, but not increase your Performance
        
        
        else if (ForageRule[i, j, t] == TRUE & i == max_Size - 1 & j == max_Performance & i > 1 & Temperature < prob_good_temp) {
          
          j <- j
          
          Population[n, t] <- 1
          
        } else if (ForageRule[i, j, t] == FALSE & i == max_Size - 1 & j == max_Performance & i > 1 & Temperature < prob_good_temp) {
          
          i <- i + 1
          
          Population[n, t] <- 1
          
        } # MAX Performance but Size = Max - 1 -> You can grow(only 1), but not increase your Performance
        
        
        else if (ForageRule[i, j, t] == TRUE & i == max_Size - 1 & j < max_Performance & i > 1 & Temperature < prob_good_temp) {
          
          j <- j + 1
          
          Population[n, t] <- 1
          
        } else if (ForageRule[i, j, t] == FALSE & i == max_Size - 1 & j == max_Performance & i > 1 & Temperature < prob_good_temp) {
          
          i <- i + 1
          
          Population[n, t] <- 1
          
        } # Performance < Max and Size = Max - 1 -> You can grow (only 1), and increase your Performance
        
        
        else if (ForageRule[i, j, t] == TRUE & i == max_Size & j < max_Performance & Temperature < prob_good_temp) {
          
          j <- j + 1
          
          Population[n, t] <- 1
          
        } else if (ForageRule[i, j, t] == FALSE & i == max_Size & j < max_Performance & Temperature < prob_good_temp) {
          
          i <- i
          
          Population[n, t] <- 1
          
        } # MAX Size but Performance < Max -> You can't grow, but you can improve Performance
        
        
        else if (ForageRule[i, j, t] == TRUE & i < max_Size - 1 & j < max_Performance & i > 1 & Temperature < prob_good_temp) {
          
          j <- j + 1
          
          Population[n, t] <- 1
          
        } else if (ForageRule[i, j, t] == FALSE & i < max_Size - 1 & j < max_Performance & i > 1 & Temperature < prob_good_temp) {
          
          i <- i + 2
          
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
  
  print("How many tadpoles are still alive?")
  print(Alive[time_steps, 2])
  
  assign("Alive", Alive, envir = globalenv())
  assign("Tadpole_state", Tadpole_state, envir = globalenv())
  
}


# Survival plot

Survival_plot <- function() {
  
  par(mfrow=c(1,1))
  par(mar=c(0, 0, 0, 0))
  
  ggplot(data = Alive, aes(x = time_step, y = N_alive, group=1)) +
    geom_line() +
    geom_point() +
    scale_x_continuous(name="Time Steps", limits=c(0, time_steps)) +
    scale_y_continuous(name="Nº tadpoles alive", limits=c(0, N))
  
}


# Investment and fitness plot

Investment_plot <- function() {
  
  par(mfrow=c(3,1))
  par(mar=c(5.1, 4.5, 1, 4.5))
  
  plot(1, type="l", xlab="Time Step", ylab="Size (cm)", xlim=c(1, time_steps), ylim=c(0, Size[max_Size]), xaxt = "n")
  axis(1, at=1:(time_steps + 1), labels = c(0:time_steps))
  for (n in 1:N) {
    lines(Tadpole_state[n, 1, ])
    
  } # Size
  
  plot(1, type="l", xlab="Time Step", ylab="Burst speed (cm/s)", xlim=c(1, time_steps), ylim=c(0, Performance[max_Performance]), xaxt = "n")
  axis(1, at=1:(time_steps + 1), labels = c(0:time_steps))
  
  for (n in 1:N) {
    lines(Tadpole_state[n, 2, ])
    
  } # Performance
  
  plot(1, type="l", xlab="Time Step", ylab="Fitness", xlim=c(1, time_steps), ylim=c(0, max(Fitness)), xaxt = "n")
  axis(1, at=1:(time_steps + 1), labels = c(0:time_steps))
  
  for (n in 1:N) {
    lines(Tadpole_state[n, 3, ])
    
  } # Fitness
  
}

# Initial parameters

N <- 10
# Number of Tadpoles


# Plot

Forward(N)

Survival_plot()

Investment_plot()




# Heatmap Survival (?)

ggplot(Survival, aes(NA, NA, fill = Survival)) + 
  geom_tile()

install.packages("pheatmap")

library(pheatmap)
par(mar = c(2, 2, 2, 2))
pheatmap(Survival[2:max_Size,], cluster_rows = FALSE, cluster_cols = FALSE)

