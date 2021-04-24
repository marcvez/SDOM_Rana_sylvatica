

#---------------------------- Forward model ----------------------------------

# In this model we will use the values obtained in the backward simulation to 
# simulate a tadpole population trying to achieve maximum fitness at the end of 
# the metamorphosis period. At each time step, the organism either lives or dies. 
# If it lives, it finds food (size and/or performance can influence how much 
# food it finds) and this is spent on growing or moving faster. 

library(ggplot2)
library(Hmisc)

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
    Inner_time <- 1
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
      
      # Bad Tº
      
      if (Prob_Survive < Survival[i, j] & Temperature > prob_good_temp & Inner_time < time_steps) {
        # If you survive...
        
        # This is how it's going to Work: If at this time step and combination of
        # Size and Performance, on your ForageRule array it says that you should 
        # invest in performance (TRUE), you add 1 to this state (j). If it says 
        # that you should invest in growth (FALSE), you add one to the growth state (i).
        # Then you write 1 on the Population matrix, in the correct time step and tadpole ID.
        
          
          if (ForageRule[i, j, t] == TRUE & i == max_Size & j == max_Performance) {
            
            j <- j
            
            Population[n, t] <- 1
            
          } else if (ForageRule[i, j, t] == FALSE & i == max_Size & j == max_Performance) {
            
            i <- i
            
            Population[n, t] <- 1
            
          } # MAx Performance and MAX Size -> You stay the same
          
          
          else if (ForageRule[i, j, t] == TRUE & i < max_Size & j == max_Performance & i > 1) {
            
            j <- j
            
            Population[n, t] <- 1
            
          } else if (ForageRule[i, j, t] == FALSE & i < max_Size & j == max_Performance & i > 1) {
            
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
          
          
          else if (ForageRule[i, j, t] == TRUE & i < max_Size & j < max_Performance & i > 1) {
            
            j <- j + 1
            
            Population[n, t] <- 1
            
          } else if (ForageRule[i, j, t] == FALSE & i < max_Size & j < max_Performance & i > 1) {
            
            i <- i + 1
            
            Population[n, t] <- 1
            
          } # Normal situation, you can always Grow or improve Performance
        
        
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
        Inner_time <- Inner_time + 1
          
      }
        
      # Good Tº
        
      else if (Prob_Survive < Survival[i, j] & Temperature < prob_good_temp & Inner_time < time_steps){
          
          
          if (ForageRule[i, j, t] == TRUE & i == max_Size & j == max_Performance) {
            
            j <- j
            
            Population[n, t] <- 1
            
          } else if (ForageRule[i, j, t] == FALSE & i == max_Size & j == max_Performance) {
            
            i <- i
            
            Population[n, t] <- 1
            
          } # MAx Performance and MAX Size -> You stay the same
          
          
          else if (ForageRule[i, j, t] == TRUE & i < max_Size - 1 & j == max_Performance & i > 1) {
            
            j <- j
            
            Population[n, t] <- 1
            
          } else if (ForageRule[i, j, t] == FALSE & i < max_Size - 1 & j == max_Performance & i > 1) {
            
            i <- i + 2
            
            Population[n, t] <- 1
            
          } # MAX Performance but Size < Max -> You can grow, but not increase your Performance
          
          
          else if (ForageRule[i, j, t] == TRUE & i == max_Size - 1 & j == max_Performance & i > 1) {
            
            j <- j
            
            Population[n, t] <- 1
            
          } else if (ForageRule[i, j, t] == FALSE & i == max_Size - 1 & j == max_Performance & i > 1) {
            
            i <- i + 1
            
            Population[n, t] <- 1
            
          } # MAX Performance but Size = Max - 1 -> You can grow(only 1), but not increase your Performance
          
          
          else if (ForageRule[i, j, t] == TRUE & i == max_Size - 1 & j < max_Performance & i > 1) {
            
            j <- j + 1
            
            Population[n, t] <- 1
            
          } else if (ForageRule[i, j, t] == FALSE & i == max_Size - 1 & j == max_Performance & i > 1) {
            
            i <- i + 1
            
            Population[n, t] <- 1
            
          } # Performance < Max and Size = Max - 1 -> You can grow (only 1), and increase your Performance
          
          
          else if (ForageRule[i, j, t] == TRUE & i == max_Size & j < max_Performance) {
            
            j <- j + 1
            
            Population[n, t] <- 1
            
          } else if (ForageRule[i, j, t] == FALSE & i == max_Size & j < max_Performance) {
            
            i <- i
            
            Population[n, t] <- 1
            
          } # MAX Size but Performance < Max -> You can't grow, but you can improve Performance
          
          
          else if (ForageRule[i, j, t] == TRUE & i < max_Size - 1 & j < max_Performance & i > 1) {
            
            j <- j + 1
            
            Population[n, t] <- 1
            
          } else if (ForageRule[i, j, t] == FALSE & i < max_Size - 1 & j < max_Performance & i > 1) {
            
            i <- i + 2
            
            Population[n, t] <- 1
            
          } # Normal situation, you can always Grow or improve Performance
        
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
        
        prob_jump <- runif(1)
        
        if(prob_jump < prob_good_temp) { 
          
          Inner_time <- Inner_time + 2
          
        } else {
          
          Inner_time <- Inner_time + 1
          
        } # if/else prob jump
        
        
        Inner_time <- min(Inner_time, time_steps)
          
      }
      
      # Inner_time == time_steps
      
      else if (Inner_time == time_steps & Prob_Survive < Survival[i, j]) { # These frogs don't die, for the moment
        
        Population[n, t] <- 1
        
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
      
      } # if Inner_time == time_steps
      
      else {
        # On the contrary, if you are dead,
        
        i <- 1
        j <- 1
        Performance_forw[j] <- 0
        # Your state is 1 for Survival and everything, and you are going to stay
        # this way forever.
        
        Population[n, t] <- 0
        # A 0 in the Population matrix symbolizes that you are dead
        
        
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
        
      } # if/else loop (dead/alive)
      
      
    } # while loop
    
    
  } # for loop
  
  print("How many tadpoles are still alive?")
  print(Alive[time_steps, 2])
  
  assign("Alive", Alive, envir = globalenv())
  assign("Tadpole_state", Tadpole_state, envir = globalenv())
  assign("Population", Population, envir = globalenv())
  
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


# Plot that displays the final values of the tadpoles' traits
# Vertical lines intercept those tadpoles that have all 3 traits maximized

Final_traits_plot <- function(){
  
  Final_Fitness <- (Tadpole_state[, 3, time_steps + 1])
  
  Final_Size <- (Tadpole_state[, 1, time_steps + 1])
  
  Final_Performance <- (Tadpole_state[, 2, time_steps + 1])
  
  Final_results <- cbind(Final_Size, Final_Performance, Final_Fitness)
  
  Max_Condition <- as.vector(which(Final_results[, 1] == max(Size) & Final_results[, 2] == max(Performance) & Final_results[, 3] == max(Final_results[, 3])))
  
  par(mfrow=c(1,1))
  par(mar=c(5.1, 4.5, 3, 4.5))
  
  plot(Final_results[, 2], pch = 19, col = '#440154FF', xlim = c(0, N + 16), 
       main = "Size, Burst speed and Fitness at the end of metamorphosis", 
       ylab = "Size (cm), Burst speed (cm/s) and Fitness", xlab = "Tadpole")
  points(Final_results[, 1], pch = 19, col = "#21908CFF")
  points(Final_results[, 3], pch = 19, col = '#FDE725FF',)
  abline(h = max(Size), lty = 2)
  abline(h = max(Performance), lty = 2)
  abline(h = max(Fitness_values), lty = 2)
  abline(v = c(Max_Condition), lty = 2)
  legend("bottomright", legend=c("Burst speed", "Size", "Fitness"),
         pch = c(19, 19, 19), col = c('#440154FF', '#21908CFF', '#FDE725FF'), lty=2, cex=0.8)
  
  assign("Final_results", Final_results, envir = globalenv())
  assign("Max_Condition", Max_Condition, envir = globalenv())
  
  
}


# Density plots of the final values of the traits

Density_plot <- function(){
  
  par(mfrow=c(3,2))
  par(mar=c(5.1, 4.5, 3, 4.5))
  
  plot(density(Final_results[,1], bw = 0.01, from = -0.1, to = max(Size) + 0.1), main = "Final Size (cm)")
  rug(Final_results[,1], col='red')
  
  plot(density(Final_results[,1], bw = 0.01, from = 4, to = max(Size) + 0.1), main = "Final Size (cm) (Alive)")
  rug(Final_results[,1], col='red')
  
  plot(density(Final_results[,2], bw = 0.01, from = -0.1, to = max(Performance) + 0.1), main = "Final Burst Speed (cm/s)")
  rug(Final_results[,2], col='red')
  
  plot(density(Final_results[,2], bw = 0.01, from = 5, to = max(Performance) + 0.1), main = "Final Burst Speed (cm/s) (Alive)")
  rug(Final_results[,2], col='red')
  
  plot(density(Final_results[,3], bw = 0.01, from = -0.1, to = max(Fitness_values) + 0.1), main = "Final Fitness")
  rug(Final_results[,3], col='red')
  
  plot(density(Final_results[,3], bw = 0.01, from = 3, to = max(Fitness_values) + 0.1), main = "Final Fitness (Alive)")
  rug(Final_results[,3], col='red')
  
}


# Histogram showing the number of tadpoles in each Fitness value

Histogram_plot <- function(){
  
  par(mfrow=c(1,1))
  hist(Tadpole_state[, 3, time_steps + 1], 
       main="Fitness Histogram", 
       xlab="Fitness values", 
       xlim=c(0,max(Fitness_values)),
       las=1, 
       breaks=1000)
  minor.tick(nx=10, ny=10)
  
  
  abline(h = length(Max_Condition), lty = 2)
  
}

# Initial parameters



N <- 100
# Number of Tadpoles


# Plot

Forward(N)

Survival_plot()

Investment_plot()

Final_traits_plot()

Density_plot()

Histogram_plot()
# Horizontal line represents the number of tadpoles with all 3 traits maximized






















# Ideas (in Spanish, don't pay attention to this)

# Cambiar valores de fitness!! Ya no hay la misma fitness para tallas grandes, sino que siempre augmenta.
# Esto podria modificar backwards simulation y que casi no se invierta en performance... (HECHO)

# Tambien, los valores que tengo puestos como performance son los valores max de 
# performance observados en estos renacuajos, pero para llegar a estos, han tenido 
# que pasar por velocidades inferiores! Deberia augmentar el numero de breaks de 
# performance o cambiar los valores y empezar antes, sin cambiar el numero de breaks. 
# Eso deberia modificar tambien la backeards simulation. (HECHO)

#Revisar dos puntos anteriores



# Hacer un mapa 3D (heat map) donde la intensidad sea la fitness, y los ejes x e y los valores de performance y talla.
# Faltaria el numero de renacuajos en cada uno de esos puntos (4D?)
# Heat map con colores suelen ser de 2D, y el color representa veces que se ha llegado a la misma combinación de valores.



library(ggplot2)
library(dplyr)
library(hrbrthemes)

Final_Fitness <- as.data.frame(Final_Fitness)

# Make the histogram
Final_Fitness %>%
  filter( Final_Fitness<max(Final_Fitness)) %>%
  ggplot( aes(x=Final_Fitness)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
  ggtitle("Night price distribution of Airbnb appartements") +
  theme_ipsum()
