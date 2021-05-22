

#---------------------------- Forward model ----------------------------------

# In this model we will use the values obtained in the backward simulation to 
# simulate a tadpole population trying to achieve maximum fitness at the end of 
# the metamorphosis period. At each time step, the organism either lives or dies. 
# If it lives, it finds food and this is spent on growing or moving faster.Each day,
# the temperature can be good or bad, and this triggers a sequence of probabilities
# that determine how much does the tadpole grow. At the end, we will get a series
# of plots that are going to represent the variety of possible final phenotypes and
# fitness.

library(ggplot2)
library(Hmisc)

# Forward simulation
Forward <- function(N) {
  
  Population <- matrix(nrow = N, ncol = time_steps + 3)
  rownames(Population) <- c(1:N) # Tadpole number xx
  colnames(Population) <- c((1:time_steps), "Size", "Performance", "Fitness") # Time step xx
  # This matrix is going to store the state of each tadpole during the whole
  # period, being 1 the value of being alive and 0, being dead. 
  # It is going to be read by the "Alive" matrix, to determine how many 
  # individuals are still alive at each time step. The last columns store the Size,
  # Performance and Fitness that tadpoles have at the last time step.
  
  
  Alive <- as.data.frame(matrix(nrow = time_steps + 1, ncol = 2))
  colnames(Alive) <- c("time_step", "N_alive")
  Alive[, 1] <- c(0:time_steps)
  Alive[1, 2] <- N
  # We are going to sum the number of dead or alive individuals at each time step.
  # For plotting.
  
  Tadpole <- c(1:N)
  State <- c("Size", "Performance", "Fitness")
  Time <- c(0:time_steps)
  Tadpole_state <- array(NA, dim = c(N, 3, time_steps + 1), dimnames=list (Tadpole, State, Time))
  # This array stores the Size, Performance and Expected Fitness values of each tadpole at each time step.
  
  Performance_forw <- Performance
  # This line exists because it was the only way I've found to not mess up the Performance array
  
  
  for (n in 1:N) {
    
    i <- 2 # sample(2:4,1)
    j <- 1 + prob_bad_temp * 10 # sample(1:3,1)
    k <- 1
    t <- 1
    
    Tadpole_state[n, 1, t] <- Size[i]
    Tadpole_state[n, 2, t] <- Performance[j]
    Tadpole_state[n, 3, t] <- Fitness[i,j,k,t]
    # We fix the initial condition in the Tadpole_state array
    
    Performance_forw[j] <- Performance[j]
    # This line exists because it was the only way I've found to not mess up the Performance array
    
    while (t < time_steps) {
      
      Prob_Survive <- runif(1)
      # Random value that determines if you live or you die.
      
      Temperature <- runif(1)
      # Random value that is going to determine if the day is good or bad, in terms of temperature
      
      Forage <- runif(1)
      # Probability of jump one time_step ahead if the temperature is good, and it depends on the 
      # temperature itself. The warmer, the higher development rate you have.
      
      if(Prob_Survive < Survival[i, j]){
        
        if (ForageRule_B[i, j, k, t] == FALSE) {
          
          if (Temperature >= prob_good_temp & Forage < Condition[i, j]) {
          # If you survive, Tº is bad and you find food...
          
          # This is how it's going to Work: If at this time step and combination of
          # Size and Performance, on your ForageRule array it says that you should 
          # invest in performance (TRUE), you add 1 to this state (j). If it says 
          # that you should invest in growth (FALSE), you add one to the growth state (i).
          # Then you write 1 on the Population matrix, in the correct time step and tadpole ID.
          
          # Now there are many lines of code due to the large combination of possible 
          # situations, but they all work in the same way. They are divided first of 
          # all according to whether you live or die. Then there are the decisions to 
          # be made if there is a bad temperature. Finally there are the decisions to 
          # be made if the temperature is good. Within this last block you may or may 
          # not jump out of time_step, and that divides the decisions into two parts. 
          
          
          # Bad Tº
          
          if (ForageRule[i, j, k, t] == TRUE & i == max_Size & j == max_Performance & i > 1) {
            
            j <- j
            
            Population[n, t] <- 1
            
          } else if (ForageRule[i, j, k, t] == FALSE & i == max_Size & j == max_Performance & i > 1) {
            
            i <- i
            
            Population[n, t] <- 1
            
          } # MAx Performance and MAX Size -> You stay the same
          
          
          else if (ForageRule[i, j, k, t] == TRUE & i < max_Size & j == max_Performance & i > 1) {
            
            j <- j
            
            Population[n, t] <- 1
            
          } else if (ForageRule[i, j, k, t] == FALSE & i < max_Size & j == max_Performance & i > 1) {
            
            i <- i
            
            Population[n, t] <- 1
            
          } # MAX Performance but Size < Max -> You can grow, but not increase your Performance
          
          
          else if (ForageRule[i, j, k, t] == TRUE & i == max_Size & j < max_Performance & i > 1) {
            
            j <- j + 1
            
            Population[n, t] <- 1
            
          } else if (ForageRule[i, j, k, t] == FALSE & i == max_Size & j < max_Performance & i > 1) {
            
            i <- i
            
            Population[n, t] <- 1
            
          } # MAX Size but Performance < Max -> You can't grow, but you can improve Performance
          
          
          else if (ForageRule[i, j, k, t] == TRUE & i < max_Size & j < max_Performance & i > 1) {
            
            j <- j + 1
            
            Population[n, t] <- 1
            
          } else if (ForageRule[i, j, k, t] == FALSE & i < max_Size & j < max_Performance & i > 1) {
            
            i <- i
            
            Population[n, t] <- 1
            
          } # Normal situation, you can always Grow or improve Performance
          
          
          Alive[t + 1, 2] <- sum(Population[, t])
          # Store the number of tadpoles that are alive at each time step.
          # There is a time step 0, and it's the initial population.
          
          Tadpole_state[n, 1, t + 1] <- Size[i]
          Tadpole_state[n, 2, t + 1] <- Performance_forw[j]
          Tadpole_state[n, 3, t + 1] <- Fitness[i, j, k, t]
          # We store the Size and Performance of each tadpole at each time Step
          
          
          Population[n, time_steps + 1] <- Size[i]
          Population[n, time_steps + 2] <- Performance_forw[j]
          Population[n, time_steps + 3] <- Fitness[i, j, k, t]
          # We write the final Size, Performance and Fitness of each Tadpole at the end 
          # of the Population matrix.
          
          t <- t + 1
          # Both the time to the end of the season and the internal time itself, 
          # which measures how far along you are in the development process, advance equally.
          
        }
          
          
          
          # Good Tº
          
          else if (Temperature <= prob_good_temp & Forage < Condition[i, j]){
            
            
            if (ForageRule[i, j, k, t] == TRUE & i == max_Size & j == max_Performance & i > 1) {
              
              j <- j
              
              Population[n, t] <- 1
              
            } else if (ForageRule[i, j, k, t] == FALSE & i == max_Size & j == max_Performance & i > 1) {
              
              i <- i
              
              Population[n, t] <- 1
              
            } # MAx Performance and MAX Size -> You stay the same
            
            
            else if (ForageRule[i, j, k, t] == TRUE & i == max_Size & j < max_Performance & i > 1) {
              
              j <- j + 1
              
              Population[n, t] <- 1
              
            } else if (ForageRule[i, j, k, t] == FALSE & i == max_Size & j < max_Performance & i > 1) {
              
              i <- i
              
              Population[n, t] <- 1
              
            } # MAX Size but Performance < Max -> You can't grow, but you can improve Performance
            
            
            else if (ForageRule[i, j, k, t] == TRUE & i < max_Size & j == max_Performance & i > 1) {
              
              j <- j
              
              Population[n, t] <- 1
              
            } else if (ForageRule[i, j, k, t] == FALSE & i < max_Size & j == max_Performance & i > 1) {
              
              i <- i + 1
              
              Population[n, t] <- 1
              
            } # MAX Performance but Size < Max -> You can grow, but not increase your Performance
            
            
            else if (ForageRule[i, j, k, t] == TRUE & i < max_Size & j < max_Performance & i > 1) {
              
              j <- j + 1
              
              Population[n, t] <- 1
              
            } else if (ForageRule[i, j, k, t] == FALSE & i < max_Size & j < max_Performance & i > 1) {
              
              i <- i + 1
              
              Population[n, t] <- 1
              
            } # Normal situation, you can always Grow or improve Performance
            
            
            Alive[t + 1, 2] <- sum(Population[, t])
            # Store the number of tadpoles that are alive at each time step.
            # There is a time step 0, and it's the initial population.
            
            Tadpole_state[n, 1, t + 1] <- Size[i]
            Tadpole_state[n, 2, t + 1] <- Performance_forw[j]
            Tadpole_state[n, 3, t + 1] <- Fitness[i, j, k, t]
            # We store the Size and Performance of each tadpole at each time Step
            
            
            Population[n, time_steps + 1] <- Size[i]
            Population[n, time_steps + 2] <- Performance_forw[j]
            Population[n, time_steps + 3] <- Fitness[i, j, k, t]
            # We write the final Size, Performance and Fitness of each Tadpole at the end 
            # of the Population matrix.
            
            
            t <- t + 1
            
            # In this case, the external time (the season of development advances) but 
            # your internal time advances at the same time or is advanced, and depends on a probability.
            
            
          }
          
          else if (Forage > Condition[i, j]){
            
            # If you don't find food
            
            j <- j
            
            i <- i
            
            Population[n, t] <- 1
            
            Alive[t + 1, 2] <- sum(Population[, t])
            # Store the number of tadpoles that are alive at each time step.
            # There is a time step 0, and it's the initial population.
            
            Tadpole_state[n, 1, t + 1] <- Size[i]
            Tadpole_state[n, 2, t + 1] <- Performance_forw[j]
            Tadpole_state[n, 3, t + 1] <- Fitness[i, j, k, t]
            # We store the Size and Performance of each tadpole at each time Step
            
            
            Population[n, time_steps + 1] <- Size[i]
            Population[n, time_steps + 2] <- Performance_forw[j]
            Population[n, time_steps + 3] <- Fitness[i, j, k, t]
            # We write the final Size, Performance and Fitness of each Tadpole at the end 
            # of the Population matrix.
            
            t <- t + 1 
            
            
          }
          
          
        } # If it's not time to metamorphose
      
        if (ForageRule_B[i, j, k, t] == TRUE) {
          
          for (t in t:time_steps) {
            
            if (Prob_Survive < Survival[i, j]){
              
              i <- i
              j <- j
              k <- k + 1
              k <- min(max_Stages, k)
              
              Population[n, t] <- 1
              
              Alive[t + 1, 2] <- sum(Population[, t])
              # Store the number of tadpoles that are alive at each time step.
              # There is a time step 0, and it's the initial population.
              
              Tadpole_state[n, 1, t + 1] <- Size[i]
              Tadpole_state[n, 2, t + 1] <- Performance_forw[j]
              Tadpole_state[n, 3, t + 1] <- Fitness[i, j, k, t]
              # We store the Size and Performance of each tadpole at each time Step
              
              
              Population[n, time_steps + 1] <- Size[i]
              Population[n, time_steps + 2] <- Performance_forw[j]
              Population[n, time_steps + 3] <- Fitness[i, j, k, t]
              # We write the final Size, Performance and Fitness of each Tadpole at the end 
              # of the Population matrix.
              
              t <- t + 1
              
              Prob_Survive <- runif(1)
              
            } else if (Prob_Survive > Survival[i, j]){
              
              i <- 1
              j <- 1
              k <- 1
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
              Tadpole_state[n, 3, t + 1] <- Fitness[i, j, k, t]
              # We store the Size and Performance of each tadpole at each time Step
              
              
              Population[n, time_steps + 1] <- Size[i]
              Population[n, time_steps + 2] <- Performance_forw[j]
              Population[n, time_steps + 3] <- Fitness[i, j, k, t]
              # We write the final Size, Performance and Fitness of each Tadpole at the end 
              # of the Population matrix.
              
              
              t <- t + 1
              
              Prob_Survive <- 1
              
            } # Dead while metamorphosing
           
          } # Loop metamorphosis
        
        } # If metamorphosis
        
      } # If you survive
      
      
      else {
        # On the contrary, if you are dead
        
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
        Tadpole_state[n, 3, t + 1] <- Fitness[i, j, k, t]
        # We store the Size and Performance of each tadpole at each time Step
        
        
        Population[n, time_steps + 1] <- Size[i]
        Population[n, time_steps + 2] <- Performance_forw[j]
        Population[n, time_steps + 3] <- Fitness[i, j, k, t]
        # We write the final Size, Performance and Fitness of each Tadpole at the end 
        # of the Population matrix.
        
        
        t <- t + 1
        
      } # if/else loop (dead/alive)
      
      
    } # while loop
    
    if (t == time_steps & k < 10) {
      
      i <- 1
      j <- 1
      k <- 1
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
      Tadpole_state[n, 3, t + 1] <- 0
      # We store the Size and Performance of each tadpole at each time Step
      
      
      Population[n, time_steps + 1] <- Size[i]
      Population[n, time_steps + 2] <- Performance_forw[j]
      Population[n, time_steps + 3] <- 0
      # We write the final Size, Performance and Fitness of each Tadpole at the end 
      # of the Population matrix.
      
      
    } else if (t == time_steps & k == 10){
      
      i <- i
      j <- j
      k <- k
      
      Population[n, t] <- 1
      # A 0 in the Population matrix symbolizes that you are dead
      
      
      Alive[t + 1, 2] <- sum(Population[, t])
      # Store the number of tadpoles that are alive at each time step.
      # There is a time step 0, and it's the initial population.
      
      Tadpole_state[n, 1, t + 1] <- Size[i]
      Tadpole_state[n, 2, t + 1] <- Performance_forw[j]
      Tadpole_state[n, 3, t + 1] <- Fitness[i, j, k, t]
      # We store the Size and Performance of each tadpole at each time Step
      
      
      Population[n, time_steps + 1] <- Size[i]
      Population[n, time_steps + 2] <- Performance_forw[j]
      Population[n, time_steps + 3] <- Fitness[i, j, k, t]
      # We write the final Size, Performance and Fitness of each Tadpole at the end 
      # of the Population matrix.
      
    }
    
    
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
  Max_Condition <- as.vector(which(Final_results[, 3] == max(Final_results[, 3])))
  assign("Final_Fitness", Final_Fitness, envir = globalenv())
  assign("Final_Size", Final_Size, envir = globalenv())
  assign("Final_Performance", Final_Performance, envir = globalenv())
  assign("Final_results", Final_results, envir = globalenv())
  assign("Max_Condition", Max_Condition, envir = globalenv())
  # This is useful for other plots
  
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
  plot(1, type="l", xlab="Time Step", ylab="Fitness", xlim=c(1, time_steps), ylim=c(0, max(Fitness[, , max_Stages, time_steps + 1])), xaxt = "n")
  axis(1, at=1:(time_steps + 1), labels = c(0:time_steps))
  
  for (n in 1:N) {
    lines(Tadpole_state[n, 3, ])
    
  } # Fitness
  
}

# Plot that displays the final values of the tadpoles' traits
# Vertical lines intercept those tadpoles that have the better fitness
Final_traits_plot <- function(){
  
  par(mfrow=c(1,1))
  par(mar=c(5.1, 4.5, 3, 4.5))
  
  plot(Final_results[, 2], pch = 19, col = '#440154FF', xlim = c(0, N), 
       ylim = c(0,max(Performance)),
       main = "Size, Burst speed and Fitness at the end of metamorphosis", 
       ylab = "Size (cm), Burst speed (cm/s) and Fitness", xlab = "Tadpole")
  points(Final_results[, 1], pch = 19, col = "#21908CFF")
  points(Final_results[, 3], pch = 19, col = '#FDE725FF',)
  abline(h = max(Size), lty = 2)
  abline(h = max(Performance), lty = 2)
  abline(h = max(Final_results[, 3]), lty = 2)
  abline(v = c(Max_Condition), lty = 2)
  legend("bottomright", legend=c("Burst speed", "Size", "Fitness"),
         pch = c(19, 19, 19), col = c('#440154FF', '#21908CFF', '#FDE725FF'), lty=2, cex=0.8)
  
  
}


# Density plots of the final values of the traits
Density_plot <- function(){
  
  par(mfrow=c(3,2))
  par(mar=c(5.1, 4.5, 3, 4.5))
  
  plot(density(Final_results[,1], bw = 0.1, from = -0.5, to = max(Size) + 0.1), main = "Final Size (cm)")
  rug(Final_results[,1], col='red')
  
  plot(density(Final_results[,1], bw = 0.1, from = 3 - 0.5, to = max(Size) + 0.1), main = "Final Size (cm) (Alive)")
  rug(Final_results[,1], col='red')
  
  plot(density(Final_results[,2], bw = 0.1, from = -0.5, to = max(Performance) + 0.4), main = "Final Burst Speed (cm/s)")
  rug(Final_results[,2], col='red')
  
  plot(density(Final_results[,2], bw = 0.1, from = 4 - 0.5, to = max(Performance) + 0.5), main = "Final Burst Speed (cm/s) (Alive)")
  rug(Final_results[,2], col='red')
  
  plot(density(Final_results[,3], bw = 0.1, from = -0.5, to = max(Fitness[, , 10, time_steps + 1]) + 0.1), main = "Final Fitness")
  rug(Final_results[,3], col='red')
  
  plot(density(Final_results[,3], bw = 0.1, from = 2 - 0.5, to = max(Fitness[, , 10, time_steps + 1]) + 0.1), main = "Final Fitness (Alive)")
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

# Comparison of three different situations, each one increasing the 
# temperature respect the previous one. 
Comparison_plot <- function(){
  
  par(mfrow=c(5,1))
  
  for(prob_good_temp in c(0.3, 0.4, 0.5, 0.6, 0.7)){
    
    prob_good_temp
    prob_bad_temp <- 1 - prob_good_temp
    days <- 60
    end_season_percentage <- 0.4  
    end_season_intensity <- 1 
    death_rate_day <- 0.012 
    N <- 100
    
    
    Decisions(prob_good_temp, prob_bad_temp, days, end_season_percentage, end_season_intensity, death_rate_day)
    
    Forward(N)
    
    Fitness_bigger_0 <- as.vector(Final_Fitness[Final_Fitness > 0])
    Performance_bigger_0 <- as.vector(Final_Performance[Final_Performance > 0])
    Size_bigger_0 <- as.vector(Final_Size[Final_Size > 0])
    
    plot(density(Final_results[,2], bw = 0.1, from = -0.5, to = max(Performance) + 0.4), col = "black", main = paste("Final Traits (blue = F, red = S, black = P) prob good temp = ", prob_good_temp))
    abline(v = mean(Performance_bigger_0))
    
    lines(density(Final_results[,1], bw = 0.1, from = 1, to = max(Size) + 0.1), col = "red")
    abline(v = mean(Size_bigger_0), col = "red")
    
    lines(density(Final_results[,3], bw = 0.1, from = 1, to = max(Fitness[, , 10, time_steps + 1]) + 0.5), col = "blue")
    abline(v = mean(Fitness_bigger_0), col = "blue")
    
    abline(h = 0)
    minor.tick(nx=10, ny=1)
    
    rm(list = ls())
    
  }
  
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

Comparison_plot()


Fitness[, , 1, 50]


# Solucionar counter-gradient performance, mirar performance inicial al salir del huevo?
# Augmento progresivo de la temperatura a medida que avanza el tiempo?








  # Ideas (in Spanish, don't pay attention to this)


# Cambiar condition values, intentar poner no_forage condition, modificar de alguna manera
# el factor prob jump para que no sea tan fuerte cuando hace calor!

# A lo mejor la probabilidad de jump no tiene sentido y simplemente deberia eliminarla de 
# la forward y la backward simulation, y cambiarla por un factor que multiplique la fitness
# similar a development_rate, que sea más alto para tadpoles de agua fria y más bajo para
# tadpoles de agu caliente (por bibliografia). De esta manera la fitness se veria afectada,
# y solo multiplicaria en Reward if Growth!

# Survival plot no debe funcionar porque a lo mejor hay alguna situación que no esta bien
# modelada y no existe acción para esa condición concreta, de manera que al no tener ninguna situación 
# (esta en un limbo) no pone un 1 o 0 en la matriz y da error. Tiene algo que ver con que la 
# probabilidad sea igual al valor concreto? Revisar todas las acciones y ver que falla en ese plot.
# Es raro que siempre falle en los mismos numeros... 

# Sigue sin gustarme lo que veo en los plots... como cambiarlo? esta es la versión más 
# realista pero no tiene sentido :(










# (It's not inside a function because it doesn't work the same way as it does 
# outside the function, I don't know why)
par(mfrow=c(3,1))
for(prob_good_temp in c(0.3,0.5,0.7)){
  
  prob_good_temp <- prob_good_temp
  prob_bad_temp <- 1 - prob_good_temp
  days <- 60
  end_season_percentage <- 0.4  
  end_season_intensity <- 1 
  death_rate_day <- 0.012 
  N <- 100
  
  
  Decisions(prob_good_temp, prob_bad_temp, days, end_season_percentage, end_season_intensity, death_rate_day)
  
  Forward(N)
  
  Fitness_bigger_0 <- as.vector(Final_Fitness[Final_Fitness > 0])
  Performance_bigger_0 <- as.vector(Final_Performance[Final_Performance > 0])
  Size_bigger_0 <- as.vector(Final_Size[Final_Size > 0])
  
  plot(density(Final_results[,2], bw = 0.1, from = -0.5, to = max(Performance) + 0.4), col = "black", main = paste("Final Traits (blue = F, red = S, black = P) prob good temp = ", prob_good_temp))
  abline(v = mean(Performance_bigger_0))
  
  lines(density(Final_results[,1], bw = 0.1, from = 1, to = max(Size) + 0.1), col = "red")
  abline(v = mean(Size_bigger_0), col = "red")
  
  lines(density(Final_results[,3], bw = 0.1, from = 1, to = max(Fitness_values) + 0.1), col = "blue")
  abline(v = mean(Fitness_bigger_0), col = "blue")
  
  abline(h = 0)
  minor.tick(nx=10, ny=1)
  
  
}








# Cambiar valores de fitness!! Ya no hay la misma fitness para tallas grandes, sino que siempre augmenta.
# Esto podria modificar backwards simulation y que casi no se invierta en performance... (HECHO)

# Tambien, los valores que tengo puestos como performance son los valores max de 
# performance observados en estos renacuajos, pero para llegar a estos, han tenido 
# que pasar por velocidades inferiores! Deberia augmentar el numero de breaks de 
# performance o cambiar los valores y empezar antes, sin cambiar el numero de breaks. 
# Eso deberia modificar tambien la backeards simulation. (HECHO)

# Revisar dos puntos anteriores

# Añadir mas tallas por arriba de 70 burst speed (no mucho más) y hacer que en el plot
# de los puntitos la ralla de fitness máxima corte por la fitness máxima conseguida en
# la población, no la posible



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




install.packages("hexbin")
library(hexbin)

Population <- as.data.frame(Population)

# Bin size control + color palette
ggplot(Population, aes(x=Size, y=Performance) ) +
  geom_hex(bins = 50) +
  scale_fill_continuous(type = "viridis") +
  theme_bw()