

#--------------------------- Forward simulation --------------------------------

# This simulation simulates a tadpole population that behave the optimum way,
# thanks to the Backwards simulation.

# Whatever happens in the Backwards simulation, has to happen in this one
# the same way. There is no calculation in this simulation, it only replicates
# the optimum behaviour. The probability of the stochastic events are 
# calculated for each time step. 

# There are some useful plots that show the evolution of the decisions, and
# also the final traits and conclusions could be extracted from these plots.


library(ggplot2)
library(Hmisc)


Forward <- function(N) {
  
  Population <- matrix(nrow = N, ncol = time_steps + 3)
  rownames(Population) <- c(1:N)
  colnames(Population) <- c((1:time_steps), "Size", "Performance", "Fitness") 
  # This matrix is going to store the state of each tadpole during the whole
  # period, being 1 the value of being alive and 0, being dead. 
  # It is going to be read by the "Alive" matrix, to determine how many 
  # individuals are still alive at each time step. The last columns store 
  # the Size, Performance and Fitness that tadpoles have at the last time step.
  
  
  Alive <- as.data.frame(matrix(nrow = time_steps + 1, ncol = 2))
  colnames(Alive) <- c("time_step", "N_alive")
  Alive[, 1] <- c(0:time_steps)
  Alive[1, 2] <- N
  # It sums the number of dead or alive individuals at each time step.
  
  
  Tadpole <- c(1:N)
  State <- c("Size", "Performance", "Fitness", "Stage")
  Time <- c(0:time_steps)
  Tadpole_state <- array(NA, dim = c(N, 4, time_steps + 1), dimnames = 
                           list (Tadpole, State, Time))
  # This array stores the Size, Performance and Expected Fitness values for 
  # each tadpole at each time step.
  
  
  Performance_forw <- Performance
  # This object is the same as Performance, going to be modified 
  # during the simulation instead of the original one.
  
  
  Adult <- array(NA, dim = c(N, 6, time_steps + 1))
  # Matrix that is going to store when do the tadpoles start the metamorphosis
  # (1, 2, 3), and when do they finish it (4, 5, 6).
  # It stores the Size (1, 4), the Performance (2, 5) and Fitness (3, 6).
  
  
  for (n in 1:N) {
    
    i <- sample(2:4,1)
    j <- 1 + 15 + ((tradeoff_advantage - 0.5) * 40)
    k <- 1
    t <- 1
    # Initial conditions for each tadpole
    
    Tadpole_state[n, 1, t] <- Size[i]
    Tadpole_state[n, 2, t] <- Performance[j]
    Tadpole_state[n, 3, t] <- Fitness[i,j,k,t]
    Tadpole_state[n, 4, t] <- k
    # We fix the initial condition in the Tadpole_state array.
    
    Performance_forw[j] <- Performance[j]
    # This object is the same as Performance, going to be modified 
    # during the simulation instead of the original one.
    
    while (t <= time_steps) {
      
      Prob_Survive <- runif(1)
      # Random value that determines if you live or you die.
      
      Temperature <- runif(1)
      # Random value that is going to determine if the day is good or bad, 
      # in terms of temperature.
      
      Forage <- runif(1)
      # Probability of finding food.
      
        
      if (Prob_Survive < Survival[i, j]) {
          
        if (ForageRule_B[i, j, k, t] == FALSE){
          
          if (Forage < Condition[i, j]){
            
            if (Temperature < prob_good_temp) {
              
              # Alive, no Metamorphosis, Food, Food Tº
              
              if (ForageRule[i, j, k, t] == FALSE) {
                
                # Invest in Growth
                
                i <- min(i + 2, max_Size)
                
              } else if (ForageRule[i, j, k, t] == TRUE) {
                
                # Invest in Performance
                
                j <- min(j + 2, max_Performance)
        
              } # if / else G/P
              
            } else {
              
              # Alive, no Metamorphosis, Food, Bad Tº
              
              if (ForageRule[i, j, k, t] == FALSE){
                
                # Invest in Growth
                
                i <- min(i + 1, max_Size)
                
              } else if (ForageRule[i, j, k, t] == TRUE){
                
                # Invest in Performance
                
                j <- min(j + 1, max_Performance)
                
              } # if / else G/P
              
            } # if / else Temperature
            
          } else {
            
            # Alive, no Metamorphosis, no Food
            
            i <- i
            
            j <- j
            
          } # if / else Food
          
          Population[n, t] <- 1
          
          Alive[t + 1, 2] <- sum(Population[, t])
          
          Tadpole_state[n, 1, t + 1] <- Size[i]
          Tadpole_state[n, 2, t + 1] <- Performance_forw[j]
          Tadpole_state[n, 3, t + 1] <- Fitness[i, j, k, t]
          Tadpole_state[n, 4, t + 1] <- k
          
          Population[n, time_steps + 1] <- Size[i]
          Population[n, time_steps + 2] <- Performance_forw[j]
          Population[n, time_steps + 3] <- Fitness[i, j, k, t]
        
          t <- t + 1
          
        } else {
          
          # Alive, Metamorphosis
          
          for (t in t:time_steps){
            
            if (Prob_Survive < Survival[i, j]){
              
              if (k < max_Stages) {
                
                if (Forage < Condition[i, j]) {
                  
                  # Food
                  
                  k <- min(k + 1, max_Stages)
                  
                } else {
                  
                  # No food, no investment
                  
                } # if/else k lower than max_Stages
                
                Prob_Survive <- runif(1)
                
                Forage <- runif(1)
                
              } else {
                
                # k = max_Stages -> frog, it doesn't die
                
                Prob_Survive <- 0
                
              }
              
              Population[n, t] <- 1
              
              Alive[t + 1, 2] <- sum(Population[, t])
              
              Tadpole_state[n, 1, t + 1] <- Size[i]
              Tadpole_state[n, 2, t + 1] <- Performance_forw[j]
              Tadpole_state[n, 3, t + 1] <- Fitness[i, j, k, t]
              Tadpole_state[n, 4, t + 1] <- k
              
              Population[n, time_steps + 1] <- Size[i]
              Population[n, time_steps + 2] <- Performance_forw[j]
              Population[n, time_steps + 3] <- Fitness[i, j, k, t]
              
              if (Tadpole_state[n, 4, t] == 1 & 
                  Tadpole_state[n, 4, t + 1] == 2) {
                
                Adult[n, 1, t] <- Size[i]
                Adult[n, 2, t] <- Performance_forw[j]
                Adult[n, 3, t] <- Fitness[i, j, k, t]
                
              } # Stores the state and time step where metamorphosis starts.
              
              if (Tadpole_state[n, 4, t] == 9 & 
                  Tadpole_state[n, 4, t + 1] == 10) {
                
                Adult[n, 4, t + 1] <- Size[i]
                Adult[n, 5, t + 1] <- Performance_forw[j]
                Adult[n, 6, t + 1] <- Fitness[i, j, k, t]
                
              } # Stores the state and time step where metamorphosis ends.
              
              t <- t + 1
              
            } else {
              
              # Dead while doing Metamorphosis
              
              i <- 1
              j <- 1
              Performance_forw[j] <- 0
              
              Population[n, t] <- 0
              
              Alive[t + 1, 2] <- sum(Population[, t])
              
              Tadpole_state[n, 1, t + 1] <- Size[i]
              Tadpole_state[n, 2, t + 1] <- Performance_forw[j]
              Tadpole_state[n, 3, t + 1] <- Fitness[i, j, k, t]
              Tadpole_state[n, 4, t + 1] <- k
              
              Population[n, time_steps + 1] <- Size[i]
              Population[n, time_steps + 2] <- Performance_forw[j]
              Population[n, time_steps + 3] <- Fitness[i, j, k, t]
              
              Prob_Survive <- 1
              
              t <- t + 1
              
            }
            
          } 
            
        } # if / else Metamorphosis
          
      } else {
        
        # Dead
        
        i <- 1
        j <- 1
        Performance_forw[j] <- 0
        
        Population[n, t] <- 0
        
        Alive[t + 1, 2] <- sum(Population[, t])
        
        Tadpole_state[n, 1, t + 1] <- Size[i]
        Tadpole_state[n, 2, t + 1] <- Performance_forw[j]
        Tadpole_state[n, 3, t + 1] <- Fitness[i, j, k, t]
        Tadpole_state[n, 4, t + 1] <- k
        
        Population[n, time_steps + 1] <- Size[i]
        Population[n, time_steps + 2] <- Performance_forw[j]
        Population[n, time_steps + 3] <- Fitness[i, j, k, t]
        
        Prob_Survive <- 1
        
        t <- t + 1
        
      } # if / else Survival
      
    } # while loop
    
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
  Max_Condition <- as.vector(which(Final_results[, 3] == 
                                     max(Final_results[, 3])))
  assign("Final_Fitness", Final_Fitness, envir = globalenv())
  assign("Final_Size", Final_Size, envir = globalenv())
  assign("Final_Performance", Final_Performance, envir = globalenv())
  assign("Final_results", Final_results, envir = globalenv())
  assign("Max_Condition", Max_Condition, envir = globalenv())
  assign("Adult", Adult, envir = globalenv())
  
} # End Forward simulation


Survival_plot <- function() {
  
  par(mfrow=c(1,1))
  par(mar=c(0, 0, 0, 0))
  
  ggplot(data = Alive, aes(x = time_step, y = N_alive, group=1)) +
    geom_line() +
    geom_point() +
    scale_x_continuous(name="Time Steps", limits=c(0, time_steps)) +
    scale_y_continuous(name="Nº tadpoles alive", limits=c(0, N))
  
} 
# Survival plot.


Investment_plot <- function() {
  
  par(mfrow=c(3,1))
  par(mar=c(5.1, 4.5, 1, 4.5))
  
  plot(1, type="l", xlab="Time Step", ylab="Size (cm)", xlim=c(1, time_steps), 
       ylim=c(0, Size[max_Size]), xaxt = "n")
  axis(1, at=1:(time_steps + 1), labels = c(0:time_steps))
  for (n in 1:N) {
    lines(Tadpole_state[n, 1, ])
    points(Adult[n, 1, ], pch = 19)
    points(Adult[n, 4, ])
  
  } # Size
  
  plot(1, type="l", xlab="Time Step", ylab="Burst speed (cm/s)", 
       xlim=c(1, time_steps), ylim=c(0, Performance[max_Performance]), 
       xaxt = "n")
  axis(1, at=1:(time_steps + 1), labels = c(0:time_steps))
  
  for (n in 1:N) {
    lines(Tadpole_state[n, 2, ])
    points(Adult[n, 2, ], pch = 19)
    points(Adult[n, 5, ])
    
  } # Performance
  plot(1, type="l", xlab="Time Step", ylab="Fitness", xlim=c(1, time_steps), 
       ylim=c(0, max(Fitness[, , max_Stages, time_steps + 1])), xaxt = "n")
  axis(1, at=1:(time_steps + 1), labels = c(0:time_steps))
  
  for (n in 1:N) {
    lines(Tadpole_state[n, 3, ])
    points(Adult[n, 3, ], pch = 19)
    points(Adult[n, 6, ])
    
  } # Fitness
  
}
# Investment and fitness plot.


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
         pch = c(19, 19, 19), col = c('#440154FF', '#21908CFF', '#FDE725FF'), 
         lty=2, cex=0.8)
  
  
  # Plot that displays the final values of the tadpoles' traits.
  # Vertical lines intercept those tadpoles that have the better fitness.
  
}
# Final traits plot


Density_plot <- function(){
  
  par(mfrow=c(3,2))
  par(mar=c(5.1, 4.5, 3, 4.5))
  
  plot(density(Final_results[,1], bw = 0.1, from = -0.5, 
               to = max(Size) + 0.3), main = "Final Size (cm)")
  rug(Final_results[,1], col='red')
  
  plot(density(Final_results[,1], bw = 0.1, from = 3 - 0.5, 
               to = max(Size) + 0.3), main = "Final Size (cm) (Alive)")
  rug(Final_results[,1], col='red')
  
  plot(density(Final_results[,2], bw = 0.1, from = -0.5, 
               to = max(Performance) + 0.4), main = "Final Burst Speed (cm/s)")
  rug(Final_results[,2], col='red')
  
  plot(density(Final_results[,2], bw = 0.1, from = 4 - 0.5, 
               to = max(Performance) + 0.5), 
       main = "Final Burst Speed (cm/s) (Alive)")
  rug(Final_results[,2], col='red')
  
  plot(density(Final_results[,3], bw = 0.1, from = -0.5, 
               to = max(Fitness[, , 10, time_steps + 1]) + 0.3), 
       main = "Final Fitness")
  rug(Final_results[,3], col='red')
  
  plot(density(Final_results[,3], bw = 0.1, from = 2 - 0.5, 
               to = max(Fitness[, , 10, time_steps + 1]) + 0.3), 
       main = "Final Fitness (Alive)")
  rug(Final_results[,3], col='red')
  
}
# Density plots of the final values of the traits.


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
# Histogram showing the number of tadpoles in each Fitness value.


Comparison_plot <- function(){
  
  par(mfrow=c(5,1))
  
  for(prob_good_temp in c(0.3, 0.4, 0.5, 0.6, 0.7)){
    
    prob_good_temp
    prob_bad_temp <- 1 - prob_good_temp
    days <- 60
    end_season_percentage <- 0.2
    end_season_intensity <- 1 
    death_rate_day <- 0.012 
    N <- 100
    
    
    Decisions(prob_good_temp, prob_bad_temp, days, end_season_percentage, 
              end_season_intensity, death_rate_day)
    
    Forward(N)
    
    Fitness_bigger_0 <- as.vector(Final_Fitness[Final_Fitness > 0])
    Performance_bigger_0 <- as.vector(Final_Performance[Final_Performance > 0])
    Size_bigger_0 <- as.vector(Final_Size[Final_Size > 0])
    
    plot(density(Final_results[,2], bw = 0.1, from = -0.5, 
                 to = max(Performance) + 0.4), col = "black", 
         main = paste(
           "Final Traits (blue = F, red = S, black = P) prob good temp = ", 
                   prob_good_temp))
    abline(v = mean(Performance_bigger_0))
    
    lines(density(Final_results[,1], bw = 0.1, from = 1, 
                  to = max(Size) + 0.5), col = "red")
    abline(v = mean(Size_bigger_0), col = "red")
    
    lines(density(Final_results[,3], bw = 0.1, from = 1, 
                  to = max(Fitness[, , 10, time_steps + 1]) + 0.5), 
          col = "blue")
    abline(v = mean(Fitness_bigger_0), col = "blue")
    
    abline(h = 0)
    minor.tick(nx=10, ny=1)
    
    rm(list = ls())
    
  }
  
}
# Comparison of 5 different situations, each one increasing the 
# temperature respect the previous one. 


Comparison_plot_little <- function(){
  
  par(mfrow=c(3,1))
  
  for(prob_good_temp in c(0.3, 0.5, 0.7)){
    
    prob_good_temp
    prob_bad_temp <- 1 - prob_good_temp
    days <- 60
    end_season_percentage <- 0.2 
    end_season_intensity <- 1 
    death_rate_day <- 0.012 
    N <- 100
    
    
    Decisions(prob_good_temp, prob_bad_temp, days, end_season_percentage, 
              end_season_intensity, death_rate_day)
    
    Forward(N)
    
    Fitness_bigger_0 <- as.vector(Final_Fitness[Final_Fitness > 0])
    Performance_bigger_0 <- as.vector(Final_Performance[Final_Performance > 0])
    Size_bigger_0 <- as.vector(Final_Size[Final_Size > 0])
    
    plot(density(Final_results[,2], bw = 0.1, from = -0.5, 
                 to = max(Performance) + 0.4), col = "black", 
         main = paste(
           "Final Traits (blue = F, red = S, black = P) prob good temp = ", 
           prob_good_temp))
    abline(v = mean(Performance_bigger_0))
    
    lines(density(Final_results[,1], bw = 0.1, from = 1, 
                  to = max(Size) + 0.5), col = "red")
    abline(v = mean(Size_bigger_0), col = "red")
    
    lines(density(Final_results[,3], bw = 0.1, from = 1, 
                  to = max(Fitness[, , 10, time_steps + 1]) + 0.5), 
          col = "blue")
    abline(v = mean(Fitness_bigger_0), col = "blue")
    
    abline(h = 0)
    minor.tick(nx=10, ny=1)
    
    rm(list = ls())
    
  }
  
}
# The same but with 3 different Temperatures (easier to plot).




# Initial parameters

N <- 100
# Number of Tadpoles




# Plots

Forward(N)

Survival_plot()

Investment_plot()

Final_traits_plot()

Density_plot()

Histogram_plot()

Comparison_plot()

Comparison_plot_little()

