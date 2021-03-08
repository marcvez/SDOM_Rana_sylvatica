# STOCHASTIC DYNAMIC MODEL: A SMALL BIRD IN WINTER
# Here we try to link concepts from the Hanna Kokko model (HK model), 
# concepts from chapter five of Clark & Mangel (C&M model),
# and Thorbjørn's code

library('plot.matrix')
library('ggplot2')
library('plot3D')

#---- State ---- 
# In the HK model, state was more or less a ranked order where 0=death
# and condition improved with increasing order. It was good for illustrative 
# purposes but not so biologically meaningful. 
# In the C&M model, we define both maximum (x_max) and minumum (m_0) values.
# An important difference between the two models is that in the C&M model,
# a bird with zero fat reserves can increase its state by foraging 
# first thing the next morning. Therefore, x=m_0 at the start of the day
# does not automatically result in death like it does in the HK model. 

m_0    <- 10    # Mass of bird with zero fat reserves (g)

x_max  <- 2.4   # Maximum fat reserves (g)
# Equivalent to maxc in HK model

x_d    <- seq(from = 0, to = x_max, 
              length.out=100)   # Discretized values of x
# In the HK model, we limited ourselves to whole numbers


#---- Actions ---- 
# In our HK model, a bird either forages or rests (# actions = 2)
# In the C&M model, a bird decides between three patches denoted by H[i] (# actions = 3)
# where i = [1, 2, 3] 
# Note: in the book they use 0, 1, 2 but using 1-3 makes subsetting more straightforward
# Each patch (H[i]) has a different risk of predation denoted by mu. 
# Each patch also has a different rate of foraging intake denoted by e, 
# which is similar to the foraging efficiency in the HK model. 

mu <- c(0, 0.001, 0.005)  # Basic daily predation risk per patch (/day) 

e  <- c(0, 0.6, 2.0)      # Net daily forage inntake for patch 1/2 (g/day)

# In both models, predation is a function of body mass.
# In the HK model we assumed a linear relationship when foraging, with 0 predation when resting. 
# In the C&M model, the increase in predation depends on the patch and energetic reserves,
# equal to mu[i]*exp(lambda*x) see p. 111. 

lambda <- 0.46     # Increase in predation risk with body mass (/g fat reserves)

# The metabolic costs during the day are also mass-dependent, equal to gamma * (m0 * x), 
# where m0 is the mass of the bird with zero fat reserves (defined above) p. 111. 
# In the HK model, recall that metabolic costs were held constant. 

gamma  <- 0.04     # Metabolic rate (g/day)
# Equivalent to c in HK model


#---- Fitness ---- 
# In the HK model, terminal fitness occurs at the end of the day. 
# In the C&M model, terminal fitness is defined as the probability of surviving the winter.
# Winter consists of 120 days (Days) and each day is divided into 50 time steps (Time). 
# We can account for this with a nested loop (see backwards iteration below). 
# Environmental stochasticity arises from overnight metabolic costs. 
# A bad night has a higher metabolic costs (c_b) and occurs with probability p_b
# A good night has a lower metabolic cost (c_g) and occurs with probability 1-p_b

Time <- 50    # Number of time periods per day (where time+1 is the beginning of nightime)
Days <- 20   # Number of days in winter (where days+1 is the start of spring)
c_g  <- 0.48  # Metabolic cost of a good night (g)
c_b  <- 1.20  # Metabolic cost of a bad night (g)
p_b  <- 0.167 # Probability of a bad night

# Recall that terminal fitness is the probability of surviving the winter (i.e. d+1)
# We can calculate it using equation 5.1 in C&M

# First, an empty array to hold fitness values from the loop
Fit<- array(data = NA, dim = c(length(x_d), Time+1, Days), 
            dimnames = list(x_d, 1:(Time+1), 1:(Days)) )

# Calculate terminal fitness F(x, t+1, d) where x=state 
for (j in 1:length(x_d)) {
  # Note: I use j as the index of an x-value in x_d and x 
  # as the value of x_d[j].
  x <- x_d[j]
  # If your state does not allow you to survive a good night,
  # survival is impossible and your fitness is zero. 
  if (x < c_g) {
    Fit[j, Time+1, Days] <- 0
    # If your state allows you to survive a good night, but not a bad one,
    # you survive with probability p_g=1-p_b (the probability of a good night). 
  } else if (x < c_b) {
    Fit[j, Time+1, Days] <- (1-p_b)
    # If your state allows you to survive a bad night, you will always survive
    # i.e. your probabiity of survival is 1. 
  } else {
    Fit[j, Time+1, Days] <- 1
  } # end if-else loop
} # end for loop

as.matrix(Fit[, Time+1, Days]) # Did it work? 
# The left column shows state, the right column shows the terminal fitness we just calculated. 
# To survive a good night requires 0.48 g of fat reserves.
# When x < 0.48 the bird dies (it cannot survive the best-case scenario). 
# To survive a bad night requires 1.2 g of fat reserves. 
# When 0.48 < x < 1.2 the probability of surivival =  1-p_b = 0.833
# When x > 1.2 (the cost of a bad night), the bird always survives (proability =1)

#---- Discrete state variable ----
# The computer discretizes the state variable 
# but in reality energeic reserves is a continuous variable. 
# We overcome this using interpolation (see C&M 2.1)

interpolate <- function (x, t, d) {
  
  # Returns the index of the closest discrete x value 
  closest_discrete_x <- function(x) {
    # Note: only the first value is returned if there are multiple 
    # equidistant values.
    return(which(abs(x_d - x) == min(abs(x_d - x)))[1])
  }
  
  # Interpolate between two values a and b. dx is a value between 0 and 1.
  linear_interpolation <- function (a, b, dx) {
    return((1-dx)*a+ b*dx)
  }
  
  # No point in doing interpolation if energy reserves are negative.
  # Bird is dead.
  if (x < 0) { return(0) }
  
  # Figure out between which two discretized values of x our x lies.
  closest <- closest_discrete_x(x)
  if (x < x_d[closest]) {
    j1 <- closest -1
    j2 <- closest
  } else if (x > x_d[closest]){
    j1 <- closest
    j2 <- closest +1
  } 
  # Fitness value for x is already present in the matrix. No need to interpolate.
  else { return( Fit[closest, t, d]) }
  
  # Calculate how x is positioned in relation to x_d[j1] and x_d[j2].
  # 0: Closer to x_d[j1]
  # 1: Closer to x_d[j2]
  delta_x <- (x-x_d[j1])/(x_d[j2]-x_d[j1])
  
  # Interpolate.
  return(linear_interpolation(Fit[j1, t, d], Fit[j2, t, d], delta_x))
}

# ---- Backwards Iteration ---- 

# We already have a blank array for the fitness loop
# Create one for the decision loop (patch choice, H)
H <- array(data = NA, dim = c(length(x_d), Time, Days), dimnames = list(
  x_d, 1:Time, 1:Days))

# Iterate backwards across days at Time+1
for (d in Days:1) {
  # Terminal fitness has already been calculated, 
  # so don't calculate fitness for T+1 on the last day.
  if (d != Days) {
    for (j in 1:length(x_d)) {
      x <- x_d[j]
      
      # Equation 5.2
      # Notice we use the interpolation function here
      # If a bird can't survive a good night... 
      if (x < c_g) {
        Fit[j, Time+1, d] <- 0      # Bird is dead, it just doesn't know it yet.
        
        # If a bird can survive a good night but not a bad night
        # its probability of survival = probability of a good night 
        # times its state (reward) after you subtract the overnight energetic cost
      } else if (x < c_b) {
        Fit[j, Time+1, d] <- (1-p_b)*interpolate(x-c_g, 1, d+1)  
        
        # Otherwise is can survive a bad night, in which case
        # fitness = prob. of a good night*state after a good night + 
        # prob. of a bad night * state after a bad night
        # this follows the same logic as the HK model
      } else {
        Fit[j, Time+1, d] <- (1-p_b)*interpolate(x-c_g, 1, d+1) +
          p_b*interpolate(x-c_b, 1, d+1)  #prob of good night*state after good night plus same for bad night
      }
    } # end if-else loop
  } # end j loop
  
  # Iterate backwards from max Time for each day
  for (t in Time:1) {
    for (j in 1:length(x_d)) {
      x <- x_d[j]
      # Vector for storing fitness values for each of the three patches.
      F_i <- vector(mode = 'numeric', length=3)
      
      # Calculate resulting fitness of choosing each patch
      # Equation 5.4
      for (i in 1:3) {
        # Equation 5.4
        # Calculating the expected state in the future.
        x_mark <- ( x + (1/Time)*(e[i] - gamma*(m_0+x)) )
        
        # Ensure that x_mark does not exceed x_max.
        x_mark <- min(c(x_max, x_mark))
        
        # Plug x_mark (5.4) into equation 5.3
        # Calculate the expected fitness given patch choice h.
        # i.e: Chance of surviving time expected future fitness in this patch.
        F_i[i] <- (1 - (1/Time)*(mu[i]*exp(lambda*x)) ) *interpolate(x_mark, t+1, d)
      } # end i loop 
      
      # Which patch choice maximizes fitness?
      Fit[j, t, d] <- max(F_i)[1]
      
      # Optimal patch choice is the one that maximizes fitness.
      # In cases where more than one patch shares the same fitness, 
      # the first one (i.e. lower risk) is chosen.
      H[j, t, d] <- which(F_i == max(F_i))[1]
      
    } # end j loop 
  } # end t loop
} # end d loop

# We can explore the decision matrix at different days, similar to Fig 5.2
# Keeping in mind that our output shows a reverse order on the y-axis
H[,,100] 


#--- Plots! ----

# Fitness function at two different times on day Days-20, t=25 and t=49 respectively.
# This figure should be equivalent to 5.1 in Clark and Mangel (1999).

# Create an empty plot for plotting lines.
plot(NA, type="n", 
     xlab="Fat reserves (g)",
     ylab="Fitness, F[x, t]", 
     xlim=c(0, x_max), ylim=c(0, 1))

# Plotting the lines.
lines(x_d, as.vector(Fit[,1,Days-8]), col = "black", lty = 3)
lines(x_d, as.vector(Fit[,15,Days-8]), col = "black", lty = 1)


# Reverse the x-dimension of the array so it is ordered from high to low. 
# Nicer when plotting.
Fit.rev <- Fit[length(x_d):1,,]
H.rev <-   H  [length(x_d):1,,]

# Plotting the optimal decision at any given time.
# black:  Patch 1 (0)
# yellow: Patch 2 (1)
# red:    Patch 3 (2)
# Should be equivalent to Figure 5.2 in Clark and Mangel (1999).
plot(H.rev[,,Days-5], breaks=c(0.5, 1.5, 2.5, 3.5), col=c("black", "yellow", "red"),
     xlab = "Time of day",
     ylab = "Fat reserves")

# Fitness 
plot(Fit.rev[,,Days-5],
     xlab = "Time of day",
     ylab = "Fat reserves")

# Fitness landscape plot.
persp3D(z = Fit.rev[,,Days-20], theta = 135, phi = 45,
        xlab = "State (x)", 
        ylab = "Time (t)",
        zlab = "Fitness (F)")



#---- Forward Iteration ---- 
# The below section implements forward iteration of the above model (i.e. 
# looking at the fate of individuals in the model).

# Number of days to forward iterate for.
Days  <- 20

# Time/day is given from the model above.

# Initial states for individual (A vector of indecies in x_d).
j_0 <- 1 # seq(from=1, to = length(x_d), by = 20)

# Number of individuals to iterate for each x_0. Total number of individuals
# is length(x_0)*N.ind:
N.ind <- 500

# X[j_0, N, D, T]:
# State of individual N with initial state j_0, at day D and time T.

X <- array(data = NA, dim = c(length(j_0), N.ind, Days*Time +1), dimnames = list(
  x_d[j_0],
  1:N.ind,
  1:(Days*Time+1)
))

for (j in 1:length(j_0)) {
  x_0 <- x_d[j_0[j]]
  for (n in 1:N.ind) {
    # Set the initial state.
    X[j, n, 1] <- x_0
    
    # Iterate through the time.
    for (z in 1:(Days*Time) ) {
      t = (z-1) %%  Time  +1 # Time of day
      d = (z-1) %/% Time  +1 # Day
      # The current state.
      x <- X[j, n, z]
      if (x < 0) {
        # Bird is dead. It will remain dead.
        x_new <- x
      } else {
        # The best decision given the current state (Found by rounding x to the closest item
        # in x_d).
        # TODO: interpolate the decision between the two closest optimal decisions.
        h <- H[which(abs(x_d - x) == min(abs(x_d - x)))[1], t, d]
        
        if (runif(1) <= (1/Time)*mu[h]*exp(lambda*x)) { # Predation risk.
          # Negative x = dead.
          x_new <- -1
        } else {
          metabolism <- (1/Time)*gamma*(m_0+x)
          foraging   <- (1/Time)*e[h]
          
          x_new <- x + foraging - metabolism
        }
      }
      
      if (t == Time) {
        # If it is the end of the day, nightly costs need to be applied as well.
        if (runif(1) < p_b) {
          # Bad night.
          x_new <- x_new - c_b
        } else {
          # Good night.
          x_new <- x_new - c_g
        }
        
        # Set the state for the start of the following day.
      } 
      
      # Set new state.
      X[j, n, z+1] <- x_new
    }
  }
}

## The below plots multiple individuals on the same plot.
# Red vertical lines:     Separates days.
# Light grey solid line:  Fat reserves necessary for surviving a bad night.
# Light grey dotted line: Fat reserves necessary for surviving a good night.

# Create an empty plot, with the necessary xlim and ylim.
plot(NA, type="n", 
     xlab="Time",
     ylab="Fat reserves (g)", xlim=c(1, (Days*Time +1)), ylim=c(0, x_max))


# Horizontal lines to indicate cost of good and bad night respectively.
abline(h=c_g, col = "blue", lty = 3)
abline(h=c_b, col = "blue", lty = 1)

# Plot each individual for the chosen x_0.
for (n in 1:N.ind) {
  lines(1:(Days*Time +1), X[1, n, ], col = "black")
}

# Plot vertical lines to indicate position of the night.
for (i in 1:Days) {
  abline(v=i*Time, col = "red", lty = 3)
}

# Calculate the proportion of individuals still alive at the start 
# of each day.

alive = data.frame(day = NULL, n_alive = NULL, p_alive = NULL)
for (d in 1:(Days+1)) {
  
  tmp <- data.frame(day = d,
                    n_alive = length(which(as.vector(X[1, , (d-1)*Time +1]) >= 0))
                    
                    
  )
  tmp$p_alive <- tmp$n_alive/N.ind
  
  alive <- rbind(alive, tmp)
}

# Plotting the proportion of individuals that are still alive 
# at the beginning of each day.
plot(NA, type="n", 
     xlab="Day",
     ylab="Proportion alive", xlim=c(1, Days+1), ylim=c(0, 1))

lines(alive$day, alive$p_alive)