# Hanna Kokko's book "Modelling for field biologists..." Chapter 5

#---------------R code----------------

## dmax = probability of death per time unit if you're very heavy
## dmin = probability of death per time unit if you're very lean
## c = rate of consuming resources
## f = feeding efficiency
## maxt = maximum time (i.e. number of time units the day is divided into)
## maxc = maximum condition (i.e. number of different condition units)
## The output is the ForageRule matrix, with 1 denoting foraging, and 0 denoting resting.

forage <- function(dmin, dmax, c, f, maxt, maxc) {
  
  ForageRule <- matrix(nrow=maxc+1, ncol=maxt)
  
  ## Reminder: rows indicate condition, columns indicate time. 
  ## Rows are chosen like this:
  ## dead=row 1, condition 1=row 2, condition 2=row 3, etc
  ## This means that best condition is maxc but this is found at row maxc+1
  ## Terminal reward increases with condition
  ## so we already know the values for the last (i.e. maxt+1st) row
  Reward <- matrix(nrow=maxc+1, ncol=maxt+1)
  Reward[,maxt+1] <- 0:maxc
  
  ## then, probability of death increases linearly with body weight
  d <- c(0, seq(dmin, dmax, length.out=maxc))
  
  ## anyone who is alive can either improve or then not...
  P_eat_up <- (1-d) * f
  P_eat_same <- (1-d) * (1-f)
  P_eat_dead <- d
  P_rest_same <- 1 - c
  P_rest_down <- c
  
  ## ...except those who already are in top condition
  ## cannot improve so they get different values here
  Ptop_eat_same <- 1 - d[length(d)]
  Ptop_eat_dead <- d[length(d)]
  
  ## we start from the end of the day and continue backwards
  for (t in maxt:1) {
    
    ## individuals who are dead have index 1
    ## individuals who are in top condition have index maxc+1
    
    ## Rules for updating fitness values:
    ## first everyone except those who already are dead, or in top condition
    
    ## We wish to compare two benefits: the expected reward
    ## from now onwards if one forages, and if one rests
    
    RewardIfForage <- matrix(nrow=maxc+1, ncol=maxt)
    RewardIfRest <- matrix(nrow=maxc+1, ncol=maxt)
    for (i in 2:maxc) {
      RewardIfForage[i,t] <- P_eat_same[i] * Reward[i,t+1] +
        P_eat_up[i] * Reward[i+1,t+1] + P_eat_dead[i] * 0
      RewardIfRest[i,t] <- P_rest_same * Reward[i,t+1] +
        P_rest_down * Reward[i-1,t+1]
    }
    
    ## Now the special cases
    
    ## dead ones don't get any rewards at all
    RewardIfForage[1,t] <- 0
    RewardIfRest[1,t] <- 0
    
    ## top ones can't improve their condition
    RewardIfForage[maxc+1,t] <- Ptop_eat_same * Reward[maxc+1,t+1] +
      Ptop_eat_dead * 0
    RewardIfRest[maxc+1,t] <- P_rest_same * Reward[maxc+1,t+1] +
      P_rest_down * Reward[maxc,t+1]
    
    ## Calculate the best foraging rule. This makes clever use
    ## of matrix notation as well as of boolean values: 
    ## if the statement is true, the value becomes 1,
    ## and zero otherwise.
    ForageRule[,t] <- RewardIfForage[,t] > RewardIfRest[,t]
    
    ## Update Reward by assuming individuals use the
    ## better of the two behavioural options in each case.
    ## The ! means 'not'.
    Reward[,t] <- ForageRule[,t] * RewardIfForage[,t] +
      as.numeric(!ForageRule[,t]) * RewardIfRest[,t]
    
  }
  
  ## Now some graphical procedures. Each state is represented as a rectangle 
  ## that will be coloured blue or white depending on whether one forages or not.
  ## This plots coloured squares in the correct position on a graph.
  
  colour <- c("white", "blue")
  require(lattice)
  require(grid)
  mypanel <- function(x, y, z, ...) {
    panel.levelplot(x, y, z, ...)
    grid.rect(x=x, y=y, width=1, height=1, default.units="native")
  }
  print(levelplot(t(ForageRule),
                  scales=list(tck=0,
                              x=list(at=1:maxt,labels=1:maxt),
                              y=list(at=1:(maxc+1),labels=0:maxc)),
                  colorkey=FALSE, col.regions=colour, aspect="fill",
                  xlab="Time", ylab="Condition", panel=mypanel))
  
  return(list(ForageRule=ForageRule))
  
}

#------------Parameters-----------------

#This script is added to save parameter values used
library(grid)
library(lattice)

dmax = 0.3 #probability of death per time unit if you're very heavy
dmin = 0.1 #probability of death per time unit if you're very lean
c = 0.4 #rate of consuming resources
f = 0.8 # feeding efficiency
maxt = 5 #maximum time (i.e. number of time units the day is divided into)
maxc = 6 #maximum condition (i.e. number of different condition units)
#The output is the ForageRule matrix, with 1 denoting foraging, and 0 denoting resting.

forage(dmin, dmax, c, f, maxt, maxc) #The plot doesn't work?

# Calling this function I only get the matrix in the console, the grid 
# appears but with no colors in it.Where is the problem? 

ForageRule_result_table <- forage(dmin, dmax, c, f, maxt, maxc)$ForageRule
plot(ForageRule_result_table[,],
     col=c("white", "blue"))


#---------------Plots-----------------------

# To obtain the next grids, you must run the lines inside the "forage" function.
# This is strange, and when you want to change the parameters, you have to 
# run again the function and the lines inside it. After doing this, you can run 
# the lines to obtain the graphs.It is weird, and I don't really know how 
# could I solve it in the way it was planned... :/

# Grid 1

library('plot.matrix')

par(mar=c(5.1, 4.1, 4.1, 4.1))
plot(
  ForageRule,
  y = NULL,
  breaks = NULL,
  col = colour,
  na.col = NULL,
  na.cell = TRUE,
  na.print = TRUE,
  digits = NA,
  fmt.cell = NULL,
  fmt.key = NULL,
  polygon.cell = NULL,
  polygon.key = NULL,
  text.cell = NULL,
  key = list(side = 4, las = 1),
  axis.col = maxt,
  axis.row = NULL,
  axis.key = NULL,
  max.col = 70,
  ylab = "Fitness",
  xlab = "time step",
  main = "Decision matrix"
  
)
axis(2, at=1:7, labels=seq(0,6,1))


# Grid 2

library(reshape2)
library(ggplot2)
ggplot(melt(ForageRule), aes(x=Var2, y=Var1, fill=value)) + geom_tile() +
  scale_fill_viridis_d(name = "Action", labels = c("Rest", "Forage"), alpha = 0.5) +
  scale_y_discrete(name = "Fitness", breaks = c(1,2,3,4,5,6,7), 
                   labels = c("0","1","2","3","4","5","6"), limit = c(1,2,3,4,5,6,7)) +
  scale_x_continuous(name="Time step", limits=c(0.5, 5.5))  +
  geom_segment(aes(x = 0.5, y = 0.5, xend = 5.5, yend = 0.5)) +
  geom_segment(aes(x = 0.5, y = 1.5, xend = 5.5, yend = 1.5)) +
  geom_segment(aes(x = 0.5, y = 2.5, xend = 5.5, yend = 2.5)) +
  geom_segment(aes(x = 0.5, y = 3.5, xend = 5.5, yend = 3.5)) +
  geom_segment(aes(x = 0.5, y = 4.5, xend = 5.5, yend = 4.5)) +
  geom_segment(aes(x = 0.5, y = 5.5, xend = 5.5, yend = 5.5)) +
  geom_segment(aes(x = 0.5, y = 6.5, xend = 5.5, yend = 6.5)) +
  geom_segment(aes(x = 0.5, y = 7.5, xend = 5.5, yend = 7.5)) +
  geom_segment(aes(x = 0.5, y = 0.5, xend = 0.5, yend = 7.5)) +
  geom_segment(aes(x = 1.5, y = 0.5, xend = 1.5, yend = 7.5)) +
  geom_segment(aes(x = 2.5, y = 0.5, xend = 2.5, yend = 7.5)) +
  geom_segment(aes(x = 3.5, y = 0.5, xend = 3.5, yend = 7.5)) +
  geom_segment(aes(x = 4.5, y = 0.5, xend = 4.5, yend = 7.5)) +
  geom_segment(aes(x = 5.5, y = 0.5, xend = 5.5, yend = 7.5)) 

# This is not the optimum way to do this, but the other options I
#have considered are worse than this one



dmax = 0.3 #probability of death per time unit if you're very heavy
dmin = 0.1 #probability of death per time unit if you're very lean
c = 0.4 #rate of consuming resources
f = 0.8 # feeding efficiency
maxt = 50 #maximum time (i.e. number of time units the day is divided into)
maxc = 10 #maximum condition (i.e. number of different condition units)
#The output is the ForageRule matrix, with 1 denoting foraging, and 0 denoting resting.


library(reshape2)
library(ggplot2)
ggplot(melt(ForageRule), aes(x=Var2, y=Var1, fill=value)) + geom_tile() +
  scale_fill_viridis_d(name = "Action", labels = c("Rest", "Forage"), alpha = 0.5) +
  scale_y_discrete(name = "Fitness", breaks = seq(1,11,1), 
                   labels = c("0","1","2","3","4","5","6","7","8","9","10"), limit = c(1,2,3,4,5,6,7,8,9,10,11)) +
  scale_x_continuous(name="Time step", limits=c(0.5, 50.5))  +
  geom_segment(aes(x = 0.5, y = 0.5, xend = 5.5, yend = 0.5)) +
  geom_segment(aes(x = 0.5, y = 1.5, xend = 5.5, yend = 1.5)) +
  geom_segment(aes(x = 0.5, y = 2.5, xend = 5.5, yend = 2.5)) +
  geom_segment(aes(x = 0.5, y = 3.5, xend = 5.5, yend = 3.5)) +
  geom_segment(aes(x = 0.5, y = 4.5, xend = 5.5, yend = 4.5)) +
  geom_segment(aes(x = 0.5, y = 5.5, xend = 5.5, yend = 5.5)) +
  geom_segment(aes(x = 0.5, y = 6.5, xend = 5.5, yend = 6.5)) +
  geom_segment(aes(x = 0.5, y = 7.5, xend = 5.5, yend = 7.5)) +
  geom_segment(aes(x = 0.5, y = 0.5, xend = 0.5, yend = 7.5)) +
  geom_segment(aes(x = 1.5, y = 0.5, xend = 1.5, yend = 7.5)) +
  geom_segment(aes(x = 2.5, y = 0.5, xend = 2.5, yend = 7.5)) +
  geom_segment(aes(x = 3.5, y = 0.5, xend = 3.5, yend = 7.5)) +
  geom_segment(aes(x = 4.5, y = 0.5, xend = 4.5, yend = 7.5)) +
  geom_segment(aes(x = 5.5, y = 0.5, xend = 5.5, yend = 7.5))

