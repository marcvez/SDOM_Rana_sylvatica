# SDO MODEL


g <- 0 # Initial growth value

p <- 0 # Initial performance value

p1 <- seq(from= 0.4, to=0.2, length.out=30) 
# Probability of finding 1 unit of food

p2 <-seq(from=0.4, to=0.75, length.out=30)
# Probability of finding 2 unit of food

pred <- seq(from=0.2, to=0.05, length.out=30)
# Probability of being predated

matrix <- cbind(p1,p2,pred)
#Matrix with the probabilities of each event for every performance value

met_rate <- 1 
#Amount of food units that every time step is used on self-maintenance


