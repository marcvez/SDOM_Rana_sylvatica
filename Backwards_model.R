

#------------------------- Backwards model ---------------------------------

# This script contains the backward simulation of the model. 
# Different object values will be generated, which will contain different variables.
# Performance -> How fast you move.
# Size -> Your size.
# Fitness -> Terminal fitness values.
# Condition -> Combination of performance and size, and ranks your aptitude in 
# relation to the maximum possible.
# Survival -> Probability of survival depending on your current size and performance


# From this point, the model creates a loop in which at each time step the 
# organism can decide whether to invest in Performance or Growth. 
# It compares which of the two options is better and stores the decision in an array (ForageRule). 
# The fitness values obtained from each of the decisions are also stored (array Fitness).

# Finally, a plot of each time step is made and it is seen which decision is the 
# optimal one starting from the last time step, where the terminal fitness of each size is known. 

# In this script, temperature plays an important role, because if the temperature 
# is bad, investing in growth does not bring benefits and you stay the same, 
# while if you invest in moving better, it does not matter what temperature you are at.
# A good temperature brings benefits to both decisions, and influences the rate of 
# internal development of the organisms (the higher the temperature, the more accelerated, 
# and this is measured by the Inner_time factor and the probability of skipping 
# time_step due to accelerated development).

