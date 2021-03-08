# SDO MODEL


growth <- 0 # Initial growth value

performance <- 0 # Initial performance value

prob_1_unit <- seq(from = 0.4, to = 0.2, length.out = 30) 
# Probability of finding 1 unit of food

prob_2_unit <-seq(from = 0.4, to = 0.75, length.out = 30)
# Probability of finding 2 unit of food

prob_predation <- seq(from = 0.2, to = 0.05, length.out = 30)
# Probability of being predated

energy_performance <- seq(from = 1, to = 30, length.out = 30)

matrix_food_predation <- cbind(energy_performance,prob_1_unit,prob_2_unit,prob_predation)
#Matrix with the probabilities of each event for every performance value

matrix_food_predation_df <- as.data.frame(matrix_food_predation)

metabolic_rate <- 1 
#Amount of food units that every time step is used on self-maintenance

library(ggplot2)
library(dplyr)
library(viridis) # I can't make it colorblind-friendly, but I would like to


Probability_plot <- ggplot(data = matrix_food_predation_df, aes(x = energy_performance, group = 1)) +
  geom_line(aes(y = prob_1_unit, color = "1 food unit")) +
  geom_line(aes(y = prob_2_unit + prob_1_unit , color = "2 food units")) +
  geom_line(aes(y = prob_predation + prob_2_unit + prob_1_unit, color="Predation")) +
  xlab('Energy invested in perforance') +
  ylab('Probability') +
  scale_colour_discrete("Legend") +
  geom_ribbon(aes(ymax = 1, ymin = prob_2_unit + prob_1_unit), fill = "blue", alpha = 0.2) +
  geom_ribbon(aes(ymax = prob_2_unit + prob_1_unit, ymin = prob_1_unit), fill = "green", alpha = 0.2) +
  geom_ribbon(aes(ymax = prob_1_unit, ymin = 0), fill="red", alpha = 0.2)

Probability_plot

#Falta hacer la función de fitness


