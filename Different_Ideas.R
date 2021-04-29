

par(mfrow=c(3,1))

prob_good_temp <- 0.2
prob_bad_temp <- 1 - prob_good_temp
days <- 60
end_season_percentage <- 0.4  
end_season_intensity <- 1 
death_rate_day <- 0.012 
N <- 100
development_rate <- 0.8


Decisions(prob_good_temp, prob_bad_temp, days, end_season_percentage, end_season_intensity, death_rate_day, development_rate)

Forward(N, prob_good_temp)

Fitness_bigger_0 <- as.vector(Final_Fitness[Final_Fitness > 0])
Performance_bigger_0 <- as.vector(Final_Performance[Final_Performance > 0])
Size_bigger_0 <- as.vector(Final_Size[Final_Size > 0])

par(mfrow=c(3,1))

plot(density(Final_results[,2], bw = 0.1, from = -0.5, to = max(Performance) + 0.4), col = "black", main = "Final Traits (blue = F, red = S, black = P) (0.3)")
abline(v = mean(Performance_bigger_0))

lines(density(Final_results[,1], bw = 0.1, from = 1, to = max(Size) + 0.1), col = "red")
abline(v = mean(Size_bigger_0), col = "red")

lines(density(Final_results[,3], bw = 0.1, from = 1, to = max(Fitness_values) + 0.1), col = "blue")
abline(v = mean(Fitness_bigger_0), col = "blue")

abline(h = 0)
minor.tick(nx=10, ny=1)



prob_good_temp <- 0.5
prob_bad_temp <- 1 - prob_good_temp
days <- 60
end_season_percentage <- 0.4  
end_season_intensity <- 1 
death_rate_day <- 0.012 
N <- 100
development_rate <- 0.5


Decisions(prob_good_temp, prob_bad_temp, days, end_season_percentage, end_season_intensity, death_rate_day, development_rate)

Forward(N, prob_good_temp)

Fitness_bigger_0 <- as.vector(Final_Fitness[Final_Fitness > 0])
Performance_bigger_0 <- as.vector(Final_Performance[Final_Performance > 0])
Size_bigger_0 <- as.vector(Final_Size[Final_Size > 0])

plot(density(Final_results[,2], bw = 0.1, from = -0.5, to = max(Performance) + 0.4), col = "black", main = "Final Traits (blue = F, red = S, black = P) (0.5)")
abline(v = mean(Performance_bigger_0))

lines(density(Final_results[,1], bw = 0.1, from = 1, to = max(Size) + 0.1), col = "red")
abline(v = mean(Size_bigger_0), col = "red")

lines(density(Final_results[,3], bw = 0.1, from = 1, to = max(Fitness_values) + 0.1), col = "blue")
abline(v = mean(Fitness_bigger_0), col = "blue")

abline(h = 0)
minor.tick(nx=10, ny=1)




prob_good_temp <- 0.8
prob_bad_temp <- 1 - prob_good_temp
days <- 60
end_season_percentage <- 0.4  
end_season_intensity <- 1 
death_rate_day <- 0.012 
N <- 100
development_rate <- 0.2

Decisions(prob_good_temp, prob_bad_temp, days, end_season_percentage, end_season_intensity, death_rate_day, development_rate)

Forward(N, prob_good_temp)

Fitness_bigger_0 <- as.vector(Final_Fitness[Final_Fitness > 0])
Performance_bigger_0 <- as.vector(Final_Performance[Final_Performance > 0])
Size_bigger_0 <- as.vector(Final_Size[Final_Size > 0])

plot(density(Final_results[,2], bw = 0.1, from = -0.5, to = max(Performance) + 0.4), col = "black", main = "Final Traits (blue = F, red = S, black = P) (0.7)")
abline(v = mean(Performance_bigger_0))

lines(density(Final_results[,1], bw = 0.1, from = 1, to = max(Size) + 0.1), col = "red")
abline(v = mean(Size_bigger_0), col = "red")

lines(density(Final_results[,3], bw = 0.1, from = 1, to = max(Fitness_values) + 0.1), col = "blue")
abline(v = mean(Fitness_bigger_0), col = "blue")

abline(h = 0)
minor.tick(nx=10, ny=1)








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


