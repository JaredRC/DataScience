name <- c("Mandi", "Amy", "Nicole", "Olivia")
distance <- c(0.8, 3.1, 2.8, 4.0)
time <- c(10, 30, 40, 50)
Hours <- time/60
SpeedMPH <- distance/Hours
df <- data.frame(Name = name, distance,Minutes = time,Hours,SpeedMPH)