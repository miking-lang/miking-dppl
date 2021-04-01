library(ggplot2)
delayed_data = read.csv("clads2-delayed.csv", sep = " ")[1:20000, ]
delayed_data$inference = rep( "delayed", nrow(delayed_data))
data = read.csv("clads2.csv", sep = " ")
data$inference = rep("immediate", nrow(data))

df = rbind(data.frame("lambda0" = delayed_data$lambda0, "inference" = delayed_data$inference, "weight" = delayed_data$weight),
 data.frame("lambda0" = data$lambda0, "inference" = data$inference, "weight" = data$weight))



ggplot(df, aes(x = epsilon, weight = weight, fill = inference)) +  geom_histogram(binwidth = 0.01) 
ggplot(delayed_data, aes(x = lambda0, weight = weight, fill = inference)) +  geom_histogram(binwidth = 0.0001) 

# aes(x=lambda0, color=inference)