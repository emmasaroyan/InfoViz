library(RCurl)
library(ggplot2)

x <- getURL("https://raw.githubusercontent.com/emmasaroyan/InfoViz/master/states_all.csv")
data <- read.csv(text = x)

#Subset of the data
data_rows <- data[,c(2,3,22,23,24,25)]

data2 <- na.omit(data_rows[,c(2,3,4,5,6)])
#Taking the mean value of all the scores from all the states for individual years
data2 <- aggregate(data2[,c(2,3,4,5)],by = list(data2$YEAR), FUN = mean)
colnames(data2) <- c("YEAR","Avg_Math_4th", "Avg_Math_8th","Avg_Reading_4th", "Avg_Reading_8th")

# Average score of math 4th grade for all states over time series 
ggplot(data2) + 
  geom_line(aes(x = data2$YEAR, y = data2$Avg_Math_4th), color = "red") +
  geom_point(aes(x = data2$YEAR, y = data2$Avg_Math_4th), color = "red") +
  labs(title = "Average Math Score across all states, 4th grade", x = "YEAR", y = "SCORE") + 
  theme(legend.position = "none") + theme_bw()

# Average score of math 8th grade for all states over time
ggplot(data2) +
  geom_line(aes(x = data2$YEAR, y = data2$Avg_Math_8th), color = "red") +
  geom_point(aes(x = data2$YEAR, y = data2$Avg_Math_8th), color = "red") +
  labs(title = "Average Math Score across all states,8th grade", x = "YEAR", y = "SCORE") + 
  theme(legend.position  = "none") + theme_bw() 
  
# Average score of reading 4th grade for all states over time 
ggplot(data2) +
  geom_line(aes(x = data2$YEAR, y = data2$Avg_Reading_4th), color = "blue") +
  geom_point(aes(x = data2$YEAR, y = data2$Avg_Reading_4th), color = "blue") +
  labs(title = "Average Reading Score across all states,4th grade", x = "YEAR", y = "SCORE") + 
  theme(legend.position = "none") + theme_bw()

# Average score of reading 8th grade for all states over time 
ggplot(data2) +
  geom_line(aes(x = data2$YEAR, y = data2$Avg_Reading_8th), color = "blue") +
  geom_point(aes(x = data2$YEAR, y = data2$Avg_Reading_8th), color = "blue") + 
  labs(title = "Average Reading Score across all states,8th grade", x = "YEAR", y = "SCORE") + 
  theme(legend.position = "none") + theme_bw()
 
#Facceted graph for 2 states: ALABAMA, ALASKA 

data_states <- na.omit(data_rows[data_rows$STATE %in% c("ALABAMA","ALASKA"),c(1,2,3,4)])

# Average score of math 8th grade for 2 states over time 
ggplot(data_states, aes(x = data_states$YEAR, y = data_states$AVG_MATH_8_SCORE)) +
  geom_line(col = "red") +
  labs(title = "Avg Math Score 8th grade in 2 states", x = "YEAR", y = "SCORE") +
  theme(legend.position = "none") + theme_bw() + 
  facet_grid(. ~ data_states$STATE) 
  
# Average score of math 4th grade for 2 states over time  
ggplot(data_states, aes(x = data_states$YEAR, y = data_states$AVG_MATH_4_SCORE)) +
  geom_line(col = "blue") +
  labs(title = "Avg Math Score 8th grade in 2 states", x = "YEAR", y = "SCORE") +
  theme(legend.position = "none") + theme_bw() + 
  facet_grid(. ~ data_states$STATE) 

  

