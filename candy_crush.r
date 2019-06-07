#Install packages
install.packages("tidyverse")

# Loading in packages
library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(scales)

# Reading in the data
data <- read_csv("/Users/varshilaredkar/Documents/Projects/Level of Difficulty in Candy Crush/candy_crush.csv")

# Printing out the first couple of rows
head(data)

print("Number of players:")
nrow(data)

print("Period for which we have data:")
range(data$dt)

# Calculating level difficulty
difficulty <- data %>% group_by(level) %>% summarize(attempts = sum(num_attempts), success = sum(num_success)) %>% 
	mutate(p_win = success/attempts)

# Printing out the level difficulty
difficulty

# Plotting the level difficulty profile
ggplot(difficulty, aes(x = level, y = p_win)) + scale_x_continuous(breaks = 1:15) + scale_y_continuous(labels = percent) + 
	geom_point() + geom_hline(yintercept = 10) + geom_line(linetype = "dashed")

# Computing the standard error of p_win for each level
difficulty <- difficulty %>% mutate(error = sqrt(p_win * (1 - p_win)/attempts))

# Adding standard error bars
ggplot(difficulty, aes(x = level, y = p_win)) + scale_x_continuous(breaks = 1:15) + scale_y_continuous(labels = percent) + 
	geom_point() + geom_hline(yintercept = 10) + geom_line(linetype = "dashed") + geom_errorbar(aes(ymin = p_win - 
	error, ymax = p_win + error))

# The probability of completing the episode without losing a single time
p <- prod(difficulty$p_win)
# Printing it out
p

# Should our level designer worry about that a lot of 
# players will complete the episode in one attempt?
if (p < 0.001) {
	should_the_designer_worry = FALSE
	print("The probability is really small, so I don't think the designer should worry that much...")
} else {
	should_the_designer_worry = FALSE
	print("The probability is slightly high, so I think the levels may have to be redesigned.")
}
