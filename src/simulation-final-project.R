# The following code is designed to generate the data sets used
# on the final project for class Psych 10 C. There are 3 problems
# all involving at least 3 variables with at least 2 levels.

library(tidyverse)

# Problem 1: Recognition for objects on visual field ----------------------

# For this first problem we have a "standardized" index of recognition 
# performace on a visual detection task. There are 2 independent 
# variables variables that we are interested in, the location 
# of stimulus on the screen labels left and right, and if they have been 
# subject to a brain split surgery to treat epilepsy. We have 30 
# observations of each condition for a total of 120.

# The probabilities of correctly recognicing an object are order according to
# rows: 1 = no split, 2 = split
# columns: 1 = right visual field, 2 = left visual field
prob_recognition <- rbind(c(0.9, 0.93),
                          c(0.925, 0.46))

# Standardized values (inverse standard normal)

mu_recognition <- qnorm(p =  prob_recognition)

mu_recognition <- rep(as.vector(mu_recognition), each = 30)

# Variations in participants score 

var_recognition <- 0.895

standard_test <- rnorm(n = 30 * 4, mean = mu_recognition, sd = sqrt(var_recognition))

visual_field <- rep(c("right", "left"), each = 60)

split_surgery <- rep(rep(c("no_surgery", "surgery"), each = 30), times = 2)

split_brain <- tibble("id" = as.character(seq(1,120)),
                      standard_test, 
                      visual_field, 
                      split_surgery)

write_csv(x = split_brain, file = "data/final-project/problem-1.csv")

# Problem 2: Cognitive decline --------------------------------------------

# For this problem we have the difference between two cognitive tests performed 
# elderly participants distanced one year apart, (outcomes can be understood as 
# a decline in cognitive ability after a year).
# There are three independent variables in the problem, first is level of 
# social interaction which has levels low and high, second is the level of
# physical actvity, again with levels low and high, finally we have an indicator
# of whether participants used the luminosity app during the year or not. We 
# have 14 observations for each condition for a total of 112.

# Mean cognitive decline values for participants that use the luminosoty app:
# rows: 1 = low social interaction, 2 = high social interaction
# columns: 1 = low phisical activity, 2 = high phisical activity
mu_cog_lum <- rbind(c(-4.1, -3.1),
                    c(-2, -0.91))

# Mean cognitive decline values for participants that do not use the luminosoty app:
# rows: 1 = low social interaction, 2 = high social interaction
# columns: 1 = low phisical activity, 2 = high phisical activity
mu_cog_no <- rbind(c(-4.05, -3.13),
                   c(-2.02, -0.92))

mu_cog <- as.vector(x = c(mu_cog_lum, mu_cog_no))

cognitive_decline <- rnorm(n = 14 * 8, mean = rep(mu_cog, each = 14), sd = 1.82)

luminosity <- rep(c("yes", "no"), each = 14 * 4)

social_interaction <- rep(rep(c("low", "high"), each = 14 * 2), times = 2)

physical_activity <- rep(rep(rep(c("low", "high"), each = 14), times = 2), 
                         times = 2)

decline <- tibble("id" = as.character(seq(1,112)),
                  cognitive_decline,
                  luminosity,
                  social_interaction,
                  physical_activity)

write_csv(x = decline, file = "data/final-project/project-2.csv")


# Problem 3: The IKEA problem ---------------------------------------------

# In this data set we have observations about the price that people assign to 
# furniture. There are 3 independent variables, an indicator function that
# takes the value TRUE if the participant build the furniture themselves and 
# false otherwise, the dificulty to build with levels "easy" and "medium" and a
# total cost variable which is continuous. We have a total of 160 observations 

# Values of beta in linear regression, the first value represents the 
# intercept which will be different by group the second row represents the 
# impact of the total value of the materials, easiness to transport has no 
# effect on the mean 

beta <- rbind(c(10, 18.2),
              c(1.06, 1.06))

self_build <- rep(c(FALSE, TRUE), each = 80)

total_cost <- rgamma(n = 160, shape = 120^2 / 10^2, rate = 120 / 10^2)

mu_price <- c()

for(i in 1: 160){
  mu_price[i] <- beta[1,(self_build[i]+1)] + beta[2,1] * total_cost[i]   
}

asking_price <- rnorm(n = 160, mean = mu_price, sd = 6)

difficulty <- rep(rep(c("easy", "medium"), each = 40), times = 2)

ikea_effect <- tibble("id" = as.character(seq(1,160)),
                      asking_price,
                      self_build,
                      difficulty,
                      total_cost)

write_csv(x = ikea_effect, file = "data/final-project/project-3.csv")
