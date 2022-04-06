library(tidyverse)

# Part 1 ------------------------------------------------------------------

# sample size by group
size_1 <- 40 

# mean by group
mu <- c(100, 110)

# standard deviation by group
sigma <- c(3, 3)

# simulate iq group 1
group_1 <- round(x = rnorm(n = size_1, mean = mu[1], sd = sigma[1]), 
                 digits = 0)

# simulate iq group 2
group_2 <- round(x = rnorm(n = size_1, mean = mu[2], sd = sigma[2]), 
                 digits = 0)

# write as a dataframe
coffee <- data.frame("iq" = c(group_1,group_2),
                     "group" = c(rep("no_coffee",size_1),
                                 rep("coffee", size_1)))

# save as csv file
write_csv(x = coffee, file = "data/week-2/hw-2-problem-1.csv")

# Part 2 ------------------------------------------------------------------

# sample size by group
size_2 <- 100 

# mean
mu <- 100

# standard deviation
sigma <- 5

# response times before training
control <- rnorm(n = size_2, mean = mu + 2, sd = sigma)

# simulate response times for control condition
experiment <- rnorm(n = size_2, mean = mu, sd = sigma)

# write as a data frame
response_time <- data.frame("response_time_before" = control,
                            "response_time_after" = c(experiment),
                            "group" = c(rep("experimental",size_2)))

# save as csv file
write_csv(x = response_time, file = "data/week-2/hw-2-problem-2.csv")


