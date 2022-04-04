# This is a simulation used to generate the example used in lecture 1.

read_csv(file = "https://raw.githubusercontent.com/ManuelVU/psych-10c-data/main/hw-problem-1.csv")

id <- rep(as.character(seq(1,100)), times = 2)

age <- rep(rpois(n = 100, lambda = rep(x = c(24, 50), each = 50)), times = 2)

test_1 <- rbinom(n = 100, size = 50, prob = rep(x = c(0.92, 0.88), each = 50))

test_2 <- rbinom(n = 100, size = 50, prob = rep(x = c(0.82, 0.65), each = 50))

time_test <- rep(c(300, 3600), each = 100)

correct <- c(test_1, test_2)

memory <- tibble(id, age, correct, time_test)

write_csv(x = memory, file = "data/week-1/example.csv")



ggplot(data = memory, 
       aes(x = correct, fill = test_id)) + 
  geom_histogram(binwidth = 1, border = "white")
  


# Example R intro ---------------------------------------------------------

# R intro example ---------------------------------------------------------

library(tidyverse)

id <- rep(seq(1,10), times = 2)

mu <- 20

control <- rnorm(n = 10, mean = mu, sd = 2)

incongruent <- rnorm(n = 10, mean = mu + 15, sd = 3)

stroop <- tibble("id" = as.character(id), 
                 "response_time" = c(control, incongruent),
                 "condition" = rep(c("control", "incongruent"), each = 10))

stroop_wide <- stroop %>% 
  pivot_wider(id_cols = id, names_from = condition, 
              values_from = response_time)

write_csv(x = stroop, file = "data/week-2/stroop.csv")
write_csv(x = stroop_wide, file = "data/week-2/stroop-wide.csv")


