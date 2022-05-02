# Simulations used on homework 3
library(tidyverse)

# nap time and memory -----------------------------------------------------

mu_control <- rep(x = 0.6, times = 45)
mu_15 <- rep(x = 0.67, times = 45)
mu_60 <- rep(x = 0.71, times = 45)

recalled <- rbinom(n = 3 * 45, size = 40, prob = c(mu_control, mu_15, mu_60))

free_recall <- tibble("id" = as.character(x = seq(1, 3 * 45)),
                      "recalled" = as.integer(recalled),
                      "condition" = rep(c("control", "15_min", "60_min"), 
                                        each = 45))
write_csv(x = free_recall, file = "data/week-4/homework3-p1.csv")

#### Simulations Used on homework 4 ----

# memory: gender and word frequency ---------------------------------------

mu <- c(9,13,
        15,10)

writen <- rbinom(n = 25*4, size = 20, prob = rep(x = mu/20, each = 25))

memory <- tibble("id" = as.character(x = seq(1, 100)),
                 "recalled" = as.integer(writen),
                 "task" = rep(c("free_recall", "recognition"), each = 50),
                 "word_frequency" = rep(rep(c("low", "high"), 
                                                 each = 25), times = 2))

write_csv(x = memory, file = "data/week-4/homework4.csv")
