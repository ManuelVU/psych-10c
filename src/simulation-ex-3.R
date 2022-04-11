# week 3 example midterm final grades:
library(tidyverse)

set.seed(198612)

mu_midterm <- 68.5

mu_final <- 69.6

tau_ind <- rnorm(n = 100, mean = 0, sd = 5)

midterm <- round(x = rnorm(n = 100, mean = mu_midterm + tau_ind, sd = 2), digits = 1)
final <- round(x = rnorm(n = 100, mean = mu_final + tau_ind, sd = 2), digits = 1)

exams <- tibble("id" = as.character(seq(1,100)),
                midterm,
                final)

write_csv(x = exams, file = "data/week-3/exams-example.csv")

