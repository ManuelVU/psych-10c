# Simulation of first example on week 6 -----------------------------------
library(tidyverse)

mu_2019_stats <- 12
mu_2020_stats <- 13
mu_2019_no <- 10
mu_2020_no <- 11

mu <- c(mu_2019_stats, mu_2019_no,
        mu_2020_stats, mu_2020_no)

anxiety <- round(rnorm(n = 27 * 4, mean = rep(x = mu, each = 27),
                       sd = 2),0)

students <- tibble("id" = seq(1, 27 * 4),
                   "anxiety" = anxiety,
                   "cohort" = rep(x = c(2019,2020), each = 27 * 2),
                   "class" = rep(x = rep(x = c("statistics", "other"),
                                         each = 27), times = 2))

write_csv(x = students, file = "data/week-5/anxiety-w5.csv")


# Simulation of second example on week 6 -----------------------------------


