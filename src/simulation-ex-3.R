# week 3 example midterm final grades:
library(tidyverse)

set.seed(198612)

midterm <- round(x = rnorm(n = 100, mean = 68.5, sd = 6), digits = 1)
final <- round(x = rnorm(n = 100, mean = 71.2, sd = 5.2), digits = 1)

exams <- tibble("id" = as.character(seq(1,100)),
                midterm,
                final)

write_csv(x = exams, file = "data/week-3/exams-example.csv")

