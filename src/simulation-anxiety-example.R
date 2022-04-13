# Example of anxiety levels on students from cohorts 2018 and 2019

library(tidyverse)

anxiety <- round(x = rnorm(n = 60, mean = 9, sd = 2.2), digits = 0)
cohort <- rep(x = c("2018","2019"), each = 30)

cohort_anxiety_1 <- tibble("id" = as.character(seq(1,60)),
                         "anxiety" = as.integer(anxiety),
                         "cohort" = as.character(cohort)) 

write_csv(x = cohort_anxiety_1, file = "data/week-3/anxiety-1-ex.csv")

anxiety <- round(x = rnorm(n = 90, mean = c(rep(9,60),rep(12,30)), sd = 2.2), digits = 0)
cohort <- rep(x = c("2018","2019","2020"), each = 30)

cohort_anxiety <- tibble("id" = as.character(seq(1,90)),
                         "anxiety" = as.integer(anxiety),
                         "cohort" = as.character(cohort)) 

write_csv(x = cohort_anxiety, file = "data/week-3/anxiety-2-ex.csv")

