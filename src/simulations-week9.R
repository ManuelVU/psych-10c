# simulation example 1 week 9 ---------------------------------------------

library(tidyverse)

age <- c(sample(x = seq(20,34), size = 50, replace = TRUE),
         sample(x = seq(50,64), size = 50, replace = TRUE))

age_id <- as.numeric(age > 34)

time_probe <- round(rgamma(n = 100, shape = 30, rate = 1),0)

mu_recall <- 95 + 5 * age_id - 0.3 * time_probe - 0.5 * age_id * time_probe 

recalled <- round(rnorm(n = 100, mean = mu_recall, sd = 6),0)

recalled <- ifelse(test = recalled > 100,
                   yes = 100,
                   no = recalled)

memory <- tibble("correct" = recalled,
                 "time_min" = time_probe,
                 "age" = age)

write_csv(x = memory, file = here::here("data/week-9/memory-example.csv"))

summary(lm(recalled~age_id))
summary(lm(recalled~age_id+time_probe))
summary(lm(recalled~age_id+time_probe+age_id*time_probe))
