# Simulation week 10 ------------------------------------------------------
library(tidyverse)

test <- rbinom(n = 80, size = 1, prob = rep(c(0.3,0.15),each=40))

covid <- tibble("test_pcr" = ifelse(test = test == 0, yes = "negative", 
                                    no = "positive"),
                "test" = test,
                "status" = rep(c("not_vaccinated", "vaccinated"),each = 40))

write_csv(x = covid, file = "data/week-10/covid-example.csv")


BIC(glm(test~status, family = binomial, data=covid))
