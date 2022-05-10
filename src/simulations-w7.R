# simulations week 7 examples ---------------------------------------------
library(tidyverse)

beta <- c(90,-1.5)

x <- rpois(n = 130, lambda = 5)

epsilon <- round(rnorm(n = 130, mean = 0, 10))

y <- beta[1] + beta[2] * x + epsilon

y <- ifelse(test = y > 100, yes = 100, no = y)

grades <- tibble("grade" = y, 
                 "classes_missed" = x)

grades %>% 
  ggplot(aes(x = classes_missed, y = grade)) +
  geom_point()

write_csv(x = grades, file = "data/week-7/sim-examples.csv")
