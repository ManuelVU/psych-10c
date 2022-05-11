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


# simple to multiple linear regression ------------------------------------

angle <- c(10,45,90,130,160,180)
ages <- sample(x = seq(7,21), size = 50, replace = TRUE)

ages <- rep(x = ages, each = length(angle))
angle <- rep(x = angle, times = 50)

beta <- c(600, 15, -10)
epsilon <- rnorm(n = length(angle), mean = 0, sd = 200)

rsp_time <- round(beta[1] + beta[2] * angle + beta[3] * ages + epsilon,2)


mental_rotation <- tibble("response_time" = rsp_time, 
                          "angle" = angle,
                          "age" = ages)

write_csv(x = mental_rotation, file = "data/week-7/mental-rotation.csv")
