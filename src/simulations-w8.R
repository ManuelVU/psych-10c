# Example week 8 ----------------------------------------------------------

library(tidyverse)

age_eff <- 0.5

gender_eff <- -13

height_eff <- 0

age <- sample(x = seq(20,70),size = 50,replace = T)

height <- round(rnorm(n = 50, mean = rep(c(70,64.5), each = 25), 
                sd = rep(c(3,2.5), each = 25)),1)

gender_id <- rep(c(0,1), each = 25)

linear_mod <- 99 + age_eff * age + height_eff * height + gender_eff * gender_id

blood_preassure <- round(rnorm(n = 50, mean = linear_mod, sd = 5),0)

sys_preassure <- tibble(blood_preassure, age, height, 
                        sex_at_birth = rep(c("male", "female"), each = 25))

write_csv(x = sys_preassure, file = "data/week-8/sys-pressure.csv")


summary(lm(blood_pressure~age+height+sex_at_birth, data = pressure))
