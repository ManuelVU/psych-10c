# simulation homework 5 ---------------------------------------------------
library(tidyverse)

eff_hr_nomiss <- 2
eff_hr_miss <- -1

intercept <- 70
diff_miss <- - 2

study_hr <- round(rgamma(n = 100, shape = 6, rate = 2),1)
missed <- rbinom(n = 100, size = 1, prob = 0.4) * rpois(n = 100, lambda = 5)

grade <- round(intercept + diff_miss * as.numeric(missed>0) + 
  eff_hr_nomiss * study_hr + 
  eff_hr_miss * as.numeric(missed>0) * study_hr +
  rnorm(n = 100, mean = 0, sd = 4),1)

stats_grades <- tibble("grade" = grade, 
                       "study_week" = study_hr,
                       "classes_missed" = missed)

write_csv(x = stats_grades, file = "data/week-8/homework5.csv")

ggplot(data = stats_grades, aes(x = study_week, y = grade, 
                                color = classes_missed == 0)) +
  geom_point() +
  geom_smooth(method = lm, formula = y~x)


BIC(lm(grade~study_week + classes_missed, data = stats_grades))

BIC(lm(grade~study_week + (classes_missed > 0), data = stats_grades))

BIC(lm(grade~study_week + (classes_missed > 0) + (classes_missed > 0) * study_week, 
       data = stats_grades))

