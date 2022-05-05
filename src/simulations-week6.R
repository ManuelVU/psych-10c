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

mu_no_no <- 0.2
mu_no_yes <- -0.6
mu_yes_no <- -0.5
mu_yes_yes <- -2.7
mu <- c(mu_no_no, mu_no_yes, mu_yes_no, mu_yes_yes)

beck_diff <- round(x = rnorm(n = 25 * 4, mean = rep(x = mu, each = 25), sd = 1.7),
                    digits = 2)

depression <- tibble("id" = seq(1, 25 * 4),
                     "beck_diff" = beck_diff,
                     "pharma_treatment" = rep(x = c("no", "yes"), each = 25 * 2),
                     "therapy" = rep(x = rep(x = c("no", "yes"), each = 25), 
                                     times = 2))

ggplot(data = depression) +
  aes(x = therapy) + 
  aes(y = beck_diff) + 
  aes(fill = pharma_treatment) +
  geom_boxplot()

anova(lm(formula = beck_diff ~ pharma_treatment + therapy + pharma_treatment * therapy,
         data = depression))


full <- depression %>% 
  group_by(pharma_treatment, therapy) %>% 
  summarise("pred" = mean(beck_diff))

mean_pharma <- depression %>% 
  group_by(pharma_treatment) %>% 
  summarise("average" = mean(beck_diff))

mean_therapy <- depression %>% 
  group_by(therapy) %>% 
  summarise("average" = mean(beck_diff))

grand_mean <- depression %>% 
  summarise("average" = mean(beck_diff)) %>% 
  pull(average)

alpha_pharma <- c(mean_pharma$average[1] - grand_mean,
                  grand_mean - mean_pharma$average[1])

beta_therapy <- c(mean_therapy$average[1] - grand_mean,
                  grand_mean - mean_therapy$average[1])


depression <- depression %>% 
  mutate("prediction_null" = grand_mean, 
         "prediction_pharma" = 
           case_when(pharma_treatment == "no" ~ grand_mean + alpha_pharma[1],
                     pharma_treatment == "yes" ~ grand_mean + alpha_pharma[2]),
         "prediction_therapy" = 
           case_when(therapy == "no" ~ grand_mean + beta_therapy[1],
                     therapy == "yes" ~ grand_mean + beta_therapy[2]),
         "prediction_additive" = 
           case_when(pharma_treatment == "no" & therapy == "no" ~ 
                       grand_mean + alpha_pharma[1] + beta_therapy[1],
                     pharma_treatment == "no" & therapy == "yes" ~ 
                       grand_mean + alpha_pharma[1] + beta_therapy[2],
                     pharma_treatment == "yes" & therapy == "no" ~ 
                       grand_mean + alpha_pharma[2] + beta_therapy[1],
                     pharma_treatment == "yes" & therapy == "yes" ~ 
                       grand_mean + alpha_pharma[2] + beta_therapy[2]),
         "prediction_full" = 
           case_when(pharma_treatment == "no" & therapy == "no" ~ full$pred[1],
                     pharma_treatment == "no" & therapy == "yes" ~ full$pred[2],
                     pharma_treatment == "yes" & therapy == "no" ~ full$pred[3],
                     pharma_treatment == "yes" & therapy == "yes" ~ full$pred[4]))

depression <- depression %>% 
  mutate("error_null" = (beck_diff - prediction_null)^2,
         "error_pharma" = (beck_diff - prediction_pharma)^2,
         "error_therapy" = (beck_diff - prediction_therapy)^2,
         "error_additive" = (beck_diff - prediction_additive)^2,
         "error_full" = (beck_diff - prediction_full)^2)

n_total <- nrow(depression)

sse_null <- sum(depression$error_null)
sse_pharma <- sum(depression$error_pharma)
sse_therapy <- sum(depression$error_therapy)
sse_additive <- sum(depression$error_additive)
sse_full <- sum(depression$error_full)

mse_null <- 1/n_total * sse_null
mse_pharma <- 1/n_total * sse_pharma
mse_therapy <- 1/n_total * sse_therapy
mse_additive <- 1/n_total * sse_additive
mse_full <- 1/n_total * sse_full

bic_null <- n_total * log(mse_null) + log(n_total)
bic_pharma <- n_total * log(mse_pharma) + 2 * log(n_total)
bic_therapy <- n_total * log(mse_therapy) + 2 * log(n_total)
bic_additive <- n_total * log(mse_additive) + 3 * log(n_total)
bic_full <- n_total * log(mse_full) + 4 * log(n_total)

depression <- depression %>% 
  select(id, pharma_treatment, therapy, beck_diff)

write_csv(x = depression, file = "data/week-5/depression-6.csv")



# Simulation end week 6 ---------------------------------------------------

no_mult_lap <- 80
no_mult_notes <- 80
no_one_lap <- 74
no_one_notes <- 74
yes_mult_lap <- 72
yes_mult_notes <- 72
yes_one_lap <- 66
yes_one_notes <- 66

mu <- c(no_mult_lap, no_mult_notes,
        no_one_lap, no_one_notes,
        yes_mult_lap, yes_mult_notes,
        yes_one_lap, yes_one_notes)

midterm <- rbinom(n = 18 * 8, size = 100, prob = rep(x = mu/100, each = 18))


scores <- tibble("midterm" = midterm,
                 "music" = rep(x = c("no_music", "music"), each = 18 * 4),
                 "study" = rep(rep(x = c("distributed", "one_day"), each = 18 * 2),
                               times = 2),
                 "notes" = rep(rep(rep(c("slides", "notes"), each = 18), times = 2),
                               times = 2))

anova(lm(midterm ~ music + study + notes, data = scores))

write_csv(x = scores, file = "data/week-5/midterm-w6.csv")



