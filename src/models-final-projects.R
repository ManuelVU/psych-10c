# Modeling split brain ----------------------------------------------------
library(tidyverse)
link <- "https://raw.githubusercontent.com/ManuelVU/psych-10c-data/main/problem-1.csv"
split <- read_csv(file = link)

ggplot(data = split, aes(x = visual_field, y = standard_test, fill = split_surgery))+
geom_boxplot()

n_total <- nrow(split)

null <- split %>% 
  summarise("mean" = mean(standard_test)) %>% 
  pull(mean)

split <- split %>% 
  mutate("prediction_null" = null,
         "error_null" = (standard_test - prediction_null)^2)

sse_null <- sum(split$error_null)
mse_null <- 1/n_total * sse_null
bic_null <- n_total * log(mse_null) + 1 * log(n_total)

me_field <- split %>% 
  group_by(visual_field) %>% 
  summarise("mean" = mean(standard_test))

alpha <- me_field$mean - null

split <- split %>% 
  mutate("prediction_field" = case_when(visual_field == "left" ~ me_field$mean[1],
                                        visual_field == "right" ~ me_field$mean[2]),
         "error_field" = (standard_test - prediction_field)^2)

sse_field <- sum(split$error_field)
mse_field <- 1/n_total * sse_field
r2_field <- (sse_null - sse_field)/sse_null
bic_field <- n_total * log(mse_field) + 2 * log(n_total)

me_sur <- split %>% 
  group_by(split_surgery) %>% 
  summarise("mean" = mean(standard_test))

beta <- me_sur$mean - null

split <- split %>% 
  mutate("prediction_sur" = case_when(split_surgery == "no_surgery" ~ me_sur$mean[1],
                                      split_surgery == "surgery" ~ me_sur$mean[2]),
         "error_sur" = (standard_test - prediction_sur)^2)

sse_sur <- sum(split$error_sur)
mse_sur <- 1/n_total * sse_sur
r2_sur <- (sse_null - sse_sur)/sse_null
bic_sur <- n_total * log(mse_sur) + 2 * log(n_total)

split <- split %>% 
  mutate("prediction_add" = case_when(visual_field == "left" & split_surgery == "no_surgery" ~ null + alpha[1] + beta[1],
                                      visual_field == "left" & split_surgery == "surgery" ~ null + alpha[1] + beta[2],
                                      visual_field == "right" & split_surgery == "no_surgery" ~ null + alpha[2] + beta[1],
                                      visual_field == "right" & split_surgery == "surgery" ~ null + alpha[2] + beta[2]),
         "error_add" = (standard_test - prediction_add)^2)

sse_add <- sum(split$error_add)
mse_add <- 1/n_total * sse_add
r2_add <- (sse_null - sse_add)/sse_null
bic_add <- n_total * log(mse_add) + 3 * log(n_total)

full <- split %>% 
  group_by(visual_field, split_surgery) %>% 
  summarise("mean" = mean(standard_test))

split <- split %>% 
  mutate("prediction_full" = case_when(visual_field == "left" & split_surgery == "no_surgery" ~ full$mean[1],
                                      visual_field == "left" & split_surgery == "surgery" ~ full$mean[2],
                                      visual_field == "right" & split_surgery == "no_surgery" ~ full$mean[3],
                                      visual_field == "right" & split_surgery == "surgery" ~ full$mean[4]),
         "error_full" = (standard_test - prediction_full)^2)

sse_full <- sum(split$error_full)
mse_full <- 1/n_total * sse_full
r2_full <- (sse_null - sse_full)/sse_null
bic_full <- n_total * log(mse_full) + 4 * log(n_total)
