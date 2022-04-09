n_total <- nrow(exams)

exams <- exams %>% 
  mutate("score_diff" = final - midterm)

null_pred <- 0

eff_pred <- mean(exams$score_diff)

exams <- exams %>% 
  mutate("null_pred" = rep(x = null_pred, times = n_total),
         "eff_pred" = rep(x = eff_pred, times = n_total)) %>% 
  mutate("null_error" = (score_diff - null_pred)^2,
         "eff_error" = (score_diff - eff_pred)^2)

errors <- exams %>% 
  summarise("sse_0" = sum(null_error),
            "sse_e" = sum(eff_error),
            "mse_0" = 1/n_total * sum(null_error),
            "mse_e" = 1/n_total * sum(eff_error))

evaluation <- errors %>% 
  summarise("r_squared" = (sse_0 - sse_e) / sse_0,
            "bic_0" = n_total * log(mse_0),
            "bic_e" = n_total * log(mse_e) + log(n_total))


ggplot(data = data.frame(grades = c(midterm,final)), 
       aes(x = grades)) +
  geom_histogram(binwidth = 1.5)

ggplot(data = data.frame(grades = c(midterm,final),
                         exam = c(rep("midterm",n_total),
                                  rep("final",n_total))), 
       aes(x = grades, fill = exam, color = exam)) +
  geom_histogram(binwidth = 1, alpha = 0.5)

ggplot(data = exams) +
  aes(x = score_diff) + 
  geom_histogram(binwidth = 1.5)



