# This is a simulation of the data on the smokers example used in week 2
smokers <- tibble("id" = as.character(seq(1,8)),
                  "lung_capacity" = round(x = rnorm(n = 8, 
                                                    mean = rep(x = c(77, 70), each = 4), 
                                                    sd = rep(x = c(2, 3), each = 4)), 
                                          digits = 1),
                  "smoke_status" = rep(c("non_smoker", "smoker"), each = 4))


write_csv(x = smokers, file = "data/week-2/example-smoke.csv")
