# PK-PD-R-study-

library("dplyr")
library("ggplot2") 

data_mean_sd <- read.csv("C:/PKPD/pkpd_dataset.csv") |> 
  filter(CYCLE == 1 & NAME == "PK Concentration") |>
  select(ID, NOMTIME, LIDV, NAME, DOSE) |>
  mutate(DOSE = as.numeric(DOSE))



data_mean_sd_1 <- data_mean_sd |> 
  group_by(DOSE, NOMTIME) |>
  summarize(LIDV_mean = mean(LIDV),
            LIDV_sd = sd(LIDV)
  )

data_mean_sd_1 |>
  ggplot(aes(x = NOMTIME, y = LIDV_mean)) + geom_line() + geom_point() +
  geom_errorbar(aes(ymin = LIDV_mean + LIDV_sd, ymax = LIDV_mean - LIDV_sd)) +
  facet_wrap(~DOSE) + labs(title = "cycle1_pk")
