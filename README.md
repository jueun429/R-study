# PK-PD

１번문제

library("dplyr")
library("ggplot2") 


dataset <- read.csv("C:/PKPD/pkpd_dataset.csv") |> 
  subset(CMT==2 & CYCLE==1,) |> 
  group_by(DOSE, ID) #%>%
mutate(DV=as.numeric(LIDV)) |> 
  mutate(DV_mean=mean(LIDV, na.rm = TRUE)) |> 
  mutate(DV_SD =sd(LIDV, na.rm = TRUE)) |> 
  mutate(DV_SD=as.numeric(LIDV_SD))


ggplot(dataset, aes(x = TIME, y = LIDV)) +
  geom_line(aes(color = factor(DOSE)), size = 0.5)
![1번](https://github.com/jueun429/PK-PD-R-study-/assets/133086206/a8424378-35f9-47a5-bf90-4adf32b06ca8)

2번문제

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

![2번](https://github.com/jueun429/PK-PD-R-study-/assets/133086206/49d4d4a5-05fd-49f4-a38e-c3d30b7281e5)
