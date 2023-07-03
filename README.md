# PK-PD

１번문제

library("dplyr")
library("ggplot2")

dataset <- read.csv("C:/PKPD/pkpd_dataset.csv") |> 
  subset(CMT==2 & CYCLE==1,) |> 
  group_by(DOSE, ID) 
mutate(DV=as.numeric(LIDV)) |> 
  mutate(DV_mean=mean(LIDV, na.rm = TRUE)) 
  mutate(DV_SD =sd(LIDV, na.rm = TRUE)) 
  mutate(DV_SD=as.numeric(LIDV_SD))


ggplot(dataset, aes(x = TIME, y = LIDV)) +
  geom_line(aes(color = factor(DOSE)), size = 0.5)

  ![1번_R](https://github.com/jueun429/PK-PD-R-study-/assets/133086206/0adc316c-53ff-496a-b70c-0a32dcbae83c)


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


3번문제

library("dplyr")
library("ggplot2")
install.packages("NonCompart")
install.packages("knitr")
library(NonCompart)
library(knitr)

NCA_1 <- read.csv("C:/PKPD/pkpd_dataset.csv") |> 
  filter(NAME == "PK Concentration" & !is.na(LIDV)) |>
  select(ID, NOMTIME, LIDV, DOSE, NAME)

nca_result <- tblNCA(NCA, key=c("ID", "DOSE"), colTime="NOMTIME", colConc="LIDV",timeUnit = "h", doseUnit="mg", concUnit="ng/mL")

NCA_CMAX <- nca_result |> 
  select(ID, DOSE, CMAX) |>
  group_by(DOSE) |>
  summarize(CMAX_mean = mean(CMAX),
            CMAX_median = median(CMAX),
            CMAX_sd = sd(CMAX),
            CMAX_min = min(CMAX),
            CMAX_max = max(CMAX)
  )

![3번CMAX](https://github.com/jueun429/PK-PD-R-study-/assets/133086206/c7af8ed1-6335-42ac-923a-e7d8586e549b)
