# R 과제

**１번문제**

```{r}
library("dplyr")
library("ggplot2")
dataset <- read.csv("C:/PKPD/pkpd_dataset.csv") |> 
  subset(CMT==2 & CYCLE==1,) |> 
  group_by(DOSE, ID) |> 
mutate(DV=as.numeric(LIDV)) |> 
  mutate(DV_mean=mean(LIDV, na.rm = TRUE)) |> 
  mutate(DV_SD =sd(LIDV, na.rm = TRUE)) |> 
  mutate(DV_SD=as.numeric(LIDV_SD))
ggplot(dataset, aes(x = TIME, y = LIDV, group = ID)) + geom_point(aes(color = factor(DOSE))) + 
  geom_line(aes(color = factor(DOSE)), size = 0.5) + facet_wrap(~DOSE) y = LIDV, group = ID)) + 
geom_line(aes(color = factor(DOSE)), size = 0.5)
```


![1과제](https://github.com/jueun429/PK-PD-R-study-/assets/133086206/f4470951-3e5b-4c5c-aa73-1f9b114a6922)

```{r}
library("dplyr")
library("ggplot2") 



dataset <- read.csv("C:/PKPD/pkpd_dataset.csv") |> 
  subset(CMT==2 & CYCLE==1,) |> 
  group_by(DOSE, ID) |> 
mutate(DV=as.numeric(LIDV)) |> 
  mutate(DV_mean=mean(LIDV, na.rm = TRUE)) |> 
  mutate(DV_SD =sd(LIDV, na.rm = TRUE)) |> 
  mutate(DV_SD=as.numeric(LIDV_SD))


ggplot(dataset, aes(x = TIME, y = LIDV, group = ID)) + geom_point(aes(color = factor(DOSE))) + 
  geom_line(aes(color = factor(DOSE)), size = 0.5)
```

![1R](https://github.com/jueun429/PK-PD-R-study-/assets/133086206/3871183b-fb70-41fe-8356-557614304823)



**2번문제**
```{r}
library("dplyr")
library("ggplot2") 

data_mean_sd <- read.csv("C:/PKPD/pkpd_dataset.csv") |> 
  filter(CYCLE == 1 & NAME == "PK Concentration") |>
  select(ID, NAME, NOMTIME, LIDV, DOSE) |>
  mutate(DOSE = as.numeric(DOSE))



data_mean_sd_1 <- data_mean_sd |> 
  group_by(DOSE, NOMTIME) |>
  summarize(LIDV_mean = mean(LIDV),
            LIDV_sd = sd(LIDV)
  )

data_mean_sd_1 |>
  ggplot(aes(x = NOMTIME, y = LIDV_mean)) + geom_line(aes(color = factor(DOSE)), size = 0.5) + geom_point(aes(color = factor(DOSE))) + geom_errorbar(aes(ymin = LIDV_mean + LIDV_sd, ymax = LIDV_mean - LIDV_sd, color = factor(DOSE))) + facet_wrap(~DOSE)
```
![2과제](https://github.com/jueun429/PK-PD-R-study-/assets/133086206/35b53f77-5e7b-4710-8327-cd5f5a1e0802)

```{r}
library("dplyr")
library("ggplot2") 

data_mean_sd <- read.csv("C:/PKPD/pkpd_dataset.csv") |> 
  filter(CYCLE == 1 & NAME == "PK Concentration") |>
  select(ID, NAME, NOMTIME, LIDV, DOSE) |>
  mutate(DOSE = as.numeric(DOSE))



data_mean_sd_1 <- data_mean_sd |> 
  group_by(DOSE, NOMTIME) |>
  summarize(LIDV_mean = mean(LIDV),
            LIDV_sd = sd(LIDV)
  )

data_mean_sd_1 |>
  ggplot(aes(x = NOMTIME, y = LIDV_mean)) + geom_line(aes(color = factor(DOSE)), size = 0.5) + geom_point(aes(color = factor(DOSE))) + geom_errorbar(aes(ymin = LIDV_mean + LIDV_sd, ymax = LIDV_mean - LIDV_sd, color = factor(DOSE)))
```

![2R](https://github.com/jueun429/PK-PD-R-study-/assets/133086206/3fb9d94d-abaf-4cc4-bd4a-211fbcbfbd0f)



**3번문제**
```{r}
library("dplyr")
library("ggplot2")
install.packages("NonCompart")
install.packages("knitr")
library(NonCompart)
library(knitr)

NCA_1 <- read.csv("C:/PKPD/pkpd_dataset.csv") |> 
  filter(NAME == "PK Concentration" & !is.na(LIDV)) |>
  select(ID, NOMTIME, LIDV, DOSE, NAME)

NCA_result <- tblNCA(NCA_1, key=c("ID", "DOSE"), colTime="NOMTIME", colConc="LIDV",timeUnit = "h", doseUnit="mg", concUnit="ng/mL")

NCA_CMAX <- NCA_result |> 
  select(ID, DOSE, CMAX) |>
  group_by(DOSE) |>
  summarize(CMAX_mean = mean(CMAX, na.rm = T),
            CMAX_median = median(CMAX, na.rm = T),
            CMAX_sd = sd(CMAX, na.rm = T),
            CMAX_min = min(CMAX, na.rm = T),
            CMAX_max = max(CMAX, na.rm = T)
  )
```

![3r](https://github.com/jueun429/PK-PD-R-study-/assets/133086206/828aa6e1-de64-47c4-824b-b62230e91b2b)

![3r-1](https://github.com/jueun429/PK-PD-R-study-/assets/133086206/b3b8b772-e7b8-491f-acf5-f17f16ecf2dd)
