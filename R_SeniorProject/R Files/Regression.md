Regression
================
Henry Siegler

``` r
library(tidyverse)
library(here)
library(tinytex)
library(estimatr)
library(texreg)
library(stargazer)
library(sandwich)
```

## ALL OPIOIDS

``` r
df <- read_csv(here("Cleaned_Data", "DATA.csv"))

mod_total <- lm_robust(total ~ PDMP + avg_age + bachelors_pct + median_income
                       + unemp_rate + white_pct + personal_health_care + 
                         factor(year) + factor(state), 
                       data = df, se_type = "HC1")
```

``` r
mod2_total <- lm(total ~ PDMP + avg_age + bachelors_pct + median_income
                       + unemp_rate + white_pct + personal_health_care +
             factor(year) + factor(state), data = df)

cov2_total <- vcovHC(mod2_total, type = "HC1")
robust_se2_total <- sqrt(diag(cov2_total))


mod1_total <- lm(total ~ PDMP + factor(year) + factor(state), data = df)
cov1_total <- vcovHC(mod1_total, type = "HC1")
robust_se1_total <- sqrt(diag(cov1_total))
```

``` r
fe_vector <- c("Fixed Effects", "Yes", "Yes")
covariate_vector <- c("Covariates", "No", "Yes")


stargazer(mod1_total, mod2_total, type = "latex",
          se = list(robust_se1_total, robust_se2_total), omit = c("factor"),
          add.lines = list(fe_vector, covariate_vector),
          dep.var.caption = "Dependent Variable: Total Opioid Overdose Death Rate per 100,000",
          dep.var.labels.include = FALSE,
          covariate.labels = c("PDMP in Operation", 
                               "Average Age", 
                               "Percentage with Bachelor's Degree",
                               "Median Income", 
                               "Unemployment Rate", 
                               "Percentage that is White", 
                               "Personal Healthcare Spending per Capita"))
```

% Table created by stargazer v.5.2.3 by Marek Hlavac, Social Policy
Institute. E-mail: marek.hlavac at gmail.com % Date and time: Mon, Jun
26, 2023 - 11:35:37 PM
## HEROIN SYNTHETIC

``` r
mod <- lm_robust(heroin_synthetic ~ PDMP + avg_age + bachelors_pct + median_income
                       + unemp_rate + white_pct + personal_health_care + 
                         factor(year) + factor(state), data = df, se_type = "HC1")
```

``` r
mod2_heroin <- lm(heroin_synthetic ~ PDMP + avg_age + bachelors_pct + median_income
                       + unemp_rate + white_pct + personal_health_care +
                    factor(year) + factor(state), data = df)

cov2_heroin <- vcovHC(mod2_heroin, type = "HC1")
robust_se2_heroin <- sqrt(diag(cov2_heroin))


mod1_heroin <- lm(heroin_synthetic ~ PDMP + factor(year) + factor(state), data = df)

cov1_heroin <- vcovHC(mod1_heroin, type = "HC1")
robust_se1_heroin <- sqrt(diag(cov1_heroin))
```

``` r
fe_vector <- c("Fixed Effects", "Yes", "Yes")
covariate_vector <- c("Covariates", "No", "Yes")


stargazer(mod1_heroin, mod2_heroin, type = "latex",
          se = list(robust_se1_heroin, robust_se2_heroin), omit = c("factor"),
          add.lines = list(fe_vector, covariate_vector),
          dep.var.caption = "Dependent Variable: Heroin and Synthetic Opioid Overdose Death Rate per 100,000",
          dep.var.labels.include = FALSE,
          covariate.labels = c("PDMP in Operation", 
                               "Average Age", 
                               "Percentage with Bachelor's Degree",
                               "Median Income", 
                               "Unemployment Rate", 
                               "Percentage that is White", 
                               "Personal Healthcare Spending per Capita"))
```

% Table created by stargazer v.5.2.3 by Marek Hlavac, Social Policy
Institute. E-mail: marek.hlavac at gmail.com % Date and time: Mon, Jun
26, 2023 - 11:35:38 PM
## PRESCRIPTION

``` r
mod_prescription <- lm_robust(prescription ~ PDMP + avg_age + bachelors_pct + median_income
                       + unemp_rate + white_pct + personal_health_care +
                         factor(year) + factor(state), data = df, se_type = "HC1")
```

``` r
mod2_prescription <- lm(prescription ~ PDMP + avg_age + bachelors_pct + median_income
                       + unemp_rate + white_pct + personal_health_care + 
                     factor(year) + factor(state), data = df)

cov2_prescription <- vcovHC(mod2_prescription, type = "HC1")
robust_se2_prescription <- sqrt(diag(cov2_prescription))


mod1_prescription <- lm(prescription ~ PDMP + factor(year) + factor(state), data = df)

cov1_prescription <- vcovHC(mod1_prescription, type = "HC1")
robust_se1_prescription <- sqrt(diag(cov1_prescription))
```

``` r
fe_vector <- c("Fixed Effects", "Yes", "Yes")
covariate_vector <- c("Covariates", "No", "Yes")


stargazer(mod1_prescription, mod2_prescription, type = "latex",
          se = list(robust_se1_prescription, robust_se2_prescription), omit = c("factor"),
          add.lines = list(fe_vector, covariate_vector),
          dep.var.caption = "Dependent Variable: Prescription Opioid Overdose Death Rate per 100,000",
          dep.var.labels.include = FALSE,
          covariate.labels = c("PDMP in Operation", 
                               "Average Age", 
                               "Percentage with Bachelor's Degree",
                               "Median Income", 
                               "Unemployment Rate", 
                               "Percentage that is White", 
                               "Personal Healthcare Spending per Capita"))
```

% Table created by stargazer v.5.2.3 by Marek Hlavac, Social Policy
Institute. E-mail: marek.hlavac at gmail.com % Date and time: Mon, Jun
26, 2023 - 11:35:38 PM
## METHADONE

``` r
df2 <- read_csv(here("Cleaned_Data", "DATA_METHADONE.csv"))

mod_methadone <- lm_robust(methadone_rate ~ PDMP + avg_age + bachelors_pct + median_income
                       + unemp_rate + white_pct + personal_health_care + 
                         factor(year) + factor(state), data = df2, se_type = "HC1")
```

``` r
mod2_methadone <- lm(methadone_rate ~ PDMP + avg_age + bachelors_pct + median_income
                       + unemp_rate + white_pct + personal_health_care + factor(year) + factor(state), data = df2)

cov2_methadone <- vcovHC(mod2_methadone, type = "HC1")
robust_se2_methadone <- sqrt(diag(cov2_methadone))


mod1_methadone <- lm(methadone_rate ~ PDMP + factor(year) + factor(state), data = df2)

cov1_methadone <- vcovHC(mod1_methadone, type = "HC1")
robust_se1_methadone <- sqrt(diag(cov1_methadone))
```

``` r
fe_vector <- c("Fixed Effects", "Yes", "Yes")
covariate_vector <- c("Covariates", "No", "Yes")


stargazer(mod1_methadone, mod2_methadone, type = "latex",
          se = list(robust_se1_methadone, robust_se2_methadone), omit = c("factor"),
          add.lines = list(fe_vector, covariate_vector),
          dep.var.caption = "Dependent Variable: Methadone Overdose Death Rate per 100,000",
          dep.var.labels.include = FALSE,
          covariate.labels = c("PDMP in Operation", 
                               "Average Age", 
                               "Percentage with Bachelor's Degree",
                               "Median Income", 
                               "Unemployment Rate", 
                               "Percentage that is White", 
                               "Personal Healthcare Spending per Capita"))
```

% Table created by stargazer v.5.2.3 by Marek Hlavac, Social Policy
Institute. E-mail: marek.hlavac at gmail.com % Date and time: Mon, Jun
26, 2023 - 11:35:38 PM
## ALL RESULTS

``` r
fe_vector_final <- c("Fixed Effects", "Yes", "Yes", "Yes", "Yes")
covariate_vector_final <- c("Covariates", "Yes", "Yes", "Yes", "Yes")


stargazer(mod2_total, mod2_heroin, mod2_prescription, mod2_methadone,
          type = "latex",
          se = list(robust_se2_total, robust_se2_heroin, 
                    robust_se2_prescription, robust_se2_methadone), 
          omit = c("factor"), 
          add.lines = list(fe_vector_final, covariate_vector_final), 
          dep.var.caption = 
            "Dependent Variable: Overdose Death Rate per 100,000 for Four Categories of Opioids:", 
          dep.var.labels.include = FALSE, 
          column.labels = c("All", "Heroin and Synthetic", "Prescription", "Methadone"),
          covariate.labels = c("PDMP in Operation", 
                               "Average Age", 
                               "Percentage with Bachelor's Degree",
                               "Median Income", 
                               "Unemployment Rate", 
                               "Percentage that is White", 
                               "Personal Healthcare Spending per Capita"))
```

% Table created by stargazer v.5.2.3 by Marek Hlavac, Social Policy
Institute. E-mail: marek.hlavac at gmail.com % Date and time: Mon, Jun
26, 2023 - 11:35:39 PM
