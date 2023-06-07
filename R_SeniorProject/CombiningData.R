library(tidyverse)

## COVARIATES

AVG_AGE <- read_csv(here("CleanedFiles", "AVG_AGE.csv"))

BACH_PCT <- read_csv(here("CleanedFiles", "BACH_PCT.csv"))

INCOME <- read_csv(here("CleanedFiles", "INCOME.csv"))

PDMP <- read_csv(here("CleanedFiles", "PDMP.csv"))

UNEMP_RATE <- read_csv(here("CleanedFiles", "UNEMP_RATE.csv"))

WHITE <- read_csv(here("CleanedFiles", "WHITE.csv"))

OVERDOSE3 <- read_csv(here("CleanedFiles", "OVERDOSE3.csv"))

METHADONE <- read_csv(here("CleanedFiles", "METHADONE.csv"))

HEALTH_CARE <- read_csv(here("CleanedFiles", "HEALTH_CARE.csv"))


## DATA 3

DATA3 <- OVERDOSE3 %>% 
  left_join(AVG_AGE, by = c("Year", "State"))

DATA3 <- DATA3 %>% 
  left_join(BACH_PCT, by = c("Year", "State")) 

DATA3 <- DATA3 %>% 
  left_join(INCOME, by = c("Year", "State"))

DATA3 <- DATA3 %>% 
  left_join(PDMP, by = "State")

DATA3$PDMP <- ifelse(DATA3$Year >= DATA3$operational_year, 1, 0)

DATA3 <- DATA3 %>% 
  left_join(UNEMP_RATE, by = c("Year", "State"))

DATA3 <- DATA3 %>% 
  left_join(WHITE, by = c("Year", "State"))

DATA3 <- DATA3 %>% 
  filter(Year > 1999)

DATA3 <- DATA3 %>% 
  select(-operational_year)

DATA3[is.na(DATA3)] <- 0

DATA3 <- DATA3 %>% 
  left_join(HEALTH_CARE, by = c("Year", "State"))

DATA3 <- DATA3 %>% 
  rename(total = Overdose_Rate,
         heroin_synthetic = heroin_rate,
         natural_and_other = other_rate,
         avg_age = AVG_AGE,
         bachelors_pct = bachelors_percent,
         white_pct = WHITE_PCT,
         state = State,
         year = Year)

write.csv(DATA3, "DATA3.csv", row.names = FALSE)


## METHADONE

DATA_METHADONE <- METHADONE %>% 
  left_join(AVG_AGE, by = c("Year", "State"))

DATA_METHADONE <- DATA_METHADONE %>% 
  left_join(BACH_PCT, by = c("Year", "State")) 

DATA_METHADONE <- DATA_METHADONE %>% 
  left_join(INCOME, by = c("Year", "State"))

DATA_METHADONE <- DATA_METHADONE %>% 
  left_join(PDMP, by = "State")

DATA_METHADONE$PDMP <- ifelse(DATA_METHADONE$Year >= DATA_METHADONE$operational_year, 1, 0)

DATA_METHADONE <- DATA_METHADONE %>% 
  left_join(UNEMP_RATE, by = c("Year", "State"))

DATA_METHADONE <- DATA_METHADONE %>% 
  left_join(WHITE, by = c("Year", "State"))

DATA_METHADONE <- DATA_METHADONE %>% 
  filter(Year > 1999)

DATA_METHADONE <- DATA_METHADONE %>% 
  select(-operational_year)

DATA_METHADONE[is.na(DATA_METHADONE)] <- 0

DATA_METHADONE <- DATA_METHADONE %>% 
  left_join(HEALTH_CARE, by = c("Year", "State"))

DATA_METHADONE <- DATA_METHADONE %>% 
  rename(avg_age = AVG_AGE,
         bachelors_pct = bachelors_percent,
         white_pct = WHITE_PCT,
         state = State,
         year = Year)

write.csv(DATA_METHADONE, "DATA_METHADONE.csv", row.names = FALSE)






