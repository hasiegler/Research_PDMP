library(tidyverse)

## READ IN ALL DATA

OVERDOSE_RATES <- read_csv(here("Cleaned_Data", "OVERDOSE_RATES.csv"))

METHADONE <- read_csv(here("Cleaned_Data", "METHADONE.csv"))

AVG_AGE <- read_csv(here("Cleaned_Data", "AVG_AGE.csv")) 

BACH_PCT <- read_csv(here("Cleaned_Data", "BACH_PCT.csv"))

INCOME <- read_csv(here("Cleaned_Data", "INCOME.csv"))

PDMP <- read_csv(here("Cleaned_Data", "PDMP.csv"))

UNEMP_RATE <- read_csv(here("Cleaned_Data", "UNEMP_RATE.csv"))

WHITE <- read_csv(here("Cleaned_Data", "WHITE.csv"))

HEALTH_CARE <- read_csv(here("Cleaned_Data", "HEALTH_CARE.csv"))


## OVERDOSE RATES

DATA <- OVERDOSE_RATES %>% 
  left_join(AVG_AGE, by = c("Year", "State"))

DATA <- DATA %>% 
  left_join(BACH_PCT, by = c("Year", "State")) 

DATA <- DATA %>% 
  left_join(INCOME, by = c("Year", "State"))

DATA <- DATA %>% 
  left_join(PDMP, by = "State")

DATA$PDMP <- ifelse(DATA$Year >= DATA$operational_year, 1, 0)

DATA <- DATA %>% 
  left_join(UNEMP_RATE, by = c("Year", "State"))

DATA <- DATA %>% 
  left_join(WHITE, by = c("Year", "State"))

DATA <- DATA %>% 
  filter(Year > 1999)

DATA <- DATA %>% 
  select(-operational_year)

DATA[is.na(DATA)] <- 0

DATA <- DATA %>% 
  left_join(HEALTH_CARE, by = c("Year", "State"))

DATA <- DATA %>% 
  rename(total = overdose_Rate,
         heroin_synthetic = heroin_rate,
         prescription = prescription_rate,
         avg_age = AVG_AGE,
         bachelors_pct = bachelors_percent,
         white_pct = WHITE_PCT,
         state = State,
         year = Year)
#write.csv(DATA, "DATA.csv", row.names = FALSE)


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
#write.csv(DATA_METHADONE, "DATA_METHADONE.csv", row.names = FALSE)
