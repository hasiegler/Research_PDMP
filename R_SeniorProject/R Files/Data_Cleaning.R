library(here)
library(tidyverse)
library(ipumsr)
library(imputeTS)




## MEDIAN INCOMES
income_data <- readxl::read_xlsx(here("Uncleaned_Data", "MedianIncomesCleaned.xlsx"))
colnames(income_data) <- str_c(colnames(income_data), " ", income_data[1,])
income_data <- income_data[-1:-2,]
income_data <- income_data[, c(1, seq(2, 80, by = 2))]
col_words <- str_split(colnames(income_data), " ")
new_col_names <- c()
for (i in 1:41){
  new_col_names[i] <- col_words[[i]][1]
}
new_col_names[1] <- "State"
colnames(income_data) <- new_col_names
income_data <- pivot_longer(income_data, cols = -State, names_to = "Year", values_to = "Median Income")
income_data <- income_data %>% 
  rename(median_income = `Median Income`) %>% 
  mutate(Year = as.numeric(Year),
         median_income = as.numeric(median_income)) %>% 
  filter(Year >= 2000,
         Year != 2021)
income_data <- income_data %>% 
  distinct(State, Year, .keep_all = TRUE) %>% 
  mutate(median_income = median_income / 1000)
#write.csv(income_data, file = "INCOME.csv", row.names = FALSE)





## OVERDOSE RATES
## All Opioids
opioid_data <- read.delim(here("Uncleaned_Data", "All_Opioids.txt"))
opioid_data <- opioid_data[,c(2, 4, 6, 7)]
opioid_data$overdose_Rate <- opioid_data$Deaths / opioid_data$Population * 100000
opioid_data <- opioid_data %>% 
  select(-Deaths, -Population)

## Heroin and Synthetic Opioids T40.1 40.4
heroin_data <- read.delim(here("Uncleaned_Data", "Heroin_Synthetic.txt"))
heroin_data <- heroin_data[,c(2, 4, 6, 7)]
heroin_data$heroin_rate <- heroin_data$Deaths / heroin_data$Population * 100000
heroin_data <- heroin_data %>% 
  select(-Deaths, -Population)

## Prescription Opioids T40.2 40.6
prescription_data <- read.delim(here("Uncleaned_Data", "Prescription_Opioids.txt"))
prescription_data <- prescription_data[,c(2, 4, 6, 7)]
prescription_data$prescription_rate <- prescription_data$Deaths / prescription_data$Population * 100000
prescription_data <- prescription_data %>% 
  select(-Deaths, -Population)

##METHADONE T40.3
methadone_data <- read.delim(here("Uncleaned_Data", "Methadone.txt"))
methadone_data <- methadone_data[,c(2, 4, 6, 7)]
methadone_data$methadone_rate <- methadone_data$Deaths / methadone_data$Population * 100000
methadone_data <- methadone_data %>% 
  select(-Deaths, -Population)


## COMBINING OVERDOSE RATES
all_overdoses<- left_join(income_data, opioid_data, by = c("Year", "State"))
all_overdoses<- left_join(all_overdoses, heroin_data, by = c("Year", "State"))
all_overdoses<- left_join(all_overdoses, prescription_data, by = c("Year", "State"))
all_overdoses<- all_overdoses %>% 
  select(-median_income)

data_missing <- all_overdoses%>% 
  group_by(State) %>% 
  summarise(miss_total = sum(is.na(overdose_Rate)),
            miss_heroin = sum(is.na(heroin_rate)),
            miss_prescription = sum(is.na(prescription_rate)))
sum(data_missing$miss_total)
sum(data_missing$miss_heroin)
sum(data_missing$miss_prescription)

## INTERPOLATING 3 RATES
interpolated_data <- list()
for (state in unique(all_overdoses$State)) {
  # Filter the data for the current state
  state_data <- all_overdoses %>% 
    filter(State == state)
  # Perform na_interpolation for each column
  state_data$overdose_Rate <- na_interpolation(state_data$overdose_Rate, option = "linear")
  state_data$prescription_rate <- na_interpolation(state_data$prescription_rate, option = "linear")
  state_data$heroin_rate <- na_interpolation(state_data$heroin_rate, option = "linear")
  # Store the interpolated dataframe for the current state
  interpolated_data[[state]] <- state_data
}

# Combine all interpolated dataframes into a single dataframe
interpolated_df <- bind_rows(interpolated_data)
# Reset row names
row.names(interpolated_df) <- NULL
#write.csv(interpolated_df, file = "OVERDOSE_RATES.csv", row.names = FALSE)


## METHADONE
methadone <- left_join(income_data, methadone_data, by = c("Year", "State"))
methadone <- methadone %>% 
  filter(State != "North Dakota",
         State != "South Dakota",
         State != "Wyoming") %>% 
  select(-median_income)
interpolated_methadone <- list()
for (state in unique(methadone$State)) {
  # Filter the data for the current state
  state_data <- methadone %>% 
    filter(State == state)
  # Perform na_interpolation for each column
  state_data$methadone_rate <- na_interpolation(state_data$methadone_rate, option = "linear")
  # Store the interpolated dataframe for the current state
  interpolated_methadone[[state]] <- state_data
}
interpolated_methadone_df <- bind_rows(interpolated_methadone)
# Reset row names
row.names(interpolated_methadone_df) <- NULL
#write.csv(interpolated_methadone_df, file = "METHADONE.csv", row.names = FALSE)





## PDMP DATES
PDMP_Year <- readxl::read_xlsx(here("Uncleaned_Data", "PDMPYear.xlsx"))
PDMP_Year <- PDMP_Year %>% 
  mutate(Year = as.numeric(Year)) %>% 
  rename(operational_year = Year)
#write.csv(PDMP_Year, file = "PDMP.csv", row.names = FALSE)






## PERSONAL HEALTH CARE EXPENDITURES
health_data <- read.csv(here("Uncleaned_Data", "PersonalHealthCare.csv"))
# fix the column names
new_colnames_health <- c()
for (i in 1:length(colnames(health_data))){
  new_colnames_health[i] <- sub("X", "", colnames(health_data)[i])
}
colnames(health_data) <- new_colnames_health
health_data <- pivot_longer(health_data, cols = -State, names_to = "Year", values_to = "personal_health_care")
health_data <- health_data %>% 
  mutate(Year = as.numeric(Year),
         personal_health_care = str_remove_all(personal_health_care, ","),
         personal_health_care = as.numeric(personal_health_care),
         personal_health_care = personal_health_care/1000)
#write.csv(health_data, file = "HEALTH_CARE.csv", row.names = FALSE)


## PERCENTAGE OF POPULATION WHITE
library(ipumsr)
cps_ddi_white1 = read_ipums_ddi(
  'race1.xml'
)
white1 = read_ipums_micro(
  cps_ddi_white1,
  data_file = 'race1.dat.gz'
)
# Create a new variable to indicate whether the person is white
white1$WHITE <- ifelse(white1$RACED== 100, 1, 0)
# Calculate the total PERWT and PERWT for white individuals by state and year
data_white1 <- aggregate(cbind(TOTAL_PERWT = PERWT, WHITE_PERWT = PERWT * WHITE) ~ YEAR + STATEFIP, data = white1, sum)
# Calculate the percentage of white population by state and year
data_white1$WHITE_PCT <- data_white1$WHITE_PERWT / data_white1$TOTAL_PERWT * 100
data_white1 <- data_white1 %>% 
  select(YEAR, STATEFIP, WHITE_PCT) %>% 
  filter(YEAR >= 2011)

cps_ddi_white2 = read_ipums_ddi(
  'race2.xml'
)
white2 = read_ipums_micro(
  cps_ddi_white2,
  data_file = 'race2.dat.gz'
)
white2$WHITE <- ifelse(white2$RACED== 100, 1, 0)
data_white2 <- aggregate(cbind(TOTAL_PERWT = PERWT, WHITE_PERWT = PERWT * WHITE) ~ YEAR + STATEFIP, data = white2, sum)
data_white2$WHITE_PCT <- data_white2$WHITE_PERWT / data_white2$TOTAL_PERWT * 100
data_white2 <- data_white2 %>% 
  select(YEAR, STATEFIP, WHITE_PCT)

cps_ddi_white3 = read_ipums_ddi(
  'race3.xml'
)
white3 = read_ipums_micro(
  cps_ddi_white3,
  data_file = 'race3.dat.gz'
)
white3$WHITE <- ifelse(white3$RACED== 100, 1, 0)
data_white3 <- aggregate(cbind(TOTAL_PERWT = PERWT, WHITE_PERWT = PERWT * WHITE) ~ YEAR + STATEFIP, data = white3, sum)
data_white3$WHITE_PCT <- data_white3$WHITE_PERWT / data_white3$TOTAL_PERWT * 100
data_white3 <- data_white3 %>% 
  select(YEAR, STATEFIP, WHITE_PCT)

# Combining Dataframes for White
data_white <- bind_rows(data_white1, data_white2, data_white3)


data_state_names <- data.frame(STATEFIP = c(1,2,4,5,6,8,9,10,11,12,13,15,16,17,
                                            18,19,20,21,22,23,24,25,26,27,28,29,
                                            30,31,32,33,34,35,36,37,38,39,40,41,42
                                            ,44,45,46,47,48,49,50,51,53,54,55,56),
                               State = c("Alabama",
                                         "Alaska",
                                         "Arizona",
                                         "Arkansas",
                                         "California",
                                         "Colorado",
                                         "Connecticut",
                                         "Delaware",
                                         "District of Columbia",
                                         "Florida",
                                         "Georgia",
                                         "Hawaii",
                                         "Idaho",
                                         "Illinois",
                                         "Indiana",
                                         "Iowa",
                                         "Kansas",
                                         "Kentucky",
                                         "Louisiana",
                                         "Maine",
                                         "Maryland",
                                         "Massachusetts",
                                         "Michigan",
                                         "Minnesota",
                                         "Mississippi",
                                         "Missouri",
                                         "Montana",
                                         "Nebraska",
                                         "Nevada",
                                         "New Hampshire",
                                         "New Jersey",
                                         "New Mexico",
                                         "New York",
                                         "North Carolina",
                                         "North Dakota",
                                         "Ohio",
                                         "Oklahoma",
                                         "Oregon",
                                         "Pennsylvania",
                                         "Rhode Island",
                                         "South Carolina",
                                         "South Dakota",
                                         "Tennessee",
                                         "Texas",
                                         "Utah",
                                         "Vermont",
                                         "Virginia",
                                         "Washington",
                                         "West Virginia",
                                         "Wisconsin",
                                         "Wyoming"))

data_white <- data_white %>% 
  left_join(data_state_names, by = "STATEFIP")

data_white <- data_white %>% 
  select(YEAR, State, WHITE_PCT) %>% 
  rename(Year = "YEAR")

#write.csv(data_white, "WHITE.csv", row.names = FALSE)
  



## BACHELOR'S DEGREE PERCENTAGE OF POPULATION
cps_ddi_educ1 = read_ipums_ddi(
  'educ1.xml'
)
educ1 = read_ipums_micro(
  cps_ddi_educ1,
  data_file = 'educ1.dat.gz'
)
educ1$bachelor_or_higher <- ifelse(educ1$EDUCD >= 101, 1, 0)
data_educ1 <- aggregate(cbind(TOTAL_PERWT = PERWT, BACHELORS_PERWT = PERWT * bachelor_or_higher) ~ YEAR + STATEFIP, data = educ1, sum)
data_educ1$bachelors_percent <- data_educ1$BACHELORS_PERWT / data_educ1$TOTAL_PERWT * 100
data_educ1 <- data_educ1 %>% 
  select(YEAR, STATEFIP, bachelors_percent)

cps_ddi_educ2 = read_ipums_ddi(
  'educ2.xml'
)
educ2 = read_ipums_micro(
  cps_ddi_educ1,
  data_file = 'educ2.dat.gz'
)
educ2$bachelor_or_higher <- ifelse(educ2$EDUCD >= 101, 1, 0)
data_educ2 <- aggregate(cbind(TOTAL_PERWT = PERWT, BACHELORS_PERWT = PERWT * bachelor_or_higher) ~ YEAR + STATEFIP, data = educ2, sum)
data_educ2$bachelors_percent <- data_educ2$BACHELORS_PERWT / data_educ2$TOTAL_PERWT * 100
data_educ2 <- data_educ2 %>% 
  select(YEAR, STATEFIP, bachelors_percent)

# Combining Dataframes for Bachelor's
data_educ <- bind_rows(data_educ1, data_educ2)
data_educ <- data_educ %>% 
  left_join(data_state_names, by = "STATEFIP")
data_educ <- data_educ %>% 
  select(YEAR, State, bachelors_percent) %>% 
  rename(Year = "YEAR")
#write.csv(data_educ, "BACH_PCT.csv", row.names = FALSE)




# AVERAGE AGE
cps_ddi_age1 = read_ipums_ddi(
  'age1.xml'
)
age1 = read_ipums_micro(
  cps_ddi_age1,
  data_file = 'age1.dat.gz'
)
data_age1 <- aggregate(cbind(AVG_AGE = AGE * PERWT) ~ YEAR + STATEFIP, data = age1, sum)
data_age1$TOTAL_PERWT <- aggregate(age1$PERWT, by = list(YEAR = age1$YEAR, STATEFIP = age1$STATEFIP), sum)$x
data_age1$AVG_AGE <- data_age1$AVG_AGE / data_age1$TOTAL_PERWT

cps_ddi_age2 = read_ipums_ddi(
  'age2.xml'
)
age2 = read_ipums_micro(
  cps_ddi_age2,
  data_file = 'age2.dat.gz'
)
data_age2 <- aggregate(cbind(AVG_AGE = AGE * PERWT) ~ YEAR + STATEFIP, data = age2, sum)
data_age2$TOTAL_PERWT <- aggregate(age2$PERWT, by = list(YEAR = age2$YEAR, STATEFIP = age2$STATEFIP), sum)$x
data_age2$AVG_AGE <- data_age2$AVG_AGE / data_age2$TOTAL_PERWT

cps_ddi_age3 = read_ipums_ddi(
  'age3.xml'
)
age3 = read_ipums_micro(
  cps_ddi_age3,
  data_file = 'age3.dat.gz'
)
data_age3 <- aggregate(cbind(AVG_AGE = AGE * PERWT) ~ YEAR + STATEFIP, data = age3, sum)
data_age3$TOTAL_PERWT <- aggregate(age3$PERWT, by = list(YEAR = age3$YEAR, STATEFIP = age3$STATEFIP), sum)$x
data_age3$AVG_AGE <- data_age3$AVG_AGE / data_age3$TOTAL_PERWT

# Combining Dataframes for Age
data_age <- bind_rows(data_age1, data_age2, data_age3)
data_age <- data_age %>% 
  left_join(data_state_names, by = "STATEFIP")
data_age <- data_age %>% 
  select(YEAR, State, AVG_AGE) %>% 
  rename(Year = "YEAR")
write.csv(data_age, "AVG_AGE.csv", row.names = FALSE)






# Unemployment Rate
cps_ddi_emp1 = read_ipums_ddi(
  'emp1.xml'
)
emp1 = read_ipums_micro(
  cps_ddi_emp1,
  data_file = 'emp1.dat.gz'
)
emp1 <- emp1 %>% 
  filter(LABFORCE == 2)
emp1$UNEMP <- ifelse(emp1$EMPSTAT == 2, 1, 0)
unemp1 <- emp1 %>% 
  group_by(YEAR, STATEFIP) %>% 
  summarise(weighted_unemp = sum(UNEMP * PERWT))
labforce1 <- emp1 %>% 
  group_by(YEAR, STATEFIP) %>% 
  summarise(weighted_labor = sum(PERWT))
data_unemp1 <- left_join(unemp1, labforce1, by = c("YEAR", "STATEFIP")) %>% 
  mutate(unemp_rate = weighted_unemp / weighted_labor * 100)

cps_ddi_emp2 = read_ipums_ddi(
  'emp2.xml'
)
emp2 = read_ipums_micro(
  cps_ddi_emp2,
  data_file = 'emp2.dat.gz'
)
emp2 <- emp2 %>% 
  filter(LABFORCE == 2)
emp2$UNEMP <- ifelse(emp2$EMPSTAT == 2, 1, 0)
unemp2 <- emp2 %>% 
  group_by(YEAR, STATEFIP) %>% 
  summarise(weighted_unemp = sum(UNEMP * PERWT))
labforce2 <- emp2 %>% 
  group_by(YEAR, STATEFIP) %>% 
  summarise(weighted_labor = sum(PERWT))
data_unemp2 <- left_join(unemp2, labforce2, by = c("YEAR", "STATEFIP")) %>% 
  mutate(unemp_rate = weighted_unemp / weighted_labor * 100)

cps_ddi_emp3 = read_ipums_ddi(
  'emp3.xml'
)
emp3 = read_ipums_micro(
  cps_ddi_emp3,
  data_file = 'emp3.dat.gz'
)
emp3 <- emp3 %>% 
  filter(LABFORCE == 2)
emp3$UNEMP <- ifelse(emp3$EMPSTAT == 2, 1, 0)
unemp3 <- emp3 %>% 
  group_by(YEAR, STATEFIP) %>% 
  summarise(weighted_unemp = sum(UNEMP * PERWT))
labforce3 <- emp3 %>% 
  group_by(YEAR, STATEFIP) %>% 
  summarise(weighted_labor = sum(PERWT))
data_unemp3 <- left_join(unemp3, labforce3, by = c("YEAR", "STATEFIP")) %>% 
  mutate(unemp_rate = weighted_unemp / weighted_labor * 100)

#Combining Dataframes for Unemployment
data_unemp <- bind_rows(data_unemp1, data_unemp2, data_unemp3)
data_unemp <- data_unemp %>% 
  left_join(data_state_names, by = "STATEFIP")
data_unemp <- data_unemp %>% 
  select(YEAR, State, unemp_rate) %>% 
  rename(Year = "YEAR")
#write.csv(data_unemp, "UNEMP_RATE.csv", row.names = FALSE)