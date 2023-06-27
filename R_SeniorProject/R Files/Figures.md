Creating Figures
================
Henry Siegler

``` r
library(tidyverse)
library(here)
library(kableExtra)
library(knitr)
library(extrafont)
```

``` r
df <- read_csv(here("Cleaned_Data", "DATA.csv"))
df2 <- read_csv(here("Cleaned_Data", "DATA_METHADONE.csv"))
```

# Figure 1

``` r
PDMP_Year <- read_csv(here("Cleaned_Data", "PDMP.csv"))
PDMP_Year <- PDMP_Year %>% 
  mutate(operational_year = as.numeric(operational_year))

cumulative_PDMPS <- PDMP_Year %>% 
  group_by(operational_year) %>% 
  summarise(count = n_distinct(State)) %>% 
  mutate(cumulative_count = cumsum(count)) %>% 
  filter(operational_year >= 2000)

new_rows <- data.frame(operational_year = c(2000,
                                            2001,
                                            2002,
                                            2015,
                                            2016,
                                            2017,
                                            2018,
                                            2019,
                                            2020),
                       count = 0,
                       cumulative_count = c(16, 
                                            16,
                                            16,
                                            50,
                                            50,
                                            50,
                                            50,
                                            50,
                                            50))

cumulative_PDMPS <- rbind(cumulative_PDMPS, new_rows)
```

``` r
figure1 <- cumulative_PDMPS %>% 
  ggplot(aes(x = operational_year, y = cumulative_count)) + 
  geom_line(size = 1, col = "blue") + 
  theme_minimal() + 
  labs(y = "Total Number of States",
       x = "Year",
       title = "Total Number of States with an\nOperational PDMP by Year",
       caption = "District of Columbia included as a U.S. State")+
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        panel.grid.minor.x = element_blank(),
        text = element_text(family = "Times New Roman", size = 12))

figure1
```

![](Figures_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
#ggsave(filename = here("figure1.jpeg"), plot = figure1, device = "jpeg", width = 7, height = 5)
```

# Figure 2

``` r
average_rate <- df %>%
  group_by(PDMP, year) %>%
  summarize(Average_Rate = mean(total),
            count = sum(PDMP ==1))

figure2 <- ggplot(average_rate, aes(x = year, y = Average_Rate, color = factor(PDMP))) + 
  geom_line(size = 1) + 
  labs(x = "Year", y = "Overdose Rate per 100,000",
       title = "Average Total Opioid Overdose Rate by PDMP Status") + 
  scale_color_manual(values = c("blue", "red")) +
  theme_minimal() + 
  theme(panel.grid.minor.x = element_blank(),
        plot.title = element_text(hjust = 0.4, size = 16),
        legend.position = "none",
        text = element_text(family = "Times New Roman", size = 12)) + 
  scale_y_continuous(limits = c(0, 40))

figure2
```

![](Figures_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
#ggsave(filename = here("figure2.jpeg"), plot = figure2, device = "jpeg", width = 7, height = 5)
```

Line labels were added afterwards in Microsoft Word.

# Figure 3

``` r
median1 <- df %>% 
  group_by(year) %>% 
  summarize(median_total = median(total),
            median_heroin = median(heroin_synthetic),
            median_prescription = median(prescription))

median2 <- df2 %>% 
  group_by(year) %>% 
  summarize(median_methadone = median(methadone_rate))

median_data <- left_join(median1, median2, by = "year")

median_long <- median_data %>% 
  pivot_longer(!year, names_to = "Rate", values_to = "Value")

captions <- median_long %>% 
  group_by(Rate) %>% 
  slice(n()) %>% 
  mutate(Captions = c(""))
```

``` r
figure3 <- ggplot(median_long, aes(x = year, y = Value, color = Rate)) +
  geom_line(size = 1)+
  labs(x = "Year", y = "Overdose Rate per 100,000",
       title = "Median Overdose Rate by Opioid Category") + 
  theme_minimal() + 
  theme(legend.position = "none",
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(hjust = 0.4, size = 16),
        text = element_text(family = "Times New Roman", size = 12)) + 
  scale_color_manual(values = c("darkorchid1", "firebrick1", "dodgerblue", "green3")) + 
  scale_y_continuous(limits = c(0,25))

figure3
```

![](Figures_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
#ggsave(filename = here("figure3.jpeg"), plot = figure3, device = "jpeg", width = 7, height = 5)
```

Line labels were added afterwards in Microsoft Word.

# Figure 4

``` r
quantile_overall <- quantile(df$total, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))

quantile_2000 <- df %>% 
  filter(year == 2000) %>% 
  pull(total) %>% 
  quantile(probs = c(0.05, 0.25, 0.5, 0.75, 0.95))

quantile_2005 <- df %>% 
  filter(year == 2005) %>% 
  pull(total) %>% 
  quantile(probs = c(0.05, 0.25, 0.5, 0.75, 0.95))

quantile_2010 <- df %>% 
  filter(year == 2010) %>% 
  pull(total) %>% 
  quantile(probs = c(0.05, 0.25, 0.5, 0.75, 0.95))

quantile_2015 <- df %>% 
  filter(year == 2015) %>% 
  pull(total) %>% 
  quantile(probs = c(0.05, 0.25, 0.5, 0.75, 0.95))

quantile_2020 <- df %>% 
  filter(year == 2020) %>% 
  pull(total) %>% 
  quantile(probs = c(0.05, 0.25, 0.5, 0.75, 0.95))


quantiles_df <- data.frame(quantile_overall,
                           quantile_2000,
                           quantile_2005,
                           quantile_2010,
                           quantile_2015,
                           quantile_2020)

quantiles_df <- t(quantiles_df)

rownames(quantiles_df) <- c("2000 - 2020",
                            "2000",
                            "2005",
                            "2010",
                            "2015",
                            "2020")

quantiles_df <- round(quantiles_df, 2)

quantiles_df
```

    ##               5%   25%   50%   75%   95%
    ## 2000 - 2020 2.16  4.58  7.25 11.44 26.81
    ## 2000        0.76  1.89  2.69  4.45  8.45
    ## 2005        2.05  3.62  5.13  7.86 11.38
    ## 2010        3.44  5.10  6.85  9.70 14.16
    ## 2015        4.64  6.32 10.83 15.17 24.09
    ## 2020        6.48 12.25 22.31 29.04 41.97

These numbers were moved to Excel to create an APA styled table.
