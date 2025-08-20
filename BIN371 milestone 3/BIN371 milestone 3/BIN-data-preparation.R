# Load required library
library(tidyverse)
library(randomForest)
library(ggplot2)
library(tidyr)
library(dplyr)

setwd("C:/Users/Lethabo/Documents/GitHub/BIN371/BIN371 milestone 3/BIN371 milestone 3")

access_healthcare <- read.csv("access-to-health-care_national_zaf.csv")
maternal_mortality <- read.csv( "maternal-mortality_national_zaf.csv")
immunization <- read.csv( "immunization_national_zaf.csv")
child_mortality <- read.csv ("child-mortality-rates_national_zaf.csv")

access_healthcare <- access_healthcare[-1, ]
child_mortality <- child_mortality[-1, ]
immunization <- immunization[-1, ]
maternal_mortality <- maternal_mortality[-1, ]

# -----------------------------
# 2. Convert relevant columns to numeric & remove missing values
# -----------------------------
convert_numeric <- function(df) {
  df %>%
    mutate(
      Value = as.numeric(as.character(Value)),
      SurveyYear = as.numeric(as.character(SurveyYear))
    ) %>%
    filter(!is.na(Value))
}

access_healthcare   <- convert_numeric(access_healthcare)
maternal_mortality  <- convert_numeric(maternal_mortality)
immunization        <- convert_numeric(immunization)
child_mortality     <- convert_numeric(child_mortality)

# -----------------------------
# 3. Aggregate duplicates
# -----------------------------
aggregate_dataset <- function(df) {
  df %>%
    group_by(Indicator, SurveyYear) %>%
    summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop")
}

access_healthcare_clean  <- aggregate_dataset(access_healthcare)
maternal_mortality_clean <- aggregate_dataset(maternal_mortality)
immunization_clean       <- aggregate_dataset(immunization)
child_mortality_clean    <- aggregate_dataset(child_mortality)

# -----------------------------
# 4. Summary statistics
# -----------------------------
summary_stats <- function(df, name) {
  cat("\n---", name, "Summary ---\n")
  df %>%
    group_by(SurveyYear) %>%
    summarise(
      Min    = min(Value, na.rm = TRUE),
      Max    = max(Value, na.rm = TRUE),
      Mean   = mean(Value, na.rm = TRUE),
      Median = median(Value, na.rm = TRUE)
    ) %>%
    print()
}

summary_stats(access_healthcare_clean, "Access to Health Care")
summary_stats(child_mortality_clean, "Child Mortality")
summary_stats(immunization_clean, "Immunization")
summary_stats(maternal_mortality_clean, "Maternal Mortality")

# -----------------------------
# 5. Compute year-to-year change
# -----------------------------
compute_change <- function(df, name) {
  df_wide <- df %>%
    pivot_wider(names_from = SurveyYear, values_from = Value, names_prefix = "Year_") %>%
    filter(!is.na(Year_1998) & !is.na(Year_2016)) %>%
    mutate(
      Absolute_Change = Year_2016 - Year_1998,
      Percent_Change  = 100 * (Year_2016 - Year_1998) / Year_1998
    )
  
  cat("\n---", name, "Change 1998 -> 2016 ---\n")
  print(df_wide)
  return(df_wide)
}

access_healthcare_change  <- compute_change(access_healthcare_clean, "Access to Health Care")
child_mortality_change     <- compute_change(child_mortality_clean, "Child Mortality")
immunization_change        <- compute_change(immunization_clean, "Immunization")
maternal_mortality_change  <- compute_change(maternal_mortality_clean, "Maternal Mortality")

# -----------------------------
# 6. Visualizations
# -----------------------------
plot_indicator <- function(df, title) {
  ggplot(df, aes(x = factor(SurveyYear), y = Value, fill = factor(SurveyYear))) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = title, x = "Survey Year", y = "Value", fill = "Year") +
    theme_minimal()
}

child_mortality_filtered <- child_mortality_clean %>%
  filter(Indicator == "Under-five mortality rate")

plot_indicator(access_healthcare_clean, "Access to Health Care in South Africa")
plot_indicator(child_mortality_filtered, "Under-five Child Mortality")
plot_indicator(immunization_clean, "Immunization Coverage in South Africa")
plot_indicator(maternal_mortality_clean, "Maternal Mortality Rates")

# Line plot for child mortality trends
ggplot(child_mortality_filtered, aes(x = SurveyYear, y = Value, group = Indicator, color = Indicator)) +
  geom_line() + geom_point(size = 3) +
  labs(title = "Under-five Child Mortality Trends", x = "Year", y = "Mortality Rate") +
  theme_minimal()

# -----------------------------
# 7. Combined summary table
# -----------------------------
combined_summary <- bind_rows(
  access_healthcare_clean  %>% mutate(Dataset = "Access to Health Care"),
  child_mortality_filtered %>% mutate(Dataset = "Child Mortality"),
  immunization_clean       %>% mutate(Dataset = "Immunization"),
  maternal_mortality_clean %>% mutate(Dataset = "Maternal Mortality")
)

cat("\n--- Combined Dataset Summary ---\n")
print(combined_summary)

# -----------------------------
# 8. Highlight top child mortality changes
# -----------------------------
child_mortality_change %>%
  arrange(desc(abs(Percent_Change))) %>%
  head(5)