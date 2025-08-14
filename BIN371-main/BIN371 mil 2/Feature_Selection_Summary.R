setwd("C:/Users/578272/OneDrive - belgiumcampus.ac.za/BIN371_R_Programming/BIN371/BIN371/")

library(knitr)
library(dplyr)
library(rmarkdown)
library(tidyr)
library(readr)

# Load datasets
immunization <- read.csv("immunization_national_zaf.csv", header=TRUE, sep=",")
maternal_mortality <- read.csv("maternal-mortality_national_zaf.csv", header=TRUE, sep=",")
access_healthcare <- read.csv("access-to-health-care_national_zaf.csv", header=TRUE, sep=",")
child_mortality <- read.csv("child-mortality-rates_national_zaf.csv", header=TRUE, sep=",")

# Function to find % missing values
missing_percentage <- function(df){
  data.frame(
    Feature = colnames(df),
    MissingPercent = colSums(is.na(df)) / nrow(df) * 100  
  )
}

# Missing value summaries
missing_access <- missing_percentage(access_healthcare) %>% mutate(Dataset = "Access_Healthcare")
missing_maternal <- missing_percentage(maternal_mortality) %>% mutate(Dataset = "Maternal_Mortality")
missing_immunization <- missing_percentage(immunization) %>% mutate(Dataset = "Immunization")
missing_child <- missing_percentage(child_mortality) %>% mutate(Dataset = "Child_Mortality")

missing_summary <- bind_rows(missing_access, missing_maternal, missing_immunization, missing_child)

# Target variable
target <- "Value"
child_mortality$Value <- as.numeric(as.character(child_mortality$Value))
if (any(is.na(child_mortality$Value))) {
  warning(sum(is.na(child_mortality$Value)), " NAs introduced when converting 'Value' to numeric.")
}

# Select numeric columns
numeric_data <- child_mortality %>% select(where(is.numeric))

# Correlation with target
cor_results <- data.frame(
  Feature = colnames(numeric_data),
  CorrelationWithTarget = sapply(numeric_data, function(x){
    if(all(is.na(x)) || sd(x, na.rm = TRUE) == 0) return(NA)
    cor(x, numeric_data[[target]], use = "complete.obs")
  })
)

# Merge missing & correlation data
feature_summary <- left_join(missing_summary, cor_results, by="Feature")

# Keep/Remove decision
feature_summary <- feature_summary %>% mutate(
  Keep = ifelse(MissingPercent > 80 | abs(CorrelationWithTarget) < 0.1, "No", "Yes"),
  Reason = case_when(
    MissingPercent > 80 ~ "Too many missing values",
    abs(CorrelationWithTarget) < 0.1 ~ "Weak correlation with target",
    TRUE ~ "Relevant"
  )
)

# Save results
write.csv(feature_summary, "Feature_Selection_Summary.csv", row.names = FALSE)
print(feature_summary)

# For now, assuming you already have one merged dataset:
merged_data <- healthcare_data # replace with your merged dataframe name

# List of targets
targets <- c("child_mortality_rate", "maternal_mortality_rate")

# Store all results
all_results <- list()

for (target in targets) {
  
  numeric_data <- merged_data %>% select(where(is.numeric))
  
  cor_results <- data.frame(
    Feature = colnames(numeric_data),
    CorrelationWithTarget = sapply(numeric_data, function(x) {
      if (all(is.na(x))) return(NA)
      cor(x, numeric_data[[target]], use = "complete.obs")
    }),
    stringsAsFactors = FALSE
  )
  
  # Remove the target from feature list
  cor_results <- cor_results %>% filter(Feature != target)
  
  # Rank: strongest absolute correlations first
  cor_results <- cor_results %>%
    arrange(desc(abs(CorrelationWithTarget)))
  
  all_results[[target]] <- cor_results
}

# Example: Access results for child mortality
child_results <- all_results[["child_mortality_rate"]]

# Example: Access results for maternal mortality
maternal_results <- all_results[["maternal_mortality_rate"]]

# Save or view top factors
head(child_results, 10)
head(maternal_results, 10)
