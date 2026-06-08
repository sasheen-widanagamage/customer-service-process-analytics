# Install and Load Required Packages

required_packages <- c(
  "tidyverse",
  "janitor",
  "skimr",
  "naniar",
  "caret",
  "pROC",
  "car",
  "broom",
  "ggcorrplot",
  "scales"
)

installed_packages <- rownames(installed.packages())

for (pkg in required_packages) {
  if (!(pkg %in% installed_packages)) {
    install.packages(pkg)
  }
}

library(tidyverse)
library(janitor)
library(skimr)
library(naniar)
library(caret)
library(pROC)
library(car)
library(broom)
library(ggcorrplot)
library(scales)


# Import Dataset

file_path <- "C:/Users/wsash/OneDrive/Desktop/Customer Service Analytics Project/data/Airline_customer_satisfaction.csv"
data_raw <- read.csv(file_path, stringsAsFactors = FALSE)

cat("Number of rows:", nrow(data_raw), "\n")
cat("Number of columns:", ncol(data_raw), "\n")
head(data_raw)

# Clean Column Names
data <- data_raw %>%
  clean_names()
print(names(data))

# Initial Dataset Inspection
cat("\nDataset Dimensions:\n")
print(dim(data))

cat("\nDataset Structure:\n")
str(data)

cat("\nDataset Summary:\n")
print(summary(data))

cat("\nSkim Summary:\n")
print(skim(data))


# Check Missing Values

missing_summary <- data.frame(
  variable = names(data),
  missing_count = colSums(is.na(data)),
  missing_percentage = round(colSums(is.na(data)) / nrow(data) * 100, 2)
)

print(missing_summary)

write.csv(
  missing_summary,
  "outputs/tables/missing_values_before_cleaning.csv",
  row.names = FALSE
)

sum(data$arrival_delay_in_minutes == 0, na.rm = TRUE)

# arrival_delay_in_minutes

if ("arrival_delay_in_minutes" %in% names(data)) {
  data <- data %>%
    mutate(
      arrival_delay_in_minutes = ifelse(
        is.na(arrival_delay_in_minutes),
        median(arrival_delay_in_minutes, na.rm = TRUE),
        arrival_delay_in_minutes
      )
    )
}

# Replace any remaining numeric missing values using median

data <- data %>%
  mutate(
    across(
      where(is.numeric),
      ~ ifelse(is.na(.), median(., na.rm = TRUE), .)
    )
  )

# Replace categorical missing values with "Unknown"

data <- data %>%
  mutate(
    across(
      where(is.character),
      ~ ifelse(is.na(.), "Unknown", .)
    )
  )

# Remove Duplicate Records
duplicate_count <- sum(duplicated(data))

cat("Number of duplicate rows found:", duplicate_count, "\n")

data <- data %>%
  distinct()

# Remove Unnecessary ID Columns If Available
data <- data %>%
  select(-any_of(c("x", "unnamed_0", "id")))

# Convert Categorical Variables into Factors
categorical_columns <- c(
  "gender",
  "customer_type",
  "type_of_travel",
  "class",
  "satisfaction"
)

for (col in categorical_columns) {
  if (col %in% names(data)) {
    data[[col]] <- as.factor(data[[col]])
  }
}

# Check Satisfaction Values
cat("\nSatisfaction categories:\n")
print(table(data$satisfaction))

# Create Binary Satisfaction Target Variable
# satisfied = 1
# dissatisfied or other categories = 0

data <- data %>%
  mutate(
    satisfaction_binary = ifelse(
      tolower(as.character(satisfaction)) == "satisfied",
      1,
      0
    )
  )

print(table(data$satisfaction_binary))


# Create Smooth Service Process Score
data <- data %>%
  mutate(
    smooth_process_score = rowMeans(
      select(
        .,
        ease_of_online_booking,
        online_boarding,
        checkin_service,
        gate_location,
        baggage_handling,
        departure_arrival_time_convenient
      ),
      na.rm = TRUE
    )
  )


# Create Overall Experience Score
data <- data %>%
  mutate(
    overall_experience_score = rowMeans(
      select(
        .,
        seat_comfort,
        food_and_drink,
        inflight_wifi_service,
        inflight_entertainment,
        online_support,
        on_board_service,
        leg_room_service,
        cleanliness
      ),
      na.rm = TRUE
    )
  )

# Create Smooth Process Groups

data <- data %>%
  mutate(
    smooth_process_group = case_when(
      smooth_process_score < 2.5 ~ "Low Smoothness",
      smooth_process_score >= 2.5 & smooth_process_score < 3.5 ~ "Moderate Smoothness",
      smooth_process_score >= 3.5 ~ "High Smoothness",
      TRUE ~ "Unknown"
    ),
    smooth_process_group = factor(
      smooth_process_group,
      levels = c("Low Smoothness", "Moderate Smoothness", "High Smoothness", "Unknown")
    )
  )

print(table(data$smooth_process_group))


# Create Arrival Delay Group
data <- data %>%
  mutate(
    arrival_delay_group = case_when(
      arrival_delay_in_minutes == 0 ~ "No Delay",
      arrival_delay_in_minutes > 0 & arrival_delay_in_minutes <= 15 ~ "Minor Delay",
      arrival_delay_in_minutes > 15 & arrival_delay_in_minutes <= 60 ~ "Moderate Delay",
      arrival_delay_in_minutes > 60 ~ "Major Delay",
      TRUE ~ "Unknown"
    ),
    arrival_delay_group = factor(
      arrival_delay_group,
      levels = c("No Delay", "Minor Delay", "Moderate Delay", "Major Delay", "Unknown")
    )
  )

print(table(data$arrival_delay_group))

# Outlier Detection
detect_outliers <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lower <- q1 - 1.5 * iqr
  upper <- q3 + 1.5 * iqr
  return(x < lower | x > upper)
}

data %>%
  summarise(
    age_outliers = sum(detect_outliers(age), na.rm = TRUE),
    flight_distance_outliers = sum(detect_outliers(flight_distance), na.rm = TRUE),
    departure_delay_outliers = sum(detect_outliers(departure_delay_in_minutes), na.rm = TRUE),
    arrival_delay_outliers = sum(detect_outliers(arrival_delay_in_minutes), na.rm = TRUE)
  )


# Cap Extreme Delay Values
cap_99 <- function(x) {
  upper_limit <- quantile(x, 0.99, na.rm = TRUE)
  ifelse(x > upper_limit, upper_limit, x)
}

data <- data %>%
  mutate(
    departure_delay_capped = cap_99(departure_delay_in_minutes),
    arrival_delay_capped = cap_99(arrival_delay_in_minutes)
  )


# Normalize Important Numeric Variables
normalize <- function(x) {
  (x - min(x, na.rm = TRUE)) / 
    (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

data <- data %>%
  mutate(
    age_norm = normalize(age),
    flight_distance_norm = normalize(flight_distance),
    smooth_process_score_norm = normalize(smooth_process_score),
    overall_experience_score_norm = normalize(overall_experience_score),
    departure_delay_norm = normalize(departure_delay_capped),
    arrival_delay_norm = normalize(arrival_delay_capped)
  )


# Dataset Check

print(dim(data))
print(colSums(is.na(data)))
print(summary(data))


# Save Cleaned Dataset
dir.create("data/cleaned", recursive = TRUE, showWarnings = FALSE)

write.csv(
  data,
  "data/cleaned/airline_customer_satisfaction_cleaned.csv",
  row.names = FALSE
)
