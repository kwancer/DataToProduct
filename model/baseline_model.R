library(dplyr)
library(readr)
library(Metrics)

# LOAD DATA
df <- read_csv("weather_soilmoisture_merged.csv")

df <- df %>%
  mutate(date = as.Date(date)) %>%
  arrange(location, date)

train_df <- df %>% filter(date < "2021-01-01")
val_df <- df %>% filter(date >= "2021-01-01", date < "2023-01-01")
test_df <- df %>% filter(date >= "2023-01-01")


df <- df %>%
  arrange(location, date) %>%
  group_by(location) %>%
  mutate(persist_pred = lag(soil_moisture, 1)) %>%
  ungroup()

# Extract persistence predictions for val/test sets
val_df <- df %>% filter(date >= "2021-01-01", date < "2023-01-01")
test_df <- df %>% filter(date >= "2023-01-01")

# Remove rows with NA persist_pred (first day of each series)
val_df <- val_df %>% filter(!is.na(persist_pred))
test_df <- test_df %>% filter(!is.na(persist_pred))

# Evaluation function
get_metrics <- function(actual, predicted) {
  tibble(
    RMSE = rmse(actual, predicted),
    MAE  = mae(actual, predicted),
    R2   = 1 - sum((actual - predicted)^2) / sum((actual - mean(actual))^2)
  )
}

# Compute persistence model metrics
metrics_val_persist <- get_metrics(val_df$soil_moisture, val_df$persist_pred)
metrics_test_persist <- get_metrics(test_df$soil_moisture, test_df$persist_pred)

cat("===== GLOBAL PERSISTENCE — VALIDATION =====\n")
print(metrics_val_persist)

cat("\n===== GLOBAL PERSISTENCE — TEST =====\n")
print(metrics_test_persist)

# Save metrics
write_csv(
  bind_rows(
    Validation_Persistence = metrics_val_persist,
    Test_Persistence = metrics_test_persist,
    .id = "Dataset"
  ),
  "persistence_model_metrics.csv"
)

cat("\nSaved persistence baseline metrics to persistence_model_metrics.csv\n")
