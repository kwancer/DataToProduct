# In this file we train the model.
library(dplyr)
library(lubridate)
library(slider)
library(readr)
library(mgcv)
library(ggplot2)
library(Metrics)


# LOAD MERGED DATA
df <- read_csv("weather_soilmoisture_merged.csv")
df <- df %>% mutate(date = as.Date(date))
df <- df %>% arrange(location, date)

# CLEAN COLUMN NAMES (fix for formulas)
df <- df %>%
  rename(
    humidity = `humidity %`,
    wind_speed = `wind_speed km/h`
  )

# FEATURE ENGINEERING
df <- df %>%
  group_by(location) %>%
  mutate(
    # Autoregressive soil moisture memory
    sm_lag1 = lag(soil_moisture, 1),

    # Rainfall windows
    rain_3d = slide_dbl(`rain mm`, sum, .before = 2, .complete = TRUE),
    rain_7d = slide_dbl(`rain mm`, sum, .before = 6, .complete = TRUE),

    # Temperature window
    temp_3d = slide_dbl(`max_temp °c`, mean, .before = 2, .complete = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    doy = yday(date) # seasonal cyclic term
  )

df <- df %>%
  filter(
    !is.na(sm_lag1),
    !is.na(rain_3d),
    !is.na(rain_7d),
    !is.na(temp_3d)
  )

cat("Rows after feature engineering:", nrow(df), "\n")

# TRAIN / VALIDATION / TEST SPLIT
train_df <- df %>% filter(date < "2021-01-01")
val_df <- df %>% filter(date >= "2021-01-01", date < "2023-01-01")
test_df <- df %>% filter(date >= "2023-01-01")

cat("Training set:", nrow(train_df), "\n")
cat("Validation set:", nrow(val_df), "\n")
cat("Test set:", nrow(test_df), "\n")


# 5. TRAIN BIG GAM MODEL (bam)
bam_mod <- bam(
  soil_moisture ~
    s(rain_3d, k = 7) +
    s(rain_7d, k = 7) +
    s(temp_3d, k = 7) +
    s(humidity, k = 7) +
    s(wind_speed, k = 7) +
    s(sm_lag1, k = 7) +
    s(doy, bs = "cc", k = 12),
  data = train_df,
  method = "fREML",
  discrete = TRUE,
  nthreads = 4
)

summary(bam_mod)


# PREDICT ON TRAIN/VAL/TEST
train_df$pred <- predict(bam_mod, newdata = train_df)
val_df$pred <- predict(bam_mod, newdata = val_df)
test_df$pred <- predict(bam_mod, newdata = test_df)


# METRICS
get_metrics <- function(df) {
  tibble(
    RMSE = rmse(df$soil_moisture, df$pred),
    MAE = mae(df$soil_moisture, df$pred),
    R2 = 1 - sum((df$soil_moisture - df$pred)^2) /
      sum((df$soil_moisture - mean(df$soil_moisture))^2)
  )
}

metrics_train <- get_metrics(train_df)
metrics_val <- get_metrics(val_df)
metrics_test <- get_metrics(test_df)

cat("===== TRAIN METRICS =====\n")
print(metrics_train)
cat("===== VALIDATION METRICS =====\n")
print(metrics_val)
cat("===== TEST METRICS =====\n")
print(metrics_test)

write_csv(
  bind_rows(
    Train = metrics_train,
    Validation = metrics_val,
    Test = metrics_test,
    .id = "Dataset"
  ),
  "bam_model_metrics.csv"
)


# PREDICTED VS ACTUAL PLOTS
plot_scatter <- function(df, name) {
  ggplot(df, aes(x = soil_moisture, y = pred)) +
    geom_point(alpha = 0.15, color = "blue") +
    geom_abline(slope = 1, intercept = 0, color = "red") +
    labs(
      title = paste("Predicted vs Actual —", name),
      x = "Actual Soil Moisture",
      y = "Predicted Soil Moisture"
    ) +
    theme_minimal()
}

ggsave("train_pred_vs_actual.png", plot_scatter(train_df, "Train"), width = 7, height = 7)
ggsave("val_pred_vs_actual.png", plot_scatter(val_df, "Validation"), width = 7, height = 7)
ggsave("test_pred_vs_actual.png", plot_scatter(test_df, "Test"), width = 7, height = 7)


# RESIDUALS OVER TIME (TEST SET)
test_df$resid <- test_df$soil_moisture - test_df$pred

p_resid <- ggplot(test_df, aes(x = date, y = resid)) +
  geom_line(alpha = 0.4) +
  geom_hline(yintercept = 0, color = "red") +
  labs(
    title = "Residuals Over Time — Test Set",
    x = "Date", y = "Residual"
  ) +
  theme_minimal()

ggsave("test_residuals.png", p_resid, width = 10, height = 5)


# SAVE SMOOTH TERM PLOTS
png("bam_smooth_terms.png", width = 1200, height = 1400)
plot(bam_mod, pages = 1, shade = TRUE, scale = 0)
dev.off()


# SAVE TRAINED MODEL
saveRDS(bam_mod, "soil_moisture_bam_model.rds")

cat("\nSaved BAM model to soil_moisture_bam_model.rds\n")
