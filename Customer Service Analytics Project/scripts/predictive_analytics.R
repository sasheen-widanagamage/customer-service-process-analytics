# Import Cleaned Dataset
data <- read.csv("data/cleaned/airline_customer_satisfaction_cleaned.csv")

data <- data %>%
  clean_names()

# Prepare Data for Predictive Modelling
model_data <- data %>%
  select(
    satisfaction_binary,
    smooth_process_score,
    overall_experience_score,
    ease_of_online_booking,
    online_boarding,
    checkin_service,
    gate_location,
    baggage_handling,
    departure_arrival_time_convenient,
    seat_comfort,
    food_and_drink,
    inflight_wifi_service,
    inflight_entertainment,
    online_support,
    on_board_service,
    leg_room_service,
    cleanliness,
    departure_delay_capped,
    arrival_delay_capped,
    age,
    flight_distance,
    customer_type,
    type_of_travel,
    class
  ) %>%
  drop_na()

model_data <- model_data %>%
  mutate(
    satisfaction_binary = factor(
      satisfaction_binary,
      levels = c(0, 1),
      labels = c("Not_Satisfied", "Satisfied")
    ),
    customer_type = as.factor(customer_type),
    type_of_travel = as.factor(type_of_travel),
    class = as.factor(class)
  )

print(dim(model_data))
print(table(model_data$satisfaction_binary))

# Train-Test Split
set.seed(123)

train_index <- createDataPartition(
  model_data$satisfaction_binary,
  p = 0.8,
  list = FALSE
)

train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

cat("Training rows:", nrow(train_data), "\n")
cat("Testing rows:", nrow(test_data), "\n")


# Train Logistic Regression Model

logistic_model <- glm(
  satisfaction_binary ~ .,
  data = train_data,
  family = binomial
)

print(summary(logistic_model))



# Save Model Coefficients

coef_table <- tidy(
  logistic_model,
  exponentiate = TRUE,
  conf.int = TRUE
)

coef_table <- coef_table %>%
  rename(
    odds_ratio = estimate,
    conf_low = conf.low,
    conf_high = conf.high
  )

print(coef_table)

write.csv(
  coef_table,
  "outputs/tables/logistic_regression_coefficients_full_model.csv",
  row.names = FALSE
)



# Make Predictions Using Full Model

pred_prob_full <- predict(
  logistic_model,
  newdata = test_data,
  type = "response"
)

pred_class_full <- ifelse(
  pred_prob_full >= 0.5,
  "Satisfied",
  "Not_Satisfied"
)

pred_class_full <- factor(
  pred_class_full,
  levels = c("Not_Satisfied", "Satisfied")
)



# Confusion Matrix - Full Model


conf_full <- confusionMatrix(
  pred_class_full,
  test_data$satisfaction_binary,
  positive = "Satisfied"
)

print(conf_full)

conf_table_full <- as.data.frame(conf_full$table)

write.csv(
  conf_table_full,
  "outputs/tables/confusion_matrix_full_model.csv",
  row.names = FALSE
)

metrics_full <- data.frame(
  model = "Full Logistic Regression",
  accuracy = as.numeric(conf_full$overall["Accuracy"]),
  sensitivity = as.numeric(conf_full$byClass["Sensitivity"]),
  specificity = as.numeric(conf_full$byClass["Specificity"]),
  precision = as.numeric(conf_full$byClass["Precision"]),
  recall = as.numeric(conf_full$byClass["Recall"]),
  f1_score = as.numeric(conf_full$byClass["F1"])
)

print(metrics_full)



# ROC Curve and AUC - Full Model


roc_full <- roc(
  response = test_data$satisfaction_binary,
  predictor = pred_prob_full,
  levels = c("Not_Satisfied", "Satisfied"),
  direction = "<"
)

auc_full <- auc(roc_full)

cat("Full Model AUC:", auc_full, "\n")

metrics_full$auc <- as.numeric(auc_full)



# Feature Selection Using Stepwise Selection


step_model <- step(
  logistic_model,
  direction = "both",
  trace = FALSE
)

print(summary(step_model))



# Save Selected Model Coefficients

coef_table_step <- tidy(
  step_model,
  exponentiate = TRUE,
  conf.int = TRUE
)

coef_table_step <- coef_table_step %>%
  rename(
    odds_ratio = estimate,
    conf_low = conf.low,
    conf_high = conf.high
  )

print(coef_table_step)

write.csv(
  coef_table_step,
  "outputs/tables/logistic_regression_coefficients_selected_model.csv",
  row.names = FALSE
)



# Make Predictions Using Selected Model

pred_prob_step <- predict(
  step_model,
  newdata = test_data,
  type = "response"
)

pred_class_step <- ifelse(
  pred_prob_step >= 0.5,
  "Satisfied",
  "Not_Satisfied"
)

pred_class_step <- factor(
  pred_class_step,
  levels = c("Not_Satisfied", "Satisfied")
)



# Confusion Matrix - Selected Model


conf_step <- confusionMatrix(
  pred_class_step,
  test_data$satisfaction_binary,
  positive = "Satisfied"
)

print(conf_step)

conf_table_step <- as.data.frame(conf_step$table)

write.csv(
  conf_table_step,
  "outputs/tables/confusion_matrix_selected_model.csv",
  row.names = FALSE
)

metrics_step <- data.frame(
  model = "Stepwise Selected Logistic Regression",
  accuracy = as.numeric(conf_step$overall["Accuracy"]),
  sensitivity = as.numeric(conf_step$byClass["Sensitivity"]),
  specificity = as.numeric(conf_step$byClass["Specificity"]),
  precision = as.numeric(conf_step$byClass["Precision"]),
  recall = as.numeric(conf_step$byClass["Recall"]),
  f1_score = as.numeric(conf_step$byClass["F1"])
)



# ROC Curve and AUC - Selected Model

roc_step <- roc(
  response = test_data$satisfaction_binary,
  predictor = pred_prob_step,
  levels = c("Not_Satisfied", "Satisfied"),
  direction = "<"
)

auc_step <- auc(roc_step)

cat("Selected Model AUC:", auc_step, "\n")

metrics_step$auc <- as.numeric(auc_step)



# Compare Full Model and Selected Model

model_comparison <- bind_rows(
  metrics_full,
  metrics_step
)

print(model_comparison)

write.csv(
  model_comparison,
  "outputs/tables/model_performance_comparison.csv",
  row.names = FALSE
)



#  ROC Curve Plot

custom_theme <- theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

roc_full_data <- data.frame(
  specificity = roc_full$specificities,
  sensitivity = roc_full$sensitivities,
  model = "Full Model"
)

roc_step_data <- data.frame(
  specificity = roc_step$specificities,
  sensitivity = roc_step$sensitivities,
  model = "Selected Model"
)

roc_plot_data <- bind_rows(roc_full_data, roc_step_data)

p1 <- ggplot(roc_plot_data, aes(x = 1 - specificity, y = sensitivity, color = model)) +
  geom_line(linewidth = 1.2) +
  geom_abline(linetype = "dashed", color = "gray50") +
  scale_color_manual(values = c("#2E86C1", "#E74C3C")) +
  labs(
    title = "ROC Curve Comparison",
    subtitle = paste(
      "Full Model AUC =", round(auc_full, 3),
      "| Selected Model AUC =", round(auc_step, 3)
    ),
    x = "False Positive Rate",
    y = "True Positive Rate",
    color = "Model"
  ) +
  custom_theme

print(p1)

ggsave("outputs/plots/11_roc_curve_model_comparison.png", p1, width = 8, height = 5, dpi = 300)



# Model Performance Comparison Plot


performance_long <- model_comparison %>%
  select(model, accuracy, sensitivity, specificity, precision, f1_score, auc) %>%
  pivot_longer(
    cols = -model,
    names_to = "metric",
    values_to = "value"
  )

p2 <- ggplot(performance_long, aes(x = metric, y = value, fill = model)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_text(
    aes(label = round(value, 3)),
    position = position_dodge(width = 0.7),
    vjust = -0.3,
    size = 3.5
  ) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_fill_manual(values = c("#2E86C1", "#E74C3C")) +
  labs(
    title = "Model Performance Comparison",
    subtitle = "Comparison of full and feature-selected logistic regression models",
    x = "Evaluation Metric",
    y = "Score",
    fill = "Model"
  ) +
  custom_theme

print(p2)

ggsave("outputs/plots/12_model_performance_comparison.png", p2, width = 9, height = 5, dpi = 300)



# Important Predictors Table


important_predictors <- coef_table_step %>%
  filter(term != "(Intercept)") %>%
  arrange(p.value) %>%
  select(term, odds_ratio, conf_low, conf_high, p.value)

print(important_predictors)

write.csv(
  important_predictors,
  "outputs/tables/important_predictors_selected_model.csv",
  row.names = FALSE
)



# Important Predictors Plot

top_predictors <- important_predictors %>%
  filter(p.value < 0.05) %>%
  slice_head(n = 10)

p3 <- ggplot(top_predictors, aes(x = reorder(term, odds_ratio), y = odds_ratio)) +
  geom_col(fill = "#27AE60", width = 0.65) +
  coord_flip() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray40") +
  labs(
    title = "Top Significant Predictors of Customer Satisfaction",
    subtitle = "Odds ratios greater than 1 increase the likelihood of satisfaction",
    x = "Predictor",
    y = "Odds Ratio"
  ) +
  custom_theme

print(p3)

ggsave("outputs/plots/13_top_predictors_odds_ratios.png", p3, width = 9, height = 6, dpi = 300)



# Save Predictions

prediction_results <- test_data %>%
  select(satisfaction_binary) %>%
  mutate(
    predicted_probability_full_model = pred_prob_full,
    predicted_class_full_model = pred_class_full,
    predicted_probability_selected_model = pred_prob_step,
    predicted_class_selected_model = pred_class_step
  )

write.csv(
  prediction_results,
  "outputs/tables/prediction_results.csv",
  row.names = FALSE
)



# Final Predictive Analytics Summary

cat("\nPredictive Analytics Summary\n")
cat("Full Model Accuracy:", round(metrics_full$accuracy, 4), "\n")
cat("Full Model AUC:", round(metrics_full$auc, 4), "\n")
cat("Selected Model Accuracy:", round(metrics_step$accuracy, 4), "\n")
cat("Selected Model AUC:", round(metrics_step$auc, 4), "\n")

if (metrics_step$auc >= 0.7) {
  cat("Interpretation: The selected logistic regression model has acceptable predictive ability.\n")
} else {
  cat("Interpretation: The selected logistic regression model has weak predictive ability and may need improvement.\n")
}
