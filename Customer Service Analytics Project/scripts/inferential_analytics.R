
# Import Cleaned Dataset
data <- read.csv("data/cleaned/airline_customer_satisfaction_cleaned.csv")

data <- data %>%
  clean_names()

cat("Cleaned dataset imported successfully.\n")
print(dim(data))



# Convert Required Variables
data <- data %>%
  mutate(
    satisfaction = as.factor(satisfaction),
    customer_type = as.factor(customer_type),
    type_of_travel = as.factor(type_of_travel),
    class = as.factor(class),
    smooth_process_group = factor(
      smooth_process_group,
      levels = c("Low Smoothness", "Moderate Smoothness", "High Smoothness")
    ),
    arrival_delay_group = as.factor(arrival_delay_group),
    satisfaction_binary = as.numeric(satisfaction_binary)
  )



# Main Hypotheses

cat("\nMain Research Statement:\n")
cat("Customers who experience smooth service processes report better overall experiences.\n\n")

cat("ANOVA Hypotheses:\n")
cat("H0: There is no significant difference in overall experience scores among smooth process groups.\n")
cat("H1: At least one smooth process group has a significantly different overall experience score.\n\n")


# One-Way ANOVA

anova_model <- aov(overall_experience_score ~ smooth_process_group, data = data)

anova_result <- tidy(anova_model)

print(anova_result)

write.csv(
  anova_result,
  "outputs/tables/anova_overall_experience_by_smooth_group.csv",
  row.names = FALSE
)



# ANOVA Result Interpretation 

anova_p_value <- anova_result$p.value[1]

if (anova_p_value < 0.05) {
  cat("\nANOVA Result: p-value is less than 0.05.\n")
  cat("Decision: Reject H0.\n")
  cat("Interpretation: Overall experience differs significantly across service smooth process groups.\n")
} else {
  cat("\nANOVA Result: p-value is greater than or equal to 0.05.\n")
  cat("Decision: Fail to reject H0.\n")
  cat("Interpretation: No statistically significant difference was found across smooth service process groups.\n")
}



# Tukey Post-Hoc Test

tukey_result <- TukeyHSD(anova_model)

print(tukey_result)

tukey_table <- as.data.frame(tukey_result$smooth_process_group)

tukey_table$comparison <- rownames(tukey_table)

tukey_table <- tukey_table %>%
  select(comparison, everything())

write.csv(
  tukey_table,
  "outputs/tables/tukey_posthoc_smooth_process_group.csv",
  row.names = FALSE
)



# ANOVA Assumption Check - Normality of Residuals

# Shapiro-Wilk test accepts maximum 5000 observations.
# Since this dataset is large, a random sample of residuals is used.

set.seed(123)

residual_sample <- sample(
  residuals(anova_model),
  size = min(5000, length(residuals(anova_model)))
)

shapiro_result <- shapiro.test(residual_sample)

print(shapiro_result)

shapiro_table <- data.frame(
  test = "Shapiro-Wilk Normality Test",
  statistic = as.numeric(shapiro_result$statistic),
  p_value = shapiro_result$p.value
)

write.csv(
  shapiro_table,
  "outputs/tables/shapiro_normality_test.csv",
  row.names = FALSE
)



# ANOVA Assumption Check - Homogeneity of Variance

levene_result <- leveneTest(overall_experience_score ~ smooth_process_group, data = data)

print(levene_result)

levene_table <- as.data.frame(levene_result)

write.csv(
  levene_table,
  "outputs/tables/levene_variance_test.csv",
  row.names = FALSE
)



# Chi-Square Test

cat("\nChi-Square Hypotheses:\n")
cat("H0: Smooth service process group and satisfaction status are independent.\n")
cat("H1: Smooth service process group and satisfaction status are associated.\n\n")

chi_table <- table(data$smooth_process_group, data$satisfaction)

print(chi_table)

chi_result <- chisq.test(chi_table)

print(chi_result)

chi_output <- tidy(chi_result)

write.csv(
  chi_output,
  "outputs/tables/chi_square_smooth_group_satisfaction.csv",
  row.names = FALSE
)

chi_percentage_table <- prop.table(chi_table, margin = 1) * 100

write.csv(
  as.data.frame.matrix(round(chi_percentage_table, 2)),
  "outputs/tables/chi_square_percentage_table.csv",
  row.names = TRUE
)



# Chi-Square Interpretation 

if (chi_result$p.value < 0.05) {
  cat("\nChi-Square Result: p-value is less than 0.05.\n")
  cat("Decision: Reject H0.\n")
  cat("Interpretation: Smooth process group and customer satisfaction are significantly associated.\n")
} else {
  cat("\nChi-Square Result: p-value is greater than or equal to 0.05.\n")
  cat("Decision: Fail to reject H0.\n")
  cat("Interpretation: No significant association was found between smooth process group and satisfaction.\n")
}



# Pearson Correlation Test

cat("\nCorrelation Hypotheses:\n")
cat("H0: There is no significant correlation between smooth process score and overall experience score.\n")
cat("H1: There is a significant correlation between smooth process score and overall experience score.\n\n")

cor_result <- cor.test(
  data$smooth_process_score,
  data$overall_experience_score,
  method = "pearson"
)

print(cor_result)

cor_table <- data.frame(
  test = "Pearson Correlation",
  correlation = as.numeric(cor_result$estimate),
  t_statistic = as.numeric(cor_result$statistic),
  df = as.numeric(cor_result$parameter),
  p_value = cor_result$p.value,
  conf_low = cor_result$conf.int[1],
  conf_high = cor_result$conf.int[2]
)

write.csv(
  cor_table,
  "outputs/tables/pearson_correlation_smooth_experience.csv",
  row.names = FALSE
)



# Two-Sample t-Test

cat("\nt-Test Hypotheses:\n")
cat("H0: Mean smooth service process score is equal for satisfied and dissatisfied customers.\n")
cat("H1: Mean smooth service process score is different for satisfied and dissatisfied customers.\n\n")

ttest_result <- t.test(smooth_process_score ~ satisfaction, data = data)

print(ttest_result)

ttest_table <- data.frame(
  test = "Two-Sample t-Test",
  t_statistic = as.numeric(ttest_result$statistic),
  df = as.numeric(ttest_result$parameter),
  p_value = format(ttest_result$p.value, scientific = TRUE),
  conf_low = ttest_result$conf.int[1],
  conf_high = ttest_result$conf.int[2],
  mean_group_1 = as.numeric(ttest_result$estimate[1]),
  mean_group_2 = as.numeric(ttest_result$estimate[2])
)

write.csv(
  ttest_table,
  "outputs/tables/t_test_smooth_score_by_satisfaction.csv",
  row.names = FALSE
)



# Confidence Intervals by Smooth Process Group

ci_table <- data %>%
  group_by(smooth_process_group) %>%
  summarise(
    n = n(),
    mean_overall_experience = mean(overall_experience_score, na.rm = TRUE),
    sd_overall_experience = sd(overall_experience_score, na.rm = TRUE),
    se = sd_overall_experience / sqrt(n),
    t_critical = qt(0.975, df = n - 1),
    ci_lower = mean_overall_experience - t_critical * se,
    ci_upper = mean_overall_experience + t_critical * se
  )

print(ci_table)

write.csv(
  ci_table,
  "outputs/tables/confidence_intervals_by_smooth_group.csv",
  row.names = FALSE
)



# Plot - Confidence Intervals

custom_theme <- theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

p1 <- ggplot(ci_table, aes(x = smooth_process_group, y = mean_overall_experience, fill = smooth_process_group)) +
  geom_col(width = 0.65) +
  geom_errorbar(
    aes(ymin = ci_lower, ymax = ci_upper),
    width = 0.2,
    linewidth = 0.8
  ) +
  geom_text(
    aes(label = round(mean_overall_experience, 2)),
    vjust = -0.6,
    fontface = "bold"
  ) +
  scale_fill_manual(values = c("#E74C3C", "#F39C12", "#27AE60")) +
  labs(
    title = "95% Confidence Intervals for Overall Experience",
    subtitle = "Comparison across smooth service process groups",
    x = "Smooth Service Process Group",
    y = "Mean Overall Experience Score"
  ) +
  custom_theme

print(p1)

ggsave("outputs/plots/09_confidence_intervals_overall_experience.png", p1, width = 8, height = 7, dpi = 300)



# Plot - Chi-Square Percentage Chart

chi_plot_data <- data %>%
  group_by(smooth_process_group, satisfaction) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(smooth_process_group) %>%
  mutate(percentage = count / sum(count))

p2 <- ggplot(chi_plot_data, aes(x = smooth_process_group, y = percentage, fill = satisfaction)) +
  geom_col(position = "fill", width = 0.65) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = c("#E74C3C", "#2E86C1")) +
  labs(
    title = "Satisfaction Composition by Smooth Process Group",
    subtitle = "Used to support the chi-square association test",
    x = "Smooth Service Process Group",
    y = "Percentage",
    fill = "Satisfaction"
  ) +
  custom_theme

print(p2)

ggsave("outputs/plots/10_chi_square_satisfaction_composition.png", p2, width = 8, height = 5, dpi = 300)
