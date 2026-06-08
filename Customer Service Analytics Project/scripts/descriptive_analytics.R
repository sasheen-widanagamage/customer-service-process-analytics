setwd("C:/Users/wsash/OneDrive/Desktop/Customer Service Analytics Project")
getwd()


# Import Cleaned Dataset


data <- read.csv("data/cleaned/airline_customer_satisfaction_cleaned.csv")

data <- data %>%
  clean_names()


print(dim(data))
print(names(data))


# Convert Required Variables


data <- data %>%
  mutate(
    satisfaction = as.factor(satisfaction),
    customer_type = as.factor(customer_type),
    type_of_travel = as.factor(type_of_travel),
    class = as.factor(class),
    smooth_process_group = as.factor(smooth_process_group),
    arrival_delay_group = as.factor(arrival_delay_group),
    satisfaction_binary = as.numeric(satisfaction_binary)
  )



# Create ggplot Theme


custom_theme <- theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 10),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )



# 7. Summary Statistics


summary_stats <- data %>%
  summarise(
    total_customers = n(),
    mean_age = mean(age, na.rm = TRUE),
    median_age = median(age, na.rm = TRUE),
    sd_age = sd(age, na.rm = TRUE),
    mean_flight_distance = mean(flight_distance, na.rm = TRUE),
    median_flight_distance = median(flight_distance, na.rm = TRUE),
    sd_flight_distance = sd(flight_distance, na.rm = TRUE),
    mean_smooth_process_score = mean(smooth_process_score, na.rm = TRUE),
    median_smooth_process_score = median(smooth_process_score, na.rm = TRUE),
    sd_smooth_process_score = sd(smooth_process_score, na.rm = TRUE),
    mean_overall_experience_score = mean(overall_experience_score, na.rm = TRUE),
    median_overall_experience_score = median(overall_experience_score, na.rm = TRUE),
    sd_overall_experience_score = sd(overall_experience_score, na.rm = TRUE),
    satisfaction_rate = mean(satisfaction_binary, na.rm = TRUE)
  )

print(summary_stats)

write.csv(
  summary_stats,
  "outputs/tables/descriptive_summary_statistics.csv",
  row.names = FALSE
)



# Satisfaction Distribution

satisfaction_summary <- data %>%
  count(satisfaction) %>%
  mutate(
    percentage = round(n / sum(n) * 100, 2)
  )

print(satisfaction_summary)

write.csv(
  satisfaction_summary,
  "outputs/tables/satisfaction_distribution.csv",
  row.names = FALSE
)


p1 <- ggplot(satisfaction_summary, aes(x = satisfaction, y = n, fill = satisfaction)) +
  geom_col(width = 0.65) +
  geom_text(
    aes(label = paste0(n, " (", percentage, "%)")),
    vjust = -0.4,
    fontface = "bold"
  ) +
  scale_fill_manual(values = c("#E74C3C", "#2E86C1")) +
  labs(
    title = "Customer Satisfaction Distribution",
    subtitle = "Distribution of satisfied and dissatisfied customers",
    x = "Satisfaction Status",
    y = "Number of Customers"
  ) +
  custom_theme

print(p1)

ggsave("outputs/plots/01_satisfaction_distribution.png", p1, width = 8, height = 7, dpi = 300)


# Smooth Process Score Distribution
p2 <- ggplot(data, aes(x = smooth_process_score)) +
  geom_histogram(binwidth = 0.25, fill = "#16A085", color = "white") +
  labs(
    title = "Distribution of Smooth Service Process Score",
    subtitle = "Higher scores indicate smoother customer service processes",
    x = "Smooth Service Process Score",
    y = "Number of Customers"
  ) +
  custom_theme

print(p2)

ggsave("outputs/plots/02_smooth_process_score_distribution.png", p2, width = 8, height = 5, dpi = 300)



# Overall Experience Score Distribution

p3 <- ggplot(data, aes(x = overall_experience_score)) +
  geom_histogram(binwidth = 0.25, fill = "#8E44AD", color = "white") +
  labs(
    title = "Distribution of Overall Experience Score",
    subtitle = "Higher scores indicate better customer experience",
    x = "Overall Experience Score",
    y = "Number of Customers"
  ) +
  custom_theme

print(p3)

ggsave("outputs/plots/03_overall_experience_score_distribution.png", p3, width = 8, height = 5, dpi = 300)



# Group Summary by Smooth Process Group

group_summary <- data %>%
  group_by(smooth_process_group) %>%
  summarise(
    customer_count = n(),
    mean_smooth_process_score = mean(smooth_process_score, na.rm = TRUE),
    mean_overall_experience_score = mean(overall_experience_score, na.rm = TRUE),
    median_overall_experience_score = median(overall_experience_score, na.rm = TRUE),
    sd_overall_experience_score = sd(overall_experience_score, na.rm = TRUE),
    satisfaction_rate = mean(satisfaction_binary, na.rm = TRUE)
  )

print(group_summary)

write.csv(
  group_summary,
  "outputs/tables/group_summary_by_smooth_process.csv",
  row.names = FALSE
)



# Box Plot: Overall Experience by Smooth Process Group


p4 <- ggplot(data, aes(x = smooth_process_group, y = overall_experience_score, fill = smooth_process_group)) +
  geom_boxplot(alpha = 0.85, outlier.alpha = 0.25) +
  scale_fill_manual(values = c("#E74C3C", "#F39C12", "#27AE60")) +
  labs(
    title = "Overall Experience by Smooth Service Process Group",
    subtitle = "Comparison of customer experience across process smoothness levels",
    x = "Smooth Service Process Group",
    y = "Overall Experience Score"
  ) +
  custom_theme

print(p4)

ggsave("outputs/plots/04_experience_by_smooth_process_group.png", p4, width = 8, height = 5, dpi = 300)



# Bar Chart: Satisfaction Rate by Smooth Process Group

satisfaction_by_group <- data %>%
  group_by(smooth_process_group) %>%
  summarise(
    satisfaction_rate = mean(satisfaction_binary, na.rm = TRUE),
    customer_count = n()
  )

p5 <- ggplot(satisfaction_by_group, aes(x = smooth_process_group, y = satisfaction_rate, fill = smooth_process_group)) +
  geom_col(width = 0.65) +
  geom_text(
    aes(label = percent(satisfaction_rate, accuracy = 0.1)),
    vjust = -0.4,
    fontface = "bold"
  ) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
  scale_fill_manual(values = c("#C0392B", "#F1C40F", "#229954")) +
  labs(
    title = "Satisfaction Rate by Smooth Service Process Group",
    subtitle = "Higher process smoothness should show higher satisfaction",
    x = "Smooth Service Process Group",
    y = "Satisfaction Rate"
  ) +
  custom_theme

print(p5)

ggsave("outputs/plots/05_satisfaction_rate_by_smooth_process_group.png", p5, width = 8, height = 5, dpi = 300)



# Mean Overall Experience by Smooth Process Group


p6 <- ggplot(group_summary, aes(x = smooth_process_group, y = mean_overall_experience_score, fill = smooth_process_group)) +
  geom_col(width = 0.65) +
  geom_text(
    aes(label = round(mean_overall_experience_score, 2)),
    vjust = -0.4,
    fontface = "bold"
  ) +
  scale_fill_manual(values = c("#E74C3C", "#F39C12", "#27AE60")) +
  labs(
    title = "Average Overall Experience by Smooth Process Group",
    subtitle = "Comparison of mean customer experience scores",
    x = "Smooth Service Process Group",
    y = "Mean Overall Experience Score"
  ) +
  custom_theme

print(p6)

ggsave("outputs/plots/06_mean_experience_by_smooth_process_group.png", p6, width = 8, height = 6, dpi = 300)



# Delay Group Summary

delay_summary <- data %>%
  group_by(arrival_delay_group) %>%
  summarise(
    customer_count = n(),
    mean_overall_experience_score = mean(overall_experience_score, na.rm = TRUE),
    satisfaction_rate = mean(satisfaction_binary, na.rm = TRUE)
  )

print(delay_summary)

write.csv(
  delay_summary,
  "outputs/tables/delay_group_summary.csv",
  row.names = FALSE
)



# Satisfaction Rate by Arrival Delay Group


p7 <- ggplot(delay_summary, aes(x = arrival_delay_group, y = satisfaction_rate, fill = arrival_delay_group)) +
  geom_col(width = 0.65) +
  geom_text(
    aes(label = percent(satisfaction_rate, accuracy = 0.1)),
    vjust = -0.4,
    fontface = "bold"
  ) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
  scale_fill_manual(values = c("#2ECC71", "#F1C40F", "#E67E22", "#C0392B", "#95A5A6")) +
  labs(
    title = "Satisfaction Rate by Arrival Delay Group",
    subtitle = "Comparison of satisfaction across delay levels",
    x = "Arrival Delay Group",
    y = "Satisfaction Rate"
  ) +
  custom_theme

print(p7)

ggsave("outputs/plots/07_satisfaction_rate_by_arrival_delay_group.png", p7, width = 9, height = 5, dpi = 300)




# Correlation Heatmap


cor_data <- data %>%
  select(
    Smooth_Process = smooth_process_score,
    Overall_Experience = overall_experience_score,
    Satisfaction = satisfaction_binary,
    Online_Booking = ease_of_online_booking,
    Online_Boarding = online_boarding,
    Checkin = checkin_service,
    Gate_Location = gate_location,
    Baggage = baggage_handling,
    Time_Convenient = departure_arrival_time_convenient,
    Departure_Delay = departure_delay_capped,
    Arrival_Delay = arrival_delay_capped
  )

cor_matrix <- cor(cor_data, use = "complete.obs")

p8 <- ggcorrplot(
  cor_matrix,
  type = "lower",
  lab = TRUE,
  lab_size = 3,
  colors = c("#E74C3C", "white", "#2E86C1"),
  outline.color = "white",
  title = "Correlation Heatmap of Service Process and Experience Variables"
) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 9),
    axis.text.y = element_text(size = 9),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "bottom",
    plot.margin = margin(10, 20, 10, 20)
  )

print(p8)

ggsave("outputs/plots/08_correlation_heatmap.png",p8,width = 10,height = 8,dpi = 300)
