# Load necessary libraries
library(ggplot2)
library(dplyr)

# Define the data
data <- data.frame(
  ID = 1:15,
  Payload_Size = c(3936, 2870, 3833, 4301, 4569, 2688, 4634, 2600, 4285, 165, 3596, 2438, 1770, 527, 3892),
  Confidence_Level = c(0.60050679, 0.52414969, 0.23894440, 0.12233363, 0.97089519, 0.72688912, 0.83573062,
                       0.45188815, 0.11131313, 0.75959667, 0.99894117, 0.17306039, 0.82524652, 0.05315528, 0.66279811),
  ML_Model = c("K-Nearest Neighbors", "K-Nearest Neighbors", "Neural Network", "Neural Network", "Neural Network",
               "K-Nearest Neighbors", "K-Nearest Neighbors", "Support Vector Machine", "Support Vector Machine",
               "K-Nearest Neighbors", "Logistic Regression", "Neural Network", "Random Forest",
               "Support Vector Machine", "Random Forest")
)

# Aggregate average confidence levels by ML model
ml_summary <- data %>%
  group_by(ML_Model) %>%
  summarize(
    Avg_Confidence_Level = mean(Confidence_Level),
    Avg_Payload_Size = mean(Payload_Size),
    .groups = "drop"
  )

# Perform K-Means clustering on ML models based on average confidence levels
set.seed(42)  # Ensure reproducibility
kmeans_result <- kmeans(ml_summary$Avg_Confidence_Level, centers = 3)

# Add cluster labels to the summary data
ml_summary$Cluster <- as.factor(kmeans_result$cluster)

# Plot the bar chart
ggplot(ml_summary, aes(x = ML_Model, y = Avg_Payload_Size, fill = Cluster)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Clustering ML Models by Confidence Levels and Payload Size",
    x = "ML Model",
    y = "Average Payload Size"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette = "Set3")
