# Load required libraries
library(ggplot2)
library(dplyr)

# Load dataset
data <- read.csv("C:/Users/admin/AppData/Local/Packages/5319275A.WhatsAppDesktop_cv1g1gvanyjgm/LocalState/sessions/DD98FAC06F763B832153946F371265CB537F782B/transfers/2026-17/StudentsPerformance.csv")

# View structure of dataset
str(data)

# View first few rows
head(data)

# Summary statistics
summary(data)

# Rename columns for easier use
colnames(data) <- c("gender", "race", "parent_edu", "lunch",
                    "test_prep", "math", "reading", "writing")

# Create average score
data$average <- rowMeans(data[, c("math", "reading", "writing")])

# Create performance categories
data$performance <- ifelse(data$average >= 85, "Excellent",
                           ifelse(data$average >= 70, "Good",
                                  ifelse(data$average >= 50, "Average", "Poor")))

# Convert to factor
data$performance <- as.factor(data$performance)

# Display updated dataset
head(data)

# Count students in each performance category
performance_count <- table(data$performance)
print(performance_count)

# Gender-wise average score
gender_avg <- data %>%
  group_by(gender) %>%
  summarise(avg_score = mean(average))

print(gender_avg)

# Plot 1: Distribution of average scores
ggplot(data, aes(x = average)) +
  geom_histogram(fill = "blue", bins = 20) +
  ggtitle("Distribution of Average Scores")

# Plot 2: Performance by gender
ggplot(data, aes(x = gender, fill = performance)) +
  geom_bar() +
  ggtitle("Performance by Gender")

# Plot 3: Math vs Reading scores
ggplot(data, aes(x = math, y = reading)) +
  geom_point(color = "red") +
  ggtitle("Math vs Reading Scores")

# Plot 4: Lunch impact on performance
ggplot(data, aes(x = lunch, y = average, fill = lunch)) +
  geom_boxplot() +
  ggtitle("Impact of Lunch on Performance")

# Correlation analysis
correlation <- cor(data$math, data$reading)
print(paste("Correlation between Math and Reading:", correlation))

# Top 5 students based on average score
top_students <- data %>%
  arrange(desc(average)) %>%
  head(5)

print(top_students)

# Save processed dataset
write.csv(data, "processed_students.csv", row.names = FALSE)

# Final message
cat("Analysis Completed Successfully!\n")
