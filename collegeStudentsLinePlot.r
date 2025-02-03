library(ggplot2)
library(dplyr)
library(readr)
library(plotly)  

# Load the dataset
df <- read_csv("Users/Klebb24/Documents/Student Mental health.csv")

# Select relevant columns and rename them
df_clean <- df %>%
  select(Age, `Your current year of Study`, `Do you have Depression?`) %>%
  rename(Year_of_Study = `Your current year of Study`, Depression = `Do you have Depression?`)

# Convert 'Do you have Depression?' to binary (Yes = 1, No = 0)
df_clean <- df_clean %>%
  mutate(Depression = ifelse(tolower(trimws(Depression)) == "yes", 1, 0))

# Convert Age to numeric
df_clean <- df_clean %>%
  mutate(Age = as.numeric(Age))

# Group by Age and calculate the depression rate
depression_by_age <- df_clean %>%
  group_by(Age) %>%
  summarise(Depression_Rate = mean(Depression, na.rm = TRUE))

# Create a scatter plot with a trend line using ggplot2
p <- ggplot(depression_by_age, aes(x = Age, y = Depression_Rate)) +
  geom_point(color = "blue", alpha = 0.6) +  # Scatter points
  geom_smooth(method = "loess", color = "red", se = FALSE) +  # Trend line
  labs(
    title = "Interactive Depression Rates Among College Students by Age",
    x = "Age",
    y = "Depression Rate"
  ) +
  theme_minimal()

# Convert ggplot to an interactive plot
interactive_plot <- ggplotly(p)

# Display the interactive plot
interactive_plot
