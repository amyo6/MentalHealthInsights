library("dplyr")
library("tidyverse")
library("ggplot2") 
library("plotly")
america <- read.csv("/Users/hallehwang/Desktop/Healthcare-related/depression_data.csv")

america$Age_Group <- cut(
  america$Age,
  breaks = c(0, 25, 35, 45, 55, 65, Inf),
  labels = c("18-25", "26-35", "36-45", "46-55", "56-65", "65+"), 
  right = FALSE 
)
america$Does_Smoke <- ifelse(america$Smoking.Status %in% c("Smoker", "Former"), 1, 0)
america$Low_Physical_Activity <- ifelse(america$Physical.Activity.Level == "Sedentary", 1, 0)
america$Unhealthy_Diet <- ifelse(america$Dietary.Habits == "Unhealthy", 1, 0)
america$Unhealthy_Sleep <- ifelse(america$Sleep.Patterns %in% c("Fair", "Poor"), 1, 0)

america$Unhealthy_Habit_Score <- rowSums(america[, c("Does_Smoke", "Low_Physical_Activity", "Unhealthy_Diet", "Unhealthy_Sleep")])

data_long <- america %>%
  gather(key = "Habit", value = "Unhealthy", 
         Does_Smoke, Low_Physical_Activity, Unhealthy_Diet, Unhealthy_Sleep)

avg_habits <- data_long %>%
  group_by(Age_Group, Habit) %>%
  summarize(Average_Unhealthy = mean(Unhealthy))

lifestyle_plot <- ggplot(avg_habits, aes(x = Age_Group, y = Average_Unhealthy, fill = Habit)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(limits = c(0,1)) + 
  labs(
    title = "Unhealthy Lifestyle Habits Across Age Groups",
    x = "Age Group",
    y = "Average Unhealthy Habit (Proportion)",
    color = "Habit"
  ) + theme(axis.text.x = element_text(angle = 45, hjust=1, size=6))

ggplotly(lifestyle_plot)
