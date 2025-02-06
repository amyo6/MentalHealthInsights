library(shiny)
library(ggplot2)
library(dplyr)
library(leaflet)
library(corrplot)
library(plotly)
library(tidyverse)

# Load datasets (replace this with my file path later)
mapstuff <- read_csv("data/places.csv", show_col_types = FALSE)
df <- read_csv("data/student_mental_health.csv", show_col_types = FALSE)
df_mxmh <- read_csv("data/mxmh_survey_results.csv", show_col_types = FALSE)
df_marriage <- read_csv("data/depression_data.csv", show_col_types = FALSE)
america <- read_csv("data/depression_data.csv", show_col_types = FALSE)

# Prepare the dataset for visualization 1
new_map <- mapstuff %>%
  select(ZCTA5, TotalPop18plus, Geolocation, DEPRESSION_CrudePrev, HOUSINSECU_CrudePrev)%>%
  rename(
    Depression_Rate = DEPRESSION_CrudePrev,
    Housing_Insecurity_Rate = HOUSINSECU_CrudePrev
  )


#preparing dataset again 
filtered_map <- new_map %>%
  filter(!(ZCTA5 %in% c(
    sprintf("%05d", 2801:2921),   # Rhode Island (02801–02921)
    sprintf("%05d", 96700:96899), # Hawaii (96700–96899)
    sprintf("%05d", 600:799),     # Puerto Rico (00600–00799)
    sprintf("%05d", 900:999),     # Puerto Rico (00900–00999)
    "99501", "99502", "99503", "99504", "99505", "99506", "99507", "99508", "99515", "99516" # Alaska
  ))) %>%
  mutate(
    state = case_when(
      ZCTA5 >= "90001" & ZCTA5 <= "96162" ~ "California",
      ZCTA5 >= "35004" & ZCTA5 <= "36925" ~ "Alabama",
      ZCTA5 >= "99501" & ZCTA5 <= "99950" ~ "Alaska",
      ZCTA5 >= "71601" & ZCTA5 <= "72959" ~ "Arkansas",
      ZCTA5 >= "85001" & ZCTA5 <= "86556" ~ "Arizona",
      ZCTA5 >= "80001" & ZCTA5 <= "81658" ~ "Colorado",
      ZCTA5 >= "06001" & ZCTA5 <= "06928" ~ "Connecticut",
      ZCTA5 >= "19701" & ZCTA5 <= "19980" ~ "Delaware",
      ZCTA5 >= "32003" & ZCTA5 <= "34997" ~ "Florida",
      ZCTA5 >= "30002" & ZCTA5 <= "31999" ~ "Georgia",
      ZCTA5 >= "50001" & ZCTA5 <= "52809" ~ "Iowa",
      ZCTA5 >= "83201" & ZCTA5 <= "83876" ~ "Idaho",
      ZCTA5 >= "60001" & ZCTA5 <= "62999" ~ "Illinois",
      ZCTA5 >= "46001" & ZCTA5 <= "47997" ~ "Indiana",
      ZCTA5 >= "66002" & ZCTA5 <= "67954" ~ "Kansas",
      ZCTA5 >= "40003" & ZCTA5 <= "42788" ~ "Kentucky",
      ZCTA5 >= "70001" & ZCTA5 <= "71497" ~ "Louisiana",
      ZCTA5 >= "01001" & ZCTA5 <= "02791" ~ "Massachusetts",
      ZCTA5 >= "48001" & ZCTA5 <= "49971" ~ "Michigan",
      ZCTA5 >= "55001" & ZCTA5 <= "56763" ~ "Minnesota",
      ZCTA5 >= "63001" & ZCTA5 <= "65899" ~ "Missouri",
      ZCTA5 >= "59001" & ZCTA5 <= "59937" ~ "Montana",
      ZCTA5 >= "27006" & ZCTA5 <= "28909" ~ "North Carolina",
      ZCTA5 >= "58001" & ZCTA5 <= "58856" ~ "North Dakota",
      ZCTA5 >= "68001" & ZCTA5 <= "69367" ~ "Nebraska",
      ZCTA5 >= "03031" & ZCTA5 <= "03897" ~ "New Hampshire",
      ZCTA5 >= "07001" & ZCTA5 <= "08989" ~ "New Jersey",
      ZCTA5 >= "87001" & ZCTA5 <= "88441" ~ "New Mexico",
      ZCTA5 >= "10001" & ZCTA5 <= "14925" ~ "New York",
      ZCTA5 >= "43001" & ZCTA5 <= "45999" ~ "Ohio",
      ZCTA5 >= "73001" & ZCTA5 <= "74966" ~ "Oklahoma",
      ZCTA5 >= "97001" & ZCTA5 <= "97920" ~ "Oregon",
      ZCTA5 >= "15001" & ZCTA5 <= "19640" ~ "Pennsylvania",
      ZCTA5 >= "29001" & ZCTA5 <= "29948" ~ "South Carolina",
      ZCTA5 >= "57001" & ZCTA5 <= "57799" ~ "South Dakota",
      ZCTA5 >= "37010" & ZCTA5 <= "38589" ~ "Tennessee",
      ZCTA5 >= "73301" & ZCTA5 <= "88595" ~ "Texas",
      ZCTA5 >= "84001" & ZCTA5 <= "84784" ~ "Utah",
      ZCTA5 >= "20001" & ZCTA5 <= "20599" ~ "Washington D.C.",
      ZCTA5 >= "98001" & ZCTA5 <= "99403" ~ "Washington",
      ZCTA5 >= "53001" & ZCTA5 <= "54990" ~ "Wisconsin",
      ZCTA5 >= "82001" & ZCTA5 <= "83128" ~ "Wyoming",
      TRUE ~ NA_character_ 
    )
  ) %>%
  filter(!is.na(state)) 


#prepare the dataset for visualization 2

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

#prepare the dataset for visualization 3

# str(df_mxmh)
# head(df_mxmh)

# mapping for categorical frequency responses
frequency_mapping <- c("Never" = 0, "Rarely" = 1, "Sometimes" = 2, "Very frequently" = 3)

# frequency columns and apply the mapping
freq_cols <- grep("Frequency", names(df_mxmh), value = TRUE)
df_mxmh[freq_cols] <- lapply(df_mxmh[freq_cols], function(x) frequency_mapping[x])

# convert frequency columns to numeric
df_mxmh[freq_cols] <- lapply(df_mxmh[freq_cols], as.numeric)

# select Relevant columns for correlation Analysis
correlation_columns <- c("Hours per day", "Anxiety", "Depression", "Insomnia", "OCD", freq_cols)
df_corr <- df_mxmh[correlation_columns]  # Create a subset

# make numeric
df_corr <- mutate_all(df_corr, as.numeric)

# compute the correlation matrix
cor_matrix <- cor(df_corr, use = "complete.obs")  # Compute correlation matrix

#prepare the dataset for visualization 4 
summary(df_marriage)

depression <- df_marriage %>%
  select("Age","Employment Status","Income", "Number of Children", "Marital Status", "Family History of Depression"
  )

summary(depression)

depression <- depression %>%
  rename(employment = "Employment Status",
         marriage = "Marital Status", 
         num_children = "Number of Children", 
         fam_depression = "Family History of Depression"
  )

depression <- na.omit(depression)

#prepare the dataset for visualization 5
america$Age_Group <- cut(
  america$Age,
  breaks = c(0, 25, 35, 45, 55, 65, Inf),
  labels = c("18-25", "26-35", "36-45", "46-55", "56-65", "65+"), 
  right = FALSE 
)
america$Does_Smoke <- ifelse(america[["Smoking Status"]] %in% c("Smoker", "Former"), 1, 0)
america$Low_Physical_Activity <- ifelse(america[["Physical Activity Level"]] == "Sedentary", 1, 0)
america$Unhealthy_Diet <- ifelse(america[["Dietary Habits"]] == "Unhealthy", 1, 0)
america$Unhealthy_Sleep <- ifelse(america[["Sleep Patterns"]] %in% c("Fair", "Poor"), 1, 0)

america$Unhealthy_Habit_Score <- rowSums(america[, c("Does_Smoke", "Low_Physical_Activity", "Unhealthy_Diet", "Unhealthy_Sleep")])

data_long <- america %>%
  gather(key = "Habit", value = "Unhealthy", 
         Does_Smoke, Low_Physical_Activity, Unhealthy_Diet, Unhealthy_Sleep)

avg_habits <- data_long %>%
  group_by(Age_Group, Habit) %>%
  summarize(Average_Unhealthy = mean(Unhealthy))

# Define Server
server <- function(input, output) {
  
  # Heatmap visualization
  output$heatmapPlot <- renderPlot({
    ggplot(filtered_map, aes_string(x = input$variable_select, y = "state", fill = "TotalPop18plus")) +
      geom_tile() +
      scale_fill_gradient(low = "blue", high = "red") +
      labs(
        x = input$variable_select,  
        y = "State",          
        fill = "Population (18+)"    
      ) +
      theme_minimal()
  })
  
  # Interactive Depression by Age Plot
  output$interactiveDepressionPlot <- renderPlotly({
    p <- ggplot(depression_by_age, aes(x = Age, y = Depression_Rate)) +
      geom_point(color = "blue", alpha = 0.6) +  # Scatter points
      geom_smooth(method = "loess", color = "red", se = FALSE) +  # Trend line
      labs(
        title = "Interactive Depression Rates Among College Students by Age",
        x = "Age",
        y = "Depression Rate"
      ) +
      theme_minimal()
    
    ggplotly(p)  # Convert to interactive plot
  })
  
  output$correlationHeatmap <- renderPlot({
    corrplot(cor_matrix, method = "color", type = "upper", 
             col = colorRampPalette(c("blue", "white", "red"))(200), 
             tl.cex = 0.8, tl.col = "black", 
             addCoef.col = "black", number.cex = 0.7,
             title = "Correlation Heatmap: Music Listening & Mental Health",
             mar = c(0,0,1,0))
  })
  
  # Interactive Marriage & Family Depression Plot
  output$marriagePlot <- renderPlotly({
    marriage_plot <- ggplot(depression, aes(x = marriage, fill = fam_depression)) +
      geom_bar(position = "dodge") +
      labs(title = "Marriage and Family History of Depression", 
           x = "Marital Status", 
           y = "Count", 
           fill = "Family Depression History") +
      scale_fill_manual(values = c("No" = "blue", "Yes" = "red")) +
      theme_minimal()
    
    ggplotly(marriage_plot)
  })
  
  # Interactive Lifestyle Habits Plot
  output$lifestylePlot <- renderPlotly({
    lifestyle_plot <- ggplot(avg_habits, aes(x = Age_Group, y = Average_Unhealthy, fill = Habit)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_y_continuous(limits = c(0,1)) + 
      scale_fill_manual(values = c("red", "blue", "grey", "black")) +
      labs(
        title = "Unhealthy Lifestyle Habits Across Age Groups",
        x = "Age Group",
        y = "Average Unhealthy Habit (Proportion)",
        fill = "Habit"
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust=1, size=6))
    
    ggplotly(lifestyle_plot)
  })
  
  
}
