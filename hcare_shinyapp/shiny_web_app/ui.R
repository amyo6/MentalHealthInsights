library(shiny)
library(ggplot2)
library(dplyr)
library(leaflet)
library(corrplot)
library(plotly)
library(tidyverse)


# Define UI
ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      body {
      font-family: 'Helvetica';
      background-color: #e3f2fd;  
      }
      
      .nav-tabs > li > a {
        background-color: #1565c0;  
        color: white;
        font-weight: bold;
        border: 1px solid #1565c0;
        border-bottom: none;
        border-radius: 8px 8px 0 0;
        margin-right: 5px;
      }
      
      .nav-tabs > li.active > a, 
      .nav-tabs > li.active > a:focus, 
      .nav-tabs > li.active > a:hover {
        background-color: white;  
        color: #1565c0; 
        border: 1px solid #1565c0;
        border-bottom: 1px solid white;
      }
      
      .nav-tabs > li > a:hover {
        background-color: #1e88e5; 
        color: white;
      }
      
      .tab-content {
        background-color: white;
        padding: 20px;
        border: 1px solid #1565c0;
        border-radius: 0 0 8px 8px;
      }
      
      h3, h4, p, li {
        color: #0d47a1;
      }
      
      .well, .panel {
        background-color: #e3f2fd !important;
        border: none;
        box-shadow: 0 2px 5px rgba(0,0,0,0.1);
      }
      
      .btn, .selectize-input {
        background-color: #1565c0 !important;
        color: white !important;
        border: none;
      }
      
      .btn:hover, .selectize-input:hover {
        background-color: #0d47a1 !important;
      }
    "))
  ),
  
  
  titlePanel("US Data Visualizations"),
  
  tabsetPanel(
    
    tabPanel("Introduction",
             fluidRow(
               column(12, 
                      h3("Introduction"),
                      p("For our project, we aimed to explore depression patterns within the United States, with a focus on raising awareness among students and young adults. 
            Mental health, particularly depression, is a growing concern that affects individuals across all age groups, but young adults—especially college students—are at a crucial stage where decisions around factors such as physical activity, diet, relationships, and coping mechanisms can significantly shape their mental well-being."),
                      p("We present a series of visualizations, including plots on college student depression, lifestyle habits, marriage, the connection between music and mental health, and US-wide distribution of depression rates. 
            Our goal is to inform young adults about the various factors related to depression in the United States and encourage proactive steps to safeguard their mental health."),
                      tags$img(src = "https://substackcdn.com/image/fetch/f_auto,q_auto:good,fl_progressive:steep/https%3A%2F%2Fsubstack-post-media.s3.amazonaws.com%2Fpublic%2Fimages%2Fa984c250-26fc-42ea-a82f-eab67a7f828e_1200x675.jpeg",
                               width = "75%", height = "75%"),
                      tags$p("The Connell Waldron from Normal People uni experience"),
                      h4("We chose to explore the following questions:"),
                      tags$ul(
                        tags$li("What trends in lifestyle habits are observed in individuals suffering from depression?"),
                        tags$li("Where are depression rates most prevalent in the United States? What potential external factors influence those rates?"),
                        tags$li("What is the relationship between depression, marital status, and the number of children?"),
                        tags$li("What are the depression rates among college students?"),
                        tags$li("How are music and mental health related?")
                        )
               )
             )
    ),
    
    
    
    
    tabPanel("Depression & Housing",
             sidebarLayout(
               sidebarPanel(
                 selectInput("variable_select", "Choose a Variable:", 
                             choices = c("Depression Rate" = "Depression_Rate", 
                                         "Housing Insecurity Rate" = "Housing_Insecurity_Rate"),
                             selected = "Depression_Rate"),
                 helpText("This visualization presents depression rates among adults in the United States, 
                 based on data from the U.S. Department of Health & Human Services. It excludes data from 
                 non-contiguous states such as Hawaii, Puerto Rico, and Rhode Island. Depression rates 
                 represent the estimated crude prevalence of depression among adults in 2022, while 
                 housing insecurity rates reflect the estimated crude prevalence of housing 
                 insecurity within the past 12 months. Users can interact with the data 
                 by exploring both depression and housing insecurity rates, allowing them to 
                 examine relationships between these variables across different states. This interactive 
                 approach helps identify trends in mental health and potential economic crises, offering 
                 insights into how depression may be influenced by housing instability.")
               ),
               
               
               mainPanel(
                 plotOutput("heatmapPlot"),
                 leafletOutput("mapPlot", height = "500px")
               )
             )),
    
    tabPanel("Depression by Age",
             sidebarLayout(
               sidebarPanel(
                 helpText("This visualization displays depression rates among college students in the USA, based on self-reported survey data. 
The dataset includes students age, year of study, and depression status. Depression rates are calculated as the percentage of students who reported experiencing depression at each age. 
The interactive plot allows users to explore trends, with options to view the data as a scatter plot or line plot. 
This analysis helps highlight patterns in mental health among students, offering insights into age-related trends in depression.")
               ),
               mainPanel(
                 plotlyOutput("interactiveDepressionPlot")  # Output for interactive plot
               )
             )),
    
    tabPanel("Correlation Heatmap",
             sidebarLayout(
               sidebarPanel(
                 helpText("This project explores the connection between music listening habits and mental health, focusing on 
                 anxiety, depression, insomnia, and OCD. Using survey responses from 736 participants, we analyzed how factors 
                 like favorite genres, listening hours, and streaming platforms relate to mental well-being. The data was 
                 collected from Reddit, Discord, and social media platforms, offering a diverse but potentially skewed sample. After 
                 preparing the data by converting listening frequencies into numerical values and handling missing entries, we computed 
                 correlations and visualized them in a heatmap. The results revealed weak but interesting trends such as a slight 
                 positive correlation between listening hours and insomnia or anxiety. Certain genres, like Metal and Rock, 
                 showed minor links to mental health metrics, while anxiety and depression had a stronger correlation with 
                 each other. While the dataset isn’t strong enough for definitive conclusions, 
                it provides a solid foundation for exploring how music impacts mental health. 
                This repository includes the dataset, R script, and heatmap, making replicating 
                or building on this work easy. It’s a small but meaningful step in understanding 
                the relationship between music and mental well-being.")
               ),
               mainPanel(
                 plotOutput("correlationHeatmap")  
               )
             )),
    
    tabPanel("Marriage & Family Depression",
             sidebarLayout(
               sidebarPanel(
                 helpText("The analysis of depression rates across different marital statuses reveals significant patterns. Widowed individuals exhibit the highest depression rate at 50%, 
                 highlighting the emotional impact of losing a spouse. Married and divorced individuals show similar depression rates of approximately 22.85% and 22.73%, respectively, 
                 suggesting that marriage does not necessarily serve as a protective factor against depression. In comparison, single individuals experience the lowest depression rate at 20.15%, which may indicate fewer relationship-related stressors. 
                These results suggest that while marital status influences depression risk, there are other factors such as grief, emotional support, and individual circumstances likely play a crucial role in mental well-being.")
               ),
               mainPanel(
                 plotlyOutput("marriagePlot")  
               )
             )),
    
    tabPanel("Unhealthy Lifestyle Habits",
             sidebarLayout(
               sidebarPanel(
                 helpText("The bar plot illustrates the prevalence of unhealthy lifestyle habits among individuals suffering from 
                 depression, segmented by various age groups. These habits include smoking, low physical activity, unhealthy diet, and poor sleep. 
                 This visualization shows the distribution of each habit, spotlighting key trends and allowing for easy comparison of their prevalence 
                 across different ages. Notably, poor sleep is a consistent issue across all age groups, 
                while low physical activity becomes more prevalent as age increases. Unhealthy diet 
                is most prominent in younger and older age groups, with smoking showing a significant 
                rise in prevalence between the 18-25 and 36-45 age ranges before leveling off in older age groups.")
               ),
               mainPanel(
                 plotlyOutput("lifestylePlot")  
               )
             ))
    
  )
)
