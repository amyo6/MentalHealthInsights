# Install and load libraries
install.packages("corrplot")      # Install corrplot for visualization
install.packages("dplyr")         # Install dplyr for data manipulation
install.packages("tidyverse")     # Install tidyverse for comprehensive tools


library(corrplot)
library(dplyr)
library(tidyverse)

# dataset

df <- read.csv("~/Desktop/healthcare-related code/mxmh_survey_results.csv", stringsAsFactors = FALSE)

# view data
str(df)
head(df)

# mapping for categorical frequency responses
frequency_mapping <- c("Never" = 0, "Rarely" = 1, "Sometimes" = 2, "Very frequently" = 3)

# frequency columns and apply the mapping
freq_cols <- grep("Frequency", names(df), value = TRUE)
df[freq_cols] <- lapply(df[freq_cols], function(x) frequency_mapping[x])

# convert frequency columns to numeric
df[freq_cols] <- lapply(df[freq_cols], as.numeric)

# select Relevant columns for correlation Analysis
correlation_columns <- c("Hours.per.day", "Anxiety", "Depression", "Insomnia", "OCD", freq_cols)
df_corr <- df[correlation_columns]  # Create a subset

# make numeric
df_corr <- mutate_all(df_corr, as.numeric)

# compute the correlation matrix
cor_matrix <- cor(df_corr, use = "complete.obs")  # Compute correlation matrix

# heatmap
corrplot(cor_matrix, method = "color", type = "upper", 
         col = colorRampPalette(c("blue", "white", "red"))(200), 
         tl.cex = 0.8, tl.col = "black", 
         addCoef.col = "black", number.cex = 0.7,
         title = "Correlation Heatmap: Music Listening & Mental Health",
         mar = c(0,0,1,0))

