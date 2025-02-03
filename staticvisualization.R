library(tidycensus)
library(tidyverse)
library(ggplot2)
library(sf)
library(ggmap)
library(dplyr)
library(stringr)
library(mapview)
library(leaflet)
install.packages("leaflet.extras")
library(leaflet.extras)
library(ggplot2)
install.packages("reshape2")
library(reshape2)
install.packages("plotly")
library(plotly)




# Loading the dataset
mapstuff <- read_csv("/Users/amyofor/Downloads/places.csv", show_col_types = FALSE)

view(mapstuff)

#lol checking something 
colnames(mapstuff)

# okay new and improved df
new_map <- mapstuff %>%
  select(ZCTA5, TotalPop18plus, Geolocation, DEPRESSION_CrudePrev, HOUSINSECU_CrudePrev)

view(new_map)

#exclude like the outer regions of the us bc it wont show on map also add in a new column--state!
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
      TRUE ~ "Unknown"
    )
  )

# View the updated dataframe
view(filtered_map)

filtered_map <- filtered_map %>%
  select(ZCTA5, state, TotalPop18plus, DEPRESSION_CrudePrev)

view(filtered_map)

filtered_map <- filtered_map %>%
  filter(state != "Unknown")



#visualization
ggplot(filtered_map, aes(x = DEPRESSION_CrudePrev , y = state, fill = TotalPop18plus )) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(
    x = "Depression Rate",  
    y = "State",          
    fill = "Population (18+)"    
  ) +
  theme_minimal()
