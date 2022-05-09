library(tidyverse)
library(readr)

#read in data
CDC_20to64_State <- read_csv("data/CDC_Mortality_State_ICD_20To64_RACE_YEAR.csv")
names(CDC_20to64_State) = make.names(colnames(CDC_20to64_State))

#preliminary cleaning strings and oddities
sapply(CDC_20to64_State, class)
' (Unreliable)'
CDC_Corrected = CDC_20to64_State %>%
  mutate(Crude.Rate =  ifelse(
    substr(Crude.Rate, nchar(Crude.Rate)-12, nchar(Crude.Rate)) == ' (Unreliable)',
    (substr(Crude.Rate, 1, nchar(Crude.Rate)-12)),(Crude.Rate)
  )) %>%
  mutate(Crude.Rate = as.numeric(Crude.Rate)) %>%
  mutate(
    X..of.Total.Deaths = substr(X..of.Total.Deaths,1,nchar(X..of.Total.Deaths)-1)
  ) %>%
  mutate(X..of.Total.Deaths = as.numeric(X..of.Total.Deaths))
 
sapply(CDC_Corrected, class)

# Pivot Wider
pivoted <- CDC_Corrected %>% 
  pivot_wider(id_cols = c("State", "Race", "Year", "Population"), 
              names_from = "ICD.Chapter", 
              values_from = Deaths)

df = CDC_Corrected %>%
  filter(Year==2000 & Race=='White')
