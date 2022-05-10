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
  mutate(X..of.Total.Deaths = as.numeric(X..of.Total.Deaths)) %>%
  filter(Year >= 2000)

## FEATURES FOR SC 
# state level unemployment 
unemp = read.csv("data/state-unemployment.csv") %>% 
  pivot_longer(cols = X2000:X2016) %>%
  mutate(Year = substring(name, 2)) %>%
  select(Area, Year, value)
colnames(unemp) = c("State", "Year", "Unemp_Rate")

# state age-sex data 2000 - 2010 (Census intercensal estimates)
state_agesex_2010 = read.csv("data/state-agesex-2000-2010.csv") %>% data.frame

# state demos racee 2000 - 2010 (Census intercensal)
state_race_2010 = read.csv("data/state-race-2000-2010.csv") %>% 
  data.frame %>%
  filter(NAME != "United States" & RACE != 0) %>% 
  select(-STATE)
colnames(state_race_2010) = c("State", "Race", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010")

state_race_2010_long = state_race_2010 %>% pivot_longer("2000":"2010") %>%
  pivot_wider(names_from = Race, names_prefix = "Race_", values_from = "value") %>%
  rename(
    Year = name,
    Race_White = Race_1,
    Race_Black = Race_2, 
    Race_AmInd = Race_3,
    Race_Asian = Race_4,
    Race_PacIsl = Race_5,
    Race_Multi = Race_6
  )

c1 = merge(unemp, state_agesex_2010)
c2 = merge(c1, state_race_2010_long)


df = CDC_Corrected %>%
  filter(Year==2000 & Race=='White')
