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
# state population (Census) - for scaling
state_pop = read.csv("data/state-pops-2000-2010.csv") %>%
  data.frame %>%
  pivot_longer(X2000:X2010) %>%
  mutate(name = recode(name,
    X2000 = "2000",
    X2001 = "2001",
    X2002 = "2002",
    X2003 = "2003",
    X2004 = "2004",
    X2005 = "2005",
    X2006 = "2006",
    X2007 = "2007",
    X2008 = "2008",
    X2009 = "2009",
    X2010 = "2010",
  ))
colnames(state_pop) = c("State", "Year", "Population")

# state level unemployment 
unemp = read.csv("data/state-unemployment.csv") %>% 
  pivot_longer(cols = X2000:X2016) %>%
  mutate(Year = substring(name, 2)) %>%
  select(Area, Year, value)
colnames(unemp) = c("State", "Year", "Unemp_Rate")

# state age-sex data 2000 - 2010 (Census intercensal estimates)
state_agesex = read.csv("data/state-agesex-2000-2010.csv") %>% data.frame
state_agesexpop = merge(state_agesex, state_pop)
state_agesexpop[,3:23] = sapply(state_agesexpop[,3:23], as.numeric)
state_agesex_clean = state_agesexpop %>%
  mutate(Age_20to34 = (X.20.to.24.years + X.25.to.29.years + X.30.to.34.years) / Population * 100,
         Age_35to44 = (X.35.to.39.years + X.40.to.44.years) / Population * 100,
         Age_45to54 = (X.45.to.49.years + X.50.to.54.years) / Population * 100,
         Age_55to64 = (X.55.to.59.years + X.60.to.64.years) / Population * 100,
         Male_pct = MALE / Population * 100,
         Female_pct = FEMALE / Population * 100) %>%
  select(State, Year, Age_20to34, Age_35to44, Age_45to54, Age_55to64, Male_pct, Female_pct)

# state demos race 2000 - 2010 (Census intercensal)
state_race = read.csv("data/state-race-2000-2010.csv") %>% 
  data.frame %>%
  filter(NAME != "United States" & RACE != 0) %>% 
  select(-STATE)
colnames(state_race) = c("State", "Race", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010")

state_race_clean = state_race %>% pivot_longer("2000":"2010") %>%
  pivot_wider(names_from = Race, names_prefix = "Race_", values_from = "value") %>%
  rename(
    Year = name,
    Race_White = Race_1,
    Race_Black = Race_2, 
    Race_AmInd = Race_3,
    Race_Asian = Race_4,
    Race_PacIsl = Race_5,
    Race_Multi = Race_6) %>% 
  merge(state_pop) %>%
  mutate(
    Race_White = Race_White / Population * 100,
    Race_Black = Race_Black / Population * 100,
    Race_AmInd = Race_AmInd / Population * 100,
    Race_Asian = Race_Asian / Population * 100,
    Race_PacIsl = Race_PacIsl / Population * 100,
    Race_Multi = Race_Multi / Population * 100,) %>% 
  select(-Population)

state_hhinc = read.csv("data/state-hhinc-2000-2010.csv") %>%
  data.frame %>%
  filter(State != "United States") %>%
  pivot_longer(X2000:X2010) %>%
  mutate(name = recode(name,
                       X2000 = "2000",
                       X2001 = "2001",
                       X2002 = "2002",
                       X2003 = "2003",
                       X2004 = "2004",
                       X2005 = "2005",
                       X2006 = "2006",
                       X2007 = "2007",
                       X2008 = "2008",
                       X2009 = "2009",
                       X2010 = "2010",
  ))
colnames(state_hhinc) = c("State", "Year", "HH_Income")

# state poverty rate (Census) 
state_poverty = read.csv("data/state-poverty-2000-2010.csv")

# state uninsured rate for population under 65 years (CPS)
state_uninsured = read.csv("data/state-uninsured.csv") 

# merge all data
c1 = merge(unemp, state_agesex_clean)
c2 = merge(c1, state_race_clean)
c3 = merge(c2, state_hhinc)
c4 = merge(c3, state_poverty)
c5 = merge(c4, state_uninsured)

# state mortality rates
mort_rate = read.csv("data/state-mortality-2000-2010.csv")

# merged data for synthetic control
sc_data = merge(c5, mort_rate) %>% select(-Deaths, -Population) %>% select(State, Year, crudemort_rate, Mort_rate, everything())
write.csv(sc_data, file = "data/sc_data.csv")

