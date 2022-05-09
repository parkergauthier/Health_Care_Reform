library(tidyverse)
mortality_1 <- read_tsv("https://raw.githubusercontent.com/parkergauthier/Health_Care_Reform/main/Compressed_Mortality1to20.txt")
mortality_2 <- read_tsv("https://raw.githubusercontent.com/parkergauthier/Health_Care_Reform/main/Compressed_Mortality21to30.txt")
mortality_3 <- read_tsv("https://raw.githubusercontent.com/parkergauthier/Health_Care_Reform/main/Compressed_Mortality31to45.txt")
mortality_4 <- read_tsv("https://raw.githubusercontent.com/parkergauthier/Health_Care_Reform/main/Compressed_Mortality46to56.txt")

df <- rbind(mortality_1, mortality_2, mortality_3, mortality_4) %>%
    select(df, -c(Notes)) %>% 
    drop_na()
