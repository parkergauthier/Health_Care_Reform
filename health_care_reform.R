library(tidyverse)
mortality_1 <- read_tsv("/Users/jirapat/Desktop/Compressed_Mortality1to20.txt")
mortality_2 <- read_tsv("/Users/jirapat/Desktop/Compressed_Mortality21to30.txt")
mortality_3 <- read_tsv("/Users/jirapat/Desktop/Compressed_Mortality31to45.txt")
mortality_4 <- read_tsv("/Users/jirapat/Desktop/Compressed_Mortality46to56.txt")

df <- rbind(mortality_1, mortality_2, mortality_3, mortality_4)
df <- select(df, -c(Notes))
