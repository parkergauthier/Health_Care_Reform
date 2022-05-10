load.libraries <- c('tidyverse')
install.lib <- load.libraries[!load.libraries %in% installed.packages()]
for(libs in install.lib) install.packages(libs, dependences = TRUE)
sapply(load.libraries, require, character = TRUE)

mortality_1 <- read_tsv("https://raw.githubusercontent.com/parkergauthier/Health_Care_Reform/main/Compressed_Mortality1to20.txt")
mortality_2 <- read_tsv("https://raw.githubusercontent.com/parkergauthier/Health_Care_Reform/main/Compressed_Mortality21to30.txt")
mortality_3 <- read_tsv("https://raw.githubusercontent.com/parkergauthier/Health_Care_Reform/main/Compressed_Mortality31to45.txt")
mortality_4 <- read_tsv("https://raw.githubusercontent.com/parkergauthier/Health_Care_Reform/main/Compressed_Mortality46to56.txt")

#rbind all the data
df <- rbind(mortality_1, mortality_2, mortality_3, mortality_4) %>%
    select(df, -c(Notes)) %>% 
    drop_na()

#cast some columns to factor
cols <- c("State", "State.Code")
df[cols] <- lapply(df[cols], factor)

#replaces the blanks in the column names with a dot
names(df) <- make.names(names(df), unique=TRUE)

#remove leading 0s in State.Code column
df$State.Code <- sub("^0+", "", df$State.Code)

#rename column
names(df)[3] <- "ICD.Chapter"
names(df)[4] <- "ICD.Chapter.Code"
