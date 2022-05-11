gc()
rm(list = ls())

df_ui <- read_csv(here("data", "state-uninsured.csv"),
               show_col_types = F) %>% 
  select(Uninsured_pct)

ns <- df_ui %>% summarise(across(everything(), ~sum(!is.na(.x))))
means <- df_ui %>% summarise(across(everything(), ~mean(.x, na.rm=T))) %>% round(3)
sds <- df_ui %>% summarise(across(everything(), ~sd(.x, na.rm=T))) %>% round(3)

stats_ui <- list(ns, means, sds) %>% 
  do.call("rbind", .) %>% 
  t() %>%
  as.data.frame %>% 
  rownames_to_column()

colnames(stats_ui) <- c("Variable",
                     "N",
                     "Mean", 
                      "Standard Deviation")

df_ue <- read_csv(here("data", "state-unemployment.csv"),
               show_col_types = F) %>% 
  select(Uninsured_pct)


ns <- df_ue %>% summarise(across(everything(), ~sum(!is.na(.x))))
means <- df_ue %>% summarise(across(everything(), ~mean(.x, na.rm=T))) %>% round(3)
sds <- df_ue %>% summarise(across(everything(), ~sd(.x, na.rm=T))) %>% round(3)

stats_ue <- list(ns, means, sds) %>% 
  do.call("rbind", .) %>% 
  t() %>%
  as.data.frame %>% 
  rownames_to_column()

colnames(stats_ue) <- c("Variable",
                     "N",
                     "Mean", 
                     "Standard Deviation")


df_sc <- read_csv(here("data", "sc-data.csv"),
                  show_col_types = F) %>% 
  select("2000":"2010")

ns <- df_sc %>% summarise(across(everything(), ~sum(!is.na(.x))))
means <- df_sc %>% summarise(across(everything(), ~mean(.x, na.rm=T))) %>% round(3)
sds <- df_sc %>% summarise(across(everything(), ~sd(.x, na.rm=T))) %>% round(3)

stats_sc <- list(ns, means, sds) %>% 
  do.call("rbind", .) %>% 
  t() %>%
  as.data.frame %>% 
  rownames_to_column()

colnames(stats_sc) <- c("Variable",
                        "N",
                        "Mean", 
                        "Standard Deviation")
 

