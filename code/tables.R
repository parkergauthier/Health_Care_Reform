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


df_sc <- read_csv(here("data", "sc_data.csv"),
                  show_col_types = F) %>% 
  select(Mort_rate, Unemp_Rate,Poverty_rate,Uninsured_pct)

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
panel_ui <- stats_ui

panel_ui %>%
  mutate(Variable = str_replace_all(Variable, "_", " "),
         Variable = str_to_title(Variable)) %>% 
  kbl(
    caption = "Main Variables Summary",
    booktabs = T,
    format = 'latex',
    label = 'tab:replicatetable2a'
  ) %>%
  kable_styling(latex_options = "HOLD_position") %>%
  write_lines(here("tables", "rep_table_ui.tex"))


panel_ue <- stats_ue

panel_ue %>%
  mutate(Variable = str_replace_all(Variable, "_", " "),
         Variable = str_to_title(Variable)) %>% 
  kbl(
    caption = "Main Variables Summary",
    booktabs = T,
    format = 'latex',
    label = 'tab:replicatetable2a'
  ) %>%
  kable_styling(latex_options = "HOLD_position") %>%
  write_lines(here("tables", "rep_table_ue.tex"))

panel_sc <- stats_sc

panel_sc %>%
  mutate(Variable = str_replace_all(Variable, "_", " "),
         Variable = str_to_title(Variable)) %>% 
  kbl(
    caption = "Main Variables Summary",
    booktabs = T,
    format = 'latex',
    label = 'tab:replicatetable2a'
  ) %>%
  kable_styling(latex_options = "HOLD_position") %>%
  write_lines(here("tables", "rep_table_sc.tex"))