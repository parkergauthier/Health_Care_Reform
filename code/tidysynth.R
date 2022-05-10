library(tidysynth)

sc_data = read.csv("data/sc_data.csv") %>% select(-X, -crudemort_rate)
sc_data[,2:19] = sapply(sc_data[,2:19], as.numeric)
sc_tbl = sc_data %>% as_tibble()

sc_out = sc_tbl %>%
  # initial the synthetic control object
  synthetic_control(outcome = Mort_rate,
                    unit = State, 
                    time = Year,
                    i_unit = "Massachusetts",
                    i_time = 2006,
                    generate_placebos = T) %>%
  # Generate the aggregate predictors used to fit the weights
  generate_predictor(time_window = 2000:2006,
                     Unemp_rate = mean(Unemp_Rate, na.rm = T),
                     Age_20to34 = mean(Age_20to34, na.rm = T),
                     Age_35to44 = mean(Age_35to44, na.rm = T),
                     Age_45to54 = mean(Age_45to54, na.rm = T),
                     Age_55to64 = mean(Age_55to64, na.rm = T),
                     Male_pct = mean(Male_pct, na.rm = T),
                     Female_pct = mean(Female_pct, na.rm = T),
                     Hisp = mean(Hisp, na.rm = T),
                     Race_White = mean(Race_White, na.rm = T),
                     Race_Black = mean(Race_Black, na.rm = T),
                     Race_Other = mean(Race_Other, na.rm = T),
                     HH_Income = mean(HH_Income, na.rm = T),
                     Poverty_rate = mean(Poverty_rate, na.rm = T),
                     Uninsured_pct = mean(Uninsured_pct, na.rm = T)) %>%
  # average mortality rate in the donor pool from 2000 - 2006
  generate_predictor(time_window = 2000:2006,
                     mort_rate = mean(Mort_rate, na.rm = T)) %>%
  # Generate the fitted weights for the synthetic control
  generate_weights(optimization_window = 2000:2006, # time to use in the optimization task
                   margin_ipop = .02,sigf_ipop = 7,bound_ipop = 6) %>% # optimizer options
  
  # Generate the synthetic control
  generate_control()

save(sc_out, file = "r_objects/sc_out.Rdata")

# plot results 
sc_plot = sc_out %>% plot_trends() 

# look at predictor weights
sc_pred_weights = sc_out$.predictor_weights %>% data.frame

# what states make up the synthetic control? 
sc_state_weights = sc_out$.unit_weights %>% data.frame

# look at sc vs. mass mortality rates
sc_comp = sc_out$.synthetic_control %>% data.frame




