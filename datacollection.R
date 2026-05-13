#### STEP 0 : load packages ####

library(dplyr)
library(modelsummary)
library(ggeffects)
library(ggplot2)
library(kableExtra)
library(patchwork)
library(brglm2)
library(clubSandwich)
library(sandwich)
library(states)
library(vdemdata)
library(wpp2024)
library(tidyr)
library(countrycode)
library(marginaleffects)
library(stargazer)
library(visdat)
library(purrr)
library(thematic)
library(haven)
library(wbstats)
library(peacesciencer)
library(peacebuilder)


#### STEP 1: upload revolutionary events and ethnic data ####

# revolutionary events
# renavco dataset
renavco_merge <- read.delim("datasources/renavco-merge.tab") %>% 
  filter(byear >= 1960) %>% 
  filter(byear == year) 
nrow(renavco_merge) # 202 events from 1960

# navco 1.3
NAVCO1.3_regr <- read.csv('datasources/NAVCO1.3_regr.csv', sep = ';') %>% 
  dplyr::rename(cow = NVC1.3_cow,
                year = NVC1.3_BYEAR, NVC1.3_camp_name = NVC1.3_CAMPAIGN,
                NVC1.3_CAMPAIGN = NVC1.3_CAMPAIGN.1) %>% 
  select(!c(NVC1.3_LOCATION, NVC1.3_EYEAR, NVC1.3_camp_name, X)) %>% 
  filter(year >= 1960)
nrow(NAVCO1.3_regr) # 473 events

# navco 2.1
NAVCO2.1_regr <- read.csv("datasources/NAVCO2.1_regr.csv", sep=",") %>% 
  dplyr::rename(cow = NVC2.1_loc_cow,
                year = NVC2.1_start_year, end_year = NVC2.1_end_year, NVC2.1_event_name = NVC2.1_camp_name) %>% 
  dplyr::mutate(cow = as.numeric(cow),
                NVC2.1_location = countrycode(cow, 'cown', 'country.name'),
                NVC2.1_VIOL= ifelse(NVC2.1_prim_meth == 0,1,0),
                NVC2.1_NONVIOL= ifelse(NVC2.1_prim_meth == 1,1,0)) %>% 
  mutate(cow = ifelse(NVC2.1_location == 'Serbia', 345, cow)) %>% #new
  drop_na(cow) %>% 
  filter(cow > 0) %>% 
  filter(year >= 1960) %>% 
  dplyr::select(!c(NVC2.1_location, NVC2.1_id, NVC2.1_prim_meth, NVC2.1_progress, NVC2.1_event_name, end_year))
nrow(NAVCO2.1_regr) # 321 event

# from Chin disseration "Military Power and Democratization" - extended list of NAVCO events
# fill NAs
# I use this list of events as having the largest number of events
NAVCO2.1_regr_extended <- read.csv("datasources/NAVCO2_1_regr_extended_v3.csv", sep=",") %>%
  dplyr::mutate(NVC2.1_loc_cow = ifelse(is.na(NVC2.1_loc_cow), 
                                        countrycode(NVC2.1_location, 'country.name', 'cown'), NVC2.1_loc_cow)) %>% 
  dplyr::rename(cow = NVC2.1_loc_cow,
                year = NVC2.1_start_year, end_year = NVC2.1_end_year, NVC2.1_event_name = NVC2.1_camp_name) %>% 
  dplyr::mutate(cow = as.numeric(cow),
                NVC2.1_location = countrycode(cow, 'cown', 'country.name'),
                NVC2.1_VIOL = ifelse(NVC2.1_prim_meth == 0,1,0),
                NVC2.1_NONVIOL = ifelse(NVC2.1_prim_meth == 1,1,0),
                NVC2.1_antiregime = ifelse(NVC2.1_camp_goals == 0, 1, 0),
                NVC2.1_territorial = ifelse(NVC2.1_camp_goals == 3, 1, 0)) %>% 
  mutate(cow = ifelse(NVC2.1_location == 'Serbia', 345, cow)) %>% #new
  drop_na(cow) %>% 
  filter(cow > 0) %>% 
  dplyr::select(!c(NVC2.1_location, NVC2.1_id, NVC2.1_prim_meth, NVC2.1_progress, NVC2.1_event_name, end_year))
nrow(NAVCO2.1_regr_extended) # 554 events

# GROWup with ethnic groups data
growup <- read.csv('datasources/GROWUPdata2.csv') %>% 
  mutate(cow = countrycode(countries_gwid, origin = "gwn", destination = "cown",
                           custom_match = c("816" = 816L, "340" = 345L))) %>% 
  drop_na(cow) %>% 
  rename_with(~ paste0("growup_", .), .cols = 4:ncol(growup)) %>% 
  select(!c(countries_gwid, countryname))
growup <- growup[!duplicated(growup[c('cow', 'year')]), ]

states <- build_states_panel(
  start_year = 1946,
  end_year = 2019,
  exclude_microstates = TRUE,
  exclude_non_un = TRUE,
  exclude_islands = TRUE
)

# data with socio-economic variables on GDP, GDP growth, oil rents, inequality measures, etc. 
econdata <- read.csv('datasources/econdata.csv')

# UN population data 
data(pop1dt)
population <- pop1dt %>% 
  dplyr::mutate(cow = countrycode(name, 'country.name', 'cown'),
                iso3c = countrycode(name, 'country.name', 'iso3c'),
                pop = round(pop)) %>% 
  drop_na(cow) %>% 
  filter(!country_code %in% c(948, 2093),
         year %in% c(1900:2024)) %>% 
  dplyr::select(cow, year, pop)

# calculate youth proportion - youth bulge argument
data(popAge1dt)
youth <- popAge1dt %>%
  dplyr::mutate(cow = countrycode(name, 'country.name', 'cown')) %>% 
  filter(!cow %in% c(948, 2093)) %>% 
  filter(age >= 15) %>% 
  group_by(cow, year) %>%
  dplyr::summarise(median_age = {
    cumulative_pop <- cumsum(pop) / sum(pop)
    ages <- age
    ages[min(which(cumulative_pop >= 0.5))]
  },
  total_15_29 = sum(pop[age >= 15 & age <= 29]),
  total_15_plus = sum(pop),
  youthbulge = total_15_29 / total_15_plus * 100
  ) %>%
  drop_na(cow) %>%
  filter(year %in% c(1945:2024)) %>% 
  dplyr::select(cow, year, median_age, youthbulge)

# load executive corruption data
vdemdata::vdem -> vdem
vdem_vars <- vdem %>%
  dplyr::mutate(iso3c = countrycode(country_name, 'country.name', 'iso3c')) %>%
  dplyr::rename(cow = COWcode) %>%
  drop_na(cow) %>% 
  filter(year %in% c(1900:2024)) %>% 
  dplyr::select(cow, year, v2x_execorr)

#combine a dataset 
dfs <- list(
  states,
  growup,
  vdem_vars,
  population,
  youth,
  econdata,
  NAVCO2.1_regr_extended
)
df_comb <- Reduce(function(x, y) {
  merge(x, y, by = c("cow", "year"), all.x = TRUE)
}, dfs) %>% arrange(cow, year)

#refine a dataset
#add logs and lags 
df_final <- df_comb %>% 
  arrange(cow, year) %>%
  group_by(cow) %>% 
  dplyr::mutate(
    log_oilrents_l = dplyr::lag(log_oilrents, n = 1),
    log_gdp_pcap_l = dplyr::lag(log_gdp_pcap, n = 1),
    gdp_growth_l = dplyr::lag(gdp_growth, n = 1),
    pop_log = log(pop+0.01), 
    pop_log_l = dplyr::lag(pop_log, n = 1),
    year = as.numeric(year),
    #v2x_polyarchy = v2x_polyarchy + 0.01,
    v2x_polyarchy_l = dplyr::lag(v2x_polyarchy, n = 1),
    #v2x_cspart_l = dplyr::lag(v2x_cspart, n = 1),
    #v2x_egal_l = dplyr::lag(v2x_egal, n = 1),
    #v2x_clphy_l = dplyr::lag(v2x_clphy, n = 1),
    v2x_execorr_l = dplyr::lag(v2x_execorr, n = 1),
    log_v2regdur_l = dplyr::lag(log_v2regdur, n = 1),
    youthbulge_l = dplyr::lag(youthbulge, n = 1),
    swid_gini_disp_l = dplyr::lag(swid_gini_disp, n = 1),
    region = countrycode(cow, 'cown', 'region'),
    period = factor(floor((year - 1946) / 10) + 1),) %>% 
  mutate_at(vars(starts_with('NVC2.1_')),
            ~replace_na(., 0)) %>% 
  mutate(across(
    starts_with("growup_"),
    ~ lag(., n = 1),
    .names = "{.col}_l"
  )) %>% 
  mutate(log_growup_excl_groups_count_l = log(growup_excl_groups_count_l+1),
         log_growup_egip_groups_count_l = log(growup_egip_groups_count_l + 1),
         log_growup_oil_giant_fields_count_l = log(growup_oil_giant_fields_count_l + 1),
         log_growup_exclpop_l = log(growup_exclpop_l + 0.1),
         log_growup_lexclpop_l = log(growup_lexclpop_l + 0.1),
         log_growup_egippop_l = log(growup_egippop_l + 0.1),
         log_growup_legippop_l = log(growup_legippop_l + 0.1),
         log_growup_discrimpop_l = log(growup_discrimpop_l + 0.1),
         log_growup_ldiscrimpop_l = log(growup_ldiscrimpop_l + 0.1)
  )

write.csv(df_final, 'datasources/df_essay.csv')
