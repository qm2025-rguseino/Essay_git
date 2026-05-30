rm(list = ls())

#### STEP 0 : load packages ####

# install.packages("remotes") 
# remotes::install_github("rguseinov/peacebuilder")
p_needed <- c('tidyverse', 'countrycode', 'wpp2024', 'vdemdata', 'states', 'wbstats', 'peacebuilder')
packages <- rownames(installed.packages())
p_to_install <- p_needed[!(p_needed %in% packages)]
if (length(p_to_install) > 0) {
  install.packages(p_to_install) 
}
sapply(p_needed, require, character.only = TRUE)
options(warn = -1)

#### STEP 1: upload revolutionary events and ethnic data ####

# from Chin disseration "Military Power and Democratization" - extended list of NAVCO events
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
# growup <- read.csv('datasources/GROWUPdata2.csv') %>% 
#   mutate(cow = countrycode(countries_gwid, origin = "gwn", destination = "cown",
#                            custom_match = c("816" = 816L, "340" = 345L))) %>% 
#   drop_na(cow) %>% 
#   rename_with(~ paste0("growup_", .), .cols = 4:ncol(growup)) %>% 
#   dplyr::select(!c(countries_gwid, countryname))
# growup <- growup[!duplicated(growup[c('cow', 'year')]), ]

## EPR ethnic groups data
EPR <- read.csv('datasources/EPR_panel.csv') %>% 
  rename_with(~ paste0("epr_", .), .cols = 3:ncol(.)) %>% 
  mutate(cow = countrycode(statename, 'country.name', 'cown')) %>% 
  drop_na(cow) %>% 
  dplyr::select(!c(statename))  

states <- build_states_panel(
  start_year = 1944,
  end_year = 2019,
  exclude_microstates = TRUE, #exclude microstates from the panel 
  exclude_non_un = TRUE, #excluded non-UN states from the panel
  exclude_islands = TRUE #exclude most small island developing states from the panel
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
  filter(year %in% c(1944:2024)) %>% 
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
  #growup,
  EPR,
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
#Add a single-grpup elite variable
#lag the egip groups
df_final <- df_comb %>% 
  mutate(year = as.numeric(as.character(year))) %>%  # ← первым делом
  arrange(cow, year) %>%
  group_by(cow) %>% 
  mutate(
    onset_type = case_when(          # ← сначала onset_type
      `NVC2.1_VIOL` == 1 ~ "Violent",
      `NVC2.1_NONVIOL` == 1 ~ "Nonviolent",
      TRUE ~ "No onset"
    ),
    onset_type = factor(
      onset_type,
      levels = c("No onset", "Nonviolent", "Violent")
    ),
    any_campaign = as.integer(onset_type != "No onset"),
    last_campaign_year = ifelse(any_campaign == 1, year, NA),
    last_campaign_year = zoo::na.locf(last_campaign_year, na.rm = FALSE),
    peace_years = ifelse(
      is.na(last_campaign_year),
      year - min(year),
      year - last_campaign_year
    ),
    peace_years_l = dplyr::lag(peace_years, n = 1)
  ) %>%
  ungroup() %>%  
  mutate_at(vars(starts_with('NVC2.1_')),
            ~replace_na(., 0)) %>% 
  group_by(cow) %>%        
  dplyr::mutate(
    elite_structure = case_when(
      epr_egip_groups_count == 0 ~ "No EGIP group",
      epr_egip_groups_count == 1 ~ "Single-group elite",
      epr_egip_groups_count >= 2 ~ "Multi-group elite"
    ), #add a variable on elite structure
    elite_structure = factor(
      elite_structure,
      levels = c("Multi-group elite", "Single-group elite", "No EGIP group")
    ),
    single_group_elite = case_when(
      epr_egip_groups_count == 1 ~ 1,
      epr_egip_groups_count >= 2 ~ 0,
      TRUE ~ NA_real_ #if EGIP == 0 -> NA
    ), #another measurement of single-elite groups
    log_oilrents_l = dplyr::lag(log_oilrents, n = 1),
    log_gdp_pcap_l = dplyr::lag(log_gdp_pcap, n = 1),
    gdp_growth_l = dplyr::lag(gdp_growth, n = 1),
    pop_log = log(pop+0.01), 
    pop_log_l = dplyr::lag(pop_log, n = 1),
    v2x_polyarchy_l = dplyr::lag(v2x_polyarchy, n = 1),
    v2x_execorr_l = dplyr::lag(v2x_execorr, n = 1),
    log_v2regdur_l = dplyr::lag(log_v2regdur, n = 1),
    youthbulge_l = dplyr::lag(youthbulge, n = 1),
    swid_gini_disp_l = dplyr::lag(swid_gini_disp, n = 1),
    region = countrycode(cow, 'cown', 'region'),
    period = factor(floor((year - 1946) / 10) + 1),
    log_peace_years_l = log(peace_years_l + 0.1)) %>% 
  mutate(across(
    starts_with("epr_"),
    ~ lag(., n = 1),
    .names = "{.col}_l"
  )) %>% 
  # mutate(log_growup_excl_groups_count_l = log(growup_excl_groups_count_l+1),
  #        log_growup_egip_groups_count_l = log(growup_egip_groups_count_l + 1),
  #        log_growup_oil_giant_fields_count_l = log(growup_oil_giant_fields_count_l + 1),
  #        log_growup_exclpop_l = log(growup_exclpop_l + 0.1),
  #        log_growup_lexclpop_l = log(growup_lexclpop_l + 0.1),
  #        log_growup_egippop_l = log(growup_egippop_l + 0.1),
  #        log_growup_legippop_l = log(growup_legippop_l + 0.1),
  #        log_growup_discrimpop_l = log(growup_discrimpop_l + 0.1),
  #        log_growup_ldiscrimpop_l = log(growup_ldiscrimpop_l + 0.1)
  mutate(log_epr_excl_groups_count_l = log(epr_excl_groups_count_l+1),
         log_epr_egip_groups_count_l = log(epr_egip_groups_count_l + 1),
         log_epr_exclpop_l = log(epr_exclpop_l + 0.1),
         log_epr_lexclpop_l = log(epr_lexclpop_l + 0.1),
         log_epr_egippop_l = log(epr_egippop_l + 0.1),
         log_epr_legippop_l = log(epr_legippop_l + 0.1),
         log_epr_discrimpop_l = log(epr_discrimpop_l + 0.1),
         log_epr_ldiscrimpop_l = log(epr_ldiscrimpop_l + 0.1),
         log_epr_alonerule_groups_count_l = log(epr_alonerule_groups_count_l + 0.1),
         log_epr_powershare_groups_count_l = log(epr_powershare_groups_count_l + 0.1)) %>% 
  mutate(elite_structure_l = dplyr::lag(elite_structure, n = 1),
         single_group_elite_l = dplyr::lag(single_group_elite, n = 1)) %>% 
  ungroup() %>% 
  mutate(
    onset_type = factor(onset_type,   
                        levels = c("No onset", "Nonviolent", "Violent"))
  ) %>% 
  dplyr::rename(country = country.x) %>% 
  dplyr::select(cow, country, year, starts_with('v2'), starts_with('epr_'), starts_with('log_'), ends_with('_l'), starts_with('NVC'),
                contains(c('gdp', 'youth', 'pop', 'growth')),
         onset_type, any_campaign, last_campaign_year, peace_years,
         elite_structure, single_group_elite, region, period) %>% 
  dplyr::select(!contains('oilrent'))

df_final_regr <- df_final %>% 
  dplyr::select(cow, country, year, NVC2.1_NONVIOL, NVC2.1_VIOL, onset_type, NVC2.1_territorial, 
                epr_IFI_l, epr_EFI_l, log_epr_egip_groups_count_l, log_epr_excl_groups_count_l, log_epr_powershare_groups_count_l, log_epr_egippop_l, epr_alonerule_groups_count_l,
                log_gdp_pcap_l, gdp_growth_l,
                pop_log_l, youthbulge_l, v2x_execorr_l, v2x_polyarchy_l, log_v2regdur_l, peace_years_l, 
                region, period)

### Add Power Equality Index ###------------------------------------------------
## Load group level data
group_level <- read_csv("datasources/data_group_level.csv")

## Create included/excluded fractionalization index
# (based on Hendrix, Cullen S., und Idean Salehyan. „Ethnicity, Nonviolent Protest, and Lethal Repression in Africa“. Journal of Peace Research 56, Nr. 4 (2019): 469–84. https://doi.org/10.1177/0022343318820088.)
group_level_FI <- group_level %>%
  filter(isrelevant == 1) %>%
  group_by(countryname, year) %>%
  mutate(
    included_total = sum(groupsize[status_egip == 1]),
    excluded_total = sum(groupsize[status_egip == 0])
  ) %>%
  summarise(
    epr_IFI_Incl = sum(
      (groupsize[status_egip == 1] /
         included_total[1])^2
    ),
    
    epr_EFI_Excl = sum(
      (groupsize[status_egip == 0] /
         excluded_total[1])^2
    )
  )

## Create Power Equality Index
group_level_ENEG <- group_level %>%
  filter(status_egip == 1) %>%
  mutate(
    power_score = case_when(
      status_pwrrank == 4 ~ 1,
      status_pwrrank == 5 ~ 2,
      status_pwrrank == 6 ~ 3,
      status_pwrrank == 7 ~ 4
    )
  ) %>%
  group_by(countryname, year) %>%
  mutate(
    power_share = power_score / sum(power_score)
  ) %>%
  summarise(
    epr_ENEG = 1 / sum(power_share^2),
    .groups = "drop"
  )

## Create alternative measure for power equality
group_level_hr <- group_level %>%
  filter(status_egip == 1) %>%
  group_by(countryname, year) %>%
  summarise(
    epr_highest_rank = max(status_pwrrank, na.rm = TRUE),
    .groups = "drop"
  )

## Create monopoly dummy
group_level_monopoly <- group_level %>%
  filter(status_egip == 1) %>%
  group_by(countryname, year) %>%
  summarise(
    epr_monopoly_dummy = ifelse(any(status_pwrrank == 7, na.rm = TRUE), 1, 0),
    .groups = "drop"
  )
  
## Merge with data
group_level_indices <- list(
  group_level_FI,
  group_level_ENEG,
  group_level_hr,
  group_level_monopoly
) %>%
  reduce(full_join,
         by = c("countryname", "year")) %>% 
  arrange(countryname, year) %>% 
  group_by(countryname) %>% 
  mutate(across(
    starts_with("epr_"),
    ~ lag(., n = 1),
    .names = "{.col}_l"
  )) %>% 
  ungroup() %>% 
  mutate(cow = countrycode(countryname, 'country.name', 'cown')) %>% 
  drop_na(cow) %>% 
  dplyr::select(!countryname)
df_final_regr <- merge(df_final_regr, group_level_indices, by = c('cow', 'year'), all.x = TRUE)

## Save datasets
write.csv(df_final, 'datasources/df_essay.csv')
saveRDS(df_final, 'datasources/df_essay.rds')
write.csv(df_final_regr, 'datasources/df_essay_regr.csv')
saveRDS(df_final_regr, 'datasources/df_essay_regr.rds')
