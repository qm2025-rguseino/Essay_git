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

#### STEP 1: upload revolutionary events and ethnic data ####

# revolutionary events
renavco_merge <- read.delim("datasources/renavco-merge.tab") %>% 
  filter(byear >= 1960) %>% 
  filter(byear == year) 
nrow(renavco_merge) # 202 events from 1960

NAVCO1.3_regr <- read.csv('datasources/NAVCO1.3_regr.csv', sep = ';') %>% 
  dplyr::rename(cow = NVC1.3_cow,
                year = NVC1.3_BYEAR, NVC1.3_camp_name = NVC1.3_CAMPAIGN,
                NVC1.3_CAMPAIGN = NVC1.3_CAMPAIGN.1) %>% 
  select(!c(NVC1.3_LOCATION, NVC1.3_EYEAR, NVC1.3_camp_name, X)) %>% 
  filter(year >= 1960)
nrow(NAVCO1.3_regr) # 473 events

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
#fill NAs
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

# NAVCO 1.3 and extended NAVCO 2.1 have the greatest number of events - could probably use these

# GROWup with ethnic groups data
growup <- read.csv('datasources/GROWUPdata2.csv') 
growup <- growup %>% 
  mutate(cow = countrycode(countries_gwid, origin = "gwn", destination = "cown",
                             custom_match = c("816" = 816L, "340" = 345L))) %>% 
  drop_na(cow) %>% 
  rename_with(~ paste0("growup_", .), .cols = 4:ncol(growup)) %>% 
  select(!c(countries_gwid, countryname))
growup <- growup[!duplicated(growup[c('cow', 'year')]), ]

# auxilliary data - do not need this yet
conflictevent <- read.csv("datasources/conflictevent.csv") %>% 
  mutate(cow = countrycode(countries_gwid, 'gwn', 'cown')) %>% 
  rename_with(~ paste0("conflict_", .), .cols = 4:ncol(conflictevent)) %>% 
  select(!c(countries_gwid, countryname))
powerresource <- read.csv('datasources/powerresource.csv') %>% 
  mutate(cow = countrycode(countries_gwid, 'gwn', 'cown')) %>% 
  rename_with(~ paste0("ethnic_", .), .cols = 4:ncol(powerresource)) %>% 
  select(!c(countries_gwid, countryname))

EGC <- read.csv('datasources/EGC2.1_20250930.csv')
EGC <- EGC %>% 
  mutate(onset = 1) %>% 
  mutate(viol = ifelse(prim_meth == 0, 1, 0),
         nonviol = ifelse(prim_meth == 1, 1, 0)) %>% 
  rename(cow = loc_cow)

#EPR <- read.csv('https://icr.ethz.ch/data/epr/core/EPR-2021.csv')
#EPR_panel <- EPR %>%
#  rowwise() %>%
#  mutate(year = list(seq(from, to))) %>%
#  unnest(year) %>%
#  ungroup() %>% 
#  mutate(cow = countrycode(gwid, 'gwn', 'cown')) %>% 
#  rename(EPR_ID = gwgroupid, EPR_GROUP = group, pop_share = size) %>% 
#  select(cow, year, EPR_GROUP, EPR_ID, pop_share, status)
  #select(cow, year, EPR_GROUP, groupid, EPR_ID, size, status)
#View(EPR_panel %>% filter(cow == 620))
#EGC_EPR <- merge(EPR_panel, EGC, by = c('year', 'cow', 'EPR_ID'), all.x = TRUE)

#NAVCO2.1 <- read.csv('NAVCO2.1_default.csv') %>% rename(cow = targ_cow) %>% mutate(onset = 1)
#NAVCO2.1_EPR <- merge(NAVCO2.1)

# replication data from the article - do not need it much as only campaign data included
NAVCO_EGC_VDEM <- read.csv("datasources/EGC_CampYear_20250930.csv")
NAVCO_EGC_VDEM <- NAVCO_EGC_VDEM %>% 
  rename(cow = loc_cow)

#### STEP 2: construct a dataset ####
#select microstates to filter them out
microstates <- cowstates %>%
  filter(microstate) %>% distinct(cowcode)

states <- state_panel(start = 1946, end = 2013, by = 'year', useGW = FALSE) %>% 
  dplyr::rename(cow = cowcode) %>% 
  filter(!cow %in% c(265, 331, 347, 713, 835, 80, 31, 680)) %>% #exclude non-UN members
  filter(!cow >= 935) %>% 
  #filter(!cow %in% c(56, 57, 58, 60)) %>% #filter islands
  filter(!(cow %in% microstates$cowcode)) %>% #filter microstates
  dplyr::mutate(country = countrycode(cow, 'cown', 'country.name'),
                country = ifelse(cow == 260, 'German Federal Republic', country)) %>% 
  mutate(country = ifelse(country == 'Yugoslavia' & year >= 2006, 'Serbia', country)) %>% 
  mutate(cow = ifelse(cow == 315, 316, cow),
         cow = ifelse(cow == 260, 255, cow)) %>% 
  filter(!(cow == 255 & year == 1990 & country == 'German Federal Republic')) %>%
  arrange(cow, year)

gdp_gapminder <- read.csv("datasources/gdp_gapminder.csv", sep=";")
gdp_gapminder <- reshape2::melt(gdp_gapminder, 
                                id.vars = c("geo", "Country.Name"), 
                                variable.name = "year", 
                                value.name = "value")
gdp_gapminder$year = gsub("X", "", as.factor(gdp_gapminder$year))
gdp_gapminder$ISO3C <- toupper(gdp_gapminder$geo)
gdp_gapminder <- gdp_gapminder %>% 
  dplyr::mutate(ISO3C = countrycode(ISO3C, "iso3c", "iso3c"),
                cow = countrycode(ISO3C, 'iso3c', 'cown')) %>% 
  dplyr::rename(iso3c = ISO3C,
                gdp_pcap = value) %>%
  dplyr::mutate(cow = case_when(
    iso3c == "SRB" ~ 345,
    #iso3c == "PSE" ~ 666,
    #iso3c == "HKG" ~ 710,
    TRUE ~ cow
  )) %>% 
  drop_na(cow) %>%
  filter(year %in% c(1945:2024)) %>% 
  dplyr::select(cow, year, gdp_pcap)
gdp_gapminder <- gdp_gapminder %>%
  dplyr::mutate(gdp_pcap = as.numeric(gdp_pcap)) %>% 
  arrange(cow, year) %>%
  group_by(cow) %>% 
  dplyr::mutate(gdp_growth = ((gdp_pcap - dplyr::lag(gdp_pcap)) / dplyr::lag(gdp_pcap) * 100)) %>%  # Compute growth rate
  ungroup()

vdemdata::vdem -> vdem
vdem_vars <- vdem %>%
  dplyr::mutate(iso3c = countrycode(country_name, 'country.name', 'iso3c')) %>%
  dplyr::rename(cow = COWcode) %>%
  drop_na(cow) %>% 
  filter(year %in% c(1900:2024)) %>% 
  dplyr::select(cow, year, v2x_polyarchy, v2x_execorr, v2x_cspart, v2x_egal, v2x_clphy)

data(pop1dt)
population <- pop1dt %>% 
  dplyr::mutate(cow = countrycode(name, 'country.name', 'cown'),
                iso3c = countrycode(name, 'country.name', 'iso3c'),
                pop = round(pop)) %>% 
  drop_na(cow) %>% 
  filter(!country_code %in% c(948, 2093),
         year %in% c(1900:2024)) %>% 
  dplyr::select(cow, year, pop)

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
  #rename(cow = country_code) %>% 
  drop_na(cow) %>%
  filter(year %in% c(1945:2024)) %>% 
  dplyr::select(cow, year, median_age, youthbulge)

natural_resources_rent <- wb_data('NY.GDP.TOTL.RT.ZS', start_date = 1900, end_date = 2024)
natural_resources_rent <- natural_resources_rent %>% 
  mutate(cow = countrycode(iso3c, 'iso3c', 'cown')) %>%
  drop_na(cow) %>%
  rename(year = date,
         natresrent = NY.GDP.TOTL.RT.ZS) %>% 
  filter(year %in% c(1900:2019)) %>%
  select(iso3c, cow, year, natresrent)

#combine a dataset 
dfs <- list(
  states,
  growup,
  gdp_gapminder,
  vdem_vars,
  population,
  youth,
  natural_resources_rent,
  NAVCO2.1_regr_extended
)
df_comb <- Reduce(function(x, y) {
  merge(x, y, by = c("cow", "year"), all.x = TRUE)
}, dfs) %>% arrange(cow, year)

#delete duplicates, if necessary
#df_final <- df_final[!duplicated(df_final[c('cow', 'year')]), ]

#refine a dataset
#add logs and lags 
df_final <- df_comb %>% 
  group_by(cow) %>% 
  dplyr::mutate(
                natresrent_log = log(natresrent+0.01),
                natresrent_l = dplyr::lag(natresrent, n = 1),
                natresrent_log_l = dplyr::lag(natresrent_log, n = 1),
                gdp_pcap = log(gdp_pcap+0.01),
                gdp_pcap_l = dplyr::lag(gdp_pcap, n = 1),
                gdp_growth_l = dplyr::lag(gdp_growth, n = 1),
                pop_log = log(pop+0.01), 
                pop_log_l = dplyr::lag(pop_log, n = 1),
                year = as.numeric(year),
                v2x_polyarchy = v2x_polyarchy + 0.01,
                v2x_polyarchy_l = dplyr::lag(v2x_polyarchy, n = 1),
                v2x_cspart_l = dplyr::lag(v2x_cspart, n = 1),
                v2x_egal_l = dplyr::lag(v2x_egal, n = 1),
                v2x_clphy_l = dplyr::lag(v2x_clphy, n = 1),
                v2x_execorr_l = dplyr::lag(v2x_execorr, n = 1),
                youthbulge_l = dplyr::lag(youthbulge, n = 1),
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

#visual diagnostics
# check documentation for KO/DO options
table(df_final$growup_onset_do_flag)
table(df_final$growup_onset_ko_flag)
hist(df_final$growup_excl_groups_count)

#### STEP 3: modeling ####
model1 <- glm(NVC2.1_VIOL ~ growup_discrimpop_l + growup_egippop_l + natresrent_l + gdp_pcap_l + gdp_growth_l + v2x_execorr_l + v2x_polyarchy_l + I(v2x_polyarchy_l^2) + as.factor(region),
                   data = df_final,
                   family = binomial(link = 'logit'), method = "brglmFit", type = "MPL_Jeffreys")
robust_model1 <- diag(vcovCL(model1, type ='HC0', cluster = df_final[rownames(model1$model),]$cow))^0.5

model2 <- glm(NVC2.1_NONVIOL ~ growup_discrimpop_l + growup_egippop_l + natresrent_l + gdp_pcap_l + gdp_growth_l + v2x_execorr_l + v2x_polyarchy_l + I(v2x_polyarchy_l^2) + as.factor(region),
              data = df_final,
              family = binomial(link = 'logit'), method = "brglmFit", type = "MPL_Jeffreys")
robust_model2 <- diag(vcovCL(model2, type ='HC0', cluster = df_final[rownames(model2$model),]$cow))^0.5

model3 <- glm(NVC2.1_VIOL ~ growup_exclpop_l + growup_egippop_l + natresrent_l + gdp_pcap_l + gdp_growth_l + v2x_execorr_l + v2x_polyarchy_l + I(v2x_polyarchy_l^2) + as.factor(region),
              data = df_final,
              family = binomial(link = 'logit'), method = "brglmFit", type = "MPL_Jeffreys")
robust_model3 <- diag(vcovCL(model3, type ='HC0', cluster = df_final[rownames(model3$model),]$cow))^0.5


model4 <- glm(NVC2.1_NONVIOL ~ growup_exclpop_l + growup_egippop_l + natresrent_l + gdp_pcap_l + gdp_growth_l + v2x_execorr_l + v2x_polyarchy_l + I(v2x_polyarchy_l^2) + as.factor(region),
              data = df_final,
              family = binomial(link = 'logit'), method = "brglmFit", type = "MPL_Jeffreys")
robust_model4 <- diag(vcovCL(model4, type ='HC0', cluster = df_final[rownames(model4$model),]$cow))^0.5

stargazer(model1, model2, model3, model4, 
          se = list(robust_model1, robust_model2, robust_model3, robust_model4),
          omit = c('region', 'period'),
          type = 'text')


#visualise
# the effect of excluded population proportion on violent campaigns onset likelihood
out_viol <- avg_predictions(model3, by = c("growup_exclpop_l"), 
                            newdata = datagrid('growup_exclpop_l' = seq(quantile(df_final$growup_exclpop_l, 0.01, na.rm=TRUE), quantile(df_final$growup_exclpop_l, 0.99, na.rm=TRUE), length.out=20), 
                                               grid_type = 'mean_or_mode'), 
                            vcov = vcovCL(model1, type ='HC0', cluster = df_final[rownames(model1$model),]$cow)) %>% 
  inferences(method = "simulation")
ggplot(out_viol, aes(x = growup_exclpop_l, y = estimate)) + 
  geom_line() +
  #geom_hline(yintercept= sum(df_final$NVC2.1_VIOL) / nrow(df_final), linetype="dashed", 
  #           color = "red") + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1) + 
  theme_bw()

#### two visualisations ####
out_excl <- avg_predictions(
  model3,
  by = "growup_exclpop_l",
  newdata = datagrid(
    growup_exclpop_l = seq(quantile(df_final$growup_exclpop_l, 0.01, na.rm=TRUE), quantile(df_final$growup_exclpop_l, 0.99, na.rm=TRUE), length.out=20),
    grid_type = 'mean_or_mode'
  ),
  vcov = vcovCL(model1, type = 'HC0', cluster = df_final[rownames(model1$model),]$cow)
)  %>% 
  inferences(method = "simulation")
out_excl$x_val <- excl_seq
out_excl$group <- "Excluded groups"

out_incl <- avg_predictions(
  model3,
  by = "growup_egippop_l",
  newdata = datagrid(
    growup_egippop_l = seq(quantile(df_final$growup_egippop_l, 0.01, na.rm=TRUE), quantile(df_final$growup_egippop_l, 0.99, na.rm=TRUE), length.out=20),
    grid_type = 'mean_or_mode'
  ),
  vcov = vcovCL(model1, type = 'HC0', cluster = df_final[rownames(model1$model),]$cow)
)  %>% 
  inferences(method = "simulation")
out_incl$x_val <- incl_seq
out_incl$group <- "Included groups"

bind_rows(out_excl, out_incl) |>
  ggplot(aes(x = x_val, y = estimate, color = group, fill = group)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, color = NA) +
  scale_color_manual(values = c("Excluded groups" = "#d73027",
                                "Included groups" = "#4575b4")) +
  scale_fill_manual(values  = c("Excluded groups" = "#d73027",
                                "Included groups" = "#4575b4")) +
  labs(
    x     = "Number of groups (original scale)",
    y     = "Predicted probability of conflict",
    color = NULL,
    fill  = NULL
  ) +
  theme_bw() +
  theme(legend.position = "bottom")


# model ethnic and nonethnic conflicts - I haven'r rerun this though
out_viol <- avg_predictions(model7, by = c("growup_excl_groups_count_l", "growup_regaut_groups_count_l"), 
                            newdata = datagrid('growup_excl_groups_count_l' = seq(quantile(df_final$growup_excl_groups_count_l, 0.01, na.rm=TRUE), quantile(df_final$growup_excl_groups_count_l, 0.99, na.rm=TRUE), length.out=20),
                                               'growup_regaut_groups_count_l' = quantile(df_final$growup_regaut_groups_count_l, c(0.1, 0.9), na.rm=TRUE), grid_type = 'mean_or_mode'), 
                            vcov = vcovCL(model7, type ='HC0', cluster = df_final[rownames(model7$model),]$cow))
ggplot(out_viol, aes(x = growup_excl_groups_count_l, y = estimate, colour = factor(growup_regaut_groups_count_l), fill = factor(growup_regaut_groups_count_l))) + 
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1) + 
  theme_bw()

out_viol <- avg_predictions(model7, by = c("log_growup_excl_groups_count_l", "log_growup_oil_giant_fields_count_l"), 
                            newdata = datagrid('log_growup_excl_groups_count_l' = seq(quantile(df_final$log_growup_excl_groups_count_l, 0.01, na.rm=TRUE), quantile(df_final$log_growup_excl_groups_count_l, 0.99, na.rm=TRUE), length.out=20),
                                                                                            'log_growup_oil_giant_fields_count_l' = quantile(df_final$log_growup_oil_giant_fields_count_l, c(0.1, 0.9), na.rm=TRUE), grid_type = 'mean_or_mode'), 
                            vcov = vcovCL(model7, type ='HC0', cluster = df_final[rownames(model7$model),]$cow))

ggplot(out_viol, aes(x = exp(log_growup_excl_groups_count_l), y = estimate, colour = factor(log_growup_oil_giant_fields_count_l), fill = factor(log_growup_oil_giant_fields_count_l))) + 
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1) + 
  theme_bw()
  #scale_colour_discrete(name = "Logged GDP per capita, t-1", labels = c('10% percentile', '90% percentile')) + scale_fill_discrete(name = "Logged GDP per capita, t-1", labels = c('10% percentile', '90% percentile')) +
  #labs(x = 'Logged central government debt (exp), t-1', y = 'Estimate') + 
  #facet_wrap(~type)

#здесь нужна не onset структура, в этом проблема
model_success1 <- glm(NVC2.1_success ~ log_ethnic_excl_groups_count_l + gdp_pcap_l + gdp_growth_l + v2x_execorr_l + v2x_polyarchy_l + I(v2x_polyarchy_l^2) + v2x_cspart_l + v2x_egal_l,
                      data = df8 %>% filter(NVC2.1_NONVIOL == 1),
                     family = binomial(link = 'logit'))
robust_model_success1 <- diag(vcovCL(model_success1, type ='HC0', cluster = df8[rownames(model_success1$model),]$cow))^0.5

model_success2 <- glm(NVC2.1_success ~ log_ethnic_excl_groups_count_l + gdp_pcap_l + gdp_growth_l + v2x_execorr_l + v2x_polyarchy_l + I(v2x_polyarchy_l^2) + v2x_cspart_l + v2x_egal_l,
                      data = df8 %>% filter(NVC2.1_VIOL == 1),
                      family = binomial(link = 'logit'))
robust_model_success2 <- diag(vcovCL(model_success2, type ='HC0', cluster = df8[rownames(model_success2$model),]$cow))^0.5
stargazer(model_success1,model_success2,
          se = list(robust_model_success1, robust_model_success2),
          type = 'text')


pred_model <- avg_predictions(model3,
                              by = c('log_ethnic_excl_groups_count_l'),
                              newdata = datagrid(
                                 "log_ethnic_excl_groups_count_l" = seq(min(df8$log_ethnic_excl_groups_count_l, na.rm=TRUE), 
                                                                        quantile(df8$log_ethnic_excl_groups_count_l, 0.97, na.rm=TRUE), 
                                                                        length.out = 20),
                                 grid_type = "mean_or_mode"
                              ),
                              #by = c("ethnic_excl_groups_count_l"), 
                              #newdata = datagrid(
                              #   "ethnic_excl_groups_count_l" = seq(0, 15, length.out = 16),
                              #   grid_type = "mean_or_mode"
                              # ),
                               # vcov = vcovCR(m4_mmad, type = "HC0", cluster = df_fisc_mmad[rownames(m4_mmad$model),]$cow))
                               vcov = vcovCL(model3, type ='HC0', cluster = df8[rownames(model3$model),]$cow)
)

ggplot(pred_model, aes(x = exp(log_ethnic_excl_groups_count_l), y = estimate)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1) +
  #geom_rug(
  #  data = df_fisc_mmad_counts %>% filter(log_debt_l >= quantile(df_fisc_mmad$log_debt_l, 0.01, na.rm = TRUE) & log_debt_l <= quantile(df_fisc_mmad$log_debt_l, 0.99, na.rm = TRUE)), aes(x = log_debt_l),
  #  inherit.aes = FALSE, alpha = 0.2
  #) +
  #labs(x = expression(Central ~ government ~ debt[t - 1]), y = "Predicted probability of a violent\nmass anti-regime mobilization onset") +
  theme_bw()