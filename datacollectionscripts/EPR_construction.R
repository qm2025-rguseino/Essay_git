library(dplyr)
library(tidyr)

options(warn = -1)

# --- Load Data ---
EPR <- read.csv("datasources/EPR.csv", stringsAsFactors = FALSE)
EGC <- read.csv("datasources/EGC2.1_20250930.csv")

# --- Classify Statuses ---
egip_statuses        <- c("MONOPOLY", "DOMINANT", "SENIOR PARTNER", "JUNIOR PARTNER")
meg_statuses         <- c("DISCRIMINATED", "POWERLESS", "SELF-EXCLUSION")
alone_rule_statuses  <- c("MONOPOLY", "DOMINANT")
power_share_statuses <- c("SENIOR PARTNER", "JUNIOR PARTNER")
discrim_statuses     <- c("DISCRIMINATED")
powerless_statuses   <- c("POWERLESS")

# --- Expand EPR to year-level (keep all columns) ---
EPR_panel <- EPR %>%
  rowwise() %>%
  mutate(year = list(seq(from, to)),
         cow = countrycode(gwid, 'gwn', 'cown', custom_match = c("340" = 340, "816" = 816))) %>%
  drop_na(cow) %>% 
  unnest(year) %>%
  ungroup()

# --- Country-Year Panel ---
panel <- EPR_panel %>%
  group_by(cow, statename, year) %>%
  summarise(
    egip_groups_count      = sum(status %in% egip_statuses, na.rm = TRUE),
    excl_groups_count      = sum(status %in% meg_statuses, na.rm = TRUE),
    regaut_groups_count    = sum(reg_aut == "True", na.rm = TRUE),
    discrim_groups_count   = sum(status %in% discrim_statuses, na.rm = TRUE),
    powerless_groups_count = sum(status %in% powerless_statuses, na.rm = TRUE),
    egippop                = sum(size[status %in% egip_statuses], na.rm = TRUE),
    exclpop                = sum(size[status %in% meg_statuses], na.rm = TRUE),
    alonerule_groups_count = sum(status %in% alone_rule_statuses, na.rm = TRUE),
    powershare_groups_count = sum(status %in% power_share_statuses, na.rm = TRUE),
    alonerulepop           = sum(size[status %in% alone_rule_statuses], na.rm = TRUE),
    powersharepop          = sum(size[status %in% power_share_statuses], na.rm = TRUE),
    total_size             = sum(size, na.rm = TRUE),
    egip_size_sum          = sum(size[status %in% egip_statuses], na.rm = TRUE),
    excl_size_sum          = sum(size[status %in% meg_statuses], na.rm = TRUE),
    discrim_size_sum       = sum(size[status %in% discrim_statuses], na.rm = TRUE),
    powerless_size_sum     = sum(size[status %in% powerless_statuses], na.rm = TRUE),
    IFI                    = sum(size[status %in% egip_statuses]^2, na.rm = TRUE),
    EFI                    = sum(size[status %in% meg_statuses]^2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    legippop      = ifelse(total_size > 0, egip_size_sum / total_size, NA_real_),
    lexclpop      = ifelse(total_size > 0, excl_size_sum / total_size, NA_real_),
    discrimpop    = discrim_size_sum,
    ldiscrimpop   = ifelse(total_size > 0, discrim_size_sum / total_size, NA_real_),
    powerlesspop  = powerless_size_sum,
    lpowerlesspop = ifelse(total_size > 0, powerless_size_sum / total_size, NA_real_)
  ) %>%
  dplyr::select(-total_size, -egip_size_sum, -excl_size_sum, -discrim_size_sum, -powerless_size_sum) %>%
  arrange(statename, year)

# --- Join EGC with EPR group-level status ---
EGC_merged <- EGC %>%
  left_join(
    EPR_panel %>% dplyr::select(gwgroupid, year, status),
    by = c("EPR_ID" = "gwgroupid", "year" = "year"),
    relationship = "many-to-one"
  )

# --- Campaign-level variables (first year only) ---
campaign_vars <- EGC_merged %>%
  group_by(id) %>%
  filter(year == min(year)) %>%
  summarise(
    egip_participated = as.integer(any(status %in% egip_statuses & PARTICIPATION == 1, na.rm = TRUE)),
    egip_claimed      = as.integer(any(status %in% egip_statuses & CLAIM == 1, na.rm = TRUE)),
    .groups = "drop"
  )
campaign_vars <- campaign_vars %>% 
  mutate(egip_participated_claimed = ifelse(egip_participated == 1 & egip_claimed == 1, 1, 0))

# --- Build EGC_final ---
EGC_final <- EGC_merged %>%
  left_join(campaign_vars, by = "id") %>%
  left_join(panel, by = c("loc_cow" = "cow", "year" = "year")) %>%
  group_by(id) %>%
  filter(year == min(year)) %>%
  ungroup()

# --- Save ---
write.csv(panel,     "datasources/EPR_panel.csv", row.names = FALSE)
write.csv(EGC_final, "datasources/EGC_final.csv", row.names = FALSE)