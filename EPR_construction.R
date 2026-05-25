library(dplyr)
library(tidyr)

# --- Load Data ---
EPR <- read.csv("EPR.csv", stringsAsFactors = FALSE)

# --- Classify Statuses ---
egip_statuses      <- c("MONOPOLY", "DOMINANT", "SENIOR PARTNER", "JUNIOR PARTNER")
meg_statuses       <- c("DISCRIMINATED", "POWERLESS", "SELF-EXCLUSION")
alone_rule_statuses <- c("MONOPOLY", "DOMINANT")
power_share_statuses <- c("SENIOR PARTNER", "JUNIOR PARTNER")
discrim_statuses   <- c("DISCRIMINATED")
powerless_statuses <- c("POWERLESS")

# --- Country-Year PanelFormat ---
EPR_panel <- EPR %>%
  rowwise() %>%
  mutate(year = list(seq(from, to))) %>%
  unnest(year) %>%
  ungroup()

# --- Aggregate Data ---
panel <- EPR_panel %>%
  group_by(statename, year) %>%
  summarise(
    # 1. EGIP COUNT
    egip_groups_count = sum(status %in% egip_statuses, na.rm = TRUE),
    
    # 2.MEG COUNT
    excl_groups_count = sum(status %in% meg_statuses, na.rm = TRUE),
    
    # 3.REGAUT COUNT
    regaut_groups_count = sum(reg_aut == "True", na.rm = TRUE),
    
    # 4. Discriminated groups count
    discrim_groups_count = sum(status %in% discrim_statuses, na.rm = TRUE),
    
    # 5. Powerless grousp count
    powerless_groups_count = sum(status %in% powerless_statuses, na.rm = TRUE),
    
    # 6.EGIP share
    egippop = sum(size[status %in% egip_statuses], na.rm = TRUE),
    
    # 7. MEG share
    exclpop = sum(size[status %in% meg_statuses], na.rm = TRUE),
    
    # 8.Rule alone count
    alonerule_groups_count = sum(status %in% alone_rule_statuses, na.rm = TRUE),
    
    # 9.Share power count
    powershare_groups_count = sum(status %in% power_share_statuses, na.rm = TRUE),
    
    # 10. Rule alone share
    alonerulepop = sum(size[status %in% alone_rule_statuses], na.rm = TRUE),
    
    # 11. Share power share
    powersharepop = sum(size[status %in% power_share_statuses], na.rm = TRUE),
    
    # 12-13. calculations for LEGIP and LEXCL
    total_size = sum(size, na.rm = TRUE),
    egip_size_sum = sum(size[status %in% egip_statuses], na.rm = TRUE),
    excl_size_sum = sum(size[status %in% meg_statuses], na.rm = TRUE),
    discrim_size_sum = sum(size[status %in% discrim_statuses], na.rm = TRUE),
    powerless_size_sum = sum(size[status %in% powerless_statuses], na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(
    # 12.LEGIP
    legippop = ifelse(total_size > 0, egip_size_sum / total_size, NA_real_),
    
    # 13. LEXCL
    lexclpop = ifelse(total_size > 0, excl_size_sum / total_size, NA_real_),
    
    # 14.Discriminated share
    discrimpop = discrim_size_sum,
    
    # 15.LDiscriminated
    ldiscrimpop = ifelse(total_size > 0, discrim_size_sum / total_size, NA_real_),
    
    # 16. Powerless share
    powerlesspop = powerless_size_sum,
    
    # 17. Lpowerless
    lpowerlesspop = ifelse(total_size > 0, powerless_size_sum / total_size, NA_real_)
  ) %>%
  dplyr::select(-total_size, -egip_size_sum, -excl_size_sum, -discrim_size_sum, -powerless_size_sum) %>%
  arrange(statename, year)

# --- Save ---
write.csv(panel, "EPR_panel.csv", row.names = FALSE)

cat("Done! Rows:", nrow(panel), "\n")
cat("Preview:\n")
print(head(panel, 3))

View(panel)
