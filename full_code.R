#### Full code for Termpaper ####

### Set up ###------------------------------------------------------------------
## Clear environment
rm(list = ls())

## Load packages; Install packages if necessary
p_needed <- c(
  "tidyverse",        # dplyr, ggplot2, tidyr throughout
  "lme4",             # glmer for RE logit
  "MASS",             # mvrnorm in OVA simulation functions
  "kableExtra",       # kbl, kable_styling for all tables
  "patchwork",        # combining histogram + density plot
  "bookdown",         # pdf_document2, cross-referencing
  "countrycode",      # datacollection.R country coding
  "pROC",             # AUC-ROC model fit
  'modelsummary',     # For summary tables
  "car",              # For model fit
  "DHARMa",
  "splines"
)

packages <- rownames(installed.packages())
p_to_install <- p_needed[!(p_needed %in% packages)]
if (length(p_to_install) > 0) {
  install.packages(p_to_install) 
}

sapply(p_needed, require, character.only = TRUE)

## Load functions
source('functions/simulation_functions.R') # use pre-defined simulation functions
source('functions/ova_sim_functions.R')    # use observed value approach (OVA) simulation functions

## Load dataset
df <- readRDS('datasources/df_EthnicElite_DissidentTactics.rds')



### Descriptive statistics ###--------------------------------------------------
## Create dataframe with categorized tactics and selected variables
summarydata <- df %>%
  mutate(
    Tactics = case_when(
      onset_type == "Nonviolent" ~ "Nonviolent",
      TRUE                       ~ "Violent"
    )
  ) %>%
  dplyr::select(
    Tactics,
    # Main independent variable for Hypothesis 1
    "Included Fractionalization Index" = epr_IFI_l,
    # Main independent variable for Hypothesis 2
    "Ethnic Power Fragmentation Index"    = epr_highest_rank,
    
    # Control variables
    "Excluded Fractionalization Index"                = epr_EFI_l,
    "Ethnic Group in Power Participation"   = egip_participated_l,
    "ln(GDP per capita)" = log_gdp_pcap_l,
    "ln(Population)"     = pop_log_l,
    "Polyarchy"          = v2x_polyarchy_l,
    "Executive Corruption" = v2x_execorr_l,
    "ln(Regime Duration)"  = log_v2regdur_l,
    "Peace Years"          = peace_years_l
  )

## Generate table comparing means across violent and non-violent tactics
datasummary_balance(
  ~Tactics,
  data    = summarydata,
  title   = "Descriptive Statistics by Campaign Tactics (NAVCO 2.1 Onsets)",
  label   = "descriptive-stats",
  output  = "kableExtra",
  fmt     = 2,
  dinm    = FALSE
) %>%
  kable_styling(
    latex_options = c("hold_position"),
    full_width    = FALSE
  ) %>%
  
  ## Create table footnotes
  footnote(
    general = paste(
      "\nAll variables are one-year lagged.",
      "\nIncluded Fractionalization Index (IFI): Higher values indicate greater fragmentation among included ethnic groups.",
      "\nEthnic Power Fragmentation Index (EPFI): Higher values indicate more fragmented power distribution among included groups."
    ),
    general_title = "Note:",
    footnote_as_chunk = TRUE,
    threeparttable = TRUE
  ) %>%

  ## Create section header for main variables
  pack_rows(
    "Main Variables",
    start_row = 1,
    end_row = 2,
    bold = TRUE
  ) %>%
  
  ## Create section header for control variables
  pack_rows(
    "Control Variables",
    start_row = 3,
    end_row = 10,
    bold = TRUE
  ) 


### Test variation of the independent variables ###-----------------------------
## Create function for measuring variation
# Define a function to calculate within-group, between-group, and total variation
# Returns the Intraclass Correlation Coefficient (ICC) to assess clustering strength

variance_decomp <- function(data, var, group) {
  
  ## Calculate group-level statistics
  data_grouped <- data %>%
    group_by(.data[[group]]) %>%
    mutate(
      group_mean = mean(.data[[var]], na.rm = TRUE),
      deviation = .data[[var]] - group_mean
    ) %>%
    ungroup()
  
  ## Calculate Total Variation
  overall_sd <- sd(data_grouped[[var]], na.rm = TRUE)
  
  ## Calculate Within-Group Variation
  within_sd <- sd(data_grouped$deviation, na.rm = TRUE)
  
  ## Calculate Between-Group Variation
  between_sd <- data_grouped %>%
    distinct(.data[[group]], group_mean) %>%
    summarise(
      sd = sd(group_mean, na.rm = TRUE)
    ) %>%
    pull(sd)
  
  ## Calculate Intraclass Correlation Coefficient (ICC)
  # Indicates proportion of total variance explained by group membership
  icc <- between_sd^2 / (between_sd^2 + within_sd^2)
  
  ## Return results as a tidy tibble
  tibble(
    variable = var,
    grouping = group,
    overall_sd = overall_sd,
    within_sd = within_sd,
    between_sd = between_sd,
    ICC = icc
  )
}

## Define key independent variables
vars <- c("epr_IFI_l", "epr_highest_rank")

## Decompose variance by country
variance_results_country <- map_dfr(
  vars,
  variance_decomp,
  data = df,
  group = "cow"
)

## Decompose variance by year
variance_results_year <- map_dfr(
  vars,
  variance_decomp,
  data = df,
  group = "year"
)

## Clean environment
rm(list = c("summarydata", "variance_results", "variance_results_country", "variance_results_year", "variance_decomp", "vars"))

### H1: Analysis ###------------------------------------------------------------
## Define a formula for H1
formula_h1 <- NVC2.1_VIOL ~ epr_IFI_l + epr_EFI_l + egip_participated_l +
  log_gdp_pcap_l + pop_log_l + v2x_polyarchy_l + v2x_execorr_l +
  log_v2regdur_l + peace_years_l + (1 | cow)  # random intercept by country

model1 <- glmer(formula_h1,
                data = df,
                family = binomial(link = "logit"),
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl = list(maxfun = 2e5))) # apply an optimizer for convergence

## Simulate predicted probabilities (Observed value approach)
simulation_number = 1000
ova_egip_full_m1  <- sim_ova_range(model1,        
                                   focal_var = "epr_IFI_l", 
                                   nsim = simulation_number,
                                   vcov_matrix = vcov(model1))$result %>% mutate(type = "Full Sample")

## Create plot for predicted probabilities 
ggplot(ova_egip_full_m1,
       aes(x = focal_value, y = mean)) +
  geom_line(linewidth = 0.9, color = "#C0392B") +
  geom_hline(yintercept = round(sum(df$NVC2.1_VIOL) / nrow(df), 2), # a benchmark probability dashed line
             linetype = 'dashed', color = 'grey', alpha = 0.8) + 
  geom_ribbon(aes(ymin = lower, ymax = upper), 
              alpha = 0.15, fill = "#C0392B", color = NA) +
  geom_rug(
    data = df %>% drop_na(epr_IFI_l),
    aes(x = epr_IFI_l),
    inherit.aes = FALSE,
    color = "#C0392B", alpha = 0.3,
    sides = "b"          # bottom only
  ) + 
  labs(x     = "IFI (0 = fully concentrated, 1 = fully fragmented)", 
       y     = "Predicted Probability \n Violent Campaign", 
       caption = paste0(
         simulation_number, " simulations, 95% CI\n",
         "Note: The dashed line represents the empirical probability of a violent event in the estimation sample (i.e., the mean of the binary outcome)."
       )) +
  theme_bw() +
  theme(
    axis.title.y     = element_text(margin = margin(r = 10)),
    panel.grid.minor = element_blank(),
    plot.caption     = element_text(hjust = 1, color = "grey50")
  )

### H1: Check assumptions and model fit ###-------------------------------------
## Check for multicollinearity
vif(model1)

## Plot residuals vs fitted
plot(fitted(model1), residuals(model1, type = "pearson"))
abline(h = 0)

## Check variance of random effect
VarCorr(model1)

## Check distribution of random effect
re <- ranef(model1)$cow[,1]

qqnorm(re)
qqline(re)



## Calculate cook's distance
cook_obs <- cooks.distance(influence(model1, obs=TRUE))

## Rerun model without influential outliers
model_obs <- update(model1, data = df[cook_obs <= 4/length(cook_obs), ])


## Check Linearity of continuous predictors on the logit scale
model1_splines <- glmer(NVC2.1_VIOL ~ ns(epr_IFI_l, df = 3) + ns(epr_EFI_l, df = 3)  + egip_participated_l +
                  log_gdp_pcap_l + pop_log_l + v2x_polyarchy_l + v2x_execorr_l +
                  log_v2regdur_l + peace_years_l + (1 | cow),
                data = df,
                family = binomial(link = "logit"),
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl = list(maxfun = 2e5)))


### Create confusion matrix ###
## Set same seed as simulations for reproducibility
set.seed(1234)

## Define the number of observations
n_total <- nrow(df)

## Randomly select 2/3 of observations for training
train_idx <- sample(1:n_total, size = floor(2/3 * n_total), replace = FALSE)

## Select remaining 1/3 used as test set
test_idx  <- setdiff(1:n_total, train_idx)

## Construct training dataset
df_train <- df[train_idx, ]

## Estimate mixed-effects logistic regression on training data
model1_train  <- glmer(formula_h1,        # random intercept by country
                       data = df_train,
                       family = binomial(link = "logit"),
                       control = glmerControl(optimizer = "bobyqa",
                                              optCtrl = list(maxfun = 2e5)))

## Extract countries that are present in the training sample
trained_cows <- rownames(ranef(model1_train)$cow)

## Construct test dataset
df_test <- df[test_idx, ] %>%
  filter(as.character(cow) %in% trained_cows) %>%  # test is only with the trained countries
  drop_na(epr_IFI_l, epr_EFI_l, egip_participated_l, log_gdp_pcap_l, 
          pop_log_l, v2x_execorr_l, v2x_polyarchy_l, log_v2regdur_l, 
          peace_years_l) # remove observations with missing values in model covariates

## Predict probabilities of violence for test data
pred_test_prob <- predict(model1_train, newdata = df_test, type = "response")

## Define actual outcome labels
actual_test <- ifelse(df_test$NVC2.1_VIOL == 1,
                      "Violent", "Nonviolent")

## Classify predictions using a 0.5 probability threshold
pred_test   <- ifelse(pred_test_prob >= 0.5, "Violent", "Nonviolent")

## Compute ROC curve and AUC
roc_obj  <- roc(response  = ifelse(actual_test == "Violent", 1, 0),
                predictor = pred_test_prob,
                quiet     = TRUE)
auc_val  <- as.numeric(auc(roc_obj))

## Construct confusion matrix components
tp <- sum(pred_test == "Violent"    & actual_test == "Violent")
tn <- sum(pred_test == "Nonviolent" & actual_test == "Nonviolent")
fp <- sum(pred_test == "Violent"    & actual_test == "Nonviolent")
fn <- sum(pred_test == "Nonviolent" & actual_test == "Violent")

## Calculate metrics
accuracy  <- (tp + tn) / (tp + tn + fp + fn)
precision <- tp / (tp + fp)
recall    <- tp / (tp + fn)   
f1        <- 2 * (precision * recall) / (precision + recall)

## Create confusion matrix table
conf_df <- data.frame(
  ` `            = c("Predicted Nonviolent", "Predicted Violent"),
  `Actual Nonviolent` = c(as.character(tn), as.character(fp)),
  `Actual Violent`    = c(as.character(fn), as.character(tp)),
  check.names = FALSE
)

## Create performance metrics table
metrics_df <- data.frame(
  ` `                 = c("Accuracy", "Precision", 
                          "Recall",  "F1 Score", "AUC"),
  `Actual Nonviolent` = c(sprintf("%.3f", accuracy), 
                          sprintf("%.3f", precision),
                          sprintf("%.3f", recall), 
                          sprintf("%.3f", f1),
                          sprintf("%.3f", auc_val)),
  `Actual Violent`    = c("", "", "", "", ""),
  check.names = FALSE
)

## Combine confusion matrix and metrics
bind_rows(conf_df, metrics_df) %>%
  kbl(
    booktabs = TRUE,
    align    = c("l", "c", "c"),
    caption  = "Confusion Matrix and Predictive Performance — Model 1",
    label    = "conf-matrix-h1",
    escape   = FALSE
  ) %>%
  kable_styling(
    latex_options = c("hold_position"),
    full_width    = FALSE
  ) %>%
  row_spec(0, bold = TRUE) %>%
  row_spec(2, extra_css = "border-bottom: 2px solid black;") %>%  # separator line
  pack_rows("Confusion Matrix", 1, 2) %>%
  pack_rows("Performance Metrics", 3, 7)

### H2: Analysis ###------------------------------------------------------------
## Define a formula for H2
formula_h2 <- NVC2.1_VIOL ~ epr_highest_rank + epr_EFI_l + egip_participated_l +
  log_gdp_pcap_l + pop_log_l + v2x_polyarchy_l + v2x_execorr_l +
  log_v2regdur_l + peace_years_l + (1 | cow)  # random intercept by country

model2 <- glmer(formula_h2,
                data = df,
                family = binomial(link = "logit"),
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl = list(maxfun = 2e5))) # apply an optimizer for convergence

## Simulate predicted probabilities (Observed value approach)
ova_egip_full_m2  <- sim_ova_range(model2,        
                                   focal_var = "epr_highest_rank", 
                                   nsim = simulation_number,
                                   vcov_matrix = vcov(model2))$result %>% mutate(type = "Full Sample")

## Create plot for predicted probabilities 
ggplot(ova_egip_full_m2,
       aes(x = focal_value, y = mean)) +
  geom_line(linewidth = 0.9, color = "#C0392B") +
  geom_hline(yintercept = round(sum(df$NVC2.1_VIOL) / nrow(df), 2), # a benchmark probability dashed line
             linetype = 'dashed', color = 'grey', alpha = 0.8) + 
  geom_ribbon(aes(ymin = lower, ymax = upper), 
              alpha = 0.15, fill = "#C0392B", color = NA) +
  geom_rug(
    data = df %>% drop_na(epr_highest_rank),
    aes(x = epr_highest_rank),
    inherit.aes = FALSE,
    color = "#C0392B", alpha = 0.3,
    sides = "b"          # bottom only
  ) + 
  labs(x     = "Power Equality Index", 
       y     = "Predicted Probability \n Violent Campaign", 
       caption = paste0(
         simulation_number, " simulations, 95% CI\n",
         "Note: The dashed line represents the empirical probability of a violent event in the estimation sample (i.e., the mean of the binary outcome)."
       )) +
  theme_bw() +
  theme(
    axis.title.y     = element_text(margin = margin(r = 10)),
    panel.grid.minor = element_blank(),
    plot.caption     = element_text(hjust = 1, color = "grey50")
  )

### H2: Check assumptions and model fit ###-------------------------------------
## Check for multicollinearity
vif(model2)

## Plot residuals vs fitted
plot(fitted(model2), residuals(model2, type = "pearson"))
abline(h = 0)

## Create QQ plot and
sim <- simulateResiduals(model2)
plot(sim)

testUniformity(sim)
testDispersion(sim)

## Check variance for country
VarCorr(model2)

## Calculate cook's distance
cook_obs <- cooks.distance(influence(model1, obs=TRUE))

## Rerun model without influential outliers
model_obs <- update(model1, data = df[cook_obs <= 4/length(cook_obs), ])


## Check Linearity of continuous predictors on the logit scale
model1_splines <- glmer(NVC2.1_VIOL ~ ns(epr_IFI_l, df = 3) + ns(epr_EFI_l, df = 3)  + egip_participated_l +
                          log_gdp_pcap_l + pop_log_l + v2x_polyarchy_l + v2x_execorr_l +
                          log_v2regdur_l + peace_years_l + (1 | cow),
                        data = df,
                        family = binomial(link = "logit"),
                        control = glmerControl(optimizer = "bobyqa",
                                               optCtrl = list(maxfun = 2e5)))

### Create confusion matrix ###
## Set same seed as simulations for reproducibility
set.seed(1234)

## Define the number of observations
n_total <- nrow(df)

## Randomly select 2/3 of observations for training
train_idx <- sample(1:n_total, size = floor(2/3 * n_total), replace = FALSE)

## Select remaining 1/3 used as test set
test_idx  <- setdiff(1:n_total, train_idx)

## Construct training dataset
df_train <- df[train_idx, ]

## Estimate mixed-effects logistic regression on training data
model2_train  <- glmer(formula_h2,        # random intercept by country
                       data = df_train,
                       family = binomial(link = "logit"),
                       control = glmerControl(optimizer = "bobyqa",
                                              optCtrl = list(maxfun = 2e5)))

## Extract countries that are present in the training sample
trained_cows <- rownames(ranef(model2_train)$cow)

## Construct test dataset
df_test <- df[test_idx, ] %>%
  filter(as.character(cow) %in% trained_cows) %>%  # test is only with the trained countries
  drop_na(epr_highest_rank, egip_participated_l, log_gdp_pcap_l, 
          pop_log_l, v2x_execorr_l, v2x_polyarchy_l, log_v2regdur_l, 
          peace_years_l) # remove observations with missing values in model covariates

## Predict probabilities of violence for test data
pred_test_prob <- predict(model2_train, newdata = df_test, type = "response")

## Define actual outcome labels
actual_test <- ifelse(df_test$NVC2.1_VIOL == 1,
                      "Violent", "Nonviolent")

## Classify predictions using a 0.5 probability threshold
pred_test   <- ifelse(pred_test_prob >= 0.5, "Violent", "Nonviolent")

## Compute ROC curve and AUC
roc_obj  <- roc(response  = ifelse(actual_test == "Violent", 1, 0),
                predictor = pred_test_prob,
                quiet     = TRUE)
auc_val  <- as.numeric(auc(roc_obj))

## Construct confusion matrix components
tp <- sum(pred_test == "Violent"    & actual_test == "Violent")
tn <- sum(pred_test == "Nonviolent" & actual_test == "Nonviolent")
fp <- sum(pred_test == "Violent"    & actual_test == "Nonviolent")
fn <- sum(pred_test == "Nonviolent" & actual_test == "Violent")

## Calculate metrics
accuracy  <- (tp + tn) / (tp + tn + fp + fn)
precision <- tp / (tp + fp)
recall    <- tp / (tp + fn)   
f1        <- 2 * (precision * recall) / (precision + recall)

## Create confusion matrix table
conf_df <- data.frame(
  ` `            = c("Predicted Nonviolent", "Predicted Violent"),
  `Actual Nonviolent` = c(as.character(tn), as.character(fp)),
  `Actual Violent`    = c(as.character(fn), as.character(tp)),
  check.names = FALSE
)

## Create performance metrics table
metrics_df <- data.frame(
  ` `                 = c("Accuracy", "Precision", 
                          "Recall",  "F1 Score", "AUC"),
  `Actual Nonviolent` = c(sprintf("%.3f", accuracy), 
                          sprintf("%.3f", precision),
                          sprintf("%.3f", recall), 
                          sprintf("%.3f", f1),
                          sprintf("%.3f", auc_val)),
  `Actual Violent`    = c("", "", "", "", ""),
  check.names = FALSE
)

## Combine confusion matrix and metrics
bind_rows(conf_df, metrics_df) %>%
  kbl(
    booktabs = TRUE,
    align    = c("l", "c", "c"),
    caption  = "Confusion Matrix and Predictive Performance — Model 1",
    label    = "conf-matrix-h1",
    escape   = FALSE
  ) %>%
  kable_styling(
    latex_options = c("hold_position"),
    full_width    = FALSE
  ) %>%
  row_spec(0, bold = TRUE) %>%
  row_spec(2, extra_css = "border-bottom: 2px solid black;") %>%  # separator line
  pack_rows("Confusion Matrix", 1, 2) %>%
  pack_rows("Performance Metrics", 3, 7)


### H1: Robustness Checks ###---------------------------------------------------

model1_africa <- glmer(formula_h1,
                       family = binomial(link = "logit"),
                       data = df %>% filter(region %in% c('Middle East & North Africa', 'Sub-Saharan Africa')),
                       control = glmerControl(optimizer = "bobyqa",
                                              optCtrl = list(maxfun = 2e5))) # filter by the regionss

model1_noterr <- glmer(formula_h1,
                       family = binomial(link = "logit"),
                       data = df %>% filter(NVC2.1_territorial == 0),
                       control = glmerControl(optimizer = "bobyqa",
                                              optCtrl = list(maxfun = 2e5))) #filter by the non-territorial campaigns


ova_egip_africa_m1  <- sim_ova_range(model1_africa,        
                                     focal_var = "epr_IFI_l", 
                                     nsim = simulation_number,
                                     vcov_matrix = vcov(model1_africa))$result %>% mutate(type = "MENA & Sub-Saharan Africa")
ova_egip_noterr_m1  <- sim_ova_range(model1_noterr,        
                                     focal_var = "epr_IFI_l", 
                                     nsim = simulation_number,
                                     vcov_matrix = vcov(model1_noterr))$result %>% mutate(type = "Excl. territorial campaigns")

ova_model1_robustness <- bind_rows(
  ova_egip_africa_m1,
  ova_egip_noterr_m1,
) %>%
  mutate(type = factor(type, levels = c(
    "MENA & Sub-Saharan Africa",
    "Excl. territorial campaigns"
  )))


ggplot(ova_model1_robustness,
       aes(x = focal_value, y = mean)) +
  geom_line(linewidth = 0.9, color = "#C0392B") +
  geom_hline(yintercept = round(sum(df$NVC2.1_VIOL) / nrow(df), 2),  # a benchmark probability dashed line
             linetype = 'dashed', color = 'grey', alpha = 0.8) + 
  geom_ribbon(aes(ymin = lower, ymax = upper), 
              alpha = 0.15, fill = "#C0392B", color = NA) +
  geom_rug(
    data = df %>% drop_na(epr_IFI_l),
    aes(x = epr_IFI_l),
    inherit.aes = FALSE,
    color = "#C0392B", alpha = 0.3,
    sides = "b"          # bottom only
  ) + 
  labs(x     = "IFI (0 = fully fragmented, 1 = fully concentrated)", 
       y     = "Predicted Probability\n(Violent vs. Nonviolent)", 
       caption = paste0(format(simulation_number), " simulations, 95% CI")) +
  theme_bw() +
  facet_wrap(~type) + 
  theme(
    axis.title.y     = element_text(margin = margin(r = 10)),
    panel.grid.minor = element_blank(),
    plot.caption     = element_text(hjust = 1, color = "grey50")
  )

### H2: Robustness Checks ###---------------------------------------------------

model2_africa <- glmer(formula_h2,
                       family = binomial(link = "logit"),
                       data = df %>% filter(region %in% c('Middle East & North Africa', 'Sub-Saharan Africa')),
                       control = glmerControl(optimizer = "bobyqa",
                                              optCtrl = list(maxfun = 2e5))) # filter by the regionss

model2_noterr <- glmer(formula_h2,
                       family = binomial(link = "logit"),
                       data = df %>% filter(NVC2.1_territorial == 0),
                       control = glmerControl(optimizer = "bobyqa",
                                              optCtrl = list(maxfun = 2e5))) #filter by the non-territorial campaigns


ova_egip_africa_m2  <- sim_ova_range(model2_africa,        
                                     focal_var = "epr_highest_rank", 
                                     nsim = simulation_number,
                                     vcov_matrix = vcov(model1_africa))$result %>% mutate(type = "MENA & Sub-Saharan Africa")
ova_egip_noterr_m2  <- sim_ova_range(model2_noterr,        
                                     focal_var = "epr_highest_rank", 
                                     nsim = simulation_number,
                                     vcov_matrix = vcov(model1_noterr))$result %>% mutate(type = "Excl. territorial campaigns")

ova_model2_robustness <- bind_rows(
  ova_egip_africa_m2,
  ova_egip_noterr_m2,
) %>%
  mutate(type = factor(type, levels = c(
    "MENA & Sub-Saharan Africa",
    "Excl. territorial campaigns"
  )))


ggplot(ova_model2_robustness,
       aes(x = focal_value, y = mean)) +
  geom_line(linewidth = 0.9, color = "#C0392B") +
  geom_hline(yintercept = round(sum(df$NVC2.1_VIOL) / nrow(df), 2),  # a benchmark probability dashed line
             linetype = 'dashed', color = 'grey', alpha = 0.8) + 
  geom_ribbon(aes(ymin = lower, ymax = upper), 
              alpha = 0.15, fill = "#C0392B", color = NA) +
  geom_rug(
    data = df %>% drop_na(epr_IFI_l),
    aes(x = epr_IFI_l),
    inherit.aes = FALSE,
    color = "#C0392B", alpha = 0.3,
    sides = "b"          # bottom only
  ) + 
  labs(x     = "IFI (0 = fully fragmented, 1 = fully concentrated)", 
       y     = "Predicted Probability\n(Violent vs. Nonviolent)", 
       caption = paste0(format(simulation_number), " simulations, 95% CI")) +
  theme_bw() +
  facet_wrap(~type) + 
  theme(
    axis.title.y     = element_text(margin = margin(r = 10)),
    panel.grid.minor = element_blank(),
    plot.caption     = element_text(hjust = 1, color = "grey50")
  )

### Function for cross validation ###-------------------------------------------
cross_validation_fun_glmer <- function(validation_number = 50,
                                       data,
                                       dep_var,
                                       main_iv) {
  
  formula_obj <- as.formula(
    paste(
      dep_var,
      "~",
      main_iv,
      "+ epr_EFI_l + log_gdp_pcap_l +",
      "egip_participated_l + pop_log_l +",
      "v2x_polyarchy_l + v2x_execorr_l +",
      "log_v2regdur_l + peace_years_l +",
      "(1 | cow)"
    ))
  
  data_realizations    <- data[data[[dep_var]] == 1, ]
  data_nonrealizations <- data[data[[dep_var]] == 0, ]
  
  test_fit   <- glmer(formula_obj, family = binomial("logit"), data = data,
                      control = glmerControl(optimizer = "bobyqa",
                                             optCtrl   = list(maxfun = 2e5)))
  coef_names <- names(fixef(test_fit))
  n_coefs    <- length(coef_names)
  
  # pre-allocate for target number of CONVERGED models
  coefs_matrix <- matrix(NA, nrow = validation_number, ncol = n_coefs)
  var_matrix   <- matrix(NA, nrow = validation_number, ncol = n_coefs)
  colnames(coefs_matrix) <- coef_names
  colnames(var_matrix)   <- coef_names
  
  n_converged  <- 0                        # converged iterations collected
  n_attempts   <- 0                        # total attempts made
  max_attempts <- validation_number * 10   # stop if too many failures
  current_seed <- 1
  
  while (n_converged < validation_number && n_attempts < max_attempts) {
    
    n_attempts   <- n_attempts + 1
    set.seed(current_seed)
    current_seed <- current_seed + 1
    
    samp1 <- sample(1:nrow(data_realizations),
                    size = floor(2/3 * nrow(data_realizations)))
    samp0 <- sample(1:nrow(data_nonrealizations),
                    size = floor(2/3 * nrow(data_nonrealizations)))
    sample_data <- rbind(data_realizations[samp1, ],
                         data_nonrealizations[samp0, ])
    
    
    model <- glmer(formula_obj, family = binomial("logit"), data = sample_data,
                   control = glmerControl(optimizer = "bobyqa",
                                          optCtrl = list(maxfun = 2e5)))
    
    #check convergence
    if (!is.null(model@optinfo$conv$lme4$messages)) {
      n_converged <- n_converged + 1
      
      vec_coefs <- fixef(model)
      vec_var   <- diag(vcov(model))
      idx       <- match(names(vec_coefs), coef_names)
      idx_valid <- !is.na(idx)
      coefs_matrix[n_converged, idx[idx_valid]] <- vec_coefs[idx_valid]
      var_matrix[n_converged,   idx[idx_valid]] <- vec_var[idx_valid]
      
      cat("Attempt", n_attempts, "| Converged:", n_converged, "/",
          validation_number, "\n")
    } else {
      cat("Attempt", n_attempts, "| Did not converge — skipping\n")
    }
  }
  
  # report final status
  if (n_converged < validation_number) {
    warning(paste0("Only ", n_converged, " of ", validation_number,
                   " converged models collected after ", 
                   max_attempts, " attempts."))
  }
  
  # trim matrices to actual converged rows
  coefs_matrix <- coefs_matrix[1:n_converged, , drop = FALSE]
  var_matrix   <- var_matrix[1:n_converged,   , drop = FALSE]
  
  # King (2001) combination rule — use n_converged throughout
  mean_coef   <- colMeans(coefs_matrix, na.rm = TRUE)
  W           <- colMeans(var_matrix,   na.rm = TRUE)
  B           <- apply(coefs_matrix, 2, var, na.rm = TRUE)
  T_var       <- W + (1 + 1 / n_converged) * B   # n_converged not n
  se_combined <- sqrt(T_var)
  z_value     <- mean_coef / se_combined
  p_value     <- 2 * (1 - pnorm(abs(z_value)))
  stars <- ifelse(p_value < 0.001, "***",
                  ifelse(p_value < 0.01,  "**",
                         ifelse(p_value < 0.05,  "*",
                                ifelse(p_value < 0.1,   ".", ""))))
  
  results <- data.frame(
    Variable     = coef_names,
    Coefficient  = round(mean_coef,   6),
    `Std. Error` = round(se_combined, 6),
    `z value`    = round(z_value,     3),
    `Pr(>|z|)`   = round(p_value,     4),
    Sig          = stars,
    check.names  = FALSE
  )
  
  return(list(
    results        = results,
    combined_coefs = mean_coef,
    combined_se    = se_combined,
    coefs_matrix   = coefs_matrix,
    var_matrix     = var_matrix,
    n_converged    = n_converged,
    n_attempts     = n_attempts
  ))
}


### H1: Apply cross validation function ###-------------------------------------
val_num <- 5
cross_val <- cross_validation_fun_glmer(data = df, dep_var = 'NVC2.1_VIOL', validation_number = val_num, 
                                        main_iv = "epr_IFI_l")


# Extract main model coefficients and SEs
main_coefs <- fixef(model1)
main_se    <- sqrt(diag(vcov(model1)))

# Cross-validated results
cv_coefs <- cross_val$combined_coefs
cv_se    <- cross_val$combined_se

var_labels <- c(
  "epr_IFI_l"         = "IFI",
  "epr_EFI_l"         = "EFI",
  "egip_participated_l" = 'Ethnic Group in Power Participation',
  "log_gdp_pcap_l"               = "ln(GDP per capita)",
  "pop_log_l"                    = "ln(Population)",
  "v2x_execorr_l"                = "Executive Corruption",
  "v2x_polyarchy_l"              = "Polyarchy",
  "log_v2regdur_l"               = "ln(Regime Duration)",
  "peace_years_l"                = "Peace Years"
)

# Combine into table
comparison_df <- data.frame(
  Variable        = names(main_coefs),
  `Main Model`    = sprintf("%.3f (%.3f)", main_coefs, main_se),
  `Cross-Validated` = sprintf("%.3f (%.3f)", cv_coefs, cv_se),
  check.names     = FALSE
) %>%
  filter(Variable != "(Intercept)") %>%
  mutate(Variable = dplyr::recode(Variable, !!!var_labels))  # apply labels

comparison_df %>%
  kbl(
    booktabs = TRUE,
    align    = c("l", "c", "c"),
    caption  = "Model 1: Main vs. Cross-Validated Coefficients",
    label    = "cv-comparison",
    escape   = FALSE
  ) %>%
  kable_styling(
    latex_options = c("hold_position"),
    full_width    = FALSE
  ) %>%
  row_spec(0, bold = TRUE) %>%
  footnote(
    general = "Intercept is not reported. Standard errors in parentheses. Cross-validated SE via King (2001) combination rule.",
    general_title = "Note:",
    footnote_as_chunk = TRUE,
    threeparttable    = TRUE 
  )


### H2: Apply cross validation function ###-------------------------------------

cross_val <- cross_validation_fun_glmer(data = df, dep_var = 'NVC2.1_VIOL', validation_number = val_num,
                                        main_iv = "epr_highest_rank")


# Extract main model coefficients and SEs
main_coefs <- fixef(model2)
main_se    <- sqrt(diag(vcov(model2)))

# Cross-validated results
cv_coefs <- cross_val$combined_coefs
cv_se    <- cross_val$combined_se

var_labels <- c(
  "epr_highest_rank"         = "Ethnic Power Fragmentation Index",
  "epr_EFI_l"         = "EFI",
  "egip_participated_l" = 'Ethnic Group in Power Participation',
  "log_gdp_pcap_l"               = "ln(GDP per capita)",
  "pop_log_l"                    = "ln(Population)",
  "v2x_execorr_l"                = "Executive Corruption",
  "v2x_polyarchy_l"              = "Polyarchy",
  "log_v2regdur_l"               = "ln(Regime Duration)",
  "peace_years_l"                = "Peace Years"
)

# Combine into table
comparison_df <- data.frame(
  Variable        = names(main_coefs),
  `Main Model`    = sprintf("%.3f (%.3f)", main_coefs, main_se),
  `Cross-Validated` = sprintf("%.3f (%.3f)", cv_coefs, cv_se),
  check.names     = FALSE
) %>%
  filter(Variable != "(Intercept)") %>%
  mutate(Variable = dplyr::recode(Variable, !!!var_labels))  # apply labels

comparison_df %>%
  kbl(
    booktabs = TRUE,
    align    = c("l", "c", "c"),
    caption  = "Model 1: Main vs. Cross-Validated Coefficients",
    label    = "cv-comparison",
    escape   = FALSE
  ) %>%
  kable_styling(
    latex_options = c("hold_position"),
    full_width    = FALSE
  ) %>%
  row_spec(0, bold = TRUE) %>%
  footnote(
    general = "Intercept is not reported. Standard errors in parentheses. Cross-validated SE via King (2001) combination rule.",
    general_title = "Note:",
    footnote_as_chunk = TRUE,
    threeparttable    = TRUE 
  )


### H1: AUC-ROC curve ###-------------------------------------------------------
# Step 1: train-test split (same seed as confusion matrix section)
set.seed(1234)

pred_test <- predict(model1_train, newdata = df_test,
                     type = "response",
                     allow.new.levels = TRUE)

valid_idx    <- !is.na(pred_test) & !is.na(df_test$NVC2.1_VIOL)
actual_test  <- df_test$NVC2.1_VIOL[valid_idx]
probs_test   <- pred_test[valid_idx]

roc_obj <- roc(response  = actual_test,
               predictor = probs_test,
               smooth    = FALSE,      # kernel smoothing
               quiet     = TRUE)

roc_df <- data.frame(
  FPR = 1 - roc_obj$specificities,
  TPR = roc_obj$sensitivities
)

ggplot(roc_df, aes(x = FPR, y = TPR)) +
  geom_ribbon(aes(ymin = 0, ymax = TPR),
              fill = "#C0392B", alpha = 0.08) +
  geom_line(color = "#C0392B", linewidth = 0.9) +
  geom_abline(slope     = 1, intercept = 0,
              linetype  = "dashed", color = "grey60",
              linewidth = 0.5) +
  annotate("text",
           x     = 0.72, y = 0.25,
           label = paste0("AUC = ", round(auc(roc_obj), 3)),
           color = "#C0392B", size = 4) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  labs(
    x       = "False positive rate (1 - Specificity)",
    y       = "True positive rate (Sensitivity)"
  ) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    plot.caption     = element_text(size = 8, color = "grey50", hjust = 1)
  )


### H2: AUC-ROC curve ###-------------------------------------------------------
set.seed(1234)

pred_test <- predict(model2_train, newdata = df_test,
                     type = "response",
                     allow.new.levels = TRUE)

valid_idx    <- !is.na(pred_test) & !is.na(df_test$NVC2.1_VIOL)
actual_test  <- df_test$NVC2.1_VIOL[valid_idx]
probs_test   <- pred_test[valid_idx]

roc_obj <- roc(response  = actual_test,
               predictor = probs_test,
               smooth    = FALSE,      
               quiet     = TRUE)

roc_df <- data.frame(
  FPR = 1 - roc_obj$specificities,
  TPR = roc_obj$sensitivities
)

ggplot(roc_df, aes(x = FPR, y = TPR)) +
  geom_ribbon(aes(ymin = 0, ymax = TPR),
              fill = "#C0392B", alpha = 0.08) +
  geom_line(color = "#C0392B", linewidth = 0.9) +
  geom_abline(slope     = 1, intercept = 0,
              linetype  = "dashed", color = "grey60",
              linewidth = 0.5) +
  annotate("text",
           x     = 0.72, y = 0.25,
           label = paste0("AUC = ", round(auc(roc_obj), 3)),
           color = "#C0392B", size = 4) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  labs(
    x       = "False positive rate (1 - Specificity)",
    y       = "True positive rate (Sensitivity)"
  ) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    plot.caption     = element_text(size = 8, color = "grey50", hjust = 1)
  )


### H1: Random Effects Within-Between (REWB) model ###--------------------------
ova_egip_full_m1_appendix <- ova_egip_full_m1 %>% mutate(type = "RE")

rewb_model <- glmer(NVC2.1_VIOL ~
                      demeaned_IFI + mean_IFI +
                      demeaned_EFI + mean_EFI +
                      log_gdp_pcap_l + pop_log_l +
                      v2x_execorr_l + v2x_polyarchy_l +
                      log_v2regdur_l + peace_years_l +
                      (1 | cow),
                    data   = df,
                    family = binomial("logit"))

ova_egip_full_rewb  <- sim_ova_range(rewb_model,        
                                     focal_var = "mean_IFI", 
                                     nsim = simulation_number,
                                     vcov_matrix = vcov(rewb_model))$result %>% mutate(type = "REWB")

ova_model1_robustness_appendix <- bind_rows(
  ova_egip_full_m1_appendix,
  ova_egip_full_rewb
) %>%
  mutate(type = factor(type, levels = c(
    "RE",
    "REWB"
  )))

rug_data <- bind_rows(
  df %>% drop_na(epr_IFI_l) %>%
    mutate(type = "RE", rug_x = epr_IFI_l),
  df %>% drop_na(mean_IFI) %>%
    mutate(type = "REWB", rug_x = mean_IFI)
) %>%
  mutate(type = factor(type, levels = c("RE", "REWB")))

ggplot(ova_model1_robustness_appendix,
       aes(x = focal_value, y = mean)) +
  geom_line(linewidth = 0.9, color = "#C0392B") +
  geom_hline(yintercept = round(sum(df$NVC2.1_VIOL) / nrow(df), 2), 
             linetype = 'dashed', color = 'grey', alpha = 0.8) + 
  geom_ribbon(aes(ymin = lower, ymax = upper), 
              alpha = 0.15, fill = "#C0392B", color = NA) +
  geom_rug(
    data        = rug_data,
    aes(x       = rug_x),
    inherit.aes = FALSE,
    color       = "#C0392B", alpha = 0.3,
    sides       = "b"
  ) + 
  labs(x     = "IFI (0 = fully fragmented, 1 = fully concentrated)", 
       y     = "Predicted Probability\n(Violent vs. Nonviolent)", 
       caption = paste0(format(simulation_number), " simulations, 95% CI")) +
  theme_bw() +
  facet_wrap(~type) + 
  theme(
    axis.title.y     = element_text(margin = margin(r = 10)),
    panel.grid.minor = element_blank(),
    plot.caption     = element_text(hjust = 1, color = "grey50")
  )

### H2: Random Effects Within-Between (REWB) model ###--------------------------
ova_egip_full_m2_appendix <- ova_egip_full_m2 %>% mutate(type = "RE")

rewb_model <- glmer(NVC2.1_VIOL ~
                      demeaned_epr_highest_rank +
                      mean_epr_highest_rank +
                      demeaned_EFI + mean_EFI +
                      log_gdp_pcap_l + pop_log_l +
                      v2x_execorr_l + v2x_polyarchy_l +
                      log_v2regdur_l + peace_years_l +
                      (1 | cow),
                    data   = df,
                    family = binomial("logit"))

ova_egip_full_rewb  <- sim_ova_range(rewb_model,        
                                     focal_var = "mean_epr_highest_rank", 
                                     nsim = simulation_number,
                                     vcov_matrix = vcov(rewb_model))$result %>% mutate(type = "REWB")

ova_model2_robustness_appendix <- bind_rows(
  ova_egip_full_m2_appendix,
  ova_egip_full_rewb
) %>%
  mutate(type = factor(type, levels = c(
    "RE",
    "REWB"
  )))

rug_data <- bind_rows(
  df %>% drop_na(epr_highest_rank) %>%
    mutate(type = "RE", rug_x = epr_highest_rank),
  df %>% drop_na(mean_IFI) %>%
    mutate(type = "REWB", rug_x = epr_highest_rank)
) %>%
  mutate(type = factor(type, levels = c("RE", "REWB")))

ggplot(ova_model1_robustness_appendix,
       aes(x = focal_value, y = mean)) +
  geom_line(linewidth = 0.9, color = "#C0392B") +
  geom_hline(yintercept = round(sum(df$NVC2.1_VIOL) / nrow(df), 2), 
             linetype = 'dashed', color = 'grey', alpha = 0.8) + 
  geom_ribbon(aes(ymin = lower, ymax = upper), 
              alpha = 0.15, fill = "#C0392B", color = NA) +
  geom_rug(
    data        = rug_data,
    aes(x       = rug_x),
    inherit.aes = FALSE,
    color       = "#C0392B", alpha = 0.3,
    sides       = "b"
  ) + 
  labs(x     = "", 
       y     = "Predicted Probability\n(Violent vs. Nonviolent)", 
       caption = paste0(format(simulation_number), " simulations, 95% CI")) +
  theme_bw() +
  facet_wrap(~type) + 
  theme(
    axis.title.y     = element_text(margin = margin(r = 10)),
    panel.grid.minor = element_blank(),
    plot.caption     = element_text(hjust = 1, color = "grey50")
  )








