# ============================================================
# Simulation-Based Predicted Probabilities — Observed Value Approach
# For logit GLM models (glm with family = binomial(link = "logit"))
#
# Follows the simulation approach from AQM2026 Week 06 & 07:
#   1. Draw S ~ MVN(coef, vcov) to capture estimation uncertainty
#   2. For each draw, compute predicted probabilities over ALL observations
#   3. Average across observations (OVA) → one value per simulation draw
#   4. Summarise across draws → mean + 95% CI
#
# Functions:
#   sim_ova_pp()       — baseline OVA: overall average Pr(Y=1)
#   sim_ova_range()    — OVA over a sequence of values for one focal variable
#   sim_ova_fd()       — first difference between two values of a focal variable
# ============================================================

library(MASS)   # mvrnorm


# ------------------------------------------------------------
# HELPER: draw simulated coefficient vectors
# ------------------------------------------------------------
.draw_S <- function(model, nsim, seed, vcov_matrix = NULL) {
  set.seed(seed)
  vc <- if (is.null(vcov_matrix)) vcov(model) else vcov_matrix
  mvrnorm(nsim, coef(model), vc)
}

# logit inverse-link
.inv_logit <- function(x) 1 / (1 + exp(-x))


# ============================================================
# 1. sim_ova_pp()
#    Baseline OVA predicted probability — no variable manipulation.
#    Useful for: "What is the average Pr(Y=1) implied by the model?"
#
# Arguments:
#   model   — fitted glm object (binomial logit)
#   nsim    — number of simulation draws (default 1000)
#   seed    — random seed (default 1234)
#   ci_lvl  — confidence level for interval (default 0.95)
#
# Returns a named list:
#   $mean   — mean predicted probability
#   $lower  — lower CI bound
#   $upper  — upper CI bound
#   $sims   — full vector of nsim average predicted probabilities
# ============================================================

sim_ova_pp <- function(model, nsim = 1000, seed = 1234, ci_lvl = 0.95) {
  
  S <- .draw_S(model, nsim, seed)
  X <- model.matrix(model)
  
  alpha <- (1 - ci_lvl) / 2
  
  # For each simulation draw s: compute Pr(Y=1|X) for every obs, then average
  pp_sims <- apply(S, 1, function(s) mean(.inv_logit(X %*% s)))
  
  list(
    mean  = mean(pp_sims),
    lower = quantile(pp_sims, alpha,       names = FALSE),
    upper = quantile(pp_sims, 1 - alpha,   names = FALSE),
    sims  = pp_sims
  )
}


# ============================================================
# 2. sim_ova_range()
#    OVA predicted probabilities over a sequence of values for one
#    focal variable, holding all other variables at their observed values.
#
# Arguments:
#   model       — fitted glm object (binomial logit)
#   focal_var   — character: column name as it appears in model.matrix(model)
#                 Use colnames(model.matrix(model)) to check exact names.
#                 For log-transformed vars already in the data, use that name
#                 (e.g. "log_gdp_pcap_l"). For squared terms use "I(var^2)".
#   focal_range — numeric vector of values to set the focal variable to.
#                 If NULL, uses seq(min, max, length.out = n_vals).
#   n_vals      — number of values in auto-generated range (default 50)
#   nsim        — number of simulation draws (default 1000)
#   seed        — random seed (default 1234)
#   ci_lvl      — confidence level for interval (default 0.95)
#
# Returns a named list:
#   $result  — data.frame with columns: focal_value, mean, lower, upper
#   $sims    — matrix of dim (nsim x length(focal_range)) of average PPs
# ============================================================

sim_ova_range <- function(model,
                          focal_var,
                          focal_range = NULL,
                          n_vals      = 50,
                          nsim        = 1000,
                          seed        = 1234,
                          ci_lvl      = 0.95,
                          vcov_matrix  = NULL) {
  
  S <- .draw_S(model, nsim, seed, vcov_matrix)
  X <- model.matrix(model)
  
  # Check focal variable exists in model matrix
  col_idx <- which(colnames(X) == focal_var)
  if (length(col_idx) == 0) {
    stop(
      paste0(
        "'", focal_var, "' not found in model matrix.\n",
        "Available columns:\n  ",
        paste(colnames(X), collapse = "\n  ")
      )
    )
  }
  
  # Build focal range if not supplied
  if (is.null(focal_range)) {
    focal_range <- seq(
      min(X[, col_idx], na.rm = TRUE),
      max(X[, col_idx], na.rm = TRUE),
      length.out = n_vals
    )
  }
  
  n_scenarios <- length(focal_range)
  alpha       <- (1 - ci_lvl) / 2
  
  # 3D array: observations x predictors x scenarios
  # Each scenario is a copy of X with focal_var set to focal_range[i]
  cases <- array(rep(X, n_scenarios), dim = c(dim(X), n_scenarios))
  for (i in seq_len(n_scenarios)) {
    cases[, col_idx, i] <- focal_range[i]
  }
  
  # For each scenario and each simulation draw: average Pr(Y=1) across obs
  val <- matrix(NA, nrow = nsim, ncol = n_scenarios)
  for (i in seq_len(n_scenarios)) {
    val[, i] <- apply(S, 1, function(s) mean(.inv_logit(cases[, , i] %*% s)))
  }
  
  result <- data.frame(
    focal_value = focal_range,
    mean        = apply(val, 2, mean),
    lower       = apply(val, 2, quantile, alpha,     names = FALSE),
    upper       = apply(val, 2, quantile, 1 - alpha, names = FALSE)
  )
  
  list(result = result, sims = val)
}


# ============================================================
# 3. sim_ova_fd()
#    OVA first difference: Pr(Y=1 | focal_var = val2) - Pr(Y=1 | focal_var = val1)
#    All other variables remain at their observed values.
#
# Arguments:
#   model      — fitted glm object (binomial logit)
#   focal_var  — character: column name in model.matrix(model)
#   val1       — baseline value (e.g. mean, min, or 0)
#   val2       — counterfactual value (e.g. mean + 1SD, max, or 1)
#   nsim       — number of simulation draws (default 1000)
#   seed       — random seed (default 1234)
#   ci_lvl     — confidence level (default 0.95)
#
# Returns a named list:
#   $mean  — mean first difference
#   $lower — lower CI bound
#   $upper — upper CI bound
#   $sims  — vector of nsim first differences (for histograms etc.)
#   $pp1   — nsim-length vector of avg PPs at val1
#   $pp2   — nsim-length vector of avg PPs at val2
# ============================================================

sim_ova_fd <- function(model,
                       focal_var,
                       val1,
                       val2,
                       nsim   = 1000,
                       seed   = 1234,
                       ci_lvl = 0.95,
                       vcov_matrix  = NULL) {
  
  out <- sim_ova_range(
    model       = model,
    focal_var   = focal_var,
    focal_range = c(val1, val2),
    nsim        = nsim,
    seed        = seed,
    ci_lvl      = ci_lvl,
    vcov_matrix = vcov_matrix 
  )
  
  pp1 <- out$sims[, 1]
  pp2 <- out$sims[, 2]
  fd  <- pp2 - pp1
  
  alpha <- (1 - ci_lvl) / 2
  
  list(
    mean  = mean(fd),
    lower = quantile(fd, alpha,     names = FALSE),
    upper = quantile(fd, 1 - alpha, names = FALSE),
    sims  = fd,
    pp1   = pp1,
    pp2   = pp2
  )
}


# ============================================================
# USAGE EXAMPLES
# ============================================================

# -- Check exact column names after model fitting:
# colnames(model.matrix(model1))

# -- 1. Overall baseline predicted probability
# ova_base <- sim_ova_pp(model1)
# ova_base$mean; ova_base$lower; ova_base$upper

# -- 2. Predicted probabilities over a range of log_gdp_pcap_l
# ova_gdp <- sim_ova_range(model1, focal_var = "log_gdp_pcap_l")
# head(ova_gdp$result)

# Plot:
# with(ova_gdp$result, {
#   plot(focal_value, mean, type = "l", ylim = c(0, 1),
#        xlab = "log(GDP per capita)", ylab = "Pr(Conflict)")
#   lines(focal_value, lower, lty = 2)
#   lines(focal_value, upper, lty = 2)
# })

# -- 3. First difference: effect of moving log_gdp_pcap_l from 25th to 75th percentile
# q25 <- quantile(df_final_methods$log_gdp_pcap_l, 0.25, na.rm = TRUE)
# q75 <- quantile(df_final_methods$log_gdp_pcap_l, 0.75, na.rm = TRUE)

# fd_gdp <- sim_ova_fd(model1,
#                      focal_var = "log_gdp_pcap_l",
#                      val1 = q25,
#                      val2 = q75)
# fd_gdp$mean; fd_gdp$lower; fd_gdp$upper
