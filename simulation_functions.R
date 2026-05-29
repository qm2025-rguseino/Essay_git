response_function <- function(x) {
  # mu: matrix of shape [nsim x K-1] (excluding reference category)
  # add reference category (fixed at 0)
  mu_full <- cbind(0, mu)          # [nsim x K]
  exp(mu_full) / rowSums(exp(mu_full))
}

stochastic_component <- function(ndraws, p) {
  # p: vector of K probabilities summing to 1
  which(rmultinom(1, size = 1, prob = p) == 1)
}

sim_function <- function(seed = 666,
                         nsim = 1000,
                         coefs,           # vectorised [(K-1)*P]
                         vcov,
                         scenario,        # [N x P] — all observations for OVA
                         K_minus_1,       # number of non-reference categories
                         predicted_values = FALSE,
                         stoch_comp = NULL) {
  
  if (is.null(dim(scenario))) stop("scenario must be a matrix")
  if (length(coefs) != K_minus_1 * ncol(scenario)) stop("coefs and scenario don't fit")
  
  set.seed(seed)
  P <- ncol(scenario)
  N <- nrow(scenario)
  K <- K_minus_1 + 1
  
  # Draw from sampling distribution
  S <- mvrnorm(nsim, coefs, vcov)    # [nsim x (K-1)*P]
  
  # For each draw: compute softmax probs, average across observations
  ev <- t(apply(S, 1, function(s) {
    coef_mat <- matrix(s, nrow = K_minus_1, ncol = P, byrow = TRUE)  # [K-1 x P]
    mu       <- coef_mat %*% t(scenario)                              # [K-1 x N]
    mu_full  <- rbind(0, mu)                                          # [K x N]
    
    # ← добавь это
    #cat("dim mu_full:", dim(mu_full), "\n")
    #cat("any NaN:", any(is.nan(mu_full)), "\n")
    #cat("any Inf:", any(is.infinite(mu_full)), "\n")
    
    probs    <- exp(mu_full) / matrix(
      colSums(exp(mu_full)), nrow = K, ncol = N, byrow = TRUE
    )                                                     # [K x N]
    rowMeans(probs)                                                   # [K] — OVA average
  }))
  #cat("dim ev:", dim(ev), "\n")  # ← и это
  # ev: [nsim x K]
  
  if (predicted_values) {
    pv <- t(apply(ev, 1, function(p) rmultinom(1, size = 1, prob = p)))
    return(list(ev = ev, pv = pv))
  }
  return(list(ev = ev))
}

simulate_predictions <- function(
    model,
    data,
    focal_var,
    outcome_var,
    vcov_mat,
    moderator_var  = NULL,
    moderator_vals = NULL,
    nsim           = 1000,
    seed           = 666,
    length_out     = 20,
    probs_seq      = c(0.01, 0.99)
) {
  
  X         <- model.matrix(model)
  col_idx   <- which(colnames(X) == focal_var)
  K_minus_1 <- nrow(coef(model))
  coefs_vec <- as.vector(t(coef(model)))
  cat_names <- c(
    levels(data[[outcome_var]])[1],
    rownames(coef(model))
  )
  
  xseq <- seq(
    quantile(data[[focal_var]], probs_seq[1], na.rm = TRUE),
    quantile(data[[focal_var]], probs_seq[2], na.rm = TRUE),
    length.out = length_out
  )
  
  # --- Interaction setup ---
  if (!is.null(moderator_var)) {
    col_mod <- which(colnames(X) == moderator_var)
    
    inter_name <- if (paste0(focal_var, ":", moderator_var) %in% colnames(X)) {
      paste0(focal_var, ":", moderator_var)
    } else {
      paste0(moderator_var, ":", focal_var)
    }
    col_inter <- which(colnames(X) == inter_name)
    mod_levels <- moderator_vals          # use as actual values directly
  } else {
    mod_levels <- list(NULL)
  }
  
  # --- Loop over moderator levels ---
  results <- lapply(seq_along(mod_levels), function(m) {
    
    mod_val   <- mod_levels[[m]]
    mod_label <- if (!is.null(moderator_var)) names(mod_levels)[m] else NA
    
    lapply(xseq, function(x_val) {
      
      X_mod            <- X
      X_mod[, col_idx] <- x_val
      
      if (!is.null(moderator_var)) {
        X_mod[, col_mod]   <- mod_val
        X_mod[, col_inter] <- x_val * mod_val
      }
      
      cat("\n--- x_val =", round(x_val, 3),
          if (!is.null(moderator_var)) paste0("| ", moderator_var, " = ", round(mod_val, 3)),
          "---\n")
      
      sim_out <- sim_function(
        seed      = seed,
        nsim      = nsim,
        coefs     = coefs_vec,
        vcov      = vcov_mat,
        scenario  = X_mod,
        K_minus_1 = K_minus_1
      )
      
      ev <- sim_out$ev
      
      data.frame(
        x_val         = x_val,
        category      = cat_names,
        ev_mean       = colMeans(ev),
        ev_lo         = apply(ev, 2, quantile, 0.025),
        ev_hi         = apply(ev, 2, quantile, 0.975),
        moderator_val = if (!is.null(moderator_var)) mod_label else NA
      )
    }) |> do.call(what = rbind)
    
  }) |> do.call(what = rbind)
  
  results
}

boot_cluster_multinom <- function(model, data, cluster_var, R = 500, seed = 123) {
  set.seed(seed)
  clusters <- unique(data[[cluster_var]])
  coef_boot <- replicate(R, {
    # Resample whole clusters
    sampled <- sample(clusters, length(clusters), replace = TRUE)
    boot_data <- map_dfr(sampled, ~ data[data[[cluster_var]] == .x, ])
    m <- update(model, data = boot_data)
    coef(m)
  })
  # Variance from bootstrap distribution
  tcrossprod(apply(coef_boot, 1, function(x) x - rowMeans(coef_boot))) / R
}