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
                         stochastic_component = NULL) {
  
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
    probs    <- exp(mu_full) / matrix(
      colSums(exp(mu_full)), nrow = K, ncol = N, byrow = TRUE
    )                                                     # [K x N]
    rowMeans(probs)                                                   # [K] — OVA average
  }))
  # ev: [nsim x K]
  
  if (predicted_values) {
    pv <- t(apply(ev, 1, function(p) rmultinom(1, size = 1, prob = p)))
    return(list(ev = ev, pv = pv))
  }
  return(list(ev = ev))
}