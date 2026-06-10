# --------------------------------------------
# Nonlinear tolerance model (true curvature)
# --------------------------------------------

library(ggplot2)

# -----------------------------
# Parameters
# -----------------------------
params <- list(
  A0 = 2,        # initial leaf area
  p0 = 1,        # base photosynthesis
  T0 = 1,        # base lifespan

  k = 1.5,       # leaf saturation parameter
  theta = 1.2,   # mapping of carbon to fitness

  beta_E = 0.6,  # elevation effect on rate
  beta_C = 0.7,  # competition effect on rate

  gamma_E = 0.6, # elevation effect on lifespan
  gamma_C = 0.7  # competition effect on lifespan
)

# -----------------------------
# Functions
# -----------------------------

f_E <- function(E, beta_E) exp(-beta_E * E)
f_C <- function(C, beta_C) exp(-beta_C * C)

h_E <- function(E, gamma_E) exp(-gamma_E * E)
h_C <- function(C, gamma_C) exp(-gamma_C * C)

fitness_fun <- function(D, E, C, params) {

  A0 <- params$A0
  p0 <- params$p0
  T0 <- params$T0
  k <- params$k
  theta <- params$theta

  beta_E <- params$beta_E
  beta_C <- params$beta_C
  gamma_E <- params$gamma_E
  gamma_C <- params$gamma_C

  # resource effects
  p <- p0 * f_E(E, beta_E) * f_C(C, beta_C)
  T <- T0 * h_E(E, gamma_E) * h_C(C, gamma_C)

  # leaf area after damage
  A <- A0 * (1 - D)

  # nonlinear carbon gain (saturating leaf value)
  C_tot <- T * p * (1 - exp(-k * A))

  # smooth mapping to fitness
  W <- C_tot^theta

  return(W)
}

# -----------------------------
# Simulate
# -----------------------------

D_vals <- seq(0, 0.95, length.out = 200)

envs <- data.frame(
  label = c("LowE_LowC", "LowE_HighC", "HighE_LowC", "HighE_HighC"),
  E = c(0, 0, 1, 1),
  C = c(0, 1, 0, 1)
)

results <- do.call(rbind, lapply(1:nrow(envs), function(i) {

  env <- envs[i, ]

  W_vals <- sapply(D_vals, function(D) {
    fitness_fun(D, env$E, env$C, params)
  })

  data.frame(
    D = D_vals,
    W = W_vals,
    env = env$label
  )
}))

# -----------------------------
# Plot
# -----------------------------

ggplot(results, aes(x = D, y = W, color = env)) +
  geom_line(linewidth = 1.2) +
  theme_bw() +
  labs(
    x = "Damage (D)",
    y = "Fitness (W)",
    color = "Environment",
    title = "Nonlinear tolerance functions (true curvature)"
  )
