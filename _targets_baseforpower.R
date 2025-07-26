#_target experiments

library(stantargets)
library(rstan)
library(targets)
library(tarchetypes)
library(tibble)

# 1. Define the true data-generating process
sim_dat <- function(n = 10L, beta1 = 0) {
  beta <- rnorm(n = 2, mean = beta1, sd = 1)
  x <- seq(from = -1, to = 1, length.out = n)
  y <- rnorm(n, beta[1] + x * beta[2], 1)
  list(
    n = n,
    x = x,
    y = y,
    .join_data = list(beta = beta)
  )
}

#  sim_dat()
#  sim_dat(beta1 = 1)


# sim_dat_2 <- function(n = 10L) {
#   beta <- rnorm(n = 2, mean = 1, sd = 1)
#   x <- seq(from = -1, to = 1, length.out = n)
#   y <- rnorm(n, beta[1] + x * beta[2], 1)
#   list(
#     n = n,
#     x = x,
#     y = y,
#     .join_data = list(beta = beta)
#   )
# }

# simulate_data_2()

# 2. create the stan code for the model
# Stan model (e.g., linear regression)
lines <- "data {
  int <lower = 1> n;
  vector[n] x;
  vector[n] y;
}
parameters {
  vector[2] beta;
}
model {
  y ~ normal(beta[1] + x * beta[2], 1);
  beta ~ normal(0, 1);
}"
writeLines(lines, "reg.stan")



targets <- tar_map(
  values <- list(
    beta1 = c(0,1)
  ),
  tar_stan_mcmc_rep_summary(
    model_data,
    "reg.stan",
    data = sim_dat(beta1 = beta1), # Runs once per rep.
    batches = 4, # Number of branch targets.
    reps = 2, # Number of model reps per branch target.
    variables = "beta",
    memory = "persistent",
    summaries = list(
      ~posterior::quantile2(.x, probs = c(0.025, 0.975))
    ),
    stdout = R.utils::nullfile(),
    stderr = R.utils::nullfile()
  )
)
list(targets)

# list(
#   tar_stan_mcmc_rep_summary(
#     model,
#     "reg.stan",
#     sim_dat_2(), # Runs once per rep.
#     batches = 4, # Number of branch targets.
#     reps = 2, # Number of model reps per branch target.
#     variables = "beta",
#     summaries = list(
#       ~posterior::quantile2(.x, probs = c(0.025, 0.975))
#     ),
#     stdout = R.utils::nullfile(),
#     stderr = R.utils::nullfile()
#   )
# )


# # Define the pipeline
# values <- tibble(
#   method_function = rlang::syms(c("method1", "method2")),
#   data_source = c("NIH", "NIAID")
# )
# targets <- tar_map(
#   values = values,
#   tar_target(analysis, method_function(data_source, reps = 10)),
#   tar_target(summary, summarize_analysis(analysis, data_source))
# )
# list(targets)
# 
# 
# 
# list(
#   tar_target(
#     name = simulated_data,
#     command = generate_data(
#       n_obs = c(50, 100, 200),  # Varying sample sizes
#       true_alpha = 1,
#       true_beta = 0.5,
#       true_sigma = 1
#     ),
#     pattern = map(n_obs), # Simulate data for each sample size
#     iteration = "list"
#   ),
#   tar_target(
#     name = fitted_model,
#     command = rstan::stan(
#       file = "linear_regression.stan",
#       data = simulated_data,
#       chains = 4,
#       iter = 2000,
#       warmup = 1000
#     ),
#     pattern = map(simulated_data), # Fit model to each dataset
#     iteration = "list"
#   ),
#   tar_target(
#     name = power_assessment,
#     command = {
#       # 5. Evaluate results (e.g., probability of beta > 0.3)
#       posterior_beta <- rstan::extract(fitted_model, "beta")$beta
#       prob_beta_gt_0.3 <- mean(posterior_beta > 0.3)
#       prob_beta_gt_0.3 # You might define a threshold for "strong evidence"
#     },
#     pattern = map(fitted_model),
#     iteration = "list"
#   )
#   # You could then summarize power_assessment to calculate the actual power for each sample size
# )

