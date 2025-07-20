# _targets.R
library(targets)
library(stantargets)

generate_data <- function(n = 10) {
  true_beta <- stats::rnorm(n = 1, mean = 0, sd = 1)
  x <- seq(from = -1, to = 1, length.out = n)
  y <- stats::rnorm(n, x * true_beta, 1)
  list(n = n, x = x, y = y, true_beta = true_beta)
}


simulate_data <- function(n = 10L) {
  beta <- rnorm(n = 2, mean = 0, sd = 1)
  x <- seq(from = -1, to = 1, length.out = n)
  y <- rnorm(n, beta[1] + x * beta[2], 1)
  list(
    n = n,
    x = x,
    y = y,
    .join_data = list(beta = beta)
  )
}

list(
  tar_stan_mcmc(
    example,
    "x.stan",
    generate_data(),
    stdout = R.utils::nullfile(),
    stderr = R.utils::nullfile()
  ),
  tar_stan_gq_rep_summary(
    postpred,
    stan_files = "gen.stan",
    fitted_params = example_mcmc_x, # one CmdStanFit object
    data = generate_data(), # Function runs once per rep.
    batches = 2, # 2 dynamic branches
    reps = 6, # 6 replications per branch
    quiet = TRUE,
    stdout = R.utils::nullfile(),
    stderr = R.utils::nullfile()
  ),
  
  tar_stan_mcmc_rep_summary(
    model,
    "model.stan",
    simulate_data(), # Runs once per rep.
    batches = 4, # Number of branch targets.
    reps = 2, # Number of model reps per branch target.
    variables = "beta",
    summaries = list(
      ~posterior::quantile2(.x, probs = c(0.025, 0.975))
    ),
    stdout = R.utils::nullfile(),
    stderr = R.utils::nullfile()
  )
)