### Generate simulated dataset given set of simulation parameters
###
### params: list of simulation parameters with the following component
### n_subjects = # of simulated subjects
### n_sims = # of simulation replicates
### treatment = treatment variable
### outcome = outcome variable
### elig = eligibility variable(s)
### Lc = vector of non-eligibility defining covariates
### models = list of nuisance model coefficients. For example values of these coefficients see specificy_inputs.R
###
### Output: Simulated dataset

generate_data <- function(params) {
  ### Unpack Models
  n_subjects <- params$n_subjects 
  pi <- params$models$pi
  eta <- params$models$eta
  lambda <- params$models$lambda
  lambda_shape <- params$models$lambda_shape
  mu <- params$models$mu
  sigma_rss <- params$models$sigma_rss
  
  ### Sample Lc from observed vectors
  row_ix <- sample(1:nrow(df_inform), size = n_subjects, replace = T) 
  
  df_sim <- 
    df_inform %>% 
    slice(row_ix) %>% 
    mutate('subject_id' = 1:nrow(.)) %>% 
    select(subject_id, all_of(params$Lc))
  
  
  ### Generate Treatment Assignments 
  df_sim <- 
    df_sim %>% 
    mutate('pi_hat' = expit(compute_model(df = ., beta = pi))) %>% 
    mutate('bs_type' = rbinom(n = n_subjects, size = 1, prob = pi_hat))
  
  ### Generate Missingness in Eligible
  df_sim <- 
    df_sim %>% 
    mutate('eta_hat' = expit(compute_model(df = ., beta = eta))) %>% 
    mutate('R' = rbinom(n = n_subjects, size = 1, prob = eta_hat))
  
  
  ### Generate Eligibility Criteria for those w/ observation
  df_sim <-
    df_sim %>% 
    mutate('lambda_hat' = exp(compute_model(df = ., beta = lambda))) %>% 
    mutate('baseline_a1c' = 3 + rgamma(n = nrow(.),
                                       shape = lambda_shape,
                                       rate = lambda_shape/lambda_hat)) 
  
  ### Generate Outcomes
  df_sim <- 
    df_sim %>% 
    mutate('mu_hat' = compute_model(df = ., beta = mu)) %>% 
    mutate('pct_wt_change' = rnorm(n = nrow(.), mean = mu_hat, sd = sigma_rss))
  
  ### Mask Le where R = 0 and compute eligibility status 
  df_sim <- 
    df_sim %>% 
    mutate('baseline_a1c' = replace(baseline_a1c, R == 0, NA_real_)) %>% 
    mutate('eligible' = as.numeric(baseline_a1c >= 5.7)) 
  
  
  ### Return simulated dataset
  df_sim <- 
    df_sim %>% 
    select(-ends_with('_hat')) 
  
  return(df_sim)
  
}


compute_truth <- function(params) {
  ### Unpack Models
  pi <- params$models$pi
  eta <- params$models$eta
  lambda <- params$models$lambda
  lambda_shape <- params$models$lambda_shape
  mu <- params$models$mu
  sigma_rss <- params$models$sigma_rss
  
  ### use large # of subjects to compute the truth
  n_subjects <- 100000000
  
  ### Sample Lc from observed vectors
  row_ix <- sample(1:nrow(df_inform), size = n_subjects, replace = T)
  
  df_sim <- 
    df_inform %>% 
    slice(row_ix) %>% 
    mutate('subject_id' = 1:nrow(.)) %>% 
    select(subject_id, all_of(params$Lc))
  
  
  ### Generate Treatment Assignments 
  df_sim <- 
    df_sim %>% 
    mutate('pi_hat' = expit(compute_model(df = ., beta = pi))) %>% 
    mutate('bs_type' = rbinom(n = n_subjects, size = 1, prob = pi_hat))
  
  ### Generate Missingness in Eligible
  df_sim <- 
    df_sim %>% 
    mutate('eta_hat' = expit(compute_model(df = ., beta = eta)),
           'eta1' = expit(compute_model(df = mutate(df_sim, 'bs_type' = 1), beta = eta))) %>% 
    mutate('R' = rbinom(n = n_subjects, size = 1, prob = eta_hat))
  
  
  ### Generate Eligibility Criteria for those w/ observation
  df_sim <-
    df_sim %>% 
    mutate('lambda_hat' = exp(compute_model(df = ., beta = lambda))) %>% 
    mutate('baseline_a1c' = 3 + rgamma(n = nrow(.),
                                       shape = lambda_shape,
                                       rate = lambda_shape/lambda_hat)) 
  
  ### Generate Outcomes
  df_sim <- 
    df_sim %>% 
    mutate('mu_hat' = compute_model(df = ., beta = mu)) %>% 
    mutate('mu0_hat' = compute_model(df = mutate(df_sim, bs_type = 0), beta = mu)) %>% 
    mutate('mu1_hat' = compute_model(df = mutate(df_sim, bs_type = 1), beta = mu)) %>% 
    mutate('pct_wt_change' = rnorm(n = nrow(.), mean = mu_hat, sd = sigma_rss)) %>% 
    mutate('individual_te' = mu1_hat - mu0_hat)
  
  ### Eligibility
  df_sim <- 
    df_sim %>% 
    mutate('eligible' = as.numeric(baseline_a1c >= 5.7)) 
  
  ### Compute ATT
  A <- df_sim$bs_type
  R <- df_sim$R
  E <- df_sim$eligible
  Y <- df_sim$pct_wt_change
  mu0 <- df_sim$mu0_hat
  eta1 <- df_sim$eta1
  
  tau_att_elig <-
    mean(A * R * E/eta1 * (Y - mu0) )/mean(A * R * E/eta1)
  
  return(tau_att_elig)
}


### K to 1 Matching
### df = dataset to do matching on
### match_vars = set of variables to exact match on
### k = ratio of matches
### pre_elig = logical, to match before (pre_elig = T) or after (pre_elig = F) eligibility filtering
match_subjects <- function(df, match_vars, k, pre_elig) {
  if(!pre_elig) {
    df <- 
      df %>% 
      filter(eligible == 1)
  }
  
  ### Shuffle Before Matching
  df <- 
    df %>% 
    dplyr::slice(sample(1:nrow(.)))
  
  match_formula <- 
    as.formula(paste0('bs_type ~', paste(match_vars, collapse = '+')))
  
  matched <-
    MatchIt::matchit(match_formula, 
                     data = df,
                     distance = 'mahalanobis',
                     exact = match_vars,
                     method = 'nearest', 
                     ratio = k)
  
  df_matched <- 
    df %>% 
    filter(subject_id %in% MatchIt::match.data(matched)$subject_id)
  
  ### Match It selects controls in order which is why we shuffled a priori
  ### Then we shuffle again to avoid all the treated units at the end of our dataset
  df_matched <- 
    df_matched %>% 
    dplyr::slice(sample(1:nrow(.)))
  
  return(df_matched)
}
