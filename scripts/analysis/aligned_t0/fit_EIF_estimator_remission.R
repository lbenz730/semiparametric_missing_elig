library(tidyverse)
library(arrow)
library(glue)
library(SuperLearner)

source('scripts/helpers.R')

### Directory where EHR data is stored
ehr_dir <- '/n/haneuse_ehr_l3/V1.0'
data_dir <- '/n/haneuse_ehr_l3/V1.0/clean_datasets'

### Combos
df_id <- 
  df_elig <- 
  crossing('bmi_lookback' = c(1, 3, 6, 12),
           'diabetes_lookback' = c(1, 3, 6, 12, 24),
           'rx_lookback' = c(0, 12)) %>% 
  mutate('scenario_id' = 1:nrow(.))

### All 40 combons of RYGB vs. VSG Data
df_complete <- read_parquet(glue('{data_dir}/aligned_t0/rygb_vsg_datasets.parquet'))

### Dataset Parameters
args <- commandArgs(trailingOnly = T)
s_id <- as.numeric(args[1])

### Filter to the dataset 
set.seed(3130)
df <- 
  df_complete %>% 
  inner_join(df_id, by = c('bmi_lookback', 'diabetes_lookback', 'rx_lookback')) %>% 
  filter(scenario_id == s_id) %>% 
  select(-R, -eligible) %>% 
  rename('R' = R_remission,
         'eligible' = eligible_remission)


eif_estimator <- function(df, models, n_splits) {
  ### Unpack nuisance model instructions
  eta <- models$eta
  epsilon_star <- models$epsilon_star
  mu <- models$mu
  xi <- models$xi
  u <- models$u
  gamma_tilde <- models$gamma_tilde
  chi_tilde <- models$chi_tilde
  
  ### Create storage for predictions
  df <- 
    df %>% 
    mutate('eta1_hat' = NA,
           'eta0_hat' = NA,
           'epsilon1_star_hat' = NA,
           'mu0_hat' = NA,
           'xi_hat' = NA,
           'u_hat' = NA,
           'gamma_tilde_hat' = NA,
           'chi_tilde_hat' = NA)
  
  
  ### Sample split
  split_ids <- 
    split(sample( 1:nrow(df) ), ceiling(1:nrow(df) / ceiling(nrow(df)/n_splits)))
  
  for(i in 1:n_splits) {
    test_ids <- split_ids[[i]]
    train_ids <- setdiff(1:nrow(df), test_ids)
    df_train <- dplyr::slice(df, train_ids)
    df_test <- dplyr::slice(df, test_ids)
    
    ### Fit eta
    cat('Fitting ETA on Split [', i, '/', n_splits, ']\n', sep = '')
    if(eta$type == 'GLM') {
      if(!eta$stratify) {
        eta_fit <- glm(eta$formula, family = 'binomial', data = df_train)
        eta1_hat <- predict(eta_fit, newdata = mutate(df_test, 'A' = 1), type = 'response')
        eta0_hat <- predict(eta_fit, newdata = mutate(df_test, 'A' = 0), type = 'response')
      } else {
        eta1_fit <- glm(eta$formula, family = 'binomial', data = filter(df_train, A == 1))
        eta1_hat <- predict(eta1_fit, newdata = mutate(df_test, 'A' = 1), type = 'response')
        eta0_fit <- glm(eta$formula, family = 'binomial', data = filter(df_train, A == 0))
        eta0_hat <- predict(eta0_fit, newdata = mutate(df_test, 'A' = 0), type = 'response')
      }
    } else if(eta$type == 'super_learner') {
      
      if(!eta$stratify) {
        ### Create Datasets for Super Learner
        Y_train <- 
          df_train %>% 
          pull(!!eta$Y)
        
        X_train <- 
          df_train %>% 
          select(all_of(!!eta$X))
        
        X_test <- 
          df_test %>% 
          select(all_of(!!eta$X))
        
        ### Convert to Matrix then back to DF to handle cases where we specify interactions
        X_train_mat <- model.matrix(eta$formula, X_train)[,-1] ### Rm intercept term since some other models will add it
        colnames(X_train_mat) <- gsub(':', '_', colnames(X_train_mat))
        
        X_test_mat_0 <- clean_test_matrix(model.matrix(eta$formula, mutate(X_test, 'A' = 0))[,-1], X_train_mat)
        X_test_mat_1 <- clean_test_matrix(model.matrix(eta$formula, mutate(X_test, 'A' = 1))[,-1], X_train_mat)
        
        X_train_df <- as.data.frame(X_train_mat)
        X_test_df_0 <- as.data.frame(X_test_mat_0)
        X_test_df_1 <- as.data.frame(X_test_mat_1)
        
        ### Random Forest Hyperparameters
        mtry_seq <- floor(sqrt(ncol(X_train_mat)) * c(1,2))
        tree_seq <- c(250, 500, 1000)
        min_n_seq <- c(5, 30, 50)
        rf_learners <-
          create.Learner("SL.ranger", tune = list('mtry' = mtry_seq,
                                                  'num.trees' = tree_seq,
                                                  'min.node.size' = min_n_seq))
        sl_eta <- 
          SuperLearner(Y = Y_train,
                       X = X_train_df, 
                       family = 'binomial',
                       SL.library = c(rf_learners$names, eta$sl_libs))
        
        eta1_hat <- predict.SuperLearner(sl_eta, newdata = X_test_df_1, onlySL = T)$pred
        eta0_hat <- predict.SuperLearner(sl_eta, newdata = X_test_df_0, onlySL = T)$pred
      } else if(eta$stratify) {
        ### Create Datasets for Super Learner
        Y_train_0 <- 
          df_train %>% 
          filter(A == 0) %>% 
          pull(!!eta$Y)
        
        Y_train_1 <- 
          df_train %>% 
          filter(A == 1) %>% 
          pull(!!eta$Y)
        
        X_train_0 <- 
          df_train %>% 
          filter(A == 0) %>% 
          select(all_of(!!eta$X))
        
        X_train_1 <- 
          df_train %>% 
          filter(A == 1) %>% 
          select(all_of(!!eta$X))
        
        X_test <- 
          df_test %>% 
          select(all_of(!!eta$X))
        
        ### Random Forest Hyperparameters
        mtry_seq <- floor(sqrt(ncol(X_train_0)) * c(0.5, 1, 2))
        tree_seq <- c(250, 500, 1000)
        min_n_seq <- c(5, 30, 50)
        rf_learners <-
          create.Learner("SL.ranger", tune = list('mtry' = mtry_seq,
                                                  'num.trees' = tree_seq,
                                                  'min.node.size' = min_n_seq))
        
        ### Fit each Super Learner Seperately
        sl_eta_0 <- 
          SuperLearner(Y = Y_train_0,
                       X = X_train_0, 
                       family = 'binomial',
                       SL.library = c(rf_learners$names, eta$sl_libs))
        
        sl_eta_1 <- 
          SuperLearner(Y = Y_train_1,
                       X = X_train_1, 
                       family = 'binomial',
                       SL.library = c(rf_learners$names, eta$sl_libs))
        
        
        eta1_hat <- predict.SuperLearner(sl_eta_1, newdata = X_test, onlySL = T)$pred
        eta0_hat <- predict.SuperLearner(sl_eta_0, newdata = X_test, onlySL = T)$pred
        
      }
      
    }
    
    df$eta1_hat[test_ids] <- eta1_hat
    df$eta0_hat[test_ids] <- eta0_hat
    
    ### Epsilon Star
    cat('Fitting EPSILON_STAR on Split [', i, '/', n_splits, ']\n', sep = '')
    if(epsilon_star$type == 'GLM') {
      epsilon_star_fit <- glm(epsilon_star$formula, family = 'binomial', data = filter(df_train, R == 1))
      epsilon1_star_hat <- predict(epsilon_star_fit, newdata = mutate(df_test, 'A' = 1), type = 'response')
    } else if(epsilon_star$type == 'super_learner') {
      ### Create datasets to train super learner
      Y_train <- 
        df_train %>% 
        filter(R == 1) %>% 
        pull(!!epsilon_star$Y)
      
      X_train <- 
        df_train %>% 
        filter(R == 1) %>% 
        select(all_of(epsilon_star$X))
      
      ### Set to predict on
      X_test <- 
        df_test %>% 
        select(all_of(epsilon_star$X)) %>% 
        mutate('A' = 1)
      
      
      ### Random Forest Hyperparameters
      mtry_seq <- floor(sqrt(ncol(X_train)) * c(0.5, 1, 2))
      tree_seq <- c(250, 500, 1000)
      min_n_seq <- c(5, 30, 50)
      rf_learners <-
        create.Learner("SL.ranger", tune = list('mtry' = mtry_seq,
                                                'num.trees' = tree_seq,
                                                'min.node.size' = min_n_seq))
      sl_epslion_star <- 
        SuperLearner(Y = Y_train,
                     X = X_train, 
                     family = 'binomial',
                     SL.library = c(rf_learners$names, epsilon_star$sl_libs))
      
      epsilon1_star_hat <- 
        as.vector(predict.SuperLearner(sl_epslion_star, newdata = X_test, onlySL = T)$pred)
      
    }
    
    df$epsilon1_star_hat[test_ids] <- epsilon1_star_hat
    
    ### Fit Mu
    cat('Fitting MU0 on Split [', i, '/', n_splits, ']\n', sep = '')
    if(mu$type == 'GLM') {
      mu_fit <- glm(mu$formula, family = 'binomial', data = filter(df_train, R == 1, eligible == 1 | !mu$elig_only, A == 0 | !mu$stratify))
      mu0_hat <- predict(mu_fit, newdata = mutate(df_test, 'A' = 0), type = 'response')
      df_train$mu0_hat <- predict(mu_fit, newdata = mutate(df_train, 'A' = 0)) ### needed for other models
    } else if(mu$type == 'super_learner') {
      ### Create datasets to train super learner
      Y_train <- 
        df_train %>% 
        filter(R == 1, A == 0 | !mu$stratify) %>% 
        filter(eligible == 1 | !mu$elig_only) %>% 
        pull(!!mu$Y)
      
      X_train <- 
        df_train %>% 
        filter(R == 1, A == 0 | !mu$stratify) %>% 
        filter(eligible == 1 | !mu$elig_only) %>% 
        select(all_of(mu$X)) 
      
      X_train_mat <- model.matrix(mu$formula, X_train)[,-1] ### Rm intercept term since some other models will add it
      colnames(X_train_mat) <- gsub(':', '_', colnames(X_train_mat))
      
      ### Set to predict on
      X_test <- 
        df_test %>% 
        select(all_of(mu$X)) %>% 
        mutate('A' = 0)
      cc_ids <- which(!is.na(X_test$baseline_a1c) & !is.na(X_test$baseline_bmi) & !is.na(X_test$DiaRem))
      X_test_mat <- clean_test_matrix(model.matrix(mu$formula, X_test)[,-1], X_train_mat)
      
      X_train_df <- as.data.frame(X_train_mat)
      X_test_df <- as.data.frame(X_test_mat)
      
      ### Random Forest Hyperparameters
      mtry_seq <- floor(sqrt(ncol(X_train_mat)) * c(0.5, 1, 2))
      tree_seq <- c(250, 500, 1000)
      min_n_seq <- c(5, 30, 50)
      rf_learners <-
        create.Learner("SL.ranger", tune = list('mtry' = mtry_seq,
                                                'num.trees' = tree_seq,
                                                'min.node.size' = min_n_seq))
      
      sl_mu <- 
        SuperLearner(Y = Y_train,
                     X = X_train_df, 
                     family = 'binomial',
                     SL.library = c(rf_learners$names, mu$sl_libs))
      
      mu0_hat <- rep(NA, nrow(df_test))
      mu0_hat[cc_ids] <- predict.SuperLearner(sl_mu, newdata = X_test_df, onlySL = T)$pred
      
      ### needed for Xi and Chi models 
      X_train_0 <- 
        df_train %>% 
        select(all_of(mu$X)) %>% 
        mutate('A' = 0)
      cc_ids <- which(!is.na(X_train_0$baseline_a1c) & !is.na(X_train_0$baseline_bmi) & !is.na(X_train_0$DiaRem))
      X_train_mat0 <- clean_test_matrix(model.matrix(mu$formula, X_train_0)[,-1], X_train_mat)
      X_train_df0 <- as.data.frame(X_train_mat0)
      df_train$mu0_hat[cc_ids] <- predict.SuperLearner(sl_mu, newdata = X_train_df0, onlySL = T)$pred
    }
    
    df$mu0_hat[test_ids] <- mu0_hat
    
    ### Fit U
    cat('Fitting U on Split [', i, '/', n_splits, ']\n', sep = '')
    if(u$type == 'GLM') {
      ### Parametric model for u
      u_fit <- glm(u$formula, family = 'binomial', data = filter(df_train, R == 1, eligible == 1 | !u$elig_only))
      u_hat <- predict(u_fit, newdata = df_test, type = 'response')
      df_train$u_hat <- predict(u_fit, newdata = df_train, type = 'response')
    } else if(u$type == 'super_learner') {
      ### Create datasets to train super learner
      Y_train <- 
        df_train %>% 
        filter(R == 1) %>% 
        filter(eligible == 1 | !u$elig_only) %>% 
        pull(!!u$Y)
      
      X_train <- 
        df_train %>% 
        filter(R == 1) %>% 
        filter(eligible == 1 | !u$elig_only) %>% 
        select(all_of(u$X)) 
      
      ### Set to predict on
      X_test <- 
        df_test %>% 
        select(all_of(u$X)) 
      cc_ids <- which(!is.na(X_test$baseline_bmi) & !is.na(X_test$baseline_a1c) & !is.na(X_test$DiaRem))
      
      X_train_mat <- model.matrix(u$formula, X_train)[,-1] ### Rm intercept term since some other models will add it
      colnames(X_train_mat) <- gsub(':', '_', colnames(X_train_mat))
      X_test_mat <- clean_test_matrix(model.matrix(u$formula, X_test)[,-1], X_train_mat)
      
      X_train_df <- as.data.frame(X_train_mat)
      X_test_df <- as.data.frame(X_test_mat)
      
      ### Random Forest Hyperparameters
      mtry_seq <- floor(sqrt(ncol(X_train)) * c(0.5, 1, 2))
      tree_seq <- c(250, 500, 1000)
      min_n_seq <- c(5, 30, 50)
      rf_learners <-
        create.Learner("SL.ranger", tune = list('mtry' = mtry_seq,
                                                'num.trees' = tree_seq,
                                                'min.node.size' = min_n_seq))
      
      sl_u <- 
        SuperLearner(Y = Y_train,
                     X = X_train_df, 
                     family = 'binomial',
                     SL.library = c(rf_learners$names, u$sl_libs))
      
      u_hat <- rep(NA, nrow(df_test))
      u_hat[cc_ids] <- predict.SuperLearner(sl_u, newdata = X_test_df, onlySL = T)$pred
      df$u_hat[test_ids] <- u_hat
      
      ### needed for Xi and Chi models 
      X_train_ <- 
        df_train %>% 
        select(all_of(u$X)) 
      
      X_train_mat_ <- clean_test_matrix(model.matrix(u$formula, X_train_)[,-1], X_train_mat)
      X_train_df_ <- as.data.frame(X_train_mat_)
      
      cc_ids <- which(!is.na(X_train_$baseline_bmi) & !is.na(X_train_$baseline_a1c) & !is.na(X_train_$DiaRem))
      df_train$u_hat[cc_ids] <- predict.SuperLearner(sl_u, newdata = X_train_df_, onlySL = T)$pred
    }
    
    ### Create Compound Outcomes for EIF Nuisance Functions
    df_train <- 
      df_train %>% 
      mutate('Emu0'= eligible * mu0_hat,
             'E_uratio' = eligible * u_hat/(1-u_hat),
             'EY_uratio' = eligible * eGFR * u_hat/(1-u_hat),
             'Emu0_uratio' = eligible * mu0_hat * u_hat/(1-u_hat)) %>% 
      mutate('Emu0' = ifelse(eligible == 0, 0, Emu0),
             'E_uratio' = ifelse(eligible == 0, 0, E_uratio),
             'EY_uratio' = ifelse(eligible == 0, 0, EY_uratio),
             'Emu0_uratio' = ifelse(eligible == 0, 0, Emu0_uratio))
    
    df_test <- 
      df_test %>% 
      mutate('Emu0'= eligible * mu0_hat,
             'E_uratio' = eligible * u_hat/(1-u_hat),
             'EY_uratio' = eligible * eGFR * u_hat/(1-u_hat),
             'Emu0_uratio' = eligible * mu0_hat * u_hat/(1-u_hat)) %>% 
      mutate('Emu0' = ifelse(eligible == 0, 0, Emu0),
             'E_uratio' = ifelse(eligible == 0, 0, E_uratio),
             'EY_uratio' = ifelse(eligible == 0, 0, EY_uratio),
             'Emu0_uratio' = ifelse(eligible == 0, 0, Emu0_uratio))
    
    cat('Fitting XI on Split [', i, '/', n_splits, ']\n', sep = '')
    if(xi$type == 'super_learner') {
      ### Create datasets to train super learner
      Y_train <- 
        df_train %>% 
        filter(R == 1, A == 1) %>% 
        pull(!!xi$Y)
      
      X_train <- 
        df_train %>% 
        filter(R == 1, A == 1) %>% 
        select(all_of(xi$X)) 
      
      X_train_mat <- model.matrix(xi$formula, X_train)[,-1] ### Rm intercept term since some other models will add it
      colnames(X_train_mat) <- gsub(':', '_', colnames(X_train_mat))
      
      ### Set to predict on
      X_test <- 
        df_test %>% 
        select(all_of(xi$X)) 
      X_test_mat <- clean_test_matrix(model.matrix(xi$formula, X_test)[,-1], X_train_mat)
      
      
      ### Random Forest Hyperparameters
      mtry_seq <- floor(sqrt(ncol(X_train_mat)) * c(0.5, 1, 2))
      tree_seq <- c(250, 500, 1000)
      min_n_seq <- c(5, 30, 50)
      rf_learners <-
        create.Learner("SL.ranger", tune = list('mtry' = mtry_seq,
                                                'num.trees' = tree_seq,
                                                'min.node.size' = min_n_seq))
      
      sl_xi <- 
        SuperLearner(Y = Y_train,
                     X = X_train_mat, 
                     SL.library = c(xi$sl_libs, rf_learners$names))
      
      xi_hat <- predict.SuperLearner(sl_xi, newdata = X_test_mat, onlySL = T)$pred
      df$xi_hat[test_ids] <- xi_hat
    }
    
    cat('Fitting GAMMA_TILDE on Split [', i, '/', n_splits, ']\n', sep = '')
    if(gamma_tilde$type == 'super_learner') {
      ### Create datasets to train super learner
      Y_train <- 
        df_train %>% 
        filter(R == 1, A == 0) %>% 
        pull(!!gamma_tilde$Y)
      
      X_train <- 
        df_train %>% 
        filter(R == 1, A == 0) %>% 
        select(all_of(gamma_tilde$X)) 
      
      X_train_mat <- model.matrix(gamma_tilde$formula, X_train)[,-1] ### Rm intercept term since some other models will add it
      colnames(X_train_mat) <- gsub(':', '_', colnames(X_train_mat))
      
      ### Set to predict on
      X_test <- 
        df_test %>% 
        select(all_of(gamma_tilde$X)) 
      X_test_mat <- clean_test_matrix(model.matrix(gamma_tilde$formula, X_test)[,-1], X_train_mat)
      
      
      ### Random Forest Hyperparameters
      mtry_seq <- floor(sqrt(ncol(X_train_mat)) * c(0.5, 1, 2))
      tree_seq <- c(250, 500, 1000)
      min_n_seq <- c(5, 30, 50)
      rf_learners <-
        create.Learner("SL.ranger", tune = list('mtry' = mtry_seq,
                                                'num.trees' = tree_seq,
                                                'min.node.size' = min_n_seq))
      
      sl_gamma_tilde <- 
        SuperLearner(Y = Y_train,
                     X = X_train_mat, 
                     SL.library = c(gamma_tilde$sl_libs, rf_learners$names))
      
      gamma_tilde_hat <- predict.SuperLearner(sl_gamma_tilde, newdata = X_test_mat, onlySL = T)$pred
      df$gamma_tilde_hat[test_ids] <- gamma_tilde_hat
      
    } 
    
    cat('Fitting CHI_TILDE on Split [', i, '/', n_splits, ']\n', sep = '')
    if(chi_tilde$type == 'super_learner') {
      ### Create datasets to train super learner
      Y_train <- 
        df_train %>% 
        filter(R == 1, A == 0) %>% 
        pull(!!chi_tilde$Y)
      
      X_train <- 
        df_train %>% 
        filter(R == 1, A == 0) %>% 
        select(all_of(chi_tilde$X)) 
      
      X_train_mat <- model.matrix(chi_tilde$formula, X_train)[,-1] ### Rm intercept term since some other models will add it
      colnames(X_train_mat) <- gsub(':', '_', colnames(X_train_mat))
      
      ### Set to predict on
      X_test <- 
        df_test %>% 
        select(all_of(chi_tilde$X)) 
      X_test_mat <- clean_test_matrix(model.matrix(chi_tilde$formula, X_test)[,-1], X_train_mat)
      
      
      ### Random Forest Hyperparameters
      mtry_seq <- floor(sqrt(ncol(X_train_mat)) * c(0.5, 1, 2))
      tree_seq <- c(250, 500, 1000)
      min_n_seq <- c(5, 30, 50)
      rf_learners <-
        create.Learner("SL.ranger", tune = list('mtry' = mtry_seq,
                                                'num.trees' = tree_seq,
                                                'min.node.size' = min_n_seq))
      
      sl_chi_tilde <- 
        SuperLearner(Y = Y_train,
                     X = X_train_mat, 
                     SL.library = c(chi_tilde$sl_libs, rf_learners$names))
      
      chi_tilde_hat <- predict.SuperLearner(sl_chi_tilde, newdata = X_test_mat, onlySL = T)$pred
      df$chi_tilde_hat[test_ids] <- chi_tilde_hat
      
    } 
  }
  
  ### Compute estimator
  tau_hat <-
    df %>% 
    ### need some value to get canceled by 0 that's not NA
    mutate('eligible' = replace(eligible, R == 0, 0),
           'mu0_hat' = replace(mu0_hat, R == 0, 0),
           'mu0_hat' = replace(mu0_hat, R == 1 & is.na(mu0_hat), 0),
           'u_hat' = replace(u_hat, R == 0, 0),
           'u_hat' = replace(u_hat, R == 1 & is.na(u_hat), 0)
    ) %>%
    ### Compute numerator and denominator one step estimators (eg., uncentered influence functions)
    mutate('alpha_OS' = A * (1 - R/eta1_hat) * epsilon1_star_hat + A * R * eligible/eta1_hat,
           'beta_OS' = 
             A * (epsilon1_star_hat * remission - xi_hat)
           + A * R/eta1_hat * ((eligible - epsilon1_star_hat) * remission - (eligible * mu0_hat - xi_hat))
           - (1-A) * R/eta1_hat * (eligible * u_hat/(1-u_hat) * (remission - mu0_hat) - (gamma_tilde_hat * remission - chi_tilde_hat))
           - (1-A) * eta0_hat/eta1_hat * ((gamma_tilde_hat * remission - chi_tilde_hat))
    ) %>% 
    ### Subject Specific Contributions (centered)
    mutate('alpha_IF' = alpha_OS - mean(alpha_OS),
           'beta_IF' = beta_OS - mean(beta_OS)) %>% 
    
    ### Overall ATT IF Subject Contribution (eg. following Takatsu 2023 Lemma S1 or Kennedy 2022 Review Example 6) 
    mutate('subject_IF'= beta_IF/mean(alpha_OS) - mean(beta_OS)/mean(alpha_OS)^2 * alpha_IF) %>%
    
    ### Estimates of ATT and Variance
    summarise('att_hat' = mean(beta_OS)/mean(alpha_OS),
              'sd' = sd(subject_IF)/sqrt(nrow(df)))
  
  
  
  return(tau_hat)
}


### Model Instructions
model_list <- 
  list(
    ### R ~ Lc + A
    'eta' = list('type' = 'super_learner',
                 'stratify' = F,
                 'Y' = 'R', 
                 'X' = c('site', 'gender', 'baseline_age', 'smoking_status', 'eGFR', 'race', 'hypertension', 'dyslipidemia', 'calendar_year', 'A'),
                 'formula' = ~site + gender + baseline_age + smoking_status + eGFR + race + hypertension + dyslipidemia + calendar_year + A,
                 'sl_libs' = c('SL.glm', 'SL.gam')),
    
    ### E ~ Lc + A + Y | R = 1
    'epsilon_star' = list('type' = 'super_learner',
                          'Y' = 'eligible',
                          'X' = c('site', 'gender', 'baseline_age', 'smoking_status', 'eGFR', 'race', 'hypertension', 'dyslipidemia', 'calendar_year', 'A', 'remission'),
                          'formula' = ~site + gender + baseline_age + smoking_status + eGFR + race + hypertension + dyslipidemia + calendar_year + A + remission,
                          'sl_libs' = c('SL.glm', 'SL.gam')),
    
    ### Y ~ Lc + Le + A  | R = 1 (usually fit amongst E = 1 as well) 
    'mu' = list('type' = 'super_learner', 
                'stratify' = F,
                'elig_only' = T,
                'Y' = 'remission',
                'X' = c('site', 'gender', 'baseline_age', 'smoking_status', 'eGFR', 'race', 'hypertension', 'dyslipidemia', 'calendar_year', 'baseline_bmi', 'baseline_a1c', 'DiaRem', 'A'),
                'formula' = ~site + gender + baseline_age + smoking_status + eGFR + race + hypertension + dyslipidemia + calendar_year + baseline_bmi + baseline_a1c + DiaRem + A,
                'sl_libs' = c('SL.glm', 'SL.gam')),
    
    ### A ~ Lc + Le | R = 1
    'u' = list('type' = 'super_learner',
               'elig_only' = T,
               'Y' = 'A',
               'X' = c('site', 'gender', 'baseline_age', 'smoking_status', 'eGFR', 'race', 'hypertension', 'dyslipidemia', 'calendar_year', 'baseline_bmi', 'baseline_a1c', 'DiaRem'),
               'formula' = ~site + gender + baseline_age + smoking_status + eGFR + race + hypertension + dyslipidemia + calendar_year + baseline_bmi + baseline_a1c + DiaRem,
               'sl_libs' = c('SL.glm', 'SL.gam')),
    
    ### Emu0 ~ Lc, Y, A = 1, R = 1
    'xi' = list('type' = 'super_learner',
                'Y' = 'Emu0',
                'X' = c('site', 'gender', 'baseline_age', 'smoking_status', 'eGFR', 'race', 'hypertension', 'dyslipidemia', 'calendar_year', 'remission'),
                'formula' = ~site + gender + baseline_age + smoking_status + eGFR + race + hypertension + dyslipidemia + calendar_year + remission,
                'sl_libs' = c('SL.polymars', 'SL.lm')),
    
    ### E * u/(1-u) ~ Lc, A = 0, R = 1, Y
    'gamma_tilde' =  list('type' = 'super_learner',
                          'Y' = 'E_uratio',
                          'X' = c('site', 'gender', 'baseline_age', 'smoking_status', 'eGFR', 'race', 'hypertension', 'dyslipidemia', 'calendar_year', 'remission'),
                          'formula' = ~site + gender + baseline_age + smoking_status + eGFR + race + hypertension + dyslipidemia + calendar_year + remission,
                          'sl_libs' = c('SL.polymars', 'SL.lm')),
    
    ### E * mu0 * u/(1-u) ~ Lc, A = 0, R = 1, Y
    'chi_tilde' = list('type' = 'super_learner',
                       'Y' = 'Emu0_uratio',
                       'X' = c('site', 'gender', 'baseline_age', 'smoking_status', 'eGFR', 'race', 'hypertension', 'dyslipidemia', 'calendar_year', 'remission'),
                       'formula' = ~site + gender + baseline_age + smoking_status + eGFR + race + hypertension + dyslipidemia + calendar_year + remission,
                       'sl_libs' = c('SL.polymars', 'SL.lm'))
  )

### Run Analysis
set.seed(3810)
df_rygb <- 
  eif_estimator(df = mutate(df, 'A' = as.numeric(bs_type == 'RYGB')),
                models = model_list,
                n_splits = 2)

df_sleeve <- 
  eif_estimator(df = mutate(df, 'A' = as.numeric(bs_type == 'SLEEVE')),
                models = model_list,
                n_splits = 2)

df_results <- 
  df_id %>% 
  filter(scenario_id == s_id) %>% 
  mutate('att_rygb' = df_rygb$att_hat,
         'sd_rygb' = df_rygb$sd,
         'att_sleeve' = -df_sleeve$att_hat, ### Since we want an ATC like
         'sd_sleeve' = df_sleeve$sd,
         'estimator' = 'EIF Ratio Estimator')

write_csv(df_results, glue('data/application/aligned_t0/diabetes_remission/EIF_{s_id}.csv'))
