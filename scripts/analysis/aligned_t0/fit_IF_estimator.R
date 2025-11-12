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
df <- 
  df_complete %>% 
  inner_join(df_id, by = c('bmi_lookback', 'diabetes_lookback', 'rx_lookback')) %>% 
  filter(scenario_id == s_id)


### if_estimator: function to compute IF estimator
### Argumements
###   df: data frame containing treatment variable (must be called A), 
###       eligibility (must be called eligible), ascertainment (must be called R), covariates, and outcome
###   models: list of specifications for each nuisance model. 
###   n_splits = # of splits to be used in sample splitting.
###
### Output: Data frame with treatment effect and standard error
if_estimator <- function(df, models, n_splits) {
  ### Unpack nuisance model instructions
  eta <- models$eta
  omega <- models$omega
  mu <- models$mu
  u <- models$u
  nu0 <- models$nu0
  nu1 <- models$nu1
  
  ### Create storage for predictions
  df <- 
    df %>% 
    mutate('eta1_hat' = NA,
           'eta0_hat' = NA,
           'omega1_hat' = NA,
           'mu0_hat' = NA,
           'u_hat' = NA,
           'nu0_hat' = NA,
           'nu1_hat' = NA)
  
  
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
    
    ### omega Star
    cat('Fitting omega on Split [', i, '/', n_splits, ']\n', sep = '')
    if(omega$type == 'GLM') {
      omega_fit <- glm(omega$formula, family = 'binomial', data = filter(df_train, R == 1))
      omega1_hat <- predict(omega_fit, newdata = mutate(df_test, 'A' = 1), type = 'response')
    } else if(omega$type == 'super_learner') {
      ### Create datasets to train super learner
      Y_train <- 
        df_train %>% 
        filter(R == 1) %>% 
        pull(!!omega$Y)
      
      X_train <- 
        df_train %>% 
        filter(R == 1) %>% 
        select(all_of(omega$X))
      
      ### Set to predict on
      X_test <- 
        df_test %>% 
        select(all_of(omega$X)) %>% 
        mutate('A' = 1)
      
      
      ### Random Forest Hyperparameters
      mtry_seq <- floor(sqrt(ncol(X_train)) * c(0.5, 1, 2))
      tree_seq <- c(250, 500, 1000)
      min_n_seq <- c(5, 30, 50)
      rf_learners <-
        create.Learner("SL.ranger", tune = list('mtry' = mtry_seq,
                                                'num.trees' = tree_seq,
                                                'min.node.size' = min_n_seq))
      sl_epslion <- 
        SuperLearner(Y = Y_train,
                     X = X_train, 
                     family = 'binomial',
                     SL.library = c(rf_learners$names, omega$sl_libs))
      
      omega1_hat <- 
        as.vector(predict.SuperLearner(sl_epslion, newdata = X_test, onlySL = T)$pred)
      
    }
    
    df$omega1_hat[test_ids] <- omega1_hat
    
    ### Fit Mu
    cat('Fitting MU0 on Split [', i, '/', n_splits, ']\n', sep = '')
    if(mu$type == 'LM') {
      mu_fit <- lm(mu$formula, data = filter(df_train, R == 1, eligible == 1 | !mu$elig_only, A == 0 | !mu$stratify))
      mu0_hat <- predict(mu_fit, newdata = mutate(df_test, 'A' = 0))
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
      cc_ids <- which(!is.na(X_test$baseline_a1c) & !is.na(X_test$baseline_bmi))
      X_test_mat <- clean_test_matrix(model.matrix(mu$formula, X_test)[,-1], X_train_mat)
      
      
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
                     X = X_train_mat, 
                     SL.library = c(rf_learners$names, mu$sl_libs))
      
      mu0_hat <- rep(NA, nrow(df_test))
      mu0_hat[cc_ids] <- predict.SuperLearner(sl_mu, newdata = X_test_mat, onlySL = T)$pred
      
      ### needed for Xi and Chi models 
      X_train_0 <- 
        df_train %>% 
        select(all_of(mu$X)) %>% 
        mutate('A' = 0)
      cc_ids <- which(!is.na(X_train_0$baseline_a1c) & !is.na(X_train_0$baseline_bmi))
      X_train_mat0 <- clean_test_matrix(model.matrix(mu$formula, X_train_0)[,-1], X_train_mat)
      df_train$mu0_hat[cc_ids] <- predict.SuperLearner(sl_mu, newdata = X_train_mat0, onlySL = T)$pred
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
      cc_ids <- which(!is.na(X_test$baseline_bmi) & !is.na(X_test$baseline_a1c))
      
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
                     X = X_train, 
                     family = 'binomial',
                     SL.library = c(rf_learners$names, u$sl_libs))
      
      u_hat <- rep(NA, nrow(df_test))
      u_hat[cc_ids] <- predict.SuperLearner(sl_u, newdata = X_test[cc_ids,], onlySL = T)$pred
      df$u_hat[test_ids] <- u_hat
      
      ### needed for Xi and Chi models 
      X_train_ <- 
        df_train %>% 
        select(all_of(u$X)) 
      
      cc_ids <- which(!is.na(X_train_$baseline_bmi) & !is.na(X_train_$baseline_a1c))
      df_train$u_hat[cc_ids] <- predict.SuperLearner(sl_u, newdata = X_train_[cc_ids,], onlySL = T)$pred
    }
    
    ### Create Compound Outcomes for IF Nuisance Functions
    df_train <- 
      df_train %>% 
      mutate('Emu0'= eligible * mu0_hat,
             'EY' = eligible * pct_wt_change) %>% 
      mutate('Emu0' = ifelse(eligible == 0, 0, Emu0))
    
    df_test <- 
      df_test %>% 
      mutate('Emu0'= eligible * mu0_hat,
             'EY' = eligible * pct_wt_change) %>% 
      mutate('Emu0' = ifelse(eligible == 0, 0, Emu0))
    
    cat('Fitting Nu0 on Split [', i, '/', n_splits, ']\n', sep = '')
    if(nu0$type == 'super_learner') {
      ### Create datasets to train super learner
      Y_train <- 
        df_train %>% 
        filter(R == 1, A == 1) %>% 
        pull(!!nu0$Y)
      
      X_train <- 
        df_train %>% 
        filter(R == 1, A == 1) %>% 
        select(all_of(nu0$X)) 
      
      X_train_mat <- model.matrix(nu0$formula, X_train)[,-1] ### Rm intercept term since some other models will add it
      colnames(X_train_mat) <- gsub(':', '_', colnames(X_train_mat))
      
      ### Set to predict on
      X_test <- 
        df_test %>% 
        select(all_of(nu0$X)) 
      X_test_mat <- clean_test_matrix(model.matrix(nu0$formula, X_test)[,-1], X_train_mat)
      
      
      ### Random Forest Hyperparameters
      mtry_seq <- floor(sqrt(ncol(X_train_mat)) * c(0.5, 1, 2))
      tree_seq <- c(250, 500, 1000)
      min_n_seq <- c(5, 30, 50)
      rf_learners <-
        create.Learner("SL.ranger", tune = list('mtry' = mtry_seq,
                                                'num.trees' = tree_seq,
                                                'min.node.size' = min_n_seq))
      
      sl_nu0 <- 
        SuperLearner(Y = Y_train,
                     X = X_train_mat, 
                     SL.library = c(nu0$sl_libs, rf_learners$names))
      
      nu0_hat <- predict.SuperLearner(sl_nu0, newdata = X_test_mat, onlySL = T)$pred
      df$nu0_hat[test_ids] <- nu0_hat
    }
    
    cat('Fitting Nu1 on Split [', i, '/', n_splits, ']\n', sep = '')
    if(nu1$type == 'super_learner') {
      ### Create datasets to train super learner
      Y_train <- 
        df_train %>% 
        filter(R == 1, A == 1) %>% 
        pull(!!nu1$Y)
      
      X_train <- 
        df_train %>% 
        filter(R == 1, A == 1) %>% 
        select(all_of(nu1$X)) 
      
      X_train_mat <- model.matrix(nu1$formula, X_train)[,-1] ### Rm intercept term since some other models will add it
      colnames(X_train_mat) <- gsub(':', '_', colnames(X_train_mat))
      
      ### Set to predict on
      X_test <- 
        df_test %>% 
        select(all_of(nu1$X)) 
      X_test_mat <- clean_test_matrix(model.matrix(nu1$formula, X_test)[,-1], X_train_mat)
      
      
      ### Random Forest Hyperparameters
      mtry_seq <- floor(sqrt(ncol(X_train_mat)) * c(0.5, 1, 2))
      tree_seq <- c(250, 500, 1000)
      min_n_seq <- c(5, 30, 50)
      rf_learners <-
        create.Learner("SL.ranger", tune = list('mtry' = mtry_seq,
                                                'num.trees' = tree_seq,
                                                'min.node.size' = min_n_seq))
      
      sl_nu1 <- 
        SuperLearner(Y = Y_train,
                     X = X_train_mat, 
                     SL.library = c(nu1$sl_libs, rf_learners$names))
      
      nu1_hat <- predict.SuperLearner(sl_nu1, newdata = X_test_mat, onlySL = T)$pred
      df$nu1_hat[test_ids] <- nu1_hat
    }
  }
  
  
  ### Compute estimator
  df_final <-
    df %>% 
    ### need some value to get canceled by 0 that's not NA
    mutate('eligible' = replace(eligible, R == 0, 0),
           'mu0_hat' = replace(mu0_hat, R == 0, 0),
           'mu0_hat' = replace(mu0_hat, R == 1 & is.na(mu0_hat), 0),
           'u_hat' = replace(u_hat, R == 0, 0),
           'u_hat' = replace(u_hat, R == 1 & is.na(u_hat), 0)
    ) %>%
    ### Compute numerator and denominator one step estimators (eg., uncentered influence functions)
    mutate('alpha_OS' = A * (1 - R/eta1_hat) * omega1_hat + A * R * eligible/eta1_hat,
           'beta_OS' = 
             A * (1 - R/eta1_hat) * (nu1_hat - nu0_hat) +
             A * R * eligible/eta1_hat * (pct_wt_change - mu0_hat) 
           - (1-A) * R * eligible/eta1_hat * u_hat/(1-u_hat) * (pct_wt_change - mu0_hat)) %>% 
    ### Subject Specific Contributions (centered)
    mutate('alpha_IF' = alpha_OS - mean(alpha_OS),
           'beta_IF' = beta_OS - mean(beta_OS)) %>% 
    
    ### Overall ATT IF Subject Contribution (eg. following Takatsu 2023 Lemma S1 or Kennedy 2022 Review Example 6) 
    mutate('subject_IF'= beta_IF/mean(alpha_OS) - mean(beta_OS)/mean(alpha_OS)^2 * alpha_IF) 
  
  ### Estimates of ATT and Variance
  tau_hat <- 
    df_final %>% 
    summarise('att_hat' = mean(beta_OS)/mean(alpha_OS),
              'sd' = sd(subject_IF)/sqrt(nrow(df)))
  
  ### Save Subject Specific Contributions
  if(!dir.exists( glue('{data_dir}/aligned_t0/outputs/') )) {
    dir.create(glue('{data_dir}/aligned_t0/outputs/'))
    dir.create(glue('{data_dir}/aligned_t0/outputs/weight_change'))
  }
  
  write_parquet(df_final, glue('{data_dir}/aligned_t0/outputs/weight_change/raw_IF_{s_id}.parquet'))
  
  return(tau_hat)
}


### Model Instructions for each nuisance function
### Each specification should contain the following structure
###       type = model_type. We strongly recommend super_learner. For select nusiance models parametric options also exist
###       Y = modeling target
###       X = covariates used in model
###       formula = model specification formula object
###       sl_libs = libraries to supply to super learner (in addition to combinations of SL.ranger)
###
###       Additional options exist for stratification by A (mu,eta) or fit among eligible population (mu, u) 
##
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
    'omega' = list('type' = 'super_learner',
                   'Y' = 'eligible',
                   'X' = c('site', 'gender', 'baseline_age', 'smoking_status', 'eGFR', 'race', 'hypertension', 'dyslipidemia', 'calendar_year', 'A'),
                   'formula' = ~site + gender + baseline_age + smoking_status + eGFR + race + hypertension + dyslipidemia + calendar_year + A,
                   'sl_libs' = c('SL.glm', 'SL.gam')),
    
    ### Y ~ Lc + Le + A  | R = 1 (usually fit amongst E = 1 as well) 
    'mu' = list('type' = 'super_learner', 
                'stratify' = F,
                'elig_only' = T,
                'Y' = 'pct_wt_change',
                'X' = c('site', 'gender', 'baseline_age', 'smoking_status', 'eGFR', 'race', 'hypertension', 'dyslipidemia', 'calendar_year', 'baseline_bmi', 'baseline_a1c', 'insulin', 'A'),
                'formula' = ~site + gender + baseline_age + smoking_status + eGFR + race + hypertension + dyslipidemia + calendar_year + baseline_bmi + baseline_a1c + insulin + A,
                'sl_libs' = c('SL.polymars', 'SL.lm')),
    
    ### A ~ Lc + Le | R = 1
    'u' = list('type' = 'super_learner',
               'elig_only' = T,
               'Y' = 'A',
               'X' = c('site', 'gender', 'baseline_age', 'smoking_status', 'eGFR', 'race', 'hypertension', 'dyslipidemia', 'calendar_year', 'baseline_bmi', 'baseline_a1c', 'insulin'),
               'formula' = ~site + gender + baseline_age + smoking_status + eGFR + race + hypertension + dyslipidemia + calendar_year + baseline_bmi + baseline_a1c + insulin,
               'sl_libs' = c('SL.glm', 'SL.gam')),
    
    ### EY ~ Lc | A = 1 R = 1
    'nu1' = list('type' = 'super_learner',
                 'Y' = 'EY',
                 'X' = c('site', 'gender', 'baseline_age', 'smoking_status', 'eGFR', 'race', 'hypertension', 'dyslipidemia', 'calendar_year'),
                 'formula' = ~site + gender + baseline_age + smoking_status + eGFR + race + hypertension + dyslipidemia + calendar_year,
                 'sl_libs' = c('SL.polymars', 'SL.lm')),
    
    'nu0' = list('type' = 'super_learner',
                 'Y' = 'Emu0',
                 'X' = c('site', 'gender', 'baseline_age', 'smoking_status', 'eGFR', 'race', 'hypertension', 'dyslipidemia', 'calendar_year'),
                 'formula' = ~site + gender + baseline_age + smoking_status + eGFR + race + hypertension + dyslipidemia + calendar_year,
                 'sl_libs' = c('SL.polymars', 'SL.lm'))
    
  )

### Run Analysis
set.seed(3810)
df_rygb <- 
  if_estimator(df = mutate(df, 'A' = as.numeric(bs_type == 'RYGB')),
               models = model_list,
               n_splits = 2)


df_results <- 
  df_id %>% 
  filter(scenario_id == s_id) %>% 
  mutate('att_rygb' = df_rygb$att_hat,
         'sd_rygb' = df_rygb$sd,
         'estimator' = 'IF Ratio Estimator')

write_csv(df_results, glue('data/application/aligned_t0/weight_change/IF_{s_id}.csv'))
