### complete case outcome regression
### Possibly weighted by missingness prob
###
### df = dataset on which to run analysis.
### models = list of model specifications 
###
### Output: ATT estimate
ipw_cc_outcome_reg <- function(df, models) {
  mu <- models$mu
  eta <- models$eta
  
  ### Fit Outcome Regression
  if(mu$type == 'LM') {
    mu_fit <- lm(mu$formula, data = filter(df, eligible == 1))
    df$mu0_hat <- predict(mu_fit, newdata = mutate(df, 'bs_type' = 0))
  }
  
  ### Fit Missingness Model
  if(is.null(eta)) {
    df$eta1_hat <- 1 
  } else if(eta$type == 'GLM') {
    eta_fit <- glm(eta$formula, family = 'binomial', data = df)
    df$eta1_hat <- predict(eta_fit, data = mutate(df, 'bs_type' == 1), type = 'response')
  }
  
  ### Compute Estimator
  tau_hat <- 
    df %>% 
    ### need some value to get canceled by 0 that's not NA
    mutate('eligible' = replace(eligible, R == 0, 0),
           'mu0_hat' = replace(mu0_hat, R == 0, 0)) %>% 
    ### Compute numerator and demoni
    summarise('alpha' = mean(bs_type * eligible * R/eta1_hat),
              'beta' = mean(bs_type * eligible * R/eta1_hat * (pct_wt_change - mu0_hat))) %>% 
    mutate('att_hat' = beta/alpha) %>% 
    pull(att_hat)
  
  return(tau_hat)
}

### Non-Parametric version of IPW CC Outcome Estimator w/ cross fitting 
### for outcome and missingness models
### complete case outcome regression
### Possibly weighted by missingness prob
###
### df = dataset on which to run analysis.
### models = list of model specifications 
### n_splits = # of sample splits for cross fitting
###
### Output: ATT estimate
crossfit_ipw_cc_outcome_reg <- function(df, models, n_splits) {
  mu <- models$mu
  eta <- models$eta
  
  df <- 
    df %>% 
    mutate('eta1_hat' = NA,
           'eta0_hat' = NA,
           'mu0_hat' = NA)
  
  
  ### Sample split
  split_ids <- 
    split(1:nrow(df), ceiling(1:nrow(df) / ceiling(nrow(df)/n_splits)))
  
  for(i in 1:n_splits) {
    ### Switch to below if ever incrase n_splits > 2
    # test_ids <- split_ids[[i]]
    # train_ids <- setdiff(1:nrow(df), test_ids)
    train_ids <- split_ids[[i]]
    test_ids <- setdiff(1:nrow(df), train_ids)
    df_train <- dplyr::slice(df, train_ids)
    df_test <- dplyr::slice(df, test_ids)
    
    if(eta$type == 'super_learner') {
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
      
      ### Random Forest Hyperparameters
      mtry_seq <- floor(sqrt(ncol(X_train)) * c(0.5, 1))
      tree_seq <- c(250, 500)
      rf_learners <-
        create.Learner("SL.ranger", tune = list('mtry' = mtry_seq,
                                                'num.trees' = tree_seq))
      sl_eta <- 
        SuperLearner(Y = Y_train,
                     X = X_train, 
                     family = 'binomial',
                     SL.library = c(rf_learners$names, eta$sl_libs))
      
      eta1_hat <- predict.SuperLearner(sl_eta, newdata = mutate(X_test, 'bs_type' = 1), onlySL = T)$pred
      df$eta1_hat[test_ids] <- eta1_hat
    }
    
    if(mu$type == 'super_learner') {
      ### Create datasets to train super learner
      Y_train <- 
        df_train %>% 
        filter(R == 1, eligible == 1, bs_type == 0 | !mu$stratify) %>% 
        pull(!!mu$Y)
      
      X_train <- 
        df_train %>% 
        filter(R == 1, eligible == 1, bs_type == 0 | !mu$stratify) %>% 
        select(all_of(mu$X)) 
      
      X_train_mat <- model.matrix(mu$formula, X_train)[,-1] ### Rm intercept term since some other models will add it
      colnames(X_train_mat) <- gsub(':', '_', colnames(X_train_mat))
      
      ### Set to predict on
      X_test <- 
        df_test %>% 
        select(all_of(mu$X)) %>% 
        mutate('bs_type' = 0)
      cc_ids <- which(!is.na(X_test$baseline_a1c))
      X_test_mat <- clean_test_matrix(model.matrix(mu$formula, X_test)[,-1], X_train_mat)
      
      
      ### Random Forest Hyperparameters
      mtry_seq <- floor(sqrt(ncol(X_train_mat)) * c(0.5, 1, 2))
      tree_seq <- c(500, 1000)
      rf_learners <-
        create.Learner("SL.ranger", tune = list('mtry' = mtry_seq,
                                                'num.trees' = tree_seq))
      
      sl_mu <- 
        SuperLearner(Y = Y_train,
                     X = X_train_mat, 
                     SL.library = c(rf_learners$names, mu$sl_libs))
      
      mu0_hat <- rep(NA, nrow(df_test))
      mu0_hat[cc_ids] <- predict.SuperLearner(sl_mu, newdata = X_test_mat, onlySL = T)$pred
      df$mu0_hat[test_ids] <- mu0_hat
    }
  }
  
  ### Compute Estimator
  tau_hat <- 
    df %>% 
    ### need some value to get canceled by 0 that's not NA
    mutate('eligible' = replace(eligible, R == 0, 0),
           'mu0_hat' = replace(mu0_hat, R == 0, 0)) %>% 
    ### Compute numerator and demoni
    summarise('alpha' = mean(bs_type * eligible * R/eta1_hat),
              'beta' = mean(bs_type * eligible * R/eta1_hat * (pct_wt_change - mu0_hat))) %>% 
    mutate('att_hat' = beta/alpha) %>% 
    pull(att_hat)
  
  return(tau_hat)
}


### Version of IF Estimator 
###
### df = dataset on which to run analysis.
### models = list of model specifications. For additional details see OMOP data example
### n_splits = # of sample splits for cross fitting
###
### Output: ATT estimate + SD
if_estimator <- function(df, models, n_splits) {
  ### Unpack nuisance model instructions
  pi <- models$pi
  eta <- models$eta
  mu <- models$mu
  lambda <- models$lambda
  nu1 <- models$nu1
  nu0 <- models$nu0
  epsilon <- models$epsilon
  
  ### Create storage for predictions
  ### May need to alter this for non-parametric estimation
  df <- 
    df %>% 
    mutate('pi_hat' = NA,
           'eta1_hat' = NA,
           'eta0_hat' = NA,
           'lambda1_hat' = NA,
           'lambda0_hat' = NA,
           'lambda_ratio' = NA,
           'mu0_hat' = NA,
           'nu1_hat' = NA,
           'nu0_hat' = NA,
           'epsilon1_hat' = NA)
  
  
  ### Sample split
  split_ids <- 
    split(1:nrow(df), ceiling(1:nrow(df) / ceiling(nrow(df)/n_splits)))
  
  for(i in 1:n_splits) {
    ### Switch to below if ever incrase n_splits > 2
    # test_ids <- split_ids[[i]]
    # train_ids <- setdiff(1:nrow(df), test_ids)
    train_ids <- split_ids[[i]]
    test_ids <- setdiff(1:nrow(df), train_ids)
    df_train <- dplyr::slice(df, train_ids)
    df_test <- dplyr::slice(df, test_ids)
    
    ### Fit Pi
    if(models$pi$type == 'GLM') {
      pi_fit <- glm(models$pi$formula, family = 'binomial', data = df_train)
      pi_hat <- predict(pi_fit, newdata = df_test, type = 'response')
    } else if(models$pi$type == 'super_learner') {
      ### Create Datasets for Super Learner
      Y_train <- 
        df_train %>% 
        pull(!!models$pi$Y)
      
      X_train <- 
        df_train %>% 
        select(all_of(models$pi$X))
      
      X_test <- 
        df_test %>% 
        select(all_of(models$pi$X))
      
      ### Random Forest Hyperparameters
      mtry_seq <- floor(sqrt(ncol(X_train)) * c(0.5, 1))
      tree_seq <- c(250, 500)
      rf_learners <-
        create.Learner("SL.ranger", tune = list('mtry' = mtry_seq,
                                                'num.trees' = tree_seq))
      sl_pi <- 
        SuperLearner(Y = Y_train,
                     X = X_train, 
                     family = 'binomial',
                     SL.library = c(rf_learners$names, pi$sl_libs))
      
      pi_hat <- predict.SuperLearner(sl_pi, newdata = X_test, onlySL = T)$pred
      
    }
    
    df$pi_hat[test_ids] <- pi_hat
    
    ### Fit eta
    if(eta$type == 'GLM') {
      eta_fit <- glm(eta$formula, family = 'binomial', data = df_train)
      eta1_hat <- predict(eta_fit, newdata = mutate(df_test, 'bs_type' = 1), type = 'response')
      eta0_hat <- predict(eta_fit, newdata = mutate(df_test, 'bs_type' = 0), type = 'response')
    } else if(eta$type == 'super_learner') {
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
      
      ### Random Forest Hyperparameters
      mtry_seq <- floor(sqrt(ncol(X_train)) * c(0.5, 1))
      tree_seq <- c(250, 500)
      rf_learners <-
        create.Learner("SL.ranger", tune = list('mtry' = mtry_seq,
                                                'num.trees' = tree_seq))
      sl_eta <- 
        SuperLearner(Y = Y_train,
                     X = X_train, 
                     family = 'binomial',
                     SL.library = c(rf_learners$names, eta$sl_libs))
      
      eta1_hat <- predict.SuperLearner(sl_eta, newdata = mutate(X_test, 'bs_type' = 1), onlySL = T)$pred
      eta0_hat <- predict.SuperLearner(sl_eta, newdata = mutate(X_test, 'bs_type' = 0), onlySL = T)$pred
      
    }
    
    df$eta1_hat[test_ids] <- eta1_hat
    df$eta0_hat[test_ids] <- eta0_hat
    
    ### Fit Lambda
    if(lambda$type == 'GLM_gamma') {
      ### Parametric model for conditional density (if known) 
      lambda_fit <- glm(lambda$formula, family = Gamma(link = "log"), data = filter(df_train, R == 1))
      lambda_shape <- 1/summary(lambda_fit)$dispersion
      lambda1_hat <-
        dgamma(x = df_test$baseline_a1c - 3,
               shape = lambda_shape, 
               rate = lambda_shape/predict(lambda_fit, newdata = mutate(df_test, 'bs_type' = 1), type = 'response'))
      lambda0_hat <-
        dgamma(x = df_test$baseline_a1c - 3,
               shape = lambda_shape, 
               rate = lambda_shape/predict(lambda_fit, newdata = mutate(df_test, 'bs_type' = 0), type = 'response'))
      
      df$lambda1_hat[test_ids] <- lambda1_hat
      df$lambda0_hat[test_ids] <- lambda0_hat
      df$lambda_ratio[test_ids] <- lambda1_hat/lambda0_hat
    } else if(lambda$type == 'super_learner') {
      ### Create datasets to train super learner
      Y_train <- 
        df_train %>% 
        filter(R == 1) %>% 
        pull(!!lambda$Y)
      
      X_train <- 
        df_train %>% 
        filter(R == 1) %>% 
        select(all_of(lambda$X)) 
      
      ### Set to predict on
      X_test <- 
        df_test %>% 
        select(all_of(lambda$X)) 
      cc_ids <- which(!is.na(X_test$baseline_a1c))
      
      ### Random Forest Hyperparameters
      mtry_seq <- floor(sqrt(ncol(X_train)) * c(0.5, 1, 2))
      tree_seq <- c(250, 500)
      rf_learners <-
        create.Learner("SL.ranger", tune = list('mtry' = mtry_seq,
                                                'num.trees' = tree_seq))
      
      sl_u <- 
        SuperLearner(Y = Y_train,
                     X = X_train, 
                     family = 'binomial',
                     SL.library = c(rf_learners$names, lambda$sl_libs))
      
      u_hat <- rep(NA, nrow(df_test))
      u_hat[cc_ids] <- predict.SuperLearner(sl_u, newdata = X_test[cc_ids,], onlySL = T)$pred
      df$lambda_ratio[test_ids] <- u_hat/(1 - u_hat) * (1 - pi_hat)/pi_hat * eta0_hat/eta1_hat
      
    }
    
    ### Fit Mu
    if(mu$type == 'LM') {
      mu_fit <- lm(mu$formula, data = filter(df_train, R == 1, eligible == 1))
      mu0_hat <- predict(mu_fit, newdata = mutate(df_test, 'bs_type' = 0))
      df_train$mu0_hat <- predict(mu_fit, newdata = mutate(df_train, 'bs_type' = 0)) ### needed for nu0
    } else if(mu$type == 'super_learner') {
      ### Create datasets to train super learner
      Y_train <- 
        df_train %>% 
        filter(R == 1, eligible == 1, bs_type == 0 | !mu$stratify) %>% 
        pull(!!mu$Y)
      
      X_train <- 
        df_train %>% 
        filter(R == 1, eligible == 1, bs_type == 0 | !mu$stratify) %>% 
        select(all_of(mu$X)) 
      
      X_train_mat <- model.matrix(mu$formula, X_train)[,-1] ### Rm intercept term since some other models will add it
      colnames(X_train_mat) <- gsub(':', '_', colnames(X_train_mat))
      
      ### Set to predict on
      X_test <- 
        df_test %>% 
        select(all_of(mu$X)) %>% 
        mutate('bs_type' = 0)
      cc_ids <- which(!is.na(X_test$baseline_a1c))
      X_test_mat <- clean_test_matrix(model.matrix(mu$formula, X_test)[,-1], X_train_mat)
      
      
      ### Random Forest Hyperparameters
      mtry_seq <- floor(sqrt(ncol(X_train_mat)) * c(0.5, 1, 2))
      tree_seq <- c(500, 1000)
      rf_learners <-
        create.Learner("SL.ranger", tune = list('mtry' = mtry_seq,
                                                'num.trees' = tree_seq))
      
      sl_mu <- 
        SuperLearner(Y = Y_train,
                     X = X_train_mat, 
                     SL.library = c(rf_learners$names, mu$sl_libs))
      
      mu0_hat <- rep(NA, nrow(df_test))
      mu0_hat[cc_ids] <- predict.SuperLearner(sl_mu, newdata = X_test_mat, onlySL = T)$pred
      
      ### needed for nu0
      X_train_0 <- 
        df_train %>% 
        select(all_of(mu$X)) %>% 
        mutate('bs_type' = 0)
      cc_ids <- which(!is.na(X_train_0$baseline_a1c))
      X_train_mat0 <- clean_test_matrix(model.matrix(mu$formula, X_train_0)[,-1], X_train_mat)
      df_train$mu0_hat[cc_ids] <- predict.SuperLearner(sl_mu, newdata = X_train_mat0, onlySL = T)$pred
    }
    
    df$mu0_hat[test_ids] <- mu0_hat
    
    ### Epsilon 
    if(epsilon$type == 'GLM') {
      epsilon_fit <- glm(epsilon$formula, family = 'binomial', data = filter(df_train, R == 1))
      epsilon1_hat <- predict(epsilon_fit, newdata = mutate(df_test, 'bs_type' = 1), type = 'response')
    } else if(epsilon$type == 'super_learner') {
      ### Create datasets to train super learner
      Y_train <- 
        df_train %>% 
        filter(R == 1) %>% 
        pull(!!epsilon$Y)
      
      X_train <- 
        df_train %>% 
        filter(R == 1) %>% 
        select(all_of(epsilon$X))
      
      ### Set to predict on
      X_test <- 
        df_test %>% 
        select(all_of(epsilon$X)) %>% 
        mutate('bs_type' = 1)
      
      
      ### Random Forest Hyperparameters
      mtry_seq <- floor(sqrt(ncol(X_train)) * c(0.5, 1))
      tree_seq <- c(250, 500)
      rf_learners <-
        create.Learner("SL.ranger", tune = list('mtry' = mtry_seq,
                                                'num.trees' = tree_seq))
      
      sl_epslion <- 
        SuperLearner(Y = Y_train,
                     X = X_train, 
                     family = 'binomial',
                     SL.library = c(rf_learners$names, epsilon$sl_libs))
      
      epsilon1_hat <- 
        as.vector(predict.SuperLearner(sl_epslion, newdata = X_test, onlySL = T)$pred)
      
    }
    
    df$epsilon1_hat[test_ids] <- epsilon1_hat
    
    ### Nu0/nu1
    df_train <- 
      df_train %>% 
      mutate('EY' = eligible * pct_wt_change,
             'Emu0'= eligible * mu0_hat)
    
    df_test <- 
      df_test %>% 
      mutate('EY' = eligible * pct_wt_change,
             'Emu0'= eligible * mu0_hat)
    
    if(nu1$type == 'mixture') {
      ### Fit mu1_tilde 
      if(nu1$mu_tilde_1$type == 'LM') {
        x_train <- 
          df_train %>% 
          filter(R == 1, eligible == 1, bs_type == 1)
        x_test <- df_test 
        
        ### Hack for cases where current smokers very rare in this strata and this isn't handled by our matrix opertaions for the non-parametric modeling
        if('current' %in% unique(x_test$smoking_status) & !('current' %in% unique(x_train$smoking_status))) {
          x_test$smoking_status[x_test$smoking_status == 'current'] <- 'former'
        } 
        
        if('no_self_report' %in% unique(x_test$smoking_status) & !('no_self_report' %in% unique(x_train$smoking_status))) {
          x_test$smoking_status[x_test$smoking_status == 'no_self_report'] <- 'former'
        } 
        mu_tilde_1_fit <- lm(nu1$mu_tilde_1$formula, data = x_train)
        mu_tilde_1_hat <- predict(mu_tilde_1_fit, newdata = x_test)
        df$nu1_hat[test_ids] <- epsilon1_hat * mu_tilde_1_hat
      }
    } else if(nu1$type == 'super_learner') {
      ### Create datasets to train super learner
      Y_train <- 
        df_train %>% 
        filter(R == 1, bs_type == 1) %>% 
        pull(!!nu1$Y)
      
      X_train <- 
        df_train %>% 
        filter(R == 1, bs_type == 1) %>% 
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
      tree_seq <- c(500, 1000)
      rf_learners <-
        create.Learner("SL.ranger", tune = list('mtry' = mtry_seq,
                                                'num.trees' = tree_seq))
      
      sl_nu1 <- 
        SuperLearner(Y = Y_train,
                     X = X_train_mat, 
                     SL.library = c(nu1$sl_libs, rf_learners$names))
      
      nu1_hat <- predict.SuperLearner(sl_nu1, newdata = X_test_mat, onlySL = T)$pred
      df$nu1_hat[test_ids] <- nu1_hat
      
    }
    
    if(nu0$type == 'mixture') {
      ### Fit mu1_tilde 
      if(nu0$mu_tilde_0$type == 'LM') {
        x_train <- 
          df_train %>% 
          filter(R == 1, eligible == 1, bs_type == 1)
        x_test <- df_test 
        
        ### Hack for cases where current smokers very rare in this strata and this isn't handled by our matrix opertaions for the non-parametric modeling
        if('current' %in% unique(x_test$smoking_status) & !('current' %in% unique(x_train$smoking_status))) {
          x_test$smoking_status[x_test$smoking_status == 'current'] <- 'former'
        }
        if('no_self_report' %in% unique(x_test$smoking_status) & !('no_self_report' %in% unique(x_train$smoking_status))) {
          x_test$smoking_status[x_test$smoking_status == 'no_self_report'] <- 'former'
        } 
        
        mu_tilde_0_fit <- lm(nu0$mu_tilde_0$formula, data = x_train)
        mu_tilde_0_hat <- predict(mu_tilde_0_fit, newdata = x_test)
        df$nu0_hat[test_ids] <- epsilon1_hat * mu_tilde_0_hat
      }
    } else if(nu0$type == 'super_learner') {
      ### Create datasets to train super learner
      Y_train <- 
        df_train %>% 
        filter(R == 1, bs_type == 1) %>% 
        pull(!!nu0$Y)
      
      X_train <- 
        df_train %>% 
        filter(R == 1, bs_type == 1) %>% 
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
      tree_seq <- c(500, 1000)
      rf_learners <-
        create.Learner("SL.ranger", tune = list('mtry' = mtry_seq,
                                                'num.trees' = tree_seq))
      
      sl_nu0 <- 
        SuperLearner(Y = Y_train,
                     X = X_train_mat, 
                     SL.library = c(nu0$sl_libs, rf_learners$names))
      
      nu0_hat <- predict.SuperLearner(sl_nu0, newdata = X_test_mat, onlySL = T)$pred
      df$nu0_hat[test_ids] <- nu0_hat
      
    }
  }
  
  ### Compute estimator
  tau_hat <-
    df %>% 
    ### need some value to get canceled by 0 that's not NA
    mutate('eligible' = replace(eligible, R == 0, 0),
           'mu0_hat' = replace(mu0_hat, R == 0, 0),
           'lambda_ratio' = replace(lambda_ratio, R == 0, 0)) %>%
    ### Compute numerator and demoninator
    mutate('alpha_OS' = bs_type * (1 - R/eta1_hat) * epsilon1_hat + bs_type * R * eligible/eta1_hat,
           'beta_OS' = bs_type * (1 - R/eta1_hat) * (nu1_hat - nu0_hat) +
             bs_type * R * eligible/eta1_hat * (pct_wt_change - mu0_hat)
           - (1-bs_type) * R * eligible/eta0_hat * (pi_hat/(1-pi_hat)) * lambda_ratio * (pct_wt_change - mu0_hat)) %>%
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



### Non-Parametric version of EIF Estimator 
###
### df = dataset on which to run analysis.
### models = list of model specifications. For additional details see OMOP data example
### n_splits = # of sample splits for cross fitting
###
### Output: ATT estimate + SD
eif_estimator <- function(df, models, n_splits) {
  ### Unpack nuisance model instructions
  eta <- models$eta
  epsilon_star <- models$epsilon_star
  mu <- models$mu
  xi <- models$xi
  u <- models$u
  gamma_tilde <- models$gamma_tilde
  chi_star_tilde <- models$chi_star_tilde
  
  # ### Alternative Route where one parametrically estimates density ratio
  # lambda <- models$lambda
  # pi <- models$pi
  # gamma <- models$gamma
  # chi_star <- models$chi_star
  # delta <- models$delta
  # chi <- models$chi 
  
  ### Create storage for predictions
  ### May need to alter this for non-parametric estimation
  df <- 
    df %>% 
    mutate('eta1_hat' = NA,
           'eta0_hat' = NA,
           'epsilon1_star_hat' = NA,
           'mu0_hat' = NA,
           'xi_hat' = NA,
           'u_hat' = NA,
           'gamma_tilde_hat' = NA,
           'chi_star_tilde_hat' = NA)
  
  
  ### Sample split
  split_ids <- 
    split(1:nrow(df), ceiling(1:nrow(df) / ceiling(nrow(df)/n_splits)))
  
  for(i in 1:n_splits) {
    ### Switch to below if ever incrase n_splits > 2
    # test_ids <- split_ids[[i]]
    # train_ids <- setdiff(1:nrow(df), test_ids)
    train_ids <- split_ids[[i]]
    test_ids <- setdiff(1:nrow(df), train_ids)
    df_train <- dplyr::slice(df, train_ids)
    df_test <- dplyr::slice(df, test_ids)
    
    ### Fit eta
    if(eta$type == 'GLM') {
      eta_fit <- glm(eta$formula, family = 'binomial', data = df_train)
      eta1_hat <- predict(eta_fit, newdata = mutate(df_test, 'bs_type' = 1), type = 'response')
      eta0_hat <- predict(eta_fit, newdata = mutate(df_test, 'bs_type' = 0), type = 'response')
    } else if(eta$type == 'super_learner') {
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
      
      ### Random Forest Hyperparameters
      mtry_seq <- floor(sqrt(ncol(X_train)) * c(0.5, 1))
      tree_seq <- c(250, 500)
      rf_learners <-
        create.Learner("SL.ranger", tune = list('mtry' = mtry_seq,
                                                'num.trees' = tree_seq))
      sl_eta <- 
        SuperLearner(Y = Y_train,
                     X = X_train, 
                     family = 'binomial',
                     SL.library = c(rf_learners$names, eta$sl_libs))
      
      eta1_hat <- predict.SuperLearner(sl_eta, newdata = mutate(X_test, 'bs_type' = 1), onlySL = T)$pred
      eta0_hat <- predict.SuperLearner(sl_eta, newdata = mutate(X_test, 'bs_type' = 0), onlySL = T)$pred
      
    }
    
    df$eta1_hat[test_ids] <- eta1_hat
    df$eta0_hat[test_ids] <- eta0_hat
    
    ### Epsilon Star
    if(epsilon_star$type == 'GLM') {
      epsilon_star_fit <- glm(epsilon_star$formula, family = 'binomial', data = filter(df_train, R == 1))
      epsilon1_star_hat <- predict(epsilon_star_fit, newdata = mutate(df_test, 'bs_type' = 1), type = 'response')
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
        mutate('bs_type' = 1)
      
      
      ### Random Forest Hyperparameters
      mtry_seq <- floor(sqrt(ncol(X_train)) * c(0.5, 1))
      tree_seq <- c(250, 500)
      rf_learners <-
        create.Learner("SL.ranger", tune = list('mtry' = mtry_seq,
                                                'num.trees' = tree_seq))
      
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
    if(mu$type == 'LM') {
      mu_fit <- lm(mu$formula, data = filter(df_train, R == 1, eligible == 1))
      mu0_hat <- predict(mu_fit, newdata = mutate(df_test, 'bs_type' = 0))
      df_train$mu0_hat <- predict(mu_fit, newdata = mutate(df_train, 'bs_type' = 0)) ### needed for other models
    } else if(mu$type == 'super_learner') {
      ### Create datasets to train super learner
      Y_train <- 
        df_train %>% 
        filter(R == 1, eligible == 1, bs_type == 0 | !mu$stratify) %>% 
        pull(!!mu$Y)
      
      X_train <- 
        df_train %>% 
        filter(R == 1, eligible == 1, bs_type == 0 | !mu$stratify) %>% 
        select(all_of(mu$X)) 
      
      X_train_mat <- model.matrix(mu$formula, X_train)[,-1] ### Rm intercept term since some other models will add it
      colnames(X_train_mat) <- gsub(':', '_', colnames(X_train_mat))
      
      ### Set to predict on
      X_test <- 
        df_test %>% 
        select(all_of(mu$X)) %>% 
        mutate('bs_type' = 0)
      cc_ids <- which(!is.na(X_test$baseline_a1c))
      X_test_mat <- clean_test_matrix(model.matrix(mu$formula, X_test)[,-1], X_train_mat)
      
      
      ### Random Forest Hyperparameters
      mtry_seq <- floor(sqrt(ncol(X_train_mat)) * c(0.5, 1, 2))
      tree_seq <- c(500, 1000)
      rf_learners <-
        create.Learner("SL.ranger", tune = list('mtry' = mtry_seq,
                                                'num.trees' = tree_seq))
      
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
        mutate('bs_type' = 0)
      cc_ids <- which(!is.na(X_train_0$baseline_a1c))
      X_train_mat0 <- clean_test_matrix(model.matrix(mu$formula, X_train_0)[,-1], X_train_mat)
      df_train$mu0_hat[cc_ids] <- predict.SuperLearner(sl_mu, newdata = X_train_mat0, onlySL = T)$pred
    }
    
    df$mu0_hat[test_ids] <- mu0_hat
    
    ### Fit U
    if(u$type == 'GLM') {
      ### Parametric model for u
      u_fit <- glm(u$formula, family = 'binomial', data = filter(df_train, R == 1))
      u_hat <- predict(u_fit, newdata = df_test, type = 'response')
      df_train$u_hat <- predict(u_fit, newdata = df_train, type = 'response')
    } else if(u$type == 'super_learner') {
      ### Create datasets to train super learner
      Y_train <- 
        df_train %>% 
        filter(R == 1) %>% 
        pull(!!u$Y)
      
      X_train <- 
        df_train %>% 
        filter(R == 1) %>% 
        select(all_of(u$X)) 
      
      ### Set to predict on
      X_test <- 
        df_test %>% 
        select(all_of(u$X)) 
      cc_ids <- which(!is.na(X_test$baseline_a1c))
      
      ### Random Forest Hyperparameters
      mtry_seq <- floor(sqrt(ncol(X_train)) * c(0.5, 1, 2))
      tree_seq <- c(250, 500)
      rf_learners <-
        create.Learner("SL.ranger", tune = list('mtry' = mtry_seq,
                                                'num.trees' = tree_seq))
      
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
      
      cc_ids <- which(!is.na(X_train_$baseline_a1c))
      df_train$u_hat[cc_ids] <- predict.SuperLearner(sl_u, newdata = X_train_[cc_ids,], onlySL = T)$pred
    }
    
    ### Create Compound Outcomes for EIF Nuisance Functions
    df_train <- 
      df_train %>% 
      mutate('Emu0'= eligible * mu0_hat,
             'E_uratio' = eligible * u_hat/(1-u_hat),
             'EY_uratio' = eligible * pct_wt_change * u_hat/(1-u_hat),
             'Emu0_uratio' = eligible * mu0_hat * u_hat/(1-u_hat))
    
    df_test <- 
      df_test %>% 
      mutate('Emu0'= eligible * mu0_hat,
             'E_uratio' = eligible * u_hat/(1-u_hat),
             'EY_uratio' = eligible * pct_wt_change * u_hat/(1-u_hat),
             'Emu0_uratio' = eligible * mu0_hat * u_hat/(1-u_hat))
    
    if(xi$type == 'super_learner') {
      ### Create datasets to train super learner
      Y_train <- 
        df_train %>% 
        filter(R == 1, bs_type == 1) %>% 
        pull(!!xi$Y)
      
      X_train <- 
        df_train %>% 
        filter(R == 1, bs_type == 1) %>% 
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
      tree_seq <- c(500, 1000)
      rf_learners <-
        create.Learner("SL.ranger", tune = list('mtry' = mtry_seq,
                                                'num.trees' = tree_seq))
      
      sl_xi <- 
        SuperLearner(Y = Y_train,
                     X = X_train_mat, 
                     SL.library = c(xi$sl_libs, rf_learners$names))
      
      xi_hat <- predict.SuperLearner(sl_xi, newdata = X_test_mat, onlySL = T)$pred
      df$xi_hat[test_ids] <- xi_hat
    }
    
    if(gamma_tilde$type == 'super_learner') {
      ### Create datasets to train super learner
      Y_train <- 
        df_train %>% 
        filter(R == 1, bs_type == 0) %>% 
        pull(!!gamma_tilde$Y)
      
      X_train <- 
        df_train %>% 
        filter(R == 1, bs_type == 0) %>% 
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
      tree_seq <- c(500, 1000)
      rf_learners <-
        create.Learner("SL.ranger", tune = list('mtry' = mtry_seq,
                                                'num.trees' = tree_seq))
      
      sl_gamma_tilde <- 
        SuperLearner(Y = Y_train,
                     X = X_train_mat, 
                     SL.library = c(gamma_tilde$sl_libs, rf_learners$names))
      
      gamma_tilde_hat <- predict.SuperLearner(sl_gamma_tilde, newdata = X_test_mat, onlySL = T)$pred
      df$gamma_tilde_hat[test_ids] <- gamma_tilde_hat
      
    } 
    
    if(chi_star_tilde$type == 'super_learner') {
      ### Create datasets to train super learner
      Y_train <- 
        df_train %>% 
        filter(R == 1, bs_type == 0) %>% 
        pull(!!chi_star_tilde$Y)
      
      X_train <- 
        df_train %>% 
        filter(R == 1, bs_type == 0) %>% 
        select(all_of(chi_star_tilde$X)) 
      
      X_train_mat <- model.matrix(chi_star_tilde$formula, X_train)[,-1] ### Rm intercept term since some other models will add it
      colnames(X_train_mat) <- gsub(':', '_', colnames(X_train_mat))
      
      ### Set to predict on
      X_test <- 
        df_test %>% 
        select(all_of(chi_star_tilde$X)) 
      X_test_mat <- clean_test_matrix(model.matrix(chi_star_tilde$formula, X_test)[,-1], X_train_mat)
      
      
      ### Random Forest Hyperparameters
      mtry_seq <- floor(sqrt(ncol(X_train_mat)) * c(0.5, 1, 2))
      tree_seq <- c(500, 1000)
      rf_learners <-
        create.Learner("SL.ranger", tune = list('mtry' = mtry_seq,
                                                'num.trees' = tree_seq))
      
      sl_chi_star_tilde <- 
        SuperLearner(Y = Y_train,
                     X = X_train_mat, 
                     SL.library = c(chi_star_tilde$sl_libs, rf_learners$names))
      
      chi_star_tilde_hat <- predict.SuperLearner(sl_chi_star_tilde, newdata = X_test_mat, onlySL = T)$pred
      df$chi_star_tilde_hat[test_ids] <- chi_star_tilde_hat
      
    } 
    
    
  }
  
  ### Compute estimator
  if(is.null(models$lambda)) {
    tau_hat <-
      df %>% 
      ### need some value to get canceled by 0 that's not NA
      mutate('eligible' = replace(eligible, R == 0, 0),
             'mu0_hat' = replace(mu0_hat, R == 0, 0),
             'u_hat' = replace(u_hat, R == 0, 0)) %>%
      ### Compute numerator and demoni
      mutate('alpha_OS' = bs_type * (1 - R/eta1_hat) * epsilon1_star_hat + bs_type * R * eligible/eta1_hat,
             'beta_OS' = bs_type * (epsilon1_star_hat * pct_wt_change - xi_hat)
             + bs_type * R/eta1_hat * ((eligible - epsilon1_star_hat) * pct_wt_change - (eligible * mu0_hat - xi_hat)) 
             - (1-bs_type) * R/eta1_hat * (eligible * u_hat/(1-u_hat) * (pct_wt_change - mu0_hat) - (gamma_tilde_hat * pct_wt_change - chi_star_tilde_hat))
             - (1-bs_type) * eta0_hat/eta1_hat * ((gamma_tilde_hat * pct_wt_change - chi_star_tilde_hat))) %>%
      
      ### Subject Specific Contributions (centered)
      mutate('alpha_IF' = alpha_OS - mean(alpha_OS),
             'beta_IF' = beta_OS - mean(beta_OS)) %>% 
      
      ### Overall ATT IF Subject Contribution (eg. following Takatsu 2023 Lemma S1 or Kennedy 2022 Review Example 6) 
      mutate('subject_IF'= beta_IF/mean(alpha_OS) - mean(beta_OS)/mean(alpha_OS)^2 * alpha_IF) %>%
      ### Estimates of ATT and Variance
      summarise('att_hat' = mean(beta_OS)/mean(alpha_OS),
                'sd' = sd(subject_IF)/sqrt(nrow(df)))
  }
  
  return(tau_hat)
}



### Wrapper function to fit a certain estimator to all datasets
### datasets: list of simulated datasets (generated by generate_data function in generate_data.R)
### estimator_info: list of fitting instructures to pass to component functions when estimating nuisaces
### returns: dataset summarizing estimates of ATT across simulation settings
fit_estimator <- function(datasets, estimator_info) {
  if(estimator_info$estimator %in% c('cc_outcome', 'iwor_cc')) {
    tau_hat <- 
      future_map_dbl(datasets, 
                     ~ipw_cc_outcome_reg(df = .x, models = estimator_info),
                     .options = furrr_options(seed = 18301))
  }
  
  if(estimator_info$estimator == 'crossfit_iwor_cc') {
    tau_hat <- 
      future_map_dbl(datasets, 
                     ~crossfit_ipw_cc_outcome_reg(df = .x, models = estimator_info, n_splits = estimator_info$n_splits),
                     .options = furrr_options(seed = 3041))
    
  }
  
  if(estimator_info$estimator %in% c('influence_function')) {
    tau_hat <- 
      future_map_dfr(datasets, 
                     ~if_estimator(df = .x, models = estimator_info, n_splits = estimator_info$n_splits), 
                     .options = furrr_options(seed = 18414301))
  }
  
  if(estimator_info$estimator %in% c('efficient_influence_function')) {
    tau_hat <- 
      future_map_dfr(datasets, 
                     ~eif_estimator(df = .x, models = estimator_info, n_splits = estimator_info$n_splits), 
                     .options = furrr_options(seed = 272891))
    
  }
  
  if(estimator_info$estimator %in% c('cc_outcome', 'iwor_cc', 'crossfit_iwor_cc')) {
    df_results <- 
      tibble('dataset_id' = 1:length(datasets),
             'estimator' = estimator_info$estimator,
             'description' = estimator_info$description,
             'correct_models' = paste0(estimator_info$true_models, collapse = '/'),
             'tau_hat' = tau_hat)
  } else {
    df_results <- 
      tibble('dataset_id' = 1:length(datasets),
             'estimator' = estimator_info$estimator,
             'description' = estimator_info$description,
             'correct_models' = paste0(estimator_info$true_models, collapse = '/'),
             'tau_hat_' = tau_hat$att_hat,
             'sd' = tau_hat$sd) %>% 
      rename('tau_hat' = tau_hat_)
  }
  
  return(df_results)
}

