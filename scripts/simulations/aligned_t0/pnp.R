library(tidyverse)
library(arrow)
library(glue)
library(haven)
library(lubridate)
library(furrr)
library(SuperLearner)

source('scripts/helpers.R')
source('scripts/simulations/aligned_t0/generate_data.R')
source('scripts/simulations/aligned_t0/estimators.R')

### Set up Parallelization
n_cores <- 64
plan(future::multisession(workers = n_cores))
options(future.globals.maxSize = 100 * 1024^3)
set.seed(71231)


### Directory where EHR data is stored
ehr_dir <- '/n/haneuse_ehr_l3/V1.0'
data_dir <- '/n/haneuse_ehr_l3/V1.0/clean_datasets'

df_inform <- read_parquet(glue('{data_dir}/bs_case_population.parquet'))

sim_id <- 1
params <- read_rds(glue('data/simulations/aligned_t0/inputs/params_{sim_id}.rds'))
estimators <- read_rds('data/simulations/aligned_t0/inputs/estimators.rds')


### Simulate Datasets
cat('Simulate Datasets\n')
datasets <- 
  future_map(1:params$n_sims, 
             ~generate_data(params), 
             .options = furrr_options(seed = 74592))



### Which mu to compute
set.seed(1031)
df <- datasets[[1]] ### Just take a single sample dataset
est <- c(1, 7, 8, 9, 10, 11, 12)
est_names <- 
  c('correct_parametric', 'super_learner', 'super_learner_a0', 'super_learner_interaction',
    'super_learner_noLM', 'super_learner_a0_noLM', 'super_learner_interaction_noLM')


### Sample split
n_splits <-  2
split_ids <- 
  split(1:nrow(df), ceiling(1:nrow(df) / ceiling(nrow(df)/n_splits)))

df_preds <- 
  as_tibble(matrix(nrow = nrow(df), ncol = length(est_names))) %>% 
  set_names(est_names)

for(j in est) {
  cat(j, '\n')
  mu <- estimators[[j]]$mu
  
  for(i in 1:n_splits) {
    train_ids <- split_ids[[i]]
    test_ids <- setdiff(1:nrow(df), train_ids)
    df_train <- dplyr::slice(df, train_ids)
    df_test <- dplyr::slice(df, test_ids)
    
    
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
      
      
    }
    
    df_preds[test_ids, which(est == j)] <- mu0_hat
  }
  
}


### Plot Result
df_plot <- 
  df_preds %>% 
  bind_cols(select(df, smoking_status, site)) %>% 
  pivot_longer(cols = -c('correct_parametric', 'smoking_status', 'site'),
               names_to = 'model',
               values_to = 'pred') %>% 
  mutate(model = case_when(model == 'super_learner' ~ 'SL1 (LM, RF, Polymars) Fit on A = 0 and A = 1',
                           model == 'super_learner_a0' ~ 'SL1 (LM, RF, Polymars) Fit on A = 0 (Stratification)',
                           model == 'super_learner_interaction' ~ 'SL1 (LM, RF, Polymars) Fit w/ A x L Interactions',
                           model == 'super_learner_noLM' ~ 'SL2 (RF, Polymars) Fit on A = 0 and A = 1',
                           model == 'super_learner_a0_noLM' ~ 'SL2 (RF, Polymars) Fit on A = 0 (Stratification)',
                           model == 'super_learner_interaction_noLM' ~ 'SL2 (RF, Polymars) Fit w/ A x L Interactions',
                           T ~ model)) %>% 
  mutate(model = factor(model, levels = c(
    'SL1 (LM, RF, Polymars) Fit on A = 0 and A = 1',
    'SL1 (LM, RF, Polymars) Fit on A = 0 (Stratification)',
    'SL1 (LM, RF, Polymars) Fit w/ A x L Interactions',
    'SL2 (RF, Polymars) Fit on A = 0 and A = 1',
    'SL2 (RF, Polymars) Fit on A = 0 (Stratification)',
    'SL2 (RF, Polymars) Fit w/ A x L Interactions'
  )))




ggplot(df_plot, aes(x = correct_parametric, y = pred)) + 
  facet_wrap(~model) +
  geom_abline(slope = 1, intercept = 0, col = 'black', lty = 2) + 
  geom_point(aes(col = site, shape = smoking_status)) + 
  scale_x_continuous(limits = c(-0.5, 0)) + 
  scale_y_continuous(limits = c(-0.5, 0)) + 
  labs(x = expression(paste(hat(mu)[0], ' from Correct Parametric Model')),
       y = expression(paste(hat(mu)[0], ' from Nonparametric SuperLearner')),
       color = 'Site',
       shape = 'Smoking Status',
       title = 'Calibration of Outcome Model',
       subtitle = 'Example Simulated Dataset') +
  theme(legend.title = element_text(size = 16))
ggsave('figures/eda/mu0_calibration.png', height = 9/1.25, width = 16/1.25)
ggsave('figures/eda/mu0_calibration.pdf', height = 9/1.25, width = 16/1.25)




