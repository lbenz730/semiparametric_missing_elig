library(tidyverse)
library(arrow)
library(glue)

### Directory where EHR data is stored
ehr_dir <- '/n/haneuse_ehr_l3/V1.0'
data_dir <- '/n/haneuse_ehr_l3/V1.0/clean_datasets'

### Combos
df_id <- 
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

mu0_formula <- 
  remission ~ 
  site + gender + baseline_age + smoking_status + eGFR + race + calendar_year + 
  hypertension + dyslipidemia + baseline_bmi + baseline_a1c + DiaRem + A + 
  A:gender + A:baseline_a1c + A:DiaRem + A:baseline_age

eta_formula <-
  R_remission ~ site + gender + baseline_age + smoking_status + eGFR + race + hypertension + dyslipidemia + calendar_year + A

### 1) Let A = 1 be RYGB
df$A <- as.numeric(df$bs_type == 'RYGB')

### 2) Fit Outcome model 
mu <- glm(mu0_formula, family = 'binomial', data = df)
df$mu0_hat <- predict(mu, newdata = mutate(df, A = 0), type = 'response')

### 3) Fit Missingness Model
eta <- glm(eta_formula, family = 'binomial', data = df)
df$eta1_hat <- predict(eta, newdata = mutate(df, 'A' = 1), type = 'response')  

tau_att <- 
  df %>% 
  ### need some value to get canceled by 0 that's not NA
  mutate('eligible_remission' = replace(eligible_remission, R_remission == 0, 0),
         'mu0_hat' = replace(mu0_hat, R_remission == 0, 0),
         'mu0_hat' = replace(mu0_hat, R_remission == 1 & eligible_remission == 0, 0),) %>% 
  ### Compute numerator and demoni
  summarise('alpha' = mean(A * eligible_remission * R_remission/eta1_hat),
            'beta' = mean(A * eligible_remission * R_remission/eta1_hat * (remission - mu0_hat))) %>% 
  mutate('att_rygb' = beta/alpha)

if(!dir.exists( glue('{data_dir}/aligned_t0/outputs/') )) {
  dir.create(glue('{data_dir}/aligned_t0/outputs/'))
  dir.create(glue('{data_dir}/aligned_t0/outputs/remission'))
}

write_parquet(df, glue('{data_dir}/aligned_t0/outputs/remission/raw_IWOR_{s_id}.parquet'))

### Boostrap SD
set.seed(104)
n_boot <- 1000
tau_boot_rygb <- rep(NA, n_boot)
for(b in 1:n_boot) {
  if(b %% 100 == 0) {
    cat('Boostrapping Iteration [', b, '/', n_boot, ']\n', sep = '')
  }
  ### Resample
  df_boot <- 
    df %>% 
    slice(sample(1:nrow(.), size = nrow(.), replace = T))
  
  ### Fit outcome model on boostrappped data
  mu_boot <- glm(mu0_formula, family = 'binomial',data = df_boot)
  df_boot$mu0_hat <- predict(mu_boot, newdata = mutate(df_boot, A = 0), type = 'response')
  eta_boot <- glm(eta_formula, family = 'binomial', data = df_boot)
  df_boot$eta1_hat <- predict(eta, newdata = mutate(df_boot, 'A' = 1), type = 'response') 
  
  tau_boot <- 
    df_boot %>% 
    ### need some value to get canceled by 0 that's not NA
    mutate('eligible_remission' = replace(eligible_remission, R_remission == 0, 0),
           'mu0_hat' = replace(mu0_hat, R_remission == 0, 0),
           'mu0_hat' = replace(mu0_hat, R_remission == 1 & eligible_remission == 0, 0),) %>% 
    ### Compute numerator and demoni
    summarise('alpha' = mean(A * eligible_remission * R_remission/eta1_hat),
              'beta' = mean(A * eligible_remission * R_remission/eta1_hat * (remission - mu0_hat))) %>% 
    mutate('att_rygb' = beta/alpha)
  
  tau_boot_rygb[b] <- tau_boot$att_rygb
  
}

df_results <- 
  df_id %>% 
  filter(scenario_id == s_id) %>% 
  select(scenario_id, everything()) %>% 
  mutate('att_rygb' = tau_att$att_rygb,
         'sd_rygb' = sd(tau_boot_rygb),
         'estimator' = 'Inverse Weighted Outcome Regression')

write_csv(df_results, glue('data/application/aligned_t0/diabetes_remission/IWOR_{s_id}.csv'))
          
          