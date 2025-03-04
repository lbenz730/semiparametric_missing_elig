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

### Complete Case Dataset
df_cc <- 
  df %>% 
  filter(eligible == 1, R == 1) 

mu0_formula <- 
  pct_wt_change ~ 
  site + gender + baseline_age + smoking_status + eGFR + race + calendar_year + 
  hypertension + dyslipidemia + baseline_bmi + baseline_a1c + insulin + A + 
  A:race + A:baseline_bmi + A:gender + A:baseline_age

### 1) Let A = 1 be RYGB
df_cc$A <- as.numeric(df_cc$bs_type == 'RYGB')

### 2) Fit Outcome model 
mu <- lm(mu0_formula, data = df_cc)
df_cc$mu0_hat <- predict(mu, newdata = mutate(df_cc, A = 0))

tau_att <- 
  df_cc %>% 
  summarise('att_rygb' = mean( (pct_wt_change - mu0_hat)[A == 1] )) 

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
    df_cc %>% 
    slice(sample(1:nrow(.), size = nrow(.), replace = T))
  
  ### Fit outcome model on boostrappped data
  mu_boot <- lm(mu0_formula, data = df_boot)
  df_boot$mu0_hat <- predict(mu_boot, newdata = mutate(df_boot, A = 0))
  
  tau_boot <- 
    df_boot %>% 
    summarise('att_rygb' = mean( (pct_wt_change - mu0_hat)[A == 1] )) 
  
  tau_boot_rygb[b] <- tau_boot$att_rygb
  
}

df_results <- 
  df_id %>% 
  filter(scenario_id == s_id) %>% 
  select(scenario_id, everything()) %>% 
  mutate('att_rygb' = tau_att$att_rygb,
         'sd_rygb' = sd(tau_boot_rygb),
         'estimator' = 'Complete Case Outcome Regression')

write_csv(df_results, glue('data/application/aligned_t0/weight_change/cc_outcome_reg_{s_id}.csv'))
          
          