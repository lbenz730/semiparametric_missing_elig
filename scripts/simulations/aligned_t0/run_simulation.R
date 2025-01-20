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

### Simulation Parameters
args <- commandArgs(trailingOnly = T)
est_id <- as.numeric(args[1])
sim_id <- as.numeric(args[2])

### Set up Parallelization
n_cores <- ifelse(sim_id %in% c(3,5,7) & est_id %in% c(13, 16), 48, 64)
n_cores <- ifelse(sim_id == 8, 48, n_cores)
plan(future::multisession(workers = n_cores))
options(future.globals.maxSize = 100 * 1024^3)
set.seed(71231)


### Directory where EHR data is stored
ehr_dir <- '/n/haneuse_ehr_l3/V1.0'
data_dir <- '/n/haneuse_ehr_l3/V1.0/clean_datasets'

df_inform <- read_parquet(glue('{data_dir}/bs_case_population.parquet'))
params <- read_rds(glue('data/simulations/aligned_t0/inputs/params_{sim_id}.rds'))
estimators <- read_rds('data/simulations/aligned_t0/inputs/estimators.rds')

### Compute True ATT given parameters
cat('Compute True ATT given parameters\n')
set.seed(750984312)
true_att <- compute_truth(params)

### Simulate Datasets
cat('Simulate Datasets\n')
datasets <- 
  future_map(1:params$n_sims, 
             ~generate_data(params), 
             .options = furrr_options(seed = 74592))

cat('Matching Datasets\n')
if(params$matching) {
  if(grepl('Eligibility Restrictive Matching', estimators[[est_id]]$description)) {
    cat('Enforcing eligibility restrictive matching\n')
    params$matching_info$pre_elig <- F  
  }
  
  matched_datasets <- 
    future_map(datasets,
               ~match_subjects(df = .x,
                               match_vars = params$matching_info$vars,
                               pre_elig = params$matching_info$pre_elig,
                               k = params$matching_info$ratio),
               .options = furrr_options(seed = 20010316))
}


### Compute Various Estimators
### No need to fit estimators on full data except on params 1-2-3 + 8
### Params 1/8: No Matching
### Params 2/3: First set of matching variables
#### Params 4/5 and 6/7 are different sizes of different matching variables
### We've however already run the unmatched analysis which is the same between 2-4-6 and 3-5-7
if(sim_id %in% c(1:3, 8)) {
  cat(names(estimators)[est_id], '\n')
  df_results <- 
    fit_estimator(datasets = datasets, 
                  estimator_info = estimators[[est_id]])
} else {
  df_results <- NULL
}

if(params$matching) {
  cat('Matching Version of', names(estimators)[est_id], '\n')
  df_results_matched <- 
    fit_estimator(datasets = matched_datasets, 
                  estimator_info = estimators[[est_id]]) %>% 
    mutate('matched_cohort' = T)
  
  ### Append to Existing Dataset
  if(!is.null(df_results)) {
    df_results <- 
      df_results %>% 
      mutate('matched_cohort' = F) %>% 
      bind_rows(df_results_matched)
  } else {
    df_results <- df_results_matched
  }
}


### Save Results
df_results <- 
  df_results %>% 
  mutate('sim_id' = sim_id, 
         'true_att' = true_att) %>% 
  select(sim_id, everything())

write_csv(df_results, glue('data/simulations/aligned_t0/outputs/sim_results_{sim_id}_{est_id}.csv'))
