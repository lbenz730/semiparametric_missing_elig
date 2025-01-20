library(tidyverse)
library(arrow)
library(glue)
library(haven)
library(lubridate)
source('scripts/helpers.R')

### Lc = c( site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR)
### Le = c(baseline_hgba1c)
### A = RYGB vs. VSG
### E = c(A1c >= 5.7) 
### Y = 3 year weight change

### Directory where EHR data is stored
ehr_dir <- '/n/haneuse_ehr_l3/V1.0'
data_dir <- '/n/haneuse_ehr_l3/V1.0/clean_datasets'

### Load in all data
### Demographics, labs etc. (Intention for what will become Lc) 
df_subjects <- read_parquet(glue('{data_dir}/microvascular_tte/subjects.parquet'))
df_enrollment <- read_parquet(glue('{data_dir}/microvascular_tte/enrollment.parquet'))
smoking <- read_parquet(glue('{data_dir}/microvascular_tte/smoking.parquet'))
df_kidney <- read_parquet(glue('{data_dir}/microvascular_tte/kidney_labs.parquet'))
case_px <- read_sas(glue('{ehr_dir}/_data_for_eric_cases/durable_cases.sas7bdat'))

### Le (BMI and A1c/diabetes related measurements) 
weights <- read_parquet(glue('{data_dir}/all_weights.parquet')) ### Cleaned Weights w/ outliers removed
diabetes_labs <- read_parquet(glue('{data_dir}/microvascular_tte/diabetes_labs.parquet'))


### Drop measures on the same date
weights <-
  weights %>% 
  distinct(subject_id, measure_date, .keep_all = T)

smoking <- 
  smoking %>% 
  distinct(subject_id, contact_date, .keep_all = T)

df_kidney <- 
  df_kidney %>% 
  distinct(subject_id, lab_date, .keep_all = T)

diabetes_labs <- 
  diabetes_labs %>% 
  distinct(subject_id, lab_date, .keep_all = T) %>% 
  mutate('a1c' = ifelse(test_type == 'HGBA1C', result, (result + 46.7)/28.7))


### Outcome Weights
### Weight @ date closest index + 3 years w/in +/- 6 month windo
outcome_weights <-
  weights %>% 
  filter(measure_date >= index_date %m+% months(36 - 6),
         measure_date <= index_date %m+% months(36 + 6)) %>% 
  group_by(subject_id) %>% 
  mutate('days_from_3_yr' = abs(as.numeric(measure_date - (index_date %m+% months(36)))) ) %>% 
  filter(days_from_3_yr == min(days_from_3_yr)) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(subject_id, 'bmi_3yr' = bmi)

### Creation of Case Data Frame
df_cases <-
  df_subjects %>% 
  ### Filter to cases
  filter(!is.na(index_date)) %>% 
  ### Join in Treatment Status
  ### Filter to just RYGB vs. Sleeve
  inner_join(select(case_px, 'subject_id' = durable_studyid, 'bs_type' = final_decision),
             by = 'subject_id') %>% 
  mutate('bs_type' = case_when(grepl('RYGB$', bs_type) ~ 'RYGB', 
                               T ~ bs_type)) %>% 
  filter(bs_type %in% c('SLEEVE', 'RYGB')) %>% 
  ### Compute Age @ Index 
  mutate('baseline_age' = as.numeric(index_date - birth_date)/365.25) %>% 
  ### BMI/HGBA1c closest to Index (w/in 6 months)
  ### Chose this to get R baseline expected value of ~75%
  left_join(select(weights, subject_id, 'baseline_bmi' = bmi, measure_date),
            by = join_by(subject_id, closest(index_date >= measure_date))) %>% 
  left_join(select(diabetes_labs, subject_id, 'baseline_a1c' = a1c, 'a1c_lab_date' = lab_date),
            by = join_by(subject_id, closest(index_date >= a1c_lab_date))) %>% 
  mutate('baseline_a1c' = ifelse(as.numeric(index_date - a1c_lab_date) > 180, NA_real_, baseline_a1c)) %>% 
  mutate('baseline_a1c' = winsorize(baseline_a1c, q = c(0, 0.995))) %>% 
  ### Smoking Status
  left_join(smoking, by = join_by(subject_id, closest(index_date >= contact_date))) %>% 
  mutate('smoking_status' = ifelse(is.na(smoking_status), 'no_self_report', smoking_status)) %>% 
  ### Kidney Labs
  left_join(df_kidney,  join_by(subject_id, closest(index_date >= lab_date))) %>% 
  ### Outcome (pct_wt_change @ 3 year)
  left_join(outcome_weights, by = 'subject_id') %>% 
  mutate('pct_wt_change' = (bmi_3yr - baseline_bmi)/baseline_bmi) %>% 
  ### Clean Up
  mutate('eligible' = as.numeric(baseline_a1c >= 5.7),
         'R' = as.numeric(!is.na(baseline_a1c))) %>% 
  select(subject_id, 
         eligible, R, ### E, R
         bs_type, ### A
         pct_wt_change, ### Y 
         baseline_a1c, ### Le
         gender, site, race, baseline_bmi, smoking_status, baseline_age, eGFR) ### Lc

### Clean up 
df_inform <- 
  df_cases %>% 
  filter(!is.na(eGFR)) %>% 
  filter(!is.na(pct_wt_change)) %>% 
  mutate('race' = ifelse(race == 'WH', 1, 0),
         'gender' = ifelse(gender == 'F', 1, 0),
         'bs_type' = ifelse(bs_type == 'RYGB', 1, 0))

write_parquet(df_inform, glue('{data_dir}/bs_case_population.parquet'))


### Treatment model
pi <-
  glm(bs_type ~ site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR,
      family = 'binomial',
      data = df_inform) 

### Complete Case Model
eta <-
  glm(R ~ site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR + bs_type,
      family = 'binomial',
      data = df_inform) 

### Model for Le
lambda <-
  glm(I(baseline_a1c - 3) ~ site + gender + race + baseline_bmi + I(baseline_bmi^2) + smoking_status + baseline_age + eGFR + bs_type,
    family = Gamma(link = "log"),
    data = filter(df_inform, R == 1)) 

lambda_shape <- 1/summary(lambda)$dispersion

### Model for Outcome
mu <-
  lm(pct_wt_change ~ bs_type + site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR + baseline_a1c + 
       bs_type:baseline_a1c +  baseline_a1c:gender + gender:baseline_bmi + bs_type:site + bs_type:smoking_status,
     data = filter(df_inform, R == 1)) 

sigma_rss <- sqrt(sum(resid(mu)^2 / mu$df.residual))

### Sanity Check on Lambda Model
df_inform$a1c_imputed <- 
  (3 + rgamma(n = nrow(df_inform),
              shape = lambda_shape,
              rate = lambda_shape/exp(predict(lambda, newdata = df_inform))))



