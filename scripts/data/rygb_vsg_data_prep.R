library(tidyverse)
library(glue)
library(arrow)
library(lubridate)
library(haven)

source('scripts/helpers.R')

set.seed(189102)

### Directory where EHR data is stored
ehr_dir <- '/n/haneuse_ehr_l3/V1.0'
data_dir <- '/n/haneuse_ehr_l3/V1.0/clean_datasets'

### Load in Data
### Demographics, labs etc.
df_subjects <- read_parquet(glue('{data_dir}/microvascular_tte/subjects.parquet'))
weights <- read_parquet(glue('{data_dir}/all_weights_further_cleaned.parquet'))### Cleaned Weights w/ outliers removed
df_pregnancy <- read_parquet(glue('{data_dir}/microvascular_tte/pregnancy.parquet'))
df_enrollment <- read_parquet(glue('{data_dir}/microvascular_tte/enrollment.parquet'))
df_kidney <- read_parquet(glue('{data_dir}/microvascular_tte/kidney_labs.parquet'))
diabetes_rx <- read_parquet(glue('{ehr_dir}/parquet_files/cases/all_diabetes_rx.parquet'))
diabetes_labs <- read_parquet(glue('{data_dir}/microvascular_tte/diabetes_labs.parquet'))
diabetes_dx<- read_parquet(glue('{ehr_dir}/parquet_files/cases/raw_diabetes_dx_cases.parquet'))
smoking <- read_parquet(glue('{data_dir}/microvascular_tte/smoking.parquet'))
surgical_px <- read_parquet( glue('{data_dir}/bs_types_reviewed.parquet'))
obesity_comorbidities <- read_parquet(glue('{data_dir}/bariatric_tte/obesity_comorbidities.parquet'))


### Pool of Surgical Patients getting RYGB or SLEEVE
df_surg <- 
  surgical_px %>% 
  filter(bs_type == 'RYGB' | bs_type == 'SLEEVE')

### Drop measures on the same date
weights <-
  weights %>% 
  distinct(subject_id, measure_date, .keep_all = T) %>% 
  filter(subject_id %in% df_surg$subject_id) %>% 
  filter(!is.na(height)) %>% 
  filter(bmi >= 10) ## Extreme Outliers

smoking <- 
  smoking %>% 
  distinct(subject_id, contact_date, .keep_all = T) %>% 
  filter(subject_id %in% df_surg$subject_id)

df_kidney <- 
  df_kidney %>% 
  distinct(subject_id, lab_date, .keep_all = T) %>% 
  filter(subject_id %in% df_surg$subject_id)

diabetes_labs <- 
  diabetes_labs %>% 
  distinct(subject_id, lab_date, .keep_all = T) %>% 
  filter(subject_id %in% df_surg$subject_id)

### Diabetes Rx
sulfonylureas <- 
  c('GLYBURIDE',
    'GLIMEPRIDE',
    'CHLORPROPAMIDE',
    'GLIPIZIDE',
    'TOLAZAMIDE')

diabetes_rx <- 
  diabetes_rx %>% 
  mutate('sulfonylureas' = grepl(paste0('(', paste(sulfonylureas, collapse = '|'), ')'), generic))

### Processing of Covariates
df_obesity <- 
  obesity_comorbidities %>% 
  inner_join(df_surg, by = 'subject_id') %>% 
  filter(adate <= index_date,
         adate >= index_date %m-% months(12)) %>% 
  group_by(subject_id) %>% 
  summarise('hypertension' = max(hypertension),
            'dyslipidemia' = max(dyslipidemia))

df_smoking <- 
  smoking %>% 
  left_join(df_surg, by = 'subject_id') %>% 
  filter(contact_date <= index_date) %>% 
  arrange(contact_date) %>% 
  group_by(subject_id) %>% 
  slice(n()) %>% 
  ungroup() %>% 
  select(subject_id, smoking_status)

df_covariates <- 
  df_surg %>% 
  select(subject_id, bs_type) %>% 
  left_join(df_smoking, by = 'subject_id') %>% 
  left_join(df_subjects, by = 'subject_id') %>% 
  left_join(df_obesity, by = 'subject_id') %>% 
  mutate('race' = case_when(race == 'WH' ~ 'WHITE',
                            race != 'WH' ~ 'NON-WHITE'),
         'baseline_age' = as.numeric(index_date - birth_date)/365.25,
         'smoking_status' = ifelse(is.na(smoking_status), 'no_self_report', smoking_status),
         'hypertension' = ifelse(is.na(hypertension), 0, hypertension),
         'dyslipidemia' = ifelse(is.na(dyslipidemia), 0, dyslipidemia)) %>% 
  select(subject_id, index_date, bs_type, site, birth_date, baseline_age, race, gender, 
         smoking_status, hypertension, dyslipidemia)

df_covariates <- 
  df_covariates %>% 
  left_join(df_kidney, by = join_by(subject_id == subject_id,
                                    closest(index_date >= lab_date))) 


### Impute Baseline SCR --> eGFR
scr_model <-
  glm(scr ~ baseline_age + site + race + gender + smoking_status + hypertension + dyslipidemia,
      family = Gamma(link = "log"),
      data = df_covariates)
shape <- 1/summary(scr_model)$dispersion
scr_imputed <-
  rgamma(n = sum(is.na(df_covariates$scr)),
         shape = shape,
         rate = shape/exp(predict(scr_model, newdata = filter(df_covariates, is.na(scr)))))
ix_na <- is.na(df_covariates$scr)
df_covariates$scr[ix_na] <- scr_imputed
df_covariates$eGFR[ix_na] <-
  ckd_epi(scr = df_covariates$scr[ix_na],
          age = df_covariates$baseline_age[ix_na],
          sex = df_covariates$gender[ix_na])


### Process Outcomes (Weight Change)
### Method of (Thaweethai et al., 2021)
bmi3yr <- function(df, time_3yr) {
  ### If single weight in period use that 
  if(nrow(df) == 1) {
    return(df$bmi) 
  } 
  
  ### If all measures occur on one side of 3 year date use closest
  if(all(df$time <= time_3yr) | all(df$time >= time_3yr)) {
    return( df$bmi[which.min(abs(df$time - time_3yr))] )
  }
  
  ### Otherwise Use Linear Regression to Estimate
  bmi_model <- lm(bmi ~ time, data = df)
  return(predict(bmi_model, newdata = tibble('time' = time_3yr)))
  
}

df_outcomes <- 
  weights %>% 
  filter(measure_date >= index_date %m+% months(30),
         measure_date <= index_date %m+% months(42)) %>% 
  mutate('yr3_date' = index_date %m+% months(36)) %>% 
  mutate('time' = as.numeric(measure_date - index_date),
         'time_3yr' = as.numeric(yr3_date - index_date)) %>% 
  group_by(subject_id, index_date, time_3yr) %>% 
  nest() %>% 
  mutate('bmi_3yr' = map2_dbl(data, time_3yr, ~bmi3yr(.x, .y))) %>% 
  ungroup() %>%
  select(subject_id, bmi_3yr)

### Diabetes Remission Outcomes (Coleman, 2016)
### A1c < 6.5 w/ no diabetes Rx for 90 days 
### Fasting Glucose w/ no diabetes Rx for 7 days
df_remission <- 
  diabetes_labs %>% 
  left_join(df_surg, by = 'subject_id') %>% 
  filter(lab_date > index_date,
         lab_date <= index_date %m+% years(3)) %>% 
  filter( (test_type == 'HGBA1C' & result_unit == '%' & result < 6.5) |
            (test_type == 'GLU_F' & tolower(result_unit) == 'mg/dl' & result < 126) ) %>% 
  ### Join in diabetes Rx other than metformin 
  left_join(diabetes_rx %>% filter(generic != 'METFORMIN'),
            by = join_by(subject_id == durable_studyid, 
                         index_date == index_date,
                         closest(lab_date >= rxdate))) %>% 
  group_by(subject_id, lab_date, test_type) %>% 
  summarise('rx_free' = min(as.numeric(lab_date - (rxdate + rxsup) ), na.rm = T)) %>% 
  mutate('remission' = as.numeric((test_type == 'HGBA1C' & rx_free >= 90) | (test_type == 'GLU_F' & rx_free >= 7))) %>% 
  group_by(subject_id) %>% 
  summarise('remission' = as.numeric(any(remission)))

### Function to build our cohort
###
### elig_start = date of eligibility ascertainment window beginning
### elig_end = date of eligibility ascertainment window end
### bmi_lookback = time to look back to gather baseline bmi values
### diabetes_lookback = time to look back to gather info on diabetes labs
### rx_lookback = time to look back to gather info on rx (0 = active, as in Fisher 2018 and O'Brien 2018 papers)
build_cohort <- function(elig_start, elig_end, bmi_lookback, diabetes_lookback, rx_lookback) {
  ### Surgical Cases Between Elig, Period, Enrolled Continuously for a year prior to surgery
  df_cohort <- 
    df_covariates %>% 
    filter(index_date >= elig_start, 
           index_date <= elig_end) %>% 
    left_join(df_enrollment, by = 'subject_id') %>% 
    filter(index_date >= enr_1yr, index_date <= enr_end) 
  
  
  ### Baseline BMI 
  df_bmi <- 
    weights %>% 
    filter(measure_date >= index_date %m-% months(bmi_lookback),
           measure_date <= index_date) %>% 
    arrange(measure_date) %>% 
    group_by(subject_id) %>% 
    summarise('baseline_bmi' = last(bmi))
  
  ### Pre-Processing A1c and Diabetes Info 
  # A1c
  df_a1c <-
    diabetes_labs %>% 
    left_join(df_surg, by = 'subject_id') %>% 
    filter(lab_date >= index_date %m-% months(diabetes_lookback),
           lab_date <= index_date) %>% 
    ### Remove extreme A1c Values
    filter((test_type == 'HGBA1C' & result <= 11) | (test_type == 'GLU_F' & (result + 46.7)/28.7 <= 11)) %>% 
    arrange(lab_date) %>% 
    group_by(subject_id) %>% 
    # slice(n()) %>%  ### If we want to consider only most recent value of both type rather than either separately
    summarise('baseline_a1c' = last(result[test_type == 'HGBA1C']),
              'baseline_gluF' = last(result[test_type == 'GLU_F'])) %>%  
    mutate('baseline_a1c' = ifelse(is.na(baseline_a1c), (baseline_gluF + 46.7)/28.7, baseline_a1c),
           'baseline_gluF' = ifelse(is.na(baseline_gluF), baseline_a1c * 28.7 - 46.7, baseline_gluF))
  
  ### Active prescriptions
  df_rx <- 
    diabetes_rx %>% 
    filter(rxdate <= index_date, 
           rxdate + rxsup >= index_date %m-% months(rx_lookback)) %>% 
    group_by('subject_id' = durable_studyid) %>%
    summarise('diabetes_rx' = 1,
              'insulin' = max(0, insulin_flg, na.rm = T),
              'sulfonylureas' = max(0, sulfonylureas, na.rm = T),
              'metformin_only' = as.numeric(all(generic == 'METFORMIN')))
  
  ### Diabetes Diagnosis for subjects w/ metformin
  df_dx <-
    diabetes_dx %>% 
    filter(adate <= index_date, 
           adate >= index_date %m-% months(diabetes_lookback)) %>% 
    filter(grepl('250', dx)) %>% 
    group_by('subject_id' = durable_studyid) %>% 
    summarise('diabetes_icd9' = 1) %>% 
    ungroup()
  
  
  ### Final Dataset
  df_final <-
    df_cohort %>%
    left_join(df_bmi, by = 'subject_id') %>% 
    left_join(df_a1c, by = 'subject_id') %>% 
    left_join(df_rx, by = 'subject_id') %>% 
    left_join(df_dx, by = 'subject_id') %>% 
    left_join(df_remission, by = 'subject_id') %>% 
    mutate('diabetes' = case_when(baseline_a1c >= 6.5 | baseline_gluF >= 126 ~ 1, ### Via Labs
                                  diabetes_rx == 1 & !metformin_only ~ 1, ### Via Rx other than metformin
                                  diabetes_rx == 1 & metformin_only & diabetes_icd9 == 1 ~ 1, ### Via Rx (metformin) + 250.x ICD-9
                                  is.na(diabetes_rx) & baseline_a1c < 6.5 & baseline_gluF < 126 ~ 0,
                                  T ~ NA),
           'insulin' = ifelse(is.na(insulin), 0, insulin),
           'sulfonylureas' = ifelse(is.na(sulfonylureas), 0, sulfonylureas),
           'remission' = ifelse(is.na(remission), 0, remission)) %>% 
    mutate('eligible' = as.numeric(baseline_bmi >= 35 & diabetes == 1),
           'R' = as.numeric(!is.na(eligible))) %>% 
    mutate('calendar_year' = year(index_date)) %>% 
    left_join(df_outcomes, by = 'subject_id') %>% 
    mutate('pct_wt_change' = (bmi_3yr - baseline_bmi)/baseline_bmi) %>% 
    mutate('DiaRem' = 
             case_when(baseline_age < 40 ~ 0,
                       baseline_age <= 49 ~ 1,
                       baseline_age <= 59 ~ 2,
                       baseline_age > 59 ~ 3) + 
             case_when(baseline_a1c < 6.5 ~ 0,
                       baseline_a1c <= 6.9 ~ 2,
                       baseline_a1c <= 8.9 ~ 4,
                       baseline_a1c > 8.9 ~ 6) +
             case_when(insulin == 0 ~ 0,
                       insulin == 1 ~ 10) +
             case_when(sulfonylureas == 0 ~ 0,
                       sulfonylureas == 1 ~ 3)
    ) %>% 
    mutate('eligible_remission' = as.numeric(DiaRem > 2 & baseline_bmi >= 35 & diabetes == 1),
           'R_remission' = as.numeric(!is.na(eligible_remission))) %>% 
    select(subject_id, index_date, ### ID variables
           eligible, R, eligible_remission, R_remission, ### E, R
           bs_type, ### A
           pct_wt_change, bmi_3yr, remission, ### Y
           baseline_bmi, diabetes, baseline_a1c, insulin, DiaRem, ### Le (and related)
           site, gender, baseline_age, smoking_status, eGFR, race, hypertension, dyslipidemia, calendar_year ### Lc
    )
  
  
  ### Two Stage Imputation for % Wt Change Outcome
  ### 1) Impute from model using baseline bmi as predictor if available
  ### 2) Otherwise Impute from model not using baseline_bmi as predictor
  Y_model <- 
    lm(pct_wt_change ~ 
         bs_type + baseline_bmi + site + gender + baseline_age + site + gender +
         baseline_age + smoking_status + eGFR + race + hypertension + dyslipidemia,
       data = df_final) 
  
  ix <- is.na(df_final$bmi_3yr) & !is.na(df_final$baseline_bmi)
  
  df_final$pct_wt_change[ix] <- 
    rnorm(n = sum(ix), 
          mean = predict(Y_model, newdata = df_final[ix,]),
          sd = sqrt(sum(resid(Y_model)^2 / Y_model$df.residual)))
  
  Y_model_noBMI <- 
    lm(pct_wt_change ~ 
         bs_type + site + gender + baseline_age + site + gender +
         baseline_age + smoking_status + eGFR + race + hypertension + dyslipidemia,
       data = df_final) 
  
  ix <- is.na(df_final$baseline_bmi)
  
  df_final$pct_wt_change[ix] <- 
    rnorm(n = sum(ix), 
          mean = predict(Y_model_noBMI, newdata = df_final[ix,]),
          sd = sqrt(sum(resid(Y_model_noBMI)^2 / Y_model_noBMI$df.residual)))
  
  
  ### Impute baseline A1c Among those w/ R = 1, E = 1, and No A1c
  a1c_model <- 
    glm(I(baseline_a1c - 2) ~ 
          site + gender + race + pct_wt_change + smoking_status + baseline_age + 
          eGFR + bs_type + hypertension + dyslipidemia,
        family = Gamma(link = "log"),
        data = filter(df_final, R == 1, eligible == 1)) 
  
  ix <- (df_final$R == 1 & df_final$eligible == 1 & is.na(df_final$baseline_a1c))
  shape <- 1/summary(a1c_model)$dispersion
  
  df_final$baseline_a1c[ix] <- 
    pmin(11, 2 + rgamma(n = sum(ix),
                        shape = shape,
                        rate = shape/exp(predict(a1c_model, newdata = df_final[ix,]))))
  
  return(df_final)
}



#### Build Variety of Cohorts
set.seed(67768)
df_elig <- 
  crossing('bmi_lookback' = c(1, 3, 6, 12),
           'diabetes_lookback' = c(1, 3, 6, 12, 24),
           'rx_lookback' = c(0, 12))

df_complete <- NULL
for(i in 1:nrow(df_elig)) {
  ### Build Cohort Under Parameter Combination
  cat('Combination', i, 'of', nrow(df_elig), '\n')
  df_combo <- 
    build_cohort(elig_start = '2008-01-01',
                 elig_end = '2011-12-31',
                 bmi_lookback = df_elig$bmi_lookback[i],
                 diabetes_lookback = df_elig$diabetes_lookback[i],
                 rx_lookback = df_elig$rx_lookback[i]) %>% 
    mutate('bmi_lookback' = df_elig$bmi_lookback[i],
           'diabetes_lookback' = df_elig$diabetes_lookback[i],
           'rx_lookback' = df_elig$rx_lookback[i])
  
  ### Append to Master DF
  df_complete <- 
    df_complete %>% 
    bind_rows(df_combo)
  
}

if(!dir.exists(glue('{data_dir}/aligned_t0'))) {
  dir.create(glue('{data_dir}/aligned_t0'))
}

write_parquet(df_complete, glue('{data_dir}/aligned_t0/rygb_vsg_datasets.parquet'))
