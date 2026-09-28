library(tidyverse)
library(arrow)
library(glue)
library(table1)

source('scripts/helpers.R')

### Custom table 1 functions to get , after the thousands
render.continuous <- function(x, ...) {
  with(stats.default(x, ...), c("",
                                "Mean (SD)"         = sprintf("%s (%s)",
                                                              signif_pad(MEAN,   3, big.mark=","),
                                                              signif_pad(SD,     3, big.mark=","))))
}

render.categorical <- function(x, ...) {
  c("", sapply(stats.apply.rounding(stats.default(x)), function(y) with(y,
                                                                        sprintf("%s (%s%%)", prettyNum(FREQ, big.mark=","), PCT))))
}

render.strat <- function (label, n, ...) {
  sprintf("<span class='stratlabel'>%s<br><span class='stratn'>(N=%s)</span></span>", 
          label, prettyNum(n, big.mark=","))
}


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
df_complete <- 
  read_parquet(glue('{data_dir}/aligned_t0/rygb_vsg_datasets.parquet'))  %>% 
  inner_join(df_id)

df1 <- 
  df_complete %>% 
  filter(scenario_id == 1)


df1 <- 
  df1 %>% 
  mutate('hypertension' = fct_rev(factor(ifelse(hypertension == 1, 'Yes', 'No'))),
         'dyslipidemia' = fct_rev(factor(ifelse(dyslipidemia == 1, 'Yes', 'No'))),
         'site' = case_when(site == 'GH' ~ 'KPWA',
                            site == 'SC' ~ 'KPSC', 
                            site == 'NC' ~ 'KPNC'),
         'calendar_year' = as.factor(calendar_year))


df_bmi <- 
  df_complete %>% 
  filter(diabetes_lookback == 1, rx_lookback == 0) %>% 
  select(subject_id, bmi_lookback, baseline_bmi) %>% 
  pivot_wider(names_from = bmi_lookback,
              values_from = baseline_bmi,
              names_prefix = 'bmi_')

df_a1c <- 
  df_complete %>% 
  filter(bmi_lookback == 1, rx_lookback == 0) %>% 
  select(subject_id, diabetes_lookback, rx_lookback, baseline_a1c) %>% 
  pivot_wider(names_from = diabetes_lookback,
              values_from = baseline_a1c,
              names_prefix = 'a1c_')

df_insulin <- 
  df_complete %>% 
  mutate('insulin' = fct_rev(factor(ifelse(insulin == 1, 'Yes', 'No')))) %>% 
  filter(bmi_lookback == 1, diabetes_lookback == 1) %>% 
  select(subject_id, rx_lookback, insulin) %>% 
  pivot_wider(names_from = rx_lookback,
              values_from = insulin,
              names_prefix = 'insulin_')


df1 <- 
  df1 %>% 
  left_join(df_bmi, by = 'subject_id') %>% 
  left_join(df_a1c, by = 'subject_id') %>% 
  left_join(df_insulin, by = 'subject_id') 

label(df1$baseline_age) <- 'Baseline Age'
label(df1$gender) <- 'Sex'
label(df1$race) <- 'Race'
label(df1$site) <- 'Site of Surgery'
label(df1$baseline_bmi) <- 'Baseline BMI'
label(df1$hypertension) <- 'Hypertension'
label(df1$dyslipidemia) <- 'Dyslipidemia'
label(df1$insulin) <- 'Insulin Use'
label(df1$smoking_status) <- 'Self-Reported Smoking Status'
label(df1$calendar_year) <- 'Calendar Year of Surgery'
label(df1$bmi_1) <- 'Baseline BMI (1-month Lookback)'
label(df1$bmi_3) <- 'Baseline BMI (3-month Lookback)'
label(df1$bmi_6) <- 'Baseline BMI (6-month Lookback)'
label(df1$bmi_12) <- 'Baseline BMI (12-month Lookback)'
label(df1$a1c_1) <- 'Baseline HbA1c (1-month Lookback)'
label(df1$a1c_3) <- 'Baseline HbA1c (3-month Lookback)'
label(df1$a1c_6) <- 'Baseline HbA1c (6-month Lookback)'
label(df1$a1c_12) <- 'Baseline HbA1c (12-month Lookback)'
label(df1$a1c_24) <- 'Baseline HbA1c (24-month Lookback)'
label(df1$insulin_0) <- 'Active Insulin Rx at Surgery'
label(df1$insulin_12) <- 'Insulin Rx in 12-months Before Surgery'


tbl_1 <- 
  table1(~baseline_age + gender + race + site + hypertension + dyslipidemia + smoking_status + 
           eGFR + calendar_year +  
           bmi_1 + bmi_3 + bmi_6 + bmi_12 + 
           a1c_1 + a1c_3 + a1c_6 + a1c_12 + a1c_24 +
           insulin_0 + insulin_12
         
         | bs_type,
         data = df1, 
         render.continuous = render.continuous,
         render.strat = render.strat,
         render.categorical = render.categorical,
         overall = F)
write_lines(tbl_1, 'tables/cohort_summary_table.html')
