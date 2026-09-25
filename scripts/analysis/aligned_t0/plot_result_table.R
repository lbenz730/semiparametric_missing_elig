library(tidyverse)
library(arrow)
library(glue)

source('scripts/helpers.R')

### Combos
df_id <- 
  crossing('bmi_lookback' = c(1, 3, 6, 12),
           'diabetes_lookback' = c(1, 3, 6, 12, 24),
           'rx_lookback' = c(0, 12)) %>% 
  mutate('scenario_id' = 1:nrow(.))

### Directory where EHR data is stored
ehr_dir <- '/n/haneuse_ehr_l3/V1.0'
data_dir <- '/n/haneuse_ehr_l3/V1.0/clean_datasets'

### All 40 combons of RYGB vs. VSG Data
df_complete <- 
  read_parquet(glue('{data_dir}/aligned_t0/rygb_vsg_datasets.parquet'))  %>% 
  inner_join(df_id)

### 3 Year Weight Change
### Read in Results
df_results <- map_dfr(dir('data/application/aligned_t0/weight_change/', full.names = T), read_csv)

df_results <-
  df_results %>% 
  mutate('estimator' = case_when(
    estimator == 'Complete Case Outcome Regression' ~ '$\\\\widehat\\\\theta_\\\\text{CC}$',
    estimator == 'Inverse Weighted Outcome Regression' ~ '$\\\\widehat\\\\theta_\\\\text{IWOR}$',
    estimator == 'IF Ratio Estimator' ~ '$\\\\widehat\\\\theta_\\\\text{IF}$',
    estimator == 'EIF Ratio Estimator' ~ '$\\\\widehat\\\\theta_\\\\text{EIF}$'
  )) %>% 
  mutate('estimator' = factor(estimator,   
                              levels = c("$\\\\widehat\\\\theta_\\\\text{CC}$",
                                         "$\\\\widehat\\\\theta_\\\\text{IWOR}$",
                                         "$\\\\widehat\\\\theta_\\\\text{IF}$",
                                         "$\\\\widehat\\\\theta_\\\\text{EIF}$")))



### sigma_y
df_id$sigma_Y <- NA
for(i in 1:40) {
  df_tmp <- 
    df_complete %>% 
    filter(R == 1, eligible == 1, scenario_id == i)
  
  df_id$sigma_Y[df_id$scenario_id == i] <- sd(df_tmp$pct_wt_change)
}


df_table <- 
  df_results %>% 
  inner_join(df_id) %>% 
  mutate('ci_low' = att_rygb - qnorm(0.975) * sd_rygb,
         'ci_high' = att_rygb + qnorm(0.975) * sd_rygb) %>% 
  ### E-Value Approximation (Vanderweele & Ding, 2017)
  mutate('rr_approx' = exp(0.91 * att_rygb/sigma_Y),
         'rr_approx' = ifelse(rr_approx < 1, 1/rr_approx, rr_approx),
         'evalue' = rr_approx + sqrt(rr_approx * (rr_approx - 1)),
         
         'rr_approx_ci' = exp(0.91 * ci_high/sigma_Y),
         'rr_approx_ci' = ifelse(rr_approx_ci < 1, 1/rr_approx_ci, rr_approx_ci),
         'evalue_ci' = rr_approx + sqrt(rr_approx_ci * (rr_approx_ci - 1)))

df_table <- 
  df_table %>% 
  mutate('att_fmt' = sprintf("%.4f (%.4f, %.4f)", att_rygb, ci_low, ci_high),
         'evalue_fmt' = sprintf("%.2f (%.2f)", evalue, evalue_ci)) %>%
  select(scenario_id, bmi_lookback, diabetes_lookback, rx_lookback, estimator, att_fmt, evalue_fmt) %>%
  pivot_wider(names_from = estimator,
              values_from = c(att_fmt, evalue_fmt),
              names_glue  = "{estimator}_{.value}") %>%
  arrange(scenario_id)


est_order <- levels(df_results$estimator)
ordered_cols <- 
  unlist(lapply(est_order, function(e) {
    c(paste0(e, "_att_fmt"), paste0(e, "_evalue_fmt"))
  }))
ordered_cols <- intersect(ordered_cols, names(df_table))

df_table <- 
  df_table %>%
  select(scenario_id, bmi_lookback, diabetes_lookback, rx_lookback, all_of(ordered_cols))


present_est <- intersect(est_order, unique(as.character(df_results$estimator)))
n_est <- length(present_est)

header_spans <- c(" " = 4, setNames(rep(2, n_est), present_est))

col_labels <-
  c("Scenario",
    "\\shortstack{BMI \\\\ Lookback}",
    "\\shortstack{Diabetes \\\\ Lookback}",
    "\\shortstack{Rx \\\\ Lookback}",
    rep(c("ATT (95\\% CI)", "E-value (CI)"), n_est))

latex_tbl <- 
  df_table %>%
  kbl(format = "latex", booktabs = TRUE, escape = FALSE,
      col.names = col_labels,
      align = c("c", "c", "c", "c", rep("c", 2 * n_est)),
      caption = "Comparison of estimators of $\\theta(P)$ on 3-year weight loss outcomes.") %>%
  add_header_above(header_spans, escape = FALSE) %>%
  kable_styling(latex_options = c("scale_down", "hold_position"))

latex_str <- as.character(latex_tbl)
latex_str <- sub("\\\\resizebox\\{\\\\textwidth\\}\\{!\\}", 
                 "\\\\resizebox{\\\\textheight}{!}", 
                 latex_str)
write(latex_str, 'tables/weight_results.tex')



### 3 Year Remission Results
### Read in Results
df_results_diabetes <- map_dfr(dir('data/application/aligned_t0/diabetes_remission/', full.names = T), read_csv)
df_results_diabetes <- 
  df_results_diabetes %>% 
  mutate('estimator' = case_when(
    estimator == 'Complete Case Outcome Regression' ~ '$\\\\widehat\\\\theta_\\\\text{CC}$',
    estimator == 'Inverse Weighted Outcome Regression' ~ '$\\\\widehat\\\\theta_\\\\text{IWOR}$',
    estimator == 'IF Ratio Estimator' ~ '$\\\\widehat\\\\theta_\\\\text{IF}$',
    estimator == 'EIF Ratio Estimator' ~ '$\\\\widehat\\\\theta_\\\\text{EIF}$'
  )) %>% 
  mutate('estimator' = factor(estimator,   
                              levels = c("$\\\\widehat\\\\theta_\\\\text{CC}$",
                                         "$\\\\widehat\\\\theta_\\\\text{IWOR}$",
                                         "$\\\\widehat\\\\theta_\\\\text{IF}$",
                                         "$\\\\widehat\\\\theta_\\\\text{EIF}$")))

df_table_t2dm <- 
  df_results_diabetes %>% 
  inner_join(df_id) %>%
  select(-sigma_Y) %>% 
  mutate('ci_low' = att_rygb - qnorm(0.975) * sd_rygb,
         'ci_high' = att_rygb + qnorm(0.975) * sd_rygb) 

df_table_t2dm <- 
  df_table_t2dm %>% 
  mutate(att_fmt = sprintf("%.4f (%.4f, %.4f)", att_rygb, ci_low, ci_high)) %>%
  select(scenario_id, bmi_lookback, diabetes_lookback, rx_lookback, estimator, att_fmt) %>%
  pivot_wider(names_from  = estimator,
              values_from = att_fmt,
              names_glue  = "{estimator}_att_fmt") %>%
  arrange(scenario_id)

est_order_t2dm <- levels(df_results_diabetes$estimator)
ordered_cols_t2dm <- paste0(est_order_t2dm, "_att_fmt")
ordered_cols_t2dm <- intersect(ordered_cols_t2dm, names(df_table_t2dm))

df_table_t2dm <- 
  df_table_t2dm %>%
  select(scenario_id, bmi_lookback, diabetes_lookback, rx_lookback, all_of(ordered_cols_t2dm))

present_est_t2dm <- intersect(est_order_t2dm, unique(as.character(df_results_diabetes$estimator)))
n_est_t2dm <- length(present_est_t2dm)

header_spans_t2dm <- c(" " = 4, setNames(rep(1, n_est_t2dm), present_est_t2dm))

col_labels_t2dm <-
  c("Scenario",
    "\\shortstack{BMI \\\\ Lookback}",
    "\\shortstack{Diabetes \\\\ Lookback}",
    "\\shortstack{Rx \\\\ Lookback}",
    rep("ATT (95\\% CI)", n_est_t2dm))

latex_tbl_t2dm <- 
  df_table_t2dm %>%
  kbl(format = "latex", booktabs = TRUE, escape = FALSE,
      col.names = col_labels_t2dm,
      align = c("c", "c", "c", "c", rep("c", n_est_t2dm)),
      caption = "Comparison of estimators of $\\theta(P)$ on 3-year diabetes remission.") %>%
  add_header_above(header_spans_t2dm, escape = FALSE) %>%
  kable_styling(latex_options = c("scale_down", "hold_position"))

latex_str_t2dm <- as.character(latex_tbl_t2dm)
latex_str_t2dm <- sub("\\\\resizebox\\{\\\\textwidth\\}\\{!\\}", 
                      "\\\\resizebox{\\\\textheight}{!}", 
                      latex_str_t2dm)

write(latex_str_t2dm, 'tables/t2dm_results.tex')
