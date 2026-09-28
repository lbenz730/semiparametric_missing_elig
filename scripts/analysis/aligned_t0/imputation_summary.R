library(tidyverse)
library(arrow)
library(glue)
library(knitr)
library(kableExtra)

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

# helper: n (pct%) with thousands separators; pct is a proportion
fmt_np <- function(n, p, digits = 1) {
  sprintf("%s (%.*f)", formatC(n, format = "d", big.mark = ","), digits, 100 * p)
}


tab <- 
  df_complete %>% 
  group_by(scenario_id, bmi_lookback, diabetes_lookback, rx_lookback) %>% 
  summarise('Y_wt_impute' = sum(Y_impute_wt),
            'pct_Y_impute' = mean(Y_impute_wt),
            'Y_wt_impute_elig_cc' = sum(eligible * R * Y_impute_wt , na.rm = T),
            'pct_wt_impute_elig_cc' = sum(eligible * R * Y_impute_wt , na.rm = T)/sum(eligible * R, na.rm = T),
            'a1c_impute_n' = sum(a1c_impute),
            'pct_a1c_impute' = mean(a1c_impute),
            'eGFR_impute' = sum(impute_eGFR),
            'pct_eGFR_impute' = mean(impute_eGFR)) %>% 
  ungroup() %>% 
  transmute('Scenario' = scenario_id,
            'BMI lookback' = bmi_lookback,
            'Diabetes lookback' = diabetes_lookback,
            'Rx lookback' = rx_lookback,
            '% Weight Change' = fmt_np(Y_wt_impute, pct_Y_impute),
            '% Weight Change, complete-case eligible' = fmt_np(Y_wt_impute_elig_cc, pct_wt_impute_elig_cc),
            'HbA1c imputed' = fmt_np(a1c_impute_n, pct_a1c_impute),
            'eGFR imputed' = fmt_np(eGFR_impute, pct_eGFR_impute))

latex_tab <- 
  kbl(
    tab,
    format   = "latex",
    booktabs = TRUE,
    escape   = FALSE,
    align    = c("c", "c", "c", "c", "c", "c", "c", "c"),
    caption  = "Imputation counts by lookback scenario, n (\\%).",
    label    = "imputation-scenarios"
  ) %>%
  add_header_above(c("Scenario ID" = 4, "n (\\%)" = 4), escape = FALSE) %>%
  kable_styling(latex_options = c("hold_position", "scale_down")) %>% 
  gsub('\\%', '\\\\%', .)

write_lines(latex_tab, "tables/imputation_table.tex")
