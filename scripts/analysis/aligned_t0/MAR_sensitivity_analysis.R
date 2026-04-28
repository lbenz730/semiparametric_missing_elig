library(tidyverse)
library(arrow)
library(glue)
library(furrr)
plan(multisession(workers = 32))


source('scripts/helpers.R')

### Directory where EHR data is stored
ehr_dir <- '/n/haneuse_ehr_l3/V1.0'
data_dir <- '/n/haneuse_ehr_l3/V1.0/clean_datasets'

### Weight Change
weight_files <- dir(glue('{data_dir}/aligned_t0/outputs/weight_change/'), full.names = T)
weight_files <- weight_files[grepl('EIF', weight_files)]
weight_EIF <- map_dfr(weight_files, read_parquet)

### Remission
t2dm_files <- dir(glue('{data_dir}/aligned_t0/outputs/remission/'), full.names = T)
t2dm_files <- t2dm_files[grepl('EIF', t2dm_files)]
t2dm_EIF <- map_dfr(t2dm_files, read_parquet)


### Sensitivity analysis for collection of EIF contributions given
### sensitivity params alpha_a1c, alpha_bmi which correspond to the 
### log odds in being complete case (R = 1) per unit increase in a1c or bmi
sensitivity <- function(df_EIF, alpha_a1c, alpha_bmi) {
  
  df_final <-
    df_EIF %>% 
    
    ### Update eta w/ sensitivity parameters
    mutate('eta1_hat' =  case_when(R == 0 ~ eta1_hat,
                                   R == 1 ~ expit(logit(eta1_hat) + 
                                                    alpha_bmi * ifelse(!is.na(baseline_bmi), (baseline_bmi - 35), 0) + 
                                                    alpha_a1c * ifelse(!is.na(baseline_a1c), (baseline_a1c - 6.5), 0))),
           
           'eta0_hat' =  case_when(R == 0 ~ eta0_hat,
                                   R == 1 ~ expit(logit(eta0_hat) +  
                                                    alpha_bmi * ifelse(!is.na(baseline_bmi), (baseline_bmi - 35), 0) + 
                                                    alpha_a1c * ifelse(!is.na(baseline_a1c), (baseline_a1c - 6.5), 0)))) %>% 
    
    
    ### need some value to get canceled by 0 that's not NA
    mutate('eligible' = replace(eligible, R == 0, 0),
           'mu0_hat' = replace(mu0_hat, R == 0, 0),
           'mu0_hat' = replace(mu0_hat, R == 1 & is.na(mu0_hat), 0),
           'u_hat' = replace(u_hat, R == 0, 0),
           'u_hat' = replace(u_hat, R == 1 & is.na(u_hat), 0)
    ) %>%
    ### Compute numerator and denominator one step estimators (eg., uncentered influence functions)
    mutate('alpha_OS' = A * (1 - R/eta1_hat) * epsilon1_hat + A * R * eligible/eta1_hat,
           'beta_OS' = 
             A * (epsilon1_hat * pct_wt_change - xi_hat)
           + A * R/eta1_hat * ((eligible - epsilon1_hat) * pct_wt_change - (eligible * mu0_hat - xi_hat))
           - (1-A) * R/eta1_hat * (eligible * u_hat/(1-u_hat) * (pct_wt_change - mu0_hat) - (gamma_hat * pct_wt_change - chi_hat))
           - (1-A) * eta0_hat/eta1_hat * ((gamma_hat * pct_wt_change - chi_hat))
    ) %>% 
    group_by(scenario_id) %>% 
    ### Subject Specific Contributions (centered)
    mutate('alpha_IF' = alpha_OS - mean(alpha_OS),
           'beta_IF' = beta_OS - mean(beta_OS))
  
  
  tau_hat <-
    df_final %>% 
    group_by(scenario_id, diabetes_lookback, bmi_lookback, rx_lookback) %>% 
    summarise('att_hat' = mean(beta_OS)/mean(alpha_OS),
              'sd' = sd(subject_IF)/sqrt(nrow(df_EIF)),
              'alpha_a1c' = alpha_a1c,
              'alpha_bmi' = alpha_bmi) %>% 
    ungroup()
  
  return(tau_hat)
}


alpha_grid <- 
  crossing('alpha_a1c' = seq(-0.3, 0.3, 0.03),
           'alpha_bmi' = seq(-0.125, 0.125, 0.01))

df_weight <- 
  future_map2_dfr(alpha_grid$alpha_a1c, alpha_grid$alpha_bmi, ~sensitivity(weight_EIF, .x, .y))



df_t2dm <- 
  future_map2_dfr(alpha_grid$alpha_a1c, alpha_grid$alpha_bmi, ~sensitivity(t2dm_EIF, .x, .y))

write_csv(df_weight, glue('{data_dir}/aligned_t0/outputs/weight_change_sensitivity.csv'))
write_csv(df_t2dm, glue('{data_dir}/aligned_t0/outputs/t2dm_sensitivity.csv'))


### Plot Results
weight_plot <- 
  df_weight %>% 
  mutate('bmi_lookback_f' = paste0('BMI~Lookback:~', bmi_lookback, ifelse(bmi_lookback > 1, '~Months', '~Month')),
         'a1c_lookback_f' = paste0('A1c~Lookback:~', diabetes_lookback, ifelse(diabetes_lookback > 1, '~Months', '~Month')),
         'rx_lookback_f' = ifelse(rx_lookback == '0', '"Diabetes via Rx:\nActive at Surgery"', 
                                  '"Diabetes via Rx:\nActive w/in 12 Months\nBefore Surgery"')) %>% 
  mutate('bmi_lookback_f' = fct_reorder(bmi_lookback_f, bmi_lookback),
         'a1c_lookback_f' = fct_reorder(a1c_lookback_f, diabetes_lookback),
         'rx_lookback_f' = fct_reorder(rx_lookback_f, rx_lookback)) 


ggplot(weight_plot %>% filter(rx_lookback == 0), aes(x = alpha_a1c, y = alpha_bmi)) + 
  facet_grid(bmi_lookback_f~a1c_lookback_f, labeller = label_parsed) + 
  geom_tile(aes(fill = att_hat), color = 'black', lwd = 0.05) +
  scale_fill_gradient2(low = 'red', mid = 'white', high = 'blue') +
  scale_x_continuous(breaks = seq(-0.3, 0.3, 0.15)) + 
  scale_y_continuous(breaks = c(-0.15, -0.1, -0.05, 0, 0.05, 0.1, 0.15)) + 
  theme(legend.text = element_text(angle = 90, hjust = 1),
        axis.title = element_text(size = 18),
        strip.text = element_text(size = 10)) +
  labs(x = 'Log Odds Ratio of Completely Observed Eligibility Information\nPer 1.0% Change in Baseline A1c',
       y = expression(atop(
         "Log Odds Ratio of Completely Observed Eligibility Information",
         "Per 1 " * "kg/m"^2 * " Change in Baseline BMI kg/m"^2  * ' (Above 35 kg/m'^2 *')'
       )),
       title = 'MAR Assumption Sensitivity Analysis: Difference in 3-Year Weight Loss',
       subtitle = 'Diabetes via Rx: Active at Surgery',
       fill = expression(paste(widehat(theta)[ATT]^elig, ' = ', widehat(E), '[', Y^RYGB - Y^SG, ' | A = RYGB, Eligible = 1]')))

ggsave('figures/aligned_t0/application/weight_sensitivity_0.pdf', height = 12/1.2, width = 16/1.2)

ggplot(weight_plot %>% filter(rx_lookback == 12), aes(x = alpha_a1c, y = alpha_bmi)) + 
  facet_grid(bmi_lookback_f~a1c_lookback_f, labeller = label_parsed) + 
  geom_tile(aes(fill = att_hat), color = 'black', lwd = 0.05) +
  scale_fill_gradient2(low = 'red', mid = 'white', high = 'blue') +
  scale_x_continuous(breaks = seq(-0.3, 0.3, 0.15)) + 
  scale_y_continuous(breaks = c(-0.15, -0.1, -0.05, 0, 0.05, 0.1, 0.15)) + 
  theme(legend.text = element_text(angle = 90, hjust = 1),
        axis.title = element_text(size = 18),
        strip.text = element_text(size = 10)) +
  labs(x = 'Log Odds Ratio of Completely Observed Eligibility Information\nPer 1.0% Change in Baseline A1c',
       y = expression(atop(
         "Log Odds Ratio of Completely Observed Eligibility Information",
         "Per 1 " * "kg/m"^2 * " Change in Baseline BMI kg/m"^2  * ' (Above 35 kg/m'^2 *')'
       )),
       title = 'MAR Assumption Sensitivity Analysis: Difference in 3-Year Weight Loss',
       subtitle = 'Diabetes via Rx: Active w/in 12 Months Before Surgery',
       fill = expression(paste(widehat(theta)[ATT]^elig, ' = ', widehat(E), '[', Y^RYGB - Y^SG, ' | A = RYGB, Eligible = 1]')))

ggsave('figures/aligned_t0/application/weight_sensitivity_12.pdf', height = 12/1.2, width = 16/1.2)
       


t2dm_plot <- 
  df_t2dm %>% 
  filter(alpha_bmi >= -0.05) %>% 
  mutate('bmi_lookback_f' = paste0('BMI~Lookback:~', bmi_lookback, ifelse(bmi_lookback > 1, '~Months', '~Month')),
         'a1c_lookback_f' = paste0('A1c~Lookback:~', diabetes_lookback, ifelse(diabetes_lookback > 1, '~Months', '~Month')),
         'rx_lookback_f' = ifelse(rx_lookback == '0', '"Diabetes via Rx:\nActive at Surgery"', 
                                  '"Diabetes via Rx:\nActive w/in 12 Months\nBefore Surgery"')) %>% 
  mutate('bmi_lookback_f' = fct_reorder(bmi_lookback_f, bmi_lookback),
         'a1c_lookback_f' = fct_reorder(a1c_lookback_f, diabetes_lookback),
         'rx_lookback_f' = fct_reorder(rx_lookback_f, rx_lookback)) 


ggplot(t2dm_plot %>% filter(rx_lookback == 0), aes(x = alpha_a1c, y = alpha_bmi)) + 
  facet_grid(bmi_lookback_f~a1c_lookback_f, labeller = label_parsed) + 
  geom_tile(aes(fill = att_hat), color = 'black', lwd = 0.05) +
  scale_fill_gradient2(low = 'red', mid = 'white', high = 'blue') +
  scale_x_continuous(breaks = seq(-0.3, 0.3, 0.15)) + 
  scale_y_continuous(breaks = c(-0.15, -0.1, -0.05, 0, 0.05, 0.1, 0.15)) + 
  theme(legend.text = element_text(angle = 90, hjust = 1),
        axis.title = element_text(size = 18),
        strip.text = element_text(size = 10)) +
  labs(x = 'Log Odds Ratio of Completely Observed Eligibility Information\nPer 1.0% Change in Baseline A1c',
       y = expression(atop(
         "Log Odds Ratio of Completely Observed Eligibility Information",
         "Per 1 " * "kg/m"^2 * " Change in Baseline BMI kg/m"^2  * ' (Above 35 kg/m'^2 *')'
       )),
       title = 'MAR Assumption Sensitivity Analysis: Difference in 3-Year Diabetes Remission Rate',
       subtitle = 'Diabetes via Rx: Active at Surgery',
       fill = expression(paste(widehat(theta)[ATT]^elig, ' = ', widehat(E), '[', Y^RYGB - Y^SG, ' | A = RYGB, Eligible = 1]')))

ggsave('figures/aligned_t0/application/t2dm_sensitivity_0.pdf', height = 12/1.2, width = 16/1.2)

ggplot(t2dm_plot %>% filter(rx_lookback == 12), aes(x = alpha_a1c, y = alpha_bmi)) + 
  facet_grid(bmi_lookback_f~a1c_lookback_f, labeller = label_parsed) + 
  geom_tile(aes(fill = att_hat), color = 'black', lwd = 0.05) +
  scale_fill_gradient2(low = 'red', mid = 'white', high = 'blue') +
  scale_x_continuous(breaks = seq(-0.3, 0.3, 0.15)) + 
  scale_y_continuous(breaks = c(-0.15, -0.1, -0.05, 0, 0.05, 0.1, 0.15)) + 
  theme(legend.text = element_text(angle = 90, hjust = 1),
        axis.title = element_text(size = 18),
        strip.text = element_text(size = 10)) +
  labs(x = 'Log Odds Ratio of Completely Observed Eligibility Information\nPer 1.0% Change in Baseline A1c',
       y = expression(atop(
         "Log Odds Ratio of Completely Observed Eligibility Information",
         "Per 1 " * "kg/m"^2 * " Change in Baseline BMI kg/m"^2  * ' (Above 35 kg/m'^2 *')'
       )),
       title = 'MAR Assumption Sensitivity Analysis: Difference in 3-Year Diabetes Remission Rate',
       subtitle = 'Diabetes via Rx: Active w/in 12 Months Before Surgery',
       fill = expression(paste(widehat(theta)[ATT]^elig, ' = ', widehat(E), '[', Y^RYGB - Y^SG, ' | A = RYGB, Eligible = 1]')))

ggsave('figures/aligned_t0/application/t2dm_sensitivity_12.pdf', height = 12/1.2, width = 16/1.2)
