library(tidyverse)
library(arrow)
library(glue)
library(patchwork)

source('scripts/helpers.R')

### Combos
df_id <- 
  crossing('bmi_lookback' = c(1, 3, 6, 12),
           'diabetes_lookback' = c(1, 3, 6, 12, 24),
           'rx_lookback' = c(0, 12)) %>% 
  mutate('scenario_id' = 1:nrow(.))

### 3 Year Weight Change
### Read in Results
df_results <- map_dfr(dir('data/application/aligned_t0/weight_change/', full.names = T), read_csv)
df_results <- 
  df_results %>% 
  mutate('bmi_lookback_f' = paste0('BMI~Lookback:~', bmi_lookback, ifelse(bmi_lookback > 1, '~Months', '~Month')),
         'rx_lookback_f' = ifelse(rx_lookback == '0', '"Diabetes via Rx:\nActive at Surgery"', 
                                  '"Diabetes via Rx:\nActive w/in 12 Months\nBefore Surgery"')) %>% 
  mutate('bmi_lookback_f' = fct_reorder(bmi_lookback_f, bmi_lookback),
         'rx_lookback_f' = fct_reorder(rx_lookback_f, rx_lookback)) %>% 
  mutate('estimator' = fct_relevel(estimator, 
                                   'Complete Case Outcome Regression',
                                   'Inverse Weighted Outcome Regression',
                                   'IF Ratio Estimator',
                                   'EIF Ratio Estimator')) %>% 
  filter(bmi_lookback != 3)

p_weightloss <- 
  ggplot(df_results, aes(x = as.factor(diabetes_lookback), y = att_rygb)) + 
  facet_grid(rx_lookback_f~bmi_lookback_f, labeller = label_parsed) + 
  geom_hline(lty = 2, lwd = 1.2, yintercept = 0, alpha = 0.2) +
  geom_errorbar(aes(xmin = as.factor(diabetes_lookback),
                    xmax = as.factor(diabetes_lookback),
                    ymin = att_rygb - qnorm(0.975) * sd_rygb,
                    ymax = att_rygb + qnorm(0.975) * sd_rygb,
                    color = estimator),
                position = position_dodge(width = 0.5)) +
  geom_point(aes(color = estimator), position = position_dodge(width = 0.5), size = 3) +
  scale_y_continuous(breaks = seq(-0.2, 0.02, 0.02), labels = scales::percent) +
  scale_color_discrete(labels = c(expression(widehat(theta)[CC]), 
                                  expression(widehat(theta)[IWOR]), 
                                  expression(widehat(theta)[IF]), 
                                  expression(widehat(theta)[EIF]))) +
  labs(x = 'Diabetes Labs Lookback Window (Months)',
       y = expression(paste(widehat(theta)[ATT]^elig, ' = ', widehat(E), '[', Y^RYGB - Y^SG, ' | A = RYGB, Eligible = 1]')),
       subtitle = 'Difference in % Weight Change',
       color = '',
       fill = '') + 
  theme(axis.text = element_text(size = 16),
        plot.subtitle = element_text(size = 24),
        legend.position = 'none')

p_weightloss
ggsave('figures/aligned_t0/application/treatment_effects_RYGB.png', height = 9, width = 16)

ratio <-
  df_results %>% 
  group_by(scenario_id) %>% 
  summarise('sd_EIF' = sd_rygb[estimator == 'EIF Ratio Estimator'], 
            'sd_IF' = sd_rygb[estimator == 'IF Ratio Estimator']) %>% 
  mutate('ratio' = sd_IF/sd_EIF) %>% 
  pull(ratio) 

ratio_d <-
  df_results_diabetes %>% 
  group_by(scenario_id) %>% 
  summarise('sd_EIF' = sd_rygb[estimator == 'EIF Ratio Estimator'], 
            'sd_IF' = sd_rygb[estimator == 'IF Ratio Estimator']) %>% 
  mutate('ratio' = sd_IF/sd_EIF) %>% 
  pull(ratio) 


### Diabetes Remission
df_results_diabetes <- map_dfr(dir('data/application/aligned_t0/diabetes_remission/', full.names = T), read_csv)
df_results_diabetes <- 
  df_results_diabetes %>% 
  mutate('bmi_lookback_f' = paste0('BMI~Lookback:~', bmi_lookback, ifelse(bmi_lookback > 1, '~Months', '~Month')),
         'rx_lookback_f' = ifelse(rx_lookback == '0', '"Diabetes via Rx:\nActive at Surgery"', 
                                  '"Diabetes via Rx:\nActive w/in 12 Months\nBefore Surgery"')) %>% 
  mutate('bmi_lookback_f' = fct_reorder(bmi_lookback_f, bmi_lookback),
         'rx_lookback_f' = fct_reorder(rx_lookback_f, rx_lookback)) %>% 
  mutate('estimator' = fct_relevel(estimator, 
                                   'Complete Case Outcome Regression',
                                   'Inverse Weighted Outcome Regression',
                                   'IF Ratio Estimator',
                                   'EIF Ratio Estimator')) %>% 
  filter(bmi_lookback != 3)

p_remission <- 
  ggplot(df_results_diabetes, aes(x = as.factor(diabetes_lookback), y = att_rygb)) + 
  facet_grid(rx_lookback_f~bmi_lookback_f, labeller = label_parsed) + 
  geom_hline(lty = 2, lwd = 1.2, yintercept = 0, alpha = 0.2) + 
  geom_errorbar(aes(xmin = as.factor(diabetes_lookback),
                    xmax = as.factor(diabetes_lookback),
                    ymin = att_rygb - qnorm(0.975) * sd_rygb,
                    ymax = att_rygb + qnorm(0.975) * sd_rygb,
                    color = estimator),
                position = position_dodge(width = 0.5)) +
  geom_point(aes(color = estimator), position = position_dodge(width = 0.5), size = 3) +
  scale_y_continuous(breaks = seq(-0.20, 0.20, 0.04), labels = scales::percent) +
  scale_color_discrete(labels = c(expression(widehat(theta)[CC]), 
                                  expression(widehat(theta)[IWOR]), 
                                  expression(widehat(theta)[IF]), 
                                  expression(widehat(theta)[EIF]))) +
  labs(x = 'Diabetes Labs Lookback Window (Months)',
       y = expression(paste(widehat(theta)[ATT]^elig, ' = ', widehat(E), '[', Y^RYGB - Y^SG, ' | A = RYGB, Eligible = 1]')),
       subtitle = 'Difference in Diabetes Remission Rate',
       color = '',
       fill = '') + 
  theme(axis.text = element_text(size = 16),
        plot.subtitle = element_text(size = 24),
        legend.text = element_text(size = 16))  

p_remission
ggsave('figures/aligned_t0/application/treatment_effects_RYGB_remission.png', height = 9, width = 16)

### Combined Figure 
(p_weightloss / p_remission) +
  plot_layout(axes = 'collect') 

ggsave('figures/aligned_t0/application/results_figure.pdf', height = 10.5, width = 16)
