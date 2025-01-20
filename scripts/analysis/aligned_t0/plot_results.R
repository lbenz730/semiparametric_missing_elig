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
                                  '"Diabetes via Rx:\nActive w/in 12 Months Before Surgery"')) %>% 
  mutate('bmi_lookback_f' = fct_reorder(bmi_lookback_f, bmi_lookback),
         'rx_lookback_f' = fct_reorder(rx_lookback_f, rx_lookback)) 

ggplot(df_results, aes(x = as.factor(diabetes_lookback), y = att_rygb)) + 
  facet_grid(rx_lookback_f~bmi_lookback_f, labeller = label_parsed) + 
  geom_ribbon(aes(ymin = att_rygb - qnorm(0.975) * sd_rygb,
                  ymax = att_rygb + qnorm(0.975) * sd_rygb,
                  fill = estimator,
                  group = estimator), alpha = 0.2) +
  geom_point(aes(color = estimator), size = 3) +
  geom_line(aes(color = estimator, group = estimator) , lwd = 1) + 
  scale_y_continuous(limits = c(-0.085, -0.015), labels = scales::percent, breaks = seq(-0.02, -0.10, -0.01)) +
  scale_color_discrete(labels = c(expression(widehat(theta)[CC]), expression(widehat(theta)[EIF]))) +
  scale_fill_discrete(labels = c(expression(widehat(theta)[CC]), expression(widehat(theta)[EIF]))) +
  theme(axis.text = element_text(size = 12),
        legend.title = element_text(size = 16),
        axis.title = element_text(size = 16)) +
  labs(x = 'Diabetes Labs Lookback Window (Months)',
       y = expression(atop(paste('Difference in Mean %-Weight Change for Eligble Subjects Undergoing RYGB'), 
                           paste(widehat(tau)[ATT]^elig, ' = ', widehat(E), '[', Y^RYGB - Y^SG, ' | A = RYGB, Eligible = 1]'))),
       color = 'Estimator',
       fill = 'Estimator',
       title = expression(paste('Effect of Roux-en-Y Gastric Bypass Among Diabetic Patients w/ BMI Exceeding 35 ', kg/m^2)),
       subtitle = 'Comparison of Effect Estimates Over Different Operationalizations of Eligibility Criteria')

ggsave('figures/aligned_t0/application/treatment_effects_RYGB.png', height = 9, width = 16)

ggplot(df_results, aes(x = as.factor(diabetes_lookback), y = att_sleeve)) + 
  facet_grid(rx_lookback_f~bmi_lookback_f, labeller = label_parsed) + 
  geom_ribbon(aes(ymin = att_sleeve - qnorm(0.975) * sd_sleeve,
                  ymax = att_sleeve + qnorm(0.975) * sd_sleeve,
                  fill = estimator,
                  group = estimator), alpha = 0.2) +
  geom_point(aes(color = estimator), size = 3) +
  geom_line(aes(color = estimator, group = estimator) , lwd = 1) + 
  scale_y_continuous(limits = c(-0.085, -0.015), labels = scales::percent, breaks = seq(-0.02, -0.10, -0.01)) +
  scale_color_discrete(labels = c(expression(widehat(theta)[CC]), expression(widehat(theta)[EIF]))) +
  scale_fill_discrete(labels = c(expression(widehat(theta)[CC]), expression(widehat(theta)[EIF]))) +
  theme(axis.text = element_text(size = 12),
        legend.title = element_text(size = 16),
        axis.title = element_text(size = 16)) +
  labs(x = 'Diabetes Labs Lookback Window (Months)',
       y = expression(atop(paste('Difference in Mean %-Weight Change for Eligble Subjects Undergoing SG'), 
                           paste(widehat(tau)[ATC]^elig, ' = ', widehat(E), '[', Y^RYGB - Y^SG, ' | A = SG, Eligible = 1]'))),
       color = 'Estimator',
       fill = 'Estimator',
       title = expression(paste('Effect of Roux-en-Y Gastric Bypass Among Diabetic Patients w/ BMI Exceeding 35 ', kg/m^2)),
       subtitle = 'Comparison of Effect Estimates Over Different Operationalizations of Eligibility Criteria')


ggsave('figures/aligned_t0/application/treatment_effects_SG.png', height = 9, width = 16)

### Diabetes Remission
df_results_diabetes <- map_dfr(dir('data/application/aligned_t0/diabetes_remission/', full.names = T), read_csv)
df_results_diabetes <- 
  df_results_diabetes %>% 
  mutate('bmi_lookback_f' = paste0('BMI~Lookback:~', bmi_lookback, ifelse(bmi_lookback > 1, '~Months', '~Month')),
         'rx_lookback_f' = ifelse(rx_lookback == '0', '"Diabetes via Rx:\nActive at Surgery"', 
                                  '"Diabetes via Rx:\nActive w/in 12 Months Before Surgery"')) %>% 
  mutate('bmi_lookback_f' = fct_reorder(bmi_lookback_f, bmi_lookback),
         'rx_lookback_f' = fct_reorder(rx_lookback_f, rx_lookback))

ggplot(df_results_diabetes, aes(x = as.factor(diabetes_lookback), y = att_rygb)) + 
  facet_grid(rx_lookback_f~bmi_lookback_f, labeller = label_parsed) + 
  geom_ribbon(aes(ymin = att_rygb - qnorm(0.975) * sd_rygb,
                  ymax = att_rygb + qnorm(0.975) * sd_rygb,
                  fill = estimator,
                  group = estimator), alpha = 0.2) +
  geom_hline(lty = 2, lwd = 1.2, yintercept = 0, alpha = 0.2) + 
  geom_point(aes(color = estimator), size = 3) +
  geom_line(aes(color = estimator, group = estimator) , lwd = 1) + 
  scale_y_continuous(limits = c(-0.1, 0.175), breaks = seq(-0.1, 0.2, 0.04), labels = scales::percent) +
  scale_color_discrete(labels = c(expression(widehat(theta)[CC]), expression(widehat(theta)[EIF]))) +
  scale_fill_discrete(labels = c(expression(widehat(theta)[CC]), expression(widehat(theta)[EIF]))) +
  theme(axis.text = element_text(size = 12),
        legend.title = element_text(size = 16),
        axis.title = element_text(size = 16)) +
  labs(x = 'Diabetes Labs Lookback Window (Months)',
       y = expression(atop(paste('Difference in Diabetes Remission Remission Rate Eligble Subjects Undergoing RYGB'), 
                           paste(widehat(tau)[ATT]^elig, ' = ', widehat(E), '[', Y^RYGB - Y^SG, ' | A = RYGB, Eligible = 1]'))),
       color = 'Estimator',
       fill = 'Estimator',
       title = expression(paste('Effect of Roux-en-Y Gastric Bypass Among Diabetic Patients w/ BMI Exceeding 35 ', kg/m^2)),
       subtitle = 'Comparison of Effect Estimates Over Different Operationalizations of Eligibility Criteria')

ggsave('figures/aligned_t0/application/treatment_effects_RYGB_remission.png', height = 9, width = 16)

ggplot(df_results_diabetes, aes(x = as.factor(diabetes_lookback), y = att_sleeve)) + 
  facet_grid(rx_lookback_f~bmi_lookback_f, labeller = label_parsed) + 
  geom_ribbon(aes(ymin = att_sleeve - qnorm(0.975) * sd_sleeve,
                  ymax = att_sleeve + qnorm(0.975) * sd_sleeve,
                  fill = estimator,
                  group = estimator), alpha = 0.2) +
  geom_hline(lty = 2, lwd = 1.2, yintercept = 0, alpha = 0.2) + 
  geom_point(aes(color = estimator), size = 3) +
  geom_line(aes(color = estimator, group = estimator) , lwd = 1) + 
  scale_y_continuous(limits = c(-0.1, 0.175), breaks = seq(-0.1, 0.2, 0.04), labels = scales::percent) +
  scale_color_discrete(labels = c(expression(widehat(theta)[CC]), expression(widehat(theta)[EIF]))) +
  scale_fill_discrete(labels = c(expression(widehat(theta)[CC]), expression(widehat(theta)[EIF]))) +
  theme(axis.text = element_text(size = 12),
        legend.title = element_text(size = 16),
        axis.title = element_text(size = 16)) +
  labs(x = 'Diabetes Labs Lookback Window (Months)',
       y = expression(atop(paste('Difference in Diabetes Remission Rate Among Eligble Subjects Undergoing SG'), 
                           paste(widehat(tau)[ATC]^elig, ' = ', widehat(E), '[', Y^RYGB - Y^SG, ' | A = SG, Eligible = 1]'))),
       color = 'Estimator',
       fill = 'Estimator',
       title = expression(paste('Effect of Roux-en-Y Gastric Bypass Among Diabetic Patients w/ BMI Exceeding 35 ', kg/m^2)),
       subtitle = 'Comparison of Effect Estimates Over Different Operationalizations of Eligibility Criteria')


ggsave('figures/aligned_t0/application/treatment_effects_SG_remission.png', height = 9, width = 16)


### Combined Figure 
ggplot(df_results, aes(x = as.factor(diabetes_lookback), y = att_rygb)) + 
  facet_grid(rx_lookback_f~bmi_lookback_f, labeller = label_parsed) + 
  geom_ribbon(aes(ymin = att_rygb - qnorm(0.975) * sd_rygb,
                  ymax = att_rygb + qnorm(0.975) * sd_rygb,
                  fill = estimator,
                  group = estimator), alpha = 0.2) +
  geom_point(aes(color = estimator), size = 3) +
  geom_line(aes(color = estimator, group = estimator) , lwd = 1) + 
  scale_y_continuous(limits = c(-0.085, -0.015), labels = scales::percent, breaks = seq(-0.02, -0.10, -0.01)) +
  scale_color_discrete(labels = c(expression(widehat(theta)[CC]), expression(widehat(theta)[EIF]))) +
  scale_fill_discrete(labels = c(expression(widehat(theta)[CC]), expression(widehat(theta)[EIF]))) +
  theme(axis.text = element_text(size = 12),
        legend.title = element_text(size = 16),
        axis.title = element_text(size = 16)) +
  labs(x = 'Diabetes Labs Lookback Window (Months)',
       y = expression(atop(paste('Difference in Mean %-Weight Change for Eligble Subjects Undergoing RYGB'), 
                           paste(widehat(tau)[ATT]^elig, ' = ', widehat(E), '[', Y^RYGB - Y^SG, ' | A = RYGB, Eligible = 1]'))),
       color = 'Estimator',
       fill = 'Estimator')


color_map <- rep(gg_color_hue(2), each = 3)
names(color_map) <- 
  c('Complete Case Outcome Regression',
    'Complete Case Outcome Regression% Weight Change',
    'Complete Case Outcome RegressionDiabetes Remission',
    'EIF Ratio Estimator',
    'EIF Ratio Estimator% Weight Change',
    'EIF Ratio EstimatorDiabetes Remission')

df_final <- 
  bind_rows(df_results %>% mutate('outcome' = '% Weight Change'),
            df_results_diabetes %>% mutate('outcome' = 'Diabetes Remission')) 

ggplot(df_final, aes(x = as.factor(diabetes_lookback), y = att_rygb)) + 
  facet_grid(rx_lookback_f~bmi_lookback_f, labeller = label_parsed) + 
  geom_ribbon(aes(ymin = att_rygb - qnorm(0.975) * sd_rygb,
                  ymax = att_rygb + qnorm(0.975) * sd_rygb,
                  fill = estimator,
                  color = paste0(estimator, outcome),
                  lty = outcome,
                  group = paste0(estimator, outcome)), 
              show.legend = F,
              alpha = 0.1, lwd = 0.5) + 
  geom_hline(lty = 2, lwd = 1.2, yintercept = 0, alpha = 0.2) + 
  geom_point(aes(color = estimator), size = 3) +
  geom_line(aes(color = estimator, group = paste0(estimator, outcome), lty = outcome), lwd = 1) +
  scale_y_continuous(breaks = seq(-0.08, 0.2, 0.04), labels = scales::percent) +
  scale_color_manual(values = color_map,
                     breaks = unique(sort(df_results$estimator)),
                     labels = c(expression(widehat(theta)[CC]), expression(widehat(theta)[EIF]))) +
  scale_fill_discrete(labels = c(expression(widehat(theta)[CC]), expression(widehat(theta)[EIF]))) +
  guides(color = guide_legend()) + 
  theme(axis.text = element_text(size = 12),
        strip.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        axis.title = element_text(size = 16)) +
  labs(title = 'Average Treatment Effect on Study Eligible Patients Undergoing Roux-en-Y Gastric Bypass',
       subtitle = 'Difference in % Weight Change and Diabetes Remission Rate at 3 Years Post Surgery',
    x = 'Diabetes Labs Lookback Window (Months)',
       y = expression(paste(widehat(tau)[ATT]^elig, ' = ', widehat(E), '[', Y^RYGB - Y^SG, ' | A = RYGB, Eligible = 1]')),
       color = 'Estimator',
       lty = 'Outcome')
ggsave('figures/aligned_t0/application/results_figure.pdf', height = 9, width = 16)
