library(tidyverse)
library(arrow)
library(glue)
library(ggridges)

source('scripts/helpers.R')

### Directory where EHR data is stored
ehr_dir <- '/n/haneuse_ehr_l3/V1.0'
data_dir <- '/n/haneuse_ehr_l3/V1.0/clean_datasets'

### Weight Change
files <- dir(glue('{data_dir}/aligned_t0/outputs/weight_change/'), full.names = T)
files <- files[grepl('EIF', files)]
df_weight <- map_dfr(files, read_parquet)

df_nuisance <- 
  df_weight %>% 
  pivot_longer(cols = c('eta1_hat', 'eta0_hat', 'epsilon1_hat'),
               names_to = 'nuisance_fx',
               values_to = 'value_hat') %>% 
  mutate('bmi_lookback_f' = paste0('BMI~Lookback:~', bmi_lookback, ifelse(bmi_lookback > 1, '~Months', '~Month')),
         'rx_lookback_f' = ifelse(rx_lookback == '0', '"Diabetes via Rx:\nActive at Surgery"', 
                                  '"Diabetes via Rx:\nActive w/in 12 Months\nBefore Surgery"')) %>% 
  mutate('bmi_lookback_f' = fct_reorder(bmi_lookback_f, bmi_lookback),
         'rx_lookback_f' = fct_reorder(rx_lookback_f, rx_lookback))  %>% 
  filter(bmi_lookback != 3)

ggplot(df_nuisance, aes(x = value_hat, y = as.factor(diabetes_lookback))) + 
  facet_grid(rx_lookback_f~bmi_lookback_f, labeller = label_parsed) + 
  geom_density_ridges(aes(fill = nuisance_fx), 
                      rel_min_height = 0.01,
                      alpha = 0.3) + 
  scale_fill_discrete(labels = c(expression(widehat(epsilon)[1]), expression(widehat(eta)[0]), expression(widehat(eta)[1]))) +
  labs(x = 'Estimated Nuisance Function Value',
       y = 'Diabetes Labs Lookback (Months)',
       fill = '') + 
  theme(axis.text = element_text(size = 16),
        strip.text = element_text(size = 14),
        legend.text = element_text(size = 16))
ggsave('figures/aligned_t0/application/nuisance_dist.pdf', height = 9/1.2, width = 16/1.2)

### Remission
files <- dir(glue('{data_dir}/aligned_t0/outputs/remission/'), full.names = T)
files <- files[grepl('EIF', files)]
df_remission <- map_dfr(files, read_parquet)

df_nuisance <- 
  df_remission %>% 
  pivot_longer(cols = c('eta1_hat', 'eta0_hat', 'epsilon1_hat'),
               names_to = 'nuisance_fx',
               values_to = 'value_hat') %>% 
  mutate('bmi_lookback_f' = paste0('BMI~Lookback:~', bmi_lookback, ifelse(bmi_lookback > 1, '~Months', '~Month')),
         'rx_lookback_f' = ifelse(rx_lookback == '0', '"Diabetes via Rx:\nActive at Surgery"', 
                                  '"Diabetes via Rx:\nActive w/in 12 Months\nBefore Surgery"')) %>% 
  mutate('bmi_lookback_f' = fct_reorder(bmi_lookback_f, bmi_lookback),
         'rx_lookback_f' = fct_reorder(rx_lookback_f, rx_lookback))  %>% 
  filter(bmi_lookback != 3)

ggplot(df_nuisance, aes(x = value_hat, y = as.factor(diabetes_lookback))) + 
  facet_grid(rx_lookback_f~bmi_lookback_f, labeller = label_parsed) + 
  geom_density_ridges(aes(fill = nuisance_fx), 
                      rel_min_height = 0.01,
                      alpha = 0.3) + 
  scale_fill_discrete(labels = c(expression(widehat(epsilon)[1]), expression(widehat(eta)[0]), expression(widehat(eta)[1]))) +
  labs(x = 'Estimated Nuisance Function Value',
       y = 'Diabetes Labs Lookback (Months)',
       fill = '') + 
  theme(axis.text = element_text(size = 16),
        strip.text = element_text(size = 14),
        legend.text = element_text(size = 16))
ggsave('figures/aligned_t0/application/nuisance_dist_remission.pdf', height = 9/1.2, width = 16/1.2)


### Compound Nuisance
### Weight Change
### Weight Change
files <- dir(glue('{data_dir}/aligned_t0/outputs/weight_change/'), full.names = T)
files <- files[grepl('IF', files)]
df_weight_IF <- 
  map_dfr(files, read_parquet) %>% 
  filter(is.na(xi_hat)) %>% 
  mutate('nu_hat' = nu1_hat - nu0_hat) %>% 
  select(subject_id, scenario_id, nu_hat)

df_weight_all <- 
  df_weight %>% 
  inner_join(df_weight_IF, by = c('subject_id', 'scenario_id')) 

df_nuisance_nest  <- 
  df_weight_all %>% 
  pivot_longer(cols = c('xi_hat', 'gamma_hat', 'chi_hat', 'nu_hat'),
               names_to = 'nuisance_fx',
               values_to = 'value_hat') %>% 
  mutate('bmi_lookback_f' = paste0('BMI~Lookback:~', bmi_lookback, ifelse(bmi_lookback > 1, '~Months', '~Month')),
         'rx_lookback_f' = ifelse(rx_lookback == '0', '"Diabetes via Rx:\nActive at Surgery"', 
                                  '"Diabetes via Rx:\nActive w/in 12 Months\nBefore Surgery"')) %>% 
  mutate('bmi_lookback_f' = fct_reorder(bmi_lookback_f, bmi_lookback),
         'rx_lookback_f' = fct_reorder(rx_lookback_f, rx_lookback))  %>% 
  filter(bmi_lookback != 3) %>% 
  mutate('nuisance_fx' = fct_relevel(nuisance_fx, 'xi_hat', 'gamma_hat', 'chi_hat', 'nu_hat')) %>% 
  mutate('nuisance_fx_f' = paste0('widehat(', gsub('_hat', '', nuisance_fx), ')')) %>% 
  mutate('nuisance_fx_f' = fct_relevel(nuisance_fx_f, 'widehat(xi)', 'widehat(gamma)', 'widehat(chi)', 'widehat(nu)'))

ggplot(df_nuisance_nest, aes(x = value_hat, y = as.factor(diabetes_lookback))) + 
  facet_grid(rx_lookback_f~nuisance_fx_f, labeller = label_parsed, scales = 'free_x') + 
  geom_vline(xintercept = 0, lty = 2, alpha = 0.5, lwd = 1.2) + 
  stat_binline(aes(fill = as.factor(bmi_lookback)), 
               draw_baseline = F,
               alpha = 0.3) + 
  labs(x = 'Estimated Nuisance Function Value',
       y = 'Diabetes Labs Lookback (Months)',
       fill = 'BMI Lookback (Months)') + 
  theme(axis.text = element_text(size = 12),
        strip.text = element_text(size = 14),
        legend.text = element_text(size = 16))

ggsave('figures/aligned_t0/application/nuisance_nested.pdf', height = 9/1.2, width = 16/1.2)
