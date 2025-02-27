library(tidyverse)
library(glue)
library(arrow)
library(patchwork)
source('scripts/helpers.R')

### Directory where EHR data is stored
ehr_dir <- '/n/haneuse_ehr_l3/V1.0'
data_dir <- '/n/haneuse_ehr_l3/V1.0/clean_datasets'

### All 40 combons of RYGB vs. VSG Data
df_complete <- read_parquet(glue('{data_dir}/aligned_t0/rygb_vsg_datasets.parquet')) 

elig_stats <- 
  df_complete %>% 
  group_by(rx_lookback, diabetes_lookback, bmi_lookback, eligible, R) %>% 
  count() %>% 
  group_by(rx_lookback, diabetes_lookback, bmi_lookback) %>% 
  mutate('pct' = n/sum(n)) %>% 
  ungroup() %>% 
  mutate('elig_status' = case_when(eligible == 1 ~ 'Ascertained Eligible',
                                   eligible == 0 ~ 'Ascertained Ineligible',
                                   R == 0 ~ 'Missing Eligibility')) %>% 
  mutate('rx_philosophy' = case_when(rx_lookback == 0 ~ 'Active at Time of Surgery',
                                     rx_lookback == 12 ~ 'Active w/in 12 Months Before Surgery'))

elig_stats_remission <- 
  df_complete %>% 
  group_by(rx_lookback, diabetes_lookback, bmi_lookback, eligible_remission, R_remission) %>% 
  count() %>% 
  group_by(rx_lookback, diabetes_lookback, bmi_lookback) %>% 
  mutate('pct' = n/sum(n)) %>% 
  ungroup() %>% 
  mutate('elig_status' = case_when(eligible_remission == 1 ~ 'Ascertained Eligible',
                                   eligible_remission == 0 ~ 'Ascertained Ineligible',
                                   R_remission == 0 ~ 'Missing Eligibility')) %>% 
  mutate('rx_philosophy' = case_when(rx_lookback == 0 ~ 'Active at Time of Surgery',
                                     rx_lookback == 12 ~ 'Active w/in 12 Months Before Surgery'))


df_elig <- 
  bind_rows(elig_stats %>% mutate('outcome' = '% Weight Change'),
            elig_stats_remission %>% mutate('outcome' = 'Diabetes Remission')) %>% 
  mutate('elig_status_' = paste0('atop("', elig_status, '","',
                                 case_when(elig_status == 'Ascertained Eligible' ~  '(R" == "1,"~"E" == "1)")',
                                           elig_status == 'Ascertained Ineligible' ~  '(R" == "1,"~"E" == "0)")',
                                           elig_status == 'Missing Eligibility' ~ '(R" == "0,"~"E" == "?)")'))) %>% 
  mutate('outcome_' = paste0('"', outcome, '"'))

p_elig_1 <-
  ggplot(df_elig %>% filter(outcome == '% Weight Change'), aes(x = as.factor(diabetes_lookback), y = n)) +
  facet_grid(elig_status_~outcome_, labeller = labeller(.cols = label_parsed, .rows = label_parsed, .multi_line = TRUE), scales = 'free_y') +
  geom_line(aes(color = as.factor(bmi_lookback),
                group = paste(rx_philosophy, bmi_lookback),
                lty = rx_philosophy)) +
  geom_point(aes(color = as.factor(bmi_lookback)),
             size = 3) + 
  labs(x = 'Diabetes Labs Lookback Window (Months)',
       y = '# of Surgical Patients',
       lty = 'TD2M via Rx',
       color = 'BMI Lookback (Months)',
       tag = 'A)',
       title = 'Eligibility Ascertainment Distribution',
       subtitle = 'Among 14,809 Patients Receiving RYGB or SG') + 
  scale_y_continuous(labels = ~scales::number(.x, big.mark = ',')) +
  theme_bw() +
  theme(legend.position = 'bottom', 
        legend.box = 'vertical',
        legend.title = element_text(size = 14),
        axis.text = element_text(size = 12), 
        plot.title = element_text(hjust = 0.5, size = 24),
        plot.subtitle = element_text(hjust = 0.5, size = 18),
        axis.title = element_text(size = 20),
        strip.text = element_text(size = 14),
        plot.tag = element_text(size = 20),
        plot.caption = element_text(size = 10),
        legend.text = element_text(size = 10))

p_elig_2 <-
  ggplot(df_elig %>% filter(outcome == 'Diabetes Remission'), aes(x = as.factor(diabetes_lookback), y = n)) +
  facet_grid(elig_status_~outcome_, labeller = labeller(.cols = label_parsed, .rows = label_parsed, .multi_line = TRUE), scales = 'free_y') +
  geom_line(aes(color = as.factor(bmi_lookback),
                group = paste(rx_philosophy, bmi_lookback),
                lty = rx_philosophy)) +
  geom_point(aes(color = as.factor(bmi_lookback)),
             size = 3) + 
  labs(x = 'Diabetes Labs Lookback Window (Months)',
       y = '# of Surgical Patients',
       lty = 'TD2M via Rx',
       color = 'BMI Lookback (Months)',
       tag = 'A)',
       title = 'Eligibility Ascertainment Distribution',
       subtitle = 'Among 14,809 Patients Receiving RYGB or SG') + 
  scale_y_continuous(labels = ~scales::number(.x, big.mark = ',')) +
  theme_bw() +
  theme(legend.position = 'bottom', 
        legend.box = 'vertical',
        legend.title = element_text(size = 14),
        axis.text = element_text(size = 12), 
        plot.title = element_text(hjust = 0.5, size = 24),
        plot.subtitle = element_text(hjust = 0.5, size = 18),
        axis.title = element_text(size = 20),
        strip.text = element_text(size = 14),
        plot.tag = element_text(size = 20),
        plot.caption = element_text(size = 10),
        legend.text = element_text(size = 10))


### Patterns of Elig Counts
df_patterns <- 
  df_complete %>% 
  group_by(subject_id) %>% 
  summarise('sum_R' = sum(R),
            'sum_elig' = sum(eligible, na.rm = T),
            'sum_inelig' = sum(!eligible, na.rm = T)) %>% 
  group_by_at(vars(contains('sum'))) %>% 
  count() %>% 
  ungroup()

hypothetical_patterns <- 
  crossing('sum_elig' = 0:40,
           'sum_inelig' = 0:40) %>% 
  mutate('sum_R' = sum_elig + sum_inelig) %>% 
  filter(sum_R <= 40) %>% 
  left_join(df_patterns) %>% 
  mutate('pattern' = case_when(sum_elig == 40 ~ 'Always Eligible\n(Always Ascertainable)',
                               sum_inelig == 40 ~ 'Never Eligible\n(Always Ascertainable)',
                               sum_R == 40 ~ 'Sometimes Eligible/Somtimes Ineligible\n(Always Ascertainable)',
                               sum_elig > 0 & sum_inelig == 0 ~ 'Eligible When Ascertainable\n(Sometimes Missing)',
                               sum_elig == 0 & sum_inelig > 0 ~ 'Ineligible When Ascertainable\n(Sometimes Missing)',
                               sum_R == 0 ~ 'Never Ascertainable',
                               sum_R > 0 ~ 'Sometimes Eligible/Sometimes Ineligible\n(Sometimes Missing)'))  %>% 
  mutate('pattern' = fct_relevel(pattern, 
                                 'Always Eligible\n(Always Ascertainable)',
                                 'Never Eligible\n(Always Ascertainable)',
                                 'Sometimes Eligible/Somtimes Ineligible\n(Always Ascertainable)',
                                 'Never Ascertainable',
                                 
                                 'Eligible When Ascertainable\n(Sometimes Missing)',
                                 'Ineligible When Ascertainable\n(Sometimes Missing)',
                                 'Sometimes Eligible/Sometimes Ineligible\n(Sometimes Missing)'))


p1 <- 
  ggplot(hypothetical_patterns, aes(x = sum_elig, y = sum_inelig, width=.5, height=.5)) + 
  facet_wrap(~'% Weight Change') +
  geom_point(aes(color = pattern, alpha = !is.na(n)), size = 0.8) +
  geom_label(aes(label = scales::number(n, big.mark = ','), fill = pattern), size = 4, alpha = 0.7) + 
  scale_alpha_manual(values = c(0.5, 0)) + 
  guides(alpha = 'none',
         fill = 'none',
         color = guide_legend(ncol = 2, override.aes = list(size = 3))) +
  scale_color_manual(values = c('dodgerblue', 'red', 'purple', 'orange',
                                'skyblue', 'salmon', 'violet')) +
  scale_fill_manual(values = c('dodgerblue', 'red', 'purple', 'orange',
                               'skyblue', 'salmon', 'violet')) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 24),
        plot.subtitle = element_text(hjust = 0.5, size = 18),
        axis.title = element_text(size = 20),
        strip.text = element_text(size = 14),
        plot.caption = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 16, hjust = 0.5),
        legend.position = 'bottom', 
        plot.tag = element_text(size = 20),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) + 
  labs(x = '# of Operationalizations Ascertained to be Eligible (R = 1, E = 1)',
       y = '# of Operationalizations Ascertained to be Ineligible (R = 1, E = 0)',
       color = 'Eligibility\n(Ascertainment)',
       title = 'Frequency of Eligibility Ascertainment/Status',
       subtitle = 'Across 40 Operationalizations of Eligibility Criteria',
       tag = 'B)')


df_patterns_remission <- 
  df_complete %>% 
  group_by(subject_id) %>% 
  summarise('sum_R' = sum(R_remission),
            'sum_elig' = sum(eligible_remission, na.rm = T),
            'sum_inelig' = sum(!eligible_remission, na.rm = T)) %>% 
  group_by_at(vars(contains('sum'))) %>% 
  count() %>% 
  ungroup()

hypothetical_patterns_remission <- 
  crossing('sum_elig' = 0:40,
           'sum_inelig' = 0:40) %>% 
  mutate('sum_R' = sum_elig + sum_inelig) %>% 
  filter(sum_R <= 40) %>% 
  left_join(df_patterns_remission) %>% 
  mutate('pattern' = case_when(sum_elig == 40 ~ 'Always Eligible\n(Always Ascertainable)',
                               sum_inelig == 40 ~ 'Never Eligible\n(Always Ascertainable)',
                               sum_R == 40 ~ 'Sometimes Eligible/Somtimes Ineligible\n(Always Ascertainable)',
                               sum_elig > 0 & sum_inelig == 0 ~ 'Eligible When Ascertainable\n(Sometimes Missing)',
                               sum_elig == 0 & sum_inelig > 0 ~ 'Ineligible When Ascertainable\n(Sometimes Missing)',
                               sum_R == 0 ~ 'Never Ascertainable',
                               sum_R > 0 ~ 'Sometimes Eligible/Sometimes Ineligible\n(Sometimes Missing)'))  %>% 
  mutate('pattern' = fct_relevel(pattern, 
                                 'Always Eligible\n(Always Ascertainable)',
                                 'Never Eligible\n(Always Ascertainable)',
                                 'Sometimes Eligible/Somtimes Ineligible\n(Always Ascertainable)',
                                 'Never Ascertainable',
                                 
                                 'Eligible When Ascertainable\n(Sometimes Missing)',
                                 'Ineligible When Ascertainable\n(Sometimes Missing)',
                                 'Sometimes Eligible/Sometimes Ineligible\n(Sometimes Missing)'))


p2 <-
  ggplot(hypothetical_patterns_remission, aes(x = sum_elig, y = sum_inelig)) + 
  facet_wrap(~'Diabetes Remission') +
  geom_point(aes(color = pattern, alpha = !is.na(n)), size = 0.8) +
  geom_label(aes(label = scales::number(n, big.mark = ','), fill = pattern), size = 4, alpha = 0.7) + 
  scale_alpha_manual(values = c(0.5, 0)) + 
  guides(alpha = 'none',
         fill = 'none',
         color = guide_legend(ncol = 2, override.aes = list(size = 3))) +
  scale_color_manual(values = c('dodgerblue', 'red', 'purple', 'orange',
                                'skyblue', 'salmon', 'violet')) +
  scale_fill_manual(values = c('dodgerblue', 'red', 'purple', 'orange',
                               'skyblue', 'salmon', 'violet')) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 24),
        plot.subtitle = element_text(hjust = 0.5, size = 18),
        axis.title = element_text(size = 20),
        strip.text = element_text(size = 14),
        plot.tag = element_text(size = 20),
        plot.caption = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 16, hjust = 0.5),
        legend.position = 'bottom',
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) + 
  labs(x = '# of Operationalizations Ascertained to be Eligible (R = 1, E = 1)',
       y = '# of Operationalizations Ascertained to be Ineligible (R = 1, E = 0)',
       color = 'Eligibility\n(Ascertainment)',
       title = 'Frequency of Eligibility Ascertainment/Status',
       subtitle = 'Across 40 Operationalizations of Eligibility Criteria',
       tag = '')




(p_elig_1 + p1) +
  plot_layout(widths = c(0.3, 0.7)) 

ggsave('figures/aligned_t0/application/elig_dist.pdf', height = 10, width = 16)

(p_elig_2 + p2) +
  plot_layout(widths = c(0.3, 0.7)) 

ggsave('figures/aligned_t0/application/elig_dist_supp.pdf', height = 10, width = 16)
