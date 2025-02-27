library(tidyverse)
library(glue)
library(arrow)
library(patchwork)
source('scripts/helpers.R')

### Directory where EHR data is stored
ehr_dir <- '/n/haneuse_ehr_l3/V1.0'
data_dir <- '/n/haneuse_ehr_l3/V1.0/clean_datasets'

library(tidyverse)
library(glue)
library(arrow)
library(lubridate)
library(haven)

source('scripts/helpers.R')

### Directory where EHR data is stored
ehr_dir <- '/n/haneuse_ehr_l3/V1.0'
data_dir <- '/n/haneuse_ehr_l3/V1.0/clean_datasets'

### Load in Data
### Demographics, labs etc.
df_subjects <- read_parquet(glue('{data_dir}/microvascular_tte/subjects.parquet'))
weights <- read_parquet(glue('{data_dir}/all_weights_further_cleaned.parquet'))### Cleaned Weights w/ outliers removed
diabetes_rx <- read_parquet(glue('{ehr_dir}/parquet_files/cases/all_diabetes_rx.parquet'))
diabetes_labs <- read_parquet(glue('{data_dir}/microvascular_tte/diabetes_labs.parquet'))
diabetes_dx <- read_parquet(glue('{ehr_dir}/parquet_files/cases/raw_diabetes_dx_cases.parquet'))
surgical_px <- read_parquet( glue('{data_dir}/bs_types_reviewed.parquet'))


### Pool of Surgical Patients getting RYGB or SLEEVE
df_surg <- 
  surgical_px %>% 
  filter(bs_type == 'RYGB' | bs_type == 'SLEEVE')

### Some pre-processing on diabetes information
sulfonylureas <- 
  c('GLYBURIDE',
    'GLIMEPRIDE',
    'CHLORPROPAMIDE',
    'GLIPIZIDE',
    'TOLAZAMIDE')

diabetes_rx <- 
  diabetes_rx %>% 
  mutate('sulfonylureas' = grepl(paste0('(', paste(sulfonylureas, collapse = '|'), ')'), generic)) %>% 
  inner_join(df_surg, by = c('index_date' = 'index_date', 
                             'durable_studyid' = 'subject_id')) %>% 
  rename('subject_id' = durable_studyid)

diabetes_labs <- 
  diabetes_labs %>% 
  filter((test_type == 'HGBA1C' & result <= 11) | (test_type == 'GLU_F' & (result + 46.7)/28.7 <= 11)) %>% 
  inner_join(df_surg) 


### Make Plot 
subject_ix <- c(10017156, 10018620, 10000957, 10017147, 10000024,  30002313)

df_labs <- 
  diabetes_labs %>% 
  filter(subject_id %in% subject_ix, test_type == 'HGBA1C') %>% 
  mutate('days_rel_surg' = as.numeric(lab_date - index_date)) %>% 
  filter(days_rel_surg >= -730, days_rel_surg <= 365) %>% 
  mutate('subject_id' = paste('Subject', map_dbl(subject_id, ~which(subject_ix == .x))),
         'subject_id' = factor(subject_id, levels = paste('Subject', 1:length(subject_ix))))

df_rx <- 
  diabetes_rx %>% 
  filter(subject_id %in% subject_ix) %>% 
  mutate('days_rel_surg' = as.numeric(rxdate - index_date)) %>% 
  mutate('days_rel_surg_sup' = as.numeric(rxdate - index_date + rxsup)) %>% 
  filter(days_rel_surg >= -730, days_rel_surg <= 365) %>% 
  mutate('med_type' = case_when(insulin_flg == 1 ~ 'Insulin',
                                sulfonylureas ~ 'Sulfonylureas',
                                generic == 'METFORMIN' ~ 'Metformin',
                                T ~ 'Other')) %>% 
  mutate('subject_id' = paste('Subject', map_dbl(subject_id, ~which(subject_ix == .x))),
         'subject_id' = factor(subject_id, levels = paste('Subject', 1:length(subject_ix)))) %>% 
  mutate('med_type' = factor(med_type, levels = c('Insulin', 'Sulfonylureas', 'Metformin', 'Other')))


# df_dx <- 
#   diabetes_dx %>% 
#   filter(grepl('250', dx)) %>% 
#   rename('subject_id' = durable_studyid) %>% 
#   filter(subject_id %in% subject_ix) %>% 
#   mutate('days_rel_surg' = as.numeric(adate - index_date)) %>% 
#   filter(days_rel_surg >= -730, days_rel_surg <= 365) %>% 
#   mutate('subject_id' = paste('Subject', map_dbl(subject_id, ~which(subject_ix == .x))),
#          'subject_id' = factor(subject_id, levels = paste('Subject', 1:length(subject_ix)))) 


p1 <-
  ggplot(df_labs, aes(x = days_rel_surg, y = result)) + 
  facet_grid('Hemoglobin A1c %' ~ subject_id, scales = 'free_y', drop = F, labeller = label_wrap_gen(width = 10)) +
  geom_vline(xintercept = 0, lty = 2, color = 'black') +
  geom_hline(yintercept = 6.5, lty = 2, color = 'red') +
  geom_point(color = 'firebrick', size = 4) +
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        strip.text.x = element_text(size = 20),
        strip.text.y = element_text(size = 20),
        axis.title = element_text(size = 22),
        panel.spacing = unit(1, "lines")) + 
  scale_y_continuous(breaks = seq(5.5, 10, 0.5), labels = function(x) paste0(sprintf('%0.1f', x), '%')) + 
  scale_x_continuous(limits = 365.25 * c(-2.25, 0.25),
                     breaks = 365.25 * c(-2, -1, 0),
                     labels = c('-24', '-12', '0')) +
  labs(x = 'Time Relative to Date of Bariatric Surgery (Months)',
       y = '')

p2 <-
  ggplot(df_rx, aes(x = days_rel_surg, y = fct_rev(med_type))) + 
  facet_grid('Diabetes Medications'  ~ subject_id, drop = F, labeller = label_wrap_gen(width = 8)) +
  geom_vline(xintercept = 0, lty = 2) +
  geom_segment(aes(x = days_rel_surg, 
                   xend = days_rel_surg_sup,
                   yend = med_type, 
                   color = med_type),
               lwd = 10, 
               alpha = 0.5) + 
  geom_point(size = 3, aes(fill = med_type), pch = 21)  +
  scale_y_discrete(drop = F) + 
  theme(legend.position = 'bottom',
        strip.text.x = element_blank(),
        strip.text.y = element_text(size = 20),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_text(size = 22),
        legend.text = element_text(size = 20),
        panel.spacing = unit(1, "lines")) + 
  scale_x_continuous(limits = 365.25 * c(-2.25, 0.25),
                     breaks = 365.25 * c(-2, -1, 0),
                     labels = c('-24', '-12', '0')) +
  labs(x = 'Time Relative to Date of Bariatric Surgery (Months)',
       y = '',
       fill = '',
       color = '')

p1/p2 + 
  plot_layout(heights = c(1.3, 1), axis_titles = "collect") + 
  plot_annotation(
    # title = 'EHR Derived Diabetes Measurements',
    # subtitle = 'Select DURABLE Surgical Patients', 
    theme = theme(plot.title = element_text(hjust = 0.5, size = 32),
                  plot.subtitle = element_text(hjust = 0.5, size = 24))) 


ggsave('figures/aligned_t0/application/diabetes_elig.png', height = 9/1.2, width = 16/1.2)
ggsave('figures/aligned_t0/application/diabetes_elig.pdf', height = 9/1.2, width = 16/1.2)


# 
# candidates <- 
#   diabetes_labs %>%
#   mutate('days_rel_surg' = as.numeric(lab_date - index_date)) %>%
#   filter(days_rel_surg >= -730, days_rel_surg <= 365) %>%
#   group_by(subject_id) %>%
#   summarise('n_a1c_above_pre' = sum(test_type == 'HGBA1C' & result >= 6.5 & days_rel_surg <= 0),
#             'n_a1c_above_post' = sum(test_type == 'HGBA1C' & result >= 6.5 & days_rel_surg > 0),
#             'n_a1c_below_pre' = sum(test_type == 'HGBA1C' & result < 6.5 & days_rel_surg <= 0),
#             'n_a1c_below_post' = sum(test_type == 'HGBA1C' & result < 6.5 & days_rel_surg > 0),
#             
#             
#             'n_glu_above_pre' = sum(test_type == 'GLU_F' & result < 126 & days_rel_surg <= 0),
#             'n_glu_above_post' = sum(test_type == 'GLU_F' & result < 126 & days_rel_surg > 0),
#             
#             'n_glu_below_pre' = sum(test_type == 'GLU_F' & result < 126 & days_rel_surg <= 0),
#             'n_glu_below_post' = sum(test_type == 'GLU_F' & result < 126 & days_rel_surg > 0)) %>%
#   filter(n_a1c_above_pre == 0, n_a1c_below_pre == 0, n_glu_above_pre == 0 , n_glu_below_pre == 0) %>% 
#   pull(subject_id) 
# 

# c2 <- 
#   diabetes_rx %>%
#   filter(subject_id %in% candidates) %>%
#   filter(rxdate + rxsup < index_date) %>%
#   mutate('days_rel_surg' = as.numeric(rxdate - index_date)) %>%
#   filter(days_rel_surg >= -730, days_rel_surg <= 365) %>%
#   group_by(subject_id) %>%
#   summarise('last_rx' = max(days_rel_surg[generic != 'METFORMIN'])) %>% 
#   filter(last_rx <= -365, last_rx > -Inf) %>% 
#   pull(subject_id) 
#   


