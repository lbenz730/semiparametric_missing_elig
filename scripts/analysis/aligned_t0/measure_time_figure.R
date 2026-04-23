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

### Cohort Building Parameters
elig_start = '2008-01-01'
elig_end = '2011-12-31'

### Load in Data
### Demographics, labs etc.
df_subjects <- read_parquet(glue('{data_dir}/microvascular_tte/subjects.parquet'))
weights <- read_parquet(glue('{data_dir}/all_weights_further_cleaned.parquet'))### Cleaned Weights w/ outliers removed
df_enrollment <- read_parquet(glue('{data_dir}/microvascular_tte/enrollment.parquet'))
diabetes_rx <- read_parquet(glue('{ehr_dir}/parquet_files/cases/all_diabetes_rx.parquet'))
diabetes_labs <- read_parquet(glue('{data_dir}/microvascular_tte/diabetes_labs.parquet'))
surgical_px <- read_parquet( glue('{data_dir}/bs_types_reviewed.parquet'))


### Pool of Surgical Patients getting RYGB or SLEEVE
df_surg <- 
  surgical_px %>% 
  filter(bs_type == 'RYGB' | bs_type == 'SLEEVE') %>% 
  filter(index_date >= elig_start, 
         index_date <= elig_end) %>% 
  left_join(df_enrollment, by = 'subject_id') %>% 
  filter(index_date >= enr_1yr, index_date <= enr_end) 


### Drop measures on the same date
weights <-
  weights %>% 
  distinct(subject_id, measure_date, .keep_all = T) %>% 
  filter(subject_id %in% df_surg$subject_id) %>% 
  filter(!is.na(height)) %>% 
  filter(bmi >= 10) ## Extreme Outliers

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


time_to_weight <- 
  weights %>% 
  filter(measure_date <= index_date, measure_date >= index_date %m-% months(12)) %>% 
  arrange(measure_date) %>% 
  group_by(subject_id) %>% 
  slice(n()) %>% 
  summarise('bmi' = bmi,
            'scale' = 'BMI',
            'time_before_index' = as.numeric(index_date - measure_date))

time_to_a1c <- 
  diabetes_labs %>% 
  inner_join(df_surg, by = 'subject_id')  %>% 
  filter(lab_date <= index_date, lab_date >= index_date %m-% months(12)) %>% 
  arrange(lab_date) %>% 
  group_by(subject_id) %>% 
  slice(n()) %>% 
  summarise('test_type' = test_type,
            'result' = result,
            'scale' = 'A1c',
            'time_before_index' = as.numeric(index_date - lab_date))

df_times <- 
  bind_rows(
    df_surg %>% 
      select(subject_id, bs_type) %>% 
      left_join(time_to_weight, by = 'subject_id') %>% 
      mutate('time_before_index' = replace(time_before_index, is.na(time_before_index), -20),
             'scale' = 'BMI',
             'partial_elig' = bmi >= 35),
    
    df_surg %>% 
      select(subject_id, bs_type) %>% 
      left_join(time_to_a1c, by = 'subject_id') %>% 
      mutate('time_before_index' = replace(time_before_index, is.na(time_before_index), -20),
             'scale' = 'A1c', 
             'partial_elig' = (test_type == 'HGBA1C' & result >= 6.5) | (test_type == 'GLU_F' & result >= 126)) 
  )

ggplot(df_times, aes(x = time_before_index)) + 
  facet_wrap(~scale, ncol = 1, scales = 'free_y') + 
  geom_histogram(aes(fill = bs_type), binwidth = 10, position = 'identity', alpha = 0.5) + 
  scale_x_continuous(breaks = c(-20, seq(0, 12, 1) * 365/12),
                     labels = c('None\nw/in\n12 Months', seq(0, 12, 1))) + 
  scale_y_continuous(labels = ~scales::number(.x, big.mark = ',')) +
  scale_fill_discrete(labels = c('RYGB', 'SG')) +
  labs(title = 'Distribution of Measurement Times',
       subtitle = 'Eligibility Defining Covariates',
       x = 'Time of Most Recent Measure (Months Before Surgery)',
       y = 'Frequency',
       fill = 'Surgical Procedure') +
  theme(legend.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 16))
ggsave('figures/aligned_t0/application/measure_times.pdf', height = 9/1.5, width = 16/1.5)
