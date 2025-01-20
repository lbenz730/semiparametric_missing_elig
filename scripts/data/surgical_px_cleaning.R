library(tidyverse)
library(glue)
library(arrow)
library(lubridate)
library(haven)

source('scripts/helpers.R')

### Directory where EHR data is stored
ehr_dir <- '/n/haneuse_ehr_l3/V1.0'
data_dir <- '/n/haneuse_ehr_l3/V1.0/clean_datasets'


surgical_px <- read_sas(glue('{ehr_dir}/_data_for_eric_cases/durable_cases.sas7bdat'))
review_gh <- read_sas(glue('{ehr_dir}/_data_for_eric_cases/chart_results_20170215.sas7bdat'))
review_nc <- read_sas(glue('{ehr_dir}/_data_for_eric_cases/final_durable_cases2_KPNC.sas7bdat'))
review_sc <- read_sas(glue('{ehr_dir}/_data_for_eric_cases/kpsc_chartrevresults21070105.sas7bdat'))

df_surgery <- 
  surgical_px %>% 
  select('subject_id' = durable_studyid, 
         index_date,
         prelim_bs_category, 
         final_decision) %>% 
  left_join(review_gh, by = c('subject_id' = 'durable_studyid')) %>% 
  left_join(review_sc, by = c('subject_id' = 'durable_studyid')) %>% 
  left_join(review_nc %>% select(durable_studyid, surgery_cat1), by = c('subject_id' = 'durable_studyid')) %>% 
  mutate('bs_type' = case_when(surgery_cat1 %in% c('1_KEEP_LAP RYGB', '2_KEEP_OPEN RYGB') ~ 'RYGB',
                               surgery_cat1 %in% c('4_KEEP_SLEEVE', 'NLP_sleeve') ~ 'SLEEVE',
                               surgery_cat1 == '3_KEEP_LAP AGB' ~ 'AGB', 
                               grepl('CONVERSION', ProcedureAssignment_Chart1) ~ 'EXCLUDE',
                               grepl('EXCLUDE', ProcedureAssignment_Chart1) ~ 'EXCLUDE',
                               ProcedureAssignment_Chart1 == 'LAP RYGB' ~ 'RYGB',
                               ProcedureAssignment_Chart1 == 'OPEN RYGB' ~ 'RYGB',
                               ProcedureAssignment_Chart1 == 'LAP AGB' ~ 'AGB',
                               ProcedureAssignment_Chart1 == 'SLEEVE' ~ 'SLEEVE',
                               Decision_DURABLE %in% c('LapRYGB', 'OpenRYG','RYGB') ~ 'RYGB',
                               Decision_DURABLE == 'SLEEVE' ~ 'SLEEVE',
                               Decision_DURABLE == 'LapAGB' ~ 'AGB',
                               Decision_DURABLE == 'exclude' ~ 'EXCLUDE',
                               final_decision %in% c('RYGB', 'OpenRYGB', 'LapRYGB') ~ 'RYGB',
                               final_decision %in% c('LapAGB', 'AGB') ~ 'AGB',
                               final_decision  == 'SLEEVE' ~ 'SLEEVE',
                               prelim_bs_category %in% c('RYGB', 'OpenRYGB', 'LapRYGB') ~ 'RYGB',
                               prelim_bs_category %in% c('LapAGB', 'AGB') ~ 'AGB',
                               prelim_bs_category  == 'SLEEVE' ~ 'SLEEVE')) %>% 
  select(subject_id, index_date, bs_type)

write_parquet(df_surgery, glue('{data_dir}/bs_types_reviewed.parquet'))

### Visualize counts of surgical procedure types over time
surg_counts <- 
  df_surgery %>% 
  filter(bs_type %in% c('RYGB', 'SLEEVE', 'AGB')) %>% 
  group_by('surg_year' = year(index_date), bs_type) %>%
  count() %>% 
  ungroup()

ggplot(surg_counts, aes(x = surg_year, y = n)) + 
  geom_line(aes(color = bs_type), lwd = 1) +
  geom_point(aes(color = bs_type), size = 3) +
  theme(axis.text = element_text(size = 12)) + 
  labs(x = 'Year of Surgery',
       y = '# of Patients',
       color = 'Bariatric Procedure',
       title = 'Distribution of Bariatric Surgery Procedures',
       subtitle = 'DURABLE Database: 1997-2015')

ggsave('figures/aligned_t0/application/bs_type_dist.png', height = 9/1.2, width = 16/1.2)
