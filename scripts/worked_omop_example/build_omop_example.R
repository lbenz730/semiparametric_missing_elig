### Worked example to illustrate the use of methods in an OMOP-CDM style
### EHR dataset. We use the omock package to access/build a synthetic dataset
### which follows the OMOP format, which allows for illustration of methods
### while sharing the access to data.

library(omock)
library(tidyverse)
library(lubridate)

### Create Database for tables from 'synthea-weight_loss-10k' synthetic data
### This is the closest synethic dataset to our motivating example of 
### bariatric surgery
### When prompted, download the CDM data
cdm <- mockCdmFromDataset(datasetName = 'synthea-weight_loss-10k')

### Create synthetic data for BMI and A1c in OMOP format since the 
### cdm$measurements table for this example is blank
set.seed(120)
n <- nrow(cdm$person)

bmi_tbl <- 
  cdm$person %>% 
  mutate(
    measurement_id = row_number(),
    measurement_concept_id = 3038553,  # BMI
    measurement_date = as_date("2022-01-01") + sample(0:365, n(), replace = TRUE),
    value_as_number = rnorm(n, mean = 45, sd = 6),  # baseline BMI
    unit_concept_id = 9529
  )

### Simulate A1c (higher on average if BMI is high)
a1c_tbl <- 
  cdm$person %>% 
  mutate(
    measurement_id = row_number() + n,
    measurement_concept_id = 40762387,  # A1c
    measurement_date = as_date("2022-01-01") + sample(0:365, n(), replace = TRUE),
    value_as_number = pmax(4, rnorm(n, mean = 5.2 + 0.06 * (bmi_tbl$value_as_number - 30), sd = 0.7)), # A1c%
    unit_concept_id = 8554
  )

cdm_measures <- bind_rows(bmi_tbl, a1c_tbl)

### Create synthetic data for treatment assignments in OMOP format
patient_summary <- 
  cdm_measures %>% 
  group_by(person_id, race_source_value, gender_source_value,) %>% 
  summarise('baseline_bmi' = value_as_number[measurement_concept_id == 3038553],
            'baseline_a1c' = value_as_number[measurement_concept_id == 40762387],
            'baseline_age' = first(2025 - year_of_birth)) %>% 
  ungroup()

proc_tbl <-
  patient_summary %>% 
  mutate('p_rygb' = 
           plogis(-2 + 0.0268 * (gender_source_value == 'F') + 
                    0.3503 * (race_source_value == 'white') + 
                    0.0163 * baseline_bmi + 
                    0.0030 * baseline_age + 
                    0.2 * baseline_a1c)
  ) %>% 
  mutate('bs_type' = rbinom(n, 1, p_rygb)) %>% 
  mutate(
    procedure_occurrence_id = row_number(),
    procedure_concept_id = case_when(
      bs_type == 1 ~ 2100001104,   # gastric bypass
      bs_type == 0 ~ 2100001105,   # sleeve gastrectomy
      TRUE ~ NA_integer_
    ),
    procedure_date = as_date("2022-06-01") + sample(0:30, n(), replace = TRUE),
    procedure_datetime = as_datetime(procedure_date),
    procedure_type_concept_id = 38000267,  # standard for EHR procedure record
    modifier_concept_id = NA_integer_,
    quantity = 1,
    provider_id = NA_integer_,
    visit_occurrence_id = NA_integer_,
    visit_detail_id = NA_integer_,
    procedure_source_value = case_when(
      bs_type == 1 ~ "Gastric bypass",
      bs_type == 0 ~ "Sleeve gastrectomy",
      TRUE ~ NA_character_
    ),
    procedure_source_concept_id = procedure_concept_id,
    domain_id = "Procedure"
  ) %>% 
  filter(!is.na(procedure_concept_id)) %>% 
  select(-bs_type, -p_rygb)

### Create additional BMI measures for 3 year BMI change in OMOP format
bmi_outcomes <- 
  bmi_tbl %>% 
  left_join(proc_tbl %>% 
              select(person_id, procedure_source_value),
            by = 'person_id') %>% 
  mutate(
    measurement_id = row_number() + 2 * n,
    measurement_concept_id = 3038553,  # BMI
    measurement_date = measurement_date +  3 * sample(365 + seq(-90, 90, 1), n(), replace = TRUE),
    value_as_number = value_as_number * (1 + 
                                           rnorm(n, 
                                                 mean =  -0.06 * (procedure_source_value == 'Gastric bypass') + 
                                                   -0.137 * (gender_source_value == 'F') - 0.0038 * value_as_number  + 
                                                   0.0019739 * (gender_source_value == 'F') * value_as_number +
                                                   -0.0154 * (race_source_value == 'white'), 
                                                 sd = 0.1)),
    unit_concept_id = 9529
  ) %>% 
  select(-procedure_source_value)

cdm_measures <- 
  cdm_measures %>% 
  bind_rows(bmi_outcomes)

### Randomly superimpose missingness in eligibility criteria
observed_elig_ids <- 
  patient_summary %>% 
  mutate('p_R' = plogis(-0.3791 - 0.1489 * (gender_source_value == 'F') + 
                          0.1006 * (race_source_value == 'white') + 
                          0.0113 * baseline_age )) %>% 
  mutate('observe_elig' = rbinom(n, 1, p_R)) %>% 
  filter(observe_elig == 1) %>% 
  pull(person_id)

### Build final analytical dataset, derived from OMOP tables
df_analysis <- 
  cdm$person %>% 
  left_join(proc_tbl %>% 
              select(person_id, procedure_source_value),
            by = 'person_id') %>% 
  left_join(cdm_measures %>% 
              select(person_id, value_as_number, measurement_concept_id),
            by = 'person_id') %>% 
  group_by(person_id, race_source_value, gender_source_value) %>% 
  summarise('baseline_bmi' = first(value_as_number[measurement_concept_id == 3038553]),
            'baseline_a1c' = first(value_as_number[measurement_concept_id == 40762387]),
            'pct_weight_change' = (last(value_as_number[measurement_concept_id == 3038553]) - baseline_bmi)/baseline_bmi,
            'baseline_age' = first(2025 - year_of_birth),
            'bs_type' = as.numeric(first(procedure_source_value == 'Gastric bypass'))) %>% 
  mutate('baseline_bmi' = replace(baseline_bmi, ! person_id %in% observed_elig_ids, NA),
         'baseline_a1c' = replace(baseline_a1c, ! person_id %in% observed_elig_ids, NA))
  mutate('eligible' = as.numeric(baseline_bmi >= 35 & baseline_a1c >= 6.5),
         'R' = as.numeric(!is.na(eligible))) %>% 
  mutate('race_source_value' = ifelse(race_source_value == 'white', 1, 0),
         'gender_source_value' = ifelse(gender_source_value == 'F', 1, 0)) %>% 
  ungroup()

write_csv(df_analysis, 'scripts/worked_omop_example/analysis_dataset.csv')
