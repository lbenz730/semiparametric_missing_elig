library(tidyverse)
library(knitr)
library(kableExtra)
library(xtable)
source('scripts/helpers.R')

df_results <- 
  map_dfr(dir('data/simulations/aligned_t0/outputs', full.names = T), read_csv) %>% 
  mutate('matched_cohort' = ifelse(is.na(matched_cohort), F, matched_cohort)) %>% 
  mutate('dataset_type' = ifelse(matched_cohort, 'Matched Cohort', 'Full Data'))

df_summary <-
  df_results %>% 
  mutate('upper' = tau_hat + qnorm(0.975) * sd,
         'lower' = tau_hat + qnorm(0.025) * sd) %>% 
  group_by(sim_id, estimator, description, correct_models, dataset_type) %>% 
  summarise('pct_bias' = 100 *mean((tau_hat - true_att)/true_att),
            'bias' = mean(tau_hat - true_att),
            'std_dev' = sd(tau_hat),
            'coverage' = 100 * mean(true_att >= lower & true_att <= upper)) %>% 
  ungroup() %>% 
  mutate('std_dev' = scales::scientific(std_dev)) %>% 
  mutate('estimator' = case_when(estimator == 'cc_outcome' ~ '$\\widehat\\theta_\\text{CC}$',
                                 estimator == 'iwor_cc' ~ '$\\widehat\\theta_\\text{IWOR}$',
                                 estimator == 'crossfit_iwor_cc' ~ '$\\widehat\\theta_\\text{IWOR}$',
                                 estimator == 'influence_function' ~ '$\\widehat\\theta_\\text{IF}$',
                                 estimator == 'efficient_influence_function' ~ '$\\widehat\\theta_\\text{EIF}$')) %>% 
  mutate('estimation_strategy' = case_when(grepl('stratify', description) ~ 2,
                                           grepl('interactions', description) ~ 3,
                                           T ~ 1),
         'nuisance_strategy' = case_when(estimator == 'Complete Case Outcome Regression' ~ 'Parametric',
                                         correct_models == '---' ~ 'Nonparametric',
                                         correct_models == 'mu;' & estimator == '$\\widehat\\theta_\\text{EIF}$' ~ 'Nonparametric',
                                         T ~ 'Parametric'),
         'sl_libs' = case_when(grepl('removing LM/GLM', description) ~ 'SL2',
                               nuisance_strategy == 'Nonparametric' ~ 'SL1',
                               T ~ '---')) %>%
  mutate('estimator' = factor(estimator, levels = c('$\\widehat\\theta_\\text{CC}$',
                                                    '$\\widehat\\theta_\\text{IWOR}$',
                                                    '$\\widehat\\theta_\\text{IF}$',
                                                    '$\\widehat\\theta_\\text{EIF}$'))) %>% 
  mutate('correct_models' = gsub('pi', '\\\\pi', correct_models),
         'correct_models' = gsub('eta', '\\\\eta', correct_models),
         'correct_models' = gsub('lambda', '\\\\lambda', correct_models),
         'correct_models' = gsub('mu', '\\\\mu', correct_models)) %>% 
  mutate('correct_models' = case_when(correct_models == '---' ~ correct_models,
                                      T ~ paste0('$', correct_models, '$'))) 


### main simulation table
options(knitr.kable.NA = '---')
df1 <- 
  df_summary %>% 
  filter(sim_id == 1 | sim_id == 8) %>% 
  pivot_wider(names_from = c('sim_id'),
              values_from = c('pct_bias', 'std_dev', 'coverage'),
              id_cols = c('estimator', 'description', 'correct_models', contains('strategy'),'sl_libs'))  %>% 
  select(estimator, correct_models, nuisance_strategy, estimation_strategy, sl_libs, everything(),
         -description) %>% 
  group_by(estimator) %>% 
  arrange(estimator, 
          desc(nuisance_strategy),
          sl_libs,
          estimation_strategy) %>% 
  select(estimator, nuisance_strategy, sl_libs, estimation_strategy, correct_models, 
         ends_with('_1'), ends_with('_8')) %>% 
  ungroup() %>% 
  slice(-c(2,12,13, 20)) %>% 
  ### Key them by estimator so it collapses as we want
  mutate('sl_libs' = paste0('ZzZ', estimator, 'ZzZ', sl_libs)) %>% 
  mutate('estimation_strategy' = paste0('ZzZ', estimator, nuisance_strategy, 'ZzZ ', estimation_strategy)) %>% 
  mutate('nuisance_strategy' = paste0('ZzZ', estimator, 'ZzZ ', nuisance_strategy)) %>% 
  mutate('correct_models' = paste0('ZzZ', estimator, 'ZzZ ', correct_models))



ltx_1 <- 
  df1 %>% 
  kbl(align = 'c',
      format = 'latex', 
      booktabs = T,
      escape = F,
      digits = c(0, 0, 0, 0, 0, 1, 2, 1, 1, 2, 1),
      col.names = c('Estimator',
                    'Strategy',
                    'SL Libs$\\textsuperscript{a}$',
                    '$\\mu_0$ Strategy$\\textsuperscript{b}$',
                    'True $\\mu/\\eta$\\textsuperscript{c}',
                    '\\%-Bias',
                    'SD',
                    'Coverage',
                    '\\%-Bias',
                    'SD',
                    'Coverage'), 
      caption = 'Comparison of estimators of $\\theta(P)$ in simulation study', label = 'sim_results') %>% 
  add_footnote(list('SL Libs = $\\texttt{SuperLearner}$ libraries: S1 = {Random Forest, LM/GLM, GAM, Polymars}; SL2 = {Random Forest, GAM, Polymars}',
                    '$\\mu_0$ Strategy: (1) Fit single $\\widehat\\mu$ on $A = 0,1$ together (2) Fit $\\widehat\\mu_0$ on $A = 0$ only (stratification) (3) Specify all $A\\times\\bm L$ interactions in design matrix for $\\widehat\\mu$',
                    'Correctly specified parametric models for $\\mu$ and/or $\\eta$'), 
               escape = F) %>% 
  collapse_rows(columns = c(1:5), 
                valign = 'middle') %>% 
  add_header_above(c(' ' = 5,
                     '$n = 10,000$ Patients' = 3, 
                     '$n = 25,000$ Patients' = 3), bold = T, escape = F) %>% 
  row_spec(0, bold = T) %>% 
  kable_styling() %>% 
  gsub('\\{c\\|c\\|c\\|c\\|c\\|c\\|c\\|c\\|c\\|c\\|c\\}',
       '\\{|Sc\\|Sc\\|Sc\\|Sc\\|Sc\\|Sc\\|Sc\\|Sc\\|Sc\\|Sc\\|Sc\\|}', .) %>% 
  gsub('\\\\begin\\{table\\}', '\\\\begin\\{table\\}\\\\tiny', .) %>% 
  gsub('ZzZ\\$\\\\widehat\\\\theta_\\\\text\\{[A-Za-z]{1,4}\\}\\$ZzZ', '', .) %>% 
  gsub('ZzZ\\$\\\\widehat\\\\theta_\\\\text\\{[A-Za-z]{1,4}\\}\\$(Parametric|Nonparametric)ZzZ', '', .) %>% 
  gsub('\\\\multirow\\{-6\\}\\{\\*\\}\\{\\\\centering','\\\\multirow\\{-6\\}\\{\\*\\}[15pt]\\{\\\\centering', .) %>% 
  gsub('\\\\multirow\\{-3\\}\\{\\*\\}\\{\\\\centering','\\\\multirow\\{-3\\}\\{\\*\\}[5pt]\\{\\\\centering', .) %>% 
  gsub('\\\\multirow\\{-9\\}\\{\\*\\}\\{\\\\centering','\\\\multirow\\{-9\\}\\{\\*\\}[25pt]\\{\\\\centering', .) %>% 
  gsub('\\\\bottomrule', '\\\\bottomrule\\\\\\\\', .)
write(ltx_1, 'tables/sim_results_main.tex')

### Supplement Table
df2 <- 
  df_summary %>% 
  filter(sim_id >= 2 & sim_id <= 7) %>% 
  filter(sim_id %in% c(2,3) | dataset_type == 'Matched Cohort') %>% 
  select(-bias) %>% 
  pivot_wider(names_from = c('dataset_type', 'sim_id'),
              id_cols = c('estimator', 'description', 'correct_models', contains('strategy'),'sl_libs'),
              values_from = c('pct_bias', 'std_dev', 'coverage')) %>% 
  group_by(estimator) %>% 
  arrange(estimator, 
          desc(nuisance_strategy),
          sl_libs,
          estimation_strategy) %>% 
  mutate('matching' = ifelse(grepl('Restrictive', description), 'Restrictive', 'Agnostic')) %>% 
  select(estimator, nuisance_strategy, sl_libs, estimation_strategy, correct_models, matching,
         ends_with('Data_2'), ends_with('Cohort_2'), ends_with('_4'), ends_with('_6'),
         ends_with('Data_3'), ends_with('Cohort_3'), ends_with('_5'), ends_with('_7')) %>% 
  ungroup() %>% 
  slice(-c(12,13, 20)) %>% 
  ### Key them by estimator so it collapses as we want
  mutate('sl_libs' = paste0('ZzZ', estimator, 'ZzZ', sl_libs)) %>% 
  mutate('estimation_strategy' = paste0('ZzZ', estimator, nuisance_strategy, 'ZzZ ', estimation_strategy)) %>% 
  mutate('nuisance_strategy' = paste0('ZzZ', estimator, 'ZzZ ', nuisance_strategy)) %>% 
  mutate('correct_models' = paste0('ZzZ', estimator, 'ZzZ ', correct_models)) %>% 
  mutate('matching' = paste0('ZzZ', correct_models, 'ZzZ ', matching)) 

df2a <- 
  df2 %>% 
  select(estimator, nuisance_strategy, sl_libs, estimation_strategy, correct_models, matching,
         ends_with('Data_2'), ends_with('Cohort_2'), ends_with('_4'), ends_with('_6'))

df2b <- 
  df2 %>% 
  select(estimator, nuisance_strategy, sl_libs, estimation_strategy, correct_models, matching,
         ends_with('Data_3'), ends_with('Cohort_3'), ends_with('_5'), ends_with('_7'))

ltx_2a <-
  df2a %>% 
  kbl(align = 'c',
      format = 'latex',
      booktabs = T,
      escape = F,
      digits = c(0, 0, 0, 0, 0, 0, rep(c(1,2,1), 4)),
      col.names = c('Estimator',
                    'Strategy',
                    'SL Libs$\\textsuperscript{a}$',
                    '$\\mu_0$ Strategy$\\textsuperscript{b}$',
                    'True $\\mu/\\eta$\\textsuperscript{c}',
                    'Matching',
                    rep(c('\\%-Bias','SD','Coverage'), 4)), 
      caption = 'Comparison of estimators of $\\theta(P)$ in simulation study, with matched cohort designs ($n = 10,000$).', label = 'sim_results_matching_a') %>% 
  add_footnote(list('SL Libs = $\\texttt{SuperLearner}$ libraries: S1 = {Random Forest, LM/GLM, GAM, Polymars}; SL2 = {Random Forest, GAM, Polymars}',
                    '$\\mu_0$ Strategy: (1) Fit single $\\widehat\\mu$ on $A = 0,1$ together (2) Fit $\\widehat\\mu_0$ on $A = 0$ only (stratification) (3) Specify all $A\\times\\bm L$ interactions in design matrix for $\\widehat\\mu$',
                    'Correctly specified parametric models for $\\mu$ and/or $\\eta$'), 
               escape = F) %>% 
  collapse_rows(columns = c(1:6), 
                valign = 'middle') %>% 
  add_header_above(c(' ' = 6,
                     'Full Data' = 3,
                     'Matched Cohort A' = 3,
                     'Matched Cohort B' = 3,
                     'Matched Cohort C' = 3), bold = T, escape = F) %>%   
  add_header_above(c(' ' = 6,'$n = 10,000$ Patients' = 12), bold = T, escape = F) %>% 
  row_spec(0, bold = T) %>% 
  kable_styling() %>% 
  gsub('cccccccccccccccccc', 'ccccccc@{}c@{}cc@{}c@{}cc@{}c@{}cc@{}c@{}c', .) %>% 
  gsub('\\\\begin\\{table\\}', '\\\\begin\\{table\\}\\\\tiny', .) %>% 
  gsub('ZzZ\\$\\\\widehat\\\\theta_\\\\text\\{[A-Za-z]{1,4}\\}\\$ZzZ', '', .) %>% 
  gsub('ZzZ\\$\\\\widehat\\\\theta_\\\\text\\{[A-Za-z]{1,4}\\}\\$(Parametric|Nonparametric)ZzZ', '', .) %>% 
  gsub('\\\\multirow\\{-6\\}\\{\\*\\}\\{\\\\centering','\\\\multirow\\{-6\\}\\{\\*\\}[15pt]\\{\\\\centering', .) %>% 
  gsub('\\\\multirow\\{-3\\}\\{\\*\\}\\{\\\\centering','\\\\multirow\\{-3\\}\\{\\*\\}[5pt]\\{\\\\centering', .) %>% 
  gsub('\\\\multirow\\{-2\\}\\{\\*\\}\\{\\\\centering','\\\\multirow\\{-2\\}\\{\\*\\}[3pt]\\{\\\\centering', .) %>% 
  gsub('\\\\multirow\\{-9\\}\\{\\*\\}\\{\\\\centering','\\\\multirow\\{-9\\}\\{\\*\\}[25pt]\\{\\\\centering', .) %>% 
  gsub('\\\\bottomrule', '\\\\bottomrule\\\\\\\\', .) %>% 
  gsub('ZzZ.*?ZzZ', '', .)

ltx_2b <-
  df2b %>% 
  kbl(align = 'c',
      format = 'latex',
      booktabs = T,
      escape = F,
      digits = c(0, 0, 0, 0, 0, 0, rep(c(1,2,1), 4)),
      col.names = c('Estimator',
                    'Strategy',
                    'SL Libs$\\textsuperscript{a}$',
                    '$\\mu_0$ Strategy$\\textsuperscript{b}$',
                    'True $\\mu/\\eta$\\textsuperscript{c}',
                    'Matching',
                    rep(c('\\%-Bias','SD','Coverage'), 4)), 
      caption = 'Comparison of estimators of $\\theta(P)$ in simulation study, with matched cohort designs ($n = 25,000$).', label = 'sim_results_matching') %>% 
  add_footnote(list('SL Libs = $\\texttt{SuperLearner}$ libraries: S1 = {Random Forest, LM/GLM, GAM, Polymars}; SL2 = {Random Forest, GAM, Polymars}',
                    '$\\mu_0$ Strategy: (1) Fit single $\\widehat\\mu$ on $A = 0,1$ together (2) Fit $\\widehat\\mu_0$ on $A = 0$ only (stratification) (3) Specify all $A\\times\\bm L$ interactions in design matrix for $\\widehat\\mu$',
                    'Correctly specified parametric models for $\\mu$ and/or $\\eta$'), 
               escape = F) %>% 
  collapse_rows(columns = c(1:6), 
                valign = 'middle') %>% 
  add_header_above(c(' ' = 6,
                     'Full Data' = 3,
                     'Matched Cohort A' = 3,
                     'Matched Cohort B' = 3,
                     'Matched Cohort C' = 3), bold = T, escape = F) %>%   
  add_header_above(c(' ' = 6,'$n = 25,000$ Patients' = 12), bold = T, escape = F) %>% 
  row_spec(0, bold = T) %>% 
  kable_styling() %>% 
  gsub('cccccccccccccccccc', 'ccccccc@{}c@{}cc@{}c@{}cc@{}c@{}cc@{}c@{}c', .) %>% 
  gsub('\\\\begin\\{table\\}', '\\\\begin\\{table\\}\\\\tiny', .) %>% 
  gsub('ZzZ\\$\\\\widehat\\\\theta_\\\\text\\{[A-Za-z]{1,4}\\}\\$ZzZ', '', .) %>% 
  gsub('ZzZ\\$\\\\widehat\\\\theta_\\\\text\\{[A-Za-z]{1,4}\\}\\$(Parametric|Nonparametric)ZzZ', '', .) %>% 
  gsub('\\\\multirow\\{-6\\}\\{\\*\\}\\{\\\\centering','\\\\multirow\\{-6\\}\\{\\*\\}[15pt]\\{\\\\centering', .) %>% 
  gsub('\\\\multirow\\{-3\\}\\{\\*\\}\\{\\\\centering','\\\\multirow\\{-3\\}\\{\\*\\}[5pt]\\{\\\\centering', .) %>% 
  gsub('\\\\multirow\\{-2\\}\\{\\*\\}\\{\\\\centering','\\\\multirow\\{-2\\}\\{\\*\\}[3pt]\\{\\\\centering', .) %>% 
  gsub('\\\\multirow\\{-9\\}\\{\\*\\}\\{\\\\centering','\\\\multirow\\{-9\\}\\{\\*\\}[25pt]\\{\\\\centering', .) %>% 
  gsub('\\\\bottomrule', '\\\\bottomrule\\\\\\\\', .) %>% 
  gsub('ZzZ.*?ZzZ', '', .)


write(paste(ltx_2a, ltx_2b, sep = '\n\n'), 'tables/sim_results_matching.tex')


### Table of Simulation Parameters
gather_coeff <- function(params) {
  bind_rows(
    tibble('model' = 'Treatment',
           'model_num' = 1,
           'term_num' = 1,
           'coeff_greek' = '$\\bm \\beta_\\pi$',
           'term' = names(params$models$pi),
           'coeff_value' = unlist(params$models$pi)),
    
    tibble('model' = 'Missingness',
           'model_num' = 2,
           'term_num' = 1,
           'coeff_greek' = '$\\bm \\beta_\\eta$',
           'term' = names(params$models$eta),
           'coeff_value' = unlist(params$models$eta)),
    
    tibble('model' = 'Eligibility Defining Covariate',
           'model_num' = 3,
           'term_num' = 1,
           'coeff_greek' = '$\\bm \\beta_\\lambda$',
           'term' = names(params$models$lambda),
           'coeff_value' = unlist(params$models$lambda)),
    
    tibble('model' = 'Eligibiliity Defining Covariate',
           'model_num' = 3,
           'term_num' = 1,
           'coeff_greek' = '$\\alpha_\\lambda$',
           'term' = '---',
           'coeff_value' = unlist(params$models$lambda_shape)),
    
    tibble('model' = 'Outcome',
           'model_num' = 4,
           'term_num' = 1,
           'coeff_greek' = '$\\bm \\beta_\\mu$',
           'term' = names(params$models$mu),
           'coeff_value' = unlist(params$models$mu)),
    
    tibble('model' = 'Outcome',
           'model_num' = 4,
           'term_num' = 2,
           'coeff_greek' = '$\\sigma^2_{\\text{y}}$',
           'term' = '---',
           'coeff_value' = params$models$sigma_rss^2),
  ) %>% 
    mutate('term' = gsub('\\_', '\\\\_', term)) %>% 
    mutate('term' = gsub('\\^', '\\\\^', term))
}

sci_notation <- function(x) {
  case_when(is.na(x) ~ '0',
            x == 0 ~ '0',
            abs(x) > 0.1 ~ sprintf('%0.2f', x),
            T ~ sanitize.numbers(format(x, 
                                        digits = 2,
                                        scientific = T),
                                 type = 'latex',
                                 math.style.exponents = T))
  
}



params1 <- read_rds('data/simulations/aligned_t0/inputs/params_1.rds')
params2 <- read_rds('data/simulations/aligned_t0/inputs/params_2.rds')

df_tbl <- 
  bind_rows(
    gather_coeff(params1) %>% mutate('params' = '1'),
    gather_coeff(params2) %>% mutate('params' = '2')
  ) %>% 
  mutate('term' = paste0('\\texttt{', term, '}')) %>% 
  mutate('coeff_value' = map_chr(coeff_value, sci_notation)) %>% 
  pivot_wider(names_from = 'params',
              values_from = 'coeff_value',
              names_prefix = 'sim_') %>% 
  arrange(model_num, term_num) %>% 
  select(-model_num, -term_num) %>% 
  mutate_at(vars(contains('sim')), ~ifelse(is.na(.x), '0', .x))

caption <- 
  paste('\\caption{Coefficients values used to generate simulated datasets. Simulation 1 is the simulation study presented in the main paper.',
        'Simulation 2 is the simulation study evaluating the eligibility agnostic matched cohort.}')

label <- '\\label{table:sim_coef}'

latex <-
  df_tbl %>% 
  kbl(align = 'c', 
      digits = 3 , 
      escape = F, 
      format = 'latex',
      col.names = c('Component Model', 'Coefficient', 'Covariate', 'Simulation 1', 'Simulation 2')) %>% 
  row_spec(0, bold = T) %>% 
  column_spec(1, border_left = T)  %>%
  column_spec(ncol(df_tbl), border_right = T) %>% 
  collapse_rows(columns = c(1, 2), valign = 'middle') %>% 
  add_header_above(c('\\\\textbf{Model Information}' = 3, ' ' = 2), escape = F) %>%
  paste0("\\begin{table}\n\\tiny\n\\centering", ., '\n\\end{table}') %>% 
  gsub('\\\\multicolumn\\{3\\}\\{c\\|\\}\\{\\\\textbf\\{Model Information\\}\\}',
       '\\\\multicolumn\\{3\\}\\{\\|Sc\\|\\}\\{\\\\textbf\\{Model Information\\}\\}', .) %>% 
  gsub('tabular\\}\\[t]', 'tabular\\}', .) %>% 
  gsub('c\\|\\c\\|c\\|c\\|>\\{\\}c', 'Sc\\|Sc\\|Sc\\|Sc>{}Sc', .) %>% 
  paste0(., label) 


latex <- unlist(strsplit(latex, '\\\\end\\{tabular\\}'))
latex <- paste0(latex[1], paste0('\\end{tabular}', caption), latex[2])
write(latex, 'tables/sim_coeff.tex')
