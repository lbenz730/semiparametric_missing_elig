library(readr)
### Simulation Setting 1
params1 <- 
  list('n_subjects' = 10000,
       'n_sims' = 1000,
       'treatment' = 'bs_type',
       'outcome' = 'pct_wt_change',
       'elig' = 'baseline_a1c',
       'Lc' = c('gender', 'site', 'race', 
                'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR'),
       'matching' = F,
       'models' = list(
         ### Pi
         ### A | Lc
         'pi' = list(
           "(Intercept)" = 0.959,
           "site[NC]" = -0.6393,
           "site[SC]" = -0.9596,
           "gender" = 0.0268,
           "race" = 0.3503,
           "baseline_bmi" = 0.0163,
           "smoking_status[former]" = -0.2934,
           "smoking_status[never]" = -0.2310,
           "smoking_status[no_self_report]" = -0.3226,
           "baseline_age" = 0.0030,
           "eGFR" = -0.0050
         ),
         
         ### Eta
         ### R | Lc, A
         'eta' = list(
           "(Intercept)" = 0.3791,
           "site[NC]" = -0.3783,
           "site[SC]" = 0.7947,
           "gender" = -0.1489,
           "race" = 0.1006,
           "baseline_bmi" = -0.0196,
           "smoking_status[former]" = 0.4353,
           "smoking_status[never]" = 0.3235,
           "smoking_status[no_self_report]" = -2.5848,
           "baseline_age" = 0.0113,
           "eGFR" = -0.0001,
           "bs_type" = 0.4970
         ), 
         
         ### Lambda
         ### Le | Lc, A, R = 1
         'lambda' = list(
           "(Intercept)" = 1.0633,
           "site[NC]" = 0.2347,
           "site[SC]" = -0.2409,
           "gender" = -0.1050,
           "race" = -0.0686,
           "baseline_bmi" = -0.0075,
           "I(baseline_bmi^2)" = 0.0001,
           "smoking_status[former]" = -0.0567,
           "smoking_status[never]" = -0.0763,
           "smoking_status[no_self_report]" = -0.0938,
           "baseline_age" = 0.0092,
           "eGFR" = 0.0007,
           "bs_type" = 0.1049
         ), 
         'lambda_shape' = 4.825344,
         
         ### Y | Lc, Le, A, R = 1
         'mu' = list(
           "(Intercept)" =  -0.236,
           "bs_type" = 0.0327,
           "site[NC]" = 0.177,
           "site[SC]" = 0.136,
           "gender" = -0.137,
           "race" = -0.0154,
           "baseline_bmi" = -0.0038,
           "smoking_status[former]" = 0.0384,
           "smoking_status[never]" = 0.0486,
           "smoking_status[no_self_report]" = -0.152,
           "baseline_age" = 0.00097,
           "eGFR" = 0.00014,
           "baseline_a1c" = 0.000222,
           "bs_type:baseline_a1c" = 0.00378,
           "gender:baseline_a1c" = 0.00484,
           "gender:baseline_bmi" =  0.0019739,
           "smoking_status[no_self_report]:bs_type" = 0.173,
           "smoking_status[never]:bs_type" = -0.0235,
           "smoking_status[former]:bs_type" = -0.0235,
           "site[NC]:bs_type" = -0.123,
           "site[SC]:bs_type" = -0.105
         ),
         'sigma_rss' =  0.1008916
         
       ))

write_rds(params1, 'data/simulations/aligned_t0/inputs/params_1.rds')


### Larger N for Simulation 1
params8 <- params1
params8$n_subjects <- 25000
write_rds(params8, 'data/simulations/aligned_t0/inputs/params_8.rds')

### Simulation Setting 2 for matched cohort
params2 <- 
  list('n_subjects' = 10000,
       'n_sims' = 1000,
       'treatment' = 'bs_type',
       'outcome' = 'pct_wt_change',
       'elig' = 'baseline_a1c',
       'Lc' = c('gender', 'site', 'race', 
                'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR'),
       'matching' = T,
       'matching_info' = list('vars' = c('gender', 'race'),
                              'pre_elig' = T,
                              'ratio' = 1),
       'models' = list(
         ### Pi
         ### A | Lc
         'pi' = list(
           "(Intercept)" = -1.3,
           "site[NC]" = -0.393,
           "site[SC]" = -0.596,
           "gender" = 0.0268,
           "race" = 0.3503,
           "baseline_bmi" = 0.0163,
           "smoking_status[former]" = -0.2934,
           "smoking_status[never]" = -0.2310,
           "smoking_status[no_self_report]" = -0.3226,
           "baseline_age" = 0.0030,
           "eGFR" = -0.0050
         ),
         
         ### Eta
         ### R | Lc, A
         'eta' = list(
           "(Intercept)" = 0.3791,
           "site[NC]" = -0.3783,
           "site[SC]" = 0.7947,
           "gender" = -0.1489,
           "race" = 0.1006,
           "baseline_bmi" = -0.0196,
           "smoking_status[former]" = 0.4353,
           "smoking_status[never]" = 0.3235,
           "smoking_status[no_self_report]" = -2.5848,
           "baseline_age" = 0.0113,
           "eGFR" = -0.0001,
           "bs_type" = 0.4970
         ), 
         
         ### Lambda
         ### Le | Lc, A, R = 1
         'lambda' = list(
           "(Intercept)" = 1.0633,
           "site[NC]" = 0.2347,
           "site[SC]" = -0.2409,
           "gender" = -0.1050,
           "race" = -0.0686,
           "baseline_bmi" = -0.0075,
           "I(baseline_bmi^2)" = 0.0001,
           "smoking_status[former]" = -0.0567,
           "smoking_status[never]" = -0.0763,
           "smoking_status[no_self_report]" = -0.0938,
           "baseline_age" = 0.0092,
           "eGFR" = 0.0007,
           "bs_type" = 0.1049
         ), 
         'lambda_shape' = 4.825344,
         
         ### Y | Lc, Le, A, R = 1
         'mu' = list(
           "(Intercept)" =  0.1,
           "bs_type" = 0.0327,
           "site[NC]" = 0.177,
           "site[SC]" = 0.136,
           "gender" = -0.137,
           "race" = -0.0154,
           "baseline_bmi" = -0.0038,
           "smoking_status[former]" = 0.0384,
           "smoking_status[never]" = 0.0486,
           "smoking_status[no_self_report]" = -0.152,
           "baseline_age" = 0.00097,
           "eGFR" = 0.00014,
           "baseline_a1c" = 0.000222,
           "bs_type:baseline_a1c" = 0.00378,
           "gender:baseline_a1c" = 0.00484,
           "gender:baseline_bmi" =  0.0019739,
           "smoking_status[no_self_report]:bs_type" = 0.173,
           "smoking_status[never]:bs_type" = -0.0235,
           "smoking_status[former]:bs_type" = -0.0235,
           "site[NC]:bs_type" = -0.123,
           "site[SC]:bs_type" = -0.105
         ),
         'sigma_rss' =  0.1008916
         
       ))

write_rds(params2, 'data/simulations/aligned_t0/inputs/params_2.rds')

### Simulation 3 w/ 25000 subjects
params3 <- params2
params3$n_subjects <- 25000
write_rds(params3, 'data/simulations/aligned_t0/inputs/params_3.rds')

### Simulation 4/5
### Matched Cohort where we match on effect modifiers
params4 <- params2
params5 <- params3
params4$matching_info$vars <- c('site', 'smoking_status') 
params5$matching_info$vars <- c('site', 'smoking_status') 
write_rds(params4, 'data/simulations/aligned_t0/inputs/params_4.rds')
write_rds(params5, 'data/simulations/aligned_t0/inputs/params_5.rds')

### Simulation 6/7
### Matched Cohort where we match on all non-eligibility confounders
params6 <- params2
params7 <- params3
params6$matching_info$vars <- c('site', 'smoking_status', 'gender', 'race') 
params7$matching_info$vars <- c('site', 'smoking_status', 'gender', 'race') 
write_rds(params6, 'data/simulations/aligned_t0/inputs/params_6.rds')
write_rds(params7, 'data/simulations/aligned_t0/inputs/params_7.rds')


### Specify Estimator Types
### 
### For super_learner type formula is formula for model.matrix to 
### convert data frame into design matrix, not formula for parametric model
### 
### Additionally for super_learner, the sl_libs argument specifies which 
### libraries to use in addition a suite of random forest options, which 
### are used for all super_learners. 
estimators <- 
  list('CC Outcome Regression' =
         list('estimator' = 'cc_outcome',
              'true_models' = c('mu'),
              'description' = 'Standard Outcome Regression for ATT among eligible complete cases',
              'mu' = list('type' = 'LM', 
                          'formula' = pct_wt_change ~ bs_type + site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR + baseline_a1c + 
                            bs_type:baseline_a1c +  baseline_a1c:gender + gender:baseline_bmi + bs_type:smoking_status + site:bs_type)
         ),
       
       ### IWOR Estimators
       'Inverse Weighted Outcome Regression' =
         list('estimator' = 'iwor_cc',
              'description' = 'Inverse Weighted Outcome Regression for ATT among eligible complete cases',
              'true_models' = c('eta', 'mu'),
              'mu' = list('type' = 'LM', 
                          'stratify' = F,
                          'formula' = pct_wt_change ~ bs_type + site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR + baseline_a1c + 
                            bs_type:baseline_a1c +  baseline_a1c:gender + gender:baseline_bmi + bs_type:smoking_status + site:bs_type),
              'eta' = list('type' = 'GLM',
                           'formula' = R ~ bs_type + site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR)
         ),
       
       'Inverse Weighted Outcome Regression (Missingness Misspecified)' =
         list('estimator' = 'iwor_cc',
              'description' = 'Inverse Weighted Outcome Regression for ATT among eligible complete cases, but missingness weights are incorrect',
              'true_models' = c('mu'),
              'mu' = list('type' = 'LM', 
                          'stratify' = F,
                          'formula' = pct_wt_change ~ bs_type + site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR + baseline_a1c + 
                            bs_type:baseline_a1c +  baseline_a1c:gender + gender:baseline_bmi + bs_type:smoking_status + site:bs_type),
              'eta' = list('type' = 'GLM',
                           'formula' = R ~ bs_type + site + gender + race + baseline_age + eGFR)
         ),
       
       'Inverse Weighted Outcome Regression (Outcome Model Misspecified)' =
         list('estimator' = 'iwor_cc',
              'description' = 'Inverse Weighted Outcome Regression for ATT among eligible complete cases, but outcome model is incorrect',
              'true_models' = c('eta'),
              'mu' = list('type' = 'LM', 
                          'stratify' = F,
                          'formula' = pct_wt_change ~ bs_type + site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR + baseline_a1c),
              'eta' = list('type' = 'GLM',
                           'formula' = R ~ bs_type + site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR)
         ),
       
       ### Influence Function Estimators
       'IF (All Parametric)' = 
         list('estimator' = 'influence_function',
              'description' = 'IF estimator w/ true parametric models based on liklihood factorization',
              'true_models' = c('pi', 'eta', 'lambda', 'mu'),
              'n_splits' = 2,
              'pi' = list('type' = 'GLM',
                          'formula' = bs_type ~ site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR),
              'eta' = list('type' = 'GLM',
                           'formula' = R ~ bs_type + site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR),
              'lambda' = list('type' = 'GLM_gamma',
                              'formula' = I(baseline_a1c - 3) ~ site + gender + race + baseline_bmi + I(baseline_bmi^2) + smoking_status + baseline_age + eGFR + bs_type),
              'mu' = list('type' = 'LM', 
                          'stratify' = F,
                          'formula' = pct_wt_change ~ bs_type + site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR + baseline_a1c + 
                            bs_type:baseline_a1c +  baseline_a1c:gender + gender:baseline_bmi + bs_type:smoking_status + site:bs_type),
              'epsilon' = list('type' = 'GLM', 
                               'formula' = eligible ~ bs_type + site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR),
              'nu1' = list('type' = 'mixture',
                           'mu_tilde_1' = list('type' = 'LM',
                                               'formula' = pct_wt_change ~ site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR + gender:baseline_bmi)
              ),
              'nu0' = list('type' = 'mixture',
                           'mu_tilde_0' = list('type' = 'LM',
                                               'formula' = mu0_hat ~ site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR + gender:baseline_bmi)
              )
         ),
       
       'IF (Known Parametric/Unknown Non-Parametric)' = 
         list('estimator' = 'influence_function',
              'description' = 'IF estimator w/ known models parametric and unknown models non-parametric',
              'true_models' = c('pi', 'eta', 'lambda', 'mu'),
              'n_splits' = 2,
              'pi' = list('type' = 'GLM',
                          'formula' = bs_type ~ site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR),
              'eta' = list('type' = 'GLM',
                           'formula' = R ~ bs_type + site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR),
              'lambda' = list('type' = 'GLM_gamma',
                              'formula' = I(baseline_a1c - 3) ~ site + gender + race + baseline_bmi + I(baseline_bmi^2) + smoking_status + baseline_age + eGFR + bs_type),
              'mu' = list('type' = 'LM', 
                          'stratify' = F,
                          'formula' = pct_wt_change ~ bs_type + site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR + baseline_a1c + 
                            bs_type:baseline_a1c +  baseline_a1c:gender + gender:baseline_bmi + bs_type:smoking_status + site:bs_type),
              'epsilon' = list('type' = 'super_learner',
                               'Y' = 'eligible',
                               'X' = c('bs_type', 'site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR'),
                               'formula' = ~ bs_type + site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR,
                               'sl_libs' = c('SL.glm', 'SL.gam')),
              'nu1' = list('type' = 'super_learner',
                           'Y' = 'EY',
                           'X' = c('site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR'),
                           'formula' = ~ site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR,
                           'sl_libs' = c('SL.polymars', 'SL.lm')),
              'nu0' = list('type' = 'super_learner',
                           'Y' = 'Emu0',
                           'X' = c('site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR'),
                           'formula' = ~ site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR,
                           'sl_libs' = c('SL.polymars', 'SL.lm'))
              
         ),
       
       'IF (Fully Non-Parametric)' = 
         list('estimator' = 'influence_function',
              'description' = 'IF estimator w/ non-parametric nuisance fx estimation',
              'true_models' = c('---'),
              'n_splits' = 2,
              'pi' = list('type' = 'super_learner',
                          'Y' = 'bs_type', 
                          'X' = c('site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR'),
                          'formula' = ~site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR,
                          'sl_libs' = c('SL.glm', 'SL.gam')),
              'eta' = list('type' = 'super_learner',
                           'Y' = 'R', 
                           'X' = c('bs_type', 'site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR'),
                           'formula' = ~bs_type + site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR,
                           'sl_libs' = c('SL.glm', 'SL.gam')),
              'lambda' = list('type' = 'super_learner',
                              'Y' = 'bs_type',
                              'X' = c('site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR', 'baseline_a1c'),
                              'formula' = ~site + gender + race  + baseline_bmi + smoking_status + baseline_age + eGFR + baseline_a1c,
                              'sl_libs' = c('SL.glm', 'SL.gam') ### via re-parameterization to u (full propensity score)
              ),
              'mu' = list('type' = 'super_learner', 
                          'Y' = 'pct_wt_change',
                          'stratify' = F,
                          'X' = c('bs_type', 'site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR', 'baseline_a1c'),
                          'formula' = ~bs_type + site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR + baseline_a1c,
                          'sl_libs' = c('SL.polymars', 'SL.lm')),
              'epsilon' = list('type' = 'super_learner',
                               'Y' = 'eligible',
                               'X' = c('bs_type', 'site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR'),
                               'formula' = ~ bs_type + site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR,
                               'sl_libs' = c('SL.glm', 'SL.gam')),
              'nu1' = list('type' = 'super_learner',
                           'Y' = 'EY',
                           'X' = c('site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR'),
                           'formula' = ~ site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR,
                           'sl_libs' = c('SL.polymars', 'SL.lm')),
              'nu0' = list('type' = 'super_learner',
                           'Y' = 'Emu0',
                           'X' = c('site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR'),
                           'formula' = ~ site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR,
                           'sl_libs' = c('SL.polymars', 'SL.lm'))
              
         ),
       
       'IF (Fully Non-Parametric, mu A = 0 Only)' = 
         list('estimator' = 'influence_function',
              'description' = 'IF estimator w/ non-parametric nuisance fx estimation, stratify estimation of mu',
              'true_models' = c('---'),
              'n_splits' = 2,
              'pi' = list('type' = 'super_learner',
                          'Y' = 'bs_type', 
                          'X' = c('site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR'),
                          'formula' = ~site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR,
                          'sl_libs' = c('SL.glm', 'SL.gam')),
              'eta' = list('type' = 'super_learner',
                           'Y' = 'R', 
                           'X' = c('bs_type', 'site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR'),
                           'formula' = ~bs_type + site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR,
                           'sl_libs' = c('SL.glm', 'SL.gam')),
              'lambda' = list('type' = 'super_learner',
                              'Y' = 'bs_type',
                              'X' = c('site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR', 'baseline_a1c'),
                              'formula' = ~site + gender + race  + baseline_bmi + smoking_status + baseline_age + eGFR + baseline_a1c,
                              'sl_libs' = c('SL.glm', 'SL.gam') ### via re-parameterization to u (full propensity score)
              ),
              'mu' = list('type' = 'super_learner', 
                          'stratify' = T,
                          'Y' = 'pct_wt_change',
                          'X' = c('site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR', 'baseline_a1c'),
                          'formula' = ~site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR + baseline_a1c,
                          'sl_libs' = c('SL.polymars', 'SL.lm')),
              'epsilon' = list('type' = 'super_learner',
                               'Y' = 'eligible',
                               'X' = c('bs_type', 'site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR'),
                               'formula' = ~ bs_type + site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR,
                               'sl_libs' = c('SL.glm', 'SL.gam')),
              'nu1' = list('type' = 'super_learner',
                           'Y' = 'EY',
                           'X' = c('site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR'),
                           'formula' = ~ site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR,
                           'sl_libs' = c('SL.polymars', 'SL.lm')),
              'nu0' = list('type' = 'super_learner',
                           'Y' = 'Emu0',
                           'X' = c('site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR'),
                           'formula' = ~ site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR,
                           'sl_libs' = c('SL.polymars', 'SL.lm'))
              
         ),
       
       
       'IF (Fully Non-Parametric, Treatment interactions in mu)' = 
         list('estimator' = 'influence_function',
              'description' = 'IF estimator w/ non-parametric nuisance fx estimation, treatment interactions in outcome model',
              'true_models' = c('---'),
              'n_splits' = 2,
              'pi' = list('type' = 'super_learner',
                          'Y' = 'bs_type', 
                          'X' = c('site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR'),
                          'formula' = ~site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR,
                          'sl_libs' = c('SL.glm', 'SL.gam')),
              'eta' = list('type' = 'super_learner',
                           'Y' = 'R', 
                           'X' = c('bs_type', 'site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR'),
                           'formula' = ~bs_type + site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR,
                           'sl_libs' = c('SL.glm', 'SL.gam')),
              'lambda' = list('type' = 'super_learner',
                              'Y' = 'bs_type',
                              'X' = c('site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR', 'baseline_a1c'),
                              'formula' = ~site + gender + race  + baseline_bmi + smoking_status + baseline_age + eGFR + baseline_a1c,
                              'sl_libs' = c('SL.glm', 'SL.gam') ### via re-parameterization to u (full propensity score)
              ),
              'mu' = list('type' = 'super_learner', 
                          'stratify' = F,
                          'Y' = 'pct_wt_change',
                          'X' = c('bs_type', 'site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR', 'baseline_a1c'),
                          'formula' = ~bs_type + site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR + baseline_a1c + 
                            bs_type:site + bs_type:gender + bs_type:race + bs_type:baseline_bmi + bs_type:smoking_status + bs_type:baseline_age + bs_type:eGFR + bs_type:baseline_a1c,
                          'sl_libs' = c('SL.polymars', 'SL.lm')),
              'epsilon' = list('type' = 'super_learner',
                               'Y' = 'eligible',
                               'X' = c('bs_type', 'site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR'),
                               'formula' = ~ bs_type + site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR,
                               'sl_libs' = c('SL.glm', 'SL.gam')),
              'nu1' = list('type' = 'super_learner',
                           'Y' = 'EY',
                           'X' = c('site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR'),
                           'formula' = ~ site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR,
                           'sl_libs' = c('SL.polymars', 'SL.lm')),
              'nu0' = list('type' = 'super_learner',
                           'Y' = 'Emu0',
                           'X' = c('site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR'),
                           'formula' = ~ site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR,
                           'sl_libs' = c('SL.polymars', 'SL.lm'))
              
         ),
       
       'IF (Fully Non-Parametric, No LM/GLM)' = 
         list('estimator' = 'influence_function',
              'description' = 'IF estimator w/ non-parametric nuisance fx estimation, removing LM/GLM candidates from SuperLearners',
              'true_models' = c('---'),
              'n_splits' = 2,
              'pi' = list('type' = 'super_learner',
                          'Y' = 'bs_type', 
                          'X' = c('site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR'),
                          'formula' = ~site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR,
                          'sl_libs' = c('SL.gam')),
              'eta' = list('type' = 'super_learner',
                           'Y' = 'R', 
                           'X' = c('bs_type', 'site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR'),
                           'formula' = ~bs_type + site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR,
                           'sl_libs' = c('SL.gam')),
              'lambda' = list('type' = 'super_learner',
                              'Y' = 'bs_type',
                              'X' = c('site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR', 'baseline_a1c'),
                              'formula' = ~site + gender + race  + baseline_bmi + smoking_status + baseline_age + eGFR + baseline_a1c,
                              'sl_libs' = c('SL.gam') ### via re-parameterization to u (full propensity score)
              ),
              'mu' = list('type' = 'super_learner', 
                          'stratify' = F,
                          'Y' = 'pct_wt_change',
                          'X' = c('bs_type', 'site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR', 'baseline_a1c'),
                          'formula' = ~bs_type + site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR + baseline_a1c,
                          'sl_libs' = c('SL.polymars')),
              'epsilon' = list('type' = 'super_learner',
                               'Y' = 'eligible',
                               'X' = c('bs_type', 'site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR'),
                               'formula' = ~ bs_type + site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR,
                               'sl_libs' = c('SL.gam')),
              'nu1' = list('type' = 'super_learner',
                           'Y' = 'EY',
                           'X' = c('site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR'),
                           'formula' = ~ site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR,
                           'sl_libs' = c('SL.polymars')),
              'nu0' = list('type' = 'super_learner',
                           'Y' = 'Emu0',
                           'X' = c('site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR'),
                           'formula' = ~ site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR,
                           'sl_libs' = c('SL.polymars'))
              
         ),
       
       
       'IF (Fully Non-Parametric, No LM/GLM, mu A = 0 Only)' = 
         list('estimator' = 'influence_function',
              'description' = 'IF estimator w/ non-parametric nuisance fx estimation stratify estimation of mu, removing LM/GLM candidates from SuperLearners',
              'true_models' = c('---'),
              'n_splits' = 2,
              'pi' = list('type' = 'super_learner',
                          'Y' = 'bs_type', 
                          'X' = c('site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR'),
                          'formula' = ~site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR,
                          'sl_libs' = c('SL.gam')),
              'eta' = list('type' = 'super_learner',
                           'Y' = 'R', 
                           'X' = c('bs_type', 'site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR'),
                           'formula' = ~bs_type + site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR,
                           'sl_libs' = c('SL.gam')),
              'lambda' = list('type' = 'super_learner',
                              'Y' = 'bs_type',
                              'X' = c('site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR', 'baseline_a1c'),
                              'formula' = ~site + gender + race  + baseline_bmi + smoking_status + baseline_age + eGFR + baseline_a1c,
                              'sl_libs' = c('SL.gam') ### via re-parameterization to u (full propensity score)
              ),
              'mu' = list('type' = 'super_learner', 
                          'stratify' = T,
                          'Y' = 'pct_wt_change',
                          'X' = c('site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR', 'baseline_a1c'),
                          'formula' = ~site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR + baseline_a1c,
                          'sl_libs' = c('SL.polymars')),
              'epsilon' = list('type' = 'super_learner',
                               'Y' = 'eligible',
                               'X' = c('bs_type', 'site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR'),
                               'formula' = ~ bs_type + site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR,
                               'sl_libs' = c('SL.gam')),
              'nu1' = list('type' = 'super_learner',
                           'Y' = 'EY',
                           'X' = c('site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR'),
                           'formula' = ~ site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR,
                           'sl_libs' = c('SL.polymars')),
              'nu0' = list('type' = 'super_learner',
                           'Y' = 'Emu0',
                           'X' = c('site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR'),
                           'formula' = ~ site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR,
                           'sl_libs' = c('SL.polymars'))
              
         ),
       
       'IF (Fully Non-Parametric, Treatment interactions in mu, No LM/GLM)' = 
         list('estimator' = 'influence_function',
              'description' = 'IF estimator w/ non-parametric nuisance fx estimation, treatment interactions in outcome model, removing LM/GLM candidates from SuperLearners',
              'true_models' = c('---'),
              'n_splits' = 2,
              'pi' = list('type' = 'super_learner',
                          'Y' = 'bs_type', 
                          'X' = c('site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR'),
                          'formula' = ~site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR,
                          'sl_libs' = c('SL.gam')),
              'eta' = list('type' = 'super_learner',
                           'Y' = 'R', 
                           'X' = c('bs_type', 'site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR'),
                           'formula' = ~bs_type + site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR,
                           'sl_libs' = c('SL.gam')),
              'lambda' = list('type' = 'super_learner',
                              'Y' = 'bs_type',
                              'X' = c('site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR', 'baseline_a1c'),
                              'formula' = ~site + gender + race  + baseline_bmi + smoking_status + baseline_age + eGFR + baseline_a1c,
                              'sl_libs' = c('SL.gam') ### via re-parameterization to u (full propensity score)
              ),
              'mu' = list('type' = 'super_learner', 
                          'stratify' = F,
                          'Y' = 'pct_wt_change',
                          'X' = c('bs_type', 'site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR', 'baseline_a1c'),
                          'formula' = ~bs_type + site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR + baseline_a1c + 
                            bs_type:site + bs_type:gender + bs_type:race + bs_type:baseline_bmi + bs_type:smoking_status + bs_type:baseline_age + bs_type:eGFR + bs_type:baseline_a1c,
                          'sl_libs' = c('SL.polymars')),
              'epsilon' = list('type' = 'super_learner',
                               'Y' = 'eligible',
                               'X' = c('bs_type', 'site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR'),
                               'formula' = ~ bs_type + site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR,
                               'sl_libs' = c('SL.gam')),
              'nu1' = list('type' = 'super_learner',
                           'Y' = 'EY',
                           'X' = c('site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR'),
                           'formula' = ~ site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR,
                           'sl_libs' = c('SL.polymars')),
              'nu0' = list('type' = 'super_learner',
                           'Y' = 'Emu0',
                           'X' = c('site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR'),
                           'formula' = ~ site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR,
                           'sl_libs' = c('SL.polymars'))
              
         ),
       
       
       ### Efficient Influence Function Estimators
       'EIF (Fully Non-Parametric)' = 
         list('estimator' = 'efficient_influence_function', 
              'Y' = 'bs_type',
              'description' = 'EIF estimator w/ non-parametric nuisance fx estimation',
              'true_models' = c('---'),
              'n_splits' = 2,
              'eta' = list('type' = 'super_learner',
                           'Y' = 'R', 
                           'X' = c('bs_type', 'site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR'),
                           'formula' = ~bs_type + site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR,
                           'sl_libs' = c('SL.glm', 'SL.gam')),
              'epsilon_star' = list('type' = 'super_learner',
                                    'Y' = 'eligible',
                                    'X' = c('bs_type', 'site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR', 'pct_wt_change'),
                                    'formula' = ~ bs_type + site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR + pct_wt_change,
                                    'sl_libs' = c('SL.glm', 'SL.gam')),
              'mu' = list('type' = 'super_learner', 
                          'stratify' = F,
                          'Y' = 'pct_wt_change',
                          'X' = c('bs_type', 'site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR', 'baseline_a1c'),
                          'formula' = ~bs_type + site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR + baseline_a1c,
                          'sl_libs' = c('SL.polymars', 'SL.lm')),
              'u' = list('type' = 'super_learner',
                         'Y' = 'bs_type',
                         'X' = c('site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR', 'baseline_a1c'),
                         'formula' = ~site + gender + race  + baseline_bmi + smoking_status + baseline_age + eGFR + baseline_a1c,
                         'sl_libs' = c('SL.glm', 'SL.gam')),
              'xi' = list('type' = 'super_learner',
                          'Y' = 'Emu0',
                          'X' = c('site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR', 'pct_wt_change'),
                          'formula' = ~ site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR  + pct_wt_change,
                          'sl_libs' = c('SL.polymars', 'SL.lm')),
              'gamma_tilde' =  list('type' = 'super_learner',
                                    'Y' = 'E_uratio',
                                    'X' = c('site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR', 'pct_wt_change'),
                                    'formula' = ~ site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR  + pct_wt_change,
                                    'sl_libs' = c('SL.polymars', 'SL.lm')),
              'chi_star_tilde' = list('type' = 'super_learner',
                                      'Y' = 'Emu0_uratio',
                                      'X' = c('site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR', 'pct_wt_change'),
                                      'formula' = ~ site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR  + pct_wt_change,
                                      'sl_libs' = c('SL.polymars', 'SL.lm'))
         ),
       
       'EIF (Fully Non-Parametric, mu A = 0 Only)' = 
         list('estimator' = 'efficient_influence_function', 
              'Y' = 'bs_type',
              'description' = 'EIF estimator w/ non-parametric nuisance fx estimation, stratify estimation of mu',
              'true_models' = c('---'),
              'n_splits' = 2,
              'eta' = list('type' = 'super_learner',
                           'Y' = 'R', 
                           'X' = c('bs_type', 'site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR'),
                           'formula' = ~bs_type + site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR,
                           'sl_libs' = c('SL.glm', 'SL.gam')),
              'epsilon_star' = list('type' = 'super_learner',
                                    'Y' = 'eligible',
                                    'X' = c('bs_type', 'site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR', 'pct_wt_change'),
                                    'formula' = ~ bs_type + site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR + pct_wt_change,
                                    'sl_libs' = c('SL.glm', 'SL.gam')),
              'mu' = list('type' = 'super_learner', 
                          'stratify' = T,
                          'Y' = 'pct_wt_change',
                          'X' = c('site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR', 'baseline_a1c'),
                          'formula' = ~site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR + baseline_a1c,
                          'sl_libs' = c('SL.polymars', 'SL.lm')),
              'u' = list('type' = 'super_learner',
                         'Y' = 'bs_type',
                         'X' = c('site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR', 'baseline_a1c'),
                         'formula' = ~site + gender + race  + baseline_bmi + smoking_status + baseline_age + eGFR + baseline_a1c,
                         'sl_libs' = c('SL.glm', 'SL.gam')),
              'xi' = list('type' = 'super_learner',
                          'Y' = 'Emu0',
                          'X' = c('site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR', 'pct_wt_change'),
                          'formula' = ~ site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR  + pct_wt_change,
                          'sl_libs' = c('SL.polymars', 'SL.lm')),
              'gamma_tilde' =  list('type' = 'super_learner',
                                    'Y' = 'E_uratio',
                                    'X' = c('site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR', 'pct_wt_change'),
                                    'formula' = ~ site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR  + pct_wt_change,
                                    'sl_libs' = c('SL.polymars', 'SL.lm')),
              'chi_star_tilde' = list('type' = 'super_learner',
                                      'Y' = 'Emu0_uratio',
                                      'X' = c('site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR', 'pct_wt_change'),
                                      'formula' = ~ site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR  + pct_wt_change,
                                      'sl_libs' = c('SL.polymars', 'SL.lm'))
         ), 
       
       'EIF (Fully Non-Parametric, Treatment interactions in mu)' = 
         list('estimator' = 'efficient_influence_function', 
              'Y' = 'bs_type',
              'description' = 'EIF estimator w/ non-parametric nuisance fx estimation, treatment interactions in outcome model',
              'true_models' = c('---'),
              'n_splits' = 2,
              'eta' = list('type' = 'super_learner',
                           'Y' = 'R', 
                           'X' = c('bs_type', 'site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR'),
                           'formula' = ~bs_type + site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR,
                           'sl_libs' = c('SL.glm', 'SL.gam')),
              'epsilon_star' = list('type' = 'super_learner',
                                    'Y' = 'eligible',
                                    'X' = c('bs_type', 'site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR', 'pct_wt_change'),
                                    'formula' = ~ bs_type + site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR + pct_wt_change,
                                    'sl_libs' = c('SL.glm', 'SL.gam')),
              'mu' = list('type' = 'super_learner', 
                          'stratify' = F,
                          'Y' = 'pct_wt_change',
                          'X' = c('bs_type', 'site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR', 'baseline_a1c'),
                          'formula' = ~bs_type + site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR + baseline_a1c + 
                            bs_type:site + bs_type:gender + bs_type:race + bs_type:baseline_bmi + bs_type:smoking_status + bs_type:baseline_age + bs_type:eGFR + bs_type:baseline_a1c,
                          'sl_libs' = c('SL.polymars', 'SL.lm')),
              'u' = list('type' = 'super_learner',
                         'Y' = 'bs_type',
                         'X' = c('site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR', 'baseline_a1c'),
                         'formula' = ~site + gender + race  + baseline_bmi + smoking_status + baseline_age + eGFR + baseline_a1c,
                         'sl_libs' = c('SL.glm', 'SL.gam')),
              'xi' = list('type' = 'super_learner',
                          'Y' = 'Emu0',
                          'X' = c('site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR', 'pct_wt_change'),
                          'formula' = ~ site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR  + pct_wt_change,
                          'sl_libs' = c('SL.polymars', 'SL.lm')),
              'gamma_tilde' =  list('type' = 'super_learner',
                                    'Y' = 'E_uratio',
                                    'X' = c('site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR', 'pct_wt_change'),
                                    'formula' = ~ site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR  + pct_wt_change,
                                    'sl_libs' = c('SL.polymars', 'SL.lm')),
              'chi_star_tilde' = list('type' = 'super_learner',
                                      'Y' = 'Emu0_uratio',
                                      'X' = c('site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR', 'pct_wt_change'),
                                      'formula' = ~ site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR  + pct_wt_change,
                                      'sl_libs' = c('SL.polymars', 'SL.lm'))
         ),
       
       'EIF (Fully Non-Parametric, No LM/GLM)' = 
         list('estimator' = 'efficient_influence_function', 
              'Y' = 'bs_type',
              'description' = 'EIF estimator w/ non-parametric nuisance fx estimation, removing LM/GLM candidates from SuperLearners',
              'true_models' = c('---'),
              'n_splits' = 2,
              'eta' = list('type' = 'super_learner',
                           'Y' = 'R', 
                           'X' = c('bs_type', 'site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR'),
                           'formula' = ~bs_type + site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR,
                           'sl_libs' = c('SL.gam')),
              'epsilon_star' = list('type' = 'super_learner',
                                    'Y' = 'eligible',
                                    'X' = c('bs_type', 'site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR', 'pct_wt_change'),
                                    'formula' = ~ bs_type + site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR + pct_wt_change,
                                    'sl_libs' = c('SL.gam')),
              'mu' = list('type' = 'super_learner', 
                          'stratify' = F,
                          'Y' = 'pct_wt_change',
                          'X' = c('bs_type', 'site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR', 'baseline_a1c'),
                          'formula' = ~bs_type + site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR + baseline_a1c,
                          'sl_libs' = c('SL.polymars')),
              'u' = list('type' = 'super_learner',
                         'Y' = 'bs_type',
                         'X' = c('site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR', 'baseline_a1c'),
                         'formula' = ~site + gender + race  + baseline_bmi + smoking_status + baseline_age + eGFR + baseline_a1c,
                         'sl_libs' = c('SL.gam')),
              'xi' = list('type' = 'super_learner',
                          'Y' = 'Emu0',
                          'X' = c('site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR', 'pct_wt_change'),
                          'formula' = ~ site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR  + pct_wt_change,
                          'sl_libs' = c('SL.polymars')),
              'gamma_tilde' =  list('type' = 'super_learner',
                                    'Y' = 'E_uratio',
                                    'X' = c('site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR', 'pct_wt_change'),
                                    'formula' = ~ site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR  + pct_wt_change,
                                    'sl_libs' = c('SL.polymars')),
              'chi_star_tilde' = list('type' = 'super_learner',
                                      'Y' = 'Emu0_uratio',
                                      'X' = c('site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR', 'pct_wt_change'),
                                      'formula' = ~ site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR  + pct_wt_change,
                                      'sl_libs' = c('SL.polymars'))
         ),
       
       'EIF (Fully Non-Parametric, mu A = 0 Only, No LM/GLM)' = 
         list('estimator' = 'efficient_influence_function', 
              'Y' = 'bs_type',
              'description' = 'EIF estimator w/ non-parametric nuisance fx estimation, stratify estimation of mu, removing LM/GLM candidates from SuperLearners',
              'true_models' = c('---'),
              'n_splits' = 2,
              'eta' = list('type' = 'super_learner',
                           'Y' = 'R', 
                           'X' = c('bs_type', 'site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR'),
                           'formula' = ~bs_type + site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR,
                           'sl_libs' = c('SL.gam')),
              'epsilon_star' = list('type' = 'super_learner',
                                    'Y' = 'eligible',
                                    'X' = c('bs_type', 'site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR', 'pct_wt_change'),
                                    'formula' = ~ bs_type + site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR + pct_wt_change,
                                    'sl_libs' = c('SL.gam')),
              'mu' = list('type' = 'super_learner', 
                          'stratify' = T,
                          'Y' = 'pct_wt_change',
                          'X' = c('site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR', 'baseline_a1c'),
                          'formula' = ~site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR + baseline_a1c,
                          'sl_libs' = c('SL.polymars')),
              'u' = list('type' = 'super_learner',
                         'Y' = 'bs_type',
                         'X' = c('site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR', 'baseline_a1c'),
                         'formula' = ~site + gender + race  + baseline_bmi + smoking_status + baseline_age + eGFR + baseline_a1c,
                         'sl_libs' = c('SL.gam')),
              'xi' = list('type' = 'super_learner',
                          'Y' = 'Emu0',
                          'X' = c('site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR', 'pct_wt_change'),
                          'formula' = ~ site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR  + pct_wt_change,
                          'sl_libs' = c('SL.polymars')),
              'gamma_tilde' =  list('type' = 'super_learner',
                                    'Y' = 'E_uratio',
                                    'X' = c('site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR', 'pct_wt_change'),
                                    'formula' = ~ site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR  + pct_wt_change,
                                    'sl_libs' = c('SL.polymars')),
              'chi_star_tilde' = list('type' = 'super_learner',
                                      'Y' = 'Emu0_uratio',
                                      'X' = c('site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR', 'pct_wt_change'),
                                      'formula' = ~ site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR  + pct_wt_change,
                                      'sl_libs' = c('SL.polymars'))
         ), 
       
       'EIF (Fully Non-Parametric, Treatment interactions in mu, No LM/GLM)' = 
         list('estimator' = 'efficient_influence_function', 
              'Y' = 'bs_type',
              'description' = 'EIF estimator w/ non-parametric nuisance fx estimation, treatment interactions in outcome model, removing LM/GLM candidates from SuperLearners',
              'true_models' = c('---'),
              'n_splits' = 2,
              'eta' = list('type' = 'super_learner',
                           'Y' = 'R', 
                           'X' = c('bs_type', 'site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR'),
                           'formula' = ~bs_type + site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR,
                           'sl_libs' = c('SL.gam')),
              'epsilon_star' = list('type' = 'super_learner',
                                    'Y' = 'eligible',
                                    'X' = c('bs_type', 'site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR', 'pct_wt_change'),
                                    'formula' = ~ bs_type + site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR + pct_wt_change,
                                    'sl_libs' = c('SL.gam')),
              'mu' = list('type' = 'super_learner', 
                          'stratify' = F,
                          'Y' = 'pct_wt_change',
                          'X' = c('bs_type', 'site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR', 'baseline_a1c'),
                          'formula' = ~bs_type + site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR + baseline_a1c + 
                            bs_type:site + bs_type:gender + bs_type:race + bs_type:baseline_bmi + bs_type:smoking_status + bs_type:baseline_age + bs_type:eGFR + bs_type:baseline_a1c,
                          'sl_libs' = c('SL.polymars')),
              'u' = list('type' = 'super_learner',
                         'Y' = 'bs_type',
                         'X' = c('site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR', 'baseline_a1c'),
                         'formula' = ~site + gender + race  + baseline_bmi + smoking_status + baseline_age + eGFR + baseline_a1c,
                         'sl_libs' = c('SL.gam')),
              'xi' = list('type' = 'super_learner',
                          'Y' = 'Emu0',
                          'X' = c('site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR', 'pct_wt_change'),
                          'formula' = ~ site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR  + pct_wt_change,
                          'sl_libs' = c('SL.polymars')),
              'gamma_tilde' =  list('type' = 'super_learner',
                                    'Y' = 'E_uratio',
                                    'X' = c('site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR', 'pct_wt_change'),
                                    'formula' = ~ site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR  + pct_wt_change,
                                    'sl_libs' = c('SL.polymars')),
              'chi_star_tilde' = list('type' = 'super_learner',
                                      'Y' = 'Emu0_uratio',
                                      'X' = c('site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR', 'pct_wt_change'),
                                      'formula' = ~ site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR  + pct_wt_change,
                                      'sl_libs' = c('SL.polymars'))
         ),
       
       'EIF (Fully Non-Parametric excpect Outcome)' = 
         list('estimator' = 'efficient_influence_function', 
              'Y' = 'bs_type',
              'description' = 'EIF estimator w/ non-parametric nuisance fx estimation (mu parametric)',
              'true_models' = c('mu'),
              'n_splits' = 2,
              'eta' = list('type' = 'super_learner',
                           'Y' = 'R', 
                           'X' = c('bs_type', 'site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR'),
                           'formula' = ~bs_type + site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR,
                           'sl_libs' = c('SL.glm', 'SL.gam')),
              'epsilon_star' = list('type' = 'super_learner',
                                    'Y' = 'eligible',
                                    'X' = c('bs_type', 'site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR', 'pct_wt_change'),
                                    'formula' = ~ bs_type + site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR + pct_wt_change,
                                    'sl_libs' = c('SL.glm', 'SL.gam')),
              'mu' = list('type' = 'LM', 
                          'stratify' = F,
                          'formula' = pct_wt_change ~ bs_type + site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR + baseline_a1c + 
                            bs_type:baseline_a1c +  baseline_a1c:gender + gender:baseline_bmi + bs_type:smoking_status + site:bs_type),
              'u' = list('type' = 'super_learner',
                         'Y' = 'bs_type',
                         'X' = c('site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR', 'baseline_a1c'),
                         'formula' = ~site + gender + race  + baseline_bmi + smoking_status + baseline_age + eGFR + baseline_a1c,
                         'sl_libs' = c('SL.glm', 'SL.gam')),
              'xi' = list('type' = 'super_learner',
                          'Y' = 'Emu0',
                          'X' = c('site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR', 'pct_wt_change'),
                          'formula' = ~ site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR  + pct_wt_change,
                          'sl_libs' = c('SL.polymars', 'SL.lm')),
              'gamma_tilde' =  list('type' = 'super_learner',
                                    'Y' = 'E_uratio',
                                    'X' = c('site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR', 'pct_wt_change'),
                                    'formula' = ~ site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR  + pct_wt_change,
                                    'sl_libs' = c('SL.polymars', 'SL.lm')),
              'chi_star_tilde' = list('type' = 'super_learner',
                                      'Y' = 'Emu0_uratio',
                                      'X' = c('site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR', 'pct_wt_change'),
                                      'formula' = ~ site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR  + pct_wt_change,
                                      'sl_libs' = c('SL.polymars', 'SL.lm'))
         ),
       
       
       ### Flexible IWOR
       'Inverse Weighted Outcome Regression (Flexible)' = 
         list('estimator' = 'crossfit_iwor_cc', 
              'Y' = 'bs_type',
              'description' = 'Inverse Weighted Outcome Regression for ATT among eligible complete cases (Flexible mu/eta)',
              'true_models' = c('---'),
              'n_splits' = 2,
              'eta' = list('type' = 'super_learner',
                           'Y' = 'R', 
                           'X' = c('bs_type', 'site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR'),
                           'formula' = ~bs_type + site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR,
                           'sl_libs' = c('SL.glm', 'SL.gam')),
              'mu' = list('type' = 'super_learner', 
                          'stratify' = F,
                          'Y' = 'pct_wt_change',
                          'X' = c('bs_type', 'site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR', 'baseline_a1c'),
                          'formula' = ~bs_type + site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR + baseline_a1c,
                          'sl_libs' = c('SL.polymars', 'SL.lm'))
         ),
       
       'Inverse Weighted Outcome Regression (Flexible, mu A = 0 Only)' = 
         list('estimator' = 'crossfit_iwor_cc', 
              'Y' = 'bs_type',
              'description' = 'Inverse Weighted Outcome Regression w/ Flexible mu/eta, stratify estimation of mu',
              'true_models' = c('---'),
              'n_splits' = 2,
              'eta' = list('type' = 'super_learner',
                           'Y' = 'R', 
                           'X' = c('bs_type', 'site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR'),
                           'formula' = ~bs_type + site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR,
                           'sl_libs' = c('SL.glm', 'SL.gam')),
              'mu' = list('type' = 'super_learner', 
                          'stratify' = T,
                          'Y' = 'pct_wt_change',
                          'X' = c('site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR', 'baseline_a1c'),
                          'formula' = ~site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR + baseline_a1c,
                          'sl_libs' = c('SL.polymars', 'SL.lm'))
         ),
       
       'Inverse Weighted Outcome Regression (Flexible, Treatment interactions in mu)' = 
         list('estimator' = 'crossfit_iwor_cc', 
              'Y' = 'bs_type',
              'description' = 'Inverse Weighted Outcome Regression w/ Flexible mu/eta, treatment interactions in outcome model',
              'true_models' = c('---'),
              'n_splits' = 2,
              'eta' = list('type' = 'super_learner',
                           'Y' = 'R', 
                           'X' = c('bs_type', 'site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR'),
                           'formula' = ~bs_type + site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR,
                           'sl_libs' = c('SL.glm', 'SL.gam')),
              'mu' = list('type' = 'super_learner', 
                          'stratify' = F,
                          'Y' = 'pct_wt_change',
                          'X' = c('bs_type', 'site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR', 'baseline_a1c'),
                          'formula' = ~bs_type + site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR + baseline_a1c + 
                            bs_type:site + bs_type:gender + bs_type:race + bs_type:baseline_bmi + bs_type:smoking_status + bs_type:baseline_age + bs_type:eGFR + bs_type:baseline_a1c,
                          'sl_libs' = c('SL.polymars', 'SL.lm'))
         ),
       
       'Inverse Weighted Outcome Regression (Flexible, No LM/GLM)' = 
         list('estimator' = 'crossfit_iwor_cc', 
              'Y' = 'bs_type',
              'description' = 'Inverse Weighted Outcome Regression for ATT among eligible complete cases (Flexible mu/eta), removing LM/GLM candidates from SuperLearners',
              'true_models' = c('---'),
              'n_splits' = 2,
              'eta' = list('type' = 'super_learner',
                           'Y' = 'R', 
                           'X' = c('bs_type', 'site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR'),
                           'formula' = ~bs_type + site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR,
                           'sl_libs' = c('SL.gam')),
              'mu' = list('type' = 'super_learner', 
                          'stratify' = F,
                          'Y' = 'pct_wt_change',
                          'X' = c('bs_type', 'site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR', 'baseline_a1c'),
                          'formula' = ~bs_type + site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR + baseline_a1c,
                          'sl_libs' = c('SL.polymars'))
         ),
       
       'Inverse Weighted Outcome Regression (Flexible, mu A = 0 Only, No LM/GLM)' = 
         list('estimator' = 'crossfit_iwor_cc', 
              'Y' = 'bs_type',
              'description' = 'Inverse Weighted Outcome Regression w/ Flexible mu/eta, stratify estimation of mu, removing LM/GLM candidates from SuperLearners',
              'true_models' = c('---'),
              'n_splits' = 2,
              'eta' = list('type' = 'super_learner',
                           'Y' = 'R', 
                           'X' = c('bs_type', 'site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR'),
                           'formula' = ~bs_type + site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR,
                           'sl_libs' = c('SL.gam')),
              'mu' = list('type' = 'super_learner', 
                          'stratify' = T,
                          'Y' = 'pct_wt_change',
                          'X' = c('site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR', 'baseline_a1c'),
                          'formula' = ~site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR + baseline_a1c,
                          'sl_libs' = c('SL.polymars'))
         ),
       
       'Inverse Weighted Outcome Regression (Flexible, Treatment interactions in mu, No LM/GLM)' = 
         list('estimator' = 'crossfit_iwor_cc', 
              'Y' = 'bs_type',
              'description' = 'Inverse Weighted Outcome Regression w/ Flexible mu/eta, treatment interactions in outcome model, removing LM/GLM candidates from SuperLearners',
              'true_models' = c('---'),
              'n_splits' = 2,
              'eta' = list('type' = 'super_learner',
                           'Y' = 'R', 
                           'X' = c('bs_type', 'site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR'),
                           'formula' = ~bs_type + site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR,
                           'sl_libs' = c('SL.gam')),
              'mu' = list('type' = 'super_learner', 
                          'stratify' = F,
                          'Y' = 'pct_wt_change',
                          'X' = c('bs_type', 'site', 'gender', 'race', 'baseline_bmi', 'smoking_status', 'baseline_age', 'eGFR', 'baseline_a1c'),
                          'formula' = ~bs_type + site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR + baseline_a1c + 
                            bs_type:site + bs_type:gender + bs_type:race + bs_type:baseline_bmi + bs_type:smoking_status + bs_type:baseline_age + bs_type:eGFR + bs_type:baseline_a1c,
                          'sl_libs' = c('SL.polymars'))
         ),
       
       ### Naive CC Estimator but always implement Eligibility Restrictive Matching
       'CC Outcome Regression (Elig. Restrictive Matching)' =
         list('estimator' = 'cc_outcome',
              'true_models' = c('mu'),
              'description' = 'Standard Outcome Regression for ATT among eligible complete cases (Eligibility Restrictive Matching)',
              'mu' = list('type' = 'LM', 
                          'formula' = pct_wt_change ~ bs_type + site + gender + race + baseline_bmi + smoking_status + baseline_age + eGFR + baseline_a1c + 
                            bs_type:baseline_a1c +  baseline_a1c:gender + gender:baseline_bmi + bs_type:smoking_status + site:bs_type)
         )
       
  )

write_rds(estimators, 'data/simulations/aligned_t0/inputs/estimators.rds')


