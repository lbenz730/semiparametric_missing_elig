# Robust Causal Inference for Point Exposures with Missing Eligibility Criteria


Benz, L., Mukherjee, R., Wang, R., Arterburn, D., Fischer, H., Lee, C., Shortreed, S.M., Haneuse, S., and Levis, A.W. "Robust Causal Inference for EHR-based Studies of Point Exposures with Missingness in Eligibility Criteria" _Under Review_ (Pre-Print)[Link Later]


## R Scripts (`scripts/`)
* __helpers.R__: File of helper functions 

### Simulations (`simulations/aligned_t0`)
Folder of scripts for a setting where time zero ($t_0$) is aligned for all subjects so we only need to consider eligibility, missingness, etc. at a single time per subject, and matching is not needed (insofar as it is a mechanism for establishing time zero). Contains an implementation of $\widehat\theta_\text{EIF}, \widehat\theta_\text{IF}$ and $\widetilde\theta_\text{IF}$, $\widehat\theta_\text{CC}$ and $\widehat\theta_\text{IWOR}$

* __estimators.R__: Implementation of estimators
* __generate_data.R__: Functions to generate simulated data
* __inform_sims.R__: Exploratory analysis to guide range of models for consideration in simulated datasets
* __run_simulation.R__: Main simulation wrapper.
* __pnp.R__: $\mu_0$ calibration between parametric and non-parametric models __(Generates Figure S1)__
* __specify_inputs.R__: Script which specifies simulation parameters and estimators for consideration
* __latex_tables.R__: Generates all tables for describing simulation results and parameters.

### Data (`data/`)
Folder of scripts used to clean and process EHR data for use in data application
* __rygb_vsg_data_prep.R__: Prep analysis dataset(s) for data application of the aligned time zero case (RYGB vs. VSG).
* __surgical_px_cleaning.R__ Clean some chart review for surgical procedure types

## Analysis (`analysis/aligned_t0`)
Folder of scripts used for data application analysis

* __diabetes_figure.R__: Plot of diabetes figure showing frequency of certain measurements for select surgical patients __(Generates Figure 1)__
* __elig_figures.R__: Plot eligibility distributions __(Generates Figure 2 and S2)__
* __plot_nuisances.R__: Plotting code for distributions of nuisance functions __(Generates Figures 3, S3, and S4)__
* __plot_results.R__: Plot point estimates and 95% confidence intervals __(Generates Figure 4)__

__Weight Change Analysis__

* __fit_CC_outcome_regression_estimator.R__: Naive ATT analysis ( $\hat\theta_\text{CC}$) for weight change outcome 
* __fit_iwor_estimator.R__: IWOR ATT analysis (with $\hat\theta_\text{IWOR}$) for weight change outcome  
* __fit_IF_estimator.R__: IF ATT analysis (with $\hat\theta_\text{IF}$) for weight change outcome  
* __fit_EIF_estimator.R__: EIF ATT analysis (with $\hat\theta_\text{EIF}$) for weight change outcome

__T2DM Remission Analysis__

* __fit_CC_outcome_remission__: Naive ATT analysis ( $\hat\theta_\text{CC}$) for diabetes remission outcome 
* __fit_iwor_estimato_remission.R__: IWOR ATT analysis (with $\hat\theta_\text{IWOR}$) for diabetes remission outcome  
* __fit_IF_estimator_remission.R__: IF ATT analysis (with $\hat\theta_\text{IF}$) for diabetes remission outcome
* __fit_EIF_estimator_remission.R__: EIF ATT analysis (with $\hat\theta_\text{EIF}$) for diabetes remission outcome



## Data (`data/`)
* Simulation inputs + results
* Data application results 

## Figures (`figures/`)
Figures saved out from various analyses

## Figures (`tables/`)
Tables saved out from various analyses

## Jobs (`jobs/`)
.sh files for batch jobs on the cluster 

* __aligned_t0_sims_loop.sh__: SBATCH job file for running simulations for comnination of estimator/simulation parameters
* __run_aligned_t0_loop.sh__: Wrapper for fully 2-D job array for __aligned_t0_sims_loop.sh__ .
* __data_application.sh__: Wrapper for submitting all the jobs for the data application, in the __application/__ sub-directory.
