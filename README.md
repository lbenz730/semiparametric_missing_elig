# Robust Causal Inference for Point Exposures with Missing Eligibility Criteria


Benz, L., Mukherjee, R., Wang, R., Arterburn, D., Fischer, H., Lee, C., Shortreed, S.M., Haneuse, S., and Levis, A.W. "Robust Causal Inference for EHR-based Studies of Point Exposures with Missingness in Eligibility Criteria" _Under Review_ ([Pre-Print](https://arxiv.org/abs/2504.16230))

---

## Artifact Map

The table below maps each figure and table in the manuscript to the script(s) that generate it and its output location. More detail on each script is available below.

**Note:** The underlying EHR data cannot be shared due to data use agreements with Kaiser Permanente. Scripts in `analysis/` and `data/` cannot be run locally. For a fully reproducible example, see the [Worked OMOP Example](worked_omop_example/).

---

### Main Figures

| Artifact | Script | Location |
|---|---|---|
| Figure 1 — Frequency of measurements for select surgical patients | [`scripts/analysis/aligned_t0/diabetes_figure.R`](scripts/analysis/aligned_t0/diabetes_figure.R) | [`figures/diabetes_elig.pdf`](figures/diabetes_elig.pdf) |
| Figure 2 — Eligibility distributions | [`scripts/analysis/aligned_t0/elig_figures.R`](scripts/analysis/aligned_t0/elig_figures.R) | [`figures/elig_dist.pdf`](figures/elig_dist.pdf) |
| Figure 3 — Distribution of time between surgery and most recent BMI/A1c measure | [`scripts/analysis/aligned_t0/measure_times_figure.R`](scripts/analysis/aligned_t0/measure_times_figure.R) | [`figures/measure_times.pdf`](figures/measure_times.pdf) |
| Figure 4 — Distributions of nuisance functions | [`scripts/analysis/aligned_t0/plot_nuisances.R`](scripts/analysis/aligned_t0/plot_nuisances.R) | [`figures/nuisance_dist.pdf`](figures/nuisance_dist.pdf) |
| Figure 5 — Point estimates and 95% confidence intervals | [`scripts/analysis/aligned_t0/plot_results.R`](scripts/analysis/aligned_t0/plot_results.R) | [`figures/results_figure.pdf`](figures/results_figure.pdf) |

---

### Supplementary Figures

| Artifact | Script | Location |
|---|---|---|
| Figure S1 — μ₀ calibration: parametric vs. non-parametric (SuperLearner) | [`simulations/aligned_t0/pnp.R`](simulations/aligned_t0/pnp.R) | [`figures/mu0_calibration.pdf`](figures/mu0_calibration.pdf) |
| Figure S2 — Eligibility distributions (supplementary) | [`scripts/analysis/aligned_t0/elig_figures.R`](scripts/analysis/aligned_t0/elig_figures.R) | [`figures/elig_dist_supp.pdf`](figures/elig_dist_supp.pdf) |
| Figure S3 — Nuisance function distribution for T2DM Remission | [`scripts/analysis/aligned_t0/plot_nuisances.R`](scripts/analysis/aligned_t0/plot_nuisances.R) | [`figures/nuisance_dist_remission.pdf`](figures/nuisance_dist_remission.pdf) |
| Figure S4 — Nuisance function distribution nested nuisance functions | [`scripts/analysis/aligned_t0/plot_nuisances.R`](scripts/analysis/aligned_t0/plot_nuisances.R) | [`figures/nuisance_nested.pdf`](figures/nuisance_nested.pdf) |
| Figure S5 — MAR sensitivity analysis (Assumption 4) | [`scripts/analysis/aligned_t0/MAR_sensitivity_analysis.R`](scripts/analysis/aligned_t0/MAR_sensitivity_analysis.R) | [`figures/weight_sensitivity_0.pdf`](figures/weight_sensitivity_0.pdf) |
| Figure S6 — MAR sensitivity analysis (Assumption 4) | [`scripts/analysis/aligned_t0/MAR_sensitivity_analysis.R`](scripts/analysis/aligned_t0/MAR_sensitivity_analysis.R) | [`figures/weight_sensitivity_12.pdf`](figures/weight_sensitivity_12.pdf) |
| Figure S7 — MAR sensitivity analysis (Assumption 4) | [`scripts/analysis/aligned_t0/MAR_sensitivity_analysis.R`](scripts/analysis/aligned_t0/MAR_sensitivity_analysis.R) | [`figures/t2dm_sensitivity_0.pdf`](figures/t2dm_sensitivity_0.pdf) |
| Figure S8 — MAR sensitivity analysis (Assumption 4) | [`scripts/analysis/aligned_t0/MAR_sensitivity_analysis.R`](scripts/analysis/aligned_t0/MAR_sensitivity_analysis.R) | [`figures/t2dm_sensitivity_12.pdf`](figures/t2dm_sensitivity_12.pdf) |

---

### Tables

| Artifact | Script | Location |
|---|---|---|
| Table S1 — Simulation results summary | [`simulations/aligned_t0/latex_tables.R`](simulations/aligned_t0/latex_tables.R) | [`tables/sim_results_main.tex`](tables/sim_results_main.tex) |
| Table S2 — Simulation parameter values | [`simulations/aligned_t0/inform_sims.R`](simulations/aligned_t0/inform_sims.R), [`simulations/aligned_t0/latex_tables.R`](simulations/aligned_t0/latex_tables.R) | [`tables/sim_coeff.tex`](tables/sim_coeff.tex) |

---

### Analysis Scripts by Estimator

The scripts below implement each estimator for both outcomes in the data application. They do not produce standalone output files directly but feed into the plotting scripts listed above.

| Estimator | Weight Change Script | T2DM Remission Script |
|---|---|---|
| Naive CC ($\hat\theta_\text{CC}$) | [`analysis/aligned_t0/fit_CC_outcome_regression_estimator.R`](analysis/aligned_t0/fit_CC_outcome_regression_estimator.R) | [`analysis/aligned_t0/fit_CC_outcome_remission.R`](analysis/aligned_t0/fit_CC_outcome_remission.R) |
| IWOR ($\hat\theta_\text{IWOR}$) | [`analysis/aligned_t0/fit_iwor_estimator.R`](analysis/aligned_t0/fit_iwor_estimator.R) | [`analysis/aligned_t0/fit_iwor_estimator_remission.R`](analysis/aligned_t0/fit_iwor_estimator_remission.R) |
| IF ($\hat\theta_\text{IF}$) | [`analysis/aligned_t0/fit_IF_estimator.R`](analysis/aligned_t0/fit_IF_estimator.R) | [`analysis/aligned_t0/fit_IF_estimator_remission.R`](analysis/aligned_t0/fit_IF_estimator_remission.R) |
| EIF ($\hat\theta_\text{EIF}$) | [`analysis/aligned_t0/fit_EIF_estimator.R`](analysis/aligned_t0/fit_EIF_estimator.R) | [`analysis/aligned_t0/fit_EIF_estimator_remission.R`](analysis/aligned_t0/fit_EIF_estimator_remission.R) |

---

### Worked OMOP Example

| Script | Description | Output |
|---|---|---|
| [`worked_omop_example/build_omop_example.R`](worked_omop_example/build_omop_example.R) | Creates synthetic EHR dataset via `omock` and prepares it for analysis | [`worked_omop_example/analysis_dataset.csv`](worked_omop_example/analysis_dataset.csv) |
| [`worked_omop_example/EIF_omop_example.R`](worked_omop_example/EIF_omop_example.R) | Applies `eif_estimator` to the prepared synthetic dataset | Console output / user-defined |

---

## R Scripts (`scripts/`)
* __helpers.R__: File of helper functions 

### Data (`data/`)
Folder of scripts used to clean and process EHR data for use in data application
* __rygb_vsg_data_prep.R__: This script is used to prep analysis dataset(s) for data application presented in the paper. It calls several raw EHR files which are not directly sharable due to data use agreements with Kaiser Permanente. Nevertheless, this script is commented with specific details on how the underlying cohort was created including application of the eligibility criteria to the entire cohort across all 40 operationalizations.
* __surgical_px_cleaning.R__ Clean some chart review for surgical procedure types and correct a few cases that were incorrectly tagged in the original EHR files.

## Analysis (`analysis/aligned_t0`)
Folder of scripts used for data application analysis. For each of the two outcomes examined, there is one script that fits each of the four estimators explored in this work. When used, specific functions in each script are commented with descriptions of input and output. Given that the underlying EHR data can not be shared, this example can not be reproduced locally. For a detailed reproducible example, please refer to the [OMOP Worked Example]

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


__Figures__
* __diabetes_figure.R__: Plot of diabetes figure showing frequency of certain measurements for select surgical patients __(Generates Figure 1)__
* __elig_figures.R__: Plot eligibility distributions __(Generates Figure 2 and S2)__
* __measure_times_figure.R__: Plots distribution of time between date of surgery and most recent measure of BMI/A1c __(Generates Figure 3)__
* __plot_nuisances.R__: Plotting code for distributions of nuisance functions __(Generates Figures 4, S3, and S4)__
* __plot_results.R__: Plot point estimates and 95% confidence intervals __(Generates Figure 4)__
* __MAR_sensitivity_analysis.R__: Conducts sensitivity analysis for Assumption 4 (Eligibility MAR) and generates Figures S5-S8.

## Worked OMOP Example (`worked_omop_example`)
Given that the underlying EHR data can not be shared, the example presented in the main text can not be reproduced locally. For a detailed reproducible example, we have created a working example based on OMOP-CDM formated data. In particular, we use the `omock` package to create a synthetic dataset based on OMOP standards which can be shared. We then demonstrate how to turn this data into an analytical dataset to be analyzed by $\widehat\theta_\text{EIF}$, and analyze the sythetic dataset.

* __build_omop_example.R__: This script creates a synthetic EHR dataset using the `omock` package. It then illustrates how to clean this example dataset and prepare the dataset for analysis by our EIF-based estimator, $\widehat\theta_\text{EIF}$. The final output of this script is the dataset `scripts/worked_omop_example/analysis_dataset.csv`.
* __EIF_omop_example.R__: This script contains a documented function `eif_estimator` which implements $\widehat\theta_\text{EIF}$. The script loads in the prepared dataset `scripts/worked_omop_example/analysis_dataset.csv` and applies `eif_estimator` to that worked data example. 

### Simulations (`simulations/aligned_t0`)
Folder of scripts for a setting where time zero ($t_0$) is aligned for all subjects so we only need to consider eligibility, missingness, etc. at a single time per subject, and matching is not needed (insofar as it is a mechanism for establishing time zero). Contains an implementation of $\widehat\theta_\text{EIF}, \widehat\theta_\text{IF}$ and $\widetilde\theta_\text{IF}$, $\widehat\theta_\text{CC}$ and $\widehat\theta_\text{IWOR}$

* __estimators.R__: Script contains functions which implement each of the four estimators explored in this work, in the context of the simulation study. Specific parameters used in simulations and estimator instructions are downloadable in `simulations/aligned_t0/inputs`.
* __generate_data.R__: Script contains function `generate_data` to generate simulated datasets given a list of instructions.
* __inform_sims.R__: Exploratory analysis to guide range of models for consideration in simulated datasets. This script is how the coefficient values used in generating simulated datasets (those in Table S2) were chosen.
* __run_simulation.R__: This script calls all functions to generate and analyze simulated datasets. In other words, this is the main simulation wrapper which controls the simulations.
* __pnp.R__: Script to examine how well $\mu_0$ is calibrated in single simulated dataset between correctly specified parametric model and 6 non-parametric models based on `SuperLearner` __(Generates Figure S1)__
* __specify_inputs.R__: Script which specifies simulation parameters and estimators for consideration. This script is specifically used to generate `params` used by `generate_data` function in __generate_data.R__.
* __latex_tables.R__: Generates all tables for describing simulation results (Table S1) and parameters (Table S2).


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
