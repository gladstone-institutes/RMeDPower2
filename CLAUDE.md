# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

RMeDPower2 is an R package for statistical power analysis in repeated measures experiments. It provides complete functionality to analyze data from hierarchical or crossed experimental designs, test modeling assumptions, identify outliers, estimate statistical power, and perform sample size calculations.

## Development Commands

This is an R package developed using standard R package development workflow:

```bash
# Install dependencies
R -e "install.packages(c('devtools', 'roxygen2'))"

# Install from GitHub (for testing)
R -e "devtools::install_github('gladstone-institutes/RMeDPower2', build_vignettes=TRUE)"

# Build and check package
R CMD build .
R CMD check RMeDPower2_*.tar.gz

# Generate documentation
R -e "roxygen2::roxygenise()"

# Run examples and tests
R -e "devtools::run_examples()"
```

## Architecture

### Core Classes (R/main_classes.R)

The package uses S4 classes to structure experimental designs and parameters:

- **RMeDesign**: Defines experimental design including response variables, conditions, experimental hierarchy, covariates, and interaction terms
- **ProbabilityModel**: Specifies error distribution assumptions (normal vs non-normal, distribution families)
- **PowerParams**: Contains parameters for power analysis including target variables, effect sizes, simulation parameters

### Key Functions

**Power Analysis Pipeline:**
1. `get_model_and_data()` - Fits linear mixed effects models using lme4
2. `calculate_power()` - Core power simulation using simr package
3. `calculatePower()` - Main user interface wrapping the pipeline

**Data Validation and Preprocessing:**
1. `diagnoseDataModel()` - Tests modeling assumptions and identifies outliers
2. `transform_data()` - Handles data transformations and outlier removal
3. `check_normality()` - Validates normality assumptions with QQ plots

**Model Building Helpers:**
- `build_fixed_formula()` - Constructs fixed effects formulas
- `build_random_formula()` - Constructs random effects formulas
- `get_residuals()` - Extracts model residuals for diagnostics

### Dependencies

**Core Analysis:**
- lme4: Linear mixed effects modeling
- simr: Power analysis simulation
- lmerTest: Extended lmer functionality

**Data Manipulation:**
- dplyr, tidyr, magrittr: Data processing
- ggplot2: Visualization

**Diagnostics:**
- DHARMa: Model assumption testing for non-normal data
- influence.ME: Outlier detection at group level
- EnvStats: Rosner's test for outlier observations

### Input Templates

JSON templates in `input_templates/` provide structure for:
- `design_template.json`: RMeDesign class parameters
- `power_param_template.json`: PowerParams class parameters  
- `stat_model_template.json`: ProbabilityModel class parameters

### Data and Documentation

- `data/`: Example datasets
  - `plate_assay_pilot_data`: Pilot data from plate assays
  - `plate_assay_pilot_data_wo_repeats`: Plate assay pilot data without repeats
  - `plate_assay_full_data`: Full plate assay data
  - `snRNAseq_cluster_count_data`: Single-nucleus RNA-seq cluster count data
  - `snRNAseq_gene_count_data`: Single-nucleus RNA-seq gene count data
  - `mouse_behavior_MWM_assay_data`: Mouse behavior Morris Water Maze assay data
  - `mouse_brain_electro_physiology_data`: Mouse brain electrophysiology data
- `man/`: Auto-generated R documentation files
- `RMeDPower_vignette.Rmd`: Comprehensive usage guide with biomedical examples

## Key Design Patterns

**Hierarchical Experimental Design Support:**
- Handles nested (e.g., cells within plates within experiments) and crossed designs
- `experimental_columns` define hierarchy order
- `crossed_columns` identify variables that can repeat across hierarchy levels

**Flexible Model Specification:**
- Supports both categorical and continuous conditions
- Optional covariate inclusion with interaction terms
- Random slopes for specified variables
- Multiple error distributions (normal, binomial, Poisson)

**Outlier Detection:**
- Two-level approach: observation-level (Rosner's test) and group-level (Cook's distance)
- Handles both raw and log-transformed data
- Visual diagnostics through QQ plots

## Usage Notes

- The package focuses on testing one main predictor with optional covariate
- Power curves test varying sample sizes or hierarchy levels
- Simulation-based approach allows realistic power estimates for complex designs
- Template-based configuration promotes reproducible analyses