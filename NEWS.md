# RMeDPower2 1.0.0

* Initial CRAN submission.
* Provides power analysis and sample size calculation for repeated measures experiments with hierarchical or crossed designs.
* Supports linear mixed effects models via `lme4` and simulation-based power analysis via `simr`.
* Handles normal and non-normal error distributions (binomial, Poisson, negative binomial).
* Includes diagnostic tools for model assumption testing and outlier detection.

# RMeDPower2 1.0.1

* Revised CRAN submission.
* Added references to the DESCRIPTION file
* Replace print statements with error statements where appropriate and otherwise with message statements
* Removed options(warn=-1) in the identified scripts

# RMeDPower2 1.0.2

* Revised CRAN submission.
* Added simulateREs = "user-specified" argument to the DHARMa::simulateResiduals() function to make it compatible with the updates to DHARMa
* Added model fit to the output of getEstimatesOfInterest

# RMeDPower2 1.0.3

* Updated plate_assay_pilot_data and plate_assay_full_data data sets to simplify illustration of package details.
