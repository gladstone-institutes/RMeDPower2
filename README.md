# RMeDPower
![image](https://user-images.githubusercontent.com/18338399/186777298-189fb773-d89b-4557-85c4-b22546f566e5.png)

## Description
Biomedical research very often involves data generated from repeated measures experiments. RMeDPower2 is an R package that provides complete functionality to analyse data coming from repeated measures experiments, i.e., where one has repeated measures from the same biological/independent units or samples.

RMeDPower2 helps test the modeling assumptions one makes, identify outlier observations, outlier units at different levels of the design, estimates statistical power or perform sample size calculations, estimate parameters of interest and also to visualize the association being tested. The functionality is limited to testing associations of one predictor (continuous or categorical, e.g., disease status or brain pathology) along with one another covariate (e.g., gender status) in the context of hierarchical or crossed experimental designs.

RMeDPower2 defines the experimental design for the data, probability model of the data generating distribution and necessary parameters required for sample size calculation using convenient S4 class objects. It uses these objects in one framework that brings together the functionality implemented in multiple R packages - lme4 (implementation of linear mixed effects models), influence.ME (identification of outlier units), EnvStats EnvStats (identification of outlier observations), DHARMa (testing of modeling assumptions for non-normal distributions), simr (sample-size calculations) and tidyverse (data manipulation and visualization).

## How to install
```
install.packages("devtools")
library(devtools)
install_github('gladstone-institutes/RMeDPower2', build_vignettes=TRUE)
library(RMeDPower2)
```

## Website
Please visit https://gladstone-institutes.github.io/RMeDPower2/index.html for a detailed description of the package and tutorials.

## AI Disclosure Statement

Generative AI tools (Claude Code, Anthropic) were used as coding assistants during the development of this package. The authors maintain full responsibility for the accuracy, reproducibility, and scientific validity of all code. AI-assisted outputs were reviewed and validated against expected behavior before integration. The research questions, analytical approaches, parameter selections, and scientific interpretations were determined independently by the authors without AI input.
