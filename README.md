# RMeDPower
![image](https://user-images.githubusercontent.com/18338399/186777298-189fb773-d89b-4557-85c4-b22546f566e5.png)

## Description
RMeDPower2 Provides complete functionality to analyse data from repeated
    measures experiments with hierarchical or crossed experimental designs.
    Supports testing modeling assumptions, identifying outlier observations
    and experimental units, estimating statistical power, and performing
    sample size calculations. Uses linear mixed effects models via 'lme4'
    and simulation-based power analysis via 'simr'. Handles both normal and
    non-normal error distributions including binomial and Poisson families.

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
