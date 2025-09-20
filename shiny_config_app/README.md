# RMeDPower2 Configuration Generator

A Shiny web application that helps users create JSON configuration files for the RMeDPower2 package classes (`RMeDesign`, `ProbabilityModel`, and `PowerParams`).

## Overview

This interactive app guides users through the process of configuring RMeDPower2 for their repeated measures experiments by:

1. **Data Upload**: Load your experimental data (CSV or RDS format)
2. **RMeDesign Configuration**: Define experimental design parameters
3. **ProbabilityModel Configuration**: Specify statistical distribution assumptions  
4. **PowerParams Configuration**: Set up power analysis parameters
5. **JSON Generation**: Download configuration files for use with RMeDPower2

## Features

- **Interactive Interface**: Step-by-step guidance through configuration
- **Data-Driven**: Column names automatically populated from uploaded data
- **Enhanced Data Preview**: Each column displayed on separate line with type information and sample values
- **Validation**: Built-in help text and parameter validation
- **Export Options**: Download individual JSON files or complete ZIP package
- **Real-time Preview**: See JSON configurations as you make changes
- **Distribution Selection**: Proper handling of statistical distribution families with automatic family_p assignment
- **Smart Defaults**: Sensible default values for all parameters when not specified
- **Enhanced Directory Selection**: Browse and select output directories with common shortcuts
- **Dual File Saving**: Files saved to both browser downloads and custom directory

## Usage

### Installing Required Packages

```r
# Navigate to the app directory
setwd("/Users/rthomas/Documents/RMeDPower2/shiny_config_app")

# Run the installation script
source("install_packages.R")
```

Or install manually:

```r
# For simple version (basic UI)
install.packages(c("shiny", "jsonlite"))

# For full version (dashboard UI)  
install.packages(c("shiny", "shinydashboard", "DT", "jsonlite"))
```

### Running the App

Two versions are available:

**Simple Version (recommended):**
```r
# Uses basic shiny interface, fewer dependencies
shiny::runApp("app_simple.R")
```

**Full Version (enhanced UI):**
```r  
# Uses dashboard interface, requires additional packages
shiny::runApp("app.R")
```

### Workflow

1. **Upload Data**: 
   - Click "Choose CSV/RDS File" and select your experimental data
   - Preview your data structure and column names

2. **Configure RMeDesign**:
   - Select response variable (outcome measure)
   - Choose condition column (main predictor)
   - Define experimental hierarchy (grouping variables)
   - Optionally add covariates and interaction terms

3. **Configure ProbabilityModel**:
   - Specify if data is normally distributed
   - For non-normal data, select appropriate distribution family (Poisson, Negative Binomial, Binomial, Gamma)

4. **Configure PowerParams**:
   - Choose target columns for power analysis
   - Set simulation parameters and effect sizes
   - Configure power curve options

5. **Generate JSON Files**:
   - Choose output directory with enhanced browsing options
   - Preview generated configurations
   - Download individual files or complete package
   - Files saved to both browser downloads and specified directory

## Configuration Guidelines

### RMeDesign Parameters

- **response_column**: Your quantitative outcome variable
- **condition_column**: Main experimental factor to test
- **experimental_columns**: Hierarchical grouping (highest to lowest level)
- **covariate**: Optional confounding variable to control for
- **crossed_columns**: Variables that repeat across hierarchy levels

### ProbabilityModel Parameters

- **error_is_non_normal**: `false` for continuous normal data, `true` for counts/proportions
- **family_p**: Distribution family for non-normal data
  - "poisson": Count data (variance ≈ mean)
  - "negative_binomial": Overdispersed count data
  - "binomial": Proportions (requires total_column)
  - "Gamma": Positive continuous, right-skewed data

### PowerParams Parameters

- **target_columns**: Experimental factors to vary in power analysis
- **levels**: 1 = add more groups, 0 = increase within-group size
- **power_curve**: 1 = generate curves, 0 = single calculation
- **nsimn**: Number of simulations (more = precision but slower)

## Output

The app generates JSON files compatible with RMeDPower2 functions:

```r
# Example usage with generated files
library(RMeDPower2)

# Load configurations
design <- readDesign("RMeDesign_config.json")
model <- readProbabilityModel("ProbabilityModel_config.json") 
power_params <- readPowerParams("PowerParams_config.json")

# Run power analysis
result <- calculatePower(data = your_data, 
                        design = design,
                        model = model, 
                        power_param = power_params)
```

## Dependencies

- shiny
- shinydashboard  
- DT
- jsonlite

## File Structure

```
shiny_config_app/
├── app.R                    # Full version (dashboard UI)
├── app_simple.R             # Simple version (basic UI)
├── ui.R                     # Dashboard UI definition
├── ui_simple.R              # Simple UI definition  
├── server.R                 # Server logic (shared)
├── install_packages.R           # Package installation script
├── test_json_generation.R       # JSON generation test
├── test_distribution_selection.R # Distribution selection test
├── test_directory_browsing.R        # Enhanced directory browsing test
├── test_default_values.R            # Default values and family_p assignment test
├── test_distribution_selection_fix.R # Distribution selection visibility fix test
├── test_conditional_panel_fix.R     # Conditional panel visibility fix verification
└── README.md                        # This file
```

## Enhanced Directory Selection

The app now features improved directory selection in the "Generate JSON" tab:

### Directory Browsing Features
- **Quick Options**: One-click access to Desktop, Documents, Downloads
- **System Directories**: Current working directory, Home, Temp folder
- **Custom Paths**: Type any valid directory path
- **Auto-Creation**: App will create directories if they don't exist (with proper permissions)
- **Real-time Validation**: Visual feedback on directory validity
- **Dual Saving**: Files saved to both browser downloads and your chosen directory

### Using Directory Selection
1. Navigate to the "Generate JSON" tab
2. Click "Browse Directory" to open the enhanced selection modal
3. Use quick options or enter a custom path
4. Click "Confirm Selection" to set your output directory
5. Download files - they'll be saved to both locations

## Smart Default Values

The app automatically provides sensible defaults for all parameters, allowing users to generate valid configurations with minimal input:

### RMeDesign Defaults
- **condition_is_categorical**: `true` (most experiments use categorical conditions)
- **outlier_alpha**: `0.05` (standard significance level)
- **na_action**: `"complete"` (complete case analysis)
- **covariate**: `""` (no covariate by default)
- **covariate_is_categorical**: `false` (continuous covariates more common)
- **include_interaction**: `false` (simpler models preferred initially)
- **crossed_columns**: `null` (nested designs more common)
- **total_column**: `""` (not needed unless binomial data)

### ProbabilityModel Defaults
- **error_is_non_normal**: `false` (normal distribution assumed initially)
- **family_p**: `null` for normal distributions, `"poisson"` when non-normal selected without specifying family

### PowerParams Defaults
- **power_curve**: `1` (generate power curves vs single calculation)
- **levels**: `1` (add more groups rather than increase within-group size)
- **alpha**: `0.05` (standard significance level)
- **nsimn**: `1000` (sufficient simulations for stable results)
- **max_size**: `null` (no size constraints by default)
- **effect_size**: `null` (estimate from data)
- **icc**: `null` (estimate from data)

### Automatic family_p Assignment
The app handles distribution family assignment intelligently:

**When Normal Distribution is selected:**
- family_p is set to `null`
- Distribution family dropdown is hidden (not needed)

**When Non-Normal Distribution is selected:**
- Distribution family dropdown becomes visible
- User can select from: Poisson, Negative Binomial, Binomial, or Gamma
- If user selects a specific family: Uses the selected family
- If no selection made: Automatically defaults to "poisson" (most common)
- Selected family is properly assigned to family_p parameter

**User Experience:**
1. Start with Normal distribution (family dropdown hidden)
2. Select "Non-normal distribution" → family dropdown appears
3. Choose specific distribution family from dropdown
4. Configuration automatically updates with proper family_p value

## Tips

- Start with smaller datasets for faster loading
- Use the data preview to verify column structure before configuration
- Refer to the built-in help text for parameter guidance
- Test configurations with example data before using with large datasets
- Keep JSON files organized by project/experiment for easy reference
- Use the enhanced directory browsing to save files directly to your project folder

## Troubleshooting

- **App won't start**: Ensure all required packages are installed
- **Data won't load**: Check file format (CSV/RDS) and file permissions
- **JSON validation errors**: Verify all required fields are completed
- **Download issues**: Check browser download settings and file permissions
- **Distribution dropdown not appearing**: Fixed in latest version - dropdown now appears when "Non-normal distribution" is selected

### Recent Updates

- **v1.3**: Enhanced data preview format
  - Each column now displayed on separate line for better readability
  - Shows data type and sample values for each column
  - Automatically truncates long values with proper indicators
  - Includes dataset dimensions summary

- **v1.2**: Enhanced directory browsing and filename prefix functionality
  - Added comprehensive file browser with directory navigation, folder creation, and OS integration
  - Implemented filename prefix options with real-time preview
  - Enhanced dual saving (browser downloads + custom directory)
  - Improved user feedback and notifications
  
- **v1.1**: Fixed conditional panel for distribution family selection - dropdown now properly appears when non-normal distribution is selected

For more information about RMeDPower2, see the [package documentation](../docs/index.html) and [configuration guide](../vignettes/RMeDPower2_Class_Configuration_Guide.Rmd).
