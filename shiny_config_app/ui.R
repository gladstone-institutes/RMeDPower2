# RMeDPower2 Configuration App UI
library(shiny)
library(shinydashboard)
library(DT)

dashboardPage(
  dashboardHeader(title = "RMeDPower2 Configuration Generator"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("1. Data Upload", tabName = "upload", icon = icon("upload")),
      menuItem("2. RMeDesign", tabName = "design", icon = icon("cogs")),
      menuItem("3. ProbabilityModel", tabName = "model", icon = icon("chart-line")),
      menuItem("4. PowerParams", tabName = "power", icon = icon("calculator")),
      menuItem("5. Generate JSON", tabName = "output", icon = icon("download"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Data Upload Tab
      tabItem(tabName = "upload",
        fluidRow(
          box(width = 12, title = "Data Upload", status = "primary", solidHeader = TRUE,
            h4("Upload your repeated measures experiment data"),
            p("Upload a CSV or RDS file containing your experimental data. The app will help you configure the RMeDPower2 classes based on your data structure."),
            
            fileInput("file", "Choose CSV/RDS File",
                     accept = c(".csv", ".rds", ".RDS"),
                     multiple = FALSE),
            
            checkboxInput("header", "Header", TRUE),
            checkboxInput("stringsAsFactors", "Strings as factors", FALSE),
            
            hr(),
            
            conditionalPanel("output.fileUploaded",
              h4("Data Preview"),
              verbatimTextOutput("dataTable"),
              br(),
              h4("Column Information"),
              verbatimTextOutput("dataStructure")
            )
          )
        )
      ),
      
      # RMeDesign Configuration Tab
      tabItem(tabName = "design",
        fluidRow(
          box(width = 12, title = "RMeDesign Configuration", status = "primary", solidHeader = TRUE,
            conditionalPanel("!output.fileUploaded",
              h4("Please upload data first", style = "color: orange;")
            ),
            
            conditionalPanel("output.fileUploaded",
              h4("Define your experimental design"),
              p("Configure the RMeDesign class parameters based on your data structure."),
              
              fluidRow(
                column(6,
                  h5("Required Parameters", style = "font-weight: bold; color: #3c8dbc;"),
                  
                  selectInput("response_column", 
                            label = div("Response Column", 
                                      style = "font-weight: bold;",
                                      br(),
                                      span("Your outcome/dependent variable", style = "font-size: 12px; color: gray;")),
                            choices = NULL),
                  
                  selectInput("condition_column",
                            label = div("Condition Column",
                                      style = "font-weight: bold;",
                                      br(), 
                                      span("Your main predictor/treatment variable", style = "font-size: 12px; color: gray;")),
                            choices = NULL),
                  
                  radioButtons("condition_is_categorical",
                             label = div("Condition Type",
                                       style = "font-weight: bold;",
                                       br(),
                                       span("Is your condition categorical or continuous?", style = "font-size: 12px; color: gray;")),
                             choices = list("Categorical (treatment groups, genotypes)" = TRUE,
                                          "Continuous (dose, age, time as numeric)" = FALSE),
                             selected = TRUE),
                  
                  selectInput("experimental_columns",
                            label = div("Experimental Hierarchy",
                                      style = "font-weight: bold;",
                                      br(),
                                      span("Grouping variables from highest to lowest level", style = "font-size: 12px; color: gray;")),
                            choices = NULL,
                            multiple = TRUE)
                ),
                
                column(6,
                  h5("Optional Parameters", style = "font-weight: bold; color: #3c8dbc;"),
                  
                  selectInput("covariate",
                            label = div("Covariate",
                                      style = "font-weight: bold;",
                                      br(),
                                      span("Confounding variable to control for (optional)", style = "font-size: 12px; color: gray;")),
                            choices = NULL),
                  
                  conditionalPanel("input.covariate != ''",
                    radioButtons("covariate_is_categorical",
                               label = div("Covariate Type",
                                         style = "font-weight: bold;",
                                         br(),
                                         span("Is your covariate categorical or continuous?", style = "font-size: 12px; color: gray;")),
                               choices = list("Categorical (gender, batch labels)" = TRUE,
                                            "Continuous (age, baseline measurements)" = FALSE),
                               selected = FALSE),
                    
                    checkboxInput("include_interaction",
                                label = div("Include Interaction",
                                          style = "font-weight: bold;",
                                          br(),
                                          span("Include interaction between condition and covariate", style = "font-size: 12px; color: gray;")),
                                value = FALSE)
                  ),
                  
                  selectInput("crossed_columns",
                            label = div("Crossed Columns",
                                      style = "font-weight: bold;",
                                      br(),
                                      span("Variables that repeat across hierarchy levels (optional)", style = "font-size: 12px; color: gray;")),
                            choices = NULL,
                            multiple = TRUE),
                  
                  selectInput("total_column",
                            label = div("Total Column",
                                      style = "font-weight: bold;",
                                      br(),
                                      span("For binomial data: denominator column (optional)", style = "font-size: 12px; color: gray;")),
                            choices = NULL),
                  
                  selectInput("random_slope_variable",
                            label = div("Random Slope Variable",
                                      style = "font-weight: bold;",
                                      br(),
                                      span("Variable with effects that vary across units (optional)", style = "font-size: 12px; color: gray;")),
                            choices = list("None" = "",
                                         "Condition effects vary across units" = "condition_column",
                                         "Covariate effects vary across units" = "covariate"),
                            selected = "")
                )
              ),
              
              hr(),
              
              h5("Advanced Options", style = "font-weight: bold; color: #3c8dbc;"),
              fluidRow(
                column(6,
                  numericInput("outlier_alpha",
                             label = div("Outlier Detection Alpha",
                                       style = "font-weight: bold;",
                                       br(),
                                       span("Significance level for outlier detection", style = "font-size: 12px; color: gray;")),
                             value = 0.05,
                             min = 0.001,
                             max = 0.1,
                             step = 0.001)
                ),
                column(6,
                  selectInput("na_action",
                            label = div("Missing Data Handling",
                                      style = "font-weight: bold;",
                                      br(),
                                      span("How to handle missing values", style = "font-size: 12px; color: gray;")),
                            choices = list("Complete cases only" = "complete",
                                         "Exclude missing" = "exclude"),
                            selected = "complete")
                )
              )
            )
          )
        )
      ),
      
      # ProbabilityModel Configuration Tab
      tabItem(tabName = "model",
        fluidRow(
          box(width = 12, title = "ProbabilityModel Configuration", status = "primary", solidHeader = TRUE,
            conditionalPanel("!output.fileUploaded",
              h4("Please upload data first", style = "color: orange;")
            ),
            
            conditionalPanel("output.fileUploaded",
              h4("Define your statistical model assumptions"),
              p("Configure the ProbabilityModel class to specify the distribution of your response variable."),
              
              fluidRow(
                column(6,
                  radioButtons("error_is_non_normal",
                             label = div("Data Distribution",
                                       style = "font-weight: bold;",
                                       br(),
                                       span("Is your response variable normally distributed?", style = "font-size: 12px; color: gray;")),
                             choices = list("Normal distribution (continuous, approximately normal)" = FALSE,
                                          "Non-normal distribution (counts, proportions, skewed)" = TRUE),
                             selected = FALSE),
                  
                  conditionalPanel("input.error_is_non_normal == 'TRUE'",
                    selectInput("family_p",
                              label = div("Distribution Family",
                                        style = "font-weight: bold;",
                                        br(),
                                        span("Select the appropriate distribution", style = "font-size: 12px; color: gray;")),
                              choices = list("Poisson (count data, variance ≈ mean)" = "poisson",
                                           "Negative Binomial (overdispersed counts)" = "negative_binomial", 
                                           "Binomial (proportions, binary outcomes)" = "binomial",
                                           "Gamma (positive continuous, right-skewed)" = "Gamma"),
                              selected = "poisson")
                  )
                )
              ),
              
              hr(),
              
              div(style = "background-color: #f9f9f9; padding: 15px; border-radius: 5px;",
                h5("Distribution Guidelines", style = "font-weight: bold; color: #3c8dbc;"),
                tags$ul(
                  tags$li(tags$strong("Normal:"), " Heights, measurements, gene expression levels (continuous data)"),
                  tags$li(tags$strong("Poisson:"), " Number of cells, events, mutations (count data where variance ≈ mean)"),
                  tags$li(tags$strong("Negative Binomial:"), " RNA-seq read counts (overdispersed count data)"),
                  tags$li(tags$strong("Binomial:"), " Number of positive cells out of total counted (requires total_column)"),
                  tags$li(tags$strong("Gamma:"), " Reaction times, concentrations (positive continuous, right-skewed)")
                )
              )
            )
          )
        )
      ),
      
      # PowerParams Configuration Tab  
      tabItem(tabName = "power",
        fluidRow(
          box(width = 12, title = "PowerParams Configuration", status = "primary", solidHeader = TRUE,
            conditionalPanel("!output.fileUploaded",
              h4("Please upload data first", style = "color: orange;")
            ),
            
            conditionalPanel("output.fileUploaded",
              h4("Configure power analysis parameters"),
              p("Set up parameters for power analysis and sample size calculations."),
              
              fluidRow(
                column(6,
                  h5("Core Parameters", style = "font-weight: bold; color: #3c8dbc;"),
                  
                  selectInput("target_columns",
                            label = div("Target Columns",
                                      style = "font-weight: bold;",
                                      br(),
                                      span("Which experimental factors to vary in power analysis", style = "font-size: 12px; color: gray;")),
                            choices = NULL,
                            multiple = TRUE),
                  
                  numericInput("levels",
                             label = div("Levels",
                                       style = "font-weight: bold;",
                                       br(),
                                       span("1 = add more groups; 0 = increase size within groups", style = "font-size: 12px; color: gray;")),
                             value = 1,
                             min = 0,
                             max = 1,
                             step = 1),
                  
                  radioButtons("power_curve",
                             label = div("Analysis Type",
                                       style = "font-weight: bold;",
                                       br(),
                                       span("Generate power curve or single calculation?", style = "font-size: 12px; color: gray;")),
                             choices = list("Power curve (varying sample sizes)" = 1,
                                          "Single power calculation" = 0),
                             selected = 1),
                  
                  numericInput("alpha",
                             label = div("Significance Level (α)",
                                       style = "font-weight: bold;",
                                       br(),
                                       span("Type I error rate", style = "font-size: 12px; color: gray;")),
                             value = 0.05,
                             min = 0.001,
                             max = 0.1,
                             step = 0.001)
                ),
                
                column(6,
                  h5("Simulation Parameters", style = "font-weight: bold; color: #3c8dbc;"),
                  
                  numericInput("nsimn",
                             label = div("Number of Simulations",
                                       style = "font-weight: bold;",
                                       br(),
                                       span("More simulations = more precision but slower", style = "font-size: 12px; color: gray;")),
                             value = 1000,
                             min = 100,
                             max = 10000,
                             step = 100),
                  
                  numericInput("max_size",
                             label = div("Maximum Sample Size",
                                       style = "font-weight: bold;",
                                       br(),
                                       span("Budget/feasibility constraint (optional)", style = "font-size: 12px; color: gray;")),
                             value = NULL,
                             min = 1),
                  
                  numericInput("effect_size",
                             label = div("Effect Size",
                                       style = "font-weight: bold;",
                                       br(),
                                       span("Known biologically meaningful effect size (optional)", style = "font-size: 12px; color: gray;")),
                             value = NULL,
                             step = 0.1),
                  
                  textInput("icc",
                            label = div("Intra-Class Correlation (ICC)",
                                      style = "font-weight: bold;",
                                      br(),
                                      span("Comma-separated values for experimental hierarchy (optional)", style = "font-size: 12px; color: gray;")),
                            placeholder = "e.g., 0.1, 0.05")
                )
              )
            )
          )
        )
      ),
      
      # Output Tab
      tabItem(tabName = "output",
        fluidRow(
          box(width = 8, title = "Generate JSON Configuration Files", status = "success", solidHeader = TRUE,
            conditionalPanel("!output.fileUploaded",
              h4("Please upload data and configure parameters first", style = "color: orange;")
            ),
            
            conditionalPanel("output.fileUploaded",
              h4("Download your configuration files"),
              p("Generate JSON files for each class that can be used with RMeDPower2 functions."),
              
              fluidRow(
                column(4,
                  h5("RMeDesign JSON", style = "font-weight: bold; color: #3c8dbc;"),
                  verbatimTextOutput("designJSON"),
                  downloadButton("downloadDesign", "Download design.json", class = "btn-primary")
                ),
                column(4,
                  h5("ProbabilityModel JSON", style = "font-weight: bold; color: #3c8dbc;"),
                  verbatimTextOutput("modelJSON"),
                  downloadButton("downloadModel", "Download model.json", class = "btn-primary")
                ),
                column(4,
                  h5("PowerParams JSON", style = "font-weight: bold; color: #3c8dbc;"),
                  verbatimTextOutput("powerJSON"),
                  downloadButton("downloadPower", "Download power.json", class = "btn-primary")
                )
              ),
              
              hr(),
              
              div(style = "text-align: center;",
                h5("Complete Package", style = "font-weight: bold; color: #3c8dbc;"),
                downloadButton("downloadAll", "Download All JSON Files (ZIP)", 
                             class = "btn-success btn-lg",
                             icon = icon("download"))
              )
            )
          ),
          
          box(width = 4, title = "Output Settings", status = "primary", solidHeader = TRUE,
            conditionalPanel("!output.fileUploaded",
              h5("Upload data first to configure output settings", style = "color: gray;")
            ),
            
            conditionalPanel("output.fileUploaded",
              h5("Save Location", style = "font-weight: bold; color: #3c8dbc;"),
              p("Choose where to save your JSON configuration files."),
              
              textInput("output_directory",
                      label = div("Directory Path",
                                style = "font-weight: bold;",
                                br(),
                                span("Full path to output directory", style = "font-size: 12px; color: gray;")),
                      value = getwd(),
                      placeholder = "Enter directory path"),
              
              fluidRow(
                column(4,
                  actionButton("browse_directory", 
                             "Browse Directory", 
                             class = "btn-secondary",
                             icon = icon("folder-open"),
                             style = "width: 100%;")
                ),
                column(4,
                  actionButton("use_current_dir", 
                             "Use Current Dir", 
                             class = "btn-info",
                             icon = icon("folder"),
                             style = "width: 100%;")
                ),
                column(4,
                  actionButton("open_directory", 
                             "Open in Finder", 
                             class = "btn-outline-secondary",
                             icon = icon("external-link-alt"),
                             style = "width: 100%;")
                )
              ),
              
              br(),
              
              conditionalPanel("output.directoryValid",
                div(
                  icon("check-circle", style = "color: green;"),
                  span(" Directory is valid and writable", style = "color: green; font-weight: bold;")
                )
              ),
              
              conditionalPanel("!output.directoryValid", 
                div(
                  icon("exclamation-triangle", style = "color: orange;"),
                  span(" Directory does not exist or is not writable", style = "color: orange; font-weight: bold;")
                )
              ),
              
              hr(),
              
              h5("File Naming Options", style = "font-weight: bold; color: #3c8dbc;"),
              
              textInput("filename_prefix",
                      label = div("Filename Prefix",
                                style = "font-weight: bold;",
                                br(),
                                span("Optional prefix for all generated files", style = "font-size: 12px; color: gray;")),
                      value = "",
                      placeholder = "e.g., study1_"),
              
              conditionalPanel("input.filename_prefix != ''",
                div(style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; margin-bottom: 10px;",
                  h6("Example Filenames:", style = "margin-top: 0; color: #495057;"),
                  div(style = "font-family: monospace; font-size: 12px; color: #6c757d;",
                      textOutput("example_filenames_full", inline = FALSE))
                )
              ),
              
              hr(),
              
              helpText("Files will be saved to both your browser's download folder and the specified directory above."),
              
              verbatimTextOutput("currentDirectory")
            )
          )
        )
      )
    )
  )
)
