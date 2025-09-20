# RMeDPower2 Configuration App Server
library(shiny)
library(jsonlite)

function(input, output, session) {
  
  # Reactive value to store uploaded data
  values <- reactiveValues(
    data = NULL,
    columns = NULL,
    output_dir = getwd()
  )
  
  # Check if file is uploaded
  output$fileUploaded <- reactive({
    return(!is.null(values$data))
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden = FALSE)
  
  # File upload handling
  observeEvent(input$file, {
    req(input$file)
    
    ext <- tools::file_ext(input$file$datapath)
    
    if(ext == "csv") {
      values$data <- read.csv(input$file$datapath, 
                              header = input$header,
                              stringsAsFactors = input$stringsAsFactors)
    } else if(ext %in% c("rds", "RDS")) {
      values$data <- readRDS(input$file$datapath)
    }
    
    if(!is.null(values$data)) {
      values$columns <- names(values$data)
      
      # Update all column choice inputs
      updateSelectInput(session, "response_column", 
                       choices = c("Select column..." = "", values$columns))
      updateSelectInput(session, "condition_column", 
                       choices = c("Select column..." = "", values$columns))
      updateSelectInput(session, "experimental_columns", 
                       choices = values$columns)
      updateSelectInput(session, "covariate", 
                       choices = c("None" = "", values$columns))
      updateSelectInput(session, "crossed_columns", 
                       choices = values$columns)
      updateSelectInput(session, "total_column", 
                       choices = c("None" = "", values$columns))
      updateSelectInput(session, "target_columns", 
                       choices = values$columns)
    }
  })
  
  # Data table output - each column on separate line
  output$dataTable <- renderText({
    req(values$data)
    
    # Get first few rows of data for preview
    preview_data <- head(values$data, 5)
    n_rows <- nrow(preview_data)
    n_cols <- ncol(preview_data)
    
    # Create column-wise preview
    preview_text <- paste0("Data Preview (first ", n_rows, " rows):\n")
    preview_text <- paste0(preview_text, paste(rep("=", 50), collapse = ""), "\n\n")
    
    for (col_name in names(preview_data)) {
      col_data <- preview_data[[col_name]]
      col_type <- class(col_data)[1]
      
      # Format the column values
      if (is.numeric(col_data)) {
        col_values <- paste(round(col_data, 3), collapse = ", ")
      } else {
        col_values <- paste(as.character(col_data), collapse = ", ")
      }
      
      # Truncate if too long
      if (nchar(col_values) > 60) {
        col_values <- paste0(substr(col_values, 1, 57), "...")
      }
      
      preview_text <- paste0(preview_text, 
                           sprintf("%-20s (%s): %s\n", 
                                 col_name, col_type, col_values))
    }
    
    preview_text <- paste0(preview_text, "\n", 
                         sprintf("Dataset: %d rows × %d columns", 
                               nrow(values$data), ncol(values$data)))
    
    return(preview_text)
  })
  
  # Data structure output
  output$dataStructure <- renderText({
    req(values$data)
    capture.output(str(values$data))
  })
  
  # Directory validation
  output$directoryValid <- reactive({
    if (!is.null(input$output_directory) && input$output_directory != "") {
      return(dir.exists(input$output_directory))
    }
    return(FALSE)
  })
  outputOptions(output, 'directoryValid', suspendWhenHidden = FALSE)
  
  # Current directory display
  output$currentDirectory <- renderText({
    if (!is.null(values$output_dir)) {
      paste("Current directory:\n", values$output_dir)
    } else {
      paste("Current directory:\n", getwd())
    }
  })
  
  # Directory handling
  observeEvent(input$output_directory, {
    if (!is.null(input$output_directory) && input$output_directory != "") {
      if (dir.exists(input$output_directory)) {
        values$output_dir <- input$output_directory
        showNotification("Directory updated successfully", type = "message", duration = 2)
      }
    }
  })
  
  # Use current directory button
  observeEvent(input$use_current_dir, {
    current_wd <- getwd()
    values$output_dir <- current_wd
    updateTextInput(session, "output_directory", value = current_wd)
    showNotification(paste("Using current directory:", current_wd), type = "message", duration = 3)
  })
  
  # Enhanced directory browsing modal with file browser capability
  observeEvent(input$browse_directory, {
    showModal(modalDialog(
      title = "Select Output Directory",
      size = "l",
      
      h4("Choose Directory"),
      p("Enter the full path to your desired output directory, or select from common locations:"),
      
      # File browser section
      wellPanel(
        h5("File Browser", style = "color: #3c8dbc;"),
        fluidRow(
          column(6,
            selectInput("drive_select", "Select Drive:",
                      choices = c("/" = "/", "Home" = "~"),
                      selected = "/")
          ),
          column(6,
            actionButton("refresh_browser", "Refresh", 
                       class = "btn-outline-secondary btn-sm",
                       icon = icon("refresh"))
          )
        ),
        
        div(style = "max-height: 300px; overflow-y: auto; border: 1px solid #ddd; padding: 10px;",
          uiOutput("directory_browser")
        ),
        
        fluidRow(
          column(9,
            textInput("selected_path", "Selected Path:", 
                    value = values$output_dir, width = "100%")
          ),
          column(3,
            actionButton("create_new_folder", "New Folder", 
                       class = "btn-outline-primary btn-sm",
                       style = "margin-top: 25px; width: 100%;")
          )
        )
      ),
      
      hr(),
      
      # Common directory shortcuts
      h5("Quick Options:"),
      fluidRow(
        column(4,
          actionButton("modal_desktop", "Desktop", class = "btn-outline-secondary btn-sm", style = "margin: 2px; width: 100%;"),
          actionButton("modal_documents", "Documents", class = "btn-outline-secondary btn-sm", style = "margin: 2px; width: 100%;")
        ),
        column(4,
          actionButton("modal_downloads", "Downloads", class = "btn-outline-secondary btn-sm", style = "margin: 2px; width: 100%;"),
          actionButton("modal_current", "Current Working Dir", class = "btn-outline-info btn-sm", style = "margin: 2px; width: 100%;")
        ),
        column(4,
          actionButton("modal_home", "Home Directory", class = "btn-outline-secondary btn-sm", style = "margin: 2px; width: 100%;"),
          actionButton("modal_temp", "Temp Directory", class = "btn-outline-secondary btn-sm", style = "margin: 2px; width: 100%;")
        )
      ),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_directory", "Confirm Selection", class = "btn-primary")
      )
    ))
    
    # Initialize browser with current directory
    values$browser_path <- if(!is.null(values$output_dir)) values$output_dir else getwd()
    updateTextInput(session, "selected_path", value = values$browser_path)
  })
  
  # Modal directory shortcuts
  observeEvent(input$modal_desktop, {
    desktop_path <- file.path(Sys.getenv("HOME"), "Desktop")
    if (dir.exists(desktop_path)) {
      updateTextInput(session, "modal_directory", value = desktop_path)
    }
  })
  
  observeEvent(input$modal_documents, {
    documents_path <- file.path(Sys.getenv("HOME"), "Documents")
    if (dir.exists(documents_path)) {
      updateTextInput(session, "modal_directory", value = documents_path)
    }
  })
  
  observeEvent(input$modal_downloads, {
    downloads_path <- file.path(Sys.getenv("HOME"), "Downloads")
    if (dir.exists(downloads_path)) {
      updateTextInput(session, "modal_directory", value = downloads_path)
    }
  })
  
  observeEvent(input$modal_current, {
    updateTextInput(session, "modal_directory", value = getwd())
  })
  
  observeEvent(input$modal_home, {
    updateTextInput(session, "modal_directory", value = Sys.getenv("HOME"))
  })
  
  observeEvent(input$modal_temp, {
    updateTextInput(session, "modal_directory", value = tempdir())
  })
  
  # Directory browser functionality
  output$directory_browser <- renderUI({
    current_path <- if(!is.null(values$browser_path)) values$browser_path else getwd()
    
    tryCatch({
      if (!dir.exists(current_path)) {
        return(div("Invalid directory path", style = "color: red;"))
      }
      
      dirs <- list.dirs(current_path, full.names = TRUE, recursive = FALSE)
      dirs <- dirs[!grepl("^\\.", basename(dirs))]  # Hide hidden directories
      
      if (length(dirs) == 0) {
        return(div("No subdirectories found", style = "color: gray;"))
      }
      
      # Create clickable directory list
      dir_buttons <- lapply(1:length(dirs), function(i) {
        dir_name <- basename(dirs[i])
        actionButton(paste0("dir_", i), 
                   label = div(icon("folder"), " ", dir_name),
                   class = "btn-link",
                   style = "text-align: left; width: 100%; margin: 2px 0;",
                   onclick = paste0("Shiny.setInputValue('selected_dir', '", dirs[i], "');"))
      })
      
      # Add parent directory option if not at root
      if (current_path != "/" && current_path != dirname(current_path)) {
        parent_button <- actionButton("parent_dir",
                                    label = div(icon("level-up-alt"), " .. (Parent Directory)"),
                                    class = "btn-link",
                                    style = "text-align: left; width: 100%; margin: 2px 0; color: #007bff;")
        dir_buttons <- c(list(parent_button), dir_buttons)
      }
      
      return(div(dir_buttons))
      
    }, error = function(e) {
      return(div(paste("Error reading directory:", e$message), style = "color: red;"))
    })
  })
  
  # Handle directory selection in browser
  observeEvent(input$selected_dir, {
    values$browser_path <- input$selected_dir
    updateTextInput(session, "selected_path", value = values$browser_path)
  })
  
  # Handle parent directory navigation
  observeEvent(input$parent_dir, {
    if (!is.null(values$browser_path)) {
      parent_path <- dirname(values$browser_path)
      if (parent_path != values$browser_path) {  # Avoid infinite loop at root
        values$browser_path <- parent_path
        updateTextInput(session, "selected_path", value = values$browser_path)
      }
    }
  })
  
  # Handle drive selection
  observeEvent(input$drive_select, {
    if (input$drive_select == "~") {
      values$browser_path <- Sys.getenv("HOME")
    } else {
      values$browser_path <- input$drive_select
    }
    updateTextInput(session, "selected_path", value = values$browser_path)
  })
  
  # Handle refresh browser
  observeEvent(input$refresh_browser, {
    # Trigger browser refresh by updating the reactive value
    values$browser_refresh <- if(is.null(values$browser_refresh)) 1 else values$browser_refresh + 1
  })
  
  # Handle path text input changes
  observeEvent(input$selected_path, {
    if (!is.null(input$selected_path) && input$selected_path != "") {
      values$browser_path <- input$selected_path
    }
  })
  
  # Create new folder functionality
  observeEvent(input$create_new_folder, {
    showModal(modalDialog(
      title = "Create New Folder",
      textInput("new_folder_name", "Folder Name:", placeholder = "Enter folder name"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_create_folder", "Create", class = "btn-primary")
      )
    ))
  })
  
  observeEvent(input$confirm_create_folder, {
    if (!is.null(input$new_folder_name) && input$new_folder_name != "") {
      new_folder_path <- file.path(values$browser_path, input$new_folder_name)
      tryCatch({
        dir.create(new_folder_path)
        values$browser_path <- new_folder_path
        updateTextInput(session, "selected_path", value = values$browser_path)
        removeModal()
        showNotification(paste("Folder created:", input$new_folder_name), type = "message", duration = 3)
      }, error = function(e) {
        showNotification(paste("Error creating folder:", e$message), type = "error", duration = 5)
      })
    }
  })
  
  # Open directory in file manager
  observeEvent(input$open_directory, {
    if (!is.null(values$output_dir) && dir.exists(values$output_dir)) {
      tryCatch({
        if (Sys.info()["sysname"] == "Darwin") {  # macOS
          system(paste("open", shQuote(values$output_dir)))
        } else if (Sys.info()["sysname"] == "Windows") {  # Windows
          system(paste("explorer", shQuote(values$output_dir)))
        } else {  # Linux
          system(paste("xdg-open", shQuote(values$output_dir)))
        }
        showNotification("Opening directory in file manager", type = "message", duration = 3)
      }, error = function(e) {
        showNotification("Could not open directory in file manager", type = "warning", duration = 3)
      })
    } else {
      showNotification("Please select a valid directory first", type = "warning", duration = 3)
    }
  })
  
  # Confirm directory selection
  observeEvent(input$confirm_directory, {
    selected_path <- if(!is.null(input$selected_path)) input$selected_path else values$browser_path
    
    if (!is.null(selected_path) && selected_path != "") {
      if (dir.exists(selected_path)) {
        values$output_dir <- selected_path
        updateTextInput(session, "output_directory", value = values$output_dir)
        removeModal()
        showNotification(paste("Directory selected:", values$output_dir), 
                        type = "message", duration = 4)
      } else {
        # Try to create the directory
        tryCatch({
          dir.create(selected_path, recursive = TRUE)
          values$output_dir <- selected_path
          updateTextInput(session, "output_directory", value = values$output_dir)
          removeModal()
          showNotification(paste("Directory created and selected:", values$output_dir), 
                          type = "message", duration = 4)
        }, error = function(e) {
          showNotification("Directory does not exist and cannot be created. Please check the path and permissions.", 
                          type = "error", duration = 5)
        })
      }
    }
  })
  
  # Filename prefix examples
  output$example_filenames <- renderText({
    if (!is.null(input$filename_prefix) && input$filename_prefix != "") {
      prefix <- input$filename_prefix
      date_suffix <- paste0("_", Sys.Date())
      paste(
        paste0(prefix, "RMeDesign_config", date_suffix, ".json"),
        paste0(prefix, "ProbabilityModel_config", date_suffix, ".json"),
        paste0(prefix, "PowerParams_config", date_suffix, ".json"),
        sep = "\n"
      )
    } else {
      ""
    }
  })
  
  output$example_filenames_full <- renderText({
    if (!is.null(input$filename_prefix) && input$filename_prefix != "") {
      prefix <- input$filename_prefix
      date_suffix <- paste0("_", Sys.Date())
      paste(
        paste0("• ", prefix, "RMeDesign_config", date_suffix, ".json"),
        paste0("• ", prefix, "ProbabilityModel_config", date_suffix, ".json"),
        paste0("• ", prefix, "PowerParams_config", date_suffix, ".json"),
        sep = "\n"
      )
    } else {
      "No prefix specified"
    }
  })
  
  # Generate RMeDesign JSON
  designConfig <- reactive({
    # Only require data to be uploaded, other parameters use defaults if not specified
    req(values$data)
    
    # Handle required parameters with defaults
    response_column <- if(!is.null(input$response_column) && input$response_column != "") {
      input$response_column
    } else {
      if(length(values$columns) > 0) values$columns[1] else "response_column"
    }
    
    condition_column <- if(!is.null(input$condition_column) && input$condition_column != "") {
      input$condition_column
    } else {
      if(length(values$columns) > 1) values$columns[2] else "condition_column"
    }
    
    experimental_columns <- if(!is.null(input$experimental_columns) && length(input$experimental_columns) > 0) {
      input$experimental_columns
    } else {
      if(length(values$columns) > 2) values$columns[3:min(4, length(values$columns))] else c("experimental_column1")
    }
    
    config <- list(
      response_column = response_column,
      condition_column = condition_column,
      condition_is_categorical = if(!is.null(input$condition_is_categorical)) {
        as.logical(input$condition_is_categorical)
      } else {
        TRUE  # Default: categorical
      },
      experimental_columns = experimental_columns,
      outlier_alpha = if(!is.null(input$outlier_alpha)) {
        input$outlier_alpha
      } else {
        0.05  # Default
      },
      na_action = if(!is.null(input$na_action)) {
        input$na_action
      } else {
        "complete"  # Default
      }
    )
    
    # Add optional parameters with defaults
    config$covariate <- if(!is.null(input$covariate) && input$covariate != "") {
      input$covariate
    } else {
      ""  # Default: no covariate
    }
    
    if(config$covariate != "") {
      config$covariate_is_categorical <- if(!is.null(input$covariate_is_categorical)) {
        as.logical(input$covariate_is_categorical)
      } else {
        FALSE  # Default: continuous covariate
      }
      
      config$include_interaction <- if(!is.null(input$include_interaction)) {
        input$include_interaction
      } else {
        FALSE  # Default: no interaction
      }
    }
    
    config$crossed_columns <- if(!is.null(input$crossed_columns) && length(input$crossed_columns) > 0) {
      input$crossed_columns
    } else {
      NULL  # Default: no crossed columns
    }
    
    config$total_column <- if(!is.null(input$total_column) && input$total_column != "") {
      input$total_column
    } else {
      ""  # Default: no total column
    }
    
    if(!is.null(input$random_slope_variable) && input$random_slope_variable != "") {
      config$random_slope_variable <- input$random_slope_variable
    }
    
    return(config)
  })
  
  # Generate ProbabilityModel JSON
  modelConfig <- reactive({
    # Default values for ProbabilityModel
    error_is_non_normal <- if(!is.null(input$error_is_non_normal)) {
      as.logical(input$error_is_non_normal)
    } else {
      FALSE  # Default: normal distribution
    }
    
    config <- list(
      error_is_non_normal = error_is_non_normal
    )
    
    # Handle family_p parameter
    if(error_is_non_normal) {
      # If non-normal is selected, family_p must be specified
      if(!is.null(input$family_p) && input$family_p != "") {
        config$family_p <- input$family_p
      } else {
        # Default to "poisson" if non-normal is selected but no family specified
        config$family_p <- "poisson"
      }
    } else {
      # If normal distribution, family_p should be null
      config$family_p <- NULL
    }
    
    return(config)
  })
  
  # Generate PowerParams JSON
  powerConfig <- reactive({
    # Provide defaults even if target_columns not specified
    target_columns <- if(!is.null(input$target_columns) && length(input$target_columns) > 0) {
      input$target_columns
    } else {
      # Default to first experimental column if available
      if(!is.null(values$data) && !is.null(input$experimental_columns) && length(input$experimental_columns) > 0) {
        input$experimental_columns[1]
      } else {
        "target_columns"  # Template default
      }
    }
    
    config <- list(
      target_columns = target_columns,
      power_curve = if(!is.null(input$power_curve)) {
        as.numeric(input$power_curve)
      } else {
        1  # Default: generate power curve
      },
      levels = if(!is.null(input$levels)) {
        input$levels
      } else {
        1  # Default: add more groups
      },
      alpha = if(!is.null(input$alpha)) {
        input$alpha
      } else {
        0.05  # Default significance level
      },
      nsimn = if(!is.null(input$nsimn)) {
        input$nsimn
      } else {
        1000  # Default number of simulations (changed from template's 10)
      }
    )
    
    # Add optional parameters with defaults
    if(!is.null(input$max_size) && !is.na(input$max_size)) {
      config$max_size <- input$max_size
    } else {
      config$max_size <- NULL  # Default: no maximum size constraint
    }
    
    if(!is.null(input$effect_size) && !is.na(input$effect_size)) {
      config$effect_size <- input$effect_size
    } else {
      config$effect_size <- NULL  # Default: estimate from data
    }
    
    if(!is.null(input$icc) && input$icc != "") {
      # Parse comma-separated ICC values
      icc_values <- tryCatch({
        as.numeric(trimws(strsplit(input$icc, ",")[[1]]))
      }, error = function(e) NULL)
      
      if(!is.null(icc_values) && all(!is.na(icc_values))) {
        config$icc <- icc_values
      } else {
        config$icc <- NULL
      }
    } else {
      config$icc <- NULL  # Default: estimate from data
    }
    
    config$breaks <- NULL  # Default: automatic breaks
    
    return(config)
  })
  
  # Display JSON outputs
  output$designJSON <- renderText({
    tryCatch({
      toJSON(designConfig(), pretty = TRUE, auto_unbox = TRUE)
    }, error = function(e) {
      "Upload data to generate configuration with default values"
    })
  })
  
  output$modelJSON <- renderText({
    tryCatch({
      toJSON(modelConfig(), pretty = TRUE, auto_unbox = TRUE)
    }, error = function(e) {
      "Upload data to generate configuration with default values"
    })
  })
  
  output$powerJSON <- renderText({
    tryCatch({
      toJSON(powerConfig(), pretty = TRUE, auto_unbox = TRUE)
    }, error = function(e) {
      "Upload data to generate configuration with default values"
    })
  })
  
  # Helper function to generate filename with prefix
  generate_filename <- function(base_name, prefix = NULL, include_date = TRUE) {
    prefix_part <- if(!is.null(prefix) && prefix != "") paste0(prefix) else ""
    date_part <- if(include_date) paste0("_", Sys.Date()) else ""
    paste0(prefix_part, base_name, date_part, ".json")
  }
  
  # Download handlers
  output$downloadDesign <- downloadHandler(
    filename = function() {
      prefix <- if(!is.null(input$filename_prefix)) input$filename_prefix else ""
      generate_filename("RMeDesign_config", prefix)
    },
    content = function(file) {
      json_content <- toJSON(designConfig(), pretty = TRUE, auto_unbox = TRUE)
      write(json_content, file)
      
      # Also save to user's specified directory
      if (!is.null(values$output_dir) && dir.exists(values$output_dir)) {
        prefix <- if(!is.null(input$filename_prefix)) input$filename_prefix else ""
        filename <- generate_filename("RMeDesign_config", prefix)
        output_file <- file.path(values$output_dir, filename)
        write(json_content, output_file)
        showNotification(paste("File also saved to:", output_file), type = "message", duration = 5)
      }
    }
  )
  
  output$downloadModel <- downloadHandler(
    filename = function() {
      prefix <- if(!is.null(input$filename_prefix)) input$filename_prefix else ""
      generate_filename("ProbabilityModel_config", prefix)
    },
    content = function(file) {
      json_content <- toJSON(modelConfig(), pretty = TRUE, auto_unbox = TRUE)
      write(json_content, file)
      
      # Also save to user's specified directory
      if (!is.null(values$output_dir) && dir.exists(values$output_dir)) {
        prefix <- if(!is.null(input$filename_prefix)) input$filename_prefix else ""
        filename <- generate_filename("ProbabilityModel_config", prefix)
        output_file <- file.path(values$output_dir, filename)
        write(json_content, output_file)
        showNotification(paste("File also saved to:", output_file), type = "message", duration = 5)
      }
    }
  )
  
  output$downloadPower <- downloadHandler(
    filename = function() {
      prefix <- if(!is.null(input$filename_prefix)) input$filename_prefix else ""
      generate_filename("PowerParams_config", prefix)
    },
    content = function(file) {
      json_content <- toJSON(powerConfig(), pretty = TRUE, auto_unbox = TRUE)
      write(json_content, file)
      
      # Also save to user's specified directory
      if (!is.null(values$output_dir) && dir.exists(values$output_dir)) {
        prefix <- if(!is.null(input$filename_prefix)) input$filename_prefix else ""
        filename <- generate_filename("PowerParams_config", prefix)
        output_file <- file.path(values$output_dir, filename)
        write(json_content, output_file)
        showNotification(paste("File also saved to:", output_file), type = "message", duration = 5)
      }
    }
  )
  
  output$downloadAll <- downloadHandler(
    filename = function() {
      prefix <- if(!is.null(input$filename_prefix)) input$filename_prefix else ""
      if (prefix != "") {
        paste0(prefix, "RMeDPower2_configs_", Sys.Date(), ".zip")
      } else {
        paste0("RMeDPower2_configs_", Sys.Date(), ".zip")
      }
    },
    content = function(file) {
      # Create temporary directory
      temp_dir <- tempdir()
      prefix <- if(!is.null(input$filename_prefix)) input$filename_prefix else ""
      
      # Generate filenames with prefix
      design_filename <- generate_filename("RMeDesign_config", prefix)
      model_filename <- generate_filename("ProbabilityModel_config", prefix)
      power_filename <- generate_filename("PowerParams_config", prefix)
      
      # Write JSON files to temp directory
      design_file <- file.path(temp_dir, design_filename)
      model_file <- file.path(temp_dir, model_filename)
      power_file <- file.path(temp_dir, power_filename)
      
      design_content <- toJSON(designConfig(), pretty = TRUE, auto_unbox = TRUE)
      model_content <- toJSON(modelConfig(), pretty = TRUE, auto_unbox = TRUE)
      power_content <- toJSON(powerConfig(), pretty = TRUE, auto_unbox = TRUE)
      
      write(design_content, design_file)
      write(model_content, model_file)
      write(power_content, power_file)
      
      # Also save individual files to user's specified directory
      if (!is.null(values$output_dir) && dir.exists(values$output_dir)) {
        user_design_file <- file.path(values$output_dir, design_filename)
        user_model_file <- file.path(values$output_dir, model_filename)
        user_power_file <- file.path(values$output_dir, power_filename)
        
        write(design_content, user_design_file)
        write(model_content, user_model_file)
        write(power_content, user_power_file)
        
        showNotification(paste("Files saved to:", values$output_dir), 
                        type = "message", duration = 5)
      }
      
      # Create zip file
      zip(file, files = c(design_file, model_file, power_file), 
          flags = "-j") # -j flag to junk paths
    }
  )
}
