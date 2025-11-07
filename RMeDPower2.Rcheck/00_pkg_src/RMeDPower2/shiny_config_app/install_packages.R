# Install required packages for RMeDPower2 Configuration App
# Run this script before using the Shiny app

# Required packages for the simple version
required_packages_simple <- c("shiny", "jsonlite")

# Required packages for the full version (with dashboard UI)
required_packages_full <- c("shiny", "shinydashboard", "DT", "jsonlite")

# Function to install packages if not already installed
install_if_missing <- function(packages) {
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      cat("Installing", pkg, "...\n")
      install.packages(pkg, dependencies = TRUE)
    } else {
      cat(pkg, "is already installed\n")
    }
  }
}

# Choose which version to install packages for
cat("RMeDPower2 Configuration App - Package Installation\n")
cat("====================================================\n\n")

cat("Choose installation option:\n")
cat("1. Simple version (basic shiny only)\n") 
cat("2. Full version (with dashboard UI)\n")
cat("3. Install all packages\n\n")

choice <- readline(prompt = "Enter choice (1, 2, or 3): ")

if (choice == "1") {
  cat("\nInstalling packages for simple version...\n")
  install_if_missing(required_packages_simple)
  cat("\nSimple version packages installed. Use 'app_simple.R' to run the app.\n")
} else if (choice == "2") {
  cat("\nInstalling packages for full version...\n")
  install_if_missing(required_packages_full)
  cat("\nFull version packages installed. Use 'app.R' to run the app.\n")
} else if (choice == "3") {
  cat("\nInstalling all packages...\n")
  install_if_missing(required_packages_full)
  cat("\nAll packages installed. You can use either version.\n")
} else {
  cat("Invalid choice. Please run the script again and choose 1, 2, or 3.\n")
}

cat("\nPackage installation complete!\n")
cat("\nTo run the app:\n")
cat("- Simple version: shiny::runApp('app_simple.R')\n")
cat("- Full version: shiny::runApp('app.R')\n")