# ============================================================================
# COMPONENT LOADER
# R/load_components.R
# ============================================================================

#' Load All Server Components
#' 
#' Sources all modular server component files
#' Call this from global.R to load all components
load_server_components <- function() {
  
  message("=== LOADING SERVER COMPONENTS ===")
  
  # Helper functions
  source("R/helpers/step_creators.R")
  source("R/helpers/data_retrievers.R")
  source("R/helpers/debug_outputs.R")
  
  # Server components
  source("R/server_components/authentication.R")
  source("R/server_components/data_processing.R") 
  source("R/server_components/table_generation.R")
  source("R/server_components/navigation.R")
  source("R/server_components/milestone_visualization.R")
  source("R/server_components/evaluation_displays.R")
  source("R/server_components/submission_handlers.R")
  
  message("✅ All server components loaded successfully")
}