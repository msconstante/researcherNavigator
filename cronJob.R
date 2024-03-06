#!/usr/bin/Rscript

# Get variable from the command line
# This is the main directory to start at
args <- commandArgs(trailingOnly = TRUE)
maindir <- args[1]
output_dir <- paste0(maindir, "/output")
#cat("Processing subdirectories in", output_dir, "\n")

# List all directories in the output directory
subdirs <- list.dirs(output_dir, full.names = TRUE, recursive = FALSE)

# Cycle through all the subdirectories and find the ones that have a 'request.rds' file
for (dir in subdirs) {
  #cat("Checking directory:", dir, "\n")
  request_file <- file.path(dir, "request.rds")
  if (file.exists(request_file)) {
    # cat("request.rds file found in", dir, "\n")
    # If results.rds exists, move to the next folder
    results_file <- file.path(dir, "results.rds")
    if (file.exists(results_file)) {
      # cat("Results file exists in", dir, "\n")
      next
    }
    # Check if a running.tmp file exists
    running_file <- file.path("output", dir, "running.tmp")
    if (file.exists(running_file)) {
      cat("Running file exists in", dir, "\n")
      next
    } else {
      # Call the rscript 'ProcessRequest.R with dir as code'
      cat("Processing", dir, "\n")
      # Create a running.tmp file
      #blank <- ""
      #saveRDS(blank, running_file)
      file.create(running_file)
      lastDir <- basename(dir)
      
      # Assuming ProcessRequest.R is in the same directory as this script
      system(paste("Rscript --verbose ProcessRequest.R", shQuote(lastDir)))
      
      # Remove running.tmp once done
      file.remove(running_file)
    }
  } else {
    cat("request.rds file not found in", dir, "\n")
  }
}

