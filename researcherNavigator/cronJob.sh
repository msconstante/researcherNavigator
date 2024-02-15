#!/bin/bash

# Get variable from the command line
# This is the main directory to start at
maindir=$1
output_dir="$1/output"
echo "Processing subdirectories in $output_dir"

# Cycle through all the subdirectories in a directory and find the ones that have a 'Request.rds' file
find "$output_dir" -mindepth 1 -maxdepth 1 -type d | while read -r dir; do
    echo "Checking directory: $dir"
    if [ -f "$dir/request.rds" ]; then
        echo "request.rds file found in $dir"
        # If results.rds exists, move to the next folder
        if [ -f "$dir/results.rds" ]; then
            echo "Results file exists in $dir"
            continue
        fi
        # Check if a running.tmp file exists
        if [ -f "$dir/running.tmp" ]; then
            echo "Running file exists in $dir"
            continue
        else
            # Call the rscript 'ProcessRequest.R with dir as code'
            echo "Processing $dir"
            # Create a running.tmp file
            touch "$dir/running.tmp"
            #extract the name of the last directory in $dir
            lastDir=$(basename $dir)
            
            # Assuming ProcessRequest.R is in the same directory as this script
            Rscript --verbose ProcessRequest.R "$lastDir"
            # Remove running.tmp once done
            rm "$dir/running.tmp"
        fi
    else
        echo "request.rds file not found in $dir"
    fi
done
