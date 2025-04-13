# Project Overview

This repository contains all data and code related to the processing, analysis, and visualization of experimental results from "Scrambling the Advantage:
A Visual Search Study of Threat Detection and Face Perception" by Linka Mitome, Aniek Peters, and Tim Kramer.

## Folder Structure

### `all_data/`
This folder contains all raw experimental data extracted from PsychoPy.  
- Each `.csv` file corresponds to a single participant's responses.  
- These files are read and combined by scripts in the `all_data_processing_statics/` folder.

## Files
### all_data_processing_statics.R
This R file contains the following:
- Data cleaning and manipulation  
- Statistical analysis  
- Visualization of results  

### model_results.csv 
This CSV file contains the output of the mixed linear-effects model created in all_data_processing_statics.R

### questionnaire_data.csv
This CSV files contains all written responses to the open-ended question asked at end of the experiment.

