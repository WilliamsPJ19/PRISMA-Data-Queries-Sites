# Stata Query Codes 
## Description
This folder contains all PRiSMA Stata codes uploaded by GW team that will be shared and used by sites. We ask that each site please **create their own fork of this repository** and upload their code and output there. 

#### :pushpin: *Updated on 1 Feb 2023:*
##### *1. Added PRiSMA1b_core_variables_check-OCT182022.do and PRiSMA-MNH-Data-Dictionary-Repository_V.2.2-FEB012023_varlist.xlsx*
##### *2. Revised original name of PRiSMA1b_core_variables_check.do to PRiSMA1b_core_variables_check_SEP292022.do to indicate this do file is for older data dicitonary: PRiSMA-MNH-Data-Dictionary-Repository_V.2.0-SEP222022.xlsx.*

## File Structure
**1a\. `PRiSMA1a_daily_duplicates_check.do`** This code is developed to be run by site data managers on a **daily** basis
   - Current version date: NOV012022 
   - input: All MNH raw data
   - output: excel file with all duplicate screening IDs, mom IDs, and infant IDs
   - timing: daily  
   - function: 
     - Checks for duplicate IDs 
     - Check to make sure all women enrolled in study are were also screened
     #### :pushpin: *includes code for all forms*
     
**1b\. `PRiSMA1b_core_variables_check.do`** This code is developed to be run by site data managers on a **daily** basis
   - Current version date: FEB012023
   - input: All MNH raw data
   - output: log file with missing core variables 
   - timing: daily  
   - function: 
     - Checks for missing core variables in the dataset 
     #### :pushpin: *includes code for all forms*
