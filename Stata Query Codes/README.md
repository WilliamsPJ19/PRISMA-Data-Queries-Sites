# Stata Query Codes 
## Description
This folder contains all PRiSMA Stata codes uploaded by GW team that will be shared and used by sites. We ask that each site please **create their own fork of this repository** and upload their code and output there. 

#### :pushpin: *Updated on 21 Apr 2023:*
##### *1. Added PRiSMA-MNH-Data-Dictionary-Repository-V.2.3-MAR272023.xlsx*
##### *2. Added 01_CoreVar_PRiSMA_Dataquery-MAR272023.do to reflect updates in data dictionary.*
##### *3. Added 02_DupID_PRiSMA_Dataquery_APR212023.do*

## How to Setup Your Files Before Running the Stata Codes: 
  #### 1. Create a folder called "data" - this is where all of the raw .csv files should be stored 
  #### 2. Create a folder called "dictionary" - this is where the data dictionary should be stored. 
  #### 3. Create a folder called "query" - this folder is where all the exported reports will be stored. 
  
## File Structure
**\. `01_CoreVar_PRiSMA_Dataquery-MAR272023.do`** This code is developed to be run by site data managers on a **daily** basis
   - Current version date: MAR272023
   - input: All MNH raw data
   - output: log file with missing core variables 
   - timing: daily  
   - function: 
     - Checks for missing core variables in the dataset 
     #### :pushpin: *includes code for all forms*

**\. `02_DupID_PRiSMA_Dataquery_APR262023.do`** This code is developed to be run by site data managers on a **daily** basis
   - Current version date: APR262023 
   - input: All MNH raw data
   - output: excel file with all duplicate screening IDs, mom IDs, and infant IDs
   - timing: daily  
   - function: 
     - Checks for duplicate IDs 
     - Check all enrolled woman has enrollment form
     #### :pushpin: *includes code for all forms*
     


**Example output for core variable checks** 
   - *Core variable query_20230426.pdf*
   - *Duplicate query_20230426.xlsx*
