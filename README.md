# PRiSMA_Data_Codes
## Description
This repository contains all PRiSMA codes uploaded by GWU team that will be shared and used by sites. We ask that each site please **create their own fork of this repository** and upload their code and output there. 

#### :pushpin: *Updated on 09/27 -- Updated PRiSMA1_datachecks_MNH00-06_v1.1_SEP272022.do* 

## File Structure
**1\. `PRiSMA1_datachecks_MNH00-06_v1.1_SEP272022.do`** This code is developed to be run by site data managers on a **daily** basis
   - Current version: v1.1-SEP272022 
      - Updated to reflect updated MNH01 CRF 
   - input: All MNH raw data
   - timing: daily  
   - function: 
     - Checks variables whose values are outside of established boundaries
       - Categorical variables have values specified by data dictionary
       - Continuous variables are within ranges specified by data dictionary
     #### :pushpin: *includes code for data checks for MNH00-MNH06*
