# PRiSMA Data Cleaning Codes
This repository contains all PRiSMA codes that will be used internally and by sites. Right now this folder holds all of the query codes 
#### :pushpin: *Last updated on 13 March 2023*

## File Structure: 
* The query codes in this repository follow a similar naming structure as Synapse with the main folder being the date of upload in yyyy-mm-dd format 
* The code will export data to two folders:
  1\. data: where wide and long data will be stored 
  2\. queries: where each query output from the codes will be stored
* An example of the overall naming structure compatible with the codes is displayed below -
  - ### Main folder: 2023-04-23
  	- ### Sub folder: data
  	- ### Sub folder: queries
 
     
## Queries Included: 
**1\. `00_DataImport_PRiSMA4a_MatDataquery.R`** 
   - input: All MNH raw data
   - output:
     - Two .RData files of the data - wide and long 
   - function: 
     - Import raw data from sites uploaded to synapse 
				
**2\. `01_CoreVar_PRiSMA4a_Mat_Dataquery.R`** 
   - input: Wide data (all raw .csv files)
   - output:
     - One .rda file with all missing or extra variable names 
   - function: 
     - Check to make sure all variables exist in the data and match data dictionary formatting 

**3\. `02_DupID_PRiSMA4a_Mat_Dataquery.R`** 
   - input: Wide data (all raw .csv files)
   - output:
     - One .rda file with all duplicate IDs 
     - One .rda file with all MomIDs missing enrollment form
   - function: 
     - Identify any duplicate IDs 
     - Idenity any MomIDs that are in the study (have forms MNH03-MNH25) but are missing an enrollment form (MNH02) 


**4\. `03_OutRange_PRiSMA4a_Mat_Dataquery.R`** 
   - input: Long data 
   - output:
     - One .rda file with all out of range values 
   - function: 
     - Check for any out of range values  

**5\. `04_VisitType_PRiSMA4a_Dataquery.R`** 
   - input: Wide data (all raw .csv files)
   - output:
     - One .rda file with all mismatched visit types 
   - function: 
     - Check for visit types that do not match the PRISMA window and gestational age at time of visit 

**6\. `05_DataExport_PRiSMA4a_Dataquery.R`** 
   - input: All .rds files 
   - output:
     - Excel sheet with a full query report 
   - function: 
     - Merge all queries together and assign query ID 
