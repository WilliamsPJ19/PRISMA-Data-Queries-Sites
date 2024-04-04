# PRISMA Data Query Codes
This repository contains all PRISMA codes that will be used internally and by sites. Right now this folder holds all of the query codes 
#### :pushpin: *Last updated on 04 April 2024*

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
**1\. `00_DataImport_MatDataquery.R`** 
   - input: All MNH raw data
   - output:
     - Two .RData files of the data - wide and long 
   - function: 
     - Import raw data from sites uploaded to synapse 
				
**2\. `01_CoreVar_Dataquery.R`** 
   - input: Wide data (all raw .csv files)
   - output:
     - One .rda file with all missing or extra variable names 
   - function: 
     - Check to make sure all variables exist in the data and match data dictionary formatting 

**3\. `02_DupID_Dataquery.R`** 
   - input: Wide data (all raw .csv files)
   - output:
     - One .rda file with all duplicate IDs 
     - One .rda file with all MomIDs missing enrollment form
   - function: 
     - Identify any duplicate IDs 
     - Idenity any MomIDs that are in the study (have forms MNH03-MNH25) but are missing an enrollment form (MNH02) 


**4\. `03_OutRange_Dataquery.R`** 
   - input: Long data 
   - output:
     - One .rda file with all out of range values 
   - function: 
     - Check for any out of range values  

**5\. `04_VisitType_Dataquery.R`** 
   - input: Wide data (all raw .csv files)
   - output:
     - One .rda file with all mismatched visit types to include in query report tab
     - One .rda file with all mismatched visit types + additional information to include as an extra tab in query report
     - One .rda file with all instances of visit types occurring on the same day
   - function: 
     - Check for visit types that do not match the PRISMA window and gestational age at time of visit

**5\. `05_MissingEnrollCrit_Dataquery.R`** 
   - input: Wide data (all raw .csv files) and Long data
   - output:
     - One .rda file with all MOMIDs that do not meet enrollment criteria 
   - function: 
     - Confirm all enrolled particpants meet our enrollment criteria as in MNH02.
     - Confirm all participants who do not meet the pre-screening criteria have reported at least one ineligibility criteria.
     - Flag all particpants with GA>48wks and are missing IPC (or closeout form). 

**6\. `07_EddGA_Dataquery.R`** 
   - input: Wide data (all raw .csv files) and Long data
   - output:
     - One .rda file with all MOMIDs that have discrepancies between ultrasound reported values and LMP
   - function: 
     - Identifies the differences between Ultrasound reported values and LMP (last menstral period) reported values.

**7\. `07_SGA_Dataquery.R`** 
   - input: Wide data (all raw .csv files) and Long data
   - output:
     - One .rda file with all MOMIDs and INFANTIDs that have SGA percentile <0.5 or >99.5 by intergrowth21 standards
   - function: 
     - Identifies infants with size for gestational age with SGA percentile <0.5 or >99.5 by intergrowth21 standard

**8\. `08_InfantDeadThenAlive_Dataquery.R`** 
   - input: Wide data (all raw .csv files) and Long data
   - output:
     - One .rda file with all INFANTIDs with a reported death or stillbirth with a subsequent visit where the vital status was reported as alive. 
   - function: 
     - Identifies all infants with an invalid visit following a reported death. 

**9\. `DataExport_Dataquery.R`** 
   - input: All .rds files 
   - output:
     - Excel sheet with a full query report 
   - function: 
     - Merge all queries together and assign query ID
    
**Documents in this repository:** 
   - Current data dictionary (v2.4) (filename: PRiSMA-MNH-Data-Dictionary-Repository-V.2.4-NOV202023_queries.xlsx)
   - Archive data dictionary (v2.3) (filename: PRiSMA-MNH-Data-Dictionary-Repository-V.2.3-MAR272023.xlsx)
   - Excel file with fetal biometry ranges. This is required for the out of range codes (filename: fetal_biometry_range.xlsx)
   - PRISMA Query Report Template (filename: PRISMA-Query-Template-07Sept2023.xlsx)
   - PRISMA Non-Query Template (filename: PRISMA-Non-Queries-template-07Sept2023.xlsx)
