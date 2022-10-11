************************************************
*PRiSMA Data Checks - MNH00, MNH02-MNH06  //will add MNH01 once it's finalized


*Change working directory
cd "CHANGE by user"

*Current date - will be used for duplicate filename
local today : display date(c(current_date), "DMY")

di `today'

*Start date of new data collection (will be different for each site)
local start_date: display date("01/08/2022", "DMY") // make into macro to use over multiple forms

di `start_date'

*************************************************
*MNH00 - Pre-Screening Form
*************************************************

/*If MOMID AND PREGID were not available for pre-screening form, then remove them in MNH00 and replace with SCRNID to run*/

import delimited using "MNH00.CSV", clear 

************MNH00 section A************

	**Screening ID		
		***Checking duplicates - should we be excepting duplicates within data sets?
		bysort SCRNID:  gen dup_SCRNID = cond(_N==1,0,_n) //if no duplicates exist then value is 0 but if duplicates exist value is >=1
		
			***Listing duplicates of SCRNID
			list SCRNID if dup_SCRNID>0 
		
	**MOM ID & Pregnancy ID		
		*Checking duplicates - we might expect duplicates of MOMID if she is enrolled with multiple pregnancies but MOMID + PREGNANCYID should be unique
		bysort MOMID PREGID:  gen dup_MOMID = cond(_N==1,0,_n) //if no duplicates exist then value is 0 but if duplicates exist value is >=1		
		***Listing duplicates of MOMID & PREGID
			list MOMID PREGID if dup_MOMID>0 
	
	**SCRN_OBSTDAT	
		***Converting string dates to date format to check if the format is okay to convert to right format
tab SCRN_OBSSTDAT, missing //check if date is missing, or in a format we cannot extract date information
		foreach var in SCRN_OBSSTDAT  {
			gen `var'_temp=`var' //extract date information from Date and time format, different date format requires different code, revise accordingly
			format `var'_temp  %tddd/nn/ccyy
			drop `var'
			rename `var'_temp `var'
			}
						
	**SCRN_OBSLOC		
		***Converting from string to numeric -- will run even if already numeric
		destring SCRN_OBSLOC, replace		
		***Listing observations with values out of range (1,2,88)
		list    SCRN_OBSLOC if (SCRN_OBSLOC!=1 & SCRN_OBSLOC!=2 & SCRN_OBSLOC!=88 & SCRN_OBSLOC!=.)
			
	**PREGNANT_IEORRES & KNOWN_DOBYN_SCORRES & SCHOOL_SCORRES & EGA_LT25_IEORRES & AGE_IEORRES
	 *CATCHMENT_IEORRES & OTHR_IEORRES 	
		***Converting from string to numeric -- will run even if already numeric
		foreach var in PREGNANT_IEORRES KNOWN_DOBYN_SCORRES SCHOOL_SCORRES EGA_LT25_IEORRES AGE_IEORRES ///
					   CATCHMENT_IEORRES OTHR_IEORRES {
		destring `var', replace
		
		***Listing observations with values out of range (0,1)
		list MOMID PREGID `var' if (`var'!=1  & `var'!=0 & `var'!=.)
		}

	**BRTHDAT
		***Converting string dates to date format to check if the format is okay to convert to right format
		foreach var in BRTHDAT  {
			gen `var'_2=`var' //extract date information from Date and time format, revise accordingly
			format `var'_2  %tddd/nn/ccyy
			}
			
	**ESTIMATED_AGE	
		***Converting from string to numeric -- will run even if already numeric
		destring ESTIMATED_AGE, replace
		
		***Listing observations with values out of range (15-50)
		list ESTIMATED_AGE if ((ESTIMATED_AGE<15 | ESTIMATED_AGE>50) & ESTIMATED_AGE!=.)			
			
	**SCHOOL_YRS_SCORRES
		***Converting from string to numeric -- will run even if already numeric
		destring SCHOOL_YRS_SCORRES, replace
		
		**listing observations with values out of range (0-20?) 
		list SCHOOL_YRS_SCORRES if ((SCHOOL_YRS_SCORRES<0 | SCHOOL_YRS_SCORRES>20) & SCHOOL_YRS_SCORRES!=.)		

	**OTHR_REASON_IEORRES
	
		***Converting from string to numeric -- will run even if already numeric
		destring OTHR_REASON_IEORRES, replace
		
		***List observations with values out of range
		list MOMID PREGID OTHR_REASON_IEORRES if ((OTHR_REASON_IEORRES<1 | OTHR_REASON_IEORRES>8) & OTHR_REASON_IEORRES!=88 & OTHR_REASON_IEORRES!=.)

************MNH00 section B************			

*CON_YN_DSDECOD & CON_LAR_YN_DSDECOD & ASSNT_YN_DSDECOD & CON_WITNESS_SIGNATURE_PRYN
		***Converting from string to numeric -- will run even if already numeric
		foreach var in CON_YN_DSDECOD CON_LAR_YN_DSDECOD ASSNT_YN_DSDECOD ///
		CON_WITNESS_SIGNATURE_PRYN {
		destring `var', replace		
		***Listing observations with values out of range (0,1)
		list MOMID PREGID `var' if (`var'!=1  & `var'!=0 & `var'!=.)
		}
		
*CON_DSSTDAT CON_LAR_SIGNDAT ASSNT_DSSTDAT CON_WITNESS_SIGNDAT
		***Converting string dates to date format to check if the format is okay to convert to right format
		foreach var in CON_DSSTDAT CON_LAR_SIGNDAT ASSNT_DSSTDAT CON_WITNESS_SIGNDAT  {
			gen `var'_2=`var' //extract date information from Date and time format, revise accordingly
			format `var'_2  %tddd/nn/ccyy
			}

************MNH00 section C************			

*CON_FUTURE_RES_DSDECOD & CON_SAMPLES_DSDECOD & FUT_CONTACT_DSDECOD
		***Converting from string to numeric -- will run even if already numeric
		foreach var in CON_FUTURE_RES_DSDECOD CON_SAMPLES_DSDECOD ///
FUT_CONTACT_DSDECOD {
		destring `var', replace		
		***Listing observations with values out of range (0,1)
		list MOMID PREGID `var' if (`var'!=1  & `var'!=0 & `var'!=.)
		}

************MNH00 section D************			

*PRV_ENROLL_YN & SURV_STUDY_YN_SCORRES
		***Converting from string to numeric -- will run even if already numeric
		foreach var in PRV_ENROLL_YN SURV_STUDY_YN_SCORRES {
		destring `var', replace	
		***Listing observations with values out of range (0,1)
		list MOMID PREGID `var' if (`var'!=1  & `var'!=0 & `var'!=.)
		}

************MNH00 section F************		
   ******Categorical and continues variable: Converting from string to numeric
         *COYN_MNH02
destring COYN_MNH00, replace
		
   ******Categorical variable: Listing observations with values out of range (1,0)	
         *COYN_MNH02
list MOMID PREGID COYN_MNH00 if (COYN_MNH00!=1 & COYN_MNH00!=0 & COYN_MNH00!=.)

   ******Date variable: Converting to date format and list out of range 
	   *FORMCOMPLDAT_MNH00		
		  ***Converting string date to date format
tab FORMCOMPLDAT_MNH00	
		foreach var in FORMCOMPLDAT_MNH00  {
			gen `var'_2=`var' //extract date information from Date and time format, revise accordingly
			format `var'_2  %tddd/nn/ccyy
			}		
			
/*Variables that are  text 
SCRN_FAC_SPFY_OBSLOC
SCRN_OTHR_SPFY_OBSLOC
OTHR_SPFY_IEORRES 
PRV_MOMID
HDSS_SUBJID
COVAL_MNH00
FORMCOMPLID_MNH00
*/


/*Alexa's Notes
	* Do we expect duplicates for screening ID?
	* Should we check with each site how SKIP patterns will look in the data? 
		In Tangerine, Kenya data had "SKIPPED" which caused numeric variables to be string until "SKIPPED" was removed
	* What is the best organization for this? By Variable? Or by type of coding? Need to think how a log file will look
	* Do we need to be checking string variables for "OTHER" - I personally don't think so
	* I wonder how we should be checking skip patterns of PREGNANT_IEORRES? 
	* check missing values..... 
*/


*************************************************
*MNH02 - Enrollment Status Form
*************************************************

import delimited using "MNH02.CSV", clear 

************MNH02 Section A************

   ******ID variable: Checking duplicates- Will we have duplicates of SCRNID in data?   
      *Screening ID
	     ***generate variable for duplicate checking
bysort SCRNID:  gen dup_SCRNID = cond(_N==1,0,_n) //if no duplicates exist then value is 0 but if duplicates exist value is >=1
         ***list duplicates-
list SCRNID if dup_SCRNID>0 

	 *SCRN_OBSSTDAT	
		***Converting string dates to date format to check if the format is okay to convert to right format
		tab SCRN_OBSSTDAT, missing
		foreach var in SCRN_OBSSTDAT  {
			gen `var'_temp=`var' //extract date information from Date and time format, revise accordingly
			format `var'_temp  %tddd/nn/ccyy
			drop `var'
			rename `var'_temp `var'
			}
			
   ******Categorical and continues variable: Converting from string to numeric 
      *SCRN_OBSLOC AGE_IEORRES PC_IEORRES CATCHMENT_IEORRES CATCH_REMAIN_IEORRES CONSENT_IEORRES
foreach var of varlist SCRN_OBSLOC AGE_IEORRES PC_IEORRES CATCHMENT_IEORRES CATCH_REMAIN_IEORRES CONSENT_IEORRES {
destring `var', replace
            } 
			
   ******Categorical variable: Listing observations with values out of range (1,2,88)
      *SCRN_OBSLOC 
list MOMID PREGID SCRN_OBSSTDAT SCRN_OBSLOC if (SCRN_OBSLOC!=1 & SCRN_OBSLOC!=2 & SCRN_OBSLOC!=88 & SCRN_OBSLOC!=.)

	  
   ******Categorical variable: Listing observations with values out of range (1,0)	  
      *AGE_IEORRES PC_IEORRES CATCHMENT_IEORRES CATCH_REMAIN_IEORRES CONSENT_IEORRES
foreach var of varlist AGE_IEORRES PC_IEORRES CATCHMENT_IEORRES CATCH_REMAIN_IEORRES CONSENT_IEORRES {
              list MOMID PREGID SCRN_OBSSTDAT `var' if (`var'!= 0 & `var'!= 1 & `var'!= .)
	}			
					
************MNH02 Section B************
   ******ID variable:					
	*MOM ID & Pregnancy ID
	   ***Checking duplicates 
bysort MOMID PREGID:  gen dup_MOMID = cond(_N==1,0,_n) //if no duplicates exist then value is 0 but if duplicates exist value is >=1
	   ***Listing duplicates of MOMID & PREGID
list MOMID PREGID if dup_MOMID>0 
	

************MNH02 Section D************
   
   ******Categorical and continues variable: Converting from string to numeric 
      *ANC_PREENROLL_YN ANC_PREENROLL_*(1-4)   
foreach var of varlist ANC_PREENROLL_YN ANC_PREENROLL_* {
destring `var', replace
   }
	
   ******Categorical variable: Listing observations with values out of range (1,0)	
      *ANC_PREENROLL_YN 
foreach var of varlist ANC_PREENROLL_YN {
                      list MOMID PREGID SCRN_OBSSTDAT `var' if (`var'!=1 & `var'!=0 & `var'!=.)
  }	
	
   ******Categorical variable: Listing observations with values out of range (1,0,77)   
foreach var of varlist ANC_PREENROLL_1 {
   list MOMID PREGID SCRN_OBSSTDAT ANC_PREENROLL_1 if (ANC_PREENROLL_1!=1 ///
   & ANC_PREENROLL_1!=0 & ANC_PREENROLL_1!=.)
   }
   //ANC_PREENROLL_1 only has 1,0 options
   
      *ANC_PREENROLL_*(1-4) ANC_PREENROLLDAT_*(1-4)                
foreach var of varlist ANC_PREENROLL_2 ANC_PREENROLL_3 ANC_PREENROLL_4 {
                      list MOMID PREGID SCRN_OBSSTDAT `var' if (`var'!=1 ///
					  & `var'!=0 & `var'!=77 & `var'!=.)
   }
   
   ******Date variable: Converting to date format and list out of range   
      *ANC_PREENROLLDAT_* (1-4)   
		***Converting string dates to date format to check if the format is okay to convert to right format
foreach var in ANC_PREENROLLDAT_*  {
   gen `var'_temp=`var' //extract date information from Date and time format, revise accordingly
   format `var'_temp  %tddd/nn/ccyy
   drop `var'
   rename `var'_temp `var'
   }

************MNH02 Section E************
   ******Categorical and continues variable: Converting from string to numeric
         *COYN_MNH02
destring COYN_MNH02, replace
		
   ******Categorical variable: Listing observations with values out of range (1,0)	
         *COYN_MNH02
list MOMID PREGID SCRN_OBSSTDAT COYN_MNH02 if ((COYN_MNH02!=1 ///
& COYN_MNH02!=0 & COYN_MNH02!=.) & ANC_PREENROLL_YN==1)

   ******Date variable: Converting to date format and list out of range 
	   *COVAL_MNH02		
		  ***Converting string date to date format
tab COVAL_MNH02, missing
gen COVAL_MNH02_temp=date(COVAL_MNH02, "DMY") // all dates should be in Day-Month-Year Format
format COVAL_MNH02_temp %d
drop COVAL_MNH02
rename COVAL_MNH02_temp COVAL_MNH02		

/*Variables that are  text
SCRN_FAC_SPFY_OBSLOC
SCRN_OTHR_SPFY_OBSLOC
COVAL_MNH02
FORMCOMPLID_MNH02*/


/*Xiaoyan's Notes
To do: add missing value checking after we have test data
To do: check duplicates within forms. Similarity between cases.
To do: check length of time to complete the form? Is it possible?
To do: check logic errors in multiple questions. N/A, DK, No vs any selection
*/


*************************************************
*MNH03 - Sociodemographic Form 
*************************************************
* Section A. Introduction
import delimited using "MNH03.CSV", clear 

**MOM ID & PREGNANCY ID 
** Checking duplicates // we should not expect any duplicate IDs 

	bysort MOMID PREGID: gen dup_MOMID = cond(_N==1,0,_n)// if there is not a duplicate, then dup_MOMID will be 0; if duplicate, it will be >1

** Listing duplicates of MOMID 
	
	list MOMID PREGID if dup_MOMID>0 //if there are any duplicates here we need to remove them 

**SCREENING DATE 
	
***Convert string date to date format 
	
gen SD_OBSSTDAT_2 = date(SD_OBSSTDAT, "DMY")// all dates should be dmy
format SD_OBSSTDAT_2 %d
drop SD_OBSSTDAT
rename SD_OBSSTDAT_2 SD_OBSSTDAT


**SCREENING LOCATION 
*** Converting from string to numeric 
 
destring SD_OBSLOC, replace 

*** Check ranges (values should either be 1,2, or 88) 
	
list MOMID PREGID SD_OBSLOC if (SD_OBSLOC!=1 & SD_OBSLOC!=2 & SD_OBSLOC!=88 & SD_OBSLOC!=.)

** Section B. General Information
*** Converting from string to numeric 
foreach x of varlist MARITAL_SCORRES MARITAL_AGE RELIGION_SCORRES CETHNIC{
destring `x', replace
} 

*** Check ranges 
	
list MOMID MARITAL_SCORRES if ((MARITAL_SCORRES<1 | MARITAL_SCORRES>5) & MARITAL_SCORRES!=.)// check for values outside range 1-5
list MOMID MARITAL_AGE if ((MARITAL_AGE<15 | MARITAL_AGE>50) & MARITAL_AGE!=.)// check for values outside range 15-50
list MOMID PREGID RELIGION_SCORRES if (RELIGION_SCORRES!=1 & RELIGION_SCORRES!=2 & RELIGION_SCORRES!=3 & RELIGION_SCORRES!=4 & RELIGION_SCORRES!=5 & RELIGION_SCORRES!=88 & RELIGION_SCORRES!=.)
list MOMID PREGID CETHNIC if ((RELIGION_SCORRES<1 | RELIGION_SCORRES>12) & RELIGION_SCORRES!=88  & RELIGION_SCORRES!=.)

** Section C. Size of household 
*** Converting from string to numeric 
foreach x of varlist HEAD_HH_FCORRES HOUSE_OCC_TOT_FCORRES HOUSE_OCC_LT5_FCORRES HOUSE_OCC_GE5_FCORRES{
destring `x', replace
} 

*** Check ranges 
foreach var of varlist HEAD_HH_FCORRES {
list MOMID PREGID `var' if (`var'!=1 &`var'!=2 & `var'!=3 & `var'!=4 & `var'!=88 & `var'!=.)
}

foreach var of varlist HOUSE_OCC_TOT_FCORRES HOUSE_OCC_LT5_FCORRES HOUSE_OCC_GE5_FCORRES{
list MOMID PREGID `var' if ((`var'<0 |`var'>30) & `var'!=.)
}

* Section D. Household Assets: Drinking Water
*** Converting from string to numeric 
foreach x of varlist H2O_FCORRES H2O_DIST_FCORRES H2O_HOURS_FCORRES H2O_MINS_FCORRES H2O_PREP_FCORRES H2O_PREP_SPFY_FCORRES_*{
destring `x', replace
} 

*** Check ranges

foreach var of varlist H2O_PREP_FCORRES H2O_PREP_SPFY_FCORRES_*{
list MOMID PREGID `var' if (`var'!=1 & `var'=0 & `var'!=.)
}

list MOMID PREGID H2O_FCORRES if ((H2O_FCORRES<1 | H2O_FCORRES>14) & H2O_FCORRES!=88 & H2O_FCORRES!=.)// check for values outside range 1-14,88


list MOMID PREGID H2O_DIST_FCORRES if ((H2O_DIST_FCORRES<1 | H2O_DIST_FCORRES>5) & H2O_DIST_FCORRES!=99 & H2O_DIST_FCORRES!=.)// check for values outside range 1-5,99; 


*Section E. Household Assets: Toilet Facilities
*** Converting from string to numeric 
foreach x of varlist TOILET_FCORRES TOILET_LOC_FCORRES TOILET_SHARE_FCORRES TOILET_SHARE_NUM_FCORRES{
destring `x', replace
} 

*** Check ranges
	
list MOMID PREGID TOILET_FCORRES if ((TOILET_FCORRES<1 | TOILET_FCORRES>12) & TOILET_FCORRES!=88 & TOILET_FCORRES!=.)// check for values outside range 1-12

 
list MOMID PREGID TOILET_LOC_FCORRES if (TOILET_LOC_FCORRES!=1 & TOILET_LOC_FCORRES!=2 & TOILET_LOC_FCORRES!=3 & TOILET_LOC_FCORRES!=88 & TOILET_LOC_FCORRES!=.)
list MOMID PREGID TOILET_SHARE_FCORRES if (TOILET_SHARE_FCORRES!=0 & TOILET_SHARE_FCORRES!=1 & TOILET_SHARE_FCORRES!=.)
list MOMID PREGID TOILET_SHARE_NUM_FCORRES if (TOILET_SHARE_NUM_FCORRES!=1 & TOILET_SHARE_NUM_FCORRES!=2 & TOILET_SHARE_NUM_FCORRES!=3 & TOILET_SHARE_NUM_FCORRES!=.)

* Section F. Household Assets: Building Structure and Materials 
*** Converting from string to numeric 
foreach x of varlist EXT_WALL_FCORRES FLOOR_FCORRES ROOF_FCORRES{
destring `x', replace
} 

*** Check ranges (EXT_WALL_FCORRES values should either be 1-17 or 88, FLOOR_FCORRES values should be between 1-9 or 88, ROOF_FCORRES values should be either 1-13 or 88) 
	
list MOMID PREGID EXT_WALL_FCORRES if ((EXT_WALL_FCORRES<1 | EXT_WALL_FCORRES>17) & EXT_WALL_FCORRES!=88 & EXT_WALL_FCORRES!=.)
list MOMID PREGID FLOOR_FCORRES if ((FLOOR_FCORRES<1 | FLOOR_FCORRES>9) & FLOOR_FCORRES!=88 & FLOOR_FCORRES!=.)
list MOMID PREGID ROOF_FCORRES if ((ROOF_FCORRES<1 | ROOF_FCORRES>13) & ROOF_FCORRES!=88 & ROOF_FCORRES!=.)

*Section G. Household Assets: Amenities 
*** Converting from string to numeric 
foreach x of varlist ELECTRICITY_FCORRES SOLAR_FCORRES INTERNET_FCORRES LANDLINE_FCORRES MOBILE_FCORRES MOBILE_NUM_FCORRES MOBILE_ACCESS_FCORRES RADIO_FCORRES RADIO_NUM_FCORRES TV_FCORRES TV_NUM_FCORRES FRIDGE_FCORRES FRIDGE_NUM_FCORRES COMPUTER_FCORRES COMPUTER_NUM_FCORRES WATCH_FCORRES WATCH_NUM_FCORRES BIKE_FCORRES BIKE_NUM_FCORRES MOTOCYCLE_FCORRES MOTORCYCLE_NUM_FCORRES CAR_FCORRES CAR_NUM_FCORRES BOAT_FCORRES BOAT_NUM_FCORRES CART_FCORRES CART_NUM_FCORRES PLOUGH_FCORRES PLOUGH_NUM_FCORRES FOAM_MATT_FCORRES FOAM_MATT_NUM_FCORRES STRAW_MATT_FCORRES STRAW_MATT_NUM_FCORRES SPRING_MATT_FCORRES SPRING_MATT_NUM_FCORRES SOFA_FCORRES SOFA_NUM_FCORRES LANTERN_FCORRES LANTERN_NUM_FCORRES SEW_FCORRES SEW_NUM_FCORRES WASH_FCORRES WASH_NUM_FCORRES BLENDER_FCORRES BLENDER_NUM_FCORRES MOSQUITO_NET_FCORRES MOSQUITO_NET_NUM_FCORRES TRICYCLES_FCORRES TRICYCLES_NUM_FCORRES TABLES_FCORRES TABLES_NUM_FCORRES CABINETS_FCORRES CABINETS_NUM_FCORRES SAT_DISH_FCORRES SAT_DISH_NUM_FCORRES DVD_CD_FCORRES DVD_CD_FCORRES_NUM_FCORRES AIRCON_FCORRES AIRCON_FCORRES_NUM_FCORRES TRACTOR_FCORRES TRACTOR_FCORRES_NUM_FCORRES OWN_RENT_SCORRES HOUSE_ROOMS_FCORRES HOUSE_ROOM_CHILD_FCORRES{ 
destring `x', replace
} 

*** Check ranges 

foreach var of varlist ELECTRICITY_FCORRES SOLAR_FCORRES INTERNET_FCORRES LANDLINE_FCORRES MOBILE_FCORRES RADIO_FCORRES TV_FCORRES FRIDGE_FCORRES COMPUTER_FCORRES WATCH_FCORRES BIKE_FCORRES MOTOCYCLE_FCORRES CAR_FCORRES BOAT_FCORRES CART_FCORRES PLOUGH_FCORRES FOAM_MATT_FCORRES STRAW_MATT_FCORRES SPRING_MATT_FCORRES SOFA_FCORRES LANTERN_FCORRES SEW_FCORRES WASH_FCORRES BLENDER_FCORRES MOSQUITO_NET_FCORRES TRICYCLES_FCORRES TABLES_FCORRES CABINETS_FCORRES HOUSE_ROOM_CHILD_FCORRES ///
SAT_DISH_FCORRES DVD_CD_FCORRES AIRCON_FCORRES TRACTOR_FCORRES HOUSE_ROOM_CHILD_FCORRES {
list MOMID PREGID `var' if ((`var'!=0 |`var'!=1) & `var'!=.)
}

foreach var of varlist MOBILE_NUM_FCORRES RADIO_NUM_FCORRES TV_NUM_FCORRES FRIDGE_NUM_FCORRES COMPUTER_NUM_FCORRES WATCH_NUM_FCORRES BIKE_NUM_FCORRES MOTORCYCLE_NUM_FCORRES CAR_NUM_FCORRES BOAT_NUM_FCORRES CART_NUM_FCORRES PLOUGH_NUM_FCORRES FOAM_MATT_NUM_FCORRES STRAW_MATT_NUM_FCORRES SPRING_MATT_NUM_FCORRES SOFA_NUM_FCORRES LANTERN_NUM_FCORRES SEW_NUM_FCORRES WASH_NUM_FCORRES BLENDER_NUM_FCORRES MOSQUITO_NET_NUM_FCORRES TRICYCLES_NUM_FCORRES TABLES_NUM_FCORRES CABINETS_NUM_FCORRES SAT_DISH_NUM_FCORRES DVD_CD_FCORRES_NUM_FCORRES AIRCON_FCORRES_NUM_FCORRES TRACTOR_FCORRES_NUM_FCORRES HOUSE_ROOMS_FCORRES{
list MOMID PREGID `var' if ((`var'<1 |`var'>10) & `var'!=.)//check for values outside range 1-10
}

list MOMID PREGID MOBILE_ACCESS_FCORRES if (MOBILE_ACCESS_FCORRES!=1 & MOBILE_ACCESS_FCORRES!=2 & MOBILE_ACCESS_FCORRES!=0 & MOBILE_ACCESS_FCORRES!=.)
list MOMID PREGID OWN_RENT_SCORRES if ((OWN_RENT_SCORRES<1 | OWN_RENT_SCORRES>6) & OWN_RENT_SCORRES!=88 & OWN_RENT_SCORRES!=.)

* Section H. Household Assets: Land and livestock  
*** Converting from string to numeric 
 foreach x of varlist LAND_FCORRES LAND_USE_FCORRES_* LIVESTOCK_FCORRES CATTLE_FCORRES CATTLE_NUM_FCORRES GOAT_FCORRES GOAT_NUM_FCORRES SHEEP_FCORRES SHEEP_NUM_FCORRES POULTRY_FCORRES POULTRY_NUM_FCORRES PIG_FCORRES PIG_NUM_FCORRES DONKEY_FCORRES DONKEY_NUM_FCORRES HORSE_FCORRES HORSE_NUM_FCORRES ANIMAL_OTHR_FCORRES ANIMAL_OTHR_NUM_FCORRES{ 
destring `x', replace
} 

*** Check ranges 
foreach var of varlist LAND_FCORRES LAND_USE_FCORRES_* LIVESTOCK_FCORRES CATTLE_FCORRES GOAT_FCORRES SHEEP_FCORRES POULTRY_FCORRES PIG_FCORRES DONKEY_FCORRES HORSE_FCORRES ANIMAL_OTHR_FCORRES{
list MOMID PREGID `var' if (`var'!=0 & `var'!=1 & `var'!=.)
}

foreach var of varlist CATTLE_NUM_FCORRES GOAT_NUM_FCORRES SHEEP_NUM_FCORRES POULTRY_NUM_FCORRES PIG_NUM_FCORRES DONKEY_NUM_FCORRES HORSE_NUM_FCORRES ANIMAL_OTHR_NUM_FCORRES{
list MOMID PREGID `var' if ((`var'<1 |`var'>100) & `var'!=.)//check for values outside range 1-100
}

*Section I. Occupation
*** Converting from string to numeric 
foreach x of varlist JOB_SCORRES PTR_SCORRES{ 
destring `x', replace
} 
*** Check ranges 
	
list MOMID PREGID JOB_SCORRES if ((JOB_SCORRES<1 | JOB_SCORRES>9) & JOB_SCORRES!=88 & JOB_SCORRES!=77  & JOB_SCORRES!=.)
list MOMID PREGID PTR_SCORRES if ((PTR_SCORRES<1 | PTR_SCORRES>8) & PTR_SCORRES!=88 & PTR_SCORRES!=77 & PTR_SCORRES!=.)

*Section J. Exposure to Household Air Pollution
*** Converting from string to numeric 
foreach x of varlist STOVE_FCORRES STOVE_FUEL_FCORRES_* COOKING_INSIDE_FCORRES COOKING_ROOM_FCORRES COOKING_LOC_FCORRES COOKING_VENT_FCORRES{ 
destring `x', replace
} 
*** Check ranges 
	
foreach var of varlist STOVE_FUEL_FCORRES_* COOKING_INSIDE_FCORRES COOKING_ROOM_FCORRES{
list MOMID PREGID `var' if (`var'!=1 & `var'!=0 & `var'!=.)
}

list MOMID PREGID STOVE_FCORRES if ((STOVE_FCORRES<1 | STOVE_FCORRES>10) & STOVE_FCORRES!=88 & STOVE_FCORRES!=.)
list MOMID PREGID COOKING_LOC_FCORRES if (COOKING_LOC_FCORRES!=1 & COOKING_LOC_FCORRES!=2 & COOKING_LOC_FCORRES!=3 & COOKING_LOC_FCORRES!=.)
list MOMID PREGID COOKING_VENT_FCORRES if (COOKING_VENT_FCORRES!=1 & COOKING_VENT_FCORRES!=0 & COOKING_VENT_FCORRES!=77 & COOKING_VENT_FCORRES!=.)

* Section K. Health behaviors 
*** Converting from string to numeric 
 foreach x of varlist SMOKE_OECOCCUR SMOKE_IN_OECDOSFRQ SMOKE_HHOLD_OECOCCUR SMOKE_HHOLD_IN_OECDOSFRQ CHEW_OECOCCUR CHEW_BNUT_OECOCCUR DRINK_OECOCCUR{ 
destring `x', replace
} 
*** Check ranges
		
foreach var of varlist SMOKE_OECOCCUR SMOKE_HHOLD_OECOCCUR CHEW_OECOCCUR CHEW_BNUT_OECOCCUR{
list MOMID PREGID `var' if ((`var'!=1 |`var'!=0) & `var'!=.)
}

	
foreach var of varlist SMOKE_IN_OECDOSFRQ SMOKE_HHOLD_IN_OECDOSFRQ{
list MOMID PREGID `var' if ((`var'<1 |`var'>5) & `var'!=.)
}
	
list MOMID PREGID DRINK_OECOCCUR if (DRINK_OECOCCUR!=1 & DRINK_OECOCCUR!=0 & DRINK_OECOCCUR!=66 & DRINK_OECOCCUR!=.)

*Section L. Plans for delivery 
*** Converting from string to numeric 
foreach x of varlist PD_BIRTH_OHOLOC PD_DM_SCORRES{ 
destring `x', replace
} 
*** Check ranges
	
list MOMID PREGID PD_BIRTH_OHOLOC if (PD_BIRTH_OHOLOC!=1 & PD_BIRTH_OHOLOC!=2 & PD_BIRTH_OHOLOC!=88 & PD_BIRTH_OHOLOC!=.)
list MOMID PREGID PD_DM_SCORRES if ((PD_DM_SCORRES<1 | PD_DM_SCORRES>5) & PD_DM_SCORRES!=88 & PD_DM_SCORRES!=.)

* Section M. Form Completion
*** Converting from string to numeric 
 
	destring COYN_MNH03, replace

***Convert string date to date format 
	
gen FORMCOMPLDAT_MNH03_2 = date(FORMCOMPLDAT_MNH03, "DMY")// all dates should be dmy
format FORMCOMPLDAT_MNH03_2 %d
drop FORMCOMPLDAT_MNH03
rename FORMCOMPLDAT_MNH03_2 FORMCOMPLDAT_MNH03

*** Check ranges

list MOMID PREGID COYN_MNH03 if (COYN_MNH03!=1 & COYN_MNH03!=0 & COYN_MNH03!=.)

*************************************************
*MNH04 - ANC Clinical Status Form
*************************************************
import delimited using "MNH04.CSV", clear 

*Section A. Introduction
**MOM ID & PREGNANCY ID 	
** Checking duplicates // we should not expect any duplicate IDs 

bysort MOMID PREGID: gen dup_MOMID = cond(_N==1,0,_n)// if there is not a duplicate, then dup_MOMID will be 0; if duplicate, it will be >1

** Listing duplicates of MOMID 
	
list MOMID PREGID if dup_MOMID>0 //if there are any duplicates here we need to remove them 

**Remainder of section A 
** Converting from string to numeric 

foreach x of varlist ANC_OBSLOC TYPE_VISIT VISIT_DSDECOD PRG_DSDECOD FETAL_LOSS_DSDECOD ANC_N_1{
destring `x', replace
} 
	
** converting string to date format 

foreach x of varlist ANC_OBSSTDAT FETAL_LOSS_DSSTDAT{
replace `x' = "" if(`x' =="SKIPPED")
gen date2=date(`x' "DMY")
rename `x' `x'_str
rename date2 `x'
format `x' %d
} 

** check ranges 
list MOMID PREGID ANC_OBSLOC if (ANC_OBSLOC!=1 & ANC_OBSLOC!=2 & ANC_OBSLOC!=88 & ANC_OBSLOC!=.)
list MOMID PREGID TYPE_VISIT if (TYPE_VISIT!=1 & TYPE_VISIT!=2 & TYPE_VISIT!=3 & TYPE_VISIT!=4 & TYPE_VISIT!=5 & TYPE_VISIT!=6 & TYPE_VISIT!=.)
list MOMID PREGID VISIT_DSDECOD if (VISIT_DSDECOD!=1 & VISIT_DSDECOD!=2 & VISIT_DSDECOD!=3 & VISIT_DSDECOD!=4 & VISIT_DSDECOD!=5 & VISIT_DSDECOD!=6 & VISIT_DSDECOD!=7 & VISIT_DSDECOD!=8 & VISIT_DSDECOD!=88 & VISIT_DSDECOD!=99 & VISIT_DSDECOD!=.)
list MOMID PREGID PRG_DSDECOD if (PRG_DSDECOD!=1 & PRG_DSDECOD!=2 & PRG_DSDECOD!=3 & PRG_DSDECOD!=.)
list MOMID PREGID FETAL_LOSS_DSDECOD if (FETAL_LOSS_DSDECOD!1 & FETAL_LOSS_DSDECOD!=2 & FETAL_LOSS_DSDECOD!=3 & FETAL_LOSS_DSDECOD!=.)
list MOMID PREGID ANC_N_1 if (FETAL_LOSS_DSDECOD!1 & FETAL_LOSS_DSDECOD!=0 & FETAL_LOSS_DSDECOD!=.)

*Section B. Medical and Obstetrical History 
** Converting from string to numeric 

foreach x of varlist PH_PREV_RPORRES PH_PREVN_RPORRES PH_LIVE_RPORRES PH_PREV_RPORRES MISCARRIAGE_RPORRES STILLBIRTH_RPORRES HTN_EVER_MHOCCUR HTN_CMOCCUR HIV_EVER_MHOCCUR HIV_MHSTDAT HIV_CMOCCUR SYPH_EVER_MHOCCUR DIABETES_EVER_MHOCCUR CARDIAC_EVER_MHOCCUR CANCER_EVER_MHOCCUR KIDNEY_EVER_MHOCCUR PRETERM_RPORRES POSTTERM_RPORRES GEST_HTN_RPORRES PREECLAMPSIA_RPORRES GEST_DIAB_RPORRES PREMATURE_RUPTURE_RPORRES OBSTR_LABOR_RPORRES UNPL_CESARIAN_PROCCUR INTERUTER_GROWTH_RPORRES LOWBIRTHWT_RPORRES MACROSOMIA_RPORRES OLIGOHYDRAMNIOS_RPORRES APH_RPORRES PPH_RPORRES MALFORM_MHOCCUR CORD_COMP_MHOCCUR BIRTH_TRAUMA_MHOCCUR BIRTH_ASPHYXIA_MHOCCUR BLOOD_DISORDER_MHOCCUR OTHR_COMP_MHOCCUR HTN_CMTRT_* HIV_CMTRT_* APH_COMP_RPTEST_* PPH_COMP_RPORRES_* MALFORM_MHTERM_*{

destring `x', replace

} 

** converting string to date format 
foreach x of varlist HTN_MHSTDAT HIV_MHSTDAT{
replace `x' = "" if(`x' =="SKIPPED")
gen date2=date(`x' "DMY")
rename `x' `x'_str
rename date2 `x'
format `x' %d
} 

** Check ranges **


foreach var of varlist PH_PREV_RPORRES HTN_CMTRT_* HIV_CMTRT_* APH_COMP_RPTEST_* PPH_COMP_RPORRES_* MALFORM_MHTERM_* {
list MOMID PREGID `var' if (`var'!= 0 & `var'!= 1 & `var'!=.)
}

foreach var of varlist MISCARRIAGE_RPORRES STILLBIRTH_RPORRES ///
HTN_EVER_MHOCCUR HTN_CMOCCUR HIV_EVER_MHOCCUR HIV_CMOCCUR SYPH_EVER_MHOCCUR DIABETES_EVER_MHOCCUR CARDIAC_EVER_MHOCCUR CANCER_EVER_MHOCCUR KIDNEY_EVER_MHOCCUR PRETERM_RPORRES POSTTERM_RPORRES GEST_HTN_RPORRES PREECLAMPSIA_RPORRES GEST_DIAB_RPORRES PREMATURE_RUPTURE_RPORRES OBSTR_LABOR_RPORRES UNPL_CESARIAN_PROCCUR INTERUTER_GROWTH_RPORRES LOWBIRTHWT_RPORRES MACROSOMIA_RPORRES OLIGOHYDRAMNIOS_RPORRES APH_RPORRES ///
PPH_RPORRES MALFORM_MHOCCUR CORD_COMP_MHOCCUR BIRTH_TRAUMA_MHOCCUR BIRTH_ASPHYXIA_MHOCCUR BLOOD_DISORDER_MHOCCUR OTHR_COMP_MHOCCUR{
list MOMID PREGID `var' if (`var'!= 0 & `var'!= 1 & `var'!= 99 & `var'!=.)
}

foreach var of varlist PH_PREVN_RPORRES PH_LIVE_RPORRES PH_OTH_RPORRES MISCARRIAGE_CT_RPORRES STILLBIRTH_CT_RPORRES {
list MOMID PREGID `var' if ((`var'<1 | `var'> 10) & `var'!=.))// values outside range 1-10
}

* Section C. Family Planning 
** Converting from string to numeric 

foreach x of varlist CURRPREG_FP_* CURRPREG_FP_REAS_* CURREPREGN_DESIRE_YN {
destring `x', replace
} 

** Check ranges 

foreach var of varlist CURRPREG_FP_* CURRPREG_FP_REAS_* CURREPREGN_DESIRE_YN {
list MOMID PREGID `var' if (`var'!= 0 & `var'!= 1 & `var'!=.)
}


*Section D. Current Clinical Status 
** Converting from string to numeric 

foreach x of varlist HPD_MHOCCUR HPD_MHTERM HPD_OECTRT_* PTL_MHOCCUR PTL_OECTRT_* APH_CEOCCUR HIV_MHOCCUR SYPH_MHOCCUR SYPH_CMTRT_* ///
SYPH_CMDOSFRQ~~~~~~ OTHR_STI_MHOCCUR GONORRHEA_MHOCCUR GONORRHEA_CMTRT_* CHLAMYDIA_MHOCCUR CHLAMYDIA_CMTRT_* CHANC_MHOCCUR CHANC_CMTRT_* GENULCER_MHOCCUR STI_OTHR_MHOCCUR STI_OTHR_CMOCCUR MALARIA_EVER_MHOCCUR MALARIA_EVER_PEMETHOD MALARIA_EVER_CMTRT_* TB_MHOCCUR TB_CMTRT_* TB_CETERM_* COVID_LBPERF COVID_LBORRES DX_OTHR_PG_MHOCCUR DX_OTHR_CMOCCUR{
destring `x', replace
} 

** converting string to date format 
foreach x of varlist COVID_LBTSTDAT {
replace `x' = "" if(`x' =="SKIPPED")
gen date2=date(`x' "DMY")
rename `x' `x'_str
rename date2 `x'
format `x' %d
} 

*** Check ranges 
*******add range check for SYPH_CMDOSFRQ once we have the range

foreach var of varlist HPD_OECTRT_* PTL_OECTRT_* SYPH_CMTRT_* GONORRHEA_CMTRT_* CHLAMYDIA_CMTRT_* CHANC_CMTRT_* MALARIA_EVER_CMTRT_* TB_CMTRT_* TB_CETERM_* COVID_LBPERF DX_OTHR_PG_MHOCCUR DX_OTHR_CMOCCUR{
list MOMID PREGID `var' if (`var'!= 0 & `var'!= 1 & `var'!=.)
}

foreach var of varlist HPD_MHOCCUR PTL_MHOCCUR APH_CEOCCUR HIV_MHOCCUR SYPH_MHOCCUR OTHR_STI_MHOCCUR GONORRHEA_MHOCCUR CHLAMYDIA_MHOCCUR CHANC_MHOCCUR GENULCER_MHOCCUR STI_OTHR_MHOCCUR STI_OTHR_CMOCCUR MALARIA_EVER_MHOCCUR TB_MHOCCUR{
list MOMID PREGID `var' if (`var'!= 0 & `var'!= 1 & `var'!= 99 & `var'!=.)
}

list MOMID PREGID HPD_MHTERM if (HPD_MHTERM!= 1 & HPD_MHTERM!= 2 & HPD_MHTERM!= 3 & HPD_MHTERM!= 99 & HPD_MHTERM!=.)
list MOMID PREGID MALARIA_EVER_PEMETHOD if (MALARIA_EVER_PEMETHOD!= 1 & MALARIA_EVER_PEMETHOD!= 2 & MALARIA_EVER_PEMETHOD!=.)
list MOMID PREGID COVID_LBORRES if (COVID_LBORRES!= 1 & COVID_LBORRES!= 0 & COVID_LBORRES!= 3 & COVID_LBORRES!= 99 & COVID_LBORRES!=.)

*Section E. Medications and Treatments 
** Converting from string to numeric 
foreach x of varlist TETANUS_CMOCCUR TETANUS_SOURCE TETANUS_GA_WKS_AGE TETANUS_GA_DAYS_AGE COVID_DOSE1_CMOCCUR COVID_DOSE1_CMTRT COVID_DOSE1_CMSTDAT_YN COVID_DOSE1_SOURCE COVID_DOSE2_CMOCCUR COVID_DOSE2_CMTRT COVID_DOSE2_CMSTDAT_YN COVID_DOSE2_SOURCE COVID_BOOST1_CMOCCUR COVID_BOOST1_CMTRT COVID_BOOST1_CMSTDAT_YN COVID_BOOST1_SOURCE FLU_CMOCCUR FLU_SOURCE FLU_GA_WKS_AGE FLU_GA_DAYS_AGE ANTIBX_CMTRT_* ANTIBX_CMINDC IPT_CMOCCUR MALARIA_CMOCCUR FOLIC_ACID_CMOCCUR IRON_CMOCCUR IFA_CMOCCUR CALCIUM_CMOCCUR VITAMIN_A_CMOCCUR ZINC_CMOCCUR MICRONUTRIENT_CMOCCUR ORAL_REHYD_CMOCCUR ANTHELMINTHIC_CMOCCUR ASPIRIN_CMOCCUR ANTIPYRETIC_CMOCCUR RX_OTHER_CMOCCUR HOSP_OHOOCCUR{
destring `x', replace
} 

** converting string to date format 
foreach x of varlist TETANUS_CMSTDAT COVID_DOSE1_CMSTDAT COVID_DOSE2_CMSTDAT COVID_BOOST1_CMSTDAT FLU_CMSTDAT HOSP_OHOSTDAT HOSP_OHOENDAT{
replace `x' = "" if(`x' =="SKIPPED")
gen date2=date(`x' "DMY")
rename `x' `x'_str
rename date2 `x'
format `x' %d
} 

*** Check ranges 

foreach var of varlist TETANUS_SOURCE COVID_DOSE1_CMSTDAT_YN COVID_DOSE2_CMSTDAT_YN COVID_BOOST1_CMSTDAT_YN FLU_SOURCE ANTIBX_CMOCCUR ANTIBX_CMTRT_* RX_OTHER_CMOCCUR HOSP_OHOOCCUR{
list MOMID PREGID `var' if (`var'!= 0 & `var'!= 1 & `var'!=.)
}

foreach var of varlist COVID_DOSE1_CMOCCUR COVID_DOSE2_CMOCCUR COVID_BOOST1_CMOCCUR{
list MOMID PREGID `var' if (`var'!= 0 & `var'!= 1 & `var'!= 77 & `var'!=.)
}

foreach var of varlist IPT_CMOCCUR MALARIA_CMOCCUR FOLIC_ACID_CMOCCUR IRON_CMOCCUR IFA_CMOCCUR CALCIUM_CMOCCUR VITAMIN_A_CMOCCUR ZINC_CMOCCUR MICRONUTRIENT_CMOCCUR ORAL_REHYD_CMOCCUR ANTHELMINTHIC_CMOCCUR ASPIRIN_CMOCCUR ANTIPYRETIC_CMOCCUR FLU_CMOCCUR{
list MOMID PREGID `var' if (`var'!= 0 & `var'!= 1 & `var'!= 99 & `var'!=.)
}

foreach var of varlist TETANUS_GA_WKS_AGE FLU_GA_WKS_AGE{
list MOMID PREGID `var' if ((`var'< 4 | `var'> 40) & `var'!=.)// check for values outside range 4-40
}

foreach var of varlist TETANUS_GA_DAYS_AGE FLU_GA_DAYS_AGE{
list MOMID PREGID `var' if ((`var'<0 | `var'> 6) & `var'!=.)// check for values outside range 0-6
}

foreach var of varlist COVID_DOSE1_SOURCE COVID_DOSE2_SOURCE COVID_BOOST1_SOURCE {
list MOMID PREGID `var' if (`var'!= 1 & `var'!= 2 &`var'!=.)
}

foreach var of varlist COVID_DOSE1_CMTRT COVID_DOSE2_CMTRT COVID_BOOST1_CMTRT{
list MOMID PREGID `var' if ((`var'< 1 | `var'> 9) & `var'!= 88 & `var'!= 99 & `var'!= .)
}

list MOMID PREGID TETANUS_CMOCCUR if (TETANUS_CMOCCUR!= 1 & TETANUS_CMOCCUR!= 0 & TETANUS_CMOCCUR!= 77 & TETANUS_CMOCCUR!= 99 & TETANUS_CMOCCUR!=.)


list MOMID PREGID ANTIBX_CMINDC if ((ANTIBX_CMINDC< 1 | ANTIBX_CMINDC> 8) & ANTIBX_CMINDC!= 88 & ANTIBX_CMINDC!= 99 & ANTIBX_CMINDC!=.)


*Section F. Health Promotion Activities 
** Converting from string to numeric 

foreach x of varlist INSECT_LSTNIGHT_OBSOCCUR NUTR_COUNSEL_OBSOCCUR HEALTH_PROMOTION_OBSOCCUR BIRTH_PLAN_OBSOCCUR BIRTH_OBSLOC {
destring `x', replace
} 

** Check ranges 

foreach var of varlist INSECT_LSTNIGHT_OBSOCCUR NUTR_COUNSEL_OBSOCCUR HEALTH_PROMOTION_OBSOCCUR BIRTH_PLAN_OBSOCCUR{
list MOMID PREGID `var' if (`var'!= 0 & `var'!= 1 & `var'!=.)
}

list MOMID PREGID BIRTH_OBSLOC if (BIRTH_OBSLOC!= 1 & BIRTH_OBSLOC!= 2 & BIRTH_OBSLOC!= 88 & BIRTH_OBSLOC!=.)

* Section G. Maternal Death 
** Converting from string to numeric 

foreach x of varlist PRG_DTH_DSDECOD ACC_DDORRES{
destring `x', replace
} 

** converting string to date format 
gen DTHDAT_2 = date(DTHDAT, "DMY")// all dates should be dmy
format DTHDAT_2 %d
drop DTHDAT
rename DTHDAT_2 DTHDAT

** converting string to time format// ALEXA CHECK FORMAT 
gen DTHTIM_2 = clock(DTHTIM, "hm")// all dates should be hh:mm
format DTHTIM_2 %tcHh:MM//check formatting
drop DTHTIM
rename DTHTIM_2 DTHTIM

** Check ranges 

list MOMID PREGID PRG_DTH_DSDECOD if (PRG_DTH_DSDECOD!= 1 & PRG_DTH_DSDECOD!= 2 & PRG_DTH_DSDECOD!=.)
list MOMID PREGID ACC_DDORRES if (ACC_DDORRES!= 1 & ACC_DDORRES!= 0 & ACC_DDORRES!= 99 & ACC_DDORRES!=.)

* Section H. Form Completion
** Converting from string to numeric 

foreach x of varlist COYN_MNH04 FORMCOMPLID_MNH04{
destring `x', replace
} 

** converting string to date format 
gen FORMCOMPLDAT_MNH04_2 = date(FORMCOMPLDAT_MNH04, "DMY")// all dates should be dmy
format FORMCOMPLDAT_MNH04_2 %d
drop FORMCOMPLDAT_MNH04
rename FORMCOMPLDAT_MNH04_2 FORMCOMPLDAT_MNH04

*** Check ranges 

list MOMID PREGID COYN_MNH04 if (COYN_MNH04!= 1 & COYN_MNH04!= 0 & COYN_MNH04!=.)

*************************************************
*MNH05 - Maternal Anthropometry Form
*************************************************

import delimited using "MNH05.csv", clear

************MNH05 Section A************
   ******ID variable:					
	  *MOMID & PREGID
	     ***Checking duplicates 
bysort MOMID PREGID:  gen dup_MOMID = cond(_N==1,0,_n) //if no duplicates exist then value is 0 but if duplicates exist value is >=1
	     ***Listing duplicates of MOMID & PREGID
list MOMID PREGID if dup_MOMID>0 
	
   ******Date variable: Converting to date format and list out of range
 	  *ANT_PEDAT
         ***Converting string date to date format
			gen ANT_PEDAT_temp=date(ANT_PEDAT, "DMY") // all dates should be in Day-Month-Year Format
			format ANT_PEDAT_temp %d
			drop ANT_PEDAT
			rename ANT_PEDAT_temp ANT_PEDAT   

   ******Categorical and continues variable: Converting from string to numeric 
      *ANT_OBSLOC TYPE_VISIT
foreach var of varlist ANT_OBSLOC TYPE_VISIT {
destring `var', replace
            }
			
   ******Categorical variable: Listing observations with values out of range (1,2,88)
      *ANT_OBSLOC 
list MOMID PREGID ANT_PEDAT ANT_OBSLOC if (ANT_OBSLOC!=1 & ANT_OBSLOC!=2 & ANT_OBSLOC!=88 & ANT_OBSLOC!=.))

	  
   ******Categorical variable: Listing observations with values out of range (1-13)	  
      *ANT_OBSLOC
list MOMID PREGID ANT_PEDAT TYPE_VISIT if ((TYPE_VISIT<1 | TYPE_VISIT>13) & ANT_OBSLOC!=.)	


************MNH05 Section B************
   ******Categorical and continues variable: Converting from string to numeric 
      *HEIGHT_PEPERF HEIGHT_PERES WEIGHT_PEPERF WEIGHT_PERES MUAC_PEPERF MUAC_PERES
foreach var of varlist HEIGHT_PEPERF HEIGHT_PERES WEIGHT_PEPERF WEIGHT_PERES MUAC_PEPERF MUAC_PERES {
destring `var', replace
            }

   ******Categorical variable: Listing observations with values out of range (1,0,77)
      *HEIGHT_PEPERF 
list MOMID PREGID ANT_PEDAT HEIGHT_PEPERF if (HEIGHT_PEPERF!=1 & HEIGHT_PEPERF!=0 & HEIGHT_PEPERF!=77 & HEIGHT_PEPERF!=.)

   ******Categorical variable: Listing observations with values out of range (1,0)	
      *WEIGHT_PEPERF  MUAC_PEPERF
foreach var of varlist WEIGHT_PEPERF  MUAC_PEPERF {
                      list MOMID PREGID ANT_PEDAT `var' if (`var'!=1 & `var'!=0 & `var'!=.)
  }	
/*
   ******Continues variable: Listing observations with values out of range
      *HEIGHT_PEPERF 
	     ***Listing observations with values out of range (x-y)
list MOMID PREGID ANT_PEDAT HEIGHT_PERES if ((HEIGHT_PERES<100 | HEIGHT_PERES>200) & HEIGHT_PEPERF!=.)
      *WEIGHT_PERES 
         ***Listing observations with values out of range (30-150kg?)
list MOMID PREGID ANT_PEDAT WEIGHT_PERES if ((WEIGHT_PERES<30 | WEIGHT_PERES>150) & WEIGHT_PERES!=.)
      *MUAC_PERES 
         ***Listing observations with values out of range (21.9-25.1cm?)
list MOMID PREGID ANT_PEDAT MUAC_PERES if ((MUAC_PERES<21.9 | MUAC_PERES>25.1) & MUAC_PERES!=.)
*/

************MNH05 Section C************
   ******Categorical and continues variable: Converting from string to numeric 
      *COYN_MNH05
foreach var of varlist COYN_MNH05 {
destring `var', replace
            }

   ******Categorical variable: Listing observations with values out of range (1,0)	
      *COYN_MNH05
foreach var of varlist COYN_MNH05 {
                      list MOMID PREGID ANT_PEDAT `var' if (`var'!=1 & `var'!=0 & `var'!=.)
  }	

   ******Date variable: Converting to date format and list out of range   
      *FORMCOMPLDAT_MNH05   
         ***Converting string date to date format   
gen FORMCOMPLDAT_MNH05_temp=date(FORMCOMPLDAT_MNH05, "DMY") // all dates should be in Day-Month-Year Format
format FORMCOMPLDAT_MNH05_temp %d
drop FORMCOMPLDAT_MNH05
rename FORMCOMPLDAT_MNH05_temp FORMCOMPLDAT_MNH05

/*Variables that are  text 
ANT_FAC_SPFY_OBSLOC
ANT_OTHR_SPFY_OBSLOC
COVAL_MNH05
FORMCOMPLID_MNH05*/

*************************************************
*MNH06 - Maternal Point-of-Care (PoC) Diagnostics
*************************************************

import delimited using "MNH06.csv", clear

************MNH06 Section A************					
   ******ID variable:					
	  *MOMID & PREGID
	     ***Checking duplicates 
bysort MOMID PREGID:  gen dup_MOMID = cond(_N==1,0,_n) //if no duplicates exist then value is 0 but if duplicates exist value is >=1
	     ***Listing duplicates of MOMID & PREGID
list MOMID PREGID if dup_MOMID>0 

   ******Date variable: Converting to date format and list out of range   
      *DIAG_VSDAT   
         ***Converting string date to date format   
foreach var of varlist DIAG_VSDAT {
gen `var'_temp=date(`var', "DMY") // all dates should be in Day-Month-Year Format"
format `var'_temp %d
drop `var'
rename `var'_temp `var'
   }

   ******Categorical and continues variable: Converting from string to numeric 
      *DIAG_OBSLOC TYPE_VISIT
foreach var of varlist DIAG_OBSLOC TYPE_VISIT {
destring `var', replace
            } 			

   ******Categorical variable: Listing observations with values out of range (1,2,88)
      *DIAG_OBSLOC 
list MOMID PREGID DIAG_VSDAT DIAG_OBSLOC if (DIAG_OBSLOC!=1 & DIAG_OBSLOC!=2 & DIAG_OBSLOC!=88 & DIAG_OBSLOC!=.)


   ******Categorical variable: Listing observations with values out of range (1-13)	  
      *TYPE_VISIT
list MOMID PREGID DIAG_VSDAT TYPE_VISIT if ((TYPE_VISIT<1 | TYPE_VISIT>13) & TYPE_VISIT!=.)	

					  
************MNH06 Section B************	
   ******Categorical and continues variable: Converting from string to numeric 
      *BP_VSSTAT*(1-3) BP_SYS_VSORRES_*(1-3) BP_DIA_VSORRES_*(1-3) TEMP_VSSTAT TEMP_VSORRES	MHR_VSSTAT	MHR_VSORRES	RR_VSSTAT	RR_VSORRES	PULSEOX_VSSTAT	PULSEOX_VSORRES	PALLOR_VSORRES	SPHB_VSSTAT	SPHB_LBORRES SINGLETON_PERES FETUS_CT_PERES FHT_VSORRES_*(1-4) FHR_VSSTAT_*(1-4) FHR_VSORRES_*(1-4) FHR_PEMETHOD*(1-4) 
foreach var of varlist BP_VSSTAT* BP_SYS_VSORRES_* BP_DIA_VSORRES_* TEMP_VSSTAT TEMP_VSORRES MHR_VSSTAT	MHR_VSORRES	RR_VSSTAT RR_VSORRES PULSEOX_VSSTAT	PULSEOX_VSORRES	PALLOR_VSORRES SPHB_VSSTAT SPHB_LBORRES SINGLETON_PERES	FETUS_CT_PERES FHT_VSORRES_*FHR_VSSTAT_* FHR_VSORRES_* FHR_PEMETHOD* {
destring `var', replace
            } 	

   ******Categorical variable: Listing observations with values out of range (1,0)	
      *BP_VSSTAT*(1-3) TEMP_VSSTAT MHR_VSSTAT RR_VSSTAT PULSEOX_VSSTAT SPHB_VSSTAT FHR_VSSTAT_2 FHR_VSSTAT_3 FHR_VSSTAT_4
foreach var of varlist BP_VSSTAT* TEMP_VSSTAT MHR_VSSTAT RR_VSSTAT PULSEOX_VSSTAT SPHB_VSSTAT FHR_VSSTAT_2 FHR_VSSTAT_3 FHR_VSSTAT_4 { 
                      list MOMID PREGID DIAG_VSDAT `var' if (`var'!=1 & `var'!=0 & `var'!=.) //*revise BP_VSSTAT_2 and BP_VSSTAT_3 if it's still 77, not available
  }	

   ******Categorical variable: Listing observations with values out of range (1,0,99)	
      *PALLOR_VSORRES
foreach var of varlist PALLOR_VSORRES {
                      list MOMID PREGID DIAG_VSDAT `var' if (`var'!=1 & `var'!=0 & `var'!=99 & `var'!=.)
  }	

     ******Categorical variable: Listing observations with values out of range (1,0,77)	
      *SINGLETON_PERES FHR_VSSTAT_1
foreach var of varlist FHR_VSSTAT_1 SINGLETON_PERES{
                      list MOMID PREGID DIAG_VSDAT `var' if (`var'!=1 & `var'!=0 & `var'!=77 & `var'!=.)
  }	
  
   ******Categorical variable: Listing observations with values out of range (1,0,2)	
      *FHT_VSORRES_1
foreach var of varlist FHT_VSORRES_1 {
                      list MOMID PREGID DIAG_VSDAT `var' if (`var'!=1 & `var'!=0 & `var'!=2 & `var'!=.)
  }	

   ******Categorical variable: Listing observations with values out of range (1,0,2,77)	
      *FHT_VSORRES_2 FHT_VSORRES_3 FHT_VSORRES_4 
foreach var of varlist FHT_VSORRES_2 FHT_VSORRES_3 FHT_VSORRES_4  {
                      list MOMID PREGID DIAG_VSDAT `var' if (`var'!=1 & `var'!=0 & `var'!=2 & `var'!=77 & `var'!=.)
  }	
 
   ******Categorical variable: Listing observations with values out of range (1-6, 88, 99)	
      *FHR_PEMETHOD*(1-4)
foreach var of varlist FHR_PEMETHOD* {
                      list MOMID PREGID DIAG_VSDAT `var' if ((`var'<1 | `var'>6) & `var'!=88 & `var'!=99 & `var'!=.)
  }	
 
   ******Continues variable: Listing observations with values out of range
/*      *BP_SYS_VSORRES_*(1-3)
	     ***Listing observations with values out of range (x-y)
foreach var of varlist BP_SYS_VSORRES_* {
list MOMID PREGID DIAG_VSDAT `var' if ((`var'<x | `var'>y) & `var'!=.)
}
      *BP_DIA_VSORRES_*(1-3)
	     ***Listing observations with values out of range (x-y)
foreach var of varlist BP_DIA_VSORRES_* {
list MOMID PREGID DIAG_VSDAT `var' if ((`var'<x | `var'>y) & `var'!=.)
}
*/
      *TEMP_VSORRES
	     ***Listing observations with values out of range (30-42 Celsius)
list MOMID PREGID DIAG_VSDAT TEMP_VSORRES if ((TEMP_VSORRES<30 | TEMP_VSORRES>42) & TEMP_VSORRES!=.)

      *MHR_VSORRES
	     ***Listing observations with values out of range (40-170 beats per minute)
list MOMID PREGID DIAG_VSDAT MHR_VSORRES if (((MHR_VSORRES<40 | MHR_VSORRES>170) & MHR_VSORRES!=.)

      *RR_VSORRES
	     ***Listing observations with values out of range (5-40 breaths per minute)
list MOMID PREGID DIAG_VSDAT RR_VSORRES if ((RR_VSORRES<5 | RR_VSORRES>40) & RR_VSORRES!=.)

      *PULSEOX_VSORRES
	     ***Listing observations with values out of range (85-100 %)
list MOMID PREGID DIAG_VSDAT PULSEOX_VSORRES if ((PULSEOX_VSORRES<85 | PULSEOX_VSORRES>100) & PULSEOX_VSORRES!=.)

      *SPHB_LBORRES
	     ***Listing observations with values out of range (1-20 g/dL)
list MOMID PREGID DIAG_VSDAT SPHB_LBORRES if ((SPHB_LBORRES<1 | SPHB_LBORRES>20) & SPHB_LBORRES!=.)

      *FETUS_CT_PERES
	     ***Listing observations with values out of range (1-4?)
 list MOMID PREGID DIAG_VSDAT FETUS_CT_PERES if ((FETUS_CT_PERES<1 | FETUS_CT_PERES>4) & FETUS_CT_PERES!=.)

      *FHR_VSORRES_*(1-4)
	     ***Listing observations with values out of range (60-190 beats per minute)
foreach var of varlist BP_DIA_VSORRES_* {
list MOMID PREGID DIAG_VSDAT `var' if ((`var'<60 | `var'>190) & `var'!=.)
}

************MNH06 Section C************			
   ******Categorical and continues variable: Converting from string to numeric 
      *HB_POC_LBPERF HB_POC_LBORRES	HB_POC_PEMETHOD MALARIA_POC_LBPERF	MALARIA_POC_LBORRES	HIV_POC_LBPERF	HIV_POC_LBORRES	SYPH_POC_LBPERF	SYPH_POC_LBORRES HBV_POC_LBPERF	HBV_POC_LBORRES	HCV_POC_LBPERF HCV_POC_LBORRES BGLUC_POC_LBPERF BGLUC_POC_MMOLL_LBORRES	BGLUC_POC_MDGL_LBORRES	COVID_POC_LBPERF COVID_POC_LBORRES
foreach var of varlist HB_POC_LBPERF HB_POC_LBORRES	HB_POC_PEMETHOD MALARIA_POC_LBPERF	MALARIA_POC_LBORRES	HIV_POC_LBPERF	HIV_POC_LBORRES	SYPH_POC_LBPERF	SYPH_POC_LBORRES HBV_POC_LBPERF	HBV_POC_LBORRES	HCV_POC_LBPERF HCV_POC_LBORRES BGLUC_POC_LBPERF BGLUC_POC_MMOLL_LBORRES	BGLUC_POC_MDGL_LBORRES	COVID_POC_LBPERF COVID_POC_LBORRES {
destring `var', replace
            } 

   ******Categorical variable: Listing observations with values out of range (1,0)	
      *HB_POC_LBPERF MALARIA_POC_LBPERF	MALARIA_POC_LBORRES	HIV_POC_LBPERF	HIV_POC_LBORRES	SYPH_POC_LBPERF	SYPH_POC_LBORRES HBV_POC_LBPERF	HBV_POC_LBORRES	HCV_POC_LBPERF HCV_POC_LBORRES BGLUC_POC_LBPERF COVID_POC_LBPERF COVID_POC_LBORRES
foreach var of varlist HB_POC_LBPERF MALARIA_POC_LBPERF	MALARIA_POC_LBORRES	HIV_POC_LBPERF	HIV_POC_LBORRES	SYPH_POC_LBPERF	SYPH_POC_LBORRES HBV_POC_LBPERF	HBV_POC_LBORRES	HCV_POC_LBPERF HCV_POC_LBORRES BGLUC_POC_LBPERF COVID_POC_LBPERF COVID_POC_LBORRES { 
                      list MOMID PREGID DIAG_VSDAT `var' if (`var'!=1 & `var'!=0 & `var'!=.) 
  }				

   ******Categorical variable: Listing observations with values out of range (1-3, 88, 99)	
      *HB_POC_PEMETHOD
foreach var of varlist HB_POC_PEMETHOD {
                      list MOMID PREGID DIAG_VSDAT `var' if ((`var'<1 | `var'>3) & `var'!=88 & `var'!=99 & `var'!=.)
  }	
  
   ******Continues variable: Listing observations with values out of range  
      *HB_POC_LBORRES
	     ***Listing observations with values out of range (1-20 g/dL)
list MOMID PREGID DIAG_VSDAT HB_POC_LBORRES if ((HB_POC_LBORRES<1 | HB_POC_LBORRES>20) & HB_POC_LBORRES!=.)  
  
      *BGLUC_POC_MMOLL_LBORRES
	     ***Listing observations with values out of range (3.6-6.6 mmol/L)
list MOMID PREGID DIAG_VSDAT BGLUC_POC_MMOLL_LBORRES if (BGLUC_POC_MMOLL_LBORRES<3.6 | BGLUC_POC_MMOLL_LBORRES>6.6) & BGLUC_POC_MMOLL_LBORRES!=.) 

      *HB_POC_LBORRES
	     ***Listing observations with values out of range (60-100 mg/dL)
list MOMID PREGID DIAG_VSDAT BGLUC_POC_MDGL_LBORRES if (BGLUC_POC_MDGL_LBORRES<60 | BGLUC_POC_MDGL_LBORRES>100) & BGLUC_POC_MDGL_LBORRES!=.)   
   					  
************MNH06 Section D************		
   ******Categorical and continues variable: Converting from string to numeric 
      *COYN_MNH06
foreach var of varlist COYN_MNH06 {
destring `var', replace
            } 

   ******Categorical variable: Listing observations with values out of range (1,0)	
      *COYN_MNH06
foreach var of varlist COYN_MNH06 { 
                      list MOMID PREGID DIAG_VSDAT `var' if (`var'!=1 & `var'!=0 & `var'!=.) 
  }	


   ******Date variable: Converting to date format and list out of range   
      *FORMCOMPLDAT_MNH06   
         ***Converting string date to date format   
gen FORMCOMPLDAT_MNH06_temp=date(FORMCOMPLDAT_MNH06, "DMY") // all dates should be in Day-Month-Year Format
format FORMCOMPLDAT_MNH06_temp %d
drop FORMCOMPLDAT_MNH06
rename FORMCOMPLDAT_MNH06_temp FORMCOMPLDAT_MNH06

   ******Time variable: Converting to time format and list out of range   
      *FORMCOMPLTIM_MNH06   
         ***Converting string time to time format   
gen FORMCOMPLTIM_MNH06_temp=clock(FORMCOMPLTIM_MNH06, "hms") // all dates should be in hh:mm Format
format FORMCOMPLTIM_MNH06_temp %tcHH:MM
drop FORMCOMPLTIM_MNH06
rename FORMCOMPLTIM_MNH06_temp FORMCOMPLTIM_MNH06

/*Variables that are  text 
DIAG_FAC_SPFY_OBSLOC
DIAG_OTHR_SPFY_OBSLOC
FHR_SPFY_PEMETHOD
FHR_SPFY_PEMETHOD_2
FHR_SPFY_PEMETHOD_3
FHR_SPFY_PEMETHOD_4
HB_POC_SPFY_PEMETHOD
COVAL_MNH06
FORMCOMPLID_MNH06*/
