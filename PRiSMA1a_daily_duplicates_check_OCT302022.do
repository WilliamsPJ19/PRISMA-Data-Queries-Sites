************************************************
*PRiSMA Daily Duplicate Check 

/**FILE SETUP**/
clear 			//clears the STATA memory of previous data that may be open
set more off 	//allows STATA to run long do files without pausing (no longer necessary after STATA 16)
cap log close 	//closes any log file that may be open due to do-file terminating early
	/*These are commands I include at the top of every do file. There are
	additional commands that I will add as we advance through the semester*/	


/**CHANGE WORKING DIRECTION**/
cd "INSERT WORKING DIRECTORY "

/**ADD DATE (dd-mm-yyyy)**/
global date "10-25-2022"


/**IMPORT LOG**/
log using "daily_checks_$date.log", replace	

*************************************************
*MNH00 - Pre-Screening Form 
*************************************************

/*If MOMID AND PREGID were not available for pre-screening form, then remove them in MNH00 and replace with SCRNID to run*/

import delimited using "data/mnh00.CSV", clear 
rename *, upper

 
bysort SCRNID: gen dup_id=cond(_N==1,0,_n) // if there is not a duplicate, then dup_MOMID will be 0; if duplicate, it will be >1
preserve

keep if dup_id>0
keep SCRNID dup_id
if dup_id==0 | dup_id==1 {
	export excel using "output/daily_duplicate_check_$date.xlsx", sheet("M00", replace) ///
		cell(A1) firstrow(variables)
}
restore

//remove duplicates and generate .dta file to help with merging codes at the end of this do file 
keep if dup_id==0
keep SCRNID dup_id
save "mnh00.dta", replace

*************************************************
*MNH01 - Ultrasound Form 
*************************************************
import delimited using "data/mnh01.CSV", clear 
rename *, upper

/* Ultrasounds can be performed at any ANC visit -- in order to differentiate, we need to assign the viist type based off GA age: 
	- type_visit = -7 = missing GA_LMP_WEEKS_SCORRES 
	- type_visit = 1 = Enrollment 
	- type_visit = 2 = ANC-20
	- type_visit = 3 = ANC-28
	- type_visit = 4 = ANC-32
	- type_visit = 5 = ANC-36
*/

gen TYPE_VISIT =.
replace TYPE_VISIT = -7 if GA_LMP_WEEKS_SCORRES < 0  
replace TYPE_VISIT = 1 if GA_LMP_WEEKS_SCORRES > 0 & GA_LMP_WEEKS_SCORRES <= 17 
replace TYPE_VISIT = 2 if GA_LMP_WEEKS_SCORRES >=18 & GA_LMP_WEEKS_SCORRES <=25 // includes late window (23-25 weeks)
replace TYPE_VISIT = 3 if GA_LMP_WEEKS_SCORRES >=26 & GA_LMP_WEEKS_SCORRES <=30 // no late window 
replace TYPE_VISIT = 4 if GA_LMP_WEEKS_SCORRES >=31 & GA_LMP_WEEKS_SCORRES <36 // includes late window (up to anc-36 visit) 
replace TYPE_VISIT = 5 if GA_LMP_WEEKS_SCORRES >=36 // window up to delivery 
 
* get instances of same screening id with the same type_visit 
bysort SCRNID TYPE_VISIT: gen dup_id=cond(_N==1,0,_n) // if there is not a duplicate, then dup_MOMID will be 0; if duplicate, it will be >1
preserve

keep if dup_id>0
keep SCRNID dup_id TYPE_VISIT US_VISIT GA_LMP_WEEKS_SCORRES
if dup_id==0 | dup_id==1 {
	export excel using "output/daily_duplicate_check_$date.xlsx", sheet("M01", replace) ///
		cell(A1) firstrow(variables)
}

restore

//remove duplicates and generate .dta file to help with merging codes at the end of this do file 
keep if dup_id==0
keep SCRNID dup_id
save "mnh01.dta", replace


*************************************************
*MNH02 - Enrollment Status Form
*************************************************
/*If MOMID AND PREGID were not available for pre-screening form, then remove them in MNH00 and replace with SCRNID to run*/

import delimited using "data/mnh02.CSV", clear 
rename *, upper

bysort SCRNID: gen dup_id=cond(_N==1,0,_n) // if there is not a duplicate, then dup_MOMID will be 0; if duplicate, it will be >1
preserve

keep if dup_id>0
keep SCRNID dup_id
if dup_id==0 | dup_id==1 {
	export excel using "output/daily_duplicate_check_$date.xlsx", sheet("M02", replace) ///
		cell(A1) firstrow(variables)
}

restore

//remove duplicates and generate .dta file to help with merging codes at the end of this do file 
keep if dup_id==0
keep SCRNID dup_id MOMID PREGID
save "mnh02.dta", replace

*************************************************
*MNH03 - Sociodemographic Form 
*************************************************
import delimited using "data/mnh03.CSV", clear 
rename *, upper

bysort MOMID PREGID: gen dup_id=cond(_N==1,0,_n) // if there is not a duplicate, then dup_MOMID will be 0; if duplicate, it will be >1
keep if dup_id>0
keep MOMID dup_id PREGID
if dup_id==0 | dup_id==1 {
	export excel using "output/daily_duplicate_check_$date.xlsx", sheet("M03", replace) ///
		cell(A1) firstrow(variables)
}

*************************************************
*MNH04 - ANC Clinical Status Form
*************************************************
import delimited using "data/mnh04.CSV", clear 
rename *, upper

bysort MOMID PREGID TYPE_VISIT: gen dup_id=cond(_N==1,0,_n) // if there is not a duplicate, then dup_MOMID will be 0; if duplicate, it will be >1
keep if dup_id>0
keep MOMID dup_id PREGID TYPE_VISIT
if dup_id==0 | dup_id==1 {
	export excel using "output/daily_duplicate_check_$date.xlsx", sheet("M04", replace) ///
		cell(A1) firstrow(variables)
}


*************************************************
*MNH05 - Maternal Anthropometry Form
*************************************************
import delimited using "data/mnh05.CSV", clear 
rename *, upper

bysort MOMID PREGID TYPE_VISIT: gen dup_id=cond(_N==1,0,_n) // if there is not a duplicate, then dup_MOMID will be 0; if duplicate, it will be >1
keep if dup_id>0
keep MOMID dup_id PREGID TYPE_VISIT
if dup_id==0 | dup_id==1 {
	export excel using "output/daily_duplicate_check_$date.xlsx", sheet("M05", replace) ///
		cell(A1) firstrow(variables)
}

*************************************************
*MNH06 - Maternal Point-of-Care (PoC) Diagnostics
*************************************************
import delimited using "data/mnh06.CSV", clear 
rename *, upper

bysort MOMID PREGID TYPE_VISIT: gen dup_id=cond(_N==1,0,_n) // if there is not a duplicate, then dup_MOMID will be 0; if duplicate, it will be >1
keep if dup_id>0
keep MOMID dup_id PREGID TYPE_VISIT
if dup_id==0 | dup_id==1 {
	export excel using "output/daily_duplicate_check_$date.xlsx", sheet("M06", replace) ///
		cell(A1) firstrow(variables)
}

*************************************************
*MNH07 - Maternal Specimen Collection
*************************************************
import delimited using "data/mnh07.CSV", clear 
rename *, upper

bysort MOMID PREGID MAT_SPEC_COLLECT_VISIT: gen dup_id=cond(_N==1,0,_n) // if there is not a duplicate, then dup_MOMID will be 0; if duplicate, it will be >1
keep if dup_id>0
keep MOMID dup_id PREGID MAT_SPEC_COLLECT_VISIT
if dup_id==0 | dup_id==1 {
	export excel using "output/daily_duplicate_check_$date.xlsx", sheet("M07", replace) ///
		cell(A1) firstrow(variables)
}

*************************************************
*MNH08 - Maternal Laboratory Results
*************************************************
import delimited using "data/mnh08.CSV", clear 
rename *, upper

bysort MOMID PREGID VISIT_LBSTDAT: gen dup_id=cond(_N==1,0,_n) // if there is not a duplicate, then dup_MOMID will be 0; if duplicate, it will be >1
keep if dup_id>0
keep MOMID dup_id PREGID VISIT_LBSTDAT
if dup_id==0 | dup_id==1 {
	export excel using "output/daily_duplicate_check_$date.xlsx", sheet("M08", replace) ///
		cell(A1) firstrow(variables)
}

*************************************************
*MNH09 - Maternal L&D Results 
*************************************************
import delimited using "data/mnh09.CSV", clear 
rename *, upper

bysort MOMID PREGID: gen dup_id=cond(_N==1,0,_n) // if there is not a duplicate, then dup_MOMID will be 0; if duplicate, it will be >1
keep if dup_id>0
keep MOMID dup_id PREGID 
if dup_id==0 | dup_id==1 {
	export excel using "output/daily_duplicate_check_$date.xlsx", sheet("M09", replace) ///
		cell(A1) firstrow(variables)
}


*************************************************
*MNH10 - Maternal Post-Delivery Outcome
*************************************************
import delimited using "data/mnh10.CSV", clear 
rename *, upper

bysort MOMID PREGID: gen dup_id=cond(_N==1,0,_n) // if there is not a duplicate, then dup_MOMID will be 0; if duplicate, it will be >1
keep if dup_id>0
keep MOMID dup_id PREGID 
if dup_id==0 | dup_id==1 {
	export excel using "output/daily_duplicate_check_$date.xlsx", sheet("M10", replace) ///
		cell(A1) firstrow(variables)
}

*************************************************
*MNH11 - Newborn Birth Outcome 
*************************************************
import delimited using "data/mnh11.CSV", clear 
rename *, upper

bysort INFANTID: gen dup_id=cond(_N==1,0,_n) // if there is not a duplicate, then dup_MOMID will be 0; if duplicate, it will be >1
keep if dup_id>0
keep INFANTID dup_id  
if dup_id==0 | dup_id==1 {
	export excel using "output/daily_duplicate_check_$date.xlsx", sheet("M11", replace) ///
		cell(A1) firstrow(variables)
}

*************************************************
*MNH12 - Maternal PNC
*************************************************
import delimited using "data/mnh12.CSV", clear 
rename *, upper

bysort MOMID PREGID PNC_N_VISIT: gen dup_id=cond(_N==1,0,_n) // if there is not a duplicate, then dup_MOMID will be 0; if duplicate, it will be >1
keep if dup_id>0
keep MOMID dup_id PREGID PNC_N_VISIT
if dup_id==0 | dup_id==1 {
	export excel using "output/daily_duplicate_check_$date.xlsx", sheet("M12", replace) ///
		cell(A1) firstrow(variables)
}


*************************************************
*MNH13 - Infant PNC
*************************************************
import delimited using "data/mnh13.CSV", clear 
rename *, upper

bysort INFANTID PNC_N_VISIT: gen dup_id=cond(_N==1,0,_n) // if there is not a duplicate, then dup_MOMID will be 0; if duplicate, it will be >1
keep if dup_id>0
keep INFANTID dup_id PNC_N_VISIT
if dup_id==0 | dup_id==1 {
	export excel using "output/daily_duplicate_check_$date.xlsx", sheet("M13", replace) ///
		cell(A1) firstrow(variables)
}

*************************************************
*MNH14 - Infant PoC Diagnostics 
*************************************************
import delimited using "data/mnh14.CSV", clear 
rename *, upper

bysort INFANTID POC_VISIT: gen dup_id=cond(_N==1,0,_n) // if there is not a duplicate, then dup_MOMID will be 0; if duplicate, it will be >1
keep if dup_id>0
keep INFANTID dup_id POC_VISIT
if dup_id==0 | dup_id==1 {
	export excel using "output/daily_duplicate_check_$date.xlsx", sheet("M14", replace) ///
		cell(A1) firstrow(variables)
}

*************************************************
*MNH15 - Infant Vaccination Status 
*************************************************
import delimited using "data/mnh15.CSV", clear 
rename *, upper


bysort INFANTID OBSTERM: gen dup_id=cond(_N==1,0,_n) // if there is not a duplicate, then dup_MOMID will be 0; if duplicate, it will be >1
keep if dup_id>0
keep INFANTID dup_id OBSTERM
if dup_id==0 | dup_id==1 {
	export excel using "output/daily_duplicate_check_$date.xlsx", sheet("M15", replace) ///
		cell(A1) firstrow(variables)
}

*************************************************
*MNH16 - ANC Client Exit Interview 
*************************************************
import delimited using "data/mnh16.CSV", clear 
rename *, upper

bysort MOMID PREGID: gen dup_id=cond(_N==1,0,_n) // if there is not a duplicate, then dup_MOMID will be 0; if duplicate, it will be >1
keep if dup_id>0
keep MOMID dup_id PREGID 
if dup_id==0 | dup_id==1 {
	export excel using "output/daily_duplicate_check_$date.xlsx", sheet("M16", replace) ///
		cell(A1) firstrow(variables)
}

*************************************************
*MNH17 - IPC Client Exit Interview 
*************************************************
import delimited using "data/mnh17.CSV", clear 
rename *, upper

bysort MOMID PREGID: gen dup_id=cond(_N==1,0,_n) // if there is not a duplicate, then dup_MOMID will be 0; if duplicate, it will be >1
keep if dup_id>0
keep MOMID dup_id PREGID 
if dup_id==0 | dup_id==1 {
	export excel using "output/daily_duplicate_check_$date.xlsx", sheet("M17", replace) ///
		cell(A1) firstrow(variables)
}

*************************************************
*MNH18 - PNC Client Exit Interview 
*************************************************
import delimited using "data/mnh18.CSV", clear 
rename *, upper

bysort MOMID PREGID: gen dup_id=cond(_N==1,0,_n) // if there is not a duplicate, then dup_MOMID will be 0; if duplicate, it will be >1
keep if dup_id>0
keep MOMID dup_id PREGID 
if dup_id==0 | dup_id==1 {
	export excel using "output/daily_duplicate_check_$date.xlsx", sheet("M18", replace) ///
		cell(A1) firstrow(variables)
}

*************************************************
*MNH19 - Maternal Hospitalization 
*************************************************
import delimited using "data/mnh19.CSV", clear 
rename *, upper

bysort MOMID PREGID VISDAT: gen dup_id=cond(_N==1,0,_n) // if there is not a duplicate, then dup_MOMID will be 0; if duplicate, it will be >1
keep if dup_id>0
keep MOMID dup_id PREGID OBSSTDAT
if dup_id==0 | dup_id==1 {
	export excel using "output/daily_duplicate_check_$date.xlsx", sheet("M19", replace) ///
		cell(A1) firstrow(variables)
}

*************************************************
*MNH20 - Infant Hospitalization 
*************************************************
import delimited using "data/mnh20.CSV", clear 
rename *, upper

bysort INFANTID OBSSTDAT: gen dup_id=cond(_N==1,0,_n) // if there is not a duplicate, then dup_MOMID will be 0; if duplicate, it will be >1
keep if dup_id>0
keep INFANTID dup_id OBSSTDAT
if dup_id==0 | dup_id==1 {
	export excel using "output/daily_duplicate_check_$date.xlsx", sheet("M20", replace) ///
		cell(A1) firstrow(variables)
}

*************************************************
*MNH21 - Adverse Events
*************************************************
import delimited using "data/mnh21.CSV", clear 
rename *, upper


bysort MOMID PREGID INFANTID AESTDAT: gen dup_id=cond(_N==1,0,_n) // if there is not a duplicate, then dup_MOMID will be 0; if duplicate, it will be >1
keep if dup_id>0
keep MOMID PREGID dup_id PREGID INFANTID AESTDAT
if dup_id==0 | dup_id==1 {
	export excel using "output/daily_duplicate_check_$date.xlsx", sheet("M21", replace) ///
		cell(A1) firstrow(variables)
}

*************************************************
*MNH23 - Maternal Closeout 
*************************************************
import delimited using "data/mnh23.CSV", clear 
rename *, upper

bysort MOMID PREGID: gen dup_id=cond(_N==1,0,_n) // if there is not a duplicate, then dup_MOMID will be 0; if duplicate, it will be >1
keep if dup_id>0
keep MOMID dup_id PREGID 
if dup_id==0 | dup_id==1 {
	export excel using "output/daily_duplicate_check_$date.xlsx", sheet("M23", replace) ///
		cell(A1) firstrow(variables)
}

*************************************************
*MNH24 - Infant Closeout 
*************************************************
import delimited using "data/mnh24.CSV", clear 
rename *, upper

bysort INFANTID: gen dup_id=cond(_N==1,0,_n) // if there is not a duplicate, then dup_MOMID will be 0; if duplicate, it will be >1
keep if dup_id>0
keep INFANTID dup_id 
if dup_id==0 | dup_id==1 {
	export excel using "output/daily_duplicate_check_$date.xlsx", sheet("M24", replace) ///
		cell(A1) firstrow(variables)
}

*************************************************
*MNH25 - Maternal Depression
*************************************************
import delimited using "data/mnh25.CSV", clear 
rename *, upper

bysort MOMID PREGID ANC_VISIT_N: gen dup_id=cond(_N==1,0,_n) // if there is not a duplicate, then dup_MOMID will be 0; if duplicate, it will be >1
keep if dup_id>0
keep MOMID dup_id PREGID ANC_VISIT_N
if dup_id==0 | dup_id==1 {
	export excel using "output/daily_duplicate_check_$date.xlsx", sheet("M25", replace) ///
		cell(A1) firstrow(variables)
}

*************************************************
*MNH26 - Maternal Fatigue
*************************************************
import delimited using "data/mnh26.CSV", clear 
rename *, upper

bysort MOMID PREGID: gen dup_id=cond(_N==1,0,_n) // if there is not a duplicate, then dup_MOMID will be 0; if duplicate, it will be >1
keep if dup_id>0
keep MOMID dup_id PREGID 
if dup_id==0 | dup_id==1 {
	export excel using "output/daily_duplicate_check_$date.xlsx", sheet("M26", replace) ///
		cell(A1) firstrow(variables)
}

	

*************************************************************************************
/*check to make sure that all women who are enrolled in the study were screened 
  (in order for this to run properly, all duplicates must be removed) 

The output of this code will be a sheet in the excel file called "Missing Enrollent". 
These are women who have ANC forms, but who do not have screening IDs in the pre-screening form (MNH00) 
*/
*************************************************************************************

 // create temp files for each form with just momid and pregid 
 foreach x in 03 04 05 06 07 08 09 10 12 16 17 18 19{
	import delimited using "data/mnh`x'.csv", clear
	rename *, upper
	keep MOMID PREGID
	tempfile mnh`x'
	save `mnh`x''
 } 
 
 // in the code above, a .dta file was generated for m00, m01, and m02 in order to remove duplicates to merge below 
use "mnh01.dta", clear 
merge 1:1 SCRNID using "mnh02.dta"
tempfile merge1
drop _merge dup_id 
keep if MOMID!="n/a"
save "merge1.dta", replace

 // merge all forms together 
 foreach x in 03 04 05 06 07 08 09 10 12 16 17 18 19{
	merge 1:1 MOMID PREGID using `mnh`x''
	drop _merge
}

keep if PREGID!="n/a" //remove any n/a pregids 

 // merge with the screening form to get any duplicates
merge 1:1 SCRNID using "mnh00.dta"

 // export to excel 
keep if _merge==1 // if a momid is in all other forms BUT enrollment, then _merge will be 1 
keep SCRNID MOMID PREGID 
export excel using "output/daily_duplicate_check_$date.xlsx", sheet("Missing Enrollment", replace) ///
		cell(A1) firstrow(variables)

		
*************************************************************************************
/* check to make sure that all infants who have PNC visits were actually born 

 The output of this code will be a sheet in the excel file called "Missing Infant Delivery".
 This will let us know if there are infants that have filled out PNC visits, but are missing a delivery form (MNH11)
 
*/
*************************************************************************************
/* will include in the code when infant data is available 

 // save mnh11 as a .dta file for merging 
foreach x in 11 {
	import delimited using "data/mnh`x'.csv", clear
	rename *, upper
	keep INFANTID
    save mnh`x'.dta, replace
 } 

 // save each infant PNC as a tempfile for merging 
foreach x in 12 13 14 15 20 21 24{
	import delimited using "data/mnh`x'.csv", clear
	rename *, upper
	keep INFANTID
	tempfile mnh`x'
	save `mnh`x''
 } 

  // merge all PNC forms together 
 foreach x in 12 13 14 15 20 21 24{
	merge 1:1 INFANTID using `mnh`x''
	drop _merge
}

 // merge PNC forms with delivery form (mnh11)
merge 1:1 SCRNID using "mnh11.dta"

 // export to excel 
keep if _merge==1 // if a infant is in all other forms BUT enrollment, then _merge will be 1 
keep infant
export excel using "output/daily_duplicate_check_$date.xlsx", sheet("Missing Infant Delivery", replace) ///
		cell(A1) firstrow(variables)	
		
*/

log close













