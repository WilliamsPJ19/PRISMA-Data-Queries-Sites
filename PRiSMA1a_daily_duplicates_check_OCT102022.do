************************************************
*PRiSMA Daily Duplicate Check 

/**FILE SETUP**/
clear 			//clears the STATA memory of previous data that may be open
set more off 	//allows STATA to run long do files without pausing (no longer necessary after STATA 16)
cap log close 	//closes any log file that may be open due to do-file terminating early
	/*These are commands I include at the top of every do file. There are
	additional commands that I will add as we advance through the semester*/	

/**CHANGE WORKING DIRECTION**/
cd " *INSERT WORKING DIRECTORY* "

/**ADD DATE (dd-mm-yyyy)**/
global date "10-10-2022"


/**IMPORT LOG**/
*log using "/daily_checks_$date.log", replace	
log using "log/daily_checks_$date.log", replace	



*************************************************
*MNH00 - Pre-Screening Form
*************************************************

/*If MOMID AND PREGID were not available for pre-screening form, then remove them in MNH00 and replace with SCRNID to run*/

import delimited using "data/mnh00.CSV", clear 
rename *, upper

bysort SCRNID: gen dup_SCRNID=cond(_N==1,0,_n) // if there is not a duplicate, then dup_MOMID will be 0; if duplicate, it will be >1
*preserve 
keep if dup_SCRNID>0
keep SCRNID dup_SCRNID
if dup_SCRNID==0 | dup_SCRNID==1 {
	export excel using "output/daily_duplicate_check_$date.xlsx", sheet("M00", replace) ///
		cell(A1) firstrow(variables)
}

*************************************************
*MNH01 - Ultrasound Form 
*************************************************
import delimited using "data/mnh01.CSV", clear 
rename *, upper

bysort SCRNID: gen dup_SCRNID=cond(_N==1,0,_n) // if there is not a duplicate, then dup_MOMID will be 0; if duplicate, it will be >1
*preserve 
keep if dup_SCRNID>0
keep SCRNID dup_SCRNID 
if dup_SCRNID==0 | dup_SCRNID==1 {
	export excel using "output/daily_duplicate_check_$date.xlsx", sheet("M01", replace) ///
		cell(A1) firstrow(variables)
}

*************************************************
*MNH02 - Enrollment Status Form
*************************************************
import delimited using "data/mnh02.CSV", clear 
rename *, upper

** look at duplicate screening IDs ** 
bysort SCRNID: gen dup_SCRNID=cond(_N==1,0,_n) // if there is not a duplicate, then dup_SCRNID will be 0; if duplicate, it will be >1

** look at duplicate MOM IDs -- MOMIDs will be assigned to women who are enrolled in the study ** 
bysort MOMID: gen MOMID_enroll=1 if (PC_IEORRES==1 & MOMID!="NA") // get MOMIDs for women who are enrolled into the study 
keep if MOMID_enroll==1 // only look at MOMIDs of women who are enrolled and assigned an ID 
bysort MOMID: gen dup_MOMID=cond(_N==1,0,_n) // if there is not a duplicate, then dup_MOMID will be 0; if duplicate, it will be >1

keep if dup_SCRNID>0 | dup_MOMID>0
keep SCRNID dup_SCRNID MOMID dup_MOMID

if dup_SCRNID==0 | dup_SCRNID==1 | dup_MOMID==0 | dup_MOMID==1 {
	export excel using "output/daily_duplicate_check_$date.xlsx", sheet("M02", replace) ///
		cell(A1) firstrow(variables)
}

log close


*************************************************
*MNH03 - Sociodemographic Form 
*************************************************
import delimited using "data/mnh03.CSV", clear 
rename *, upper

bysort MOMID: gen dup_MOMID=cond(_N==1,0,_n) // if there is not a duplicate, then dup_MOMID will be 0; if duplicate, it will be >1
keep if dup_MOMID>0
keep MOMID dup_MOMID PREGID SD_OBSSTDAT
if dup_MOMID==0 | dup_MOMID==1 {
	export excel using "output/daily_duplicate_check_$date.xlsx", sheet("M03", replace) ///
		cell(A1) firstrow(variables)
}

*************************************************
*MNH04 - ANC Clinical Status Form
*************************************************
import delimited using "data/mnh04.CSV", clear 
rename *, upper

bysort MOMID: gen dup_MOMID=cond(_N==1,0,_n) // if there is not a duplicate, then dup_MOMID will be 0; if duplicate, it will be >1
keep if dup_MOMID>0
keep MOMID dup_MOMID PREGID ANC_OBSSTDAT TYPE_VISIT
if dup_MOMID==0 | dup_MOMID==1 {
	export excel using "output/daily_duplicate_check_$date.xlsx", sheet("M04", replace) ///
		cell(A1) firstrow(variables)
}


*************************************************
*MNH05 - Maternal Anthropometry Form
*************************************************
import delimited using "data/mnh05.CSV", clear 
rename *, upper

bysort MOMID: gen dup_MOMID=cond(_N==1,0,_n) // if there is not a duplicate, then dup_MOMID will be 0; if duplicate, it will be >1
keep if dup_MOMID>0
keep MOMID dup_MOMID PREGID ANT_PEDAT TYPE_VISIT
if dup_MOMID==0 | dup_MOMID==1 {
	export excel using "output/daily_duplicate_check_$date.xlsx", sheet("M05", replace) ///
		cell(A1) firstrow(variables)
}
*************************************************
*MNH06 - Maternal Point-of-Care (PoC) Diagnostics
*************************************************
import delimited using "data/mnh06.CSV", clear 
rename *, upper

bysort MOMID: gen dup_MOMID=cond(_N==1,0,_n) // if there is not a duplicate, then dup_MOMID will be 0; if duplicate, it will be >1
keep if dup_MOMID>0
keep MOMID dup_MOMID PREGID DIAG_VSDAT TYPE_VISIT
if dup_MOMID==0 | dup_MOMID==1 {
	export excel using "output/daily_duplicate_check_$date.xlsx", sheet("M06", replace) ///
		cell(A1) firstrow(variables)
}

*************************************************
*MNH07 - Maternal Specimen Collection
*************************************************
import delimited using "data/mnh07.CSV", clear 
rename *, upper

bysort MOMID: gen dup_MOMID=cond(_N==1,0,_n) // if there is not a duplicate, then dup_MOMID will be 0; if duplicate, it will be >1
keep if dup_MOMID>0
keep MOMID dup_MOMID PREGID MAT_SPEC_COLLECT_DAT MAT_SPEC_COLLECT_VISIT
if dup_MOMID==0 | dup_MOMID==1 {
	export excel using "output/daily_duplicate_check_$date.xlsx", sheet("M07", replace) ///
		cell(A1) firstrow(variables)
}


*************************************************
*MNH08 - Maternal Laboratory Results
*************************************************
import delimited using "data/mnh08.CSV", clear 
rename *, upper

bysort MOMID: gen dup_MOMID=cond(_N==1,0,_n) // if there is not a duplicate, then dup_MOMID will be 0; if duplicate, it will be >1
keep if dup_MOMID>0
keep MOMID dup_MOMID PREGID LBSTDAT VISIT_LBSTDAT
if dup_MOMID==0 | dup_MOMID==1 {
	export excel using "output/daily_duplicate_check_$date.xlsx", sheet("M08", replace) ///
		cell(A1) firstrow(variables)
}

*************************************************
*MNH09 - Maternal L&D Results 
*************************************************
import delimited using "data/mnh09.CSV", clear 
rename *, upper

bysort MOMID: gen dup_MOMID=cond(_N==1,0,_n) // if there is not a duplicate, then dup_MOMID will be 0; if duplicate, it will be >1
keep if dup_MOMID>0
keep MOMID dup_MOMID PREGID MAT_LD_OHOSTDAT
if dup_MOMID==0 | dup_MOMID==1 {
	export excel using "output/daily_duplicate_check_$date.xlsx", sheet("M09", replace) ///
		cell(A1) firstrow(variables)
}


*************************************************
*MNH10 - Maternal Post-Delivery Outcome
*************************************************
import delimited using "data/mnh10.CSV", clear 
rename *, upper

bysort MOMID: gen dup_MOMID=cond(_N==1,0,_n) // if there is not a duplicate, then dup_MOMID will be 0; if duplicate, it will be >1
keep if dup_MOMID>0
keep MOMID dup_MOMID PREGID 
if dup_MOMID==0 | dup_MOMID==1 {
	export excel using "output/daily_duplicate_check_$date.xlsx", sheet("M10", replace) ///
		cell(A1) firstrow(variables)
}


*************************************************
*MNH11 - Newborn Birth Outcome 
*************************************************
import delimited using "data/mnh11.CSV", clear 
rename *, upper

** duplicate MOMID ** 
bysort MOMID: gen dup_MOMID=cond(_N==1,0,_n) // if there is not a duplicate, then dup_MOMID will be 0; if duplicate, it will be >1
keep if dup_MOMID>0

** duplicate INFANTID ** 
bysort INFANTID: gen dup_INFANTID=cond(_N==1,0,_n) // if there is not a duplicate, then dup_MOMID will be 0; if duplicate, it will be >1
keep if dup_INFANTID>0

keep MOMID dup_MOMID PREGID INFANTID dup_INFANTID VISIT_OBSSTDAT
if dup_MOMID==0 | dup_MOMID==1 | dup_INFANTID==0 | INFANTID==1{
	export excel using "output/daily_duplicate_check_$date.xlsx", sheet("M11", replace) ///
		cell(A1) firstrow(variables)
}

*************************************************
*MNH12 - Maternal PNC
*************************************************
import delimited using "data/mnh12.CSV", clear 
rename *, upper

bysort MOMID: gen dup_MOMID=cond(_N==1,0,_n) // if there is not a duplicate, then dup_MOMID will be 0; if duplicate, it will be >1
keep if dup_MOMID>0
keep MOMID dup_MOMID PREGID VISIT_OBSSTDAT PNC_N_VISIT
if dup_MOMID==0 | dup_MOMID==1 {
	export excel using "output/daily_duplicate_check_$date.xlsx", sheet("M12", replace) ///
		cell(A1) firstrow(variables)
}


*************************************************
*MNH13 - Infant PNC
*************************************************
import delimited using "data/mnh13.CSV", clear 
rename *, upper

** duplicate MOMID ** 
bysort MOMID: gen dup_MOMID=cond(_N==1,0,_n) // if there is not a duplicate, then dup_MOMID will be 0; if duplicate, it will be >1
keep if dup_MOMID>0

** duplicate INFANTID ** 
bysort INFANTID: gen dup_INFANTID=cond(_N==1,0,_n) // if there is not a duplicate, then dup_MOMID will be 0; if duplicate, it will be >1
keep if dup_INFANTID>0

keep MOMID dup_MOMID PREGID INFANTID dup_INFANTID VISIT_OBSSTDAT PNC_N_VISIT
if dup_MOMID==0 | dup_MOMID==1 | dup_INFANTID==0 | INFANTID==1{
	export excel using "output/daily_duplicate_check_$date.xlsx", sheet("M13", replace) ///
		cell(A1) firstrow(variables)
}


*************************************************
*MNH14 - Infant PoC Diagnostics 
*************************************************
import delimited using "data/mnh14.CSV", clear 
rename *, upper

** duplicate MOMID ** 
bysort MOMID: gen dup_MOMID=cond(_N==1,0,_n) // if there is not a duplicate, then dup_MOMID will be 0; if duplicate, it will be >1
keep if dup_MOMID>0

** duplicate INFANTID ** 
bysort INFANTID: gen dup_INFANTID=cond(_N==1,0,_n) // if there is not a duplicate, then dup_MOMID will be 0; if duplicate, it will be >1
keep if dup_INFANTID>0

keep MOMID dup_MOMID PREGID INFANTID dup_INFANTID VISIT_OBSSTDAT POC_VISIT
if dup_MOMID==0 | dup_MOMID==1 | dup_INFANTID==0 | INFANTID==1{
	export excel using "output/daily_duplicate_check_$date.xlsx", sheet("M14", replace) ///
		cell(A1) firstrow(variables)
}

*************************************************
*MNH15 - Infant Vaccination Status 
*************************************************
import delimited using "data/mnh15.CSV", clear 
rename *, upper

** duplicate MOMID ** 
bysort MOMID: gen dup_MOMID=cond(_N==1,0,_n) // if there is not a duplicate, then dup_MOMID will be 0; if duplicate, it will be >1
keep if dup_MOMID>0

** duplicate INFANTID ** 
bysort INFANTID: gen dup_INFANTID=cond(_N==1,0,_n) // if there is not a duplicate, then dup_MOMID will be 0; if duplicate, it will be >1
keep if dup_INFANTID>0

keep MOMID dup_MOMID PREGID INFANTID dup_INFANTID OBSSTDAT OBSTERM
if dup_MOMID==0 | dup_MOMID==1 | dup_INFANTID==0 | INFANTID==1{
	export excel using "output/daily_duplicate_check_$date.xlsx", sheet("M15", replace) ///
		cell(A1) firstrow(variables)
}

*************************************************
*MNH16 - ANC Client Exit Interview 
*************************************************
import delimited using "data/mnh16.CSV", clear 
rename *, upper

bysort MOMID: gen dup_MOMID=cond(_N==1,0,_n) // if there is not a duplicate, then dup_MOMID will be 0; if duplicate, it will be >1
keep if dup_MOMID>0
keep MOMID dup_MOMID PREGID VISDAT
if dup_MOMID==0 | dup_MOMID==1 {
	export excel using "output/daily_duplicate_check_$date.xlsx", sheet("M16", replace) ///
		cell(A1) firstrow(variables)
}

*************************************************
*MNH17 - IPC Client Exit Interview 
*************************************************
import delimited using "data/mnh17.CSV", clear 
rename *, upper

bysort MOMID: gen dup_MOMID=cond(_N==1,0,_n) // if there is not a duplicate, then dup_MOMID will be 0; if duplicate, it will be >1
keep if dup_MOMID>0
keep MOMID dup_MOMID PREGID VISDAT
if dup_MOMID==0 | dup_MOMID==1 {
	export excel using "output/daily_duplicate_check_$date.xlsx", sheet("M17", replace) ///
		cell(A1) firstrow(variables)
}

*************************************************
*MNH18 - PNC Client Exit Interview 
*************************************************
import delimited using "data/mnh18.CSV", clear 
rename *, upper

bysort MOMID: gen dup_MOMID=cond(_N==1,0,_n) // if there is not a duplicate, then dup_MOMID will be 0; if duplicate, it will be >1
keep if dup_MOMID>0
keep MOMID dup_MOMID PREGID VISDAT
if dup_MOMID==0 | dup_MOMID==1 {
	export excel using "output/daily_duplicate_check_$date.xlsx", sheet("M18", replace) ///
		cell(A1) firstrow(variables)
}

*************************************************
*MNH19 - Maternal Hospitalization 
*************************************************
import delimited using "data/mnh19.CSV", clear 
rename *, upper

bysort MOMID: gen dup_MOMID=cond(_N==1,0,_n) // if there is not a duplicate, then dup_MOMID will be 0; if duplicate, it will be >1
keep if dup_MOMID>0
keep MOMID dup_MOMID PREGID OBSSTDAT TIMING_OHOCAT
if dup_MOMID==0 | dup_MOMID==1 {
	export excel using "output/daily_duplicate_check_$date.xlsx", sheet("M19", replace) ///
		cell(A1) firstrow(variables)
}

*************************************************
*MNH20 - Infant Hospitalization 
*************************************************
import delimited using "data/mnh20.CSV", clear 
rename *, upper

** duplicate MOMID ** 
bysort MOMID: gen dup_MOMID=cond(_N==1,0,_n) // if there is not a duplicate, then dup_MOMID will be 0; if duplicate, it will be >1
keep if dup_MOMID>0

** duplicate INFANTID ** 
bysort INFANTID: gen dup_INFANTID=cond(_N==1,0,_n) // if there is not a duplicate, then dup_MOMID will be 0; if duplicate, it will be >1
keep if dup_INFANTID>0

keep MOMID dup_MOMID PREGID INFANTID dup_INFANTID OBSSTDAT
if dup_MOMID==0 | dup_MOMID==1 | dup_INFANTID==0 | INFANTID==1{
	export excel using "output/daily_duplicate_check_$date.xlsx", sheet("M20", replace) ///
		cell(A1) firstrow(variables)
}

*************************************************
*MNH21 - Adverse Events
*************************************************
import delimited using "data/mnh21.CSV", clear 
rename *, upper

** duplicate MOMID ** 
bysort MOMID: gen dup_MOMID=cond(_N==1,0,_n) // if there is not a duplicate, then dup_MOMID will be 0; if duplicate, it will be >1
keep if dup_MOMID>0

** duplicate INFANTID ** 
bysort INFANTID: gen dup_INFANTID=cond(_N==1,0,_n) // if there is not a duplicate, then dup_MOMID will be 0; if duplicate, it will be >1
keep if dup_INFANTID>0

keep MOMID dup_MOMID PREGID INFANTID dup_INFANTID AESTDAT
if dup_MOMID==0 | dup_MOMID==1 | dup_INFANTID==0 | INFANTID==1{
	export excel using "output/daily_duplicate_check_$date.xlsx", sheet("M21", replace) ///
		cell(A1) firstrow(variables)
}

*************************************************
*MNH22 - Protocol Deviation 
*************************************************
import delimited using "data/mnh22.CSV", clear 
rename *, upper

** duplicate MOMID ** 
bysort MOMID: gen dup_MOMID=cond(_N==1,0,_n) // if there is not a duplicate, then dup_MOMID will be 0; if duplicate, it will be >1
keep if dup_MOMID>0

** duplicate INFANTID ** 
bysort INFANTID: gen dup_INFANTID=cond(_N==1,0,_n) // if there is not a duplicate, then dup_MOMID will be 0; if duplicate, it will be >1
keep if dup_INFANTID>0

keep MOMID dup_MOMID PREGID INFANTID dup_INFANTID DVSTDAT
if dup_MOMID==0 | dup_MOMID==1 | dup_INFANTID==0 | INFANTID==1{
	export excel using "output/daily_duplicate_check_$date.xlsx", sheet("M22", replace) ///
		cell(A1) firstrow(variables)
}

*************************************************
*MNH23 - Maternal Closeout 
*************************************************
import delimited using "data/mnh23.CSV", clear 
rename *, upper

** duplicate MOMID ** 
bysort MOMID: gen dup_MOMID=cond(_N==1,0,_n) // if there is not a duplicate, then dup_MOMID will be 0; if duplicate, it will be >1
keep if dup_MOMID>0

** duplicate INFANTID ** 
bysort INFANTID: gen dup_INFANTID=cond(_N==1,0,_n) // if there is not a duplicate, then dup_MOMID will be 0; if duplicate, it will be >1
keep if dup_INFANTID>0

keep MOMID dup_MOMID PREGID INFANTID dup_INFANTID CLOSE_DSSTDAT
if dup_MOMID==0 | dup_MOMID==1 | dup_INFANTID==0 | INFANTID==1{
	export excel using "output/daily_duplicate_check_$date.xlsx", sheet("M23", replace) ///
		cell(A1) firstrow(variables)
}

*************************************************
*MNH24 - Infant Closeout 
*************************************************
import delimited using "data/mnh24.CSV", clear 
rename *, upper

** duplicate MOMID ** 
bysort MOMID: gen dup_MOMID=cond(_N==1,0,_n) // if there is not a duplicate, then dup_MOMID will be 0; if duplicate, it will be >1
keep if dup_MOMID>0

** duplicate INFANTID ** 
bysort INFANTID: gen dup_INFANTID=cond(_N==1,0,_n) // if there is not a duplicate, then dup_MOMID will be 0; if duplicate, it will be >1
keep if dup_INFANTID>0

keep MOMID dup_MOMID PREGID INFANTID dup_INFANTID CLOSE_DSSTDAT
if dup_MOMID==0 | dup_MOMID==1 | dup_INFANTID==0 | INFANTID==1{
	export excel using "output/daily_duplicate_check_$date.xlsx", sheet("M24", replace) ///
		cell(A1) firstrow(variables)
}

*************************************************
*MNH25 - Maternal Depression
*************************************************
import delimited using "data/mnh25.CSV", clear 
rename *, upper

bysort MOMID: gen dup_MOMID=cond(_N==1,0,_n) // if there is not a duplicate, then dup_MOMID will be 0; if duplicate, it will be >1
keep if dup_MOMID>0
keep MOMID dup_MOMID PREGID OBSSTDAT ANC_VISIT_N
if dup_MOMID==0 | dup_MOMID==1 {
	export excel using "output/daily_duplicate_check_$date.xlsx", sheet("M25", replace) ///
		cell(A1) firstrow(variables)
}


*************************************************
*MNH26 - Maternal Fatigue
*************************************************
import delimited using "data/mnh26.CSV", clear 
rename *, upper

bysort MOMID: gen dup_MOMID=cond(_N==1,0,_n) // if there is not a duplicate, then dup_MOMID will be 0; if duplicate, it will be >1
keep if dup_MOMID>0
keep MOMID dup_MOMID PREGID FTGE_OBSTDAT FTGE_STAGE
if dup_MOMID==0 | dup_MOMID==1 {
	export excel using "output/daily_duplicate_check_$date.xlsx", sheet("M26", replace) ///
		cell(A1) firstrow(variables)
}

















