************************************************
*QUERY #2 -- CHECK FOR DUPLICATE IDs
* Written by: Xiaoyan Hu & Stacie Loisate
* Last updated: 21 April 2023

*Input: Data in mnh##.csv format
*Function: identify any duplicate IDs 
*Output: .xlsx file with all duplicate IDs and MOMID with missing enrollment 

/*Customize: 1. step2: revise global setting to own local 
             2. step3: include the form numbers to check (deselect forms not available using /*##*/)
			 !!!don't move forms among different local definitions and don't delete forms
			 3. step5: deselect the forms not for query by using /*##*/ (These are forms to match MOMID with mnh02)
*/
********************************************************************************

***step1. set up folders under your directory:
/*
1. data (save data here) 
2. query (query file will be exported here)
*/

***step2. set up your directory
clear 			//clears the STATA memory of previous data that may be open
set more off 	//allows STATA to run long do files without pausing (no longer necessary after STATA 16)
cap log close 	//closes any log file that may be open due to do-file terminating early

*revise to you local directory/folder!
gl dir="D:/Users/xyh/Documents/Dictionary" 
gl da="$dir/data" 
gl query="$dir/query"
gl today: display %tdCYND date(c(current_date), "DMY")

***step3. define local forms (deselect forms not available using /*##*/)
*include the form number to check
*forms using SCRNID to identify duplicates (form 00 02)
local enroll_form 00 02 
*forms using SCRNID and VISIT_TYPE to identify duplicates (form 01)
local us_form 01
*forms using MOMID and PREGID to identify duplicates (form 03 09 10 16 17 18 19 23)
local mat_form 03 /*09 10 16 17 18 19 23*/
*forms using MOMID, PREGID and VISIT_TYPE to identify duplicates (form 04 05 06 07 08 12 25 26)
local multi_mat_form /*04 05 06 07 08 12 25 26 */
*forms using MOMID, PREGID and visitDate to identify duplicates (form 19)
local random_mat_form /*19*/
*forms using MOMID, PREGID, INFANTID to identify duplicates (form 11 22 24)
local inf_form /*11 22 24*/
*forms using MOMID, PREGID, INFANTID and VISIT_TYPE to identify duplicates (form 13 14 15)
local multi_inf_form /*13 14 15*/
*forms using MOMID, PREGID, INFANTID and visitDate to identify duplicates (form 20 21)
local random_inf_form /*20 21*/

***step4. rund duplicates check query and export duplicats in each form
***forms using SCRNID to identify duplicates
foreach x of local enroll_form {       
import delimited using "$da/mnh`x'.csv", clear
rename *, upper

bysort SCRNID MOMID PREGID: gen dup_id=cond(_N==1,0,_n) // if no duplicate, then dup_id will be 0; if duplicate, it will be >0

preserve
*save query report 
keep if dup_id > 0
keep SCRNID MOMID PREGID dup_id
if dup_id==0 | dup_id==1 {
	export excel using "$query/duplicate_check_$today.xlsx", sheet("MNH`x'", replace) ///
		cell(A1) firstrow(variables)
}
		
restore
*save data with no duplciates ID for later use
keep if dup_id == 0 | dup_id == 1
save "$da/mnh`x'", replace
}

***forms using SCRNID and VISIT_TYPE to identify duplicates
foreach x of local us_form {       
import delimited using "$da/mnh`x'.csv", clear
rename *, upper

bysort SCRNID TYPE_VISIT: gen dup_id=cond(_N==1,0,_n) // if no duplicate, then dup_id will be 0; if duplicate, it will be >0

preserve
*save query report
keep if dup_id > 0
keep SCRNID MOMID PREGID TYPE_VISIT dup_id
if (dup_id==0 | dup_id==1) & TYPE_VISIT != 13 & TYPE_VISIT != 14 {
	export excel using "$query/duplicate_check_$today.xlsx", sheet("MNH`x'", replace) ///
		cell(A1) firstrow(variables)
}
				
restore
*save data with no duplciates ID for later use
keep if dup_id == 0 | dup_id == 1
save "$da/mnh`x'", replace
}

*forms using MOMID and PREGID to identify duplicates
foreach x of local mat_form {       
import delimited using "$da/mnh`x'.csv", clear
rename *, upper

bysort MOMID PREGID: gen dup_id=cond(_N==1,0,_n) // if no duplicate, then dup_id will be 0; if duplicate, it will be >0

preserve
*save query report
keep if dup_id > 0
keep MOMID PREGID dup_id
if (dup_id==0 | dup_id==1) {
	export excel using "$query/duplicate_check_$today.xlsx", sheet("MNH`x'", replace) ///
		cell(A1) firstrow(variables)
}
				
restore
*save data with no duplciates ID for later use
keep if dup_id == 0 | dup_id == 1
save "$da/mnh`x'", replace
}

*forms using MOMID, PREGID and VISIT_TYPE to identify duplicates
foreach x of local multi_mat_form {       
import delimited using "$da/mnh`x'.csv", clear
rename *, upper

bysort MOMID PREGID TYPE_VISIT: gen dup_id=cond(_N==1,0,_n) // if no duplicate, then dup_id will be 0; if duplicate, it will be >0

preserve
*save query report
keep if dup_id > 0
keep MOMID PREGID TYPE_VISIT dup_id
if (dup_id==0 | dup_id==1) & TYPE_VISIT != 13 & TYPE_VISIT != 14 {
	export excel using "$query/duplicate_check_$today.xlsx", sheet("MNH`x'", replace) ///
		cell(A1) firstrow(variables)
}	
			
restore
*save data with no duplciates ID for later use
keep if dup_id == 0 | dup_id == 1
save "$da/mnh`x'", replace
}

*forms using MOMID, PREGID and visitDate to identify duplicates
foreach x of local random_mat_form {       
import delimited using "$da/mnh`x'.csv", clear
rename *, upper

bysort MOMID PREGID OBSSTDAT: gen dup_id=cond(_N==1,0,_n) // if no duplicate, then dup_id will be 0; if duplicate, it will be >0

preserve
*save query report
keep if dup_id > 0
keep MOMID PREGID OBSSTDAT dup_id
if (dup_id==0 | dup_id==1) {
	export excel using "$query/duplicate_check_$today.xlsx", sheet("MNH`x'", replace) ///
		cell(A1) firstrow(variables)
}
				
restore
*save data with no duplciates ID for later use
keep if dup_id == 0 | dup_id == 1
save "$da/mnh`x'", replace
}

*forms using MOMID, PREGID, INFANTID to identify duplicates
foreach x of local inf_form {       
import delimited using "$da/mnh`x'.csv", clear
rename *, upper

bysort MOMID PREGID INFANTID: gen dup_id=cond(_N==1,0,_n) // if no duplicate, then dup_id will be 0; if duplicate, it will be >0

preserve
*save query report
keep if dup_id > 0
keep MOMID PREGID INFANTID dup_id
if (dup_id==0 | dup_id==1){
	export excel using "$query/duplicate_check_$today.xlsx", sheet("MNH`x'", replace) ///
		cell(A1) firstrow(variables)
}
				
restore
*save data with no duplciates ID for later use
keep if dup_id == 0 | dup_id == 1
save "$da/mnh`x'", replace
}

*forms using MOMID, PREGID, INFANTID and VISIT_TYPE to identify duplicates
foreach x of local inf_form {       
import delimited using "$da/mnh`x'.csv", clear
rename *, upper

bysort MOMID PREGID INFANTID TYPE_VISIT: gen dup_id=cond(_N==1,0,_n) // if no duplicate, then dup_id will be 0; if duplicate, it will be >0

preserve
*save query report
keep if dup_id > 0
keep MOMID PREGID INFANTID TYPE_VISIT dup_id
if (dup_id==0 | dup_id==1) & TYPE_VISIT != 13 & TYPE_VISIT != 14 {
	export excel using "$query/duplicate_check_$today.xlsx", sheet("MNH`x'", replace) ///
		cell(A1) firstrow(variables)
}
				
restore
*save data with no duplciates ID for later use
keep if dup_id == 0 | dup_id == 1
save "$da/mnh`x'", replace
}

*forms using MOMID, PREGID, INFANTID and vistDate to identify duplicates
foreach x of local random_inf_form {       
import delimited using "$da/mnh`x'.csv", clear
rename *, upper
*define visitDate 
capture confirm variable OBSSTDAT
if !_rc {
	gen vistDate = OBSSTDAT
}
capture confirm variable AESTDAT
if !_rc {
	gen vistDate = AESTDAT
	}

bysort MOMID PREGID INFANTID vistDate: gen dup_id=cond(_N==1,0,_n) // if no duplicate, then dup_id will be 0; if duplicate, it will be >0

preserve
*save query report
keep if dup_id > 0
keep MOMID PREGID INFANTID vistDate dup_id
if (dup_id==0 | dup_id==1) {
	export excel using "$query/duplicate_check_$today.xlsx", sheet("MNH`x'", replace) ///
		cell(A1) firstrow(variables)
}
				
restore
*save data with no duplciates ID for later use
keep if dup_id == 0 | dup_id == 1
save "$da/mnh`x'", replace
}

*************************************************************************************
* comparing mom id 
* This code will check that all moms who are enrolled had an enrollment form 
*************************************************************************************
*step 5. define all forms to check MOMID not in enrollment form
*deselect the forms not for query by using /*##*/ (These are forms to match MOMID with mnh02)
local momid_form 03 /*04 05 06 07 08 09 10*/ ///
/*11 12 13 14 15 16 17 18 19 20*/ ///
/*21 22 23 24 25 */ 26

*create temp files for each form with just momid and pregid 
 foreach  y of local momid_form {
	import delimited using "$da/mnh`y'.csv", clear
	rename *, upper
	keep MOMID PREGID
	tempfile mnh`y'
	save `mnh`y''
 } 
 
*merge all forms except enrollment
 foreach y of local momid_form {
	merge 1:1 MOMID PREGID using `mnh`y''
	drop _merge
}

*step 6. export MomID don't have an enrollment form
merge 1:1 MOMID PREGID using "$da/mnh02.dta", generate (miss_enroll)
keep if miss_enroll == 1
keep MOMID PREGID 
export excel using "$query/duplicate_check_$today.xlsx", ///
sheet("Missing enrollment", replace) cell(A1) firstrow(variables)
