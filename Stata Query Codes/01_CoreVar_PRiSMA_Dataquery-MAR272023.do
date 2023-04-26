********************************************************************************
*QUERY #1 -- CHECK FOR CORE VARIABLE NAMES 
* Written by: Xiaoyan Hu
* Last updated: 21 April 2023

*Input: *1. PRiSMA-MNH-Data-Dictionary-Repository-V.2.3-MAR272023.xlsx
        *2. Data in mnh##.csv format
*Function: check to make sure all variables exist in the data and match data dictionary formatting 
*Output: query file in .smcl and .pdf format

/*Customize: 1. step1: *set up folders under your directory
             2. step2: *revise to you local directory/folder!
             3. step3: *deselect the forms not for query by using /*##*/ !
	     4. step4: *revise to comment the command not applicable for each site! 
*/
********************************************************************************


********************************************************************************
***step1. set up folders under your directory:
********************************************************************************
/*
1. dictionry (save "PRiSMA-MNH-Data-Dictionary-Repository-V.2.3-MAR272023.xlsx" here)
2. data (save data here) 
3. query (query file will be exported here)
*/

********************************************************************************
***step2. set up your directory
********************************************************************************
clear 			//clears the STATA memory of previous data that may be open
set more off 	//allows STATA to run long do files without pausing (no longer necessary after STATA 16)
cap log close 	//closes any log file that may be open due to do-file terminating early

*revise to you local directory/folder!
gl dir="D:/Users/xyh/Documents/Dictionary" 
gl dic="$dir/dictionary"
gl da="$dir/data" 
gl query="$dir/query"

<<<<<<< HEAD
*deselect the data not for query by using /*##*/ !
local form_num 00 01 02 03 04 05 06 07 08 09 10 ///
11 12 13 14 15 16 17 /*18*/ 19 /*20*/ ///
21 22 23 24 25 26

********************************************************************************
***step3. Import variable names in data dictionary and save .dta file 
********************************************************************************
=======
* step3. deselect the forms not for query by using /*##*/ !
local form_num 00 01 02 03 /*04 05 06 07 08 09 10 ///
11 12 13 14 15 16 17 18 19 20 ///
21 22 23 24 25*/ 26

***step4. Import variable names in data dictionary and save .dta file 
>>>>>>> f3d8df8f577c131a335136ea3bf95ea1310c2acf
foreach y in 00 01 02 03 04 05 06 07 08 09 10 ///
11 12 13 14 15 16 17 18 19 20 ///
21 22 23 24 25_Zambia 25_Pakistan 25_Ghana 25_India 25_Kenya 26 {
  import excel "$dic/PRiSMA-MNH-Data-Dictionary-Repository-V.2.3-MAR272023.xlsx", ///
    sheet("Data Dictionary") cellrange(A1:E3952) firstrow clear //cellrange need revise if using different data dictionary
    drop FormName SectionName Question 
    keep if Form == "MNH`y'"
    gen n = _n //index of variable
	local nvar = _N //total n within each form
    reshape wide VariableName, i(Form) j(n) 
    drop Form
	forval j = 1/`nvar' {
     local varname = strtoname(strtrim(VariableName`j'[1]))
     capture rename VariableName`j'  `varname'
    } 
	rename *, upper
  save "$dic/dict_mnh`y'", replace
}

*step5. revise to comment the command not applicable for each site!
// use "$dic/dict_mnh25_Zambia", clear
// use "$dic/dict_mnh25_Pakistan", clear
// use "$dic/dict_mnh25_Ghana", clear
// use "$dic/dict_mnh25_India", clear
use "$dic/dict_mnh25_Kenya", clear

*save mnh25.dta
save "$dic/dict_mnh25", replace

<<<<<<< HEAD
********************************************************************************
***step4 start query file
********************************************************************************
=======
***step6. start query file
>>>>>>> f3d8df8f577c131a335136ea3bf95ea1310c2acf
gl today: display %tdCYND date(c(current_date), "DMY")
log using "$query/Core variable query_$today.smcl", replace 
quietly log off //stop log temporarily

<<<<<<< HEAD
********************************************************************************
***step5. import data to match variable 
********************************************************************************
=======
***step7. import data to match variable 
>>>>>>> f3d8df8f577c131a335136ea3bf95ea1310c2acf
foreach x of local form_num {       
	import delimited using "$da/mnh`x'.csv", clear
rename *, upper
save "$da/mnh`x'", replace

<<<<<<< HEAD
********************************************************************************
***step6. store variable list 
********************************************************************************
=======
***step8. store variable list 
>>>>>>> f3d8df8f577c131a335136ea3bf95ea1310c2acf
des using "$dic/dict_mnh`x'.dta", varlist
loc dic`x'_vars `r(varlist)'
des using "$da/mnh`x'.dta", varlist
loc da`x'_vars `r(varlist)'

<<<<<<< HEAD
********************************************************************************
***step7. compare variables 
********************************************************************************
=======
***step9. compare variables 
>>>>>>> f3d8df8f577c131a335136ea3bf95ea1310c2acf
loc common`x': list dic`x'_vars & da`x'_vars
loc dict_only`x': list dic`x'_vars - da`x'_vars
loc data_only`x': list da`x'_vars - dic`x'_vars

<<<<<<< HEAD
********************************************************************************
***step8. display mismatch part
********************************************************************************
=======
***step10. display mismatch part
>>>>>>> f3d8df8f577c131a335136ea3bf95ea1310c2acf
quietly log on //start log 

di _dup(66) "-" 
di as result "******Core variables in MNH`x'******"
di _dup(66) "-" 
di as text "Matched variables: `common`x''" //will show varialbes in both data dictionary and data
di _newline
di as error "Extra variable in data: `data_only`x''" //will show variables in data but not in data dictionary
di _newline
di as error "Missing variables in data!!!: `dict_only`x''" //will show variables in CRFs/data dictionary but not in data
di _newline 

quietly log off //stop log temporarily
}

********************************************************************************
***step9. end log file
********************************************************************************
quietly log close

translate "$query/Core variable query_$today.smcl" "$query/Core variable query_$today.pdf", replace
