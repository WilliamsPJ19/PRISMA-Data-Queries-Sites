********************************************************************************
*Checking if variables in data are matched with data dictionary

/*Input: 1. PRiSMA-MNH-Data-Dictionary-Repository-V.2.2-FEB012023.xlsx
         2. Data in mnh##.csv format
  Output: log file in .smcl and .pdf format
  Customize: 1.(line 29-32)
             step2:revise global setting as needed 
             2.(line 35)
			 step2:revise form numbers as needed 
			 (only add/remove number ##, for example 00 will call mnh00.csv under data folder)
			 3.(line 57-61)
			 step 3:comment the command for not applicable site 
*/
********************************************************************************

***step1. set up folders under your directory:
/*
1. dictionry (save "PRiSMA-MNH-Data-Dictionary-Repository-V.2.2-FEB012023.xlsx" here)
2. data (save data here) 
3. log (log file will be exported here)
*/

***step2. set up your directory
clear 
set more off
cap log close 

*revise to you local directory/folder!
gl dir="/Users/Xiaoyan/Documents/GWU/data dictionary" 
gl dic="$dir/dictionary"
gl da="$dir/data" 
gl log="$dir/log"

*revise form number as needed!
local form_num 00 01 02 

***step3. Import variable names in data dictionary and save .dta file 
foreach y in 00 01 02 03 04 05 06 07 08 09 10 ///
11 12 13 14 15 16 17 18 19 20 ///
21 22 23 24 25_Zambia 25_Pakistan 25_Ghana 25_India 25_Kenya 26 {
  import excel "$dic/PRiSMA-MNH-Data-Dictionary-Repository-V.2.2-FEB012023.xlsx", ///
    sheet("Data Dictionary") cellrange(A1:E3847) firstrow clear
    drop FormName SectionName Question
    keep if Form == "MNH`y'"
    gen n = _n
	local nvar = _N
    reshape wide VariableName, i(Form) j(n) 
    drop Form
	forval j = 1/`nvar' {
     local varname = strtoname(strtrim(VariableName`j'[1]))
     capture rename VariableName`j'  `varname'
    }
  save "$dic/dict_mnh`y'", replace
}

*revise to comment the command not applicable for each site!
// use "$dic/dict_mnh25_Zambia", clear
use "$dic/dict_mnh25_Pakistan", clear
// use "$dic/dict_mnh25_Ghana", clear
// use "$dic/dict_mnh25_India", clear
// use "$dic/dict_mnh25_Kenya", clear

*save mnh25.dta
save "$dic/dict_mnh25", replace

***step4 start log file
gl today: display %tdCYND date(c(current_date), "DMY")
log using "$log/Core variable check_$today.smcl", replace 
quietly log off //stop log temporarily

***step5. import data to match variable 
*revise form numbers here!
foreach x of local form_num {       
	import delimited using "$da/mnh`x'.csv", clear
rename *, upper
save "$da/data_mnh`x'", replace

***step6. store variable list 
des using "$dic/dict_mnh`x'.dta", varlist
loc dic`x'_vars `r(varlist)'
des using "$da/data_mnh`x'.dta", varlist
loc da`x'_vars `r(varlist)'

***step7. compare variables 
loc common`x': list dic`x'_vars & da`x'_vars
loc dict_only`x': list dic`x'_vars - da`x'_vars
loc data_only`x': list da`x'_vars - dic`x'_vars

***step8. display mismatch part
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

***step9. end log file
quietly log close

translate "$log/Core variable check_$today.smcl" "$log/Core variable check_$today.pdf", replace
