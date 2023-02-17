********************************************************************************
*Checking if variables in data are matched with data dictionary

/*Input: 1. PRiSMA-MNH-Data-Dictionary-Repository-V.2.2-FEB012023_varlist.xlsx
         2. Data in mnh##.csv format
  Output: log file in .smcl and .pdf format
  Customize: step2:
             1. revise global setting as needed 
             2. revise form numbers as needed 
			 (only add/remove number ##, for example 00 will call mnh00.csv under data folder)
*/
********************************************************************************

***step1. set up folders under your directory:
/*
1. dictionry (save "PRiSMA-MNH-Data-Dictionary-Repository-V.2.2-FEB012023_varlist.xlsx" here)
2. data (save data here) 
3. log (log file will be exported here)
*/

***step2. set up your directory
clear 
set more off
cap log close 

gl dir="/Users/Xiaoyan/Documents/GWU/data dictionary" //revise to you local directory!
gl dic="$dir/dictionary"
gl da="$dir/data" //revise if you saved data in different folder!
gl log="$dir/log"

local form_num 00 01 02 //revise form number as needed!

***step3. Import variable names in data dictionary and save .dta file 
foreach form in mnh00 mnh01 mnh02 mnh03 mnh04 mnh05 mnh06 mnh07 mnh08 mnh09 mnh10 ///
mnh11 mnh12 mnh13 mnh14 mnh15 mnh16 mnh17 mnh18 mnh19 mnh20 ///
mnh21 mnh22 mnh23 mnh24 mnh25_Zambia mnh25_Pakistan mnh25_Ghana mnh25_India mnh25_Kenya mnh26 {
import excel "$dic/PRiSMA-MNH-Data-Dictionary-Repository-V.2.2-FEB012023_varlist.xlsx", sheet(`form') firstrow clear
save "$dic/dict_`form'", replace
}

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
loc di`x'_vars `r(varlist)'
des using "$da/data_mnh`x'.dta", varlist
loc da`x'_vars `r(varlist)'

***step7. compare variables 
loc common`x': list di`x'_vars & da`x'_vars
loc dict_only`x': list di`x'_vars - da`x'_vars
loc data_only`x': list da`x'_vars - di`x'_vars

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
