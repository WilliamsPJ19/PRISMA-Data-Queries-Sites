********************************************************************************
*Checking if variables in data are matched with data dictionary

*Step 1 to 3 need to run once;
*Step 4 and 9 need to run everytime to start the check
*Step 5 to 8 need to run for every form for check (find the form number and run step 5-8 under it)
********************************************************************************

***step1. set up three folders under your directory:
/*
1. dictionry (save PRiSMA-MNH-Data-Dictionary-Repository_V.2.0-OCT182022_varlist.xlsx here)
2. data (save new data here)
3. log (will export checking result in log)
*/

***step2. set up your directory
clear 
set more off
cap log close 

gl dir="/Users/Xiaoyan/Documents/GWU/data dictionary"
gl dic="$dir/dictionary"
gl da="$dir/data"
gl log="$dir/log"

***step3. Import variable names in data dictionary and save .dta file - revise variable names in form tab in excel accordingly if CRF changed
foreach form in mnh00 mnh01 mnh02 mnh03 mnh04 mnh05 mnh06 mnh07 mnh08 mnh09 mnh10 ///
mnh11 mnh12 mnh13 mnh14 mnh15 mnh16 mnh17 mnh18 mnh19 mnh20 ///
mnh21 mnh22 mnh23 mnh24 mnh25_Zambia mnh25_Pakistan mnh25_Ghana mnh25_India mnh25_Kenya mnh26 {
import excel "$dic/PRiSMA-MNH-Data-Dictionary-Repository_V.2.1-OCT182022_varlist.xlsx", sheet(`form') firstrow clear
save "$dic/dict_`form'", replace
}

***step4 start log file
gl today: display %tdCYND date(c(current_date), "DMY")
log using "$log/Core variable check_$today.smcl", replace 
quietly log off //stop log temporarily

************************************
*MNH00 Pre-Screening Form
************************************

***step5. import data to match variable 
import delimited using "$da/mnh00.csv", clear
rename *, upper
save "$da/data_mnh00", replace

***step6. store variable list 
des using "$dic/dict_mnh00.dta", varlist
loc di00_vars `r(varlist)'
des using "$da/data_mnh00.dta", varlist
loc da00_vars `r(varlist)'

***step7. compare variables 
loc common00: list di00_vars & da00_vars
loc dict_only00: list di00_vars - da00_vars
loc data_only00: list da00_vars - di00_vars

***step8. display mismatch part
quietly log on //start log 

di in red "******Core variables in MNH00******"
di  "Matched variables: `common00'" //will show varialbes in both data dictionary and data
di "Extra variable in data: `data_only00'" //will show variables in data but not in data dictionary
di as error "Missing variables in data!!!: `dict_only00'" //will show variables in CRFs/data dictionary but not in data

quietly log off //stop log temporarily

************************************
*MNH01 Ultrasound Exam
************************************

***step5. import data to match variable 
import delimited using "$da/mnh01.csv", clear
rename *, upper
save "$da/data_mnh01", replace

***step6. store variable list 
des using "$dic/dict_mnh01.dta", varlist
loc di01_vars `r(varlist)'
des using "$da/data_mnh01.dta", varlist
loc da01_vars `r(varlist)'

***step7. compare variables 
loc common01: list di01_vars & da01_vars
loc dict_only01: list di01_vars - da01_vars
loc data_only01: list da01_vars - di01_vars

***step8. display mismatch part
quietly log on //start log 

di in red "******Core variables in MNH01******"
di  "Matched variables: `common01'" //will show varialbes in both data dictionary and data
di "Extra variable in data: `data_only01'" //will show variables in data but not in data dictionary
di as error "Missing variables in data!!!: `dict_only01'" //will show variables in CRFs/data dictionary but not in data

quietly log off //stop log temporarily

************************************
*MNH02 Enrollment Status Form
************************************

***step5. import data to match variable 
import delimited using "$da/mnh02.csv", clear
rename *, upper
save "$da/data_mnh02", replace

***step6. store variable list 
des using "$dic/dict_mnh02.dta", varlist
loc di02_vars `r(varlist)'
des using "$da/data_mnh02.dta", varlist
loc da02_vars `r(varlist)'

***step7. compare variables 
loc common02: list di02_vars & da02_vars
loc dict_only02: list di02_vars - da02_vars
loc data_only02: list da02_vars - di02_vars

***step8. display mismatch part
quietly log on //start log 

di in red "******Core variables in MNH02******"
di  "Matched variables: `common02'" //will show varialbes in both data dictionary and data 
di "Extra variable in data: `data_only02'" //will show variables in data but not in data dictionary
di as error "Missing variables in data!!!: `dict_only02'" //will show variables in CRFs/data dictionary but not in data

quietly log off //stop log temporarily

************************************
*MNH03 Sociodemographic Form
************************************

***step5. import data to match variable 
import delimited using "$da/mnh03.csv", clear
rename *, upper
save "$da/data_mnh03", replace

***step6. store variable list 
des using "$dic/dict_mnh03.dta", varlist
loc di03_vars `r(varlist)'
des using "$da/data_mnh03.dta", varlist
loc da03_vars `r(varlist)'

***step7. compare variables 
loc common03: list di03_vars & da03_vars
loc dict_only03: list di03_vars - da03_vars
loc data_only03: list da03_vars - di03_vars

***step8. display mismatch part
quietly log on //start log 

di in red "******Core variables in MNH03******"
di  "Matched variables: `common03'" //will show varialbes in both data dictionary and data
di "Extra variable in data: `data_only03'" //will show variables in data but not in data dictionary
di as error "Missing variables in data!!!: `dict_only03'" //will show variables in CRFs/data dictionary but not in data

quietly log off //stop log temporarily

************************************
*MNH04 ANC Clinical Status Form
************************************

***step5. import data to match variable 
import delimited using "$da/mnh04.csv", clear
rename *, upper
save "$da/data_mnh04", replace

***step6. store variable list 
des using "$dic/dict_mnh04.dta", varlist
loc di04_vars `r(varlist)'
des using "$da/data_mnh04.dta", varlist
loc da04_vars `r(varlist)'

***step7. compare variables 
loc common04: list di04_vars & da04_vars
loc dict_only04: list di04_vars - da04_vars
loc data_only04: list da04_vars - di04_vars

***step8. display mismatch part
quietly log on //start log 

di in red "******Core variables in MNH04******"
di  "Matched variables: `common04'" //will show varialbes in both data dictionary and data 
di "Extra variable in data: `data_only04'" //will show variables in data but not in data dictionary
di as error "Missing variables in data!!!: `dict_only04'" //will show variables in CRFs/data dictionary but not in data

quietly log off //stop log temporarily

************************************
*MNH05 Maternal Anthropometry Form
************************************

***step5. import data to match variable 
import delimited using "$da/mnh05.csv", clear
rename *, upper
save "$da/data_mnh05", replace

***step6. store variable list 
des using "$dic/dict_mnh05.dta", varlist
loc di05_vars `r(varlist)'
des using "$da/data_mnh05.dta", varlist
loc da05_vars `r(varlist)'

***step7. compare variables 
loc common05: list di05_vars & da05_vars
loc dict_only05: list di05_vars - da05_vars
loc data_only05: list da05_vars - di05_vars

***step8. display mismatch part
quietly log on //start log 

di in red "******Core variables in MNH05******"
di  "Matched variables: `common05'" //will show varialbes in both data dictionary and data
di "Extra variable in data: `data_only05'" //will show variables in data but not in data dictionary
di as error "Missing variables in data!!!: `dict_only05'" //will show variables in CRFs/data dictionary but not in data

quietly log off //stop log temporarily

************************************
*MNH06 Maternal Point-of-Care (PoC) Diagnostics
************************************

***step5. import data to match variable 
import delimited using "$da/mnh06.csv", clear
rename *, upper
save "$da/data_mnh06", replace

***step6. store variable list 
des using "$dic/dict_mnh06.dta", varlist
loc di06_vars `r(varlist)'
des using "$da/data_mnh06.dta", varlist
loc da06_vars `r(varlist)'

***step7. compare variables 
loc common06: list di06_vars & da06_vars
loc dict_only06: list di06_vars - da06_vars
loc data_only06: list da06_vars - di06_vars

***step8. display mismatch part
quietly log on //start log 

di in red "******Core variables in MNH06******"
di  "Matched variables: `common06'" //will show varialbes in both data dictionary and data
di "Extra variable in data: `data_only06'" //will show variables in data but not in data dictionary
di as error "Missing variables in data!!!: `dict_only06'" //will show variables in CRFs/data dictionary but not in data

quietly log off //stop log temporarily

************************************
*MNH07 Maternal Specimen Collection
************************************

***step5. import data to match variable 
import delimited using "$da/mnh07.csv", clear
rename *, upper
save "$da/data_mnh07", replace

***step6. store variable list 
des using "$dic/dict_mnh07.dta", varlist
loc di07_vars `r(varlist)'
des using "$da/data_mnh07.dta", varlist
loc da07_vars `r(varlist)'

***step7. compare variables 
loc common07: list di07_vars & da07_vars
loc dict_only07: list di07_vars - da07_vars
loc data_only07: list da07_vars - di07_vars

***step8. display mismatch part
quietly log on //start log 

di in red "******Core variables in MNH07******"
di  "Matched variables: `common07'" //will show varialbes in both data dictionary and data
di "Extra variable in data: `data_only07'" //will show variables in data but not in data dictionary
di as error "Missing variables in data!!!: `dict_only07'" //will show variables in CRFs/data dictionary but not in data

quietly log off //stop log temporarily

************************************
*MNH08 Maternal Laboratory Results
************************************

***step5. import data to match variable 
import delimited using "$da/mnh08.csv", clear
rename *, upper
save "$da/data_mnh08", replace

***step6. store variable list 
des using "$dic/dict_mnh08.dta", varlist
loc di08_vars `r(varlist)'
des using "$da/data_mnh08.dta", varlist
loc da08_vars `r(varlist)'

***step7. compare variables 
loc common08: list di08_vars & da08_vars
loc dict_only08: list di08_vars - da08_vars
loc data_only08: list da08_vars - di08_vars

***step8. display mismatch part
quietly log on //start log 

di in red "******Core variables in MNH08******"
di  "Matched variables: `common08'" //will show varialbes in both data dictionary and data
di "Extra variable in data: `data_only08'" //will show variables in data but not in data dictionary
di as error "Missing variables in data!!!: `dict_only08'" //will show variables in CRFs/data dictionary but not in data

quietly log off //stop log temporarily

************************************
*MNH09 Maternal Labor and Delivery Outcome
************************************

***step5. import data to match variable 
import delimited using "$da/mnh09.csv", clear
rename *, upper
save "$da/data_mnh09", replace

***step6. store variable list 
des using "$dic/dict_mnh09.dta", varlist
loc di09_vars `r(varlist)'
des using "$da/data_mnh09.dta", varlist
loc da09_vars `r(varlist)'

***step7. compare variables 
loc common09: list di09_vars & da09_vars
loc dict_only09: list di09_vars - da09_vars
loc data_only09: list da09_vars - di09_vars

***step8. display mismatch part
quietly log on //start log 

di in red "******Core variables in MNH09******"
di  "Matched variables: `common09'" //will show varialbes in both data dictionary and data
di "Extra variable in data: `data_only09'" //will show variables in data but not in data dictionary
di as error "Missing variables in data!!!: `dict_only09'" //will show variables in CRFs/data dictionary but not in data

quietly log off //stop log temporarily

************************************
*MNH10 Maternal Post-Delivery Outcome
************************************

***step5. import data to match variable 
import delimited using "$da/mnh10.csv", clear
rename *, upper
save "$da/data_mnh10", replace

***step6. store variable list 
des using "$dic/dict_mnh10.dta", varlist
loc di10_vars `r(varlist)'
des using "$da/data_mnh10.dta", varlist
loc da10_vars `r(varlist)'

***step7. compare variables 
loc common10: list di10_vars & da10_vars
loc dict_only10: list di10_vars - da10_vars
loc data_only10: list da10_vars - di10_vars

***step8. display mismatch part
quietly log on //start log 

di in red "******Core variables in MNH10******"
di  "Matched variables: `common10'" //will show varialbes in both data dictionary and data
di "Extra variable in data: `data_only10'" //will show variables in data but not in data dictionary
di as error "Missing variables in data!!!: `dict_only10'" //will show variables in CRFs/data dictionary but not in data

quietly log off //stop log temporarily

************************************
*MNH11 Newborn Birth Outcome Form
************************************

***step5. import data to match variable 
import delimited using "$da/mnh11.csv", clear
rename *, upper
save "$da/data_mnh11", replace

***step6. store variable list 
des using "$dic/dict_mnh11.dta", varlist
loc di11_vars `r(varlist)'
des using "$da/data_mnh11.dta", varlist
loc da11_vars `r(varlist)'

***step7. compare variables 
loc common11: list di11_vars & da11_vars
loc dict_only11: list di11_vars - da11_vars
loc data_only11: list da11_vars - di11_vars

***step8. display mismatch part
quietly log on //start log 

di in red "******Core variables in MNH11******"
di  "Matched variables: `common11'" //will show varialbes in both data dictionary and data
di "Extra variable in data: `data_only11'" //will show variables in data but not in data dictionary
di as error "Missing variables in data!!!: `dict_only11'" //will show variables in CRFs/data dictionary but not in data

quietly log off //stop log temporarily

************************************
*MNH12 Maternal PNC Clinical Status
************************************

***step5. import data to match variable 
import delimited using "$da/mnh12.csv", clear
rename *, upper
save "$da/data_mnh12", replace

***step6. store variable list 
des using "$dic/dict_mnh12.dta", varlist
loc di12_vars `r(varlist)'
des using "$da/data_mnh12.dta", varlist
loc da12_vars `r(varlist)'

***step7. compare variables 
loc common12: list di12_vars & da12_vars
loc dict_only12: list di12_vars - da12_vars
loc data_only12: list da12_vars - di12_vars

***step8. display mismatch part
quietly log on //start log 

di in red "******Core variables in MNH12******"
di  "Matched variables: `common12'" //will show varialbes in both data dictionary and data
di "Extra variable in data: `data_only12'" //will show variables in data but not in data dictionary
di as error "Missing variables in data!!!: `dict_only12'" //will show variables in CRFs/data dictionary but not in data

quietly log off //stop log temporarily

************************************
*MNH13 Infant PNC Clinical Status
************************************

***step5. import data to match variable 
import delimited using "$da/mnh13.csv", clear
rename *, upper
save "$da/data_mnh13", replace

***step6. store variable list 
des using "$dic/dict_mnh13.dta", varlist
loc di13_vars `r(varlist)'
des using "$da/data_mnh13.dta", varlist
loc da13_vars `r(varlist)'

***step7. compare variables 
loc common13: list di13_vars & da13_vars
loc dict_only13: list di13_vars - da13_vars
loc data_only13: list da13_vars - di13_vars

***step8. display mismatch part
quietly log on //start log 

di in red "******Core variables in MNH13******"
di  "Matched variables: `common13'" //will show varialbes in both data dictionary and data
di "Extra variable in data: `data_only13'" //will show variables in data but not in data dictionary
di as error "Missing variables in data!!!: `dict_only13'" //will show variables in CRFs/data dictionary but not in data

quietly log off //stop log temporarily

************************************
*MNH14 Infant Point-of-Care Diagnostics 
************************************

***step5. import data to match variable 
import delimited using "$da/mnh14.csv", clear
rename *, upper
save "$da/data_mnh14", replace

***step6. store variable list 
des using "$dic/dict_mnh14.dta", varlist
loc di14_vars `r(varlist)'
des using "$da/data_mnh14.dta", varlist
loc da14_vars `r(varlist)'

***step7. compare variables 
loc common14: list di14_vars & da14_vars
loc dict_only14: list di14_vars - da14_vars
loc data_only14: list da14_vars - di14_vars

***step8. display mismatch part
quietly log on //start log 

di in red "******Core variables in MNH14******"
di  "Matched variables: `common14'" //will show varialbes in both data dictionary and data
di "Extra variable in data: `data_only14'" //will show variables in data but not in data dictionary
di as error "Missing variables in data!!!: `dict_only14'" //will show variables in CRFs/data dictionary but not in data

quietly log off //stop log temporarily

************************************
*MNH15 Infant Vaccination Form
************************************

***step5. import data to match variable 
import delimited using "$da/mnh15.csv", clear
rename *, upper
save "$da/data_mnh15", replace

***step6. store variable list 
des using "$dic/dict_mnh15.dta", varlist
loc di15_vars `r(varlist)'
des using "$da/data_mnh15.dta", varlist
loc da15_vars `r(varlist)'

***step7. compare variables 
loc common15: list di15_vars & da15_vars
loc dict_only15: list di15_vars - da15_vars
loc data_only15: list da15_vars - di15_vars

***step8. display mismatch part
quietly log on //start log 

di in red "******Core variables in MNH15******"
di  "Matched variables: `common15'" //will show varialbes in both data dictionary and data
di "Extra variable in data: `data_only15'" //will show variables in data but not in data dictionary
di as error "Missing variables in data!!!: `dict_only15'" //will show variables in CRFs/data dictionary but not in data

quietly log off //stop log temporarily

************************************
*MNH16 Antenatal Care Client Exit Interview
************************************

***step5. import data to match variable 
import delimited using "$da/mnh16.csv", clear
rename *, upper
save "$da/data_mnh16", replace

***step6. store variable list 
des using "$dic/dict_mnh16.dta", varlist
loc di16_vars `r(varlist)'
des using "$da/data_mnh16.dta", varlist
loc da16_vars `r(varlist)'

***step7. compare variables 
loc common16: list di16_vars & da16_vars
loc dict_only16: list di16_vars - da16_vars
loc data_only16: list da16_vars - di16_vars

***step8. display mismatch part
quietly log on //start log 

di in red "******Core variables in MNH16******"
di  "Matched variables: `common16'" //will show varialbes in both data dictionary and data
di "Extra variable in data: `data_only16'" //will show variables in data but not in data dictionary
di as error "Missing variables in data!!!: `dict_only16'" //will show variables in CRFs/data dictionary but not in data

quietly log off //stop log temporarily

************************************
*MNH17 Intrapartum Care Client Exit Interview
************************************

***step5. import data to match variable 
import delimited using "$da/mnh17.csv", clear
rename *, upper
save "$da/data_mnh17", replace

***step6. store variable list 
des using "$dic/dict_mnh17.dta", varlist
loc di17_vars `r(varlist)'
des using "$da/data_mnh17.dta", varlist
loc da17_vars `r(varlist)'

***step7. compare variables 
loc common17: list di17_vars & da17_vars
loc dict_only17: list di17_vars - da17_vars
loc data_only17: list da17_vars - di17_vars

***step8. display mismatch part
quietly log on //start log 

di in red "******Core variables in MNH17******"
di  "Matched variables: `common17'" //will show varialbes in both data dictionary and data
di "Extra variable in data: `data_only17'" //will show variables in data but not in data dictionary
di as error "Missing variables in data!!!: `dict_only17'" //will show variables in CRFs/data dictionary but not in data

quietly log off //stop log temporarily

************************************
*MNH18 Postnatal Care Client Exit Interview
************************************

***step5. import data to match variable 
import delimited using "$da/mnh18.csv", clear
rename *, upper
save "$da/data_mnh18", replace

***step6. store variable list 
des using "$dic/dict_mnh18.dta", varlist
loc di18_vars `r(varlist)'
des using "$da/data_mnh18.dta", varlist
loc da18_vars `r(varlist)'

***step7. compare variables 
loc common18: list di18_vars & da18_vars
loc dict_only18: list di18_vars - da18_vars
loc data_only18: list da18_vars - di18_vars

***step8. display mismatch part
quietly log on //start log 

di in red "******Core variables in MNH18******"
di  "Matched variables: `common18'" //will show varialbes in both data dictionary and data
di "Extra variable in data: `data_only18'" //will show variables in data but not in data dictionary
di as error "Missing variables in data!!!: `dict_only18'" //will show variables in CRFs/data dictionary but not in data

quietly log off //stop log temporarily

************************************
*MNH19 Maternal Hospitalization
************************************

***step5. import data to match variable 
import delimited using "$da/mnh19.csv", clear
rename *, upper
save "$da/data_mnh19", replace

***step6. store variable list 
des using "$dic/dict_mnh19.dta", varlist
loc di19_vars `r(varlist)'
des using "$da/data_mnh19.dta", varlist
loc da19_vars `r(varlist)'

***step7. compare variables 
loc common19: list di19_vars & da19_vars
loc dict_only19: list di19_vars - da19_vars
loc data_only19: list da19_vars - di19_vars

***step8. display mismatch part
quietly log on //start log 

di in red "******Core variables in MNH19******"
di  "Matched variables: `common19'" //will show varialbes in both data dictionary and data
di "Extra variable in data: `data_only19'" //will show variables in data but not in data dictionary
di as error "Missing variables in data!!!: `dict_only19'" //will show variables in CRFs/data dictionary but not in data

quietly log off //stop log temporarily

************************************
*MNH20 Infant Hospitalization Form
************************************

***step5. import data to match variable 
import delimited using "$da/mnh20.csv", clear
rename *, upper
save "$da/data_mnh20", replace

***step6. store variable list 
des using "$dic/dict_mnh20.dta", varlist
loc di20_vars `r(varlist)'
des using "$da/data_mnh20.dta", varlist
loc da20_vars `r(varlist)'

***step7. compare variables 
loc common20: list di20_vars & da20_vars
loc dict_only20: list di20_vars - da20_vars
loc data_only20: list da20_vars - di20_vars

***step8. display mismatch part
quietly log on //start log 

di in red "******Core variables in MNH20******"
di  "Matched variables: `common20'" //will show varialbes in both data dictionary and data
di "Extra variable in data: `data_only20'" //will show variables in data but not in data dictionary
di as error "Missing variables in data!!!: `dict_only20'" //will show variables in CRFs/data dictionary but not in data

quietly log off //stop log temporarily


************************************
*MNH21 Adverse Events
************************************

***step5. import data to match variable 
import delimited using "$da/mnh21.csv", clear
rename *, upper
save "$da/data_mnh21", replace

***step6. store variable list 
des using "$dic/dict_mnh21.dta", varlist
loc di21_vars `r(varlist)'
des using "$da/data_mnh21.dta", varlist
loc da21_vars `r(varlist)'

***step7. compare variables 
loc common21: list di21_vars & da21_vars
loc dict_only21: list di21_vars - da21_vars
loc data_only21: list da21_vars - di21_vars

***step8. display mismatch part
quietly log on //start log 

di in red "******Core variables in MNH21******"
di  "Matched variables: `common21'" //will show varialbes in both data dictionary and data
di "Extra variable in data: `data_only21'" //will show variables in data but not in data dictionary
di as error "Missing variables in data!!!: `dict_only21'" //will show variables in CRFs/data dictionary but not in data

quietly log off //stop log temporarily

************************************
*MNH22 Protocol Deviation
************************************

***step5. import data to match variable 
import delimited using "$da/mnh22.csv", clear
rename *, upper
save "$da/data_mnh22", replace

***step6. store variable list 
des using "$dic/dict_mnh22.dta", varlist
loc di22_vars `r(varlist)'
des using "$da/data_mnh22.dta", varlist
loc da22_vars `r(varlist)'

***step7. compare variables 
loc common22: list di22_vars & da22_vars
loc dict_only22: list di22_vars - da22_vars
loc data_only22: list da22_vars - di22_vars

***step8. display mismatch part
quietly log on //start log 

di in red "******Core variables in MNH22******"
di  "Matched variables: `common22'" //will show varialbes in both data dictionary and data
di "Extra variable in data: `data_only22'" //will show variables in data but not in data dictionary
di as error "Missing variables in data!!!: `dict_only22'" //will show variables in CRFs/data dictionary but not in data

quietly log off //stop log temporarily

************************************
*MNH23 Maternal Close-out Form
************************************

***step5. import data to match variable 
import delimited using "$da/mnh23.csv", clear
rename *, upper
save "$da/data_mnh23", replace

***step6. store variable list 
des using "$dic/dict_mnh23.dta", varlist
loc di23_vars `r(varlist)'
des using "$da/data_mnh23.dta", varlist
loc da23_vars `r(varlist)'

***step7. compare variables 
loc common23: list di23_vars & da23_vars
loc dict_only23: list di23_vars - da23_vars
loc data_only23: list da23_vars - di23_vars

***step8. display mismatch part
quietly log on //start log 

di in red "******Core variables in MNH23******"
di  "Matched variables: `common23'" //will show varialbes in both data dictionary and data
di "Extra variable in data: `data_only23'" //will show variables in data but not in data dictionary
di as error "Missing variables in data!!!: `dict_only23'" //will show variables in CRFs/data dictionary but not in data

quietly log off //stop log temporarily

************************************
*MNH24 Infant Close-out Form
************************************

***step5. import data to match variable 
import delimited using "$da/mnh24.csv", clear
rename *, upper
save "$da/data_mnh24", replace

***step6. store variable list 
des using "$dic/dict_mnh24.dta", varlist
loc di24_vars `r(varlist)'
des using "$da/data_mnh24.dta", varlist
loc da24_vars `r(varlist)'

***step7. compare variables 
loc common24: list di24_vars & da24_vars
loc dict_only24: list di24_vars - da24_vars
loc data_only24: list da24_vars - di24_vars

***step8. display mismatch part
quietly log on //start log 

di in red "******Core variables in MNH24******"
di  "Matched variables: `common24'" //will show varialbes in both data dictionary and data
di "Extra variable in data: `data_only24'" //will show variables in data but not in data dictionary
di as error "Missing variables in data!!!: `dict_only24'" //will show variables in CRFs/data dictionary but not in data

quietly log off //stop log temporarily

************************************
*MNH25 Maternal Depression Form 
************************************

***step5. import data to match variable 
import delimited using "$da/mnh25.csv", clear
rename *, upper
save "$da/data_mnh25", replace

***step6. store variable list 
des using "$dic/xxxxxx.dta", varlist //replace xxxxxx with dict_mnh25_country, for example dict_mnh25_Pakistan
loc di25_vars `r(varlist)'
des using "$da/data_mnh25.dta", varlist
loc da25_vars `r(varlist)'

***step7. compare variables 
loc common25: list di25_vars & da25_vars
loc dict_only25: list di25_vars - da25_vars
loc data_only25: list da25_vars - di25_vars

***step8. display mismatch part
quietly log on //start log 

di in red "******Core variables in MNH25******"
di  "Matched variables: `common25'" //will show varialbes in both data dictionary and data
di "Extra variable in data: `data_only25'" //will show variables in data but not in data dictionary
di as error "Missing variables in data!!!: `dict_only25'" //will show variables in CRFs/data dictionary but not in data

quietly log off //stop log temporarily

************************************
*MNH26 Maternal Fatigue Questionnaire (FACIT)
************************************

***step5. import data to match variable 
import delimited using "$da/mnh26.csv", clear
rename *, upper
save "$da/data_mnh26", replace

***step6. store variable list 
des using "$dic/dict_mnh26.dta", varlist
loc di26_vars `r(varlist)'
des using "$da/data_mnh26.dta", varlist
loc da26_vars `r(varlist)'

***step7. compare variables 
loc common26: list di26_vars & da26_vars
loc dict_only26: list di26_vars - da26_vars
loc data_only26: list da26_vars - di26_vars

***step8. display mismatch part
quietly log on //start log 

di in red "******Core variables in MNH26******"
di  "Matched variables: `common26'" //will show varialbes in both data dictionary and data
di "Extra variable in data: `data_only26'" //will show variables in data but not in data dictionary
di as error "Missing variables in data!!!: `dict_only26'" //will show variables in CRFs/data dictionary but not in data

quietly log off //stop log temporarily


***step9. end log file
quietly log close

translate "$log/Core variable check_$today.smcl" "$log/Core variable check_$today.pdf", replace
