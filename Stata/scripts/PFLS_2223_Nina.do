

set more off
clear all


cd "/Users/ninabilirossi/Desktop/MSC THESIS/Data works/My data prep"


*Generate weighted or unweighted version - only activate if not running via Master.do
global weight=0  //equal to 1 if generate weighted variable

if ${weight}==1{
	global string_weight "w"
}
else{
	global string_weight "unw"
}
 

********************************************************************************
************ Def. 3: Casual worker *********************************************
********************************************************************************

/*
Use principal only and principal and subsidiary activity to identify casual worker. Thus generate two variables, one based on principal activity only and one based on both principal and subsidiary activity. 
I only used the first time visits (and not the other dataset with the revisits too).
Also created this definition based on both daily and weekly activity status and based on weekly activity status only. 
 
*/

*Load person level data with only first time visits
use "/Users/ninabilirossi/Desktop/MSC THESIS/Data works/My data prep/PLFS India/PLFS_Data_2022-23/Download_dta/perv1.dta", clear

*Check if duplicates
gen ID=b1q1_perv1+b1q13_perv1+b1q14_perv1+b1q15_perv1+b4q1_perv1
duplicates tag ID, gen(dup)
assert dup==0
drop dup

*Identify casual worker based on principal activity
gen casual_w_P=1 if inlist(b5pt1q3_perv1, "11", "21", "41", "51")

*Identify casual worker based on principal and subsidiary activity
gen casual_w_PS=1 if inlist(b5pt1q3_perv1, "11", "21", "41", "51")
replace casual_w_PS=1 if inlist(b5pt2q3_perv1, "11", "21", "41", "51")

*Define worker based on principal activity (for later to aggregate to State level)
gen worker_P=1 if inlist(b5pt1q3_perv1, "11", "12", "21", "31", "41", "51")

*Define worker based on principal and subsidiary activity (for later to aggregate to State level)
gen worker_PS=1 if inlist(b5pt1q3_perv1, "11", "12", "21", "31", "41", "51")
replace worker_PS=1 if inlist(b5pt2q3_perv1, "11", "12", "21", "31", "41", "51")

*Define labor force based on principal activity (employed plus unemployed) (see documentation - beggars and prostitutes not in here..)
gen labor_force_P=1 if inlist(b5pt1q3_perv1, "11", "12", "21", "31", "41", "51", "81")

*Define labor force based on principal and subsidiary activity (employed plus unemployed) (see documentation - beggars and prostitutes not in here..)
gen labor_force_PS=1 if inlist(b5pt1q3_perv1, "11", "12", "21", "31", "41", "51", "81")
replace labor_force_PS=1 if inlist(b5pt2q3_perv1, "11", "12", "21", "31", "41", "51", "81")


foreach var in b6q4_3pt2 b6q4_act2_3pt6_perv1{
	tostring `var', gen(`var'_str)
	drop `var'
	rename `var'_str `var'
	replace `var'="" if `var'=="."
}

//status codes 42, 61, 62, 71, 72 only in weekly and daily status but not in principal and subsidiary status

*Identify casual worker based on weekly status
gen casual_w_W=1 if inlist(b6q5_perv1, "11", "21", "41", "42", "51") 

*Identify casual worker based on daily and weekly status
gen casual_w_WD=1 if inlist(b6q5_perv1, "11", "21", "41", "42", "51") 
foreach y of varlist b6q4_*  {
replace casual_w_WD=1 if inlist(`y', "11", "21", "42", "41", "51")
}

*Define workers based on weekly status
gen worker_W=1 if inlist(b6q5_perv1, "11", "12", "21", "31", "41", "42")
replace worker_W=1 if inlist(b6q5_perv1, "51", "61", "62", "71", "72")

*Define labor force based on weekly status 
gen labor_force_W=worker_W
replace labor_force_W=1 if inlist(b6q5_perv1, "81", "82")

*Define workers based on weekly and daily status
gen worker_WD=1 if inlist(b6q5_perv1, "11", "12", "21", "31", "41", "42")
replace worker_WD=1 if inlist(b6q5_perv1, "51", "61", "62", "71", "72")
foreach y of varlist b6q4_*  {
replace worker_WD=1 if inlist(`y', "11", "12", "21", "31", "41", "42")
replace worker_WD=1 if inlist(`y', "51", "61", "62", "71", "72")
}

*Define labor force based on weekly and daily status 
gen labor_force_WD=worker_WD
replace labor_force_WD=1 if inlist(b6q5_perv1, "81", "82")
foreach y of varlist b6q4_*  {
replace labor_force_WD=1 if inlist(`y', "81", "82")
}

********************************************************************************
********* Combine all definitions **********************************************
********************************************************************************

*Don't have to combine but can just continue with the one dataset I have

replace casual_w_PS=0 if casual_w_PS!=1 
replace casual_w_P=0 if casual_w_P!=1 
replace worker_PS=0 if worker_PS!=1 
replace labor_force_PS=0 if labor_force_PS!=1 
replace worker_P=0 if worker_P!=1 
replace labor_force_P=0 if labor_force_P!=1 
replace casual_w_W=0 if casual_w_W!=1
replace casual_w_WD=0 if casual_w_WD!=1
replace worker_W=0 if worker_W!=1
replace labor_force_W=0 if labor_force_W!=1
replace worker_WD=0 if worker_WD!=1 
replace labor_force_WD=0 if labor_force_WD!=1 

*Variable names for easy comparison
*P: based on principal activity status
*PS: based on principal and subsidiary activity status
*W: based on weekly activtiy status
*WD: based on weekly and daily activity status 

foreach x in P PS W WD {
	foreach y in worker labor_force {
		assert `y'_`x'==1 if casual_w_`x'==1
	}
}


*Generate sex indicators
gen male = (b4q5_perv1=="1")
gen female = (b4q5_perv1=="2")
gen nonbin = (b4q5_perv1=="3")

tab b4q5_perv1

*Generate sector indicators
gen rural = (b1q3_perv1=="1")
gen urban = (b1q3_perv1=="2")

********************************************************************************
********* Aggregate to State level *********************************************
********************************************************************************

***** Yearly *****
preserve

/*
*Did earlier, but we do not want this, thus undo
*district 3 of state 25 is Dadra and Nagar Haveli and district 1 and 2 is Daman & Diu
replace state_perv1="26" if state_perv1=="25" & distcode_perv1=="03"

*Put new state Ladakh into Jammu & Kashmir to have consistent states over time
replace state_perv1="01" if state_perv1=="37"

*Add state Telangana to Andhra Pradesh as it was before 2014 (to be consistent with NSS data)
replace state_perv1="28" if state_perv1=="36"
*/


*Generate combined weight (taking both the subsamples together)
gen wgt=mult_perv1/100 if NSS_perv1==NSC_perv1
replace wgt=mult_perv1/200 if NSS_perv1!=NSC_perv1

if ${weight}==1{
	egen pop_tot_s=total(wgt/4), by(state_perv1)
}
else{
	gen temp=1
	egen pop_tot_s=total(temp), by(state_perv1)
	drop temp
}

*Keep only age 14 to 65
keep if b4q6_perv1>13 & b4q6_perv1<66

*Define working age population
if ${weight}==1{
	egen pop_workingage_s=total(wgt/4), by(state_perv1)
}
else{
	gen temp=1
	egen pop_workingage_s=total(temp), by(state_perv1)
	drop temp
}


*Aggregate overall, male, and female counts to state level
if ${weight}==1{
	foreach x in casual_w_P casual_w_PS worker_P worker_PS labor_force_P labor_force_PS casual_w_W casual_w_WD worker_W labor_force_W worker_WD labor_force_WD{
		egen `x'_s=total(`x'*wgt/4), by(state_perv1)
		egen `x'_m_s=total(`x'*male*wgt/4), by(state_perv1)
		egen `x'_f_s=total(`x'*female*wgt/4), by(state_perv1)
		egen `x'_rur_s=total(`x'*rural*wgt/4), by(state_perv1)
		egen `x'_urb_s=total(`x'*urban*wgt/4), by(state_perv1)
	}
	egen pop_s=total(wgt/4), by(state_perv1)
	egen pop_m_s=total(male*wgt/4), by(state_perv1)
	egen pop_f_s=total(female*wgt/4), by(state_perv1)
	egen pop_rur_s=total(rural*wgt/4), by(state_perv1)
	egen pop_urb_s=total(urban*wgt/4), by(state_perv1)
}
else{
	foreach x in casual_w_P casual_w_PS worker_P worker_PS labor_force_P labor_force_PS casual_w_W casual_w_WD worker_W labor_force_W worker_WD labor_force_WD{
		egen `x'_s=total(`x'), by(state_perv1)
		egen `x'_m_s=total(`x'*male), by(state_perv1)
		egen `x'_f_s=total(`x'*female), by(state_perv1)
		egen `x'_rur_s=total(`x'*rural), by(state_perv1)
		egen `x'_urb_s=total(`x'*urban), by(state_perv1)
	}
	gen temp=1
	egen pop_s=total(temp), by(state_perv1)
	egen pop_m_s=total(male), by(state_perv1)
	egen pop_f_s=total(female), by(state_perv1)
	egen pop_rur_s=total(rural), by(state_perv1)
	egen pop_urb_s=total(urban), by(state_perv1)
	drop temp
}

keep state_perv1 *_s 

foreach x in casual_w_P casual_w_PS worker_P worker_PS labor_force_P labor_force_PS casual_w_W casual_w_WD worker_W labor_force_W worker_WD labor_force_WD pop {
	rename `x'_s `x'
	rename `x'_m_s `x'_m
	rename `x'_f_s `x'_f
	rename `x'_rur_s `x'_rur
	rename `x'_urb_s `x'_urb
}


rename pop_tot_s pop_tot
rename pop_workingage_s pop_workingage

duplicates drop state_perv1, force


*Generate shares - overall
gen s_casual_w_worker_PS=casual_w_PS/worker_PS 
gen s_casual_w_worker_P=casual_w_P/worker_P 
gen s_casual_w_worker_WD=casual_w_WD/worker_WD 
gen s_casual_w_worker_W=casual_w_W/worker_W 

gen s_casual_w_lf_PS=casual_w_PS/labor_force_PS 
gen s_casual_w_lf_P=casual_w_P/labor_force_P 
gen s_casual_w_lf_WD=casual_w_WD/labor_force_WD 
gen s_casual_w_lf_W=casual_w_W/labor_force_W 

gen s_casual_w_pop_PS=casual_w_PS/pop 
gen s_casual_w_pop_P=casual_w_P/pop 
gen s_casual_w_pop_WD=casual_w_WD/pop 
gen s_casual_w_pop_W=casual_w_W/pop 

*Generate shares - male
gen s_casual_w_worker_PS_m=casual_w_PS_m/worker_PS_m 
gen s_casual_w_worker_P_m=casual_w_P_m/worker_P_m 
gen s_casual_w_worker_WD_m=casual_w_WD_m/worker_WD_m 
gen s_casual_w_worker_W_m=casual_w_W_m/worker_W_m 

gen s_casual_w_lf_PS_m=casual_w_PS_m/labor_force_PS_m 
gen s_casual_w_lf_P_m=casual_w_P_m/labor_force_P_m 
gen s_casual_w_lf_WD_m=casual_w_WD_m/labor_force_WD_m 
gen s_casual_w_lf_W_m=casual_w_W_m/labor_force_W_m 

gen s_casual_w_pop_PS_m=casual_w_PS_m/pop_m 
gen s_casual_w_pop_P_m=casual_w_P_m/pop_m 
gen s_casual_w_pop_WD_m=casual_w_WD_m/pop_m 
gen s_casual_w_pop_W_m=casual_w_W_m/pop_m 

*Generate shares - female
gen s_casual_w_worker_PS_f=casual_w_PS_f/worker_PS_f 
gen s_casual_w_worker_P_f=casual_w_P_f/worker_P_f 
gen s_casual_w_worker_WD_f=casual_w_WD_f/worker_WD_f 
gen s_casual_w_worker_W_f=casual_w_W_f/worker_W_f 

gen s_casual_w_lf_PS_f=casual_w_PS_f/labor_force_PS_f 
gen s_casual_w_lf_P_f=casual_w_P_f/labor_force_P_f 
gen s_casual_w_lf_WD_f=casual_w_WD_f/labor_force_WD_f 
gen s_casual_w_lf_W_f=casual_w_W_f/labor_force_W_f 

gen s_casual_w_pop_PS_f=casual_w_PS_f/pop_f 
gen s_casual_w_pop_P_f=casual_w_P_f/pop_f 
gen s_casual_w_pop_WD_f=casual_w_WD_f/pop_f 
gen s_casual_w_pop_W_f=casual_w_W_f/pop_f 

*Generate shares - rural
gen s_casual_w_worker_PS_rur=casual_w_PS_rur/worker_PS_rur 
gen s_casual_w_worker_P_rur=casual_w_P_rur/worker_P_rur 
gen s_casual_w_worker_WD_rur=casual_w_WD_rur/worker_WD_rur 
gen s_casual_w_worker_W_rur=casual_w_W_rur/worker_W_rur 

gen s_casual_w_lf_PS_rur=casual_w_PS_rur/labor_force_PS_rur 
gen s_casual_w_lf_P_rur=casual_w_P_rur/labor_force_P_rur 
gen s_casual_w_lf_WD_rur=casual_w_WD_rur/labor_force_WD_rur 
gen s_casual_w_lf_W_rur=casual_w_W_rur/labor_force_W_rur 

gen s_casual_w_pop_PS_rur=casual_w_PS_rur/pop_rur 
gen s_casual_w_pop_P_rur=casual_w_P_rur/pop_rur 
gen s_casual_w_pop_WD_rur=casual_w_WD_rur/pop_rur 
gen s_casual_w_pop_W_rur=casual_w_W_rur/pop_rur 


*Generate shares - urban
gen s_casual_w_worker_PS_urb=casual_w_PS_urb/worker_PS_urb 
gen s_casual_w_worker_P_urb=casual_w_P_urb/worker_P_urb 
gen s_casual_w_worker_WD_urb=casual_w_WD_urb/worker_WD_urb 
gen s_casual_w_worker_W_urb=casual_w_W_urb/worker_W_urb 

gen s_casual_w_lf_PS_urb=casual_w_PS_urb/labor_force_PS_urb 
gen s_casual_w_lf_P_urb=casual_w_P_urb/labor_force_P_urb 
gen s_casual_w_lf_WD_urb=casual_w_WD_urb/labor_force_WD_urb 
gen s_casual_w_lf_W_urb=casual_w_W_urb/labor_force_W_urb 

gen s_casual_w_pop_PS_urb=casual_w_PS_urb/pop_urb 
gen s_casual_w_pop_P_urb=casual_w_P_urb/pop_urb 
gen s_casual_w_pop_WD_urb=casual_w_WD_urb/pop_urb 
gen s_casual_w_pop_W_urb=casual_w_W_urb/pop_urb 

gen data="PLFS_2223"
gen time="July 2022 - June 2023"

*Add state names
merge 1:1 state_perv1 using "/Users/ninabilirossi/Desktop/MSC THESIS/Data works/My data prep/PLFS India/PLFS_Data_2022-23/Download_dta/State_names.dta"
assert _merge==3
/*undo:
assert _merge==3 if state_perv1!="37" & state_perv1!="26" & state_perv1!="36"
assert _merge==2 if state_perv1=="37" & state_perv1=="36"
assert _merge==1 if state_perv1=="26"
drop if state_perv1=="37" | state_perv1=="36"
*/
drop _merge

rename state_perv1 state_code_2223

/*undo:
replace state_name="DAMAN & DIU" if state_code_2324=="25"
replace state_name="D & N HAVELI" if state_code_2324=="26"
*/

*Rename variables depending if weighted or unweighted
if ${weight}==1{
	foreach var in pop_tot pop_workingage casual_w_PS casual_w_P worker_PS labor_force_PS worker_P labor_force_P casual_w_W casual_w_WD worker_W labor_force_W worker_WD labor_force_WD pop pop_m pop_f casual_w_PS_m casual_w_P_m worker_PS_m labor_force_PS_m worker_P_m labor_force_P_m casual_w_W_m casual_w_WD_m worker_W_m labor_force_W_m worker_WD_m labor_force_WD_m casual_w_PS_f casual_w_P_f worker_PS_f labor_force_PS_f worker_P_f labor_force_P_f casual_w_W_f casual_w_WD_f worker_W_f labor_force_W_f worker_WD_f labor_force_WD_f s_casual_w_worker_PS s_casual_w_worker_P s_casual_w_worker_WD s_casual_w_worker_W s_casual_w_lf_PS s_casual_w_lf_P s_casual_w_lf_WD s_casual_w_lf_W s_casual_w_pop_PS s_casual_w_pop_P s_casual_w_pop_WD s_casual_w_pop_W s_casual_w_worker_PS_m s_casual_w_worker_P_m s_casual_w_worker_WD_m s_casual_w_worker_W_m s_casual_w_lf_PS_m s_casual_w_lf_P_m s_casual_w_lf_WD_m s_casual_w_lf_W_m s_casual_w_pop_PS_m s_casual_w_pop_P_m s_casual_w_pop_WD_m s_casual_w_pop_W_m s_casual_w_worker_PS_f s_casual_w_worker_P_f s_casual_w_worker_WD_f s_casual_w_worker_W_f s_casual_w_lf_PS_f s_casual_w_lf_P_f s_casual_w_lf_WD_f s_casual_w_lf_W_f s_casual_w_pop_PS_f s_casual_w_pop_P_f s_casual_w_pop_WD_f s_casual_w_pop_W_f pop_urb pop_rur casual_w_PS_urb casual_w_P_urb worker_PS_urb labor_force_PS_urb worker_P_urb labor_force_P_urb casual_w_W_urb casual_w_WD_urb worker_W_urb labor_force_W_urb worker_WD_urb labor_force_WD_urb casual_w_PS_rur casual_w_P_rur worker_PS_rur labor_force_PS_rur worker_P_rur labor_force_P_rur casual_w_W_rur casual_w_WD_rur worker_W_rur labor_force_W_rur worker_WD_rur labor_force_WD_rur s_casual_w_worker_PS_urb s_casual_w_worker_P_urb s_casual_w_worker_WD_urb s_casual_w_worker_W_urb s_casual_w_lf_PS_urb s_casual_w_lf_P_urb s_casual_w_lf_WD_urb s_casual_w_lf_W_urb s_casual_w_pop_PS_urb s_casual_w_pop_P_urb s_casual_w_pop_WD_urb s_casual_w_pop_W_urb s_casual_w_worker_PS_rur s_casual_w_worker_P_rur s_casual_w_worker_WD_rur s_casual_w_worker_W_rur s_casual_w_lf_PS_rur s_casual_w_lf_P_rur s_casual_w_lf_WD_rur s_casual_w_lf_W_rur s_casual_w_pop_PS_rur s_casual_w_pop_P_rur s_casual_w_pop_WD_rur s_casual_w_pop_W_rur{
		rename `var' `var'_w
	}
}
else{
	foreach var in pop_tot pop_workingage casual_w_PS casual_w_P worker_PS labor_force_PS worker_P labor_force_P casual_w_W casual_w_WD worker_W labor_force_W worker_WD labor_force_WD pop pop_m pop_f casual_w_PS_m casual_w_P_m worker_PS_m labor_force_PS_m worker_P_m labor_force_P_m casual_w_W_m casual_w_WD_m worker_W_m labor_force_W_m worker_WD_m labor_force_WD_m casual_w_PS_f casual_w_P_f worker_PS_f labor_force_PS_f worker_P_f labor_force_P_f casual_w_W_f casual_w_WD_f worker_W_f labor_force_W_f worker_WD_f labor_force_WD_f s_casual_w_worker_PS s_casual_w_worker_P s_casual_w_worker_WD s_casual_w_worker_W s_casual_w_lf_PS s_casual_w_lf_P s_casual_w_lf_WD s_casual_w_lf_W s_casual_w_pop_PS s_casual_w_pop_P s_casual_w_pop_WD s_casual_w_pop_W s_casual_w_worker_PS_m s_casual_w_worker_P_m s_casual_w_worker_WD_m s_casual_w_worker_W_m s_casual_w_lf_PS_m s_casual_w_lf_P_m s_casual_w_lf_WD_m s_casual_w_lf_W_m s_casual_w_pop_PS_m s_casual_w_pop_P_m s_casual_w_pop_WD_m s_casual_w_pop_W_m s_casual_w_worker_PS_f s_casual_w_worker_P_f s_casual_w_worker_WD_f s_casual_w_worker_W_f s_casual_w_lf_PS_f s_casual_w_lf_P_f s_casual_w_lf_WD_f s_casual_w_lf_W_f s_casual_w_pop_PS_f s_casual_w_pop_P_f s_casual_w_pop_WD_f s_casual_w_pop_W_f pop_urb pop_rur casual_w_PS_urb casual_w_P_urb worker_PS_urb labor_force_PS_urb worker_P_urb labor_force_P_urb casual_w_W_urb casual_w_WD_urb worker_W_urb labor_force_W_urb worker_WD_urb labor_force_WD_urb casual_w_PS_rur casual_w_P_rur worker_PS_rur labor_force_PS_rur worker_P_rur labor_force_P_rur casual_w_W_rur casual_w_WD_rur worker_W_rur labor_force_W_rur worker_WD_rur labor_force_WD_rur s_casual_w_worker_PS_urb s_casual_w_worker_P_urb s_casual_w_worker_WD_urb s_casual_w_worker_W_urb s_casual_w_lf_PS_urb s_casual_w_lf_P_urb s_casual_w_lf_WD_urb s_casual_w_lf_W_urb s_casual_w_pop_PS_urb s_casual_w_pop_P_urb s_casual_w_pop_WD_urb s_casual_w_pop_W_urb s_casual_w_worker_PS_rur s_casual_w_worker_P_rur s_casual_w_worker_WD_rur s_casual_w_worker_W_rur s_casual_w_lf_PS_rur s_casual_w_lf_P_rur s_casual_w_lf_WD_rur s_casual_w_lf_W_rur s_casual_w_pop_PS_rur s_casual_w_pop_P_rur s_casual_w_pop_WD_rur s_casual_w_pop_W_rur{
		rename `var' `var'_unw
	}
}



save "/Users/ninabilirossi/Desktop/MSC THESIS/Data works/My data prep/PLFS processed/PLFS_2223_State_N_${string_weight}.dta", replace


/*Checked: 
- Total pop number (not only ages 15-65 but all) roughly above 1 billion 
- Pop per states also seem to be about right (made a very short check for a few states)
*/


restore

***** Quarterly *****


*Not done because makes no sense with causual_w (as long as based on usual principal and subsidiary activity status)






***** Yearly: With considering migration *****
/*

No info on migration in this data

*/


********************************************************************************
********* Aggregate to Region level *********************************************
********************************************************************************
*NOT DONE YET DONE








