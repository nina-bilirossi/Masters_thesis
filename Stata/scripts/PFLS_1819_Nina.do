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
****************** Def. 1: Informal worker *************************************
********************************************************************************

*Not possible with this round, since no information on frequency of payment. 


********************************************************************************
********* Def. 2: Individual working in informal firm **************************
********************************************************************************

*Not possible with this round, since no information on electricity (only on number of workers)

********************************************************************************
************ Def. 3: Casual worker *********************************************
********************************************************************************

/*
Use principal only and principal and subsidiary activity to identify casual worker. Thus generate two variables, one based on principal activity only and one based on both principal and subsidiary activity. 
I only used the first time visits (and not the other dataset with the revisits too).
Also created this definition based on both daily and weekly activity status and based on weekly activity status only. 
*/

*Load person level data with only first time visits
use "/Users/ninabilirossi/Desktop/MSC THESIS/Data works/My data prep/PLFS India/PLFS_Data_2018-19/Download_dta/PerV1_2018-19.dta", clear

*Check if duplicates
gen ID=fsu_per_fv+b1q13_per_fv+b1q14_per_fv+b1q15_per_fv+b4q1_per_fv
duplicates tag ID, gen(dup)
assert dup==0
drop dup

*Identify casual worker based on principal activity
gen casual_w_P=1 if inlist(b5pt1q3_per_fv, "11", "21", "41", "51")

*Identify casual worker based on principal and subsidiary activity
gen casual_w_PS=1 if inlist(b5pt1q3_per_fv, "11", "21", "41", "51")
replace casual_w_PS=1 if inlist(b5pt2q3_per_fv, "11", "21", "41", "51")

*Define worker based on principal activity (for later to aggregate to State level)
gen worker_P=1 if inlist(b5pt1q3_per_fv, "11", "12", "21", "31", "41", "51")

*Define worker based on principal and subsidiary activity (for later to aggregate to State level)
gen worker_PS=1 if inlist(b5pt1q3_per_fv, "11", "12", "21", "31", "41", "51")
replace worker_PS=1 if inlist(b5pt2q3_per_fv, "11", "12", "21", "31", "41", "51")

*Define labor force based on principal activity (employed plus unemployed)
gen labor_force_P=1 if inlist(b5pt1q3_per_fv, "11", "12", "21", "31", "41", "51", "81")

*Define labor force based on principal and subsidiary activity (employed plus unemployed)
gen labor_force_PS=1 if inlist(b5pt1q3_per_fv, "11", "12", "21", "31", "41", "51", "81")
replace labor_force_PS=1 if inlist(b5pt2q3_per_fv, "11", "12", "21", "31", "41", "51", "81")

//status codes 42, 61, 62, 71, 72 only in weekly and daily status but not in principal and subsidiary status

*Identify casual worker based on weekly status
gen casual_w_W=1 if inlist(b6q5_per_fv, "11", "21", "41", "42", "51") 

*Identify casual worker based on daily and weekly status
gen casual_w_WD=1 if inlist(b6q5_per_fv, "11", "21", "41", "42", "51") 
foreach y of varlist b6q4_3pt*  {
replace casual_w_WD=1 if inlist(`y', "11", "21", "42", "41", "51")
}

*Define workers based on weekly status
gen worker_W=1 if inlist(b6q5_per_fv, "11", "12", "21", "31", "41", "42")
replace worker_W=1 if inlist(b6q5_per_fv, "51", "61", "62", "71", "72")

*Define labor force based on weekly status 
gen labor_force_W=worker_W
replace labor_force_W=1 if inlist(b6q5_per_fv, "81", "82")

*Define workers based on weekly and daily status
gen worker_WD=1 if inlist(b6q5_per_fv, "11", "12", "21", "31", "41", "42")
replace worker_WD=1 if inlist(b6q5_per_fv, "51", "61", "62", "71", "72")
foreach y of varlist b6q4_3pt*  {
replace worker_WD=1 if inlist(`y', "11", "12", "21", "31", "41", "42")
replace worker_WD=1 if inlist(`y', "51", "61", "62", "71", "72")
}

*Define labor force based on weekly and daily status 
gen labor_force_WD=worker_WD
replace labor_force_WD=1 if inlist(b6q5_per_fv, "81", "82")
foreach y of varlist b6q4_3pt*  {
replace labor_force_WD=1 if inlist(`y', "81", "82")
}


********************************************************************************
********* Combine all definitions **********************************************
********************************************************************************

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
gen male = (b4q5_per_fv=="1")
gen female = (b4q5_per_fv=="2")
gen nonbin = (b4q5_per_fv=="3")

tab b4q5_per_fv

*Generate sector indicators
gen rural = (b1q3_per_fv=="1")
gen urban = (b1q3_per_fv=="2")



********************************************************************************
********* Aggregate to State level *********************************************
********************************************************************************

***** Yearly *****
preserve

/*
*Did earlier, but we do not want this, thus undo
*Add state Telangana to Andhra Pradesh as it was before 2014 (to be consistent with NSS data) --> only use this if we want to combine NSS and PLFS data together and have consistent states over both surveys
replace state_per_fv="28" if state_per_fv=="36"
*/

*Split Ladakh and Jammu & Kashmir already here since these two states split on 31. Okt 2019 and we want consistent states over time, is not 100% correct (bc are not States yet with laws etc. but consistent over time) but is the best we can do. 
replace state_per_fv="0134" if state_per_fv=="01" & (b1q4_per_fv=="03"|b1q4_per_fv=="04" )
*Combine D+D and D+N Haveli already here since these two states merge on 26. Jan 2020 and we want consistent states over time, is not 100% correct (bc are not States yet with laws etc. but consistent over time) but is the best we can do. 
replace state_per_fv="2526" if state_per_fv=="25" |  state_per_fv=="26" 


*Generate combined weight
gen wgt=MULT_per_fv/100 if NSS_per_fv==NSC_per_fv
replace wgt=MULT_per_fv/200 if NSS_per_fv!=NSC_per_fv

if ${weight}==1{
	egen pop_tot_s=total(wgt/4), by(state_per_fv)
}
else{
	gen temp=1
	egen pop_tot_s=total(temp), by(state_per_fv)
	drop temp
}

*Keep only age 14 to 65
destring b4q6_per_fv, replace
keep if b4q6_per_fv>13 & b4q6_per_fv<66

* Define working age population
if ${weight}==1{
    egen pop_workingage_s=total(wgt/4), by(state_per_fv)
}
else{
    gen temp=1
    egen pop_workingage_s=total(temp), by(state_per_fv)
    drop temp
}

*Aggregate overall, male, and female counts to state level
if ${weight}==1{
	foreach x in casual_w_P casual_w_PS worker_P worker_PS labor_force_P labor_force_PS casual_w_W casual_w_WD worker_W labor_force_W worker_WD labor_force_WD{
		egen `x'_s=total(`x'*wgt/4), by(state_per_fv)
		egen `x'_m_s=total(`x'*male*wgt/4), by(state_per_fv)
		egen `x'_f_s=total(`x'*female*wgt/4), by(state_per_fv)
		egen `x'_rur_s=total(`x'*rural*wgt/4), by(state_per_fv)
		egen `x'_urb_s=total(`x'*urban*wgt/4), by(state_per_fv)
	}
	egen pop_s=total(wgt/4), by(state_per_fv)
	egen pop_m_s=total(male*wgt/4), by(state_per_fv)
	egen pop_f_s=total(female*wgt/4), by(state_per_fv)
	egen pop_rur_s=total(rural*wgt/4), by(state_per_fv)
	egen pop_urb_s=total(urban*wgt/4), by(state_per_fv)
}
else{
	foreach x in casual_w_P casual_w_PS worker_P worker_PS labor_force_P labor_force_PS casual_w_W casual_w_WD worker_W labor_force_W worker_WD labor_force_WD{
		egen `x'_s=total(`x'), by(state_per_fv)
		egen `x'_m_s=total(`x'*male), by(state_per_fv)
		egen `x'_f_s=total(`x'*female), by(state_per_fv)
		egen `x'_rur_s=total(`x'*rural), by(state_per_fv)
		egen `x'_urb_s=total(`x'*urban), by(state_per_fv)
	}
	gen temp=1
	egen pop_s=total(temp), by(state_per_fv)
	egen pop_m_s=total(male), by(state_per_fv)
	egen pop_f_s=total(female), by(state_per_fv)
	egen pop_rur_s=total(rural), by(state_per_fv)
	egen pop_urb_s=total(urban), by(state_per_fv)
	drop temp
}

keep state_per_fv *_s 

foreach x in casual_w_P casual_w_PS worker_P worker_PS labor_force_P labor_force_PS casual_w_W casual_w_WD worker_W labor_force_W worker_WD labor_force_WD pop {
	rename `x'_s `x'
	rename `x'_m_s `x'_m
	rename `x'_f_s `x'_f
	rename `x'_rur_s `x'_rur
	rename `x'_urb_s `x'_urb
}

*pop_tot has no gender split so just rename the overall
rename pop_tot_s pop_tot
rename pop_workingage_s pop_workingage

duplicates drop state_per_fv, force

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


sort state_per_fv

gen data="PLFS_1819"
gen time="July 2018 - June 2019"

*Add state names
merge 1:1 state_per_fv using "/Users/ninabilirossi/Desktop/MSC THESIS/Data works/Shared with me/PLFS India/PLFS_Data_2018-19/Download_dta/State_names.dta"
assert _merge==3 if !inlist(state_per_fv, "0134", "25", "26", "2526")
drop if state_per_fv=="25" | state_per_fv=="26" 
drop _merge

rename state_per_fv state_code_1819

replace state_name="LADAKH" if state_code_1819=="0134"
replace state_name="D & N. HAVELI & DAMAN & DIU" if state_code_1819=="2526"


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

save "/Users/ninabilirossi/Desktop/MSC THESIS/Data works/My data prep/PLFS processed/PLFS_1819_State_N_${string_weight}.dta", replace


restore
