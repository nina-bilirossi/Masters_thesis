set more off
clear all

*Generate weighted or unweighted version
global weight=0  //equal to 1 if generate weighted variable

if ${weight}==1{
	global string_weight "w"
}
else{
	global string_weight "unw"
}


********************************************************************************
************** PLFS: Combine yearly state level data ****************************
********************************************************************************

cd "/Users/ninabilirossi/Desktop/MSC THESIS/Data works/My data prep/PLFS processed"


use PLFS_1718_State_N_${string_weight}.dta, clear
rename state_code_1718 state_code_1819
append using PLFS_1819_State_N_${string_weight}.dta

rename state_code_1819 state_code_1920
append using PLFS_1920_State_N_${string_weight}.dta

rename state_code_1920 state_code_2021
append using PLFS_2021_State_N_${string_weight}.dta

replace state_name="ANDAMAN & N. ISLAND" if state_name=="A & N ISLAND"
replace state_name="PUDUCHERY" if state_name=="PUDUCHERRY"
replace state_name="TAMILNADU" if state_name=="TAMIL NADU"
replace state_name="UTTARAKHAND" if state_name=="UTTRAKHAND"
append using PLFS_2122_State_N_${string_weight}.dta

rename state_code_2122 state_code_2223
append using PLFS_2223_State_N_${string_weight}.dta

rename state_code_2223 state_code_2324
append using PLFS_2324_State_N_${string_weight}.dta

rename state_code_2021 state_code_PLFS_old
rename state_code_2324 state_code_PLFS_new

destring state_code_PLFS_new, replace

duplicates tag state_name time, gen(dup)
assert dup==0
drop dup

*Generate unique state_code for each state
egen state_code_PLFS=max(state_code_PLFS_new), by(state_name)

assert state_code_PLFS==state_code_PLFS_new if state_code_PLFS!=. & state_code_PLFS_new!=.
destring state_code_PLFS_old, replace
assert state_code_PLFS==state_code_PLFS_old if  state_code_PLFS!=. & state_code_PLFS_old!=. & state_code_PLFS_old!=134 & state_code_PLFS_old!=2526
duplicates tag state_code_PLFS time if  state_code_PLFS!=., gen(dup)
assert dup==0 if  state_code_PLFS!=.
drop dup

/*
replace state_code_PLFS=38 if state_name=="DAMAN & DIU"
replace state_code_PLFS=39 if state_name=="D & N HAVELI"
*/

drop state_code_PLFS_new state_code_PLFS_old

ds

keep  s_casual_w_lf_P_m_unw ///
		s_casual_w_lf_PS_m_unw ///
		s_casual_w_lf_P_f_unw ///
		s_casual_w_lf_PS_f_unw ///
		s_casual_w_lf_P_unw ///
		s_casual_w_lf_PS_unw ///
		s_casual_w_lf_P_rur_unw ///
		s_casual_w_lf_PS_rur_unw ///
		s_casual_w_lf_P_urb_unw ///
		s_casual_w_lf_PS_urb_unw ///
		data ///
		time ///
		state_name


tempfile PLFS_all_s
save `PLFS_all_s'




/*
*MERGE PART: COMMENT OUT LATER - WAS ONLY TO MATCH STATE NAMES ABOVE IN MERGE
use PLFS_1718_State_${string_weight}.dta, clear
rename state_code_1718 state_code_1819
rename state_name state_name_1718
merge 1:1 state_code_1819 using PLFS_1819_State_${string_weight}.dta
assert _merge==3 
assert state_name==state_name_1718
drop _merge state_name

rename state_code_1819 state_code_1920
merge 1:1 state_code_1920 using PLFS_1920_State_${string_weight}.dta
assert _merge==3
assert state_name==state_name_1718
drop _merge state_name

rename state_code_1920 state_code_2021
merge 1:1 state_code_2021 using PLFS_2021_State_${string_weight}.dta
assert _merge==3
assert state_name==state_name_1718
drop _merge state_name

*rename state_code_2021 state_code_2122
*merge 1:1 state_code_2122 using PLFS_2122_State_${string_weight}.dta
*assert _merge==3
*assert state_name==state_name_2122
*Don't merge with code but with name because D&N and Daman & Diu were combined into one state now and Ladakh is explicitely added
*TODO: IF WE WANT TO USE ALL YEARS EITHER MERGE D&N AND DAMAN & DIU TO ONE STATE OR SEPERATE STATES FOR ALL YEARS, SAME FOR LADAKH - DONE - but undid it again
rename state_name_1718 state_name
replace state_name="ANDAMAN & N. ISLAND" if state_name=="A & N ISLAND"
replace state_name="PUDUCHERY" if state_name=="PUDUCHERRY"
replace state_name="TAMILNADU" if state_name=="TAMIL NADU"
replace state_name="UTTARAKHAND" if state_name=="UTTRAKHAND"
merge 1:1 state_name using PLFS_2122_State_${string_weight}.dta
drop _merge


merge 1:1 state_name using PLFS_2223_State_${string_weight}.dta
assert _merge!=2
assert state_code_2122==state_code_2223
drop _merge state_code_2122

merge 1:1 state_name using PLFS_2324_State_${string_weight}.dta
assert state_code_2223==state_code_2324
drop _merge state_code_2223
*/



********************************************************************************
**************** Combine NSS & PLFS ********************************************
********************************************************************************


use `PLFS_all_s'
replace state_name="A & N ISLANDS" if state_name=="ANDAMAN & N. ISLAND"
replace state_name="CHATTISGARH" if state_name=="CHHATTISGARH"
replace state_name="DADRA & NAGAR HAVELI" if state_name=="D & N HAVELI"
replace state_name="LAKSHDWEEP" if state_name=="LAKSHADWEEP"
replace state_name="ORISSA" if state_name=="ODISHA"
replace state_name="PONDICHERRY" if state_name=="PUDUCHERY"
replace state_name="TAMIL NADU" if state_name=="TAMILNADU"
replace state_name="UTTARANCHAL" if state_name=="UTTARAKHAND"

/*
append using `NSS_all_s'

egen test=max(state_code), by(state_name)
assert test==state_code_PLFS if state_code_PLFS!=. & test!=. //& state_code_PLFS!=38  & state_code_PLFS!=39 //38 and 39 exceptions
assert test==state_code if state_code!=.
egen test2=max(state_code_PLFS), by(state_name)

*wrong: assert test2==state_code if state_code!=.
*Code 25 and 26 used for different states (because of merge and split of states) (36 and 37 are fine)
*25 used to be DAMAN & DIU ; 26 used to be DADRA & NAGAR HAVELI and then 25 is in newer years D & N. HAVELI & DAMAN & DIU
*thus create new state code if D & N. HAVELI & DAMAN & DIU
drop state_code*
gen state_code=test
replace state_code=38 if state_name=="D & N. HAVELI & DAMAN & DIU"
replace state_code=test2 if state_code==.
drop test*
*/

save "new_PLFS_all_s_N_${string_weight}.dta", replace
export delimited using "new-PLFS_all_s_N_${string_weight}.csv", replace

/*
********************************************************************************
**************** FIRST LOOK & SUMMARY STATS ************************************
********************************************************************************
sort state_code data

*Check if total Indian pop is increasing over time
egen pop_ind=total(pop_tot), by(data)
encode data, gen(data_id)
twoway line pop_ind data_id if state_code==1, xlabel(, valuelabel)
*more or less increasing over time and around 1 billion (cheched with weighted var)


*Check if pop_tot (total population, not just working age) makes sense and is consistent over different data for each state
levelsof state_code, local(states)


foreach s of local states {
qui levelsof state_name if state_code == `s', local(sname)
twoway line pop_tot data_id if state_code==`s'  , xlabel(, valuelabel angle(45)) title(`sname') name(graph`s'_pop, replace)
} //& strpos(data, "NSS")
*A few examples (all based on weighted var): 
*Assam strange: suddenly a lot higher population in PLFS_2223
*Lakshdweep: first from 60'000 pop to 30'000 and then vice versa.., better if only use NSS rounds
*not super confident in data with this graphs
*Uttaranchal very low in NSS_50 - maybe do not artificially create the state there


*Check how stable population numbers are 
egen mean=mean(pop_tot), by(state_code)
egen sd=sd(pop_tot), by(state_code)
gen cv=sd/mean

bysort state_name: sum pop_tot, detail

*Plot share informal in worker over time for each state
levelsof state_code, local(states)
sort state_code data
foreach s of local states {
qui levelsof state_name if state_code == `s', local(sname)
twoway line s_casual_w_worker_P_unw s_worker_inf_worker_P_unw s_inf_cont_worker_WD_unw data_id if state_code==`s', xlabel(, valuelabel angle(45)) title(`sname') name(graph`s'_inf, replace)
}

*Generate share informal in worker whole india
egen casual_w_ind_PS=total(casual_w_PS), by(data)
egen worker_inf_ind_PS=total(worker_inf_PS), by(data)
replace worker_inf_ind_PS=. if worker_inf_ind_PS==0
egen inf_cont_ind_WD=total(inf_cont_WD), by(data)
replace inf_cont_ind_WD=. if inf_cont_ind_WD==0
egen worker_ind_PS=total(worker_PS), by(data)
egen worker_ind_WD=total(worker_WD), by(data)
gen s_casual_w_worker_ind_PS=casual_w_ind_PS/worker_ind_PS
gen s_worker_inf_worker_ind_PS=worker_inf_ind_PS/worker_ind_PS
gen s_inf_cont_worker_ind_WD=inf_cont_ind_WD/worker_ind_WD  //WRONG- DIVIDE BY WD
twoway line s_casual_w_worker_ind_PS s_worker_inf_worker_ind_PS s_inf_cont_worker_ind_WD data_id if state_code==1, xlabel(, valuelabel)
twoway line casual_w_ind worker_inf_ind_PS inf_cont_ind_WD data_id if state_code==1, xlabel(, valuelabel)
twoway line worker_ind_PS data_id if state_code==1, xlabel(, valuelabel)




NEXT
- DO SUMMARY STATS AND GRAPHS 
- CHECK IF LEVLE DATA NO OUTLAYERS: GENERATE POP OVERALL (NOT JUST WORKING AGE) TO CHECK THIS TOO 

quarterly data: 
POPLULATION IN THIS QUARTER FOR THIS ROUND FOR ARUNCHAL PRADESH IS AN OUTLAYER! 
CHECK WHAT IS GOING ON

