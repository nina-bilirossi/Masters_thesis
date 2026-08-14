set more off
clear all

cd "/Users/ninabilirossi/Desktop/MSC THESIS/Data works/My data prep"

* 1. Load Data
use "PLFS India/PLFS_Data_2017-18/Download_dta/hh_per_fv_2017-18.dta", clear

* 2. Generate Combined Weights
gen wgt = MULT_per_fv/100 if NSS_per_fv == NSC_per_fv
replace wgt = MULT_per_fv/200 if NSS_per_fv != NSC_per_fv

* 3. Filter for Working Age (15-65)
destring b4q6_per_fv, replace
keep if b4q6_per_fv > 13 & b4q6_per_fv < 66

* 4. Identify All Workers (Principal + Subsidiary Status)
gen worker_PS = 0
replace worker_PS = 1 if inlist(b5pt1q3_per_fv, "11", "12", "21", "31", "41", "51")
replace worker_PS = 1 if inlist(b5pt2q3_per_fv, "11", "12", "21", "31", "41", "51")

* 5. Extract 2-Digit NIC Codes (Fixed for String Variables)
* First, convert to a clean 5-digit string format to safeguard leading zeros
gen nic_p_temp = string(real(b5pt1q5_per_fv), "%05.0f")
gen nic_s_temp = string(real(b5pt2q5_per_fv), "%05.0f")

* Now extract the first 2 digits safely
gen nic_principal  = substr(nic_p_temp, 1, 2)
gen nic_subsidiary = substr(nic_s_temp, 1, 2)

* Clean up temporary variables
drop nic_p_temp nic_s_temp

* 6. Identify Agricultural Workers 
gen agri_worker_PS = 0
* Count if their Principal activity is Agriculture:
replace agri_worker_PS = 1 if inlist(b5pt1q3_per_fv, "11", "12", "21", "31", "41", "51") & inlist(nic_principal, "01", "02", "03")
* Count if their Subsidiary activity is Agriculture (and they weren't already counted):
replace agri_worker_PS = 1 if inlist(b5pt2q3_per_fv, "11", "12", "21", "31", "41", "51") & inlist(nic_subsidiary, "01", "02", "03")

* 7. Aggregate to State Level using the 'collapse' command for simplicity
* This instantly applies the weights and sums up the workers per state
collapse (sum) total_workers=worker_PS total_agri=agri_worker_PS [iw=wgt], by(state_per_fv)

* 8. Generate the Final Share
gen share_agri_workers = total_agri / total_workers


* 1. Sort the final processed output cleanly
sort state_per_fv

* 2. Export explicitly to a new CSV file
export delimited using "/Users/ninabilirossi/Desktop/MSC THESIS/Data works/My data prep/PLFS processed/PLFS_1718_Agri_Share_State_Clean.csv", replace

clear
