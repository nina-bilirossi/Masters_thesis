set more off
clear all

* Set working directory
cd "/Users/ninabilirossi/Desktop/MSC THESIS/Data works/My data prep"

* 1. Load Raw Data
use "PLFS India/PLFS_Data_2017-18/Download_dta/hh_per_fv_2017-18.dta", clear

* 2. Generate Combined Weights
gen wgt = MULT_per_fv/100 if NSS_per_fv == NSC_per_fv
replace wgt = MULT_per_fv/200 if NSS_per_fv != NSC_per_fv

* 3. Filter for Working Age Population (15-65)
destring b4q6_per_fv, replace
keep if b4q6_per_fv > 13 & b4q6_per_fv < 66

* 4. Standardize Custom State Codes *BEFORE* the collapse to ensure correct grouping
replace state_per_fv="0134" if state_per_fv=="01" & (b1q4_per_fv=="03" | b1q4_per_fv=="04")
replace state_per_fv="2526" if state_per_fv=="25" | state_per_fv=="26"

* 5. Identify All Active Workers (Principal + Subsidiary Status)
gen worker_PS = 0
replace worker_PS = 1 if inlist(b5pt1q3_per_fv, "11", "12", "21", "31", "41", "51")
replace worker_PS = 1 if inlist(b5pt2q3_per_fv, "11", "12", "21", "31", "41", "51")

* 6. Extract 2-Digit NIC Codes (Fixed for String Type variables)
* NIC codes starting with 01, 02, and 03 signify the Agriculture Sector:
* 01 = Crop & animal production, hunting, and related activities
* 02 = Forestry and logging
* 03 = Fishing and aquaculture
gen nic_p_temp = string(real(b5pt1q5_per_fv), "%05.0f")
gen nic_s_temp = string(real(b5pt2q5_per_fv), "%05.0f")

gen nic_principal  = substr(nic_p_temp, 1, 2)
gen nic_subsidiary = substr(nic_s_temp, 1, 2)
drop nic_p_temp nic_s_temp

* 7. Identify Agricultural Workers
gen agri_worker_PS = 0
replace agri_worker_PS = 1 if inlist(b5pt1q3_per_fv, "11", "12", "21", "31", "41", "51") & inlist(nic_principal, "01")
replace agri_worker_PS = 1 if inlist(b5pt2q3_per_fv, "11", "12", "21", "31", "41", "51") & inlist(nic_subsidiary, "01")

* 8. Create Explicit Weighted Variables
* This lets us aggregate both types of data in one single step
gen worker_PS_weighted = worker_PS * wgt
gen agri_worker_PS_weighted = agri_worker_PS * wgt

* 9. Collapse to State Level (Calculates sums for both weighted and unweighted variations)
collapse (sum) total_workers_w=worker_PS_weighted total_agri_w=agri_worker_PS_weighted ///
               total_workers_unw=worker_PS total_agri_unw=agri_worker_PS, by(state_per_fv)

* 10. Compute the Two Distinct Shares
gen share_agri_weighted = total_agri_w / total_workers_w
gen share_agri_unweighted = total_agri_unw / total_workers_unw

* 11. Merge with State Names Matrix to map the readable text strings
merge 1:1 state_per_fv using "/Users/ninabilirossi/Desktop/MSC THESIS/Data works/Shared with me/PLFS India/PLFS_Data_2017-18/Download_dta/State_names.dta"

* Clean up merge results according to your layout rules
assert _merge==3 if !inlist(state_per_fv, "0134", "25", "26", "2526")
drop if state_per_fv=="25" | state_per_fv=="26"
drop _merge

* Name your custom combined regions
replace state_name="LADAKH & JAMMU KASHMIR COMBINED" if state_per_fv=="0134"
replace state_name="D & N. HAVELI & DAMAN & DIU" if state_per_fv=="2526"

* 12. Export to a completely New CSV file (keeping your old .dta files untouched)
sort state_per_fv
export delimited using "PLFS processed/PLFS_1718_Agri_Share_State_Both_Weights.csv", replace

clear
