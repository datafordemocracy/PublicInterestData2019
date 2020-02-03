## Included files

### Data acquisition

* readfile.R: read encrypted files and save as csv to encrypted volume (match_cville_CPS_to_FC.csv, referral.csv, placement.csv)
* acsdata.R: get population and child population estimates by race from American Community Survey, read referral.RDS (see below), create data frame with population and referral totals by race (cville_acs.Rdata)

### Cleaning scripts

* data_clean.R: read match_cville_CPS_to_FC.csv file, rename variables (from data_dictionary), format and recode CPS variables, format and recode FC variables, save data (dss.RDS, dss_clean.RData)
* referral_long.R: read referral.csv (match_cville_CPS_to_FC.csv contains up to first 5 referrals only; this contains all captured referrals), remove FC variables, reshape to long format (referral is unit; initially, child is unit), save data (ref_long.RDS)
* referral_clean.R: read ref_long.RDS, apply CPS formatting and recoding from data_clean.R, save data (referral.RDS)
* cps_clean.R: read dss.RDS (child-level) and referral.RDS (referral-level) data, additional recoding and variable creation necessary for analysis, save data (cps_clean.Rdata)
* fc_clean.R: read dss.RDS (child-level), additional recoding and variable creation necessary for analysis, save data (foster_clean.RDS)
* placement.R: read dss_clean.RData, reshape placement to long, format and recode, save data (placement.RDS)

### Visualization scripts

* acs_cps_visualization.R: read cville_acs.RData, create Figure 1
* cps_visualization.R: read cps_clean.RData, create Figures 2, 5, 9
* fc_viz.R: read foster_clean.RDS, create Figures 8, 10, 12, 15, 16, 18
* sankey_child_ggalluvia.R: read dss.RDS, create Figure 4

### Analysis scripts

* cps_analysis.R: read cps_clean.RData, models in Tables A1-A6, Figures 3, 6, 7
* fc_analysis.R: read foster_clean.RDS, models in Tables A7-A11, A15-A17, Figures 11, 13, 17
* fc_transitions.R: read placement.RDS, models in Tables A12-A14, Figure 14

