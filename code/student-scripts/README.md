
# Student Submitted Scripts

## Stuart, Rishabh, Ana

Overview:
  * focused on referrals and age
  * submitted one script, now called `SRA_final.R`
  * loads `cville_acs.Rdata`produced by the `acsdata.R` script. 
  * must run `gen_qoi.R` to get predicted probability functions
  
Visualizations: 
  * Population Proportions and Referral Proportions by Race (Figure 1)
  * Racial Disproportionality Index in Referrals (not currently in report)
  * Average Number of Referrals by Race (Figure 2)
  * Probability of Entering Foster Care by Race (Figure 4)
  * Age Density by Race (not currently in report)
  * *Note: Predicted Number of Referrals by Race (Figure 3 in the report) is not found in this script*
  
Models:
  * logit model of foster care entrance
    * significant effects for multirace (more likely) and hispanic (less likely)
  * negative binomial model for number of referrals
    * significant effects for black, multirace (more referrals) and hispanic (less referrals)
  
## Janie, Alex, Carolynn

Overview: 
  * focused on removal reasons and financial support
  * submitted 3 scripts: `JAC_script1_clean.R`, `JAC_script2_pre_analysis.R`, `JAC_script3_models&effects.R`
  * cleaning script loads `dss.Rds`, produces `JAC_clean.RData`, subset to children in foster care
  * analysis and visualization scripts load `JAC_clean.RData`
  
Visualizations: 
  * Reasons for Removal from Home (Figure 9)
  * Financial Supports Received (Figure 8)
  * Probability of Removal for Neglect by Race (Figure 10)
  * Probability of Removal for Housing Quality by Race (Figure 11)
  * Probability of Removal for Parent Drug Abuse by Race (Figure 12)
  
Tests:
  * chi-square tests of removal reason by race
    * parent alcohol use, physical abuse, jail significant, relinquish close to significant (.12)
  * chi-square test of removal type (court-ordered, voluntary) by race (p=.1754)
  * ANOVA of monthly pay, significant difference by race (p=0.03)
  * Tukey multiple comparisons of means 
    * black-white difference significant, multiracial-white difference close (0.11)
  * chi-square tests of financial supports by race
    * only AFDC is significant-ish (0.08)

Models:
  * logit model of removal due to neglect 
    * no significant effect for race
  * logit model of removal due to inadequate housing
    * no significant effect for race
  * logit model of removal due to parental drug abuse
    * no significant effect by race


## Carolyn, Savannah, Kathryn 

Overview: 
  * focused on disability status, discharge reason, case goals, and current placement setting
  * submitted 3 scripts: `CSK_Cleaning.R`, `CSK_Visualization.R`, `CSK_Analysis_new.R`
  * cleaning script loads `dss.rds`, produces `foster_clean.rds`
  * analysis and visualization scripts load `foster_clean.rds`
  * *note: much of the visualization script produces error messages on my machine)*

Visualizations: 
  * Disability Diagnosis by Race (Figure 5)
  * Discharge Reason by Race (Figure 16)
  * Case Goals by Race (Figure 15)
  * Case Goal Assignment and Discharge Reason (Figure 17)
  * Current Placement Setting by Race (Figure 13)
  * Predicted Probability of Discharge Reason by Race (Figure 18)
  * Probability of Placement with Kin Foster Family by Race (Figure 14)

Tests: 
  * chi-square test for disability status and disability types (do not run for me)
  * chi-square test for discharge reason (do not run for me)
  * Fisher test for counts (not significant)
  * chi-square test for current placement setting (do not run for me)
  
Models 
  * multinomial logit models with ANOVA for discharge reason, significant for race
  * logit model for case goals, not significant for race
  * foster family kin logit model, not significant for race

## Brago, Hannah, Conor

Overview: 
  * focused on 'ever' variables (ever screened in, investigated, a finding, unsafe) and family structure
  * submitted 3 scripts: `BCH_Cleaning.R`, `BCH_Analysis.R`, `BCH_Visualizations.R`
  * cleaning script loads `dss.rds`, produces `BCH_clean.RData`
  * analysis and visualization scripts load `BCH_clean.RData`
  
Visualizations: 
  * Probability of Being Screened In by Race/Ethnicity (not in report)
  * Probability of Being Investigated by Race (not in report)
  * Probability of Having a Finding by Race (does not run)
  * Probability of Being Determined to be Unsafe by Race (not in report)
  * Probability of Being Determined to be Unsafe by Race (not in report)
  * Probability of Coming From a Single Parent Home (Figure 6)
  * Probability of Being Placed in a Single Parent Foster Home (Figure 7)

Models: 
  * logit model of ever screened in
    * race is significant with only demographics, not significant with abuse/neglect types
  * logit model of ever investigated, no significant effect by race
  * ordered logit for ever a finding, no significant effect by race
  * logit model of ever unsafe, no significant effect by race
  * logit model of coming from a single parent home
    * significant for black children (p=0.02), low but not significant for multiracial children (0.15)
  * logit model of being placed in a single parent home
    * significant for multiracial children
  
















