/*******************************************************************************
* Hayes' Process Model 14: Moderated Mediation Analysis
* Panel Data Analysis with Multiple Imputation for Finance Research
* 
* Model: X → M → Y with moderation of M → Y path by W
* X (fintech) → M (access_finance) → Y (finstb)
* Moderation: W (tax_rev) moderates M → Y path
*
* Author: faisalnawazmir@gmail.com
* Date: 14 August 2025 
* Stata Version: 17+
* Note: Data assumed to be multiply imputed using MI
*******************************************************************************/

/*******************************************************************************
* SECTION 1: Data 
*******************************************************************************/

cls
clear all
//use data.dta
use simulated_model14_data.dta
//xtset country_id year, yearly



// Define variables
global y "finstb"           // Outcome: Financial stability
global x "fintech"          // Independent: Fintech adoption
global m "access_finance"   // Mediator: Access to finance
global w "tax_rev"          // Moderator: Tax revenue
global controls "gdp  "  // Controls

// Display variable definitions
display "Model 14 Variable Definitions:"
display "Y (Outcome): $y - Financial Stability"
display "X (Predictor): $x - Fintech Adoption"
display "M (Mediator): $m - Access to Finance"
display "W (Moderator): $w - Tax Revenue"
display "Controls: $controls"



// Center control variables
foreach var in $x $y $w $m $controls {
    capture  gen c_`var' = `var'
}

// Create the interaction term for moderated mediation (M*W)
//tweak
*generate c_access_finance_X_tax_rev = c_fintech * c_tax_rev

// Center variables (recommended for moderated mediation)
egen c_fintech_mean = mean(c_fintech)
egen c_access_finance_mean = mean(c_access_finance) 
egen c_tax_rev_mean = mean(c_tax_rev)

generate c_fintech_c = c_fintech - c_fintech_mean
generate c_access_finance_c = c_access_finance - c_access_finance_mean
generate c_tax_rev_c = c_tax_rev - c_tax_rev_mean

// Create centered interaction term
generate c_access_finance_c_X_tax_rev_c = c_access_finance_c * c_tax_rev_c


/*******************************************************************************
* SECTION 2: VERIFY MI SETUP AND DESCRIPTIVE STATISTICS
*******************************************************************************/

// Generate descriptive statistics across imputations
//mi estimate: mean $y $x $m $w $controls
//correlate $y $x $m $w $controls


/*******************************************************************************
* SECTION 3: PANEL DATA SETUP WITH MI
*******************************************************************************/

// Set panel structure (adjust 'country_id' and 'year' to your actual variable names)

 xtset country_id year, yearly

// Panel data diagnostics with MI
//mi xeq: xtdescribe
 //xtsum $y $x $m $w $controls

/*******************************************************************************
* SECTION 4: HAYES MODEL 14 - PATH ANALYSIS WITH MI
*******************************************************************************/

display ""
display "==================================================================="
display "           HAYES PROCESS MODEL 14 ANALYSIS (MI VERSION)"
display "==================================================================="
// Path a: access_finance <- fintech gdp (H1)
// PATH A: X → M (not moderated in Model 14)
display ""
display "PATH A: Effect of X ($x) on M ($m)"
display "---------------------------------------------------"

// Using areg with year fixed effects and country clustering
//mi estimate, post:
 areg c_access_finance c_fintech c_gdp     , absorb(year) //vce(cluster country_id)
estimates store path_a_mi

// Extract and display path A results
matrix path_a_results = r(table)
scalar a_coeff = path_a_results[1,1]  // coefficient for c_fintech
scalar a_se = path_a_results[2,1]     // standard error
scalar a_pval = path_a_results[4,1]   // p-value

display "Path A coefficient (a): " a_coeff
display "Path A standard error: " a_se
display "Path A p-value: " a_pval
display as text "H1 Test: Effect of fintech on access_finance = " a_coeff

// PATH B & C': M → Y and X → Y (with moderation of M → Y by W)
// Path b: finstb <- access_finance tax_rev mw fintech gdp (H2, H3, H4)
display ""
display "PATHS B & C': Effects on Y ($y) with moderation"
display "---------------------------------------------------"

// Using areg with year fixed effects and country clustering
//mi estimate, post: 
areg c_finstb c_fintech c_access_finance c_tax_rev c_access_finance_c_X_tax_rev_c c_gdp       , absorb(year) //vce(cluster country_id)
estimates store full_model_mi

// Extract coefficients from MI results
matrix full_results = r(table)
scalar cprime_coeff = full_results[1,1]  // c_fintech coefficient
scalar b_coeff = full_results[1,2]       // c_access_finance coefficient  
scalar b3_coeff = full_results[1,4]      // interaction coefficient
scalar cprime_se = full_results[2,1]
scalar b_se = full_results[2,2]
scalar b3_se = full_results[2,4]

display "Path C' coefficient (direct effect): " cprime_coeff
display "Path B coefficient (M → Y): " b_coeff
display "Moderation coefficient (M × W → Y): " b3_coeff
display as text "H2 Test: Effect of access_finance on finstb = " b_coeff
display as text "H3 Test: Moderation effect (mw) = " b3_coeff
display as text "H4 Test: Direct effect of fintech on finstb = " cprime_coeff



/*******************************************************************************
* SECTION 5: CONDITIONAL INDIRECT EFFECTS WITH MI
*******************************************************************************/
// (H5, H6)
display ""
display "CONDITIONAL INDIRECT EFFECTS"
display "==================================================================="

// Calculate moderator levels across imputations
//mi estimate: 
summarize c_tax_rev

// Get pooled descriptive statistics for moderator
//mi xeq 0:
 summarize c_tax_rev, detail
scalar w_mean = r(mean)
scalar w_sd = r(sd)

// Define moderator levels (mean, ±1 SD)
scalar w_low = w_mean - w_sd
scalar w_mean_level = w_mean
scalar w_high = w_mean + w_sd

display "Moderator levels:"
display "Low (-1 SD): " w_low
display "Mean (0): " w_mean_level  
display "High (+1 SD): " w_high

// Calculate conditional indirect effects
display ""
display "Conditional Indirect Effects (a × b):"
display "---------------------------------------------------"

// Low moderator level
scalar cond_b_low = b_coeff + (b3_coeff * w_low)
scalar cond_indirect_low = a_coeff * cond_b_low
display "At Low W: " cond_indirect_low

// Mean moderator level
scalar cond_b_mean = b_coeff + (b3_coeff * w_mean_level)
scalar cond_indirect_mean = a_coeff * cond_b_mean
display "At Mean W: " cond_indirect_mean

// High moderator level
scalar cond_b_high = b_coeff + (b3_coeff * w_high)
scalar cond_indirect_high = a_coeff * cond_b_high
display "At High W: " cond_indirect_high

/*******************************************************************************
* SECTION 6: DELTA METHOD FOR CONDITIONAL INDIRECT EFFECTS STANDARD ERRORS
*******************************************************************************/

display ""
display "DELTA METHOD STANDARD ERRORS FOR CONDITIONAL INDIRECT EFFECTS"
display "==================================================================="

// Program to calculate conditional indirect effects with Delta method
program define cond_indirect_delta, rclass
    version 18
    syntax [, w_val(real 0)]
    
    // Path A - using areg
    quietly areg c_access_finance c_fintech c_gdp    , absorb(year) // vce(cluster country_id)
    estimates store path_a_temp
    
    // Store path A coefficient
    local a_coeff = _b[c_fintech]
    local a_se = _se[c_fintech]
    
    // Full model - using areg
    quietly areg c_finstb c_fintech c_access_finance c_tax_rev c_access_finance_c_X_tax_rev_c c_gdp       , absorb(year) // vce(cluster country_id)
    estimates store full_temp
    
    // Store path B coefficients
    local b_coeff = _b[c_access_finance]
    local b_interact = _b[c_access_finance_c_X_tax_rev_c]
    local b_se = _se[c_access_finance]
    local b_interact_se = _se[c_access_finance_c_X_tax_rev_c]
    
    // Calculate conditional indirect effect
    local conditional_b = `b_coeff' + `b_interact' * `w_val'
    local indirect = `a_coeff' * `conditional_b'
    
    // Delta method standard error calculation
    // For indirect effect a*(b + c*w), where a, b, c are coefficients
    // SE ≈ sqrt(a²*var(b + c*w) + (b + c*w)²*var(a))
    // Simplified version assuming independence between models
    local conditional_b_var = (`b_se')^2 + (`w_val')^2 * (`b_interact_se')^2
    local indirect_se = sqrt((`a_se')^2 * (`conditional_b')^2 + (`a_coeff')^2 * `conditional_b_var')
    
    display "Conditional Indirect Effect at W = " `w_val'
    display "  Coefficient: " `indirect'
    display "  Std Error: " `indirect_se'
    
    // Return results
    return scalar ind_eff = `indirect'
    return scalar ind_se = `indirect_se'
    
    // Clean up
    estimates drop path_a_temp full_temp
end

// Calculate conditional indirect effects with standard errors
display "Conditional Indirect Effects with Delta Method SEs:"
display "---------------------------------------------------"

// Low W
cond_indirect_delta, w_val(`=scalar(w_low)') 
scalar ind_low_coeff = r(ind_eff)
scalar ind_low_se = r(ind_se)
scalar ind_low_z = ind_low_coeff / ind_low_se
scalar ind_low_pval = 2 * normal(-abs(ind_low_z))

display "At Low W:"
display "  Coefficient: " ind_low_coeff
display "  Std Error: " ind_low_se
display "  z-value: " ind_low_z
display "  p-value: " ind_low_pval

// Mean W
cond_indirect_delta, w_val(`=scalar(w_mean_level)')
scalar ind_mean_coeff = r(ind_eff)
scalar ind_mean_se = r(ind_se)
scalar ind_mean_z = ind_mean_coeff / ind_mean_se
scalar ind_mean_pval = 2 * normal(-abs(ind_mean_z))

display "At Mean W:"
display "  Coefficient: " ind_mean_coeff
display "  Std Error: " ind_mean_se
display "  z-value: " ind_mean_z
display "  p-value: " ind_mean_pval

// High W
cond_indirect_delta, w_val(`=scalar(w_high)')
scalar ind_high_coeff = r(ind_eff)
scalar ind_high_se = r(ind_se)
scalar ind_high_z = ind_high_coeff / ind_high_se
scalar ind_high_pval = 2 * normal(-abs(ind_high_z))

display "At High W:"
display "  Coefficient: " ind_high_coeff
display "  Std Error: " ind_high_se
display "  z-value: " ind_high_z
display "  p-value: " ind_high_pval

/*******************************************************************************
* SECTION 7: MI BOOTSTRAP FOR CONDITIONAL INDIRECT EFFECTS
*******************************************************************************/

display ""
display "MI BOOTSTRAP CONFIDENCE INTERVALS"
display "==================================================================="

// Bootstrap program for MI conditional indirect effects
program define mi_cond_indirect_boot, rclass
    version 18
    
    // Path A: X → M - using areg
    //quietly mi estimate, post:
	quietly areg c_access_finance c_fintech c_gdp       , absorb(year) //vce(cluster country_id)
    matrix temp_a = r(table)
    scalar a_boot = temp_a[1,1]
    
    // Full model: paths to Y - using areg
    quietly  areg c_finstb c_fintech c_access_finance c_tax_rev c_access_finance_c_X_tax_rev_c c_gdp       , absorb(year) //vce(cluster country_id)
    matrix temp_full = r(table)
    scalar b_boot = temp_full[1,2]
    scalar b3_boot = temp_full[1,4]
    
    // Calculate conditional indirect effects
    scalar cond_b_low_boot = b_boot + (b3_boot * w_low)
    scalar cond_b_mean_boot = b_boot + (b3_boot * w_mean_level)
    scalar cond_b_high_boot = b_boot + (b3_boot * w_high)
    
    return scalar ind_low = a_boot * cond_b_low_boot
    return scalar ind_mean = a_boot * cond_b_mean_boot
    return scalar ind_high = a_boot * cond_b_high_boot
    
    // Index of moderated mediation
    return scalar index_mm = a_boot * b3_boot
end

// Run MI bootstrap (reduce reps for testing, increase for final analysis)
display "Running MI Bootstrap (500 replications)..."
bootstrap r(ind_low) r(ind_mean) r(ind_high) r(index_mm), reps(500) seed(12345): mi_cond_indirect_boot

// Display bootstrap results
// Display CIs to test H5 and H6
estat bootstrap, percentile bc


/*******************************************************************************
* SECTION 8: INDEX OF MODERATED MEDIATION WITH MI - CORRECTED VERSION
*******************************************************************************/
display ""
display "INDEX OF MODERATED MEDIATION"
display "==================================================================="


// SOLUTION 3: Step-by-step with saved results (Most transparent)
display ""
display "SOLUTION 3: Using stored estimates (Most detailed)"
display "--------------------------------------------------"

// Estimate and store path a
//mi estimate, post: 
areg c_access_finance c_fintech c_gdp  , absorb(year) //vce(cluster country_id)
estimates store mi_path_a
matrix list e(b)
matrix list e(V)

// Estimate and store full model
//mi estimate, post:
 areg c_finstb c_fintech c_access_finance c_tax_rev c_access_finance_c_X_tax_rev_c c_gdp       , absorb(year) //vce(cluster country_id)
estimates store mi_full_model

// Extract coefficients manually
estimates restore mi_path_a
scalar a_final = _b[c_fintech]
scalar a_se_final = _se[c_fintech]

estimates restore mi_full_model  
scalar b3_final = _b[c_access_finance_c_X_tax_rev_c]
scalar b3_se_final = _se[c_access_finance_c_X_tax_rev_c]

// Final calculation
scalar index_final = a_final * b3_final
scalar index_se_final = sqrt((b3_final^2 * a_se_final^2) + (a_final^2 * b3_se_final^2))

display ""
display "FINAL RESULTS - Index of Moderated Mediation:"
display "=============================================="
display "Coefficient a (X → M): " %9.6f a_final " (SE: " %9.6f a_se_final ")"
display "Coefficient b3 (M×W → Y): " %9.6f b3_final " (SE: " %9.6f b3_se_final ")"
display "Index (a × b3): " %9.6f index_final " (SE: " %9.6f index_se_final ")"
display ""
display "This index quantifies how the indirect effect (X → M → Y)"
display "changes for each unit increase in the moderator W."

display ""
display "==================================================================="

/*******************************************************************************
* SECTION 9: MODEL SUMMARY WITH MI RESULTS - CORRECTED VERSION
*******************************************************************************/
display ""
display "MODEL SUMMARY - HAYES PROCESS MODEL 14 (MI VERSION)"
display "==================================================================="

// Extract direct effect from the full model (c' coefficient)
estimates restore mi_full_model
scalar cprime_coeff = _b[c_fintech]
scalar cprime_se = _se[c_fintech]
scalar cprime_z = cprime_coeff / cprime_se
scalar cprime_pval = 2 * (1 - normal(abs(cprime_z)))

display "Direct Effect (c'): " %9.6f cprime_coeff " (SE: " %9.6f cprime_se ", p = " %6.3f cprime_pval ")"

// Calculate conditional indirect effects at different levels of moderator
// First, get moderator (tax_rev) descriptive statistics for +/- 1SD
quietly summarize c_tax_rev
scalar w_mean_new = r(mean)
scalar w_sd_new = r(sd)
scalar w_low_new = w_mean_new - w_sd_new     // -1SD
scalar w_high_new = w_mean_new + w_sd_new    // +1SD

display ""
display "Moderator (Tax Revenue) Values:"
display "  Low (-1SD):  " %9.6f w_low_new
display "  Mean:        " %9.6f w_mean_new  
display "  High (+1SD): " %9.6f w_high_new

// Calculate conditional indirect effects using the coefficients from your existing code
// Use a_final and b3_final from Section 8, and get b1 from current model
scalar b1_coeff = _b[c_access_finance]  // Main effect of mediator
scalar b1_se = _se[c_access_finance]

// Conditional indirect effects using existing scalars
scalar ind_low_coeff_new = a_final * (b1_coeff + b3_final * w_low_new)
scalar ind_mean_coeff_new = a_final * (b1_coeff + b3_final * w_mean_new)
scalar ind_high_coeff_new = a_final * (b1_coeff + b3_final * w_high_new)

// Standard errors for conditional indirect effects using Delta method
// SE = sqrt[a²×SE(b1+b3×W)² + (b1+b3×W)²×SE(a)²]
// Where SE(b1+b3×W)² = SE(b1)² + W²×SE(b3)² + 2×W×Cov(b1,b3)

// Get covariance between b1 and b3
matrix V_full = e(V)
local b1_pos = colnumb(e(b), "c_access_finance")
local b3_pos = colnumb(e(b), "c_access_finance_X_tax_rev")
scalar cov_b1_b3 = V_full[`b1_pos', `b3_pos']

// Standard errors for conditional effects
scalar se_cond_low_new = sqrt(b1_se^2 + (w_low_new^2 * b3_se_final^2) + (2 * w_low_new * cov_b1_b3))
scalar se_cond_mean_new = sqrt(b1_se^2 + (w_mean_new^2 * b3_se_final^2) + (2 * w_mean_new * cov_b1_b3))  
scalar se_cond_high_new = sqrt(b1_se^2 + (w_high_new^2 * b3_se_final^2) + (2 * w_high_new * cov_b1_b3))

// Full standard errors for indirect effects
scalar ind_low_se_new = sqrt((a_se_final^2 * (b1_coeff + b3_final * w_low_new)^2) + (a_final^2 * se_cond_low_new^2))
scalar ind_mean_se_new = sqrt((a_se_final^2 * (b1_coeff + b3_final * w_mean_new)^2) + (a_final^2 * se_cond_mean_new^2))
scalar ind_high_se_new = sqrt((a_se_final^2 * (b1_coeff + b3_final * w_high_new)^2) + (a_final^2 * se_cond_high_new^2))

// P-values for conditional indirect effects
scalar ind_low_z_new = ind_low_coeff_new / ind_low_se_new
scalar ind_mean_z_new = ind_mean_coeff_new / ind_mean_se_new
scalar ind_high_z_new = ind_high_coeff_new / ind_high_se_new

scalar ind_low_pval_new = 2 * (1 - normal(abs(ind_low_z_new)))
scalar ind_mean_pval_new = 2 * (1 - normal(abs(ind_mean_z_new)))
scalar ind_high_pval_new = 2 * (1 - normal(abs(ind_high_z_new)))

display ""
display "Conditional Indirect Effects (X → M → Y):"
display "=========================================="
display "  At Low W (-1SD):  " %9.6f ind_low_coeff_new "  (SE: " %9.6f ind_low_se_new ", p = " %6.3f ind_low_pval_new ")"
display "  At Mean W:        " %9.6f ind_mean_coeff_new "  (SE: " %9.6f ind_mean_se_new ", p = " %6.3f ind_mean_pval_new ")"
display "  At High W (+1SD): " %9.6f ind_high_coeff_new "  (SE: " %9.6f ind_high_se_new ", p = " %6.3f ind_high_pval_new ")"

// Calculate p-value for index of moderated mediation
scalar index_z_final = index_final / index_se_final
scalar index_pval_final = 2 * (1 - normal(abs(index_z_final)))

display ""
display "Index of Moderated Mediation: " %9.6f index_final "  (SE: " %9.6f index_se_final ", p = " %6.3f index_pval_final ")"

// Check significance of moderated mediation
if index_pval_final < 0.05 {
    display "*** Moderated mediation is statistically significant at p < 0.05"
}
else {
    display "    Moderated mediation is not statistically significant at p < 0.05"
}

display ""
display "==================================================================="
display "DETAILED MODEL RESULTS:"
display "==================================================================="

// Display Path A results
estimates restore mi_path_a
display ""
display "PATH A RESULTS (X → M): Financial Technology → Access to Finance"
display "----------------------------------------------------------------"
estimates replay

// Display Full Model results  
estimates restore mi_full_model
display ""
display "FULL MODEL RESULTS (M + M×W + X → Y): Moderated Mediation Model"
display "---------------------------------------------------------------"
estimates replay

display ""
display "==================================================================="
display "INTERPRETATION GUIDE:"
display "==================================================================="
display "• Direct Effect (c'): Effect of X on Y controlling for M"
display "• Conditional Indirect Effects: Indirect effect X→M→Y at different W levels"
display "• Index of Moderated Mediation: How much indirect effect changes per unit of W"
display "• Significant index indicates that W moderates the X→M→Y pathway"
display "=================================================================="

//
//
//
/*******************************************************************************
* SECTION 12: EXPORT MI RESULTS
*******************************************************************************/
//
// Restore MI model estimates
estimates restore full_model_mi

// Export main results to CSV
esttab . using "hayes_model14_mi_results.csv", ///
    cells("b(fmt(7)) se(fmt(7)) t(fmt(2)) p(fmt(4))") ///
    collabels("Coef" "Std_Err" "t" "P>|t|") ///
    label replace ///
    nomtitles ///
    noobs

// Prepare conditional effects matrix
matrix cond_effects = J(3, 3, .)
matrix rownames cond_effects = "Low_W_(-1SD)" "Mean_W" "High_W_(+1SD)"
matrix colnames cond_effects = "Effect" "Std_Err" "P>|z|"

matrix cond_effects[1,1] = ind_low_coeff, ind_low_se, ind_low_pval
matrix cond_effects[2,1] = ind_mean_coeff, ind_mean_se, ind_mean_pval
matrix cond_effects[3,1] = ind_high_coeff, ind_high_se, ind_high_pval

// Append conditional effects to CSV
esttab matrix(cond_effects) using "hayes_model14_mi_results.csv", ///
    collabels("Effect" "Std_Err" "P>|z|") ///
    mlabels("") ///
    title("Conditional Indirect Effects") ///
    nomtitles ///
    append

// Append Index of Moderated Mediation
file open table using "hayes_model14_mi_results.csv", write append
file write table _n "Index of Moderated Mediation" _n
file write table "IMM," (index_final) _n
file close table

// Save final dataset
save "hayes_model14_mi_analysis.dta", replace
// // End of program




/*******************************************************************************
* SECTION : FOR SUMMARY AND CORRELATION TALBE
*******************************************************************************/


cls
clear all
use data.dta
xtset country_id year, yearly

xtsum fintech finstb gdp access_finance tax_rev

// 3. Visualize key variables
xtline finstb, overlay title("Financial Stability Over Time")
histogram finstb, by(year)

// 4. Summary statistics by groups
bysort country_id: summarize fintech finstb access_finance tax_rev gdp

global varlist "fintech finstb access_finance tax_rev gdp"



// Create correlation matrix with stars
estpost correlate $varlist, matrix listwise
esttab using "correlation_table.rtf", ///
    unstack not noobs compress replace ///
    title("Correlation Matrix") ///
    star(* 0.10 ** 0.05 *** 0.01)

	
	
* for summary table
	
// First, apply descriptive labels to your variables
label  variable finstb "Financial Stability"
label variable fintech "Fintech Adoption"
label variable access_finance  "Access to Finance"
label variable tax_rev "Tax Revenue"
label variable gdp "GDP"

// Now, run estpost and esttab with more options
estpost summarize $varlist

esttab using "Summary_Table.rtf", ///
    cells("mean(fmt(2)) sd(fmt(2)) min(fmt(0)) max(fmt(0)) count(fmt(0))") ///
    collabels("Mean" "Std. Dev." "Min" "Max" "N") ///
    title("Table 1: Summary Statistics") ///
    nonumber ///
    label ///
    addnotes("This table presents summary statistics for the key variables in our dataset." "Source: 2024 World Development Indicators.") ///
    replace
	
	
	
	
	
	
	
	
	
	
	
********* Additional consideration******
*by panelvar: summarize varname    // Summary by panel
*bysort timevar: summarize varname // Summary by time period
*pwcorr $varlist, sig    // Pairwise correlations
*spearman $varlist       // Spearman correlations

//ssc install corrtable
//corrtable varlist, fmt(%4.3f) star
***************	***********************