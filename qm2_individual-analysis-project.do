* QM2 Indivudual Analysis Project
* Topic: Management and Development Economics
* March 13, 2019
*------------------------------------------------------------------*
// cd "C:\Users\amyhuang\Desktop" 
clear 
capture log close 
set more off 
log using iap_huang.smcl, replace
use IAP_MD.dta, clear
save IAP_MD_huang.dta, replace

*--------------------------Data Overview--------------------------*
desc
codebook EaseofBusiness //27 missing values
li Country EaseofBusiness if EaseofBusiness ==.
drop if EaseofBusiness ==.
* The unit of observation is country, 27 of 216 countries are missing Ease of 
* Doing Business(EDB) scores. Dropping out countries without EDB scores to 
* prevent those data bias the results.

lab var CAB "Current Account Balance (% of GDP)"
lab var EaseofBusiness "Ease of Doing Business Score (0-100)"

*-------------------------Model Building-------------------------*
pwcorr EaseofBusiness CAB, star(.05) // correlation: .3653*

/* Motivating graph */
tw lfit EaseofBusiness CAB, lc(cranberry) || ///
	scatter EaseofBusiness CAB, graphregion(color(white)) mc(black) ///
	msize(small) legend(off) ytitle("Ease of Doing Business Score" "(0-100)") ///
	title("Ease of Doing Business Score" "Against Current Account Balance") ///
	note("Note: There is a pattern that countries with higher proportion of current account balance to GDP" "have higher Ease of Doing Business Scores.")
gr export "iap_huang-1.png", replace

/* Regrssion Analysis */
eststo clear
eststo iap1: reg EaseofBusiness CAB
//current account balance has statistically positive impact on EDB score

/* Control other factors */
d GDPpercap_2010const EXPTime_BorderComp EXPTime_DocComp ElectricityAccess

/* Variables Reproduction & Transformation */
* Adding up border compliance time and documentary time to estimate the 
* total export time
gen ExpTime = EXPTime_BorderComp+EXPTime_DocComp
label var ExpTime "Time to export: Documentary compliance and Border compliance (hours)"

* Log GDP per capita, Days to deal with construction permits, and Time to 
* export to eliminate the effect of the skewness of data
gen lGDPpercap_2010const = ln(GDPpercap_2010const)
label var lGDPpercap_2010const "Logged GDP per capita (constant 2010 US$)"
gen lExpTime = ln(ExpTime)
label var lExpTime "Logged Time to export: Documentary compliance and Border compliance (hours)"

* Correlation of independent variables
pwcorr CAB lGDPpercap_2010const lExpTime ElectricityAccess, star(.95)
reg EaseofBusiness CAB lGDPpercap_2010const lExpTime ElectricityAccess
// GDP per capita, export time, and electricity access are correlated with current account balance ratio, and have impact on EDB score, indicating omitted variable bias in the first model

eststo iap2: reg EaseofBusiness CAB lGDPpercap_2010const 
eststo iap3: reg EaseofBusiness CAB lGDPpercap_2010const lExpTime
eststo iap4: reg EaseofBusiness CAB lGDPpercap_2010const lExpTime ElectricityAccess
// the impact of current account balance decreases when we add more explanatory variables to the model, and losses statistical significance in the forth regression; logged GDP per capita reports the highest coefficient

*--------------------Regression Diagnostic------------------*
/* Scatter plot */
* Model Accuracy
reg EaseofBusiness CAB lGDPpercap_2010const lExpTime ElectricityAccess
predict EDB_hat
scatter EaseofBusiness EDB_hat, msize(small) mc(black) || ///
	scatteri 30 30 85 85, recast(line) lpattern(vshortdash) lc(cranberry) ///
	legend(order (1 "Ease of Doing Business" "Score (0-100)" 2 "45-degree line") ring(0) row(2) bplace(se)) ///
	ytitle("Actual value") ///
	xtitle(Predicted value) xlab(20(20)100) ///
	graphregion(color(white)) ///
	title("Predicted vs Actual") ///
	note("Note: There is a strong correlation between the model's predictions and its actual values.")
gr export "iap_huang-2.png", replace


/* Collinearity */
reg EaseofBusiness CAB lGDPpercap_2010const lExpTime ElectricityAccess
vif //VIFs are all smaller than 3, low collinearity

/* Outlier */
reg EaseofBusiness CAB lGDPpercap_2010const lExpTime ElectricityAccess
predict stud, rstudent
predict lev, leverage
predict cooksd, cooksd
predict dfits, dfit
gen obs=_n
gen absrstud=abs(stud)
label var absrstud "Absolute Value of Studentized Residuals"
gen absdfits=abs(dfits)
label var absdfits "Absolute Value of DFFIT"
gen absrstudsq=abs(stud)
label var absrstudsq "Studentized Residuals Squared"
ereturn li
scalar k=e(df_m) // k=4	
scalar nn=e(N) //nn=153

* Outlier under various thresholds 
count if abs(stud)>2 & stud!=. 
count if lev>(2*k+2)/nn & lev!=. 
count if cooksd>4/nn & cooksd!=. 
count if abs(dfits)>2*sqrt(k/nn) & dfits!=.	
count if (absrstud>2 & lev>(2*k+2)/nn & cooksd>4/nn) & (absrstud!=. & lev!=. & cooksd!=.)
count if (absrstud>2 & lev>(2*k+2)/nn & absdfits>2*sqrt(k/nn)) & (absrstud!=. & lev!=. & absdfits!=.)
count if (absrstud>2 & lev>(2*k+2)/nn & cooksd>4/nn & absdfits>2*sqrt(k/nn)) & (absrstud!=. & lev!=. & cooksd!=. & absdfits!=.)  //no extreme outlier
* Generate a variable for outliers of any threshold (1: outlier; 0: not outlier)
gen outlier=0
replace outlier=1 if (abs(stud)>2 | lev>(2*k+2)/nn | cooksd>4/nn | abs(dfits)>2*sqrt(k/nn)) //56 outliers generated

/* Scatter plot - Outliers */
twoway scatter lev absrstud if outlier==0 & lev !=. & absrstud!=., mcolor(black) msize(small) || ///
	scatter lev absrstud if outlier==1 & lev !=. & absrstud!=., mcolor(cranberry) msize(small) mlabel(CountryCode) mlabsize(tiny) ///
	xtitle("Standardized Residuals Squared") ///
	ytitle("Leverage") ///
	graphregion(color(white)) ///
	legend(label(1 "Non-Outliers") label(2 "Outliers") bplace(ne) ring(0) col(1)) ///
	title(Outliers) note("Note: Outliers are counted on the thresholds of rstudent, leverage, Cook's distance, and DFFIT. Outliers are lebeled with country code.")
graph export "iap_huang-3.png", replace

/* Regression without outliers */
eststo iap5:reg EaseofBusiness CAB lGDPpercap_2010const lExpTime ElectricityAccess if outlier==0
* the model regains significance after removing outliers

/* Heteroskedasticity */
rvfplot, yline(0) mlabsize(small) msize(small) mc(black) ///
	title("Residual Plot") ///
	xtitle("Predicted values for Ease of Doing Business Score") graphregion(color(white)) ///
	note("Note: Residuals are distributed symmetrically and have no clear patterns;" "however, they are clustered around +10 and -10 of the y-axis.")
gr export "iap_huang-5.png", replace

hettest
imtest, white

* Robust Regression
eststo iap6: reg EaseofBusiness CAB lGDPpercap_2010const lExpTime ElectricityAccess, robust


* Export regression table
esttab iap* using iap_tab.tex, replace se ar2 scalars(rmse) title(Regression Table\label{tab1}) note()


*-------------------------Closing commands----------------------------*
log close
clear
