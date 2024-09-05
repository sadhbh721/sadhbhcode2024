// Sadhbh O'Foghlu (23212356) Econometrics Assignment 1
// ECON42700: March 2024 
*Using "Trade Data project.dta"


clear 
clear all
use "C:\Users\sadhb\OneDrive\Documents\Econometrics 2024\Trade Data project.dta"

//Q1. (A)
////// Problems in Levels //////

//// Start with OLS //// **
*linear model
*regress the variables to determine their significance
global xlist landlocked_im landlocked_ex logGDPC_im logGDPC_ex remoteness_im remoteness_ex logdist logGDP_ex logGDP_im comlang colonial openness comFTA
global ylist trade
reg $ylist $xlist
*landlocked importer, landlocked exporter, log GDP importer, log GDP exporter, common language, colonial and openess all insignificant.

// OLS Estimate with Robust Results 
reg $ylist $xlist, robust
estimates store OLS_Estimates
predict yhat_ols1
*robust standard errors used to account for heteroskedasticity and is less sensitive to outliers. 
*colonial and openness are still insignificant with robust standard errors. 
*the OLS coefficients are large which is due to how trade is measured in levels of 1000.

// Scatter plot for OLS Predicted values
twoway (scatter trade yhat_ols, mlabposition(0) mlabsize(small) mlabcolor(pink)), ///
       legend(label(1 "OLS")) title("OLS Predicted Values vs. True Values")

// Summary Statistics 
summarize $ylist $xlist
*18,360 observations, all coefficients are significant at the 5% level except for openess and colonial (p>0.05)
corr $xlist
*check correlation between independent variables 

summarize trade
tabulate trade if trade==0
tabulate trade if trade > 10000000
histogram trade if trade > 10000000, discrete freq start(10000000)
tabulate trade if trade < 1000
histogram trade if trade < 1000, discrete freq start (0)
*max is 101,000,000(1.01e+08) and the minimum value is 0. (remember this is measured in 1000s)
*8,747 observations are zero values. 
*65 observations > 10,0000
*There are too many values to have them all in one histogram. 

// Check for Heteroskedasticity
reg $ylist $xlist
estat hettest, rhs iid
*There is heteroskedasticity in the model, variance of residuals is not constant, violates linear model assumption.   

//// Poisson Model //// **
*probability distribution for count data, calculates the probability of y taking on a specific discrete value. 

// Poison Model Coefficients 
poisson $ylist $xlist, robust 			// Coefficients semi elasticities 
estimates store Poisson_Estimates


display exp(-.68)
display exp(-.847)
display exp(.128)
display exp(.149)
display exp(.610)
display exp(.709)
display exp(-.832)
display exp(.740)
display exp(.748)
display exp(.797)
display exp(.012)
display exp(-.070)
display exp(.212)
display exp(-33.09)
*one unit increase in independent variable causes dependent variable to increase by a factor of e to the coefficient.

// Poisson Regression Predicted Values
predict yhat_pois1, n
summarize yhat_pois1
summarize $ylist

// Goodness of Fit Measure
poisson trade $xlist, nolog          
correlate trade yhat_pois1
display "Squared correlation between y and yhat = " r(rho)^2
*this suggests a strong relationship between the predicted and true value of the dependent variable at .8574. 

// Poisson Log Likelihood
capture program drop mypoisson_lf
program mypoisson_lf
  args lnf xb
  tempvar lnyfact mu
  local y "$ML_y1"
  generate double `lnyfact' =lnfactorial(`y')
  generate double `mu' = exp(`xb')
  quietly replace `lnf' = -`mu' + `y'*`xb' - `lnyfact'
 end
 
gen log_likelihood_poisson = $ylist* ln(yhat_pois1) - yhat_pois1 - lnfactorial($ylist)
summarize log_likelihood_poisson


// Scatter plot for Poisson Predicted Values
twoway (scatter trade yhat_pois, mlabposition(0) mlabsize(small) mlabcolor(red)), ///
       legend(label(1 "Poisson")) title("Poisson Predicted Values vs. True Values")


// Poisson Model Marginal Effects
poisson $ylist $xlist 
margins, dydx(*)  						// AME 

poisson $ylist $xlist
margins, dydx(*) atmeans 				// MEM

// Test for Overdispersion
*generate a y* (latent variable which cannot be directly observed)
quietly poisson $ylist $xlist, vce(robust)
predict muhat1, n
quietly generate ystar1 = ((trade-muhat1)^2 - trade)/muhat1
regress ystar1 muhat1, noconstant noheader
test muhat1 = 1
*There is overdispersion in the data, should try a different estimation model. 

tabstat trade, stats(mean var)
*mean and variance are not equal (overdispersion)

// Poisson Distribution

set obs 18360  // 
generate ypois1 = rpoisson(1)
generate ypois3 = rpoisson(3)
generate ypois5 = rpoisson(5)
generate ypois10 = rpoisson(10)

*asymmetrical, longer tail, shows how distribution changes as lambda varies. 
graph twoway (histogram ypois1, lcolor(blue) fcolor(blue)) ///
              (histogram ypois3, lcolor(yellow) fcolor(yellow)) ///
              (histogram ypois5, lcolor(purple) fcolor(purple)) ///
              (histogram ypois10, lcolor(pink) fcolor(pink)), ///
              title("Poisson Distribution") ///
              legend(label(1 "mu=1") label(2 "mu=3") label(3 "mu=5") label(4 "mu=10")) ///
              graphregion(fcolor(white) lcolor(white)) ///
              legend(region(lcolor(white))) ///
              plotregion(style(none) c(white) fc(white))

foreach num in 1 3 5 10 {
    preserve // Preserve the dataset before modifications
    generate dd`num' = 1
    collapse (sum) dd`num', by(ypois`num')
    gen d`num' = dd`num' / 10000
    restore // Restore the dataset to its original state
}

	
/// Quasi-Poisson /// **
*Useful when there is overdispersion in count data 
glm $ylist $xlist, family(poisson) link(log) vce(robust)
estimates store Quassi_Poisson
*colonial, openess and comFTA are insignificant 
*Quasi-Poisson yields the same results as the Poisson with standard errors so we will just refer to the Poisson now. 
	

/// Zero Inflated Poisson (ZIP) /// **
*another method to tackle excess zeros 
*joint estimation treats getting zero as a success. So the probability of getting zero is a success. 
zip trade $xlist, inflate($xlist) nolog
estimates store Zero_Inflated_Estimates




//Q1 (B) 
////// Log Transforming the Data //////

*logging zero values does not work since it generates missing values. 
*log of zeros results in a selection problem - excluding part of the sample of interest. 

// OLS Log of Trade 
reg logtrade $xlist 
generate result_missing = 9613/18360
display result_missing
*52.36% of observations remain while 47.64% are now considered missing values. 

// OLS Estimates on Log Transformed Dependendent Variable
reg logtrade $xlist
estimates store logtrade_OLS

// Data Set up 
set obs 18360
set seed 200
gen x1=rnormal(1,1)
gen x2=rnormal(1,1)
gen u=rnormal(0,1)
gen y_latent=0.5*x1+3*x2+u
gen y=floor(max(y_latent, 0))
su
su y, det
scatter y y_latent
histogram y, discrete

preserve
gen y_star=1
collapse (sum) y_star, by(y)
gen Density=y_star/1000

/// Truncated Count Data Model /// 

/// Two Step Model /// **
*maximum likelihood estimation separately maximises the zeros and the positive values. 
*another estimator to address excess zeros 

// First Step:Logit
logit $ylist $xlist, nolog
*if it says variable trade not found, just run it again 
estimates store Logit_Estimates
predict logitxb, xb 

// Marginal effects of Logit 
quietly logit $ylist $xlist, vce(robust)
margins, dydx(*) atmean noatlegend

// Negative Binomial Model - A method that tackles data containing lots of zeros
nbreg $ylist $xlist, nolog
quietly nbreg trade $xlist, robust
estimates store NegBinom
*alpha is the overdispersion parameter, which shows alpha is significant at 9.382689

// Negative Binomial Marginal Effects 
margins, dydx(*) 					
estimates restore NegBinom
margins, dydx(*) atmeans 

// Negative Binomial Predicted Values
predict yhatNB, n
summarize yhatNB
summarize $ylist 

// Zero truncated negative binomial
*only looks at trade values greater than zero 
ztnb $ylist $xlist if $ylist>0, nolog
estimates store NegBinom_ZT

// Marginal effects of Zero-truncated Negative Binomial 
ztnb trade $xlist if trade>0, nolog 

quietly ztnb trade $xlist if trade>0, vce(robust)
margins, dydx(*) atmean noatlegend


/// Solve Selection Problem ///

*******// Heckman 2-step *******
*regression model with sample selection
*used to identify, correct and ommit the sample selection bias
*probit assumes underlying normally distributed latent variable 
*create a binary variable to estimate participation

gen participant=(trade>0)
tabstat trade, by(participant) stat(mean sd min max n)
reg trade participant

probit participant $xlist
*probability of being a participant based on the other independent variables. 
*remoteness of importer and remoteness of exporter do not have a significant effect on participation. 

predict yhat4, xb
reg trade $xlist, robust 
*what variables have an effect on whether you participate but do not have an effect on trade 
*repeat from earlier, remember colonial and openess do not have a significant effect on trade but they both effect participation (exclusion restriction)

//Inverse Mills Ratio
gen imr = normalden(yhat4)/normal(yhat4)
label variable imr "Inverse Mills ratio"

// Second Step
*outcome equation
reg trade landlocked_im logGDPC_im logGDPC_ex remoteness_im remoteness_ex logdist logGDP_ex logGDP_im comlang comFTA imr 
*imr is statistically significant, therefore there was sample selection bias 


heckman trade logdist logGDP_ex logGDP_im comlang comFTA, select(participant = logGDPC_ex logGDPC_im logGDP_ex logGDP_im logdist comlang colonial openness comFTA) twostep
estimates store Heckman_Selection
*inverse mills coefficient is significant
*all coefficients are significant 




//Q1 (C) 
////// Adding a Constant to Trade Flows //////

gen tradenew =trade +1000 
gen lntradenew = ln(tradenew)

summarize lntradenew 

// OLS for the log of the new trade variable
reg lntradenew $xlist 
estimates store ols_regress_tradenew
tabulate lntradenew if lntradenew==0
*The log of this new variable "tradenew" does not generate any missing values as there are no zero values. 
*all variables are significant except for remoteness of importer

// Check for Heteroskedasticity 
estat hettest, rhs iid

//// Tobit Model //// **
tobit lntradenew $xlist, ll(0)
estimates store tobit_regress_tradenew
*all independent variables are significant except for remoteness of importer 


gen x5=rnormal(20,1) 			//
gen u1=rnormal(0,1)				//
scalar beta0=-20
scalar beta1=1
gen ystar2=beta0+beta1*x5+u1     //generate latent outcome variable y*   

gen ytrunc=ystar2 if ystar2>0     //generate truncated outcome variable
gen ycens=ystar2 if ystar2>0      //generate censored outcome variable
replace ycens=0 if ycens==.

//running OLS on latent variable 
reg ystar2 x5
estimates store y_star2
predict ystar_p

//running OLS on censored variable
reg ycens x5
estimates store y_cens
predict ycens_p

//running OLS on truncated variable
reg ytrunc x5
estimates store y_trunc
predict ytrunc_p
	
// OLS Results for y* applying censoring and truncation 

	esttab y_star2 y_cens y_trunc, ///
    cells("b(fmt(3) star)" "se(fmt(3) par)" "p(fmt(3) star)") ///
    stats(N r2) ///
    starlevels(* 0.05 ** 0.01 *** 0.001) ///
    title(OLS Results) ///
    mlabels("yi*" "yi censored at 0" "yi truncated at 0") ///
    collabels("x5" "Constant")

///// Tobit Results /////
// Tobit for Censored Data 
tobit ycens x5 , ll(0)
estimates store tobit_censored
margins, dydx(x5) predict(ystar(0,.)) atmeans

// Tobit for Truncated Data
truncreg ytrunc x5 , ll(0)
estimates store tobit_truncated
margins, dydx(x5) predict(e(0,.)) atmeans

/// Tobit Results 
esttab tobit_censored tobit_truncated, ///
    cells("b(fmt(3) star)" "se(fmt(3) par)" "p(fmt(3) star)") ///
    stats(N r2) ///
    starlevels(* 0.05 ** 0.01 *** 0.001) ///
    title(Tobit Results) ///
    mlabels("yi censored at 0" "yi truncated at 0") ///
    collabels("x5" "Constant")
 

// Kernel Density Plot for trade before transformation
 kdensity trade, bwidth(0.20) normal n(4000)
 *it is evident there is a large cluster of zeros

// Kernel Density Plot for trade after added constant and log transformation
 kdensity lntradenew, bwidth(0.20) normal n(4000)
 *now the density is more right skewed, there is no cluster of zeros 




// Q2 One Big Table of Results 


// (A)
// Compare OLS, Poisson Estimates, Quassi-Poisson Estimates and ZIP in one table 
ssc install estout
estout OLS_Estimates Poisson_Estimates Zero_Inflated_Estimates Quassi_Poisson, ///
    cells(b(star fmt(%9.2f)) se(par fmt(%9.2f))) ///
    stats(r2 N ml) ///
    varwidth(20) ///
    label ///
    legend ///
    starlevels(* 0.05 ** 0.01 *** 0.001)
 
*poisson and quassi-poisson are identical in the estimates which suggests there is not significant overdispersion. While the ZIP is not too far off. 
*R^2 for OLS is quite low with the independent variables only accounting for 9% of the variation in trade. 
*poisson estimates coefficients are interpreted differently - one unit increase in independent variable causes dependent variable to increase by a factor of e to the coefficient. Also the coefficients are adjusted for robust SE. 
*quassi-poisson allows for overdispersion, coefficients interpreted same as poisson
*poisson - assumptions do not hold since there is equidispersion 



// (B)
// Table showing Two-Step Model (Logit and Negative Binomial)
estout Logit_Estimates NegBinom_ZT, ///
    cells(b(star fmt(%9.2f)) se(par fmt(%9.2f))) ///
    stats(r2 N ml) ///
    varwidth(20) ///
    label ///
    legend ///
    starlevels(* 0.05 ** 0.01 *** 0.001)

*this method allows excess zeros and overdispersion
*a positive coefficient in the logit model implies that the probability of y=1 is increasing, meaning that trade is above 0. 
*mainly used for count data - will try tobit in the next step 

// Heckman Two-Step Model
estout Heckman_Selection, ///
    cells(b(star fmt(%9.2f)) se(par fmt(%9.2f))) ///
    stats(r2 N ml) ///
    varwidth(20) ///
    label ///
    legend ///
    starlevels(* 0.05 ** 0.01 *** 0.001)

// Table showing Two-Step and Heckman Model
estout Logit_Estimates NegBinom_ZT Heckman_Selection, ///
    cells(b(star fmt(%9.2f)) se(par fmt(%9.2f))) ///
    stats(r2 N ml) ///
    varwidth(20) ///
    label ///
    legend ///
    starlevels(* 0.05 ** 0.01 *** 0.001)

*logit estimates probability of observing trade>0 
*Heckman combines probit on the selection equation and OLS on the outcome equation which includes the IMR as a regressor. 
*Heckman model appears to be the best fit and is easy to implement - but not as efficient as MLE 

// (C)
/// Compare OLS and Tobit Results
estout ols_regress_tradenew tobit_regress_tradenew, ///
    cells(b(star fmt(%9.2f)) se(par fmt(%9.2f))) ///
    stats(r2 N) ///
    varwidth(20) ///
    label ///
    legend ///
    starlevels(* 0.05 ** 0.01 *** 0.001)
*since there are no longer any zero observations due to the added constant and log transformation, the OLS and Tobit yield the same results. 
	
// Table of all Results 
	estout OLS_Estimates Poisson_Estimates Zero_Inflated_Estimates Quassi_Poisson Logit_Estimates NegBinom_ZT ols_regress_tradenew tobit_regress_tradenew Heckman_Selection, ///
    cells(b(star fmt(%9.2f)) se(par fmt(%9.2f))) ///
    stats(r2 N ml) ///
    varwidth(20) ///
    label ///
    legend ///
    starlevels(* 0.05 ** 0.01 *** 0.001)
	
// Table of Condensed Results 
	estout OLS_Estimates Zero_Inflated_Estimates Quassi_Poisson ols_regress_tradenew tobit_regress_tradenew Heckman_Selection, ///
    cells(b(star fmt(%9.2f)) se(par fmt(%9.2f))) ///
    stats(r2 N ml) ///
    varwidth(20) ///
    label ///
    legend ///
    starlevels(* 0.05 ** 0.01 *** 0.001)
	
	
	

// Q3 (A)
*Marginal effect for log distance for all methods (logdist)

**Average Marginal Effect (AME) - calculates the marginal effect for each observation of the independent variable and then the marginal effects are averaged across all observations to compute the overall effect.

**Marginal Effect at Sample Mean (MEM) - calculates the average of each variable and obtains the marginal effect for an arbitrary person with those mean values. The key thing is this arbitrary person does not exist. It focuses on average sample characteristics. 


// OLS
reg $ylist $xlist
margins, dydx(logdist)
estimates store OLS_AME
*for OLS the marginal effect on the independent variables on the dependent variable is simply equal to the coefficients - coefficient of x1 is given by Beta 1 
*for log variables - interpret the effect in percentage terms 
*log distance for OLS - -184363.1 meaning that for a 1% increase in distance, trade is predicted to decrease by -184363.1 units. 

marginsplot

// Poisson Regression 
poisson $ylist $xlist 
margins, dydx(logdist)  					// AME 
estimates store Poisson_AME
*-.8325245 is the coefficient for logdist
*AME for logdist = -143302 meaning this is the expected average effect of the decrease in counts of trade when distance increases 1%. 

poisson $ylist $xlist
margins, dydx(logdist) atmeans				// MEM
estimates store Poisson_MEM
*MEM for logdist = -2478.077 showing a decrease in expected counts of trade as the distance increases 1% for this arbitrary person. 

// Zero Inflated Poisson (ZIP)
zip trade $xlist, inflate($xlist) nolog

margins, dydx(logdist)                     // AME
estimates store ZIP_AME
* AME for logdist = -143613.3 which is very close to Poisson value since this is the Poisson component of the model. It presents the expected average effect of the decrease in counts of trade when distance increases 1%. 

margins, dydx(logdist) atmean noatlegend    // MEM
estimates store ZIP_MEM
*MEM for logdist = -2227.98 which is also similar to Poisson value since it is still the Poisson component of model. It shows a decrease in expected counts of trade as the distance increases 1% for this arbitrary person.

margins, dydx(logdist) predict(equation(inflate)) atmean noatlegend  // MEM
estimates store ZIP_InflateMEM
* .7593844 - this is the inflation component of the ZIP. This presents the probability of observing excess zeros is .7593844 for an arbitrary individual. Taking the exponent of this gives us 2.137. This marginal effect is positive since it views probability of being zero as a success. 

// Negative Binomial 
nbreg $ylist $xlist, nolog
quietly nbreg trade $xlist, robust
*negative binomial accounts for the overdispersion which the Poisson does not. 

margins, dydx(logdist)  							// AME
estimates store NegBinom_AME
*AME for logdist = -5802416 showing a decrease in expected average counts of trade when distance increases 1%. 


margins, dydx(logdist) atmeans 						// MEM
estimates store NegBinom_MEM
*MEM for logdist = -2186.916 showing showing a decrease in expected counts of trade as the distance increases 1% for this hypothetical person. 


// Two Step Model

*Marginal Effect of Logit
quietly logit $ylist $xlist, vce(robust)

margins, dydx (logdist) 							// AME
estimates store Logit_AME
*AME for logdist = -.0911639 

margins, dydx(logdist) atmean noatlegend 			// MEM
estimates store Logit_MEM
*MEM for logdist = -.1836652 shows as distance increases 1%, the likelihood of observing trade decreases by -.1837.

*Marginal Effect of Zero-truncated Negative Binomial 
*deals with the excess zeros in trade and focuses on only positive values of trade. 
ztnb trade $xlist if trade>0, nolog 

margins, dydx (logdist) 							//AME
estimates store ZeroT_NB_AME
* AME for logdist = -372809.3 


margins, dydx(logdist) atmean noatlegend 			// MEM
estimates store ZeroT_NB_MEM
 *MEM for logdist = -19886.21


// Heckman Selection Model //
*ME of x1 has 2 elements: the direct effect on the mean of y2 and the effect through IMR which increases the probability of observing y2 through Pr(y1=1)
*generated a variable for participant in Q1(B) to show which countries do participate in trade

// Selection Equation
probit participant $xlist
*probability of being a participant in trade, not the probability of trading itself. 
mfx, predict(p)
*this method also gives the result -.1587999. It determines the marginal effects based on the predicted probabilities.

margins, dydx (logdist)								// AME
estimates store Probit_AME
*AME for logdist = -.0867756 meaning a 1% increase in log distance is linked with an 8.68% decrease in the probability of a country participating in trade.

margins, dydx (logdist) atmean						// MEM
estimates store Probit_MEM
*MEM for logdist = -.1587999 meaning a 1% increase in log distance relates to a 15.88% decrease in the probability of participating in trade. 

// Outcome Equation
heckman trade logdist logGDP_ex logGDP_im comlang comFTA, select(participant = logGDPC_ex logGDPC_im logGDP_ex logGDP_im logdist comlang colonial openness comFTA) twostep
estimates store Heckman_Selection

margins, dydx (logdist)								// AME
estimates store Heckman_AME
 *-601886.7 

margins, dydx (logdist) atmean 						// MEM
*-601886.7 
estimates store Heckman_MEM

*AME and MEM are the exact same for the Heckman outcome equation so for every 1% increase in distance the expected change in trade when we account for sample selection is equal to -601886.7. 


// Tobit Model 
*deals with trade transformed with added constant and is then log transformed. 
tobit lntradenew $xlist, ll(0)

margins, dydx (logdist) 
estimates store Tob_AME 							// AME
* -.5016262 

margins, dydx (logdist) atmean 
estimates store Tob_MEM								// MEM
 *-.5016262

*It makes sense that AME and MEM are the same for the Tobit model with the log of the new trade variable since essentially it is just OLS now since there are no excess zero values. So for a 1% increase in distance, trade is expected to decrease -.5016262.
*More distance between countries presents reluctance and decreases in trade. 

// Tobit for Censored Data 
*9058 observations are left censored
tobit ycens $xlist , ll(0)
margins, dydx(logdist) predict(ystar(0,.)) atmeans 			// MEM
estimates store TobCens_MEM
*.0060824 meaning a 1% increase in log distance is connected to an increase in the value of the censored outcome. 

// Tobit for Truncated Data
*0 observations are truncated
truncreg ytrunc $xlist , ll(0)
margins, dydx(logdist) predict(e(0,.)) atmeans				// MEM
estimates store TobTrun_MEM
*-.0064473  
	
// Load saved estimates
est restore OLS_AME
est restore Poisson_AME
est restore Poisson_MEM
est restore ZIP_AME
est restore ZIP_MEM
est restore ZIP_InflateMEM
est restore NegBinom_AME
est restore NegBinom_MEM
est restore Logit_AME
est restore Logit_MEM
est restore ZeroT_NB_AME
est restore ZeroT_NB_MEM
est restore Probit_AME
est restore Probit_MEM
est restore Heckman_AME
est restore Heckman_MEM
est restore TobCens_MEM
est restore TobTrun_MEM
est restore Tob_AME
est restore Tob_MEM

clear
*have to clear so that the table below will work 
*output was too messy trying to compile all the data 

//// Table of MEM and AME for Log Distance ///// ***
input str20 Estimation_Model AME_LOGDIST MEM_LOGDIST
"OLS" -184363.1 .
"Poisson" -143302 -2478.077
"ZIP" -143613.3 -2227.98
"ZIP-Inflated" . 0.7593844
"Negative Bin" -5802416 -2186.916
"Logit" -0.0911639 -0.1836652
"Zero-truncated NB" -372809.3 -19886.21
"Probit" -0.0867756 -0.1587999
"Heckman" -601886.7 -601886.7
"Tobit Censored" . 0.0060824
"Tobit Truncated" . -0.0064473
"Tobit" -0.5016262 -0.5016262
end

list
*the command list works, it shows the AME and MEM of log distance for each model in a neat table.

use "C:\Users\sadhb\OneDrive\Documents\Econometrics 2024\Trade Data project.dta"
*have to open data set again now or import the excel trade data file


// Graph of Trade against Log Distance 
twoway (scatter trade logdist, mcolor(purple) title("Trade against Log Distance")) 

corr trade logdist 
*indicates a weak negative correlation. 

*have to define this again
gen tradenew = (trade +1000)
gen lntradenew = ln(tradenew)

// Graph of Log of Trade against Log Distance 
twoway (scatter lntradenew logdist, mcolor(purple) title("Log of Trade against Log Distance"))
*this changes the points of data as they shift upwards 

corr lntradenew logdist 
*shows a stronger negative correlation than previous.

*summary statistics for log distance 
summarize logdist



// Q3(B) How do we interpret the effect of distance on trade? 
*log distance is significant in each model.
*log distance refers to a percent change in distance. 


*have to define these again
global xlist landlocked_im landlocked_ex logGDPC_im logGDPC_ex remoteness_im remoteness_ex logdist logGDP_ex logGDP_im comlang colonial openness comFTA
global ylist trade
gen participant=(trade>0)

// OLS
reg $ylist $xlist
*marginal effects are the same as slope coefficients 
*slope coefficient and ME for logdist = -184363.1 - as distance increases 1%, trade decreases by 184363 units. 

// Poisson
poisson $ylist $xlist, robust 
display exp(-.8325245)
*= .43494987 - this is how we interpret the log distance coefficient 
*for a 1% increase in distance, the count of trade is expected to decrease by a factor of .4349
*we don't include the Quasi-Poisson here since it generates the same results and coefficients as the Poisson. 

// ZIP // 
zip trade $xlist, inflate($xlist) nolog

// Count Component
display exp(-.827551 )
*take exponent of coefficient to get effect
*= .43711848 - for a 1% increase in distance, expected count of trade decreases by a factor of 0.437. 
*(1-0.437 = 0.563)

// Inflation Component 
*take exponent of coefficient to get effect
display exp(.7593844)
*2.1369603 - when distance increases 1%, the probability of getting a zero in the trade data increases by a factor of 2.137 
*treats zero as a success 

// Negative Binomial 
nbreg $ylist $xlist, nolog
quietly nbreg trade $xlist, robust

display exp(-1.482922)
*= .2269735 - the expected count of trade will decrease by a factor of .227 when distance increases 1%. (1-.227 = 0.773)

// Two-step //
// Logit 
logit $ylist $xlist, nolog
display exp(-0.7594013)
* = .4679465 - for a 1% increase in distance, the chance of trade being positive decreases by a factor of 0.468. (1-0.468 = 0.532)
*a negative coefficient for log distance implies that the probability that trade is greater than 0 is decreasing. 

// Zero-truncated Negative Binomial
ztnb trade $xlist if trade>0, nolog 
display exp(-1.007158)
*= .36525556 - for a 1% increase in distance, excluding trade at or below 0, the expected count of trade will decrease by a factor .365. 
*(1-.365 = .635)

// Heckman Selection Model //
// Probit
probit participant $xlist
display exp(-.4062081)
* = .66617152 - for a 1% increase in distance the probability of participating in trade decreases by a factor of .666. 

// Outcome equation
heckman trade logdist logGDP_ex logGDP_im comlang comFTA, select(participant = logGDPC_ex logGDPC_im logGDP_ex logGDP_im logdist comlang colonial openness comFTA) twostep
*OLS is run on the outcome equation
*trade = -601886.7 - for a 1% increase in distance trade decreases 601,886 units. 
*participant = -.4054951 - for a 1% increase in distance while accounting for sample selection with the IMR, participating in trade decreases -.405 units. 

// Tobit Model 
*remember this is the log of trade so we can interpret it as a percentage change in trade 
tobit lntradenew $xlist, ll(0)
* = -.5016262. 
*there is no longer excess zeros since trade has been transformed, we can interpret it the same as OLS. So the marginal effect is equal to the coefficient of -.5016 so a 1% increase in distance will decrease trade by 0.5016%. 





 




