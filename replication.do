sysdir set PLUS "C:\Users\beyogr23\Downloads\Replication"
cd "C:\Users\beyogr23\Downloads\Replication"
use hrs_heads.dta, clear

*cleaning given by the authors*
*-------------------------------
* Create Variables and Labels
*-------------------------------
{

*education
g rlowed = raedyrs  <12
	replace rlowed=. if missing(raedyrs)
	label var rlowed "Less than HS"
*Here, the authors are generating a new dummy variable called rlowed that accounts for people that completed less than 12 years of education, therefore less than highschool level. 
*Then they replace the missing values with a dot. After that, they labelled the new variable with "Less than HS" *

*marital status
g couple = rmstat == 1 | rmstat == 2
	replace couple=. if missing(rmstat)
	label var couple "Couple"
*they create a new dummy variable that accounts for couple based on the marital status variable rmstat. Then, they create a label "Couple"
g nevermarried = rmstat==8
	replace nevermarried=. if missing(rmstat)
	label var nevermarried "Nevermarried"
*they create a new dummy variable that accounts for people that were never married based on the marital status variable rmstat. Then, they create a label "Nevermarried"
g divorcedorseperated = rmstat==4 | rmstat==5 | rmstat==6
	replace divorcedorseperated=. if missing(rmstat)
	label var divorcedorseperated "Divorced or Separated"
*they create a new dummy variable that accounts for people that are divorced or seperated based on the marital status variable rmstat. Then, they create a label "Divorced or Separated"
g widowed = rmstat==7
	replace widowed=. if missing(rmstat)
	label var widowed "Widowed"

*they create a new dummy variable that accounts for widowed based on the marital status variable rmstat. Then, they create a label "Widowed"
	
*demographics
g female = ragender == 2
	replace female=. if missing(ragender)
	label var female "Female"
*they create a new dummy variable that accounts for females based on the gender variable ragender. Then, they create a label "Female"
g hispanic = rahispan == 1
	replace hispanic=. if missing(rahispan)
	label var hispanic "Hispanic"
*they create a new dummy variable that accounts for hispanic people based on the hispanic dummy variable rahispan. Then, they create a label "Hispanic"

g white = raracem == 1
	replace white=. if missing(raracem)
	label var white "White"
*they create a new dummy variable that accounts for white people based on the race variable raracem. Then, they create a label "White"

g black = raracem == 2
	replace black=. if missing(raracem)
	label var black "Black"
*they create a new dummy variable that accounts for black people based on the race variable raracem. Then, they create a label "Black"

g other = raracem == 3
	replace other=. if missing(raracem)
	label var other "Other Race"	
*they create a new dummy variable that accounts for other race based on the race variable raracem. Then, they create a label "Other Race"

label var hchild "Num. children"
*They labelled the hchild variable "Num. children"*

g t = Year-1990
	label var t "Survey Year"
g t2 = t*t
	label var t2 "Survey Year Sq."
g t3 = t*t*t
	label var t3 "Survey Year Cu."
*Here they generate a variable that counts the year of survey. 
*After that they generate the square and cube of the year of survey variable.
	
replace rage = rage/100
replace rage2=rage*rage
label var rage "Age"
label var rage2 "Age Sq."
*Here, they convert the age variable into decimal. After that, they replace rage2 with the squarred of rage. 
*They label rage with "Age" and rage2 with "Age Sq."

label var rwork  "Working"
label var rhiltc  "LT Insurance"
*Here, they are labelling rwork with "Working" and rhiltc with "Lt Insurance"

replace rbeqany = rbeqany/100
	label var rbeqany "any beq"
replace rbeq100 = rbeq100/100
	label var rbeq100 "beq 100K"
replace rbeq10k = rbeq10k/100
	label var rbeq10k "beq 10K"
*Here, they are just dividing rbeqany, rbeq10k and rbeq100 by 100. 
*after taht, they labelled each of the variable "any beq", "beq 10K" and "beq 100K" respectively. 

gen Wealth= hatotb*NPV/1000000
	label var Wealth "Wealth in MN"

g htcamt12 = htcamt*NPV
g hfcamt12 = hfcamt*NPV

gen giveany = htcamt12 >0
	replace giveany = . if htcamt12 ==.
g getany = hfcamt12 > 0
	replace getany=. if hfcamt12==.
	
	replace htcamt12 = htcamt12/10000
	replace hfcamt12 = hfcamt12/10000
*Here, they are converting NPV to 2012 dollars.*

label var htcamt12 "T to kid"
label var rbeq10k "Beq 10k"
label var hfcamt12 "T from kid"
label var rbeq100 "Beq 100k"
*Here, they are labelling each variables.*

***Social Security Income
gen SSInc=risret
replace SSInc =sisret if missing(risret ) & !missing(sisret )
replace SSInc =sisret+risret  if !missing(risret ) & !missing(sisret )
replace SSInc=SSInc*NPV
drop if SSInc==. //missing if died or did not respond
drop if SSInc<=100 
gen inc000 = SSInc/10000
label var inc000 "SS Income"

*care hours converted to monthly
replace rkdcarehr=rkdcarehr/24
*Here, they are converting hours of help from children to their parents in hours by month.

g anycaregive = .
replace anycaregive = 0 if rkdcarehr == 0
replace anycaregive = 1 if rkdcarehr >0
replace anycaregive = . if rkdcarehr == .
*they create a new dummy variable that states whether parents help in the care of their grandchildren based on the hours  of childcare variable rkdcarehr.
*They left a missing value when rkdcarehr variable had a missing value.*

g anycareget = .
replace anycareget = 0 if rhlphrs == 0
replace anycareget = 0 if rhlpdays == 0
replace anycareget = 1 if rhlphrs > 0
replace anycareget = 1 if rhlpdays > 0 
replace anycareget = . if rhlpdays == .
replace anycareget = . if rhlphrs == .
*they create a new dummy variable that states whether parents received help in term of hours or days from their children based on the rhlphrs and rhlpdays variable. 
*They left a missing value when rhlpdays and rhlphrs variables had a missing value.*

}
***

*** Drop Missing Values 
{
drop if couple==.
drop if hchild==.
drop if female == .
drop if black==.
drop if hispanic==.
drop if rabplace==.
drop if htcamt12 ==.
drop if hchild==.
}
*Here, they are removing every individual with missing value from the dataset




















**************************BEGINNING ASSIGNEMENT*********************************
*Generating conditional variables for money transfers*
*The authors define monetary transfers made or received as being transfers totalling more than 500 USD since the last survey. Therefore we generate the following variables to consider only monetary transfers above 500 USD.*
gen htcamt_cond = htcamt if htcamt > 500
gen hfcamt_cond = hfcamt if hfcamt > 500


*Labelling*
label variable SSInc "Social Security income"
label variable rkdcarehr "Hours care given"
label variable rhlphrs "Hours help received"
label variable rhlpdays "Days help received"
label variable anycaregive "Any care given?"
label variable anycareget "Any help received?"
label variable giveany "Any transfer made?"
label variable getany "Any transfer received?"
label variable inc000 "Social Security income"
label variable htcamt "Amount transfers made"
label variable htcamt_cond "Amount transfers made conditional"
label variable rbeqany "Any bequest planned?"
label variable rbeq10k "Bequest > 10K planned?"
label variable rbeq100 "Bequest > 100K planned?"
label variable hfcamt "Amount transfers received"
label variable hfcamt_cond "Amount transfers received conditional"






*****************************DESCRIPTIVE STATISTICS REPLICATION*****************
*Tables*

**Sample description**
**We replicate the descriptive statistics used by the authors in the text**

***Table 1: Sample description, continuous control variables***
tabstat rage raedyrs hchild, statistics(mean)

***Tables 2-8: Sample description, dummy and categorical control variables***
tabulate rwork 
tabulate rmstat
tabulate ragender
tabulate raracem
tabulate rahispan
tabulate rhiltc

**Variables of interest**

***Table 9: Monetary transfers***
eststo clear
global tables "C:\Users\beyogr23\Downloads\Replication\table"
eststo: estpost sum  SSInc giveany hfcamt hfcamt_cond getany htcamt htcamt_cond

**Table 10: Time transfers**
eststo clear
global tables "C:\Users\beyogr23\Downloads\Replication\table"
eststo: estpost sum  SSInc anycaregive rkdcarehr anycareget rhlphrs rhlpdays

***Table 11: Planned bequests***
eststo clear
global tables "C:\Users\beyogr23\Downloads\Replication\table"
eststo: estpost sum  SSInc rbeqany rbeq10k rbeq100

***Table 12: Monetary transfers - Descriptive statistics
est clear
estpost tabstat SSInc giveany htcamt htcamt_cond getany hfcamt hfcamt_cond rbeqany rbeq10k rbeq100, col(statistics) stats(mean) 
esttab using $tables\moneydesc.rtf, label replace title(Monetary transfers - Descriptive statistics) b(%9.3f) se(%9.3f) cells(mean(fmt(2))) unstack

***Table 13: Time transfers - Descriptive statistics
est clear
estpost tabstat anycaregive rkdcarehr anycareget rhlphrs rhlpdays, col(statistics) stats(mean)
esttab using $tables\timedesc.rtf, label replace title(Time transfers - Descriptive statistics) b(%9.3f) se(%9.3f) cells(mean(fmt(2)))  unstack





*****************************TABLE REPLICATION

*Regression Table 1*
quietly global control rlowed rage rage2 female nevermarried divorcedorseperated widowed hchild black other hispanic rwork rhiltc Wealth i.rabplace t t2 t3
quietly eststo clear

*column (1)
quietly probit giveany inc000 $control, cluster(hhid)
quietly eststo:margins, dydx(inc000) post

*column (2)
quietly tobit htcamt12 inc000 $control, ll vce(cluster hhid)
quietly eststo:margins, dydx(inc000) post

*column (3)
quietly reg rbeq10k inc000 $control, cluster(hhid)
quietly eststo

*column (4)
quietly reg rbeq100 inc000 $control, cluster(hhid)
quietly eststo

*column (5)
quietly probit getany inc000 $control, cluster(hhid)
quietly eststo:margins, dydx(inc000) post

*column (6)
quietly tobit hfcamt12 inc000 $control, ll vce(cluster hhid)
quietly eststo:margins, dydx(inc000) post

esttab using $tables\table1.rtf, b(%9.3f) se(%9.3f) label replace title(Table 1—: Impact of Social Security Benefits on Monetary Transfers) note(Notes: This table shows the coefficient on Social Security Benefits for separate regressions in each column. All dollar amounts shown are in $10,000. Covariates not shown are described in Section II. The dependent variable in columns (3) and (4) is a number between 0 and 1. Columns (1) and (5) report mean marginal effects from probit models, columns (3) and (4) report OLS estimates, and columns (2) and (6) report mean marginal effects from tobit models. Robust standard errors are clustered at the household level and shown in parentheses.) mlabels("Any transfer?" "Amount of transfer" "Plan Bequest > 10K ?" "Plan Bequest > 100K ?" "Any transfer?" "Amount of transfer") mgroups("Parent-to-child" "Child-to-parent", pattern(1 0 0 0 1 0)) varwidth(30) keep(inc000)

*Regression Table 2*
eststo clear

*column (1)
quietly probit anycaregive inc000 $control, cluster(hhid)
quietly eststo:margins, dydx(inc000) post

*column (2)
quietly tobit rkdcarehr inc000 $control, ll vce(cluster hhid)
quietly eststo:margins, dydx(inc000) post

*column (3)
quietly probit anycareget inc000 $control, cluster(hhid)
quietly eststo:margins, dydx(inc000) post

*column (4)
quietly tobit rhlpdays inc000 $control , ll vce(cluster hhid)
quietly eststo:margins, dydx(inc000) post

*column (5)
quietly tobit rhlphrs inc000 $control, ll vce(cluster hhid)
quietly eststo:margins, dydx(inc000) post

esttab using $tables\table2.rtf, b(%9.3f) se(%9.3f) label replace title(Table 2—: Impact of Social Security Benefits on Time Transfers) note(Notes: This table shows the coefficient on Social Security Benefits for separate regressions in each column. All dollar amounts shown are in $10,000. Covariates not shown are described in Section II. Columns (1) and (3) report mean marginal effects from probit models and columns (2), (4), and (5) report mean marginal effects from tobit models. Robust standard errors are clustered at the household level and shown in parentheses.) mlabels("Any childcare?" "Hours of childcare" "Any help?" "Days of help" "Hours of help") mgroups("Parent-to-child" "Child-to-parent", pattern(1 0 1 0 0)) varwidth(30) keep(inc000)





*****************************ROBUSTNESS : PROBIT

*1. the goodness of fit for probit : percent correctly predicted*

*here by doing the sum of getany (ie the frequence of 1) we realized that 1s only make for 0,6% and 31% of the distribution so we thought of changing treshold 
sum giveany
sum getany

*So instead of 0,5 we opted for 0,06 and 0,31 hoping for a better fit 
*threshold of 0.5
eststo clear
qui probit giveany inc000 $control, cluster (hhid)
estat class, cutoff(0.5)
eststo clear

*threshold of 0.31
qui probit giveany inc000 $control, cluster (hhid)
estat class, cutoff(0.31)
eststo clear

*threshold of 0.5
qui probit getany inc000 $control, cluster (hhid)
estat class, cutoff(0.5)
eststo clear 

*threshold of 0.06
qui probit getany inc000 $control, cluster (hhid)
estat class, cutoff(0.06)
eststo clear


*here the issue is that by lowering the threshold we increase the sensivity but also decrease the specificity. In other words now the model is more sensible to the rare value but also count more 
*false positivy which causes a loss of precision in the model which is why in both cases the correctly predicted % is lower



*2. the probability linear model*
reg anycaregive inc000 $control, cluster(hhid)

reg getany inc000 $control, cluster(hhid)

*Here, by comparing the result of the plm and the ones when using the marginal effect of probit we see that we have almost same result




*****************************ROBUTNESS : OLS
*column (3)
reg rbeq10k inc000 $control, cluster(hhid)
di e(r2)

*24% of the variance of the probability of bequeasting more than 10,000$ is explained by our model.*

*column (4)
reg rbeq100 inc000 $control, cluster(hhid)
di e(r2)

*28.4% of the variance of the probability of bequeasting more than 100,000$ is explained by our model.*




*****************************ROBUTNESS : TOBIT
*1. Two-part model : we see here that for each of our dependent variables there is an important share of parents and childen (i.e ppl who don't transfers anything to their children / parent)
*that is why we think the 2 part model may be a best fit 

*We see that our dependent variables have a large share of zero values hense the use of the two-part model as robustness check

histogram htcamt12
histogram hfcamt12
histogram rkdcarehr
histogram rhlphrs
histogram rhlpdays



*Here we are doing the two-part model: amount of monetary transfers from parent to children
quietly twopm htcamt12 inc000 $control, firstpart(probit) secondpart(regress) vce(cluster hhid) select(hurdle) model(hurdle) family(lognormal) drop(firstpart)
margins, dydx(inc000) atmeans

*Amount of monetary transfers from children to parent
quietly twopm hfcamt12 inc000 $control, firstpart(probit) secondpart(regress) vce(cluster hhid) select(hurdle) model(hurdle) family(lognormal) drop(firstpart)
margins, dydx(inc000) atmeans


*Amount of time transfers from parent to children (childcare)
quietly twopm rkdcarehr inc000 $control, firstpart(probit) secondpart(regress) vce(cluster hhid) select(hurdle) model(hurdle) family(lognormal) drop(firstpart)
margins, dydx(inc000) atmeans

*Amount of time transfers from children to parent (hours)
quietly twopm rhlphrs inc000 $control, firstpart(probit) secondpart(regress) vce(cluster hhid) select(hurdle) model(hurdle) family(lognormal) drop(firstpart)
margins, dydx(inc000) atmeans


*Amount of time transfers from children to parent (days)
quietly twopm rhlpdays inc000 $control, firstpart(probit) secondpart(regress) vce(cluster hhid) select(hurdle) model(hurdle) family(lognormal) drop(firstpart)
margins, dydx(inc000) atmeans



*Interpretation : two part model - when using the two part models we see that the coefficient is smaller. This can be explained by the fact that when using the Tobit model the efcct of the an additionel can be over-represenrted due to high number of 0. Indeed, if we assume that parents that do make monetary tra,nsfers to children do so due to insufficient income, then the impact of social security benefits is going to be more imporatnt for that part of the pop. However, when we look only at the pop of parants that are already giving the effect of SSInc can only be smaller. They already have the means to transfer to their children so the increase of revenue from SSInc is going to be more marginal*



********************NEW TABLES BASED ON OUR ROBUSTNESS SECTION
*Table 1bis*
eststo clear

*column (1) PLM model
quietly reg giveany inc000 $control, cluster(hhid)
eststo

*column (2) Twopart model
quietly twopm htcamt12 inc000 $control, firstpart(probit) secondpart(regress) vce(cluster hhid) select(hurdle) model(hurdle) family(lognormal) drop(firstpart)
eststo: margins, dydx(inc000) atmeans

*column (3) PLM model
quietly reg getany inc000 $control, cluster(hhid)
eststo

*column (4) Twopart model
quietly twopm hfcamt12 inc000 $control, firstpart(probit) secondpart(regress) vce(cluster hhid) select(hurdle) model(hurdle) family(lognormal) drop(firstpart)
eststo: margins, dydx(inc000) atmeans

esttab using $tables\table1bis.rtf, b(%9.3f) se(%9.3f) label replace title(Table 1bis—: Impact of Social Security Benefits on Monetary Transfers) note(Notes: This table shows the coefficient on Social Security Benefits for separate regressions in each column. All dollar amounts shown are in $10,000. Columns (1) and (3) report the coefficents from the probability linear models,  and columns (2) and (4) report mean marginal effects from the two part models.) mlabels("Any transfer?" "Amount of transfer" "Any transfer?" "Amount of transfer") mgroups("Parent-to-child" "Child-to-parent", pattern(1 0 1 0)) varwidth(15) keep(inc000)


*Table 2bis*
eststo clear

*column (1) PLM model
quietly reg anycaregive inc000 $control, cluster(hhid)
eststo

*column (2) Twopart model
quietly twopm rkdcarehr inc000 $control, firstpart(probit) secondpart(regress) vce(cluster hhid) select(hurdle) model(hurdle) family(lognormal) drop(firstpart)
eststo: margins, dydx(inc000) atmeans

*column (3) PLM model
quietly reg anycareget inc000 $control, cluster(hhid)
eststo

*column (4) Twopart model
quietly twopm rhlpdays inc000 $control, firstpart(probit) secondpart(regress) vce(cluster hhid) select(hurdle) model(hurdle) family(lognormal) drop(firstpart)
eststo: margins, dydx(inc000) atmeans

*column (5) Twopart model
quietly twopm rhlphrs inc000 $control, firstpart(probit) secondpart(regress) vce(cluster hhid) select(hurdle) model(hurdle) family(lognormal) drop(firstpart)
eststo: margins, dydx(inc000) atmeans

esttab using $tables\table2bis.rtf , b(%9.3f) se(%9.3f) label replace title(Table 2bis—: Impact of Social Security Benefits on Time Transfers) note(Notes: This table shows the coefficient on Social Security Benefits for separate regressions in each column. All dollar amounts shown are in $10,000. Columns (1) and (3) report coefficent from the probility linear models and columns (2), (4), and (5) report mean marginal effects from the two part models.) mlabels("Any childcare?" "Hours of childcare" "Any help?" "Days of help" "Hours of help") mgroups("Parent-to-child" "Child-to-parent", pattern(1 0 1 0 0)) varwidth(15) keep(inc000) 






****************************GRAPHICS AND EXTENSION******************************
*We define Social Security Benefits quintiles*
xtile SSInc_5= SSInc, nq(5)
label variable SSInc_5 "Social Security Benefit Quintiles"


***************************Graphics replication
*We replicate the graphs included in the text*
preserve
collapse (mean) SSInc giveany hfcamt hfcamt_cond getany htcamt htcamt_cond anycaregive rkdcarehr anycareget rhlphrs rbeqany rbeq10k rbeq100 ragender rage raracem rahispan rmstat rhiltc rwork hchild raedyrs couple Year, by(SSInc_5)

*Graph 1: Probability of money transfers according to social security benefits*
twoway connected giveany getany SSInc_5, legend(off) lcolor(blue black) mcolor(blue black) msymbol(S O) ytitle("Probability of Monetary Transfer") text(0.27 1.5 "Parent-to-child") text(0.12 1.5 "Child-to-parent") title("{it:{stSerif:Panel A: Probability of Money Transfer}}", position(6) color(black))
graph export "$tables\graph1.pdf", replace


*Graph 2: Hours care and help according to social security benefits*
twoway connected rkdcarehr rhlphrs SSInc_5, legend(off) lcolor(blue black) mcolor(blue black) msymbol(S O) ylabel(0 (5) 25) ytitle("Hours Helped (Monthly)") text(22 1.7 "Child-to-parent") text(8 1.7 "Parent-to-child") title("{it:{stSerif:Panel B: Hours of Help Transferred}}", position(6) color(black))
graph export "$tables\graph2.pdf", replace

restore




*********************************Extension

*HOW DO OUR DEPENDENT VARIABLES EVOLVE YEARLY OVER THE SPAN OF THE PANEL STUDY?*
**We collapse the data by year**
preserve
collapse (mean) SSInc giveany hfcamt hfcamt_cond getany htcamt htcamt_cond anycaregive rkdcarehr anycareget rhlphrs rbeqany rbeq10k rbeq100 ragender rage raracem rahispan rmstat rhiltc rwork hchild raedyrs couple, by(Year)

**Graph 3: Social Security Benefits by year**
twoway connected SSInc hfcamt htcamt Year, title("Social Security Benefits") subtitle("by year") ytitle("Social Security Benefits in USD") legend(label(1 "Social security benefits") label(2 "Money given") label(3 "Money received")) 
graph export "$tables\graph8.pdf", replace
restore


*HOW DOES THE AMOUNT OF MONEY TRANSFERS VARY ACROSS GENDER?*
**This allows us to observe whether intergenerational monetary transfers are gender biased** 

**Graph 4: Amount of transfers by gender**
graph box hfcamt_cond htcamt_cond, over(ragender) title("Money transfers") subtitle("by gender") ytitle("USD") noout legend(label(1 "Money given") label(2 "Money received"))
graph export "$tables\graph9.pdf", replace


*HOW DO TIME TRANSFERS VARY ACROSS MARITAL STATUS?* 
**This allows us to observe how hours of care given and received vary for coupled and single people, across each social security benefit quintile*
preserve
collapse (mean) SSInc giveany hfcamt12 hfcamt_cond getany htcamt12 htcamt_cond anycaregive rkdcarehr anycareget rhlphrs rbeqany rbeq10k rbeq100 rage ragender rmstat raracem rahispan rhiltc rwork hchild raedyrs, by(couple SSInc_5)

**Graph 5: Care given by marital status**
graph bar (mean) rkdcarehr, over(couple, axis(off) sort(1)) over(SSInc_5) asyvars title("Hours care given") subtitle("by marital status" "for each Social Security Income Quintile") ytitle("Monthly hours since last survey") legend(label(1 "Not partnered") label(2 "Partnered"))
graph export "$tables\graph10.pdf", replace

**Graph 6: Help received by marital status**
graph bar (mean) rhlphrs, over(couple, axis(off) sort(1)) over(SSInc_5) asyvars title("Hours help received") subtitle("by marital status" "for each Social Security Income Quintile") ytitle("Monthly hours since last survey") legend(label(1 "Not partnered") label(2 "Partnered"))
graph export "$tables\graph11.pdf", replace

restore









