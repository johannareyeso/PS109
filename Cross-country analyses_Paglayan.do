/* This .do file provides the code that produces Figures 1 and 2 of the main text and Figures A3-A6 and Tables A2-A3 of the Online Supplementary Materials of:

Paglayan, Agustina S. 2021. "From Rebellion to Indoctrination: The Violent Origins of Primary Education Systems." American Political Science Review. 

Before executing the code, download the following file in your selected path/ directory:
Civil War and Primary Enrollment_firstwar.dta

This code was generated with Stata version 16.

*/

clear all
set more off, perm
global path "C:\Users\pagla\Dropbox\Stanford\Dissertation\Civil War, State Consolidation, and the Spread of Mass Education\Papers\Submission\APSR\APSR Replication\"  	// PATH TO THE DATASETS
cd "${path}"
global graphs "C:\Users\pagla\Dropbox\Stanford\Dissertation\Civil War, State Consolidation, and the Spread of Mass Education\Papers\Submission\APSR\APSR Figures and Tables\"  	// WHERE TO SAVE GRAPHS

* FIGURE 1: Average Primary School Enrollment Rate 10 Years Before the Outbreak and 20 Years After the End of Civil War, Europe and Latin America 1828-2015
*__________________________________________________________________________________________________________________________________________________________

use "${path}Civil War and Primary Enrollment_firstwar_APSR.dta", clear

gen everCW=(begin_yr!=.)
gen yeargphcent=.
replace yeargphcent=year-begin_yr if year<begin_yr & everCW!=0
replace yeargphcent=year-end_yr if year>end_yr & everCW!=0

	drop if yeargphcent<-10
	drop if yeargphcent>20 & yeargphcent!=.

gen treatment=(yeargphcent>0 & yeargphcent<=20)    

bysort year: egen controlmean=mean(primratio) if treatment==0  // control group's mean primratio in each year 
bysort year: egen controlmean2=mean(controlmean)				// assigns control group mean values to each country in every year

* PANEL A: All civil wars

preserve
collapse (mean) primratio controlmean2 , by(yeargphcent)
graph twoway (line primratio yeargphcent if yeargphcent<0, lcolor(black) lpattern(solid) lwidth(thick)) ///
(line controlmean2 yeargphcent if yeargphcent<0, lcolor(gs10) lpattern(solid) lwidth(thick)) ///
(line primratio yeargphcent if yeargphcent>0, lcolor(black) lpattern(solid) lwidth(thick)) ///
(line controlmean2 yeargphcent if yeargphcent>0, lcolor(gs10) lpattern(solid) lwidth(thick)) ///
(lfit primratio yeargphcent if yeargphcent<0, lcolor(black) lpattern(dash) lwidth(thin)) ///
(lfit controlmean2 yeargphcent if yeargphcent<0, lcolor(gs8) lpattern(dash) lwidth(thin)) ///
(lfit primratio yeargphcent if yeargphcent>0, lcolor(black) lpattern(dash) lwidth(thin)) ///
(lfit controlmean2 yeargphcent if yeargphcent>0, lcolor(gs8) lpattern(dash) lwidth(thin)) if yeargphcent<=20 & yeargphcent>=-10 , ///
legend(order(1 2) label(1 "Treated") label(2 "Control")) /// 
xtitle("Years from Civil War") ///
ytitle("Primary School Enrollment Rate") ///
xlin(0, lpattern(dash) lwidth(thin)) ///
scheme(s1manual) name(Figure1A, replace)
graph export "${graphs}Fig1,PanelA.pdf", replace
restore

* PANEL B: Civil wars that occurred under non-democratic regimes 

preserve
drop if CWtransition==1 	// drops countries where civil war overlapped with a regime transition, leaving only those countries where civil war occurred in a context of stable non-democracy (note: there are no countries in the sample where civil war occurred in a context of stable democracy)
collapse (mean) primratio controlmean2 , by(yeargphcent)
graph twoway (line primratio yeargphcent if yeargphcent<0, lcolor(black) lpattern(solid) lwidth(thick)) ///
(line controlmean2 yeargphcent if yeargphcent<0, lcolor(gs10) lpattern(solid) lwidth(thick)) ///
(line primratio yeargphcent if yeargphcent>0, lcolor(black) lpattern(solid) lwidth(thick)) ///
(line controlmean2 yeargphcent if yeargphcent>0, lcolor(gs10) lpattern(solid) lwidth(thick)) ///
(lfit primratio yeargphcent if yeargphcent<0, lcolor(black) lpattern(dash) lwidth(thin)) ///
(lfit controlmean2 yeargphcent if yeargphcent<0, lcolor(gs8) lpattern(dash) lwidth(thin)) ///
(lfit primratio yeargphcent if yeargphcent>0, lcolor(black) lpattern(dash) lwidth(thin)) ///
(lfit controlmean2 yeargphcent if yeargphcent>0, lcolor(gs8) lpattern(dash) lwidth(thin)) if yeargphcent<=20 & yeargphcent>=-10 , ///
legend(order(1 2) label(1 "Treated") label(2 "Control")) /// 
xtitle("Years from Civil War") ///
ytitle("Primary School Enrollment Rate") ///
xlin(0, lpattern(dash) lwidth(thin)) ///
scheme(s1manual) name(Figure1B, replace)
graph export "${graphs}Fig1,PanelB.pdf", replace
restore

* FIGURE 2 and FIGURE A2: Event Study Estimate of the Impact of Civil War on Primary School Enrollment Rates 
*___________________________________________________________________________________________________________

* PANEL A: All civil wars

use "${path}Civil War and Primary Enrollment_firstwar_APSR.dta", clear

gen everCW=(begin_yr!=.)

gen yeargphcent=.
replace yeargphcent=year-begin_yr
* Alternative definition of the treatment for FIGURE A2: 
* replace yeargphcent=year-end_yr			// unblind this line to generate Panel A of FIGURE A2, where the treatment is the end of the civil war instead of the onset

	drop if yeargphcent<-10
	drop if yeargphcent>20 & yeargphcent!=.

replace yeargphcent = year if everCW==0

gen treatment=(yeargphcent>0 & yeargphcent<=20)    

tsset Countryid year
estimates clear
xtreg primratio treatment i.year i.Countryid , vce(cluster Countryid)

	forvalues n = 1/10 {
		gen pre`n' = (yeargphcent==-`n')
	}
	forvalues n = 0/20 {
		gen post`n' = (yeargphcent==`n')
	}
		
		local vars primratio
		foreach y of local vars {

		tempname memhold2`y'
		tempfile results2`y'
		postfile `memhold2`y'' year pre prese post postse using `results2`y''

		preserve

			xtreg  `y' i.year pre2-pre10 post0-post20,  fe vce(cluster Countryid)
			estimates store Fig2A

			forval n = 2(1)10 {
				post `memhold2`y'' (`n') (_b[pre`n']) (_se[pre`n']) (.) (.)
			}
				
			forval n =0(1)20 {
				post `memhold2`y'' (`n') (.) (.) (_b[post`n']) (_se[post`n']) 
			}	

		postclose `memhold2`y''
		use `results2`y'', clear

		gen yearpre = -1*year if pre !=.
		gen yearpost = year if post !=.

		gen prehi = pre + 1.96*prese
		gen prelo = pre - 1.96*prese

		gen posthi = post + 1.96*postse
		gen postlo = post - 1.96*postse

		twoway (rcap prehi prelo yearpre, lpattern(solid) lcolor(gs8)) ///
		(rcap posthi postlo yearpost, lpattern(solid) lcolor(gs8))    ///
		(scatter pre yearpre, msymbol(o) msize(large) mcolor(black)) ///
		(scatter post yearpost, msymbol(o) msize(large) mcolor(black)) ,  ///
		xtitle("Years since Civil War") ytitle("Effect of Civil War on Primary Enrollment Rates") title(`tit`y'') ///
		yline(0) xlin(0, lcolor(gs8) lpattern(dash)) ///
		ylabel(-5(5)35) ///
		legend(off) scale(0.9) scheme(s1manual) name(Figure2A, replace) 
		graph export "${graphs}Fig2,PanelA.pdf", replace
		restore
	}

* PANEL B: Civil wars that occurred under non-democratic regimes 

use "${path}Civil War and Primary Enrollment_firstwar_APSR.dta", clear

drop if CWtransition==1
gen everCW=(begin_yr!=.)
gen yeargphcent=.
replace yeargphcent=year-begin_yr
* Alternative definition of the treatment for FIGURE A2: 
* replace yeargphcent=year-end_yr			// unblind this line to generate Panel B of FIGURE A2, where the treatment is the end of the civil war instead of the onset

	drop if yeargphcent<-10
	drop if yeargphcent>20 & yeargphcent!=.

replace yeargphcent = year if everCW==0

gen treatment=(yeargphcent>0 & yeargphcent<=20)    

tsset Countryid year
xtreg primratio treatment i.year i.Countryid , vce(cluster Countryid)

	forvalues n = 1/10 {
		gen pre`n' = (yeargphcent==-`n')
	}
	forvalues n = 0/20 {
		gen post`n' = (yeargphcent==`n')
	}
		
		local vars primratio
		foreach y of local vars {

		tempname memhold2`y'
		tempfile results2`y'
		postfile `memhold2`y'' year pre prese post postse using `results2`y''

		preserve

			xtreg  `y' i.year pre2-pre10 post0-post20,  fe vce(cluster Countryid)
			estimates store Fig2B

			forval n = 2(1)10 {
				post `memhold2`y'' (`n') (_b[pre`n']) (_se[pre`n']) (.) (.)
			}
				
			forval n =0(1)20 {
				post `memhold2`y'' (`n') (.) (.) (_b[post`n']) (_se[post`n']) 
			}	

		postclose `memhold2`y''
		use `results2`y'', clear

		gen yearpre = -1*year if pre !=.
		gen yearpost = year if post !=.

		gen prehi = pre + 1.96*prese
		gen prelo = pre - 1.96*prese

		gen posthi = post + 1.96*postse
		gen postlo = post - 1.96*postse

		twoway (rcap prehi prelo yearpre, lpattern(solid) lcolor(gs8)) ///
		(rcap posthi postlo yearpost, lpattern(solid) lcolor(gs8))    ///
		(scatter pre yearpre, msymbol(o) msize(large) mcolor(black)) ///
		(scatter post yearpost, msymbol(o) msize(large) mcolor(black)) ,  ///
		xtitle("Years since Civil War") ytitle("Effect of Civil War on Primary Enrollment Rates") title(`tit`y'') ///
		yline(0) xlin(0, lcolor(gs8) lpattern(dash)) ///
		ylabel(-5(5)20) ///
		legend(off) scale(0.9) scheme(s1manual) name(Figure2B, replace) 
		graph export "${graphs}Fig2,PanelB.pdf", replace
		restore
	}

estout Fig2A Fig2B using "${graphs}Fig2_Table.xls", replace ///
cells(b(star fmt(6)) se(par(`"="("'`")""')fmt(4))) drop(*year) label stats(N r2_a, labels("Observations" "Adj. R-Square")) starlevels(* 0.05 ** 0.01) stardetach

/* Unblind this section and lines 89 and 162 to save the table of results for Figure A2 of the Online Appendix:
estout Fig2A Fig2B using "${graphs}FigA2_Table.xls", replace ///
cells(b(star fmt(6)) se(par(`"="("'`")""')fmt(4))) drop(*year) label stats(N r2_a, labels("Observations" "Adj. R-Square")) starlevels(* 0.05 ** 0.01) stardetach */ 

	
* FIGURE A3: IMPACT OF BANKING CRISES ON PRIMARY SCHOOL ENROLLMENT RATES
*_______________________________________________________________________

* This figure uses Reinhart and Rogoff's data on the timing of banking crises across countries from 1857-2013. For each country in Europe and Latin America, the analysis focuses on the impact on enrollment rates of the first banking crisis for which there is adequate pre- and post-crisis enrollment data needed to estimate the effect of banking crises on primary school enrollment rates. 

use "${path}Civil War and Primary Enrollment_firstwar_APSR.dta", clear

capture drop begin_yr end_yr
gen begin_yr=.		// indicates the year when a banking crisis began, focusing in any given country on the first banking crisis for which there is adequate pre- and post-crisis enrollment data needed to estimate the effect of banking crises on primary school enrollment rates
gen end_yr=.		// indicates the year when the banking crisis ended

replace begin_yr=1931 if WDICountryVWCode=="ARG"			
replace   end_yr=1934 if WDICountryVWCode=="ARG"

replace begin_yr=1873 if WDICountryVWCode=="AUT"			
replace   end_yr=1873 if WDICountryVWCode=="AUT"

replace begin_yr=1931 if WDICountryVWCode=="BEL"			
replace   end_yr=1931 if WDICountryVWCode=="BEL"

replace begin_yr=1990 if WDICountryVWCode=="BRA"			
replace   end_yr=1990 if WDICountryVWCode=="BRA"

replace begin_yr=1926 if WDICountryVWCode=="CHL"			
replace   end_yr=1926 if WDICountryVWCode=="CHL"

replace begin_yr=1991 if WDICountryVWCode=="FIN"			
replace   end_yr=1991 if WDICountryVWCode=="FIN"

replace begin_yr=1907 if WDICountryVWCode=="FRA"			
replace   end_yr=1907 if WDICountryVWCode=="FRA"

replace begin_yr=1866 if WDICountryVWCode=="GBR"			
replace   end_yr=1866  if WDICountryVWCode=="GBR"

replace begin_yr=1891 if WDICountryVWCode=="ITA"			
replace   end_yr=1893 if WDICountryVWCode=="ITA"

replace begin_yr=1981 if WDICountryVWCode=="MEX"			
replace   end_yr=1981 if WDICountryVWCode=="MEX"

replace begin_yr=1939 if WDICountryVWCode=="NLD"			
replace   end_yr=1939 if WDICountryVWCode=="NLD"

replace begin_yr=1923 if WDICountryVWCode=="PRT"			
replace   end_yr=1923 if WDICountryVWCode=="PRT"

replace begin_yr=1931 if WDICountryVWCode=="URY"			
replace   end_yr=1931 if WDICountryVWCode=="URY"

replace begin_yr=1931 if WDICountryVWCode=="CHE"			
replace   end_yr=1933 if WDICountryVWCode=="CHE"

replace begin_yr=1857 if WDICountryVWCode=="DEU"			
replace   end_yr=1857 if WDICountryVWCode=="DEU"

replace begin_yr=1921 if WDICountryVWCode=="DNK"			
replace   end_yr=1921 if WDICountryVWCode=="DNK"

replace begin_yr=1921 if WDICountryVWCode=="NOR"			
replace   end_yr=1921 if WDICountryVWCode=="NOR"

replace begin_yr=1907 if WDICountryVWCode=="SWE"			
replace   end_yr=1907 if WDICountryVWCode=="SWE"

* PANEL A:

gen everBC=(begin_yr!=.)
gen yeargphcent=.
replace yeargphcent=year-begin_yr if year<begin_yr & everBC!=0
replace yeargphcent=year-end_yr if year>=end_yr & everBC!=0

gen treatment=(yeargphcent>=0 & yeargphcent<=20)    
tsset Countryid year
xtreg primratio treatment i.year i.Countryid , vce(cluster Countryid)

bysort year: egen controlmean=mean(primratio) if treatment==0   // control group's mean primratio in each year 
bysort year: egen controlmean2=mean(controlmean)				// assigns control group mean values to each country in every year

preserve
collapse (mean) primratio controlmean2, by(yeargphcent)
graph twoway (line primratio yeargphcent if yeargphcent<0, lcolor(midblack) lpattern(solid) lwidth(thick)) ///
(line controlmean2 yeargphcent if yeargphcent<0, lcolor(gs8) lpattern(solid) lwidth(thick)) ///
(line primratio yeargphcent if yeargphcent>=0, lcolor(midblack) lpattern(solid) lwidth(thick)) ///
(line controlmean2 yeargphcent if yeargphcent>=0, lcolor(gs8) lpattern(solid) lwidth(thick)) ///
(lfit primratio yeargphcent if yeargphcent<0, lcolor(black) lpattern(dash) lwidth(thin)) ///
(lfit controlmean2 yeargphcent if yeargphcent<0, lcolor(black) lpattern(dash) lwidth(thin)) ///
(lfit primratio yeargphcent if yeargphcent>=0, lcolor(black) lpattern(dash) lwidth(thin)) ///
(lfit controlmean2 yeargphcent if yeargphcent>=0, lcolor(black) lpattern(dash) lwidth(thin)) if yeargphcent>=-10 & yeargphcent<=20 , ///
legend(order(1 2) label(1 "Treated") label(2 "Control")) /// 
xtitle("Years since Banking Crisis") ///
ytitle("Primary School Enrollment Rate") ///
xlin(0, lpattern(dash) lwidth(thin)) ///
scheme(s1manual) name(FigA3_PanelA, replace)
restore

* PANEL B:

drop if yeargphcent<-10
drop if yeargphcent>20 & yeargphcent!=.
replace yeargphcent = year if everBC==0

drop treatment 
gen treatment=(yeargphcent>=0 & yeargphcent<=20)    // The 20-year threshold can be changed 

tsset Countryid year
estimates clear
xtreg primratio treatment i.year i.Countryid , vce(cluster Countryid)

	forvalues n = 1/10 {
		gen pre`n' = (yeargphcent==-`n')
	}
	forvalues n = 0/20 {
		gen post`n' = (yeargphcent==`n')
	}
		
		local vars primratio
		foreach y of local vars {

		tempname memhold2`y'
		tempfile results2`y'
		postfile `memhold2`y'' year pre prese post postse using `results2`y''

		preserve

			xtreg  `y' i.year pre2-pre10 post0-post20,  fe vce(cluster Countryid)
			estimates store FigA3B

			forval n = 2(1)10 {
				post `memhold2`y'' (`n') (_b[pre`n']) (_se[pre`n']) (.) (.)
			}
				
			forval n =0(1)20 {
				post `memhold2`y'' (`n') (.) (.) (_b[post`n']) (_se[post`n']) 
			}	

		postclose `memhold2`y''
		use `results2`y'', clear

		gen yearpre = -1*year if pre !=.
		gen yearpost = year if post !=.

		gen prehi = pre + 1.96*prese
		gen prelo = pre - 1.96*prese

		gen posthi = post + 1.96*postse
		gen postlo = post - 1.96*postse

		twoway (rcap prehi prelo yearpre, lpattern(solid) lcolor(gs8)) ///
		(rcap posthi postlo yearpost, lpattern(solid) lcolor(gs8))    ///
		(scatter pre yearpre, msymbol(o) msize(large) mcolor(black)) ///
		(scatter post yearpost, msymbol(o) msize(large) mcolor(black)) ,  ///
		xtitle("Years since Banking Crisis") ytitle("Effect of Banking Crisis on Primary Enrollment Rates") title(`tit`y'') ///
		yline(0) xlin(0, lcolor(gs8) lpattern(dash)) ///
		ylabel(-5(5)20) ///
		legend(off) scale(0.9) scheme(s1manual) name(FigA3_PanelB, replace) 

		restore
	}

estout FigA3B using "${graphs}FigA3_PanelB_Table.xls", replace ///
cells(b(star fmt(6)) se(par(`"="("'`")""')fmt(4))) drop(*year) label stats(N r2_a, labels("Observations" "Adj. R-Square")) starlevels(* 0.05 ** 0.01) stardetach

* FIGURE A5: ROBUSTNESS TO OUTLIERS: EVENT STUDY ESTIMATES OF THE EFFECT OF CIVIL WAR, DROPPING ONE COUNTRY AT A TIME
*_____________________________________________________________________________________________________________________

* PANEL A: All civil wars, excluding one country at a time

use "${path}Civil War and Primary Enrollment_firstwar_APSR.dta", clear

gen everCW=(begin_yr!=.)
gen yeargphcent=.
replace yeargphcent=year-begin_yr if year<begin_yr & everCW!=0
replace yeargphcent=year-end_yr if year>end_yr & everCW!=0
replace yeargphcent=year-begin_yr

	drop if yeargphcent<-10
	drop if yeargphcent>20 & yeargphcent!=.

replace yeargphcent = year if everCW==0

gen treatment=(yeargphcent>0 & yeargphcent<=20)    

tsset Countryid year

	forvalues n = 1/10 {
		gen pre`n' = (yeargphcent==-`n')
	}
	forvalues n = 0/20 {
		gen post`n' = (yeargphcent==`n')
	}
		
estimates clear
levelsof WDICountryVWCode if everCW==1, local(vars)
foreach i of local vars{
preserve
		keep if WDICountryVWCode!="`i'"
		tempname memhold2primratio`i'
		tempfile results2primratio`i'
		postfile `memhold2primratio`i'' year pre prese post postse using `results2primratio`i''

			xtreg  primratio i.year pre2-pre10 post0-post20,  fe vce(cluster Countryid)
			estimates store FigA5A`i'

			forval n = 2(1)10 {
				post `memhold2primratio`i'' (`n') (_b[pre`n']) (_se[pre`n']) (.) (.)
			}
				
			forval n =0(1)20 {
				post `memhold2primratio`i'' (`n') (.) (.) (_b[post`n']) (_se[post`n']) 
			}	

		postclose `memhold2primratio`i''
		use `results2primratio`i'', clear

		gen yearpre = -1*year if pre !=.
		gen yearpost = year if post !=.

		gen prehi = pre + 1.96*prese
		gen prelo = pre - 1.96*prese

		gen posthi = post + 1.96*postse
		gen postlo = post - 1.96*postse

		twoway (rcap prehi prelo yearpre, lpattern(solid) lcolor(gs8)) ///
		(rcap posthi postlo yearpost, lpattern(solid) lcolor(gs8))    ///
		(scatter pre yearpre, msymbol(o) msize(vlarge) mcolor(black)) ///
		(scatter post yearpost, msymbol(o) msize(vlarge) mcolor(black)) ,  ///
		title("Excluding `i'", size(large)) ///
		xtitle("Years since Civil War") ytitle("Effect of Civil War on Enrollment Rates") ///
		yline(0) xlin(0, lcolor(gs8) lpattern(dash)) ///
		ylabel(-5(5)18) ///
		legend(off) scale(0.5) scheme(s1manual) name(gwo`i', replace) nodraw
restore
}

graph combine gwoARG gwoAUT gwoBOL gwoBRA gwoCHL gwoCOL gwoCRI gwoCUB gwoDOM gwoECU gwoESP gwoFIN gwoFRA gwoGRC gwoGTM gwoHND gwoITA gwoMEX gwoNIC gwoPER gwoPRY gwoSLV gwoURY, scheme(s1manual) name(FigA5_PanelA, replace)

estout FigA5A* using "${graphs}FigA5_PanelA_Table.xls", replace ///
cells(b(star fmt(6)) se(par(`"="("'`")""')fmt(4))) drop(*year) label stats(N r2_a, labels("Observations" "Adj. R-Square")) starlevels(* 0.05 ** 0.01) stardetach

* PANEL B: Civil wars that occurred under non-democratic regimes, excluding one country at a time 
	
use "${path}Civil War and Primary Enrollment_firstwar_APSR.dta", clear

drop if CWtransition==1
gen everCW=(begin_yr!=.)
gen yeargphcent=.
replace yeargphcent=year-begin_yr if year<begin_yr & everCW!=0
replace yeargphcent=year-end_yr if year>end_yr & everCW!=0
replace yeargphcent=year-begin_yr

	drop if yeargphcent<-10
	drop if yeargphcent>20 & yeargphcent!=.

replace yeargphcent = year if everCW==0

gen treatment=(yeargphcent>0 & yeargphcent<=20)    

tsset Countryid year

	forvalues n = 1/10 {
		gen pre`n' = (yeargphcent==-`n')
	}
	forvalues n = 0/20 {
		gen post`n' = (yeargphcent==`n')
	}
		
estimates clear
levelsof WDICountryVWCode if everCW==1, local(vars)
foreach i of local vars{
preserve
		keep if WDICountryVWCode!="`i'"
		tempname memhold2primratio`i'
		tempfile results2primratio`i'
		postfile `memhold2primratio`i'' year pre prese post postse using `results2primratio`i''

			xtreg  primratio i.year pre2-pre10 post0-post20,  fe vce(cluster Countryid)
			estimates store FigA5B`i'

			forval n = 2(1)10 {
				post `memhold2primratio`i'' (`n') (_b[pre`n']) (_se[pre`n']) (.) (.)
			}
				
			forval n =0(1)20 {
				post `memhold2primratio`i'' (`n') (.) (.) (_b[post`n']) (_se[post`n']) 
			}	

		postclose `memhold2primratio`i''
		use `results2primratio`i'', clear

		gen yearpre = -1*year if pre !=.
		gen yearpost = year if post !=.

		gen prehi = pre + 1.96*prese
		gen prelo = pre - 1.96*prese

		gen posthi = post + 1.96*postse
		gen postlo = post - 1.96*postse

		twoway (rcap prehi prelo yearpre, lpattern(solid) lcolor(gs8)) ///
		(rcap posthi postlo yearpost, lpattern(solid) lcolor(gs8))    ///
		(scatter pre yearpre, msymbol(o) msize(vlarge) mcolor(black)) ///
		(scatter post yearpost, msymbol(o) msize(vlarge) mcolor(black)) ,  ///
		title("Excluding `i'", size(large)) ///
		xtitle("Years since Civil War") ytitle("Effect of Civil War on Enrollment Rates") ///
		yline(0) xlin(0, lcolor(gs8) lpattern(dash)) ///
		ylabel(-5(5)18) ///
		legend(off) scale(0.5) scheme(s1manual) name(gwo`i', replace) nodraw
restore
}

graph combine gwoARG gwoAUT gwoBOL gwoBRA gwoCHL gwoECU gwoHND gwoMEX gwoPER gwoPRY gwoSLV gwoURY, scheme(s1manual) name(FigA5_PanelB, replace)

estout FigA5B* using "${graphs}FigA5_PanelB_Table.xls", replace ///
cells(b(star fmt(6)) se(par(`"="("'`")""')fmt(4))) drop(*year) label stats(N r2_a, labels("Observations" "Adj. R-Square")) starlevels(* 0.05 ** 0.01) stardetach

* FIGURE A6: INDIVIDUAL COUNTRY TRENDS
*______________________________________

* PANEL B: Countries with non-democratic regime before, during, and after the war

use "${path}Civil War and Primary Enrollment_firstwar_APSR.dta", clear

gen everCW=(begin_yr!=.)
gen yeargphcent=.
replace yeargphcent=year-begin_yr if year<begin_yr & everCW!=0
replace yeargphcent=year-end_yr if year>end_yr & everCW!=0

	drop if yeargphcent<-10
	drop if yeargphcent>20 & yeargphcent!=.

levelsof WDICountryVWCode if CWnondemocratic==1, local(country)
foreach i of local country {
graph twoway (line primratio yeargphcent if yeargphcent>=-10 & yeargphcent<0, lcolor(midblue) lpattern(solid) lwidth(thick)) ///
(line primratio yeargphcent if yeargphcent>=0 & yeargphcent<=20, lcolor(midblue) lpattern(solid) lwidth(thick)) ///
(lfit primratio yeargphcent if yeargphcent<0, lcolor(blue) lpattern(dash) lwidth(thin)) ///
(lfit primratio yeargphcent if yeargphcent>0, lcolor(blue) lpattern(dash) lwidth(thin)) ///
if WDICountryVWCode=="`i'", ///
legend(off) /// 
title("`i'") ///
xtitle("Years from Civil War") ///
ytitle("Primary School Enrollment Rate") ///
xlin(0, lpattern(dash) lwidth(thin)) /// 
scheme(s1manual) scale(0.7) name(g3`i', replace) nodraw
}
graph combine g3ARG g3AUT g3BOL g3BRA g3CHL g3ECU g3HND g3MEX g3PER g3PRY g3SLV g3URY, scheme(s1manual) name(FigA6_PanelB, replace)

* PANEL C: Countries where civil war period of analysis overlapped with regime change

use "${path}Civil War and Primary Enrollment_firstwar_APSR.dta", clear

gen everCW=(begin_yr!=.)
gen yeargphcent=.
replace yeargphcent=year-begin_yr if year<begin_yr & everCW!=0
replace yeargphcent=year-end_yr if year>end_yr & everCW!=0

	drop if yeargphcent<-10
	drop if yeargphcent>20 & yeargphcent!=.

levelsof WDICountryVWCode if CWtransition==1, local(country)
foreach i of local country {
graph twoway (line primratio yeargphcent if yeargphcent>=-10 & yeargphcent<0, lcolor(midblue) lpattern(solid) lwidth(thick)) ///
(line primratio yeargphcent if yeargphcent>=0 & yeargphcent<=20, lcolor(midblue) lpattern(solid) lwidth(thick)) ///
(lfit primratio yeargphcent if yeargphcent<0, lcolor(blue) lpattern(dash) lwidth(thin)) ///
(lfit primratio yeargphcent if yeargphcent>0, lcolor(blue) lpattern(dash) lwidth(thin)) ///
if WDICountryVWCode=="`i'", ///
legend(off) /// 
title("`i'") ///
xtitle("Years from Civil War") ///
ytitle("Primary School Enrollment Rate") ///
xlin(0, lpattern(dash) lwidth(thin)) /// 
scheme(s1manual) scale(0.7) name(g5`i', replace) nodraw
}
graph combine g5COL g5CRI g5CUB g5DOM g5ESP g5FIN g5FRA g5GRC g5GTM g5ITA g5NIC, scheme(s1manual) name(FigA6_PanelC, replace)

* PANEL A: Summary of Panels B and C (individual country slope and intercept shifts):

use "${path}Civil War and Primary Enrollment_firstwar_APSR.dta", clear

gen everCW=(begin_yr!=.)
gen yeargphcent=.
replace yeargphcent=year-begin_yr if year<begin_yr & everCW!=0
replace yeargphcent=year-end_yr if year>end_yr & everCW!=0

	drop if yeargphcent<-10
	drop if yeargphcent>20 & yeargphcent!=.

replace yeargphcent = year if everCW==0
count if yeargphcent==1			

gen treatment=(yeargphcent>0 & yeargphcent<=20)    
sort WDICountryVWCode year 

xtset Countryid year
xtreg primratio yeargphcent treatment c.treatment#c.yeargphcent if everCW==1, fe vce(cluster Countryid)
estimates clear
levelsof WDICountryVWCode if everCW==1, local(cntry)
foreach i of local cntry {
display "`i'"
reg primratio yeargphcent treatment c.treatment#c.yeargphcent if WDICountryVWCode=="`i'"
estimates store e1`i'
}
estout e1ARG e1AUT e1BOL e1BRA e1CHL e1COL e1CRI e1CUB e1DOM e1ECU e1ESP e1FIN e1FRA e1GRC e1GTM e1HND e1ITA e1MEX e1NIC e1PER e1PRY e1SLV e1URY e1VEN using "${graphs}FigA6_Table.xls", replace cells(b(star fmt(6)) se(par(`"="("'`")""')fmt(4)) ci(fmt(4))) label stats(N r2_a, labels("Observations" "Adj. R-Square")) starlevels(* 0.10 ** 0.05 *** 0.01) stardetach


* TABLE A2: HETEREGENEOUS EFFECT OF CIVIL WAR ON PRIMARY SCHOOL ENROLLMENT RATE DEPENDING ON WHETHER LIBERALS WON THE WAR OR NOT
*________________________________________________________________________________________________________________________________

use "${path}Civil War and Primary Enrollment_firstwar_APSR.dta", clear

drop if CWtransition==1
gen liberals=(WDICountryVWCode=="BOL" | WDICountryVWCode=="BRA" | WDICountryVWCode=="CHL" | WDICountryVWCode=="PER" | WDICountryVWCode=="PRY" | WDICountryVWCode=="VEN" )

gen yeargphcent=.
replace yeargphcent=year-begin_yr if begin_yr!=.
	drop if yeargphcent<-10
	drop if yeargphcent>20 & yeargphcent!=.
replace yeargphcent = year if begin_yr==.
gen treatment=(yeargphcent>0 & yeargphcent<=20)    

tsset Countryid year
fvset base 1 year
estimates clear
xtreg primratio treatment i.year, fe vce(cluster Countryid)
estimates store DID
xtreg primratio treatment c.treatment#c.liberals i.year, fe vce(cluster Countryid)
estimates store DID2

estout DID DID2 using "${graphs}Liberals.xls", replace ///
cells(b(star fmt(6)) se(par(`"="("'`")""')fmt(4))) drop(*year) label stats(N r2_a, labels("Observations" "Adj. R-Square")) starlevels(* 0.10 ** 0.05 *** 0.01) stardetach


* TABLE A3: LINEAR EFFECT OF CIVIL WAR ON PRIMARY SCHOOL ENROLLMENT RATE, WITH AND WITHOUT CORRECTION PROPOSED BY GOODMAN-BACON (2021) AND BAKER ET.AL. (2021)
*______________________________________________________________________________________________________________________________________________________

* PANEL A: Linear difference-in-differences pooling all treated units

use "${path}Civil War and Primary Enrollment_firstwar_APSR.dta", clear

gen yeargphcent=.
replace yeargphcent=year-begin_yr if begin_yr!=.
	drop if yeargphcent<-10
	drop if yeargphcent>20 & yeargphcent!=.
replace yeargphcent = year if begin_yr==.
gen treatment=(yeargphcent>0 & yeargphcent<=20)    

tsset Countryid year
fvset base 1 year
estimates clear
xtreg primratio treatment i.year, fe vce(cluster Countryid)

* PANEL B: Linear difference-in-differences estimated separately for each treated cohort

use "${path}Civil War and Primary Enrollment_firstwar_APSR.dta", clear

gen eventyr = year - begin_yr
tostring begin_yr, gen(treatgroup)
levelsof begin_yr, local(treatlist)

foreach k of local treatlist {
preserve
keep if treatgroup == "`k'" | treatgroup == "."
gen group=`k'
tempfile group`k'
save `group`k''
restore
}

u `group1848', clear
foreach l in 1879 1891 1893 1899 1904 1911 1912 1918 1920 1924 1932 1934 1944 1948 1952 1965 1966 1978 {
append using `group`l''
}

gen yeargphcent=.
replace yeargphcent=year-begin_yr if begin_yr!=.
	drop if yeargphcent<-10
	drop if yeargphcent>20 & yeargphcent!=.
replace yeargphcent = year if begin_yr==.
gen treatment=(yeargphcent>0 & yeargphcent<=20)    

reghdfe primratio  treatment, abs(group##Countryid group##year) cluster(Countryid##group)
