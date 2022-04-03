cd "D:\Documents\MA Economics\Courses - Winter 2022\ECON6645 - Applied Econometrics\Assignment 5"

//1
clear
use ".\SLID2.dta"

gen ln_income = log(income)
gen experience2 = experience^2
gen age2 = age^2

est clear
eststo: reg ln_income educ_level experience experience2 age age2 marital ib3.region vm female imm, vce(cluster id)
esttab using tab1.tex, replace


//2
bysort id (wave): keep if _N==2

xtset id wave
*isid id wave
est clear
eststo: xtreg ln_income yrs_schooling experience experience2 age age2 i.marital ib3.region vm female imm, fe
esttab using tab2.tex, replace



//testing
xtset id wave
xtreg ln_income yrs_schooling, fe

foreach var in ln_income yrs_schooling{
	bys id wave: egen miw`var'=mean(`var')
	*bys wave: egen mwa`var'=mean(`var')
	*	gen md`var'=`var' -mid`var'-mwa`var'
}

reg mdln_income mdyrs_schooling

sort id