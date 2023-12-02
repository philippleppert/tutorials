use Hdma.dta, clear

reg deny dir black

reg deny dir black, vce(robust)

probit deny_num dir black

logit deny_num dir black

logit deny_num dir i.ccs

margins ccs, at(dir = 0.3308136)

marginsplot

logit deny_num dir i.black

margins, dydx(_all)

margins, dydx(_all) atmeans

gen ccs_mod = .
replace ccs_mod = 1 if ccs == 1
replace ccs_mod = 2 if inlist(ccs, 2,3)
replace ccs_mod = 3 if inlist(ccs, 4,5,6) 

ologit ccs_mod pbcr black

gen status = ""
replace status = "Not single & not self employed" if  single == 1 & self == 1 
replace status = "Single & not self employed" if single == 2 & self == 1
replace status = "Not single & self employed" if single == 1 & self == 2 
replace status = "Single & self employed" if single == 2 & self == 2 

tab status

encode status, gen(n_status)

tab n_status

mlogit n_status pbcr dir, base(1)

margins, at(dir = (0(0.5)1.5)) predict(outcome(4)) over(pbcr)
marginsplot

* Diagnostics
logit deny_num lvr black pbcr

predict db, dbeta
