* Demonstration Abhaengigkeit Beta von Schiefe
version 12

preserve
set seed 741967

// Wait! I need to calculate 1000 regression-models
quietly {
	
	clear
	set obs 1000
	
	// X1-Variable (skewness constant)
	generate X1 = _n<500
	summarize X1
	local sx1 = r(sd)

	// Errors (standard normal distributed)
	generate error = invnormal(uniform())
	
	// Sort Data at random to reduce multicollinarity
	generate r = runiform()
	sort r
	
	// generate empty X2 and Y 
	generate X2 = .
	generate Y = .
	
	// Postfile
	postfile betas b1 b2 beta1 beta2 skewness using betas, replace
	
	forv i = 5/995 {

		// Generate X2 (change skewness in each round)
		replace X2 = _n<=`i'
		summarize X2, detail
		local sx2 = r(sd)
		local skewness = r(skewness)
				
		// Generate Y with b_1,2 =2
		replace Y = 2*X1 + 2*X2 + error
		summarize Y
		local sy = r(sd)
		
		// calculate Regression
		regress Y X1 X2
		
		// Save Results
		post betas  ///
		  (_b[X1]) (_b[X2])  ///
		  (_b[X1] * `sx1'/`sy') (_b[X2] * `sx2'/`sy')  ///
		  (`skewness')
	}
	postclose betas
	
	use betas, clear
	
	* Graphik-labels
	lab var beta1 "X{subscript:1} (skewness constant)"
	lab var beta2 "X{subscript:2} (skewness varies)"
	
	graph twoway ///
	  || line beta1 beta2 skewness  ///
	  || , ytitle(Standardized Coefficient)  ///
	  title("Standardized coefficients by skewness") ///
	  subtitle("Y = b{subscript:1}X{subscript:1} + b{subscript:2}X{subscript:2} + e with b{subscript:1,2}=2") ///
	  xtitle(Skewness of X{subscript:2})
}

erase betas.dta
restore

exit
