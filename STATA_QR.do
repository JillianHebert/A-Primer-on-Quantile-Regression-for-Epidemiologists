/*
	Title:		Quantile Regression STATA Code
	Author:		Aayush Khadka, Jilly Hebert, and Anusha Vable
	Institution: University of California, San Francisco
*/
	
	****************************************************************************
	clear all
	
	** Setting directories and loading data
	
	
	****************************************************************************
	
	** Mean Model
	
	****************************************************************************
	
	** Create OLS results sheet
	putexcel set `saveloc'qr_results, modify sheet("OLS")
	putexcel A1 = "quantile"	
	putexcel B1 = "coef" //Saves school year estimate
	putexcel C1 = "lci"	
	putexcel D1 = "uci"	
	
	
	** Bootstrap 95% CI's
	regress sbp c.schlyrs c.age c.age2 i.gender i.race c.rameduc c.rafeduc //
	i.southern i.year, vce(boostrap, reps(500))
	
	** Other modeling option
	*regress sbp c.schlyrs c.age c.age2 i.female i.black i.latinx //
	*c.rameduc c.rafeduc i.southern i.y08 i.y10 i.y12 i.y14 i.y16 i.y18, //
	*vce(bootstrap, reps(500))
	
	
	** Extracting results
	matrix param = r(table)	
	
	** Storing results
	putexcel A2 = -0.10
	putexcel B2 = param[1,1]
	putexcel C2 = param[5,1]
	putexcel D2 = param[6,1]
	
	
	****************************************************************************
	
	** Conditional Quantile Regression (CQR)
	
	****************************************************************************
	
	** Create CQR results sheet
	putexcel set `saveloc'qr_results.xlsx, modify sheet("CQR")
	putexcel A1 = "quantile"			
	putexcel B1 = "coef" //Saves school year estimate
	putexcel C1 = "lci"	
	putexcel D1 = "uci"	
	
	** Creating a counter
	local c = 2
	
	**Bootstrap 95% CIs
	forval i=0.01(0.01)0.99 {
		
		bsqreg sbp c.schlyrs c.age c.age2 i.gender i.race //
		c.rameduc c.rafeduc i.southern i.year, quantile(`i') reps(500)
		
		** Other modeling option
		*bsqreg sbp c.schlyrs c.age c.age2 i.female i.black i.latinx //
		*c.rameduc c.rafeduc i.southern i.y08 i.y10 i.y12 i.y14 i.y16 i.y18, //
		*quantile(`i') reps(500)
		
		** Extracting results
		matrix param = r(table)
		
		** Storing results
		putexcel A`c' = `i'*100
		putexcel B`c' = param[1,1]
		putexcel C`c' = param[5,1]
		putexcel D`c' = param[6,1]
		
		** Updating counter
		local c = `c' + 1		
		
	}
	
	
	****************************************************************************
	
	** Unconditional Quantile Regression (UQR)
	
	****************************************************************************
	
	** Create CQR results sheet
	putexcel set `saveloc'qr_results.xlsx, modify sheet("UQR")
	putexcel A1 = "quantile"			
	putexcel B1 = "coef" //Saves school year estimate
	putexcel C1 = "lci"	
	putexcel D1 = "uci"	
	
	** Creating a counter
	local c = 2
	
	**Bootstrap 95% CI's
	forval i=1(1)99 {
		
		rifhdreg sbp c.schlyrs c.age c.age2 i.gender i.race c.rameduc //
		c.rafeduc i.southern i.year, rif(q(`i')) vce(bootstrap, reps(500))
		
		** Other modeling option
		*rifhdreg sbp c.schlyrs c.age c.age2 i.female i.black i.latinx //
		*c.rameduc c.rafeduc i.southern i.y08 i.y10 i.y12 i.y14 i.y16 i.y18, //
		*rif(q(`i')) vce(bootstrap, reps(500))
		
		** Extracting results
		matrix param = r(table)
		
		** Storing results
		putexcel A`c' = `i'
		putexcel B`c' = param[1,1]
		putexcel C`c' = param[5,1]
		putexcel D`c' = param[6,1]
		
		** Updating counter
		local c = `c' + 1		
		
	}
	
	
	