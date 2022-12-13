	
program define densdecomp 
	
		* Author: Martim Leitao, Northwestern University, Kellogg School of Management (KSM)
		* Date: 12/13/2022
		
		version 17.0 
		syntax varlist(min=4 max=4) [ ,BEGIN(real 4) ///
									   END(real 4) ///
									   FIRMbins(real 3) ///
									   DISPLAYbins(real 3) ///
									   WORKERbins(real 3)]
										
		*set trace on
									   
		tokenize `varlist'
		
		di as blue "Worker ID Chosen: `1' " 
		di as blue "Firm ID Chosen: `2' "
		di as blue "Year Variable Chosen: `3' "
		di as blue "Wage metric chosen: `4' "
		
		// Select years you want

		keep if `3' == `begin' | `3' == `end'
		
		// Create mean firm wage
		
		bys `2' `3': gegen mean_f = mean(`4')	
		
		// Create bins on the basis of firm average earnings 
	
		foreach t of numlist `begin' `end' {	
			gquantiles quant_`t' = mean_f if `3'==`t', xtile nq(`firmbins')
		}
		
		egen fquant = rowmax(quant*)
		drop quant*
		
		// Create bins inside firm-based bins
	
		forvalues j=1(1)`firmbins' {
				gquantiles quant_`j' = log_wage if `3'==`begin' & fquant==`j', xtile nq(`workerbins')
		}
		
		egen wquant_begin = rowmax(quant*)
		drop quant*
			
		forvalues j=1(1)`firmbins' {
				gquantiles quant_`j' = log_wage if `3'==`end' & fquant==`j', xtile nq(`workerbins')
		}
		
		egen wquant_end = rowmax(quant*)
		egen wquant = rowmax(wquant_begin wquant_end)
		drop quant* wquant_begin wquant_end
		
		// Create mean earnings in i-j bin
	
		bys `3' wquant fquant: gegen double mean_i_j = mean(`4')
	
		// Create mean earning in j bin
	
		bys `3' fquant: gegen double mean_j = mean(`4')
	
		// Generate deviation from firm bin average
	
		gen double deviation_i_j = mean_i_j - mean_j	
		
		// Sort and reduce data

		sort `3' wquant fquant
		duplicates drop `3' wquant fquant, force
		keep `3' deviation_i_j mean_i_j mean_j wquant fquant
		
		// 1. Produce simluation for actual data
	
		preserve 
			// Adjust and create bins
			sort fquant `3' wquant
			gen double true_ij = deviation_i_j + mean_j
			gquantiles quant_true_begin = true_ij if `3'==`begin', xtile nq(`displaybins')
			gquantiles quant_true_end = true_ij if `3'==`end', xtile nq(`displaybins')
			egen quant = rowmax(quant_true_begin quant_true_end)
			
			// Collapse 
			gcollapse (mean) true_ij, by(quant `3')
			bys quant (`3'): gen dif_true = true_ij-true_ij[_n-1]
			drop if dif_true==.
			save "true_aux", replace
			tw connected dif_true quant, scheme(sj) title("Counterfactual Earnings Change")
			
		restore
		
		// 2. What would happen if Between Firm Inequality had remained constant (only within)
	
		preserve 
			// Create between component constant
			gen aux = mean_j if `3'==`begin'
			bys fquant: gegen aux_mean = mean(aux)
			drop aux
			
			// Create metric
			gen double no_between = deviation_i_j + aux_mean
			gquantiles quant_no_between_begin = no_between if `3'==`begin', xtile nq(`displaybins')
			gquantiles quant_no_between_end = no_between if `3'==`end', xtile nq(`displaybins')
			egen quant = rowmax(quant_no_between_begin quant_no_between_end)
			
			// Collapse 
			gcollapse (mean) no_between, by(quant `3')
			bys quant (`3'): gen dif_no_between = no_between-no_between[_n-1]
			drop if dif_no_between==.
			save "no_between_aux", replace
			tw connected dif_no_between quant, scheme(sj) title("Counterfactual Earnings Change, no Between Effect")
		restore
		
			
		// 3. What would happen if Within Firm Inequality had remained constant (only within)
	
		preserve 
			// Create between component constant
			gen aux = deviation_i_j if `3'==`begin'
			bys wquant: gegen aux_dev = mean(aux)
			drop aux
			
			// Create metric
			gen double no_within = aux_dev + mean_j
			gquantiles quant_no_within_begin = no_within if `3'==`begin', xtile nq(`displaybins')
			gquantiles quant_no_within_end = no_within if `3'==`end', xtile nq(`displaybins')
			egen quant = rowmax(quant_no_within_begin quant_no_within_end)
			
			// Collapse 
			gcollapse (mean) no_within, by(quant `3')
			bys quant (`3'): gen dif_no_within = no_within-no_within[_n-1]
			drop if dif_no_within==.
			save "no_within_aux", replace
			tw connected dif_no_within quant, scheme(sj) title("Counterfactual Earnings Change, no Within Effect")
		restore		
		
		// 4. Make overall figure
		
			use "true_aux", clear
			merge 1:1 quant using "no_within_aux"
			drop _merge
			merge 1:1 quant using "no_between_aux"
			drop _merge
			erase "true_aux.dta"
			erase "no_within_aux.dta"
			erase "no_between_aux.dta"
	
			// Make the Figure
			
			tw connected dif_true quant, lcolor(dknavy) lwidth(thick) mcolor(white) mlcolor(dknavy) ///
			|| connected dif_no_within quant, lcolor(sandb) lwidth(thick) mcolor(white) mlcolor(sandb) ///
			|| connected dif_no_between quant, xlabel(#10, labsize(small) tposition(inside) angle(horizontal)) ///
			ylabel(#6, grid angle(horizontal) labsize(small)  tposition(inside)) xsize(4) ///
			xtitle("Quantile", size(small)) ysize(3) ///
			ytitle("Non-parametric Log Change", size(small)) yline(0, lcolor(black) lpattern(dash)) legend(rows(3) lcolor(none)) ///
			clegend(region(lcolor(none))) zscale(lcolor(none)) plegend(region(lcolor(none))) scheme(s1color) ///
			graphregion(lcolor(none) ifcolor(white)) legend(size(small)) graphregion(fcolor(white) lcolor(none) ifcolor(white)) ///
			lcolor(cranberry) lwidth(thick) mcolor(white) mlcolor(cranberry) ///
			legend(order(1 "Individual Log Earnings Change" 2 "Counterfactual: No Within-Firm Effect" 3 "Counterfactual: No Between-Firm Effect"))

end 
	

	