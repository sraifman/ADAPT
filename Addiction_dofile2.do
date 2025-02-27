


 ****************************************************************************************************************** 
 *Title: ADAPT study 
* Purpose: Assess Research Question 1. 
*Is baseline substance use (exposure) associated with baseline DAP and pre-pregnancy DAP (outcomes)?
*******************************************************************************************************************  

  
 // Models USING ALL PRE-PREG MEASURES OF DAP // 
 
 xtset id waveodduncutb
 *levels: id, time, clinic
  
 keep dap_score_ic alc4_monthc audc_impd cannabisc druguse dap_score_ict agecatfbl  race_catR parityR  religiousR schooling  employeddi foodinsecurdi housinginsecurdi mainqualcohab   waveuncutb sample siter id statesite waveodduncutb  newpregevcutb wave  dayssincebl newpregnextcutb tstart tstop tstop_preg tstop_concep
 
 xtdescribe  
	 	
	** Non-imputed unadjusted and adjusted mixed models
	*binge
	xtmixed dap_score_ic i.alc4_monthc if waveuncutb!=. & sample==1 || siter: || id:,   residual(ar 1, t(waveodduncutb)) 
	xtmixed dap_score_ic i.alc4_monthc i.agecatfbl  i.race_catR i.parityR  i.religiousR i.schooling  i.employeddi i.foodinsecurdi i.housinginsecurdi i.mainqualcohab    if waveuncutb!=. & sample==1 || siter: || id:,  residual(ar 1, t(waveodduncutb)) 
	  *audc_impd
	xtmixed dap_score_ic i.audc_impd if waveuncutb!=. & sample==1 || siter: || id:,  residual(ar 1, t(waveodduncutb)) 
	xtmixed dap_score_ic i.audc_impd i.agecatfbl  i.race_catR i.parityR  i.religiousR i.schooling   i.employeddi i.foodinsecurdi i.housinginsecurdi i.mainqualcohab   if waveuncutb!=. & sample==1  || siter: || id:,   residual(ar 1, t(waveodduncutb)) 
	*cannabis
	xtmixed dap_score_ic i.cannabisc if waveuncutb!=. & sample==1  || siter: || id:, residual(ar 1, t(waveodduncutb)) 
	xtmixed dap_score_ic i.cannabisc i.agecatfbl  i.race_catR i.parityR i.religiousR i.schooling   i.employeddi i.foodinsecurdi i.housinginsecurdi i.mainqualcoha if waveuncutb!=. & sample==1   || siter: || id:,  residual(ar 1, t(waveodduncutb)) 
	*druguse
	xtmixed dap_score_ic i.druguse   if waveuncutb!=. & sample==1 || siter: || id:, residual(ar 1, t(waveodduncutb)) 
	xtmixed dap_score_ic i.druguse i.agecatfbl  i.race_catR i.parityR i.religiousR i.schooling   i.employeddi i.foodinsecurdi i.housinginsecurdi i.mainqualcoha  if waveuncutb!=. & sample==1  || siter: || id:,  residual(ar 1, t(waveodduncutb)) 

	
	** Imputed unadjusted and adjusted mixed models 
	
	use "Imputed data RQ1.dta"
	log using RQ1_imputed

	mi xtset id waveodduncutb
	
 //binge drinking 
  mi estimate: xtmixed dap_score_ic i.alc4_monthc if waveuncutb!=. & sample==1 || siter: || id:,   residual(ar 1, t(waveodduncutb)) 
  mi estimate: xtmixed dap_score_ic i.alc4_monthc i.agecatfbl  i.race_catR i.parityR  i.religiousR i.schooling  i.employeddi i.foodinsecurdi i.housinginsecurdi i.mainqualcohab    if waveuncutb!=. & sample==1 || siter: || id:,  residual(ar 1, t(waveodduncutb)) 
  
	    
  //audit-c score
	ta audc_impd if wave==1 & sample==1, m
	mi estimate: xtmixed dap_score_ic i.audc_impd if waveuncutb!=. & sample==1 || siter: || id:,   residual(ar 1, t(waveodduncutb)) 
	mi estimate: xtmixed dap_score_ic i.audc_impd i.agecatfbl  i.race_catR i.parityR  i.religiousR i.schooling   i.employeddi i.foodinsecurdi i.housinginsecurdi i.mainqualcohab   if waveuncutb!=. & sample==1  || siter: || id:,    residual(ar 1, t(waveodduncutb)) 

		
  //cannabis
  	ta cannabisc if wave==1 & sample==1, m
	   mi estimate: xtmixed dap_score_ic i.cannabisc if waveuncutb!=. & sample==1  || siter: || id:,  residual(ar 1, t(waveodduncutb)) 
       mi estimate: xtmixed dap_score_ic i.cannabisc i.agecatfbl  i.race_catR i.parityR i.religiousR i.schooling   i.employeddi i.foodinsecurdi i.housinginsecurdi i.mainqualcohab if waveuncutb!=. & sample==1   || siter: || id:,   residual(ar 1, t(waveodduncutb)) 

	//drug use 

	mi estimate: xtmixed dap_score_ic i.druguse   if waveuncutb!=. & sample==1 || siter: || id:,   residual(ar 1, t(waveodduncutb)) 
	mi estimate: xtmixed dap_score_ic i.druguse i.agecatfbl  i.race_catR i.parityR i.religiousR i.schooling   i.employeddi i.foodinsecurdi i.housinginsecurdi i.mainqualcohab  if waveuncutb!=. & sample==1  || siter: || id:,  residual(ar 1, t(waveodduncutb)) 

		log close
	
	
	 