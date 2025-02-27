
*********************************************
* Title: ADAPT Study
* Purpose: Cleaning and preparing data
* Last updated by: Sarah
*********************************************
 

use APPENDED UC+ PMC + NPM + AMB + Inclinic Surveys + UD 2023 06 19.dta
 
 
 // TABLE 1 //
    
  tab1 agecatfbl race_catR religiousR employeddi  mainqualcohab parityR schooling foodinsecurdi housinginsecurdi statesite dap_score_ict audc_impd alc4_monthc cannabisc druguse  if wave==1 & audc_impd==0 & sample==1, m 

  tab1 agecatfbl race_catR religiousR employeddi  mainqualcohab parityR schooling foodinsecurdi housinginsecurdi statesite dap_score_ict audc_impd alc4_monthc cannabisc druguse  if wave==1 & audc_impd==1 & sample==1, m 
 
 **pvals for Table 1
  xtset id waveodduncutb
    melogit audc_impd i.agecatfbl if wave==1 & sample==1 || siter:,   
   testparm i.agecatfbl  
   melogit audc_impd i.race_catR if wave==1 & sample==1 || siter:,     
   testparm i.race_catR  
   melogit audc_impd i.religiousR if wave==1 & sample==1 || siter:,     
   testparm i.religiousR  
   melogit audc_impd i.employeddi if wave==1 & sample==1 || siter:,     
   testparm i.employeddi  
   melogit audc_impd i.schooling  if wave==1 & sample==1 || siter:,    
   testparm i.schooling  
   melogit audc_impd i.mainqualcohab if wave==1 & sample==1 || siter:,    
   testparm i.mainqualcohab  
   melogit audc_impd i.parityR if wave==1 & sample==1 || siter:,      
   testparm i.parityR  
   melogit audc_impd i.foodinsecurdi if wave==1 & sample==1 || siter:,    
	testparm i.foodinsecurdi  
   melogit audc_impd i.housinginsecurdi if wave==1 & sample==1 || siter:,    
   testparm i.housinginsecurdi  
   melogit audc_impd i.statesite if wave==1 & sample==1 || siter:,    
   testparm i.statesite  
   melogit audc_impd i.dap_score_ict if wave==1 & sample==1 || siter: ,   
   testparm i.dap_score_ict  
   melogit audc_impd dap_score_ic if wave==1 & sample==1 || siter: ,  
    

  *summarize age for Table 1, including by alc use
 sum agetv if wave == 1 & sample==1, detail  
  sum agetv if wave == 1 & sample==1 & audc_impd==0, detail  
  sum agetv if wave == 1 & sample==1 & audc_impd==1, detail  
 
 *summarize dap score for Table 1, by alc use
 hist dap_score_ic if wave==1 & sample==1  
 sum dap_score_ic if wave==1 & sample==1 , detail //  median = 2.4 iqr 1.5-3.2
 sum dap_score_ic if wave==1 & sample==1  & audc_impd==0, detail // median = 2.4 iqr 1.7-3.3
 sum dap_score_ic if wave==1 & sample==1  & audc_impd==1, detail // median = 2.6 iqr 1.7-3.4
ttest dap_score_ic if wave==1 & sample==1 , by(audc_impd)


 // Figure 2   
 *tabulate outcome by dap score just prior to pregnancy report for those who became pregnant 
  ta  audc_impd dapentrypmctR if wave==1 & sample==1 & newpregevcutb ==1, m co chi
  ta  alc4_monthc dapentrypmctR if wave==1 & sample==1 & newpregevcutb ==1, m co chi
  ta  cannabisc dapentrypmctR if wave==1 & sample==1 & newpregevcutb ==1, m co chi
  ta  druguse dapentrypmctR if wave==1 & sample==1 & newpregevcutb ==1, m co chi
 

 // Flow Chart missing data 
   * identify number missing observations for outcomes and DAP score 
 ta pregatbl if wave==1    
 ta audc_impd if wave==1, m  
 ta alc4_monthc if wave==1, m  
 ta cannabisc if wave==1, m  
 ta druguse if wave==1, m  
 ta dap_score_ict if wave==1, m  
   
 
 // Assess whether baseline SU is associated with LTFU
	
	stset tstop_lost, failure(lostthis) id(id)
	stcox audc_impdBL,  vce(cluster siter)  // p=0.94
	stcox i.cannabisBL3cat,  vce(cluster siter) 
		testparm i.cannabisBL3cat	 // p=0.02	
	stcox i.alc4_monthcBLR3cat,  vce(cluster siter) 
		testparm i.alc4_monthcBLR3cat	 // p=0.01	
	stcox druguseBL,  vce(cluster siter) // p=0.54
	stcox dapbl, vce(cluster siter)		
		/*p=0.70*/
	stcox ib5.statesite, vce(cluster siter)									
		testparm i.statesite
		/*p=0.02*/
		
*All baseline 
	stcox ib3.agecatbl i.audc_impdBL i.race_catc usbornr i.parityblf ib1.mainliveblt i.religiousbl inschbl i.empbl dapbl ib5.statesite, vce(cluster siter)
	testparm i.audc_impdBL // p=0.25

	stcox ib3.agecatbl i.cannabisBL3cat i.race_catc usbornr i.parityblf ib1.mainliveblt i.religiousbl inschbl i.empbl dapbl ib5.statesite, vce(cluster siter)
	testparm i.cannabisBL3cat // p=0.23

	stcox ib3.agecatbl i.alc4_monthcBLR3cat i.race_catc usbornr i.parityblf ib1.mainliveblt i.religiousbl inschbl i.empbl dapbl ib5.statesite, vce(cluster siter)
	testparm i.alc4_monthcBLR3cat // p=0.32

	sts graph, by(audc_impdBL)		
	sts graph, by(cannabisBL3cat)		
	sts graph, by(alc4_monthcBLR3cat)		
	sts graph, by(druguseBL)		

	// Get rates per 100 person-years   
	*Cannabis use
	stptime, at(365.3) by(cannabisBL3cat)	
	*never
	display 100-(0.00034888*100*365.3)
	*less than daily 
	display 100-(0.00055693 *100*365.3)
	*daily or more
	display 100-(0.00044537*100*365.3)

	*Alcohol use
	stptime, at(365.3) by(alc4_monthcBLR3cat)		
	*never binge
	display 100-(0.00041686*100*365.3)
	*binge 1-3 times
	display 100-(0.00033993*100*365.3)
	*binge weekly
	display 100-(0.00061506*100*365.3)

	*Other drug use
	stptime, at(365.3) by(druguseBL)		
	*no
	display 100-(0.00039602*100*365.3)
	*yes
	display 100-(0.00046339*100*365.3)
	

	// MULTIPLE IMPUTATION //
		/*
		*1) The first imputation is for research question 1 (mixed models exploring association between baseline substance use and DAP). 
		*2) The second imputation is for researxh question 2 (cox models exploring association between baseline substance use x DAP in predicting time to pregnancy. See Paul Allison Statistical Horizons https://statisticalhorizons.com/wp-content/uploads/2012/01/Allison_SurvivalAnalysis.pdf for details on imputation for survival analysis.
*/
		
		** Create IMPUTED DATASET 1  

		**Subset the data to reduce imputing time
		keep dap_score_ic dap_score_ict alc4_monthc audc_impd cannabisc druguse agecatfbl race_catR parityR religiousR schooling  employeddi  foodinsecurdi housinginsecurdi mainqualcohab sample waveuncutb id  waveodduncutb siter statesite  agecatfbl  newpregevcutb newpregnextcutb  wave 
		drop if sample!=1  
		drop if waveodduncutb==.  
 
 		** Set as MI data  
			mi set wide

		** Register MI variables	 
			mi register imputed dap_score_ic  alc4_monthc audc_impd cannabisc druguse race_catR parityR religiousR schooling  employeddi  foodinsecurdi housinginsecurdi mainqualcohab   
			mi register regular id  waveodduncutb siter statesite newpregnextcutb agecatfbl
	
			mi describe  
			
			* DRY RUN TO MAKE SURE PREDICTION EQUATIONS ARE AS DESIRED
			mi impute chained (regress) dap_score_ic  (logit) audc_impd  druguse employeddi foodinsecurdi housinginsecurdi (ologit)  alc4_monthc cannabisc  parityR (mlogit) race_catR  religiousR schooling mainqualcoha  =   waveodduncutb i.statesite i.agecatfbl , dryrun  

			* GENERATE IMPUTATIONS

			mi impute chained (regress) dap_score_ic  (logit) audc_impd  druguse employeddi foodinsecurdi housinginsecurdi (ologit)  alc4_monthc cannabisc  parityR (mlogit) race_catR  religiousR schooling mainqualcoha  =   waveodduncutb i.statesite i.agecatfbl  , rseed(1678) augment add(20) force dots noisily  
			
		
			save ".../Imputed data RQ1.dta", replace
			use ".../Imputed data RQ1.dta"


  ** Create IMPUTED DATASET 2  
 
			*create logged time var for imputed cox models 
		gen lntstop_preg=ln(tstop_preg)
		replace lntstop_preg=0.001 if tstop_preg==0

  
  		keep dap_score_ic dap_score_ict dapentrypmctR alc4_monthc audc_impd cannabisc druguse agecatfbl race_catR parityR religiousR schooling  employeddi  foodinsecurdi housinginsecurdi mainqualcohab sample waveuncutb id waveodduncutb siter statesite lntstop_preg  tstop_preg tstop newpregevcutb newpregnextcutb waveodduncutb wave   

			stset tstop_preg, failure(newpregnextcutb) id(id)
			
			*Estimate baseline survival for each outcome
			
			*heavy alcohol use
			stcox i.audc_impd##i.dap_score_ict i.agecatfbl   i.race_catR  i.parityR  i.religiousR i.mainqualcohab i.schooling i.employeddi i.foodinsecurdi i.housinginsecurdi  i.statesite if sample==1 & waveuncutb!=.,  vce(cluster siter) basesurv(basesurv_audc) 
testparm i.audc_impd#i.dap_score_ict

			*any drinking
			stcox i.alc4_monthc##i.dap_score_ict i.agecatfbl   i.race_catR  i.parityR  i.religiousR i.mainqualcohab i.schooling i.employeddi i.foodinsecurdi i.housinginsecurdi  i.statesite if sample==1 & waveuncutb!=.,  vce(cluster siter) basesurv(basesurv_alc4_monthc) 
testparm i.alc4_monthc#i.dap_score_ict

			*cannabis
			stcox i.cannabisc##i.dap_score_ict i.agecatfbl   i.race_catR  i.parityR  i.religiousR i.mainqualcohab i.schooling i.employeddi i.foodinsecurdi i.housinginsecurdi  i.statesite if sample==1 & waveuncutb!=.,  vce(cluster siter) basesurv(basesurv_cannabisc) 
testparm i.cannabisc#i.dap_score_ict

			*other drug use
			stcox i.druguse##i.dap_score_ict i.agecatfbl   i.race_catR  i.parityR  i.religiousR i.mainqualcohab i.schooling i.employeddi i.foodinsecurdi i.housinginsecurdi  i.statesite if sample==1 & waveuncutb!=.,  vce(cluster siter) basesurv(basesurv_druguse) 
testparm i.druguse#i.dap_score_ict

		** Set as MI data  
			mi set wide

		** Register MI variables	 
			mi register imputed dap_score_ic dap_score_ict  alc4_monthc audc_impd cannabisc druguse race_catR parityR religiousR schooling  employeddi  foodinsecurdi housinginsecurdi mainqualcohab   
			mi register regular id waveuncutb  waveodduncutb siter statesite lntstop_preg tstop_preg newpregnextcutb agecatfbl  dapentrypmctR basesurv_druguse basesurv_cannabisc basesurv_audc basesurv_alc4_monthc
	
			mi describe  
	
		*GENERATE IMPUTATIONS

			mi impute chained (regress) dap_score_ic  (logit) audc_impd  druguse employeddi foodinsecurdi housinginsecurdi (ologit)  alc4_monthc cannabisc  parityR (mlogit) race_catR  religiousR schooling mainqualcoha  =  lntstop_preg  waveuncutb i.statesite i.agecatfbl i.newpregnextcutb  , rseed(1333) augment add(20) force dots noisily  
	
			save ".../Imputed data RQ2a.dta", replace

	

		 
 
	
	 
