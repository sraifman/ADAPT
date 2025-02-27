


 ****************************************************************************************************************** 
 *Title: ADAPT study 
* Purpose: Assess Research Question 2. 
*Is baseline substance use (exposure) associated with incident pregnancy, using DAP to account for pregnancy intendedness?
*******************************************************************************************************************  

 
	** What proportion of all unintended and intended pregnancies are among people who use substances?  
	 *heavy drinking
	ta audc_impd dapentrypmctR if wave==1 & sample==1 & newpregevcutb==1, m col chi 
 		prtesti 130 .39 71 .54 // p=0.04
	 *any drinking
	ta alc4_monthc dapentrypmctR if newpregevcutb==1 & wave==1 & sample==1, m col chi  
 		prtesti 130 .33 71 .44  
		prtesti 130 .05 71 .10  
		prtesti 130 .38 71 .54  
	 *cannabis
	ta cannabisc dapentrypmctR if newpregevcutb==1 & wave==1 & sample==1, m col chi 
		prtesti 130 .27 71 .39  
	 *other drug use
	ta druguse dapentrypmctR if newpregevcutb==1 & wave==1 & sample==1, m col chi
		prtesti 130 .008 71 .056  

 	
	////////    Fully adjusted Cox model - Complete Case  //////// 
		 	

			stset tstop_preg, failure(newpregnextcutb) id(id)  
		 
		 *Outcome AUDIT-C heavy drinking
		
			stcox i.audc_impd##i.dap_score_ict i.agecatfbl   i.race_catR  i.parityR  i.religiousR i.mainqualcohab i.schooling i.employeddi i.foodinsecurdi i.housinginsecurdi   if sample==1 & waveuncutb!=.,  vce(cluster siter) basesurv(basesurv) 
			
			margins i.audc_impd#i.dap_score_ict, predict(xb)
 			
			//Postestimation 
			*Low
			lincom (1.audc_impd + 0.dap_score_ict + 1.audc_impd#0.dap_score_ict) - (0.audc_impd +0.dap_score_ict + 0.audc_impd#0.dap_score_ict), hr
			
			*Mod
			lincom (1.audc_impd + 1.dap_score_ict + 1.audc_impd#1.dap_score_ict) - (0.audc_impd +1.dap_score_ict + 0.audc_impd#1.dap_score_ict), hr
			*High
			lincom (1.audc_impd + 2.dap_score_ict + 1.audc_impd#2.dap_score_ict) - (0.audc_impd +2.dap_score_ict + 0.audc_impd#2.dap_score_ict), hr

			**log relative hazards
			gen neglow=-0.43
			gen neglow_cil=-0.98
			gen neglow_cih=0.12
			
			gen negmid=-1.00
			gen negmid_cil=-1.64
			gen negmid_cih=-0.37

			gen neghigh=-1.96
			gen neghigh_cil = -2.72
			gen neghigh_cih = -1.20
			
			gen poslow=-0.28
			gen poslow_cil=-0.91
			gen poslow_cih=0.35
			
			gen posmid=-1.54
			gen posmid_cil=-2.23
			gen posmid_cih=-0.84
			
			gen poshigh=-1.44
			gen poshigh_cil=-2.19
			gen poshigh_cih=-0.69
			
			** exp(neglow) = HR for that group
			di exp(neglow) // .65050909
			di exp(neglow_cil) // 0.38
			di exp(neglow_cih) // 1.13

			di exp(negmid) //.36787944
			di exp(negmid_cil)
			di exp(negmid_cih)
			
			di exp(neghigh) //.14085842
			di exp(neghigh_cil)
			di exp(neghigh_cih)
			
			di exp(poslow) // .75578374
			di exp(poslow_cil)
			di exp(poslow_cih)
			
			di exp(posmid) //.21438111
			di exp(posmid_cil)
			di exp(posmid_cih)
			
			di exp(poshigh) //.23692775
			di exp(poshigh_cil)
			di exp(poshigh_cih)

			
	*For each of the 6 groups, I'm modifying the basesurv values over time (up or down) by the exponentiated form of the hazard
	gen pneglow = 1-(basesurv^exp(neglow))
	gen pneglow_cil = 1-(basesurv^exp(neglow_cil))
	gen pneglow_cih = 1-(basesurv^exp(neglow_cih))

	gen pnegmid = 1-(basesurv^exp(negmid))
	gen pnegmid_cil = 1-(basesurv^exp(negmid_cil))
	gen pnegmid_cih = 1-(basesurv^exp(negmid_cih))

	gen pneghigh = 1-(basesurv^exp(neghigh))
	gen pneghigh_cil = 1-(basesurv^exp(neghigh_cil))
	gen pneghigh_cih = 1-(basesurv^exp(neghigh_cih))

	gen pposlow = 1-(basesurv^exp(poslow))
	gen pposlow_cil = 1-(basesurv^exp(poslow_cil))
	gen pposlow_cih = 1-(basesurv^exp(poslow_cih))

	gen pposmid = 1-(basesurv^exp(posmid))
	gen pposmid_cil = 1-(basesurv^exp(posmid_cil))
	gen pposmid_cih = 1-(basesurv^exp(posmid_cih))

	gen pposhigh = 1-(basesurv^exp(poshigh))
	gen pposhigh_cil = 1-(basesurv^exp(poshigh_cil))
	gen pposhigh_cih = 1-(basesurv^exp(poshigh_cih))


	 
	**Use these hazards per group in a bar chart to demonstrate the flipping of heightened risk by DAP category. 
	tab pneglow if _t==410 //.2616942
	ta pneglow_cil if _t==410 // 16
	ta pneglow_cih if _t==410  // 41

	tab pnegmid if _t==410 // .1576661 
	tab pnegmid_cil if _t==410 // .09 
	tab pnegmid_cih if _t==410 // .28  

	tab pneghigh if _t==410 //.0635848 
	tab pneghigh_cil if _t==410 //.03
	tab pneghigh_cih if _t==410 //.13

	tab pposlow if _t==410 //.2970695  
	tab pposlow_cil if _t==410 //.17
	tab pposlow_cih if _t==410 //.48 

	tab pposmid if _t==410 //.0951511
	tab pposmid_cil if _t==410 //.05
	tab pposmid_cih if _t==410 //.18

	tab pposhigh if _t==410 //.1046164
	tab pposhigh_cil if _t==410 //.05
	tab pposhigh_cih if _t==410 //.21


	////////    Fully adjusted models with imputation  //////// 

		**** Used basesurv from non-imputed models and combined with imputed HRs. 
			
		use "/Imputed data RQ2a.dta", clear

		mi stset tstop_preg, failure(newpregnextcutb) id(id) 
 
		*Outcome: AUDIT-C 
		
		mi estimate, hr post: stcox i.audc_impd##i.dap_score_ict i.agecatfbl   i.race_catR  i.parityR  i.religiousR i.mainqualcohab i.schooling i.employeddi i.foodinsecurdi i.housinginsecurdi  i.statesite if sample==1 & waveuncutb!=.,  vce(cluster siter) 
			

			estimates store outcome
			*est restore  outcome
			mimrgns i.audc_impd#i.dap_score_ict, predict(xb)
			
			//Postestimation  
			*Low
			lincom (1.audc_impd + 0.dap_score_ict + 1.audc_impd#0.dap_score_ict) - (0.audc_impd +0.dap_score_ict + 0.audc_impd#0.dap_score_ict), hr
			
			*Mod
			lincom (1.audc_impd + 1.dap_score_ict + 1.audc_impd#1.dap_score_ict) - (0.audc_impd +1.dap_score_ict + 0.audc_impd#1.dap_score_ict), hr
			*High
			lincom (1.audc_impd + 2.dap_score_ict + 1.audc_impd#2.dap_score_ict) - (0.audc_impd +2.dap_score_ict + 0.audc_impd#2.dap_score_ict), hr

			
			**log relative hazards
			cap drop neglow
			gen neglow=-0.47
			gen neglow_cil=-1.02
			gen neglow_cih=0.08
			
			cap drop negmid
			gen negmid=-1.04
			gen negmid_cil=-1.66
			gen negmid_cih=-0.42
			
			cap drop neghigh
			gen neghigh=-1.92
			gen neghigh_cil=-2.67
			gen neghigh_cih=-1.17
			
			cap drop poslow
			gen poslow=-0.32
			gen poslow_cil=-0.93
			gen poslow_cih=0.30
			
			cap drop posmid
			gen posmid=-1.52
			gen posmid_cil=-2.23
			gen posmid_cih=-0.80
			
			cap drop poshigh
			gen poshigh=-1.50
			gen poshigh_cil=-2.23
			gen poshigh_cih=-0.76
			
			** exp(neglow) = HR for that group
			di exp(neglow)  
			di exp(neglow_cil)  
			di exp(neglow_cih)  
			
			di exp(negmid)  
			di exp(negmid_cil)  
			di exp(negmid_cih) 
			
			di exp(neghigh)  
			di exp(neghigh_cil) 
			di exp(neghigh_cih)  
			
			di exp(poslow)  
			di exp(poslow_cil)  
			di exp(poshigh_cih)  
			
			di exp(posmid)  
			di exp(posmid_cil)  
			di exp(posmid_cih)  
			
			di exp(poshigh)  
			di exp(poshigh_cil)  
			di exp(poshigh_cih)  

			
			est restore  outcome
			cap drop pneglow
			gen pneglow = 1-(basesurv_audc^exp(neglow))
			gen pneglowcil = 1-(basesurv_audc^exp(neglow_cil))
			gen pneglowcih = 1-(basesurv_audc^exp(neglow_cih))

			cap drop pnegmid
			gen pnegmid = 1-(basesurv_audc^exp(negmid))
			gen pnegmidcil = 1-(basesurv_audc^exp(negmid_cil))
			gen pnegmidcih = 1-(basesurv_audc^exp(negmid_cih))

			cap drop pneghigh
			gen pneghigh = 1-(basesurv_audc^exp(neghigh))
			gen pneghighcil = 1-(basesurv_audc^exp(neghigh_cil))
			gen pneghighcih = 1-(basesurv_audc^exp(neghigh_cih))

			cap drop pposlow
			gen pposlow = 1-(basesurv_audc^exp(poslow))
			gen pposlowcil = 1-(basesurv_audc^exp(poslow_cil))
			gen pposlowcih = 1-(basesurv_audc^exp(poslow_cih))

			cap drop pposmid
			gen pposmid = 1-(basesurv_audc^exp(posmid))
			gen pposmidcil = 1-(basesurv_audc^exp(posmid_cil))
			gen pposmidcih = 1-(basesurv_audc^exp(posmid_cih))

			cap drop pposhigh
			gen pposhigh = 1-(basesurv_audc^exp(poshigh))
			gen pposhighcil = 1-(basesurv_audc^exp(poshigh_cil))
			gen pposhighcih = 1-(basesurv_audc^exp(poshigh_cih))

			tab1 pneglow pnegmid pneghigh pposlow pposmid pposhigh

			** Hazards per group at 410 days 
			tab pneglow if _t==410  
			tab pneglowcil if _t==410 
			tab pneglowcih if _t==410  

			tab pnegmid if _t==410  
			tab pnegmidcil if _t==410  
			tab pnegmidcih if _t==410  

			tab pneghigh if _t==410  
			tab pneghighcil if _t==410  
			tab pneghighcih if _t==410  
			 
			tab pposlow if _t==410  
			tab pposlowcil if _t==410  
			tab pposlowcih if _t==410  

			tab pposmid if _t==410  
			tab pposmidcil if _t==410  
			tab pposmidcih if _t==410  

			tab pposhigh if _t==410  
			tab pposhighcil if _t==410   
			tab pposhighcih if _t==410   

			
		
		//////// Binge drinking //////////
		
		**3-cat binge drinking 
		
			mi estimate, hr post: stcox i.alc4_monthc##i.dap_score_ict i.agecatfbl   i.race_catR  i.parityR  i.religiousR i.mainqualcohab i.schooling i.employeddi i.foodinsecurdi i.housinginsecurdi i.statesite   if sample==1 & waveuncutb!=.,  vce(cluster siter)  
		
			estimates store outcome
			*est restore  outcome
			mimrgns i.alc4_monthc#i.dap_score_ict, predict(xb)
			
			//postestimation to get estimates by DAP stratum 
			* low  DAP
		lincom (1.alc4_monthc + 0.dap_score_ict+1.alc4_monthc#0.dap_score_ict) - (0.alc4_monthc +0.dap_score_ict + 0.alc4_monthc#0.dap_score_ict), hr 
		lincom (2.alc4_monthc + 0.dap_score_ict+2.alc4_monthc#0.dap_score_ict) - (0.alc4_monthc +0.dap_score_ict + 0.alc4_monthc#0.dap_score_ict), hr 
			*mid DAP
		lincom (1.alc4_monthc + 1.dap_score_ict+1.alc4_monthc#1.dap_score_ict) - (0.alc4_monthc +1.dap_score_ict + 0.alc4_monthc#1.dap_score_ict), hr
		lincom (2.alc4_monthc + 1.dap_score_ict+2.alc4_monthc#1.dap_score_ict) - (0.alc4_monthc +1.dap_score_ict + 0.alc4_monthc#1.dap_score_ict), hr
			*diff between 1-3 binge vs never  within high level DAP
			lincom (1.alc4_monthc +2.dap_score_ict+1.alc4_monthc#2.dap_score_ict) - (0.alc4_monthc +2.dap_score_ict+0.alc4_monthc#2.dap_score_ict), hr
			*diff between weekly binge vs never  within high level DAP
			lincom (2.alc4_monthc +2.dap_score_ict+2.alc4_monthc#2.dap_score_ict) - (0.alc4_monthc +2.dap_score_ict+0.alc4_monthc#2.dap_score_ict), hr

			**log relative hazards
			gen nobingelow=-0.48
			gen nobingelowcil=-1.02
			gen nobingelowcih=0.05
			
			cap drop nobingemid
			gen nobingemid=-1.10
			gen nobingemidcil=-1.66
			gen nobingemidcih=-0.52

			gen nobingehigh=-1.95
			gen nobingehighcil=-2.67
			gen nobingehighcih=-1.22

			gen somebingelow= -0.34
			gen somebingelowcil= -0.93
			gen somebingelowcih= 0.25

			gen somebingemid=-1.44
			gen somebingemidcil=-2.29
			gen somebingemidcih=-0.60

			gen somebingehigh=-1.49
			gen somebingehighcil=-2.32
			gen somebingehighcih=-0.66

			gen wklybingelow=-0.13
			gen wklybingelowcil=-1.13
			gen wklybingelowcih=0.88

			gen wklybingemid=-1.22
			gen wklybingemidcil=-2.11
			gen wklybingemidcih=-0.32

			gen wklybingehigh=-1.31
			gen wklybingehighcil=-2.29
			gen wklybingehighcih=-0.32

			
			** exp(neglow) = HR for that group
			di exp(nobingelow)   
			di exp(nobingelowcil)    
			di exp(nobingelowcih)  

			di exp(nobingemid)  
			di exp(nobingemidcil)  
			di exp(nobingemidcih)  

			di exp(nobingehigh)  
			di exp(nobingehighcil)  
			di exp(nobingehighcih)  

			di exp(somebingelow)  
			di exp(somebingelowcil)  
			di exp(somebingelowcih)  

			di exp(somebingemid)  
			di exp(somebingemidcil)  
			di exp(somebingemidcih)  
			
			di exp(somebingehigh)  
			di exp(somebingehighcil)  
			di exp(somebingehighcih)  
			
			di exp(wklybingelow)  
			di exp(wklybingelowcil)  
			di exp(wklybingelowcih)  
			
			di exp(wklybingemid)  
			di exp(wklybingemidcil)  
			di exp(wklybingemidcih)  
			
			di exp(wklybingehigh)  
			di exp(wklybingehighcil)  
			di exp(wklybingehighcih)  

						
			*For each of the 6 groups, modify the basesurv values over time by the exponentiated form of the hazard
			gen pnobingelow = 1-(basesurv_alc4_monthc^exp(nobingelow))
			gen pnobingelowcil = 1-(basesurv_alc4_monthc^exp(nobingelowcil))
			gen pnobingelowcih = 1-(basesurv_alc4_monthc^exp(nobingelowcih))

			gen pnobingemid = 1-(basesurv_alc4_monthc^exp(nobingemid))
			gen pnobingemidcil = 1-(basesurv_alc4_monthc^exp(nobingemidcil))
			gen pnobingemidcih = 1-(basesurv_alc4_monthc^exp(nobingemidcih))

			gen pnobingehigh = 1-(basesurv_alc4_monthc^exp(nobingehigh))
			gen pnobingehighcil = 1-(basesurv_alc4_monthc^exp(nobingehighcil))
			gen pnobingehighcih = 1-(basesurv_alc4_monthc^exp(nobingehighcih))

			gen psomebingelow = 1-(basesurv_alc4_monthc^exp(somebingelow))
			gen psomebingelowcil = 1-(basesurv_alc4_monthc^exp(somebingelowcil))
			gen psomebingelowcih = 1-(basesurv_alc4_monthc^exp(somebingelowcih))

			gen psomebingemid = 1-(basesurv_alc4_monthc^exp(somebingemid))
			gen psomebingemidcil = 1-(basesurv_alc4_monthc^exp(somebingemidcil))
			gen psomebingemidcih = 1-(basesurv_alc4_monthc^exp(somebingemidcih))

			gen psomebingehigh = 1-(basesurv_alc4_monthc^exp(somebingehigh))
			gen psomebingehighcil = 1-(basesurv_alc4_monthc^exp(somebingehighcil))
			gen psomebingehighcih = 1-(basesurv_alc4_monthc^exp(somebingehighcih))

			gen pwklybingelow = 1-(basesurv_alc4_monthc^exp(wklybingelow))
			gen pwklybingelowcil = 1-(basesurv_alc4_monthc^exp(wklybingelowcil))
			gen pwklybingelowcih = 1-(basesurv_alc4_monthc^exp(wklybingelowcih))

			gen pwklybingemid = 1-(basesurv_alc4_monthc^exp(wklybingemid))
			gen pwklybingemidcil = 1-(basesurv_alc4_monthc^exp(wklybingemidcil))
			gen pwklybingemidcih = 1-(basesurv_alc4_monthc^exp(wklybingemidcih))

			gen pwklybingehigh = 1-(basesurv_alc4_monthc^exp(wklybingehigh))
			gen pwklybingehighcil = 1-(basesurv_alc4_monthc^exp(wklybingehighcil))
			gen pwklybingehighcih = 1-(basesurv_alc4_monthc^exp(wklybingehighcih))



			** Hazards per group in a chart  
			tab pnobingelow if _t==410 
			tab pnobingelowcil if _t==410  
			tab pnobingelowcih if _t==410  

			tab pnobingemid if _t==410  
			tab pnobingemidcil if _t==410  
			tab pnobingemidcih if _t==410  

			tab pnobingehigh if _t==410  
			tab pnobingehighcil if _t==410   
			tab pnobingehighcih if _t==410  
			 
			tab psomebingelow if _t==410  
			 tab psomebingelowcil if _t==410  
			tab psomebingelowcih if _t==410  

			tab psomebingemid if _t==410 
			tab psomebingemidcil if _t==410  
			tab psomebingemidcih if _t==410  

			tab psomebingehigh if _t==410  
				tab psomebingehighcil if _t==410  
			tab psomebingehighcih if _t==410  

			tab pwklybingelow if _t==410  
			tab pwklybingelowcil if _t==410    
			tab pwklybingelowcih if _t==410  

			tab pwklybingemid if _t==410  
			tab pwklybingemidcil if _t==410  
			tab pwklybingemidcih if _t==410  

			tab pwklybingehigh if _t==410  
				tab pwklybingehighcil if _t==410    
			tab pwklybingehighcih if _t==410  


		/////// Cannabis //////
				
			mi estimate, hr post: stcox i.cannabisc##i.dap_score_ict  i.agecatfbl   i.race_catR  i.parityR i.religiousR i.mainqualcohab i.schooling i.employeddi i.foodinsecurdi i.housinginsecurdi i.statesite  if sample==1 & waveuncutb!=., hr efron vce(cluster siter)
	
				estimates store outcome
			*est restore  outcome
			mimrgns i.cannabisc#i.dap_score_ict, predict(xb)
			
			//postestimation to get estimates by DAP stratum  
			* low  DAP
			lincom (1.cannabisc + 0.dap_score_ict+1.cannabisc#0.dap_score_ict) - (0.cannabisc +0.dap_score_ict + 0.cannabisc#0.dap_score_ict), hr 
			lincom (2.cannabisc + 0.dap_score_ict+2.cannabisc#0.dap_score_ict) - (0.cannabisc +0.dap_score_ict + 0.cannabisc#0.dap_score_ict), hr 
			*mid DAP
			lincom (1.cannabisc + 1.dap_score_ict+1.cannabisc#1.dap_score_ict) - (0.cannabisc +1.dap_score_ict + 0.cannabisc#1.dap_score_ict), hr
			lincom (2.cannabisc + 1.dap_score_ict+2.cannabisc#1.dap_score_ict) - (0.cannabisc +1.dap_score_ict + 0.cannabisc#1.dap_score_ict), hr
			*diff between 1-3 binge vs never  within high level DAP
			lincom (1.cannabisc +2.dap_score_ict+1.cannabisc#2.dap_score_ict) - (0.cannabisc +2.dap_score_ict+0.cannabisc#2.dap_score_ict), hr
			*diff between weekly binge vs never  within high level DAP
			lincom (2.cannabisc +2.dap_score_ict+2.cannabisc#2.dap_score_ict) - (0.cannabisc +2.dap_score_ict+0.cannabisc#2.dap_score_ict), hr


		lab var cannabisc "Cannabis (3-cat)"

			**log relative hazards
			gen neverlow=-0.46
			gen neverlow_cil=-1.02
			gen neverlow_cih=0.10

			gen nevermid=-1.28
			gen nevermid_cil=-1.85
			gen nevermid_cih=-0.71

			gen neverhigh=-1.60
			gen neverhigh_cil=-2.35
			gen neverhigh_cih=-0.86

			gen ltdailylow= 0.04
			gen ltdailylow_cil= -0.65
			gen ltdailylow_cih= 0.72
			
			gen ltdailymid=-0.99
			gen ltdailymid_cil=-1.79
			gen ltdailymid_cih=-0.20

			gen ltdailyhigh=-1.75
			gen ltdailyhigh_cil=-2.68
			gen ltdailyhigh_cih=-0.83

			gen dailylow=-0.18
			gen dailylow_cil=-0.97
			gen dailylow_cih=0.60

			gen dailymid=-0.72
			gen dailymid_cil=-1.62
			gen dailymid_cih=0.18

			gen dailyhigh=-1.52
			gen dailyhigh_cil=-2.38
			gen dailyhigh_cih=-0.67

			
			** exp(neglow) = HR for that group
			di exp(neverlow)  
			di exp(neverlow_cil)  
			di exp(neverlow_cih)  

			di exp(nevermid)  
			di exp(nevermid_cil)  
			di exp(nevermid_cih)  

			di exp(neverhigh)  
			di exp(neverhigh_cil)  
			di exp(neverhigh_cih)  

			di exp(ltdailylow)  
			di exp(ltdailylow_cil)  
			di exp(ltdailylow_cih)  

			di exp(ltdailymid) 
			di exp(ltdailymid_cil)  
			di exp(ltdailymid_cih)  

			di exp(ltdailyhigh)  
			di exp(ltdailyhigh_cil)  
			di exp(ltdailyhigh_cih)  

			di exp(dailylow) 
			di exp(dailylow_cil)  
			di exp(dailylow_cih)  

			di exp(dailymid) 
			di exp(dailymid_cil)  
			di exp(dailymid_cih)  
			
			di exp(dailyhigh)  
			di exp(dailyhigh_cil)  
			di exp(dailyhigh_cih)  

						
			*For each of the 6 groups, modify the basesurv values over time by the exponentiated form of the hazard
			gen pneverlow = 1-(basesurv_cannabisc^exp(neverlow))
			gen pneverlow_cil = 1-(basesurv_cannabisc^exp(neverlow_cil))
			gen pneverlow_cih = 1-(basesurv_cannabisc^exp(neverlow_cih))

			gen pnevermid = 1-(basesurv_cannabisc^exp(nevermid))
			gen pnevermid_cil = 1-(basesurv_cannabisc^exp(nevermid_cil))
			gen pnevermid_cih = 1-(basesurv_cannabisc^exp(nevermid_cih))

			gen pneverhigh = 1-(basesurv_cannabisc^exp(neverhigh))
			gen pneverhigh_cil = 1-(basesurv_cannabisc^exp(neverhigh_cil))
			gen pneverhigh_cih = 1-(basesurv_cannabisc^exp(neverhigh_cih))

			gen pltdailylow = 1-(basesurv_cannabisc^exp(ltdailylow))
			gen pltdailylow_cil = 1-(basesurv_cannabisc^exp(ltdailylow_cil))
			gen pltdailylow_cih = 1-(basesurv_cannabisc^exp(ltdailylow_cih))

			gen pltdailymid = 1-(basesurv_cannabisc^exp(ltdailymid))
			gen pltdailymid_cil = 1-(basesurv_cannabisc^exp(ltdailymid_cil))
			gen pltdailymid_cih = 1-(basesurv_cannabisc^exp(ltdailymid_cih))

			gen pltdailyhigh = 1-(basesurv_cannabisc^exp(ltdailyhigh))
			gen pltdailyhigh_cil = 1-(basesurv_cannabisc^exp(ltdailyhigh_cil))
			gen pltdailyhigh_cih = 1-(basesurv_cannabisc^exp(ltdailyhigh_cih))

			gen pdailylow = 1-(basesurv_cannabisc^exp(dailylow))
			gen pdailylow_cil = 1-(basesurv_cannabisc^exp(dailylow_cil))
			gen pdailylow_cih = 1-(basesurv_cannabisc^exp(dailylow_cih))

			gen pdailymid = 1-(basesurv_cannabisc^exp(dailymid))
			gen pdailymid_cil = 1-(basesurv_cannabisc^exp(dailymid_cil))
			gen pdailymid_cih = 1-(basesurv_cannabisc^exp(dailymid_cih))

			gen pdailyhigh = 1-(basesurv_cannabisc^exp(dailyhigh))
			gen pdailyhigh_cil = 1-(basesurv_cannabisc^exp(dailyhigh_cil))
			gen pdailyhigh_cih = 1-(basesurv_cannabisc^exp(dailyhigh_cih))


			 
			** Hazards per group in a chart  
			tab pneverlow if _t==410 
			tab pneverlow_cil if _t==410  
			tab pneverlow_cih if _t==410  

			tab pnevermid if _t==410  
			tab pnevermid_cil if _t==410  
			tab pnevermid_cih if _t==410 

			tab pneverhigh if _t==410  
			tab pneverhigh_cil if _t==410  
			tab pneverhigh_cih if _t==410  

			tab pltdailylow if _t==410   
			tab pltdailylow_cil if _t==410  
			tab pltdailylow_cih if _t==410  

			tab pltdailymid if _t==410  
			tab pltdailymid_cil if _t==410 
			tab pltdailymid_cih if _t==410  

			tab pltdailyhigh if _t==410 
			tab pltdailyhigh_cil if _t==410  
			tab pltdailyhigh_cih if _t==410 
			
			tab pdailylow if _t==410    
			tab pdailylow_cil if _t==410  
			tab pdailylow_cih if _t==410    

			tab pdailymid if _t==410  
			tab pdailymid_cil if _t==410  
			tab pdailymid_cih if _t==410  

			tab pdailyhigh if _t==410  
			tab pdailyhigh_cil if _t==410  
			tab pdailyhigh_cih if _t==410  	

	
		// Repeat for Other Drug Use // 
	
			mi estimate, hr post: stcox i.druguse##i.dap_score_ict  i.agecatfbl   i.race_catR  i.parityR i.religiousR i.mainqualcohab i.schooling i.employeddi i.foodinsecurdi i.housinginsecurdi i.statesite   if sample==1 & waveuncutb!=.  , hr efron vce(cluster siter)
			mi testparm i.druguse#i.dap_score_ict
		
				estimates store outcome
			*est restore  outcome
			mimrgns i.druguse#i.dap_score_ict, predict(xb)
			
		//Postestimation
			*Low DAP
			lincom (1.druguse + 0.dap_score_ict + 1.druguse#0.dap_score_ict) - (0.druguse +0.dap_score_ict + 0.druguse#0.dap_score_ict), hr
			*Mod DAP
			lincom (1.druguse + 1.dap_score_ict + 1.druguse#1.dap_score_ict) - (0.druguse +1.dap_score_ict + 0.druguse#1.dap_score_ict), hr
			*High DAP
			lincom (1.druguse +2.dap_score_ict+1.druguse#2.dap_score_ict) - (0.druguse +2.dap_score_ict+ 0.druguse#2.dap_score_ict), hr

			**log relative hazards
			gen nolow=-0.46
			gen nolowcil=-1.02
			gen nolowcih=0.11
			
			gen nomid=-1.27
			gen nomidcil=-1.87
			gen nomidcih=-0.66

			gen nohigh=-1.78
			gen nohighcil=-2.49
			gen nohighcih=-1.06

			gen yeslow=-1.01
			gen yeslowcil=-3.10
			gen yeslowcih=1.08

			gen yesmid=-0.69
			gen yesmidcil=-1.76
			gen yesmidcih=0.38

			gen yeshigh=-1.38
			gen yeshighcil=-2.32
			gen yeshighcih=-0.45

			
			** exp(nolow) = HR for that group
			di exp(nolow)  
			di exp(nolowcil)  
			di exp(nolowcih)  

			di exp(nomid)  
			di exp(nomidcil)  
			di exp(nomidcih)  

			di exp(nohigh)  
			di exp(nohighcil)  
			di exp(nohighcih)  

			di exp(yeslow)  
			di exp(yeslowcil)  
			di exp(yeslowcih)  

			di exp(yesmid)  
			di exp(yesmidcil)  
			di exp(yesmidcih)  

			di exp(yeshigh) 
			di exp(yeshighcil)  
			di exp(yeshighcih)  


		*For each of the 6 groups, modify the basesurv values over time  
		gen pnolow = 1-(basesurv_druguse^exp(nolow))
		gen pnolowcil = 1-(basesurv_druguse^exp(nolowcil))
		gen pnolowcih = 1-(basesurv_druguse^exp(nolowcih))

		gen pnomid = 1-(basesurv_druguse^exp(nomid))
		gen pnomidcil = 1-(basesurv_druguse^exp(nomidcil))
		gen pnomidcih = 1-(basesurv_druguse^exp(nomidcih))

		gen pnohigh = 1-(basesurv_druguse^exp(nohigh))
		gen pnohighcil = 1-(basesurv_druguse^exp(nohighcil))
		gen pnohighcih = 1-(basesurv_druguse^exp(nohighcih))

		gen pyeslow = 1-(basesurv_druguse^exp(yeslow))
		gen pyeslowcil = 1-(basesurv_druguse^exp(yeslowcil))
		gen pyeslowcih = 1-(basesurv_druguse^exp(yeslowcih))

		gen pyesmid = 1-(basesurv_druguse^exp(yesmid))
		gen pyesmidcil = 1-(basesurv_druguse^exp(yesmidcil))
		gen pyesmidcih = 1-(basesurv_druguse^exp(yesmidcih))

		gen pyeshigh = 1-(basesurv_druguse^exp(yeshigh))
		gen pyeshighcil = 1-(basesurv_druguse^exp(yeshighcil))
		gen pyeshighcih = 1-(basesurv_druguse^exp(yeshighcih))


			
		** Hazards per group in a chart  
		tab pnolow if _t==410  
		tab pnolowcil if _t==410 
		tab pnolowcih if _t==410  

		tab pnomid if _t==410   
		tab pnomidcil if _t==410    
		tab pnomidcih if _t==410  

		tab pnohigh if _t==410   
		tab pnohighcil if _t==410  
		tab pnohighcih if _t==410  

		tab pyeslow if _t==410  
		tab pyeslowcil if _t==410  
		tab pyeslowcih if _t==410  

		tab pyesmid if _t==410  
		tab pyesmidcil if _t==410  
		tab pyesmidcih if _t==410  

		tab pyeshigh if _t==410 
		tab pyeshighcil if _t==410 		
		tab pyeshighcih if _t==410  
			 
			 
	
	
