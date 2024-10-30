doedit "C:\Users\DEL\OneDrive - Kemri Wellcome Trust\Documents\Moses Ngari - Moses Ngari's files\Delphine_Julie\CHAIN_data\Analysis_script.do" 
use CHAIN_cohort.dta,clear
recode pop_density   (0/1500=0) (1501/181000=1),gen(rural_urban)
label define  lrural 0"Urban" 1"Rural"
describe rural_urban
codebook rural
describe pop_density
describe rural_urban site
view site
summarize site
list site
inspect rural_urban site
tabulate rural_urban site
tabulate site rural_urban
label define rural_urban 0"Urban" 1"Rural"
tabulate site rural_urban
label variable rural_urban ""
codebook rural_urban
label values rural_urban
tabulate site rural_urban
browse rural_urban
gen rural_urban1 = 0 if pop_density < 300 | has_livestock_hwa == 1 | pcarer_pincome == 1
 tabulate rural_urban1
tabulate site rural_urban1
codebook rural_urban1
replace rural_urban1=1 if rural_urban==1 & rural_urban1==.
replace rural_urban1=2 if rural_urban==2 & rural_urban1==.
label values rural_urban1 lrural
tabulate site rural_urban1
label define rural_urban1 0 "rural" 1 "urban"
label values rural_urban rural_urban1
tabulate site rural_urban
tabulate site rural_urban1
codebook rural_urban1
label values rural_urban Residence
label values rural_urban
tabulate site rural_urban


//New look
use CHAIN_cohort.dta,clear
tab site
tab urban
gen rural_popdensity=0
replace rural_popdensity=1 if pop_density<500 & africa==1
replace rural_popdensity=1 if pop_density<1500 & africa==0
tab site rural_popdensity,row

merge 1:1 record_id using hsurv_data_Rural.dta
keep if _merge==3
drop _merge

egen nanimals=rowtotal( hsurv_num_cows hsurv_num_sheep hsurv_num_equine hsurv_num_goats)
replace nanimals=0 if hsurv_has_livestock !=1

replace rural_popdensity=1 if rural_popdensity==0 & (hsurv_hsehead_income_src___1==1 | nanimals>0)
label define lrural1 0"Urban" 1"Rural" 
label values rural_popdensity lrural1
tab site rural_popdensity,row

recode pop_density (1501/190000=0) (300/1500.999=1) (0/299.999=2),gen(wb_rural_urban)
label define lpopd 0"Urban" 1"Peri-urban" 2"Rural"
label values wb_rural_urban lpopd

glm rural_popdensity i.adm_dead , family(binomial) link(log) eform
//Proportion rural
tab rural_popdensity
tab wb_rural_urban
ci prop rural_popdensity
cii prop 3101 651
cii prop 3101 550
cii prop 3101 1900

//Factors assicoated rural vs urban
label define lagem 0">11 months" 1"6 to 11 months" 2"<6 months"
label values age_group_adm lagem
replace wlz_adm= wlz_disch if wlz_adm==.
recode wlz_adm (-11/-1.9999=1) (. -2/1.999=0) (2/5=2),gen(adm_wast)
label define lwast0 0"Normal" 1"Wasted" 2"Overweight"
label values adm_wast lwast0 
recode mental_cat (0/2=0) (3/4=1),gen(mental_binary)
label define lmb 0"None-Mild" 1"Moderate-severe"
label values mental_binary lmb 
recode study_hosp_dist (0/4.999=0) (5/9.999999=1) (10/120=2),gen(study_dist_group)
label define ldist 0"<5km" 1"5 to 9km" 2">=10km"
label values study_dist_group ldist
recode nearest_hosp_dist (0/4.999=0) (5/9.999999=1) (10/120=2),gen(nearest_dist_group)
label values nearest_dist_group ldist
replace travel_time=2 if travel_time>2
recode mom_sick (0=1) (1=0),gen(momsick)
label define lmomsick 0"No" 1"Yes"
label values momsick lmomsick

gen logagemons=log(agemons)
meglm rural_popdensity logagemons || site:, family(binomial) link(logit) eform base
meglm rural_popdensity i.age_group_adm || site:, family(binomial) link(logit) eform base
meglm rural_popdensity i.sex || site:, family(binomial) link(logit) eform base
meglm rural_popdensity i.shock || site:, family(binomial) link(logit) eform base
meglm rural_popdensity i.sev_pneum_adm || site:, family(binomial) link(logit) eform base
meglm rural_popdensity i.diarrhoea || site:, family(binomial) link(logit) eform base
meglm rural_popdensity i.malaria_rdt_adm || site:, family(binomial) link(logit) eform base
meglm rural_popdensity i.iconsciousness || site:, family(binomial) link(logit) eform base
meglm rural_popdensity i.bldglucose_group_adm || site:, family(binomial) link(logit) eform base
meglm rural_popdensity i.abc || site:, family(binomial) link(logit) eform base
meglm rural_popdensity i.adm_wast  || site:, family(binomial) link(logit) eform base
meglm rural_popdensity i.stunt_adm  || site:, family(binomial) link(logit) eform base
meglm rural_popdensity i.hiv  || site:, family(binomial) link(logit) eform base
meglm rural_popdensity i.birth_size || site:, family(binomial) link(logit) eform base
meglm rural_popdensity i.chronic_adm || site:, family(binomial) link(logit) eform base
meglm rural_popdensity i.breast_feeding_adm || site:, family(binomial) link(logit) eform base
meglm rural_popdensity i.pri_carer_mom || site:, family(binomial) link(logit) eform base
meglm rural_popdensity i.momsick || site:, family(binomial) link(logit) eform base
meglm rural_popdensity i.educ_level || site:, family(binomial) link(logit) eform base
meglm rural_popdensity i.mental_binary || site:, family(binomial) link(logit) eform base
meglm rural_popdensity i.worktype || site:, family(binomial) link(logit) eform base
meglm rural_popdensity i.assets_quintiles || site:, family(binomial) link(logit) eform base
meglm rural_popdensity i.food_insecurity || site:, family(binomial) link(logit) eform base
meglm rural_popdensity i.h_toilet || site:, family(binomial) link(logit) eform base
meglm rural_popdensity i.water_unavail || site:, family(binomial) link(logit) eform base
meglm rural_popdensity logdistance_studyhosp || site:, family(binomial) link(logit) eform base
meglm rural_popdensity i.study_dist_group || site:, family(binomial) link(logit) eform base
meglm rural_popdensity i.nearest_dist_group || site:, family(binomial) link(logit) eform base
meglm rural_popdensity i.travelcost || site:, family(binomial) link(logit) eform base
meglm rural_popdensity i.travel_time || site:, family(binomial) link(logit) eform base

meglm rural_popdensity i.sex i.malaria_rdt_adm i.abc i.hiv i.momsick i.educ_level i.worktype i.assets_quintiles i.h_toilet i.water_unavail i.study_dist_group i.nearest_dist_group i.travelcost i.travel_time || site:, family(binomial) link(logit) eform base



//Inpatient deaths
tab rural_popdensity adm_dead,row
meglm rural_popdensity i.adm_dead i.sex logagemons|| site:, family(binomial) link(logit) eform

tab wb_rural_urban adm_dead,row chi
ologit wb_rural_urban i.adm_dead i.sex logagemons,or
meologit wb_rural_urban i.adm_dead  i.sex logagemons || site:, or

mlogit wb_rural_urban i.adm_dead  i.sex logagemons, base(0)
mlogit, rrr

mlogit wb_rural_urban i.adm_dead  i.sex logagemons i.site, base(0)
mlogit, rrr


//30-days mortality
***Read the data and set time of follow-up for those who died on date of admission to 0.5days and drop the community participants
replace end_date=end_date+0.5 if end_date==date_adm

***************************************************
***Sampling weights;these are final weights averaged from four sites (Kilifi, Migori, Banfora and Dhaka)
***************************************************
gen nweight=1
replace nweight=0.39 if abc==1    //weights used are 25% for SAM, 16% for MAM and 59% NW
replace nweight=0.40 if abc==2

*************************************************
***Add LTFU weights, 27 units generated using the inverse probabaility approach
************************************************
merge 1:1 record_id using CHAIN_ltfu_30days_weights.dta,nogen

*combine LTFU and sampling weights 
cap drop ltfu_sample_wt
gen ltfu_sample_wt = nweight*ltfu_weights


*********************************************************
****Set data to survival analysis format, split at 30.4days and drop deaths after 30days
*********************************************************
cap stset,clear
*stset end_date [pweight=ltfu_weights], id(record_id) failure(dead) origin(time date_adm) exit(time .) scale(30.44)        // LTFU wt
*stset end_date [pweight=nweight], id(record_id) failure(dead) origin(time date_adm) exit(time .) scale(30.44)               // sample weigs (not needed for mereg
stset end_date, id(record_id) failure(dead) origin(time date_adm) exit(time .) scale(30.44) 

stsplit d30, at(1)
tab d30 dead
drop if d30>0
replace dead=0 if dead==.

tab dead abc,col chi
strate,per(1000)
strate rural_popdensity,per(1000)
strate wb_rural_urban,per(1000)



/*
stset end_date, id(record_id) failure(dead) origin(time date_adm) exit(time .)
stsplit d30, at(30.44)
tab d30 dead
drop if d30>0
replace dead=0 if dead==.
*/
**********************************************************
***Base model & plot the cumulitive hazard curve using the mixed effect multilevel model
**********************************************************
*Get log of age
gen logagemons=log(agemons)

tab rural_popdensity dead,row
tab sex dead,row
tab logagemons dead,row
tab malaria_rdt_adm dead,row
tab  abc dead,row
tab  hiv dead,row
tab  momsick dead,row
tab educ_level dead,row
tab worktype dead,row
tab assets_quintiles dead,row
tab h_toilet dead,row
tab water_unavail dead ,row
tab study_dist_group dead,row
tab nearest_dist_group dead ,row
tab travelcost dead ,row
tab travel_time ,row
 


mestreg i.rural_popdensity logagemons i.sex  [pweight=ltfu_weights] || site:, distribution(weib) vce(robust) 

mestreg logagemons i.sex i.malaria_rdt_adm i.abc i.hiv i.momsick i.educ_level i.worktype i.assets_quintiles i.h_toilet i.water_unavail i.study_dist_group i.nearest_dist_group i.travelcost i.travel_time [pweight=ltfu_weights] || site:, distribution(weib) vce(robust) 

cap drop base0
predict base0, eta
roctab dead base0      // Base AUC 0.8090 (0.77982 to 0.83812) //
rocreg dead base0, probit breps(1000)  //bootsrap AUC 0.8101786(95% CI 0.779578  to 0.8407792) 


stcurve,cumhaz at1(rural_popdensity=0) at2(rural_popdensity=1)




//Senstivity analysis
mestreg i.wb_rural_urban logagemons i.sex  [pweight=ltfu_weights] || site:, distribution(weib) vce(robust) 
stcurve,cumhaz at1(wb_rural_urban=0) at2(wb_rural_urban=1) at3(wb_rural_urban=2)

mestreg i.wb_rural_urban logagemons i.sex i.malaria_rdt_adm i.abc i.hiv i.momsick i.educ_level i.worktype i.assets_quintiles i.h_toilet i.water_unavail i.study_dist_group i.nearest_dist_group i.travelcost i.travel_time [pweight=ltfu_weights] || site:, distribution(weib) vce(robust) 

stcurve,cumhaz at1(wb_rural_urban=0) at2(wb_rural_urban=1) at3(wb_rural_urban=2)





//Post-dsicharge mortality
use CHAIN_cohort.dta,clear

//Start by dropping inpatient deaths
drop if adm_dead==1
drop if date_disch==.
***Drop  Withdrew/LTFU before discharge with no follow-ups
drop if wltfu_type==1 |wltfu_type==3

gen adm_time= date_disch-date_adm 
replace date_last=date_last+0.5 if date_last==date_disch 


**************************************************************
***Sampling weights;these are final weights averaged from four sites (Kilifi, Migori, Banfora and Dhaka)
*************************************************************
gen nweight=1
replace nweight=0.39 if abc==1   //weights used are 25% for SAM, 16% for MAM and 59% NW
replace nweight=0.40 if abc==2

******************************************************************
***Add LTFU weights; 27 LTFU weights
******************************************************************
merge 1:1 record_id using CHAIN_ltfu_Post_discharge_weights.dta,nogen

*combine LTFU and sampling weights 
cap drop ltfu_sample_wt
gen ltfu_sample_wt = nweight*ltfu_weights


******************************************************************
*Set data for survival analysis
******************************************************************
cap stset,clear
stset date_last, id(record_id) failure(dead) origin(time date_disch) exit(time .) scale(30.44)


tab dead rural_popdensity,col chi
tab dead wb_rural_urban,col chi
strate,per(1000)
strate rural_popdensity,per(1000)
strate wb_rural_urban,per(1000)


**********************************************************
***Base model & plot the cumulitive hazard curve using the mixed effect multilevel model
**********************************************************
*Get log of age
gen logagemons=log(agemons)
gen logadm_time=log(adm_time)

//Factors assicoated rural vs urban
recode study_hosp_dist (0/4.999=0) (5/9.999999=1) (10/120=2),gen(study_dist_group)
label define ldist 0"<5km" 1"5 to 9km" 2">=10km"
label values study_dist_group ldist
recode nearest_hosp_dist (0/4.999=0) (5/9.999999=1) (10/120=2),gen(nearest_dist_group)
label values nearest_dist_group ldist
replace travel_time=2 if travel_time>2
recode mom_sick (0=1) (1=0),gen(momsick)
label define lmomsick 0"No" 1"Yes"
label values momsick lmomsi


mestreg i.rural_popdensity logagemons i.sex  [pweight=ltfu_weights] || site:, distribution(weib) vce(robust) 

mestreg  logagemons logadm_time i.sex i.malaria_rdt_adm i.abc i.hiv i.momsick i.educ_level i.worktype i.assets_quintiles i.h_toilet i.water_unavail i.study_dist_group i.nearest_dist_group i.travelcost i.travel_time [pweight=ltfu_weights] || site:, distribution(weib) vce(robust) 

cap drop base0
predict base0, eta
roctab dead base0      // Base AUC 0.8090 (0.77982 to 0.83812) //
rocreg dead base0, probit breps(1000)  //bootsrap AUC 0.8101786(95% CI 0.779578  to 0.8407792) 


stcurve,cumhaz at1(rural_popdensity=0) at2(rural_popdensity=1)


//Senstivity analysis
mestreg i.wb_rural_urban logagemons i.sex  [pweight=ltfu_weights] || site:, distribution(weib) vce(robust) 
stcurve,cumhaz at1(wb_rural_urban=0) at2(wb_rural_urban=1) at3(wb_rural_urban=2)

mestreg  logagemons logadm_time i.sex i.malaria_rdt_adm i.abc i.hiv i.momsick i.educ_level i.worktype i.assets_quintiles i.h_toilet i.water_unavail i.study_dist_group i.nearest_dist_group i.travelcost i.travel_time [pweight=ltfu_weights] || site:, distribution(weib) vce(robust) 

stcurve,cumhaz at1(wb_rural_urban=0) at2(wb_rural_urban=1) at3(wb_rural_urban=2)








********************************************
***Plot survival curves
*********************************************
//30-days 
use CHAIN_cohort.dta,clear
replace end_date=end_date+0.5 if end_date==date_adm

cap stset,clear
stset end_date, id(record_id) failure(dead) origin(time date_adm) exit(time .) 

stsplit d30, at(30.43)
tab d30 dead
drop if d30>0
replace dead=0 if dead==.

***Fig. 2B (the stcurve produces smooth curves and don't allow for numbers at risk)
sts graph,cumhaz  by(rural_popdensity) ylab(0(0.02)0.1) xlab(0(7)28) ytitle (Cumulative hazard) xtitle(Time from admission (days)) scheme(s1color) risktable legend(pos(1) bmargin(small)  ring(0) col(1)) 


gen nweight=1
replace nweight=0.39 if abc==1    //weights used are 25% for SAM, 16% for MAM and 59% NW
replace nweight=0.40 if abc==2

merge 1:1 record_id using CHAIN_ltfu_30days_weights.dta,nogen
*combine LTFU and sampling weights 
cap drop ltfu_sample_wt
gen ltfu_sample_wt = nweight*ltfu_weights

gen logagemons=log(agemons)


mestreg i.rural_popdensity logagemons i.sex  [pweight=ltfu_weights] || site:, distribution(weib) vce(robust) 
stcurve,cumhaz at1(rural_popdensity=0) at2(rural_popdensity=1) xlab(0 7 14 21 28 30) ylab(0(0.02)0.1)



sts graph,cumhaz  by(wb_rural_urban) ylab(0(0.02)0.15) xlab(0(7)28) ytitle (Cumulative hazard) xtitle(Time from admission (days)) scheme(s1color) risktable legend(pos(1) bmargin(small)  ring(0) col(1)) 


mestreg i.wb_rural_urban logagemons i.sex  [pweight=ltfu_weights] || site:, distribution(weib) vce(robust) 
stcurve,cumhaz at1(wb_rural_urban=0) at2(wb_rural_urban=1) at3(wb_rural_urban=2) xlab(0 7 14 21 28 30) ylab(0(0.02)0.1)





//Post-discharge curves
use CHAIN_cohort.dta,clear

//Start by dropping inpatient deaths
drop if adm_dead==1
drop if date_disch==.
***Drop  Withdrew/LTFU before discharge with no follow-ups
drop if wltfu_type==1 |wltfu_type==3

gen adm_time= date_disch-date_adm 
replace date_last=date_last+0.5 if date_last==date_disch 


**************************************************************
***Sampling weights;these are final weights averaged from four sites (Kilifi, Migori, Banfora and Dhaka)
*************************************************************
gen nweight=1
replace nweight=0.39 if abc==1   //weights used are 25% for SAM, 16% for MAM and 59% NW
replace nweight=0.40 if abc==2

******************************************************************
***Add LTFU weights; 27 LTFU weights
******************************************************************
merge 1:1 record_id using CHAIN_ltfu_Post_discharge_weights.dta,nogen

*combine LTFU and sampling weights 
cap drop ltfu_sample_wt
gen ltfu_sample_wt = nweight*ltfu_weights


******************************************************************
*Set data for survival analysis
******************************************************************
cap stset,clear
stset date_last, id(record_id) failure(dead) origin(time date_disch) exit(time .) 


gen logagemons=log(agemons)

***Fig. 2B (the stcurve produces smooth curves and don't allow for numbers at risk)
sts graph,cumhaz  by(rural_popdensity) ylab(0(0.02)0.1) xlab(0(30)180) ytitle (Cumulative hazard) xtitle(Time from discharge (days)) scheme(s1color) risktable legend(pos(1) bmargin(small)  ring(0) col(1)) 


mestreg i.rural_popdensity logagemons i.sex  [pweight=ltfu_weights] || site:, distribution(weib) vce(robust) 
stcurve,cumhaz at1(rural_popdensity=0) at2(rural_popdensity=1) xlab(0(30)180) ylab(0(0.02)0.1) 



sts graph,cumhaz  by(wb_rural_urban) ylab(0(0.02)0.1) xlab(0(30)180) ytitle (Cumulative hazard) xtitle(Time from discharge (days)) scheme(s1color) risktable legend(pos(1) bmargin(small)  ring(0) col(1)) 


mestreg i.wb_rural_urban logagemons i.sex  [pweight=ltfu_weights] || site:, distribution(weib) vce(robust) 
mestreg i.wb_rural_urban logagemons  i.sex i.malaria_rdt_adm i.abc i.hiv i.momsick i.educ_level i.worktype i.assets_quintiles i.h_toilet i.water_unavail i.study_dist_group i.nearest_dist_group i.travelcost i.travel_time [pweight=ltfu_weights] || site:, distribution(weib) vce(robust) 

stcurve,cumhaz at1(wb_rural_urban=0) at2(wb_rural_urban=1) at3(wb_rural_urban=2) xlab(0(30)180) ylab(0(0.02)0.1)
