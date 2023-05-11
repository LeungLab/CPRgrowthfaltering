#######################################################
# Code for GEMS growth faltering
#######################################################
rm(list=ls()) #this clears the workspace
graphics.off(); #close all graphics windows


library(tidyverse)
library(lubridate)
library(gridExtra)
library(pROC)
library(data.table)
library(fields)
library(zoo)
library(ks)
library(KernSmooth)
library(ranger)
library(viridis)
library(purrr)
library(broom)
library(profvis)
library(furrr)
library(mice)
library(glmnet)
library(glmnetUtils)
library(cvAUC) #this is a wrapper of AUC inside the ROCR package
library(table1)
library(data.table)
library(bit64)
library(pdp)

setwd('')

####################
#GEMS CASES
################### import GEMS data ####
#starting with full dataset
gems1_orig <- read.csv("", header=T)
#22567 observations, has type=Case/Control


gems1=gems1_orig %>% select(site,	type, f3_gender,	f3_drh_turgor	,	f3_drh_iv	,	f3_drh_hosp,
                            f4a_relationship,	f4a_dad_live	,					
                            f4a_prim_schl,	f4a_ppl_house	,	f4a_yng_children	,			
                            f4a_slp_rooms,	f4a_floor	,	f4a_house_elec	,  			
                            f4a_house_bike,	f4a_house_phone	,	f4a_house_tele	,    			
                            f4a_house_car	,	f4a_house_cart	,	f4a_house_scoot	,    			
                            f4a_house_fridge	,	f4a_house_agland	,	f4a_house_radio	,   			
                            f4a_house_boat	,	f4a_house_none	,	f4a_fuel_elec	,   			
                            f4a_fuel_biogas	,	f4a_fuel_grass	,	f4a_fuel_propane	,   			
                            f4a_fuel_coal	,	f4a_fuel_dung	,	f4a_fuel_natgas	, 			
                            f4a_fuel_charcoal	,	f4a_fuel_crop	,	f4a_fuel_kero	,   			
                            f4a_fuel_wood	,	f4a_fuel_other	,	f4a_ani_goat	,     			
                            f4a_ani_sheep	,	f4a_ani_dog	,	f4a_ani_cat	,      			
                            f4a_ani_cow	,	f4a_ani_rodents	,	f4a_ani_fowl	,       			
                            f4a_ani_other	,	f4a_ani_no	,	f4a_water_house	,    			
                            f4a_water_covwell	,	f4a_water_yard	,	f4a_water_covpwell	, 			
                            f4a_water_pubtap	,	f4a_water_prospring	,	f4a_water_well	,			
                            f4a_water_unspring	,	f4a_water_pubwell	,	f4a_water_river	,    			
                            f4a_water_pond	,	f4a_water_deepwell	,	f4a_water_rain	,   			
                            f4a_water_shallwell	,	f4a_water_bought	,	f4a_water_othr	,    			
                            f4a_water_bore	,	f4a_ms_water	,	f4a_fetch_water	,    			
                            f4a_trip_day	,	f4a_trip_week	,	f4a_water_avail	,   			
                            f4a_store_water	,	f4a_trt_water	,	f4a_trt_method	,   			
                            f4a_notrt_water	,	f4a_disp_feces	,    					
                            f4a_fac_waste	,	f4a_share_fac	,	f4a_wash_eat	,    			
                            f4a_wash_cook	,	f4a_wash_nurse	,	f4a_wash_def	,      			
                            f4a_wash_animal	,	f4a_wash_child	,	f4a_wash_othr	,      			
                            f4a_wash_use	,	f4a_breastfed	,	f4a_drh_days	,     			
                            f4a_max_stools	,	f4a_drh_blood	,	f4a_drh_vomit	,      			
                            f4a_drh_thirst	,	f4a_drh_lessdrink	,    					
                            f4a_drh_bellypain	,	f4a_drh_restless	,   					
                            f4a_drh_lethrgy	,	f4a_drh_consc	,	f4a_drh_strain	,  			
                            f4a_drh_prolapse	,	f4a_drh_cough	,    					
                            f4a_drh_conv	,	f4a_cur_thirsty	,	f4a_cur_skin	,    			
                            f4a_cur_restless	,	f4a_cur_drymouth	,   					
                            f4a_cur_fastbreath	,	f4a_hometrt_ors	,	f4a_hometrt_maize	,  			
                            f4a_hometrt_milk	,	f4a_hometrt_herb	,	f4a_hometrt_zinc	, 			
                            f4a_hometrt_none	,	f4a_hometrt_othrliq	,	f4a_hometrt_ab	,  			
                            f4a_hometrt_othr1	,	f4a_hometrt_othr2	,	f4a_offr_drink	,    			
                            f4a_seek_outside	,	f4a_seek_pharm	,	f4a_seek_friend	,    			
                            f4a_seek_healer	,	f4a_seek_doc	,	f4a_seek_privdoc	,   			
                            f4a_seek_remdy	,	f4a_seek_other	,	f4b_haz	,  			
                            f4b_muac	,	f4b_temp	,	f4b_resp	,           			
                            f4b_chest_indrw	,	f4b_eyes	,	f4b_mouth	,          			
                            f4b_skin	,	f4b_mental	,	f4b_rectal	,         			
                            f4b_bipedal	,	f4b_abn_hair	,	f4b_under_nutr	,     			
                            f4b_skin_flaky	,	f4b_observe_stool	,	f4b_nature_stool	,   			
                            f4b_recommend	,	f4b_volume	,  					
                            f4b_admit	,	f9_memory_aid	,	wealth_index	,       			
                            wiq	,	base_age	,	
                            f5_exp_drh	, 	f5_exp_dys	, 	f5_exp_cou	, 	f5_exp_fever	, 	
                            f5_diag_typ	, 	f5_diag_mal	, 	f5_diag_pne	,			
                            f5_exp_rectal	, 	f5_exp_convul	, 	f5_exp_arthritis	,			
                            f5_rectal	, 	f5_bipedal	, 	f5_abn_hair	, 	f5_under_nutr	, 	f5_skin_flaky,
                            f5_ms_water	, 	f5_main_cont	, 	f5_treat_water	, 	f5_trt_meth	, 	
                            f5_wash_where	, 	f5_wash_piped	, 	f5_wash_noptap	, 	f5_wash_tap	, 	f5_wash_basin,
                            f5_wash_soap	, 	f5_wash_ash	, 					
                            f5_child_feces	, 	f5_feces_visible	, 	f5_feces_else	, 	f5_house_feces,
                            f5_haz, f4b_outcome, f5_status, agegroup, f4b_height, f5_height,
                            f4b_date, f5_date, f4b_haz_f, f7_date, f7_haz, f7_haz_f, f7_height,
                            f5_diag_othr,f5_child_health,
                            f7_relation	,	f7_dad_live	,		
                            f7_prim_schl	,	f7_ppl_house	,	f7_yng_childrn	,
                            f7_slp_rooms	,	f7_floor	,	f7_house_elec	,  
                            f7_house_bike	,	f7_house_phone	,	f7_house_tele	,    
                            f7_house_car	,	f7_house_cart	,	f7_house_scoot	,    
                            f7_house_fridge	,	f7_house_agland	,	f7_house_radio	,   
                            f7_house_boat	,	f7_house_none	,	f7_fuel_elec	,   
                            f7_fuel_biogas	,	f7_fuel_grass	,	f7_fuel_propane	,   
                            f7_fuel_coal	,	f7_fuel_dung	,	f7_fuel_natgas	, 
                            f7_fuel_charcoal	,	f7_fuel_crop	,	f7_fuel_kero	,   
                            f7_fuel_wood	,	f7_fuel_other	,	f7_ani_goat	,     
                            f7_ani_sheep	,	f7_ani_dog	,	f7_ani_cat	,      
                            f7_ani_cow	,	f7_ani_rodents	,	f7_ani_fowl	,       
                            f7_ani_other	,	f7_ani_no	,	f7_water_house	,    
                            f7_water_covwell	,	f7_water_yard	,	f7_water_covpwell	, 
                            f7_water_pubtap	,	f7_water_prospring	,	f7_water_well	,
                            f7_water_unspring	,	f7_water_pubwell	,	f7_water_river	,    
                            f7_water_pond	,	f7_water_deepwell	,	f7_water_rain	,   
                            f7_water_shallwell	,	f7_water_bought	,	f7_water_othr	,    
                            f7_water_bore	,	f7_ms_water	,	f7_water_avail	,   
                            f7_store_water	,	f7_trt_water	,	f7_trt_method	,   
                            f7_disp_feces	,    				
                            f7_fac_waste	,	f7_share_fac	,	f7_wash_eat	,    
                            f7_wash_cook	,	f7_wash_nurse	,	f7_wash_def	,      
                            f7_wash_animal	,	f7_wash_child	,	f7_wash_othr	,      
                            f7_wash_use	,	f7_breastfed	,   		
                            f7_seekcare	,				
                            f7_height	,	f7_muac	,	f7_haz	,  
                            f7_temp	,	f7_resp	,   		
                            f7_bipedal	,	f7_abn_hair	,	f7_under_nutr	,     
                            f7_skin_flaky, caseid,
                            
                            f7_med_cotr,f7_med_gent,f7_med_chlor,
                            f7_med_eryth,f7_med_azith,f7_med_omacr,
                            f7_med_peni,f7_med_amoxy,f7_med_ampi,
                            f7_med_nalid,f7_med_cipro,f7_med_sele,
                            f7_med_otherant,
                            f11_antibiotic,
                            f11_anti_ampi,f11_anti_nali,f11_anti_cotr,
                            f11_anti_cipr,f11_anti_sele,f11_anti_gent,
                            f11_anti_chlo,f11_anti_eryt,f11_anti_azit,
                            f11_anti_macr,f11_anti_peni,f11_anti_amox,
                            f11_anti_other,f4a_hometrt_ab,f4a_seek_remdy,f4a_seek_remdy_spec,
                            f4b_trt_give_cxl,f4b_trt_give_gent,f4b_trt_give_chlor,
                            f4b_trt_give_ery,f4b_trt_give_azi,f4b_trt_give_macr,
                            f4b_trt_give_pen,f4b_trt_give_amox,f4b_trt_give_ampi,
                            f4b_trt_give_nalid,f4b_trt_give_cpnr,f4b_trt_give_slpy,
                            f4b_trt_give_othr,f4b_trt_pres_cxl,f4b_trt_pres_gent,
                            f4b_trt_pres_chlor,
                            f4b_trt_pres_ery,f4b_trt_pres_azi,f4b_trt_pres_macr,
                            f4b_trt_pres_pen,f4b_trt_pres_amox,f4b_trt_pres_ampi,
                            f4b_trt_pres_nalid,f4b_trt_pres_cpnr,f4b_trt_pres_slpy,
                            f4b_trt_pres_othr
                            
                            # f4b_whz, f4b_whz_f, f4b_med_whz, f4b_med_whz_f,
                            # f4b_find_whz, f4b_find_whz_f, f4b_find_med_whz, f4b_find_med_whz_f,
                            # f4b_rehyd_whz, f4b_rehyd_whz_f, f4b_rehyd_med_whz, f4b_rehyd_med_whz_f,
                            # f4b_out_whz, f4b_out_whz_f, f4b_out_med_whz, f4b_out_med_whz_f,
                            # f4b_last_whz, f4b_last_whz_f, f4b_last_med_whz, f4b_last_med_whz_f,
                            # f5_whz, f5_whz_f, f5_med_whz, f5_med_whz_f,
                            # f7_whz, f7_whz_f, f7_med_whz, f7_med_whz_f 
                            
)


#variables w/ small cell sizes, dropping for now: "f4a_chlorine","f4a_primcare","f4a_mom_live",
# "f4a_drh_undrink","f4a_drh_fever","f4a_drh_breath","f4b_skin_pinch",
# "f5_diag_meng"

#stuff from Ben
gems1=gems1 %>% mutate(any_breast_fed=factor(case_when((f4a_breastfed==1|f4a_breastfed==2)~1,TRUE~0))) #SMA dichotomizing breastfeeding
gems1=gems1 %>% mutate(any_breast_fed2=factor(case_when((f4a_breastfed==0|f4a_breastfed==1)~0,TRUE~1)))
gems1=gems1 %>% mutate(cont=case_when(site %in% c(1,2,3,4) ~ 1,
                                      TRUE ~ 2)) #SMA creating "cont"inent variable
gems1$site=as.factor(gems1$site)
gems1$index=1:dim(gems1)[1] #SMA creating an ID for each observation

################### define variables ####
cases_orig <- gems1 %>% filter(type=="Case")
#9439

cases_orig=cases_orig %>% mutate(haz_dif = f4b_haz - f5_haz)
# summary(cases$haz_dif)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# -9.1100 -0.0800  0.1800  0.2027  0.4800  8.3500    1051 
cases_orig=cases_orig %>% mutate(haz_1.0=(case_when(haz_dif>=1.0 ~ 1, TRUE~0)))
# # table(cases$haz_1.0)
# # 0    1 
# # 9270  169 
cases_orig=cases_orig %>% mutate(haz_0.5=(case_when(haz_dif>=0.5 ~ 1, TRUE~0)))
# # table(cases$haz_0.5)
# # 0    1 
# # 8907  532 
#see notebook for manual checks

cases_orig=cases_orig %>% mutate(change_ht = f5_height - f4b_height)
# summary(cases$change_ht)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# -25.033   0.800   1.633   1.772   2.500  41.833    1051 

# #f4b_outcome: 1=resolved; 2=improved; 3=no better; 4=worse; 5=died at hosp; 6=unknown/LTF
# #f5_child_health: 1=appears healthy; 2=improved but not back to normal; 3=no better; 4=worse; 5=died
#death_all: any death after hospital admit
cases_orig=cases_orig %>% mutate(death_all=(case_when(
                                    f4b_outcome==5 ~ 1,
                                    f5_child_health==5 ~ 1, 
                                    TRUE~0)),
                                 death_hosp=(case_when(
                                    f4b_outcome==5 ~ 1,
                                    TRUE~0)),
                                 death_home=(case_when(
                                    (f4b_outcome!=5 & f5_child_health==5) ~1,
                                    TRUE~0)))
# table(cases_orig$death_all)
# 0    1 
# 9249  190
# table(cases_orig$death_hosp)
# 0    1 
# 9390   49 
# table(cases_orig$death_home)
# 0    1 
# 9298  141 

cases_orig$f4b_date_date <- as.Date(as.character(cases_orig$f4b_date))
cases_orig$f5_date_date <- as.Date(as.character(cases_orig$f5_date))
cases_orig$fup_days <- as.numeric(cases_orig$f5_date_date - cases_orig$f4b_date_date)

#create variable where NA missing have been set to value 9
cases_orig=cases_orig %>% mutate(f4b_outcome_miss=replace_na(f4b_outcome,9),
                                 f5_child_health_miss=replace_na(f5_child_health,9))
#only a couple missing and one 9, set those to no(0)
cases_orig$f4a_drh_lethrgy <- as.numeric(cases_orig$f4a_drh_lethrgy)
cases_orig <- cases_orig %>% mutate(f4a_drh_lethrgy_miss=case_when((f4a_drh_lethrgy==9) ~ as.numeric(NA), TRUE~f4a_drh_lethrgy))
cases_orig$f4a_drh_restless <- as.numeric(cases_orig$f4a_drh_restless)
cases_orig <- cases_orig %>% mutate(f4a_drh_restless=case_when((f4a_drh_restless==9) ~ as.numeric(NA), TRUE~f4a_drh_restless))
cases_orig$f4a_drh_cough <- as.numeric(cases_orig$f4a_drh_cough)
cases_orig <- cases_orig %>% mutate(f4a_drh_cough_miss=case_when((f4a_drh_cough==9) ~ as.numeric(NA), TRUE~f4a_drh_cough))

#too few in 14(dam or earth pan), set to other(18)
#combine 9 and 10 (both are covered well)
cases_orig$f4a_ms_water <- as.numeric(cases_orig$f4a_ms_water)
cases_orig <- cases_orig %>% mutate(f4a_ms_water=case_when((f4a_ms_water==14)~18,(f4a_ms_water==9)~10,TRUE~f4a_ms_water))

#assuming f5_child_health==0 are also missing since aren't in codebook
cases_orig=cases_orig %>% mutate(f5_child_health_miss=(case_when(f5_child_health==0 ~ 9, TRUE~f5_child_health_miss)))# table(cases$f4b_outcome_miss)
# # 1    2    3    4    5    6    9 
# # 614 4179 4187   11   49  398    1
# table(is.na(cases$f4b_outcome))
# # FALSE  TRUE 
# # 9438     1
# table(cases$f5_child_health_miss)
# # 1    2    3    4    5    9
# # 6266  834  839  470  186  844
# table(is.na(cases$f5_child_health))
# # FALSE  TRUE 
# # 8604   835 

#very few 5 so put in other category
cases_orig$f4a_disp_feces <- as.numeric(cases_orig$f4a_disp_feces)
cases_orig=cases_orig %>% mutate(f4a_disp_feces=(case_when(f4a_disp_feces==5 ~ 6, TRUE~f4a_disp_feces)))

#combining categories. not sure what 0 is, isn't in dictionary
table(cases_orig$f4a_trt_method)
#    0    1    2    3    4    5    6    7 
# 6740    3  656  751 1197   73   14    5
cases_orig$f4a_trt_method <- as.numeric(cases_orig$f4a_trt_method)
cases_orig=cases_orig %>% mutate(f4a_trt_method=(case_when(f4a_trt_method==1 | f4a_trt_method==6 | f4a_trt_method==7 ~ 7, TRUE~f4a_trt_method)))
table(cases_orig$test)
# 0    2    3    4    5    7 
# 6740  656  751 1197   73   22 

#creating antibiotic variables - want one var for each of: any antibiotic at any time, b/f seek care, when present care
#stool collection antibiotic variables (can be from cases or controls)
abx_stool <- c("f11_antibiotic",
               "f11_anti_ampi","f11_anti_nali","f11_anti_cotr",
               "f11_anti_cipr","f11_anti_sele","f11_anti_gent",
               "f11_anti_chlo","f11_anti_eryt","f11_anti_azit",
               "f11_anti_macr","f11_anti_peni","f11_anti_amox",
               "f11_anti_other")
#cases b/f coming health center (non-medical questionnaire)
abx_cases_pre <- c("f4a_hometrt_ab","f4a_seek_remdy","f4a_seek_remdy_spec")
#cases given at health center
abx_cases_at <- c("f4b_trt_give_cxl","f4b_trt_give_gent","f4b_trt_give_chlor",
                  "f4b_trt_give_ery","f4b_trt_give_azi","f4b_trt_give_macr",
                  "f4b_trt_give_pen","f4b_trt_give_amox","f4b_trt_give_ampi",
                  "f4b_trt_give_nalid","f4b_trt_give_cpnr","f4b_trt_give_slpy",
                  "f4b_trt_give_othr")
#cases given at health center for tx at home
abx_cases_home <- c("f4b_trt_pres_cxl","f4b_trt_pres_gent","f4b_trt_pres_chlor",
                    "f4b_trt_pres_ery","f4b_trt_pres_azi","f4b_trt_pres_macr",
                    "f4b_trt_pres_pen","f4b_trt_pres_amox","f4b_trt_pres_ampi",
                    "f4b_trt_pres_nalid","f4b_trt_pres_cpnr","f4b_trt_pres_slpy",
                    "f4b_trt_pres_othr")

cases_orig$abx_bf <- cases_orig$f4a_hometrt_ab #if received abx before present for care
cases_orig$abx_at <- ifelse(rowSums(cases_orig[,abx_cases_at]==1)>0,1,0) #if received abx during care
cases_orig$abx_home <- ifelse(rowSums(cases_orig[,abx_cases_home]==1)>0,1,0) #if received abx rx to take home after care
#f4b_trt_pres_ampi missing for one person, assuming just a data entry error, set to 0
cases_orig$abx_home <- ifelse(is.na(cases_orig$abx_home),0,cases_orig$abx_home)
cases_orig$abx_ever <- ifelse(cases_orig$abx_bf==1 | cases_orig$abx_at==1 | cases_orig$abx_home==1,1,0)  

cases_orig <- cases_orig %>% mutate(month=month(f4b_date_date))

table(cases_orig$f4a_floor)
#    1    2    3    4    6    7    8    9   10 
# 2490  746    4    4   18  262 5813   89   13 
cases_orig=cases_orig %>% mutate(f4a_floor=(case_when((f4a_floor==1 | f4a_floor==2 |
                                                         f4a_floor==3 | f4a_floor==4 | f4a_floor==10) ~ 0, #natural, rudimentary, other floor
                                                      TRUE~1))) #finished floor
# table(cases_orig$test)
# # 0    1 
# # 3257 6182 

#combine to fewer categories: f4a_ms_water
table(cases_orig$f4a_ms_water)
#   1    2    3    4    5    6    7    8   10   11   12   13   15   16   17   18 
# 668  933 3481   72  272  109  972  703  127   58   56  374  529  658  336   91 
cases_orig=cases_orig %>% mutate(f4a_ms_water=(case_when((f4a_ms_water==6 | f4a_ms_water==13 | f4a_ms_water==14) ~ 0, #surface 
                                                 (f4a_ms_water==4 | f4a_ms_water==5 | f4a_ms_water==12 | f4a_ms_water==16) ~ 1, #unimproved
                                                 (f4a_ms_water==3 | f4a_ms_water==7 | f4a_ms_water==8 | f4a_ms_water==9 |
                                                    f4a_ms_water==10 | f4a_ms_water==11 | f4a_ms_water==15 | f4a_ms_water==17) ~ 2, #other improved
                                                 (f4a_ms_water==1 | f4a_ms_water==2) ~ 3, #piped
                                                 TRUE~4))) #other
# table(cases_orig$test)
# 0    1    2    3    4 
# 483 1058 6206 1601   91 
# #use JMP drinking water services ladder
# #surface (subset of unimproved)
# 6-pond/lake
# 13-river/stream
# 14-dam/earth pan
# #unimproved
# 4-open well in house/yard
# 5-open public well
# 12-unprotected spring
# 16-bought
# #other improved
# 3-public tap
# 7-deep tube well
# 8-shallow tube well
# 9-covered well in house/yard
# 10-covered public well
# 11-protected spring
# 15-rainwater
# 17-bore hole
# #safely managed/ piped into dwelling/plot/yard
# 1-piped into house
# 2-piped into yard
# #other
# 18-other

summary(cases_orig$f4a_relationship)
#    1    2    3    4    5    6    7    8    9   10 
# 8941   81   49   12  206    6  125    6    2   11 
#creating a combined category for non-father male relation OR non relation (4,6,8,9)
cases_orig$f4a_relationship <- as.numeric(cases_orig$f4a_relationship)
cases_orig=cases_orig %>% mutate(f4a_relationship=(case_when((f4a_relationship==4 | f4a_relationship==6 | f4a_relationship==8 | f4a_relationship==9) ~ 9, #non-father male relation OR 
                                                         TRUE~f4a_relationship))) #what was originally

#stunting at f-up, not growth faltering
cases_orig=cases_orig %>% mutate(post.stunt=(case_when(f5_haz<(-2) ~ 1, TRUE~0)))
#table(cases_orig$f5_haz,cases_orig$post.stunt)

#convert these to factors
vars <- c("f4a_ms_water","f4a_fac_waste","f4a_dad_live","f4b_recommend",
          "f4a_relationship","f4a_prim_schl","f4a_floor","f4a_disp_feces",
          "f4a_wash_use","f4a_water_avail","f4a_trt_method","f4a_drh_blood",
          "f4a_drh_vomit","f4a_drh_thirst","f4a_drh_lessdrink","f4a_drh_bellypain",
          "f4a_drh_restless","f4a_drh_lethrgy_miss","f4a_drh_consc","f4a_drh_strain",
          "f4a_drh_prolapse","f4a_drh_cough_miss","f4a_drh_conv","f4a_cur_thirsty",
          "f4a_cur_skin","f4a_cur_restless","f4a_cur_drymouth","f4a_cur_fastbreath",
          "f4b_nature_stool","month"
)
cases_orig[vars] <- lapply(cases_orig[vars], factor)

#collected as categorical, but ordinal so leaving as numeric for now: 
#f4a_prim_schl, f4a_offr_drink, f4a_max_stools, f4a_breastfed, f4b_mouth, f4b_skin, f4b_mental


################### inclusion/exclusion for growth faltering ####
#cases_orig n=9439

#from Brader_2019:
# Children presenting with prolonged (> 7 days' duration) and 
# persistent (> 14days' duration) diarrhea were excluded
# >>> per Ben 4/8/2020 this already taken care of (prolonged and persistent diarrhea kiddos not in dataset)
# We also excluded children with implausible length/LAZ values
# (LAZ > 6 or < - 6 and change in (delta) LAZ>3;
# a length gain of > 8 cm for follow-up periods 49-60 days and
# > 10 cm for periods 61-91 days among infants <= 6 months,
# a length gain of > 4 cm for follow-up periods 49-60 days and
# > 6 cm for periods 61-91 days among children > 6 months, or
# length values that were > 1.5 cm lower at follow-up than at enrollment.

#keep based on Brader HAZ plausability
cases_orig$keep <- ifelse((cases_orig$base_age<=6 & cases_orig$fup_days>=49 & cases_orig$fup_days<=60 & cases_orig$change_ht<=8),1,
                    ifelse((cases_orig$base_age<=6 & cases_orig$fup_days>=61 & cases_orig$fup_days<=91 & cases_orig$change_ht<=10),1,
                    ifelse((cases_orig$base_age>6 & cases_orig$fup_days>=49 & cases_orig$fup_days<=60 & cases_orig$change_ht<=4),1,
                    ifelse((cases_orig$base_age>6 & cases_orig$fup_days>=61 & cases_orig$fup_days<=91 & cases_orig$change_ht<=6),1,
                    0))))
table(cases_orig$keep)
# 0    1 
# 215 8207 
#change_ht and f4b_haz_f missing for those w/o fup so no 0's listed, are all missing

cases_gf <- cases_orig %>% filter((fup_days>=49 & fup_days<=91)& #n=9329; f-up time period criteria
         (f4b_haz_f==0)& #n=9287; f4b_haz_f: Flag for _ZLEN<-6 or _ZLEN>6
         (abs(haz_dif)<=3.0)& #n=8255
         (keep==1)& #n=8166
         (change_ht>=-1.5)& #8055
         (f5_status==1)& #n=8053; (==1 60d f-up conducted, 0==not conducted)
         (!is.na(haz_dif))) #n=8053

#cases_gf is now those who meet HAZ plausability and have follow-up HAZ measurement
# table(cases_gf$f4b_outcome_miss,is.na(cases_gf$f5_haz),cases_gf$death_all)
# FALSE
# 1   575
# 2  3566
# 3  3592
# 4     4
# 6   316
#316 kids were LTF in hosp but still have fup HAZ and aren't dead so keep in growth falter analysis



################### drop missing since can't have missing in RF, define "names" variables interested in ####
complete_gf <- cases_gf %>% filter(!is.na(f4a_dad_live)&!is.na(f4a_prim_schl)&!is.na(f4a_slp_rooms)&!is.na(f4a_water_avail)&
                                     !is.na(f4a_disp_feces)&!is.na(f4a_share_fac)&!is.na(f4a_wash_use)&!is.na(f4a_drh_days)&
                                     !is.na(f4a_drh_thirst)&!is.na(f4a_drh_restless)&!is.na(f4a_drh_lethrgy_miss)&!is.na(f4a_drh_conv)&
                                     !is.na(f4a_cur_skin)&!is.na(f4a_cur_restless)&!is.na(f4a_cur_drymouth)&!is.na(f4a_cur_fastbreath)&
                                     !is.na(f4a_offr_drink)&!is.na(f4b_temp)&!is.na(f4b_chest_indrw)&!is.na(f4b_mouth)&
                                     !is.na(f4b_skin)&!is.na(f4b_under_nutr)&!is.na(f4b_nature_stool)&!is.na(f4a_ppl_house)&
                                     !is.na(f4b_resp)&!is.na(f4b_abn_hair)&!is.na(f3_drh_iv)&!is.na(f4a_store_water)&
                                     !is.na(f4b_mental)&(f4a_drh_vomit!="9")&!is.na(f4a_drh_consc)&(f4a_drh_consc!="9")&
                                     !is.na(f4a_drh_cough_miss))
#8053 to 7639 observations



# select variables we're interested in. have checked all appropriate ones are factor
names <- c("site","f3_gender","f3_drh_turgor","f3_drh_iv","f3_drh_hosp",
           "f4a_relationship","f4a_dad_live",
           "f4a_prim_schl","f4a_ppl_house","f4a_yng_children",
           "f4a_slp_rooms","f4a_floor","f4a_house_elec",  
           "f4a_house_bike","f4a_house_phone","f4a_house_tele",    
           "f4a_house_car","f4a_house_cart","f4a_house_scoot",    
           "f4a_house_fridge","f4a_house_agland","f4a_house_radio",   
           "f4a_house_boat","f4a_house_none","f4a_fuel_elec",   
           "f4a_fuel_biogas","f4a_fuel_grass","f4a_fuel_propane",   
           "f4a_fuel_coal","f4a_fuel_dung","f4a_fuel_natgas", 
           "f4a_fuel_charcoal","f4a_fuel_crop","f4a_fuel_kero",   
           "f4a_fuel_wood","f4a_fuel_other","f4a_ani_goat",     
           "f4a_ani_sheep","f4a_ani_dog","f4a_ani_cat",      
           "f4a_ani_cow","f4a_ani_rodents","f4a_ani_fowl",       
           "f4a_ani_other","f4a_ani_no","f4a_water_house",    
           "f4a_water_covwell","f4a_water_yard","f4a_water_covpwell", 
           "f4a_water_pubtap","f4a_water_prospring","f4a_water_well",
           "f4a_water_unspring","f4a_water_pubwell","f4a_water_river",    
           "f4a_water_pond","f4a_water_deepwell","f4a_water_rain",   
           "f4a_water_shallwell","f4a_water_bought","f4a_water_othr",    
           "f4a_water_bore","f4a_ms_water","f4a_water_avail",   
           "f4a_store_water","f4a_trt_water","f4a_trt_method",   
           "f4a_disp_feces",    
           "f4a_fac_waste","f4a_share_fac","f4a_wash_eat",    
           "f4a_wash_cook","f4a_wash_nurse","f4a_wash_def",      
           "f4a_wash_animal","f4a_wash_child","f4a_wash_othr",      
           "f4a_wash_use","f4a_breastfed","f4a_drh_days",     
           "f4a_max_stools","f4a_drh_blood","f4a_drh_vomit",      
           "f4a_drh_thirst","f4a_drh_lessdrink",    
           "f4a_drh_bellypain","f4a_drh_restless",   
           "f4a_drh_lethrgy_miss","f4a_drh_consc","f4a_drh_strain",  
           "f4a_drh_prolapse","f4a_drh_cough_miss",    
           "f4a_drh_conv","f4a_cur_thirsty","f4a_cur_skin",    
           "f4a_cur_restless","f4a_cur_drymouth",   
           "f4a_cur_fastbreath","f4a_hometrt_ors","f4a_hometrt_maize",  
           "f4a_hometrt_milk","f4a_hometrt_herb","f4a_hometrt_zinc", 
           "f4a_hometrt_none","f4a_hometrt_othrliq","f4a_hometrt_ab",  
           "f4a_hometrt_othr1","f4a_hometrt_othr2","f4a_offr_drink",    
           "f4a_seek_outside","f4a_seek_pharm","f4a_seek_friend",    
           "f4a_seek_healer","f4a_seek_doc","f4a_seek_privdoc",   
           "f4a_seek_remdy","f4a_seek_other","f4b_haz",  
           #"f4b_muac",
           "f4b_temp","f4b_resp",           
           "f4b_chest_indrw","f4b_eyes","f4b_mouth",          
           "f4b_skin","f4b_mental","f4b_rectal",         
           "f4b_bipedal","f4b_abn_hair","f4b_under_nutr",     
           "f4b_skin_flaky","f4b_observe_stool","f4b_nature_stool",   
           "f4b_recommend",  
           "f4b_admit", 
           #"wealth_index", 
           "base_age"
           #f5_ variables are at f-up, don't have to inform prediction at hosp admit; haven't checked if any of these f5_ should be factors
           # 'f5_exp_drh', 'f5_exp_dys', 'f5_exp_cou', 'f5_exp_fever', 
           # 'f5_diag_typ', 'f5_diag_mal', 'f5_diag_pne',
           # 'f5_exp_rectal', 'f5_exp_convul', 'f5_exp_arthritis',
           # 'f5_rectal', 'f5_bipedal', 'f5_abn_hair', 'f5_under_nutr', 'f5_skin_flaky',
           # 'f5_ms_water_factor', 'f5_main_cont', 'f5_treat_water', 'f5_trt_meth', 
           # 'f5_wash_where', 'f5_wash_noptap', 'f5_wash_tap', 
           # 'f5_wash_basin', 'f5_wash_soap', 'f5_wash_ash', 
           # 'f5_child_feces_factor', 'f5_feces_visible', 'f5_house_feces_factor',
)

#table(cases$f5_child_health_miss,is.na(cases$f5_skin_flaky))
#f4a_share_fac leads to some being dropped, keep considering for now
#variables w/ small cell sizes, dropping for now: "f4a_chlorine","f4a_primcare","f4a_mom_live",
# "f4a_drh_undrink","f4a_drh_fever","f4a_drh_breath","f4b_skin_pinch",
# "f5_diag_meng","f5_wash_piped","f4a_fetch_water","f4a_trip_day","f4a_trip_week",
# f4a_notrt_water, f4b_volume, 
#these lead to lots being dropped >> are physical exam at f-up, not useful for predicting what happens at enrollment, so not analyze anymore
#f5_rectal
#f5_bipedal
#f5_abn_hair
#f5_under_nutr
#f5_skin_flaky

#checking all appropriates ones are factorized
# temp<-temp_gf[names]
# str(temp)
# summary(temp)

################### cases into age groups, continents for growth faltering ####
table(complete_gf$agegroup)
# 1    2    3 
# 3279 2729 1877 
summary(complete_gf$agegroup)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   1.000   2.000   1.822   2.000   3.000
#table(cases$base_age,cases$agegroup)
#agegroup 1=0-11mo, 2=12-23mo, 3=24-50mo
#summary(cases$base_age)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00    8.00   13.00   17.01   23.00   59.00 
cases_gf_age1 <- complete_gf %>% filter(agegroup == 1)
cases_gf_age2 <- complete_gf %>% filter(agegroup == 2)
cases_gf_age3 <- complete_gf %>% filter(agegroup == 3)
cases_gf_age4 <- complete_gf %>% filter(agegroup == 1 | agegroup==2)

#agegroup 1=0-11mo, 2=12-23mo, 3=24-50mo
#3306+2606=5912

cases_gf_Gambia <- complete_gf %>% filter(site == 1)
cases_gf_Mali <- complete_gf %>% filter(site == 2)
cases_gf_Mozam <- complete_gf %>% filter(site == 3)
cases_gf_Kenya <- complete_gf %>% filter(site == 4)
cases_gf_India <- complete_gf %>% filter(site == 5)
cases_gf_Bang <- complete_gf %>% filter(site == 6)
cases_gf_Pak <- complete_gf %>% filter(site == 7)

cases_gf_Afr <- complete_gf %>% filter(site==1 | site==2 | site==3 | site==4)
cases_gf_Asia <- complete_gf %>% filter(site==5 | site==6 | site==7)

cases_gf_notStunt <- complete_gf %>% filter(f4b_haz >=-2)

################### merge in etiology info ####
complete_gf_afe<-complete_gf
#drop the extra 1 at the end of each caseid
complete_gf_afe$caseid <- as.numeric(substr(complete_gf_afe$caseid,1,9))
#dplyr version of substr: https://stringr.tidyverse.org/reference/str_sub.html
AFes <- read.csv("/GEMS with AFes.csv", header=T)
#is no data here for controls, so just drop those
AFes <- AFes %>% filter(case.control==1)
complete_gf_afe <- complete_gf_afe %>% left_join(AFes, by=c("caseid"="Case.ID")) %>%
  filter(!is.na(shigella_eiec_afe) & !is.na(cryptosporidium_afe)) %>%
  mutate(shigella_afe=(case_when(shigella_eiec_afe>=0.5 ~ 1, TRUE~0)),
         crypto_afe=(case_when(cryptosporidium_afe>=0.5 ~ 1, TRUE~0)),
         viral=(case_when((astrovirus_afe>=0.5 | norovirus_gii_afe>=0.5 |
                             rotavirus_afe>=0.5 | sapovirus_afe>=0.5 |
                             adenovirus_40_41_afe>=0.5)~ 1, TRUE~0))) %>%
  mutate(shigella_afe_0.3=(case_when(shigella_eiec_afe>=0.3 ~ 1, TRUE~0)),
         crypto_afe_0.3=(case_when(cryptosporidium_afe>=0.3 ~ 1, TRUE~0)),
         viral_0.3=(case_when((astrovirus_afe>=0.3 | norovirus_gii_afe>=0.3 |
                             rotavirus_afe>=0.3 | sapovirus_afe>=0.3 |
                             adenovirus_40_41_afe>=0.3)~ 1, TRUE~0))) %>%
  mutate(shigella_afe_0.7=(case_when(shigella_eiec_afe>=0.7 ~ 1, TRUE~0)),
         crypto_afe_0.7=(case_when(cryptosporidium_afe>=0.7 ~ 1, TRUE~0)),
         viral_0.7=(case_when((astrovirus_afe>=0.7 | norovirus_gii_afe>=0.7 |
                                 rotavirus_afe>=0.7 | sapovirus_afe>=0.7 |
                                 adenovirus_40_41_afe>=0.7)~ 1, TRUE~0)))
  
#table(test2$shigella_eiec_afe,test2$shigella_afe)
#table(test$viral)
#cbind(test$viral,test$astrovirus_afe,test$norovirus_gii_afe,test$rotavirus_afe,test$sapovirus_afe,test$adenovirus_40_41_afe)


################### descriptive for pub ####
table(complete_gf$haz_0.5)
# 0    1 
# 5895 1744 
table(complete_gf$haz_1.0)
# 0    1 
# 7282  357 
1744/7639 #0.2283021
357/7639 #0.04673387

table(complete_gf$agegroup)
# 1    2    3 
# 3196 2622 1821 
table(complete_gf$haz_0.5,complete_gf$agegroup)
# 1    2    3
# 0 2119 2038 1738
# 1 1077  584   83
table(complete_gf$haz_1.0,complete_gf$agegroup)
# 1    2    3
# 0 2903 2558 1821
# 1  293   64    0

table(complete_gf$site)
# 1    2    3    4    5    6    7 
# 788 1715  420 1042 1457 1316  901 
table(complete_gf$haz_0.5,complete_gf$site)
# 1    2    3    4    5    6    7
# 0  537 1460  275  746 1188 1012  677
# 1  251  255  145  296  269  304  224
table(complete_gf$haz_1.0,complete_gf$site)
# 1    2    3    4    5    6    7
# 0  740 1664  378  966 1402 1278  854
# 1   48   51   42   76   55   38   47

#age dist of outcome
temp <- complete_gf %>% filter(haz_0.5==1)
hist(temp$base_age, main="Diarrhea cases who growth falter (difHAZ0.5")
hist(temp$f4b_haz, main="Baseline HAZ of cases who growth falter (difHAZ0.5")

#GROWTH FALTERING
#overlapping histogram of baseline HAZ among all
p1 <- hist(complete_gf[which(complete_gf$haz_0.5==0),]$f4b_haz)
p2 <- hist(complete_gf[which(complete_gf$haz_0.5==1),]$f4b_haz)
#jpeg("/overlap_hist.jpg",width=600,height=480,quality=400)
#png("/overlap_hist.png",units="px",width=3000,height=2400,res=300)
plot( p1, col=rgb(1,0,0,1/4), xlim=c(-6,6),ylim=c(0,2000),xlab="baseline HAZ",main="Growth Faltering in GEMS by Baseline HAZ")
plot( p2, col=rgb(0,0,1,1/4),  xlim=c(-6,6),ylim=c(0,2000), add=T)
legend('topright',c('no growth falter','growth falter'),fill = c(rgb(1,0,0,1/4),rgb(0,0,1,1/4)), bty = 'n')
dev.off()

#median baseline HAZ by growth falter status
summary((complete_gf[which(complete_gf$haz_0.5==0),])$f4b_haz)
summary((complete_gf[which(complete_gf$haz_0.5==1),])$f4b_haz)

#median baseline HAZ by site
table(complete_gf$site)
table(complete_gf$haz_0.5,complete_gf$site)
table(complete_gf$haz_1.0,complete_gf$site)
summary(cases_gf_Gambia$f4b_haz)
summary(cases_gf_Mali$f4b_haz)
summary(cases_gf_Mozam$f4b_haz)
summary(cases_gf_Kenya$f4b_haz)
summary(cases_gf_India$f4b_haz)
summary(cases_gf_Bang$f4b_haz)
summary(cases_gf_Pak$f4b_haz)

p1 <- hist(complete_gf[which(complete_gf$haz_0.5==0 & complete_gf$site==1),]$f4b_haz)
p2 <- hist(complete_gf[which(complete_gf$haz_0.5==1 & complete_gf$site==1),]$f4b_haz)
plot( p1, col=rgb(1,0,0,1/4), xlim=c(-6,6),ylim=c(0,150),xlab="baseline HAZ",main="Growth faltering by baseline HAZ \n in the Gambia")
plot( p2, col=rgb(0,0,1,1/4),  xlim=c(-6,6),ylim=c(0,150), add=T)
legend('topright',c('no growth falter','growth falter'),fill = c(rgb(1,0,0,1/4),rgb(0,0,1,1/4)), bty = 'n')
summary((complete_gf[which(complete_gf$haz_0.5==0 & complete_gf$site==1),])$f4b_haz)
summary((complete_gf[which(complete_gf$haz_0.5==1 & complete_gf$site==1),])$f4b_haz)

p1 <- hist(complete_gf[which(complete_gf$haz_0.5==0 & complete_gf$site==2),]$f4b_haz)
p2 <- hist(complete_gf[which(complete_gf$haz_0.5==1 & complete_gf$site==2),]$f4b_haz)
plot( p1, col=rgb(1,0,0,1/4), xlim=c(-6,6),ylim=c(0,500),xlab="baseline HAZ",main="Growth faltering by baseline HAZ \n in Mali")
plot( p2, col=rgb(0,0,1,1/4),  xlim=c(-6,6),ylim=c(0,500), add=T)
legend('topright',c('no growth falter','growth falter'),fill = c(rgb(1,0,0,1/4),rgb(0,0,1,1/4)), bty = 'n')
summary((complete_gf[which(complete_gf$haz_0.5==0 & complete_gf$site==2),])$f4b_haz)
summary((complete_gf[which(complete_gf$haz_0.5==1 & complete_gf$site==2),])$f4b_haz)

p1 <- hist(complete_gf[which(complete_gf$haz_0.5==0 & complete_gf$site==3),]$f4b_haz)
p2 <- hist(complete_gf[which(complete_gf$haz_0.5==1 & complete_gf$site==3),]$f4b_haz)
plot( p1, col=rgb(1,0,0,1/4), xlim=c(-6,6),ylim=c(0,150),xlab="baseline HAZ",main="Growth faltering by baseline HAZ \n in Mozambique")
plot( p2, col=rgb(0,0,1,1/4),  xlim=c(-6,6),ylim=c(0,150), add=T)
legend('topright',c('no growth falter','growth falter'),fill = c(rgb(1,0,0,1/4),rgb(0,0,1,1/4)), bty = 'n')
summary((complete_gf[which(complete_gf$haz_0.5==0 & complete_gf$site==3),])$f4b_haz)
summary((complete_gf[which(complete_gf$haz_0.5==1 & complete_gf$site==3),])$f4b_haz)

p1 <- hist(complete_gf[which(complete_gf$haz_0.5==0 & complete_gf$site==4),]$f4b_haz)
p2 <- hist(complete_gf[which(complete_gf$haz_0.5==1 & complete_gf$site==4),]$f4b_haz)
plot( p1, col=rgb(1,0,0,1/4), xlim=c(-6,6),ylim=c(0,150),xlab="baseline HAZ",main="Growth faltering by baseline HAZ \n in Kenya")
plot( p2, col=rgb(0,0,1,1/4),  xlim=c(-6,6),ylim=c(0,150), add=T)
legend('topright',c('no growth falter','growth falter'),fill = c(rgb(1,0,0,1/4),rgb(0,0,1,1/4)), bty = 'n')
summary((complete_gf[which(complete_gf$haz_0.5==0 & complete_gf$site==4),])$f4b_haz)
summary((complete_gf[which(complete_gf$haz_0.5==1 & complete_gf$site==4),])$f4b_haz)

p1 <- hist(complete_gf[which(complete_gf$haz_0.5==0 & complete_gf$site==5),]$f4b_haz)
p2 <- hist(complete_gf[which(complete_gf$haz_0.5==1 & complete_gf$site==5),]$f4b_haz)
plot( p1, col=rgb(1,0,0,1/4), xlim=c(-6,6),ylim=c(0,300),xlab="baseline HAZ",main="Growth faltering by baseline HAZ \n in India")
plot( p2, col=rgb(0,0,1,1/4),  xlim=c(-6,6),ylim=c(0,300), add=T)
legend('topright',c('no growth falter','growth falter'),fill = c(rgb(1,0,0,1/4),rgb(0,0,1,1/4)), bty = 'n')
summary((complete_gf[which(complete_gf$haz_0.5==0 & complete_gf$site==5),])$f4b_haz)
summary((complete_gf[which(complete_gf$haz_0.5==1 & complete_gf$site==5),])$f4b_haz)

p1 <- hist(complete_gf[which(complete_gf$haz_0.5==0 & complete_gf$site==6),]$f4b_haz)
p2 <- hist(complete_gf[which(complete_gf$haz_0.5==1 & complete_gf$site==6),]$f4b_haz)
plot( p1, col=rgb(1,0,0,1/4), xlim=c(-6,6),ylim=c(0,300),xlab="baseline HAZ",main="Growth faltering by baseline HAZ \n in Bangladesh")
plot( p2, col=rgb(0,0,1,1/4),  xlim=c(-6,6),ylim=c(0,300), add=T)
legend('topright',c('no growth falter','growth falter'),fill = c(rgb(1,0,0,1/4),rgb(0,0,1,1/4)), bty = 'n')
summary((complete_gf[which(complete_gf$haz_0.5==0 & complete_gf$site==6),])$f4b_haz)
summary((complete_gf[which(complete_gf$haz_0.5==1 & complete_gf$site==6),])$f4b_haz)

p1 <- hist(complete_gf[which(complete_gf$haz_0.5==0 & complete_gf$site==7),]$f4b_haz)
p2 <- hist(complete_gf[which(complete_gf$haz_0.5==1 & complete_gf$site==7),]$f4b_haz)
plot( p1, col=rgb(1,0,0,1/4), xlim=c(-6,6),ylim=c(0,150),xlab="baseline HAZ",main="Growth faltering by baseline HAZ \n in Pakistan")
plot( p2, col=rgb(0,0,1,1/4),  xlim=c(-6,6),ylim=c(0,150), add=T)
legend('topright',c('no growth falter','growth falter'),fill = c(rgb(1,0,0,1/4),rgb(0,0,1,1/4)), bty = 'n')
summary((complete_gf[which(complete_gf$haz_0.5==0 & complete_gf$site==7),])$f4b_haz)
summary((complete_gf[which(complete_gf$haz_0.5==1 & complete_gf$site==7),])$f4b_haz)


#median baseline HAZ by age
table(complete_gf$agegroup)
table(complete_gf$haz_0.5,complete_gf$agegroup)
table(complete_gf$haz_1.0,complete_gf$agegroup)

p1 <- hist(cases_gf_age1[which(cases_gf_age1$haz_0.5==0 & cases_gf_age1$site==1),]$f4b_haz)
p2 <- hist(cases_gf_age1[which(cases_gf_age1$haz_0.5==1 & cases_gf_age1$site==1),]$f4b_haz)
plot( p1, col=rgb(1,0,0,1/4), xlim=c(-6,6),ylim=c(0,80),xlab="baseline HAZ",main="Growth faltering by baseline HAZ \n in 0-11mo")
plot( p2, col=rgb(0,0,1,1/4),  xlim=c(-6,6),ylim=c(0,80), add=T)
legend('topright',c('no growth falter','growth falter'),fill = c(rgb(1,0,0,1/4),rgb(0,0,1,1/4)), bty = 'n')
summary((cases_gf_age1[which(cases_gf_age1$haz_0.5==0 & cases_gf_age1$site==1),])$f4b_haz)
summary((cases_gf_age1[which(cases_gf_age1$haz_0.5==1 & cases_gf_age1$site==1),])$f4b_haz)

p1 <- hist(cases_gf_age2[which(cases_gf_age2$haz_0.5==0 & cases_gf_age2$site==1),]$f4b_haz)
p2 <- hist(cases_gf_age2[which(cases_gf_age2$haz_0.5==1 & cases_gf_age2$site==1),]$f4b_haz)
plot( p1, col=rgb(1,0,0,1/4), xlim=c(-6,6),ylim=c(0,80),xlab="baseline HAZ",main="Growth faltering by baseline HAZ \n in 12-23mo")
plot( p2, col=rgb(0,0,1,1/4),  xlim=c(-6,6),ylim=c(0,80), add=T)
legend('topright',c('no growth falter','growth falter'),fill = c(rgb(1,0,0,1/4),rgb(0,0,1,1/4)), bty = 'n')
summary((cases_gf_age2[which(cases_gf_age2$haz_0.5==0 & cases_gf_age2$site==1),])$f4b_haz)
summary((cases_gf_age2[which(cases_gf_age2$haz_0.5==1 & cases_gf_age2$site==1),])$f4b_haz)

p1 <- hist(cases_gf_age3[which(cases_gf_age3$haz_0.5==0 & cases_gf_age3$site==1),]$f4b_haz)
p2 <- hist(cases_gf_age3[which(cases_gf_age3$haz_0.5==1 & cases_gf_age3$site==1),]$f4b_haz)
plot( p1, col=rgb(1,0,0,1/4), xlim=c(-6,6),ylim=c(0,30),xlab="baseline HAZ",main="Growth faltering by baseline HAZ \n in 24-59mo")
plot( p2, col=rgb(0,0,1,1/4),  xlim=c(-6,6),ylim=c(0,30), add=T)
legend('topright',c('no growth falter','growth falter'),fill = c(rgb(1,0,0,1/4),rgb(0,0,1,1/4)), bty = 'n')
summary((cases_gf_age3[which(cases_gf_age3$haz_0.5==0 & cases_gf_age3$site==1),])$f4b_haz)
summary((cases_gf_age3[which(cases_gf_age3$haz_0.5==1 & cases_gf_age3$site==1),])$f4b_haz)


#dates 
options(max.print=1500)
table(gems1_orig$enrolldate) #Dec 2007 - Mar 2011


################### FUNCTion for var screening, regression fitting, AUC calc ####
CPR.funct <- function(data,outcome,iter,nvars_opts){
  out=ranger(as.formula(paste(outcome,'~',paste(names,collapse="+"),sep="")),data=data,num.trees=1000,importance="impurity")
  imps=importance(out)
  df_imps_full=data.frame(names=names(imps),var_red=as.numeric(imps)) %>% arrange(desc(var_red))


  result=data.frame(iter=NA,nvar=NA,true=NA,pred_glm=NA,pred_RF=NA)
  test_record <- NA
  train_record <- NA

  for (each in 1:iter){
    print(each)
    train=data %>% sample_frac(.80,replace=F)

    test=data[-which(data$index %in% train$index),]

    train_record <- c(train_record,table(train[,outcome])[["1"]]) #data$haz_0.5 is equivalent to data[,"haz_0.5"]
    test_record <- c(test_record,table(test[,outcome])[["1"]])


    out=ranger(as.formula(paste(outcome,'~',paste(names,collapse="+"),sep="")),data=train,num.trees=1000,importance="impurity")
    df_imps=data.frame(names=names(ranger::importance(out)),imps=ranger::importance(out)) %>% arrange(desc(imps))
    for (nvars in nvars_opts){

      print(nvars)
      out1=glm(as.formula(paste(outcome,'~',paste(df_imps$names[1:nvars],collapse="+"),sep="")),data=train,family="binomial",control=glm.control(maxit=50))
      out2=ranger(as.formula(paste(outcome,'~',paste(df_imps$names[1:nvars],collapse="+"),sep="")),data=train,num.trees=1000)

      df=data.frame(iter=each,nvar=nvars,true=test[[outcome]],pred_glm=as.numeric(predict(out1,newdata=test,type="response")),pred_RF=as.numeric(predict(out2,data=test,type="response")$predictions))
      result=rbind(result,df)
    }
  }
  result<-result[-1,]
  train_record<-train_record[-1]
  test_record<-test_record[-1]

  AUCs<-result %>% split(.$nvar) %>% purrr::map(~ci.cvAUC(.$pred_glm,.$true,folds=.$iter))
  AUCs2<-result %>% split(.$nvar) %>% purrr::map(~ci.cvAUC(.$pred_RF,.$true,folds=.$iter))

  AUC_df<-rbind(bind_rows(AUCs %>% purrr::map(~data.frame(AUC=.$cvAUC,SE=.$se,lower=.$ci[1],upper=.$ci[2],level=.$confidence,Model="LR"))),
                bind_rows(AUCs2 %>% purrr::map(~data.frame(AUC=.$cvAUC,SE=.$se,lower=.$ci[1],upper=.$ci[2],level=.$confidence,Model="RF"))))
  AUC_df$nvar<-rep(nvars_opts,2)
  AUC_df


  calib_fits=data.frame(nvar=NA,iter=NA,intc=NA,intc_LCI=NA,intc_UCI=NA,slope=NA,slope_LCI=NA,slope_UCI=NA)
  for (nvars in nvars_opts){
    for (each in 1:iter){
      data.temp <- result %>% filter(nvar==nvars & iter==each)

      intercept <- glm(true~1,offset=log(pred_glm/(1-pred_glm)),family="binomial",data=data.temp)
      #summary(intercept) #Should have intercept of 0. intercept is calibration intercept
      slope <- glm(true~log(pred_glm/(1-pred_glm)),family="binomial",data=data.temp)
      #summary(slope) #Should have slope of 1. beta coefficient = slope=calibration slope

      df=data.frame(nvar=nvars,iter=each,
                    intc=coef(intercept),intc_LCI=confint(intercept)[1],intc_UCI=confint(intercept)[2],
                    slope=coef(slope)[2],slope_LCI=confint(slope)[2,1],slope_UCI=confint(slope)[2,2])
      calib_fits=rbind(calib_fits,df)

    }
  }
  calib_fits<-calib_fits[-1,]
  calib <- calib_fits %>% group_by(nvar) %>% summarize(mean(intc),mean(intc_LCI),mean(intc_UCI),
                                                 mean(slope),mean(slope_LCI),mean(slope_UCI))
  names(calib) <- c("nvar","intc","intc_LCI","intc_UCI","slope","slope_LCI","slope_UCI")  #renaming
  
  decilesCC <- result %>% split(.,list(.$iter,.$nvar),drop=TRUE) %>% purrr::map(. %>% arrange(pred_glm)) %>% #now have a df for each iteration of each nvar
    purrr::map(~mutate(.x, decile_glm=ntile(pred_glm,10))) %>% #create predicted glm decile groups; equivalent to: purrr::map(list_resB, ~mutate(.x, decile_glm=ntile(pred_glm,10))); str(temp3)
    bind_rows(.) %>% split(.,f=.$nvar) %>% #a list of df for each nvar which contains all iter for that nvar. "nest" might be better for this
    purrr::map(., . %>% group_by(decile_glm) %>% summarize(mean(true),mean(pred_glm))) #for each decile in each nvar, have an avg true and avg predicted
  
  output<-list(df_imps=df_imps_full,result=result,train_record=train_record,test_record=test_record,AUC_df=AUC_df,decilesCC=decilesCC,calib=calib,iter=iter,nvars_opts=nvars_opts)
  
}
# output<-CPR.funct(complete_gf,"haz_0.5",3,c(2,5))
# str(output) #output[["result"]] output[["train_record"]] output[["test_record"]]


################### MAIN growth falter all cases ####
main <- CPR.funct(data=complete_gf,outcome="haz_0.5",iter=100,nvars_opts=c(1:10,15,20,30,40,50))
#save(main, file = "/main_df_GEMS059.Rdata")

main[["df_imps"]]
main[["AUC_df"]]
main[["calib"]]

# names    var_red
# 1               base_age 98.6571054
# 2                f4b_haz 84.2432042
# 3               f4b_resp 54.7441770
# 4               f4b_temp 50.9883421
# 5          f4a_ppl_house 38.0483068
# 6          f4a_slp_rooms 25.2874478
# 7           f4a_drh_days 25.2720523
# 8          f4a_share_fac 23.9963262
# 9          f4a_breastfed 23.8613424
# 10      f4a_yng_children 21.2134819
# 11         f4a_prim_schl 19.5686986
# 12        f4a_offr_drink 16.6183196
# 13         f4b_recommend 14.8203765
# 14        f4a_disp_feces 13.5702658
# 15         f4a_fac_waste 13.1105780
# 16     f4a_drh_bellypain 12.2127341
# 17                  site 12.2048997
# 18        f4a_max_stools 12.1748879
# 19          f4a_dad_live 11.4517686
# 20          f4a_ms_water 11.3210592
# 21            f4b_mental 10.3619294
# 22        f4a_trt_method 10.2738970
# 23       f4a_water_avail 10.0746688
# 24         f4a_drh_vomit 10.0613148
# 25             f3_gender  9.2233728
# 26          f4a_wash_use  9.0043604
# 27    f4a_drh_cough_miss  8.8750442
# 28             f4b_mouth  8.6756919
# 29        f4a_wash_nurse  8.6586457
# 30  f4a_drh_lethrgy_miss  8.6259742
# 31         f4a_wash_cook  8.5930846
# 32       f4a_cur_thirsty  8.4182980
# 33        f4a_wash_child  8.2754783
# 34        f4a_house_bike  8.2184331
# 35        f4a_drh_thirst  8.1375273
# 36      f4a_hometrt_none  7.7776173
# 37          f4a_wash_def  7.7117729
# 38        f4a_house_tele  7.7100210
# 39       f4a_hometrt_ors  7.6210445
# 40      f4a_seek_outside  7.6179078
# 41      f4a_cur_restless  7.4927374
# 42      f4a_drh_restless  7.4910908
# 43      f4a_cur_drymouth  7.4848594
# 44        f4a_house_elec  7.4634719
# 45          f4a_ani_fowl  7.4573977
# 46       f4a_house_radio  7.3429845
# 47              f4b_skin  7.2541528
# 48       f4a_ani_rodents  7.2276782
# 49       f4a_house_phone  7.2169607
# 50          f4a_cur_skin  7.1150053
# 51             f3_drh_iv  7.0477100
# 52      f4a_house_agland  7.0369805
# 53       f4a_store_water  7.0286675
# 54           f4a_ani_cat  6.9496394
# 55           f4a_ani_dog  6.8988615
# 56          f4a_ani_goat  6.8166028
# 57           f3_drh_hosp  6.7730793
# 58          f4a_wash_eat  6.7400938
# 59     f4a_drh_lessdrink  6.6735013
# 60             f4a_floor  6.5180547
# 61           f4a_ani_cow  6.4491186
# 62         f4a_trt_water  6.4344934
# 63         f3_drh_turgor  6.2613162
# 64             f4b_admit  6.1440527
# 65         f4a_fuel_wood  6.1355313
# 66        f4a_drh_strain  6.1262022
# 67      f4a_water_pubtap  6.0789508
# 68     f4a_hometrt_othr1  5.9760242
# 69         f4a_drh_blood  5.8580812
# 70       f4a_house_scoot  5.8279109
# 71     f4a_hometrt_maize  5.7156459
# 72     f4a_fuel_charcoal  5.5323488
# 73         f4a_ani_sheep  5.5021320
# 74    f4a_cur_fastbreath  5.4973715
# 75        f4a_seek_pharm  5.4441128
# 76        f4a_hometrt_ab  5.4016115
# 77      f4b_nature_stool  5.0985113
# 78        f4b_under_nutr  5.0869036
# 79       f4a_wash_animal  5.0042321
# 80      f4a_house_fridge  4.8729214
# 81    f4a_water_deepwell  4.7071382
# 82      f4a_hometrt_herb  4.6082297
# 83         f4a_ani_other  4.5623860
# 84              f4b_eyes  4.4084726
# 85         f4a_wash_othr  4.3904282
# 86        f4a_house_cart  4.2629585
# 87         f4a_fuel_kero  4.0533707
# 88            f4a_ani_no  4.0305699
# 89        f4a_water_yard  3.8254059
# 90        f4a_water_bore  3.6422947
# 91     f4a_water_pubwell  3.6338947
# 92   f4a_water_shallwell  3.4620050
# 93     f4a_hometrt_othr2  3.4443904
# 94         f4a_house_car  3.4313488
# 95        f4a_seek_remdy  3.2042858
# 96         f4a_fuel_crop  3.1805253
# 97        f4a_fuel_grass  3.1322165
# 98      f4a_seek_privdoc  3.0949054
# 99     f4a_water_covwell  3.0808846
# 100        f4a_fuel_dung  3.0700656
# 101      f4a_fuel_natgas  3.0541499
# 102         f4a_seek_doc  3.0522861
# 103        f4a_drh_consc  2.9204047
# 104     f4a_relationship  2.8470782
# 105     f4a_water_bought  2.7334920
# 106      f4a_seek_healer  2.7189183
# 107      f4b_chest_indrw  2.6837357
# 108         f4b_abn_hair  2.6779699
# 109     f4a_hometrt_zinc  2.6648370
# 110      f4a_water_house  2.6296838
# 111       f4a_water_rain  2.6198709
# 112        f4a_fuel_coal  2.5570361
# 113     f4a_fuel_propane  2.5481752
# 114       f4a_water_well  2.5469671
# 115      f4a_fuel_biogas  2.3718021
# 116      f4a_water_river  2.2574899
# 117       f4a_seek_other  2.0341402
# 118       f4a_house_none  1.8517947
# 119   f4a_water_covpwell  1.8398559
# 120  f4a_water_prospring  1.7905305
# 121         f4a_drh_conv  1.7313350
# 122       f4a_water_pond  1.3985506
# 123       f4a_water_othr  1.3196229
# 124  f4a_hometrt_othrliq  1.2534410
# 125       f4b_skin_flaky  1.1871484
# 126     f4a_drh_prolapse  1.1683226
# 127          f4b_bipedal  1.0636020
# 128       f4a_fuel_other  1.0188820
# 129       f4a_house_boat  0.9369976
# 130   f4a_water_unspring  0.7479167
# 131     f4a_hometrt_milk  0.6674872
# 132      f4a_seek_friend  0.3977289
# 133        f4a_fuel_elec  0.3259463
# 134    f4b_observe_stool  0.2586242
# 135           f4b_rectal  0.1785052


# AUC          SE     lower     upper level Model nvar
# 1  0.6887095 0.001452098 0.6858634 0.6915555  0.95    LR    1
# 2  0.7138374 0.001430564 0.7110335 0.7166412  0.95    LR    2
# 3  0.7137607 0.001428145 0.7109615 0.7165598  0.95    LR    3
# 4  0.7217278 0.001415537 0.7189534 0.7245022  0.95    LR    4
# 5  0.7213855 0.001416203 0.7186098 0.7241612  0.95    LR    5
# 6  0.7211792 0.001416447 0.7184030 0.7239554  0.95    LR    6
# 7  0.7210433 0.001417008 0.7182660 0.7238206  0.95    LR    7
# 8  0.7213727 0.001415005 0.7185993 0.7241461  0.95    LR    8
# 9  0.7215800 0.001413251 0.7188100 0.7243499  0.95    LR    9
# 10 0.7213855 0.001413936 0.7186143 0.7241568  0.95    LR   10
# 11 0.7413099 0.001356784 0.7386507 0.7439692  0.95    LR   15
# 12 0.7487515 0.001335159 0.7461346 0.7513684  0.95    LR   20
# 13 0.7484695 0.001335987 0.7458510 0.7510880  0.95    LR   30
# 14 0.7466196 0.001337568 0.7439980 0.7492412  0.95    LR   40
# 15 0.7458105 0.001339426 0.7431852 0.7484357  0.95    LR   50
# 16 0.6845387 0.001463604 0.6816701 0.6874073  0.95    RF    1
# 17 0.6871353 0.001494572 0.6842060 0.6900646  0.95    RF    2
# 18 0.6915594 0.001473358 0.6886716 0.6944471  0.95    RF    3
# 19 0.6953942 0.001465882 0.6925211 0.6982673  0.95    RF    4
# 20 0.6985449 0.001456634 0.6956900 0.7013999  0.95    RF    5
# 21 0.7021609 0.001454660 0.6993098 0.7050120  0.95    RF    6
# 22 0.7058307 0.001447654 0.7029934 0.7086681  0.95    RF    7
# 23 0.7090325 0.001434472 0.7062210 0.7118440  0.95    RF    8
# 24 0.7115923 0.001425864 0.7087977 0.7143870  0.95    RF    9
# 25 0.7134757 0.001419071 0.7106944 0.7162570  0.95    RF   10
# 26 0.7374204 0.001377103 0.7347213 0.7401195  0.95    RF   15
# 27 0.7455093 0.001355535 0.7428525 0.7481661  0.95    RF   20
# 28 0.7484576 0.001345239 0.7458210 0.7510942  0.95    RF   30
# 29 0.7500621 0.001342542 0.7474308 0.7526934  0.95    RF   40
# 30 0.7521060 0.001338280 0.7494830 0.7547290  0.95    RF   50


# nvar    intc intc_LCI intc_UCI slope slope_LCI slope_UCI
# <dbl>   <dbl>    <dbl>    <dbl> <dbl>     <dbl>     <dbl>
#   1     1 0.00291   -0.123    0.127 1.01      0.821      1.22
# 2     2 0.00363   -0.124    0.129 1.01      0.839      1.20
# 3     3 0.00362   -0.124    0.129 1.01      0.835      1.19
# 4     4 0.00410   -0.124    0.130 1.00      0.836      1.18
# 5     5 0.00422   -0.124    0.130 1.00      0.834      1.18
# 6     6 0.00416   -0.124    0.130 1.00      0.832      1.18
# 7     7 0.00427   -0.124    0.130 0.998     0.831      1.17
# 8     8 0.00442   -0.123    0.130 0.997     0.830      1.17
# 9     9 0.00469   -0.123    0.130 0.994     0.827      1.17
# 10    10 0.00481   -0.123    0.131 0.992     0.826      1.17
# 11    15 0.00363   -0.126    0.131 0.957     0.808      1.11
# 12    20 0.00381   -0.127    0.132 0.942     0.799      1.09
# 13    30 0.00293   -0.128    0.132 0.914     0.775      1.06
# 14    40 0.00307   -0.128    0.132 0.888     0.752      1.03
# 15    50 0.00351   -0.128    0.133 0.870     0.736      1.01

#intercept <0, overestimation
#intercept >0, underestimation
#slopes <1, estimated risks too extreme in both directions
#slope >1 risk estimates to moderate
#slightly positive slope indicates slight underestimation

temp <- main[["decilesCC"]][c("1","2","3","4","5","6","7","8","9","10","15","20","30","40","50")]
names(temp) <- c("1-var","2-var","3-var","4-var","5-var","6-var","7-var","8-var","9-var","10-var","15-var","20-var","30-var","40-var","50-var")  #renaming
#jpeg("/GF_CC_GEMS059.jpg",width=600,height=480,quality=400)
#png("/GF_CC_GEMS059.png",units="px",width=3000,height=2400,res=300)
plot(x=seq(0,1,by=0.1),y=seq(0,1,by=0.1),type="l",
     xlab="Predicted Probability",ylab="Observed Proportion",
     main=expression(paste("Calibration Curve:","">=0.5,Delta,"HAZ in cases 0-59mo in GEMS")),
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5)
points(temp$`5-var`$`mean(pred_glm)`,temp$`5-var`$`mean(true)`,col="red",pch=1,cex=2,lwd=2)
points(temp$`10-var`$`mean(pred_glm)`,temp$`10-var`$`mean(true)`,col="blue",pch=2,cex=2,lwd=2)
legend("topleft",col=c("red","blue"),c("5-variable","10-variable"),pch=c(1,2),cex=1.5)
dev.off()

AUC_df <- main[["AUC_df"]]
#save(AUC_df, file = "/AUC_df_GEMS059.Rdata")
#load(file = "/AUC_df_GEMS059.Rdata")

#jpeg("/GF_AUCs_GEMS059.jpg",width=600,height=480,quality=400)
#png("/GF_AUCs_GEMS059.png",units="px",width=3000,height=2400,res=300)
par(mar=c(5,5,4,2))
plot(AUC_df$nvar[1:length(main[["nvars_opts"]])[1]],AUC_df$AUC[1:length(main[["nvars_opts"]])[1]],
     xlab="number of variables",ylab="AUC",
     #main="MAIN MODEL >=0.5 deltaHAZ in cases 0-59mo",
     main=expression(paste("">=0.5,Delta,"HAZ in cases 0-59mo in GEMS")),
     ylim=c(0.5,0.8),
     pch=1,col="red",cex=2,lwd=2,cex.axis=1.5,cex.lab=1.5,cex.main=1.5)
points(AUC_df$nvar[1:length(main[["nvars_opts"]])[1]],AUC_df$AUC[(length(main[["nvars_opts"]])[1]+1):dim(AUC_df)[1]],
       pch=2,col="blue",cex=2,lwd=2)
legend("topleft",c("logistic regression","random forest regression"),col=c("red","blue"),pch=c(1,2),cex=1.5)
dev.off()

glm_gf <- glm(haz_0.5~base_age+f4b_haz+f4b_resp+f4b_temp+f4a_ppl_house+
                f4a_slp_rooms+f4a_drh_days+f4a_share_fac+f4a_breastfed+
                f4a_yng_children,
              data=complete_gf,family="binomial",control=glm.control(maxit=50))
summary(glm_gf)
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)      -8.6409894  1.1393362  -7.584 3.34e-14 ***
#   base_age         -0.0840504  0.0045161 -18.611  < 2e-16 ***
#   f4b_haz           0.2603649  0.0248475  10.479  < 2e-16 ***
#   f4b_resp         -0.0038573  0.0034322  -1.124   0.2611    
# f4b_temp          0.2498079  0.0310382   8.048 8.39e-16 ***
#   f4a_ppl_house    -0.0008956  0.0052255  -0.171   0.8639    
# f4a_slp_rooms     0.0006781  0.0129355   0.052   0.9582    
# f4a_drh_days     -0.0242400  0.0222905  -1.087   0.2768    
# f4a_share_fac    -0.0108033  0.0076076  -1.420   0.1556    
# f4a_breastfed    -0.1627464  0.0690744  -2.356   0.0185 *  
#   f4a_yng_children -0.0072213  0.0208834  -0.346   0.7295  
round(exp(coef(glm_gf)),2)
# (Intercept)         base_age          f4b_haz         f4b_resp         f4b_temp 
# 0.00             0.92             1.30             1.00             1.28 
# f4a_ppl_house    f4a_slp_rooms     f4a_drh_days    f4a_share_fac    f4a_breastfed 
# 1.00             1.00             0.98             0.99             0.85 
# f4a_yng_children 
# 0.99 
round(exp(confint(glm_gf)),2)
# 2.5 % 97.5 %
#   (Intercept)       0.00   0.00
# base_age          0.91   0.93
# f4b_haz           1.24   1.36
# f4b_resp          0.99   1.00
# f4b_temp          1.21   1.36
# f4a_ppl_house     0.99   1.01
# f4a_slp_rooms     0.98   1.03
# f4a_drh_days      0.93   1.02
# f4a_share_fac     0.97   1.00
# f4a_breastfed     0.74   0.97
# f4a_yng_children  0.95   1.03


################### main growth faltering ROC curve ####
#want a single ROC surve for CV in test datasets. use "result"
result<-main[["result"]]

roc.data=result %>% split(.,list(.$nvar),drop=TRUE) %>% .$"10" %>% #subset to all the iter's (v-fold cross validations) for a given nvar (number of predictor variables in CPR)
  split(.,list(.$iter),drop=TRUE) #now have a list for each iter
#iter are the folds
# table($iter)
# 1    2    3    4    5    6    7    8    9   10 
# 1529 1529 1529 1529 1529 1529 1529 1529 1529 1529 
#>>> now 1528 per iteration

#reformatting the data so in the same layout as example for 
predict_combo=list(NA)
true_combo=list(NA)

for (i in 1:iter){ #this iter from the main RF/LR loop
  temp.list <- list(roc.data[[i]]$pred_glm)
  predict_combo <- c(predict_combo,temp.list)
  
  temp.list2 <- list(roc.data[[i]]$true)
  true_combo <- c(true_combo,temp.list2)
  
}
predict_combo=predict_combo[-1]
true_combo=true_combo[-1]
str(predict_combo)
str(true_combo)
combo <- list(predict_combo,true_combo)
names(combo) <- c("pred_glm","true")  #renaming
str(combo)



CV.roc.data <- cvAUC(predictions=combo$pred_glm, labels=combo$true) #perf is an object of class "performance" from the ROCR pckg

# #find the desired points for obtained Sp for given Se
# #options(max.print=2000)
# Se.Sp<-data.frame(Se=round(roc.obj.5var$sensitivities,2),Sp=round(roc.obj.5var$specificities,2))
# Se.Sp[which(Se.Sp$Se==0.85),]
# #between the point (x0[i], y0[i]) and the point (x1[i], y1[i])
# #x0, y0, x1 = x0, y1 = y0,

# #Plot fold AUCs >>> SMA: a line for each fold, ie iteration
# plot(CV.roc.data$perf, col="grey82", lty=3, main="Cross-validated ROC Curve")

CV.roc.data.GF.2 <- CV.roc.data
CV.roc.data.GF.5 <- CV.roc.data
CV.roc.data.GF.10 <- CV.roc.data
#to save
CV.roc.data.all <- list(CV.roc.data.GF.2,CV.roc.data.GF.5,CV.roc.data.GF.10)
names(CV.roc.data.all) <- c("GF.2","GF.5","GF.10")  #renaming
str(CV.roc.data.all)
#save(CV.roc.data.all, file = "/CV.roc.data.all.Rdata")

#load(file = "/CV.roc.data.all.Rdata")
#(there's a better way to do this as a list...)
CV.roc.data.GF.2 <- CV.roc.data.all$GF.2
CV.roc.data.GF.5 <- CV.roc.data.all$GF.5
CV.roc.data.GF.10 <- CV.roc.data.all$GF.10


#Plot CV AUC >>> SMA: a single line of the averaged cross-validated ROC curve
#jpeg("/roc_GFonly.jpg",width=480,height=480,quality=400)
#png("/roc_GFonly_highres.png",units="px",width=3000,height=3000,res=300)
plot(CV.roc.data.GF.2$perf, avg="vertical", main="Cross-validated ROC Curves for Growth Faltering",
     col="#1c61b6", lwd=2, lty=2) 
plot(CV.roc.data.GF.5$perf, avg="vertical", col="#1c61b6", lwd=2, lty=3, add=TRUE) 
plot(CV.roc.data.GF.10$perf, avg="vertical", col="#1c61b6", lwd=2, lty=6, add=TRUE) 
legend("bottomright", 
       legend = c("2-var","5-var","10-var"), 
       col = c("#1c61b6"),
       lty = c(1,1,2,3,6),
       lwd = 2)
segments(x0=0,y0=0.8,x1=0.47,y1=0.8,lty=2,col="gray")
segments(x0=0.47,y0=0.8,x1=0.47,y1=0.0,lty=2,col="gray")
dev.off()


################### main + 0-11(age_1) growth falter ####
main.011 <- CPR.funct(data=cases_gf_age1,outcome="haz_0.5",iter=100,nvars_opts=c(5,10))
main.011[["df_imps"]]
main.011[["AUC_df"]]
main.011[["calib"]]

# names    var_red
# 1                f4b_haz 43.6780195
# 2               f4b_temp 30.9734638
# 3               f4b_resp 29.4488818
# 4          f4a_ppl_house 21.9724267
# 5               base_age 21.6368371
# 6          f4a_share_fac 14.7416981
# 7           f4a_drh_days 14.6773120
# 8          f4a_slp_rooms 14.5117656
# 9       f4a_yng_children 12.4650200
# 10         f4a_prim_schl 11.3541789
# 11        f4a_offr_drink  9.0583005
# 12         f4a_fac_waste  7.8780427
# 13        f4a_disp_feces  7.2459011
# 14     f4a_drh_bellypain  7.0832070
# 15          f4a_ms_water  6.9498313
# 16          f4a_dad_live  6.9103415
# 17            f4b_mental  6.8291584
# 18        f4a_max_stools  6.7306446
# 19                  site  6.7255544
# 20         f4a_breastfed  6.2298817
# 21        f4a_trt_method  5.8759477
# 22       f4a_water_avail  5.8157632
# 23         f4b_recommend  5.6290697
# 24             f3_gender  5.3343461
# 25             f4b_mouth  5.1720469
# 26    f4a_drh_cough_miss  5.1714536
# 27          f4a_wash_use  5.1313865
# 28        f4a_wash_nurse  5.0482814
# 29         f4a_wash_cook  5.0187585
# 30       f4a_cur_thirsty  4.9729247
# 31  f4a_drh_lethrgy_miss  4.9404482
# 32        f4a_drh_thirst  4.9319071
# 33        f4a_wash_child  4.8618117
# 34         f4a_drh_vomit  4.8185690
# 35          f4a_wash_def  4.7221373
# 36        f4a_house_bike  4.6369512
# 37          f4a_ani_fowl  4.5577904
# 38      f4a_cur_drymouth  4.5004644
# 39      f4a_cur_restless  4.4442305
# 40        f4a_house_tele  4.3764456
# 41       f4a_hometrt_ors  4.3482698
# 42      f4a_drh_restless  4.3377185
# 43          f4a_cur_skin  4.2644964
# 44              f4b_skin  4.2542976
# 45       f4a_house_phone  4.2526469
# 46      f4a_hometrt_none  4.2456691
# 47       f4a_house_radio  4.2299551
# 48      f4a_seek_outside  4.2010977
# 49     f4a_drh_lessdrink  4.1748129
# 50           f4a_ani_dog  4.0522772
# 51       f4a_store_water  4.0412591
# 52       f4a_ani_rodents  3.9886497
# 53     f4a_hometrt_maize  3.9849575
# 54           f4a_ani_cat  3.9677938
# 55         f3_drh_turgor  3.9543751
# 56          f4a_wash_eat  3.8576427
# 57         f4a_fuel_wood  3.8254417
# 58        f4a_drh_strain  3.8039742
# 59        f4a_house_elec  3.7409311
# 60         f4a_trt_water  3.7098787
# 61          f4a_ani_goat  3.5816679
# 62      f4a_water_pubtap  3.4730379
# 63       f4a_house_scoot  3.4621954
# 64             f4a_floor  3.4522983
# 65           f4a_ani_cow  3.3878534
# 66             f3_drh_iv  3.3597592
# 67     f4a_hometrt_othr1  3.2798716
# 68     f4a_fuel_charcoal  3.1729687
# 69      f4a_house_agland  3.0888359
# 70        f4a_seek_pharm  3.0705534
# 71             f4b_admit  3.0630332
# 72         f4a_drh_blood  3.0354631
# 73       f4a_wash_animal  3.0037410
# 74         f4a_ani_sheep  3.0008784
# 75        f4a_hometrt_ab  2.9717778
# 76    f4a_cur_fastbreath  2.9295236
# 77           f3_drh_hosp  2.9003485
# 78    f4a_water_deepwell  2.8736405
# 79        f4b_under_nutr  2.8507906
# 80      f4a_hometrt_herb  2.7996182
# 81      f4a_house_fridge  2.7391081
# 82            f4a_ani_no  2.5801359
# 83         f4a_wash_othr  2.5345834
# 84              f4b_eyes  2.4523250
# 85         f4a_fuel_kero  2.4073173
# 86        f4a_water_yard  2.4037525
# 87         f4a_ani_other  2.4025820
# 88      f4b_nature_stool  2.3647366
# 89        f4a_house_cart  2.3295991
# 90     f4a_water_covwell  2.2647974
# 91      f4a_water_bought  2.1645167
# 92         f4a_house_car  2.0849177
# 93     f4a_water_pubwell  1.9897217
# 94         f4a_fuel_crop  1.9534060
# 95   f4a_water_shallwell  1.9396993
# 96      f4a_seek_privdoc  1.9113478
# 97       f4a_fuel_natgas  1.8748146
# 98        f4a_fuel_grass  1.7881969
# 99        f4a_water_bore  1.7313771
# 100      f4a_seek_healer  1.6934261
# 101        f4a_fuel_coal  1.6379365
# 102        f4a_fuel_dung  1.6266786
# 103       f4a_water_rain  1.6197245
# 104     f4a_relationship  1.6190425
# 105     f4a_hometrt_zinc  1.6170109
# 106         f4a_seek_doc  1.6018027
# 107       f4a_seek_remdy  1.5860066
# 108    f4a_hometrt_othr2  1.5745743
# 109      f4a_water_house  1.5462799
# 110      f4b_chest_indrw  1.5455555
# 111        f4a_drh_consc  1.4582050
# 112     f4a_fuel_propane  1.3975268
# 113      f4a_water_river  1.3750092
# 114      f4a_fuel_biogas  1.2350986
# 115       f4a_seek_other  1.2190569
# 116         f4b_abn_hair  1.1741416
# 117       f4a_water_well  1.1149276
# 118       f4a_house_none  1.0742560
# 119       f4b_skin_flaky  0.9741653
# 120       f4a_fuel_other  0.7755233
# 121   f4a_water_covpwell  0.7355930
# 122     f4a_drh_prolapse  0.6871403
# 123  f4a_hometrt_othrliq  0.6871280
# 124       f4a_water_pond  0.6487646
# 125       f4a_house_boat  0.6423633
# 126  f4a_water_prospring  0.6236766
# 127     f4a_hometrt_milk  0.5884259
# 128       f4a_water_othr  0.5348684
# 129          f4b_bipedal  0.4883732
# 130   f4a_water_unspring  0.4433975
# 131         f4a_drh_conv  0.4277270
# 132        f4a_fuel_elec  0.2575643
# 133      f4a_seek_friend  0.1993300
# 134           f4b_rectal  0.1319697
# 135    f4b_observe_stool  0.0000000


# AUC          SE     lower     upper level Model nvar
# 1 0.5967801 0.002391460 0.5920929 0.6014673  0.95    LR    5
# 2 0.5972497 0.002368754 0.5926071 0.6018924  0.95    LR   10
# 3 0.5592524 0.002393490 0.5545613 0.5639436  0.95    RF    5
# 4 0.5672807 0.002398252 0.5625802 0.5719812  0.95    RF   10


# A tibble: 3 x 7
# nvar   intc intc_LCI intc_UCI slope slope_LCI slope_UCI
# <dbl>  <dbl>    <dbl>    <dbl> <dbl>     <dbl>     <dbl>
# 1     5 0.0104   -0.157    0.175 0.902     0.446      1.37
# 2    10 0.0127   -0.156    0.178 0.818     0.414      1.23

# temp <- main.011[["decilesCC"]][c("5", "10")]
# names(temp) <- c("5-var","10-var")  #renaming
# #names(temp) <- c("1-var","2-var","3-var","4-var","5-var","6-var","7-var","8-var","9-var","10-var","15-var","20-var","30-var","40-var","50-var")  #renaming
# plot(x=seq(0,1,by=0.1),y=seq(0,1,by=0.1),type="l",
#      xlab="Predicted Probability",ylab="Observed Proportion",
#      #main="Calibration Curve")
#      main=expression(paste("Calibration Curve:","">=0.5,Delta,"HAZ in cases 0-11mo in GEMS")))
# points(temp$`5-var`$`mean(pred_glm)`,temp$`5-var`$`mean(true)`,col="red",pch=1)
# points(temp$`10-var`$`mean(pred_glm)`,temp$`10-var`$`mean(true)`,col="blue",pch=2)
# legend("topleft",col=c("red","blue"),c("5-variable","10-variable"),pch=c(1,2))

################### main + 12-23(age_2) growth falter ####
main.1223 <- CPR.funct(data=cases_gf_age2,outcome="haz_0.5",iter=100,nvars_opts=c(5,10))
main.1223[["df_imps"]]
main.1223[["AUC_df"]]
main.1223[["calib"]]

# names     var_red
# 1                f4b_haz 27.24791164
# 2               f4b_resp 18.76739509
# 3               f4b_temp 17.72924255
# 4               base_age 16.06085064
# 5          f4a_ppl_house 13.05689213
# 6          f4a_slp_rooms  9.37465759
# 7          f4b_recommend  9.01448115
# 8           f4a_drh_days  8.43460229
# 9          f4a_share_fac  7.65986123
# 10      f4a_yng_children  6.89641705
# 11         f4a_prim_schl  6.82958375
# 12        f4a_offr_drink  6.19371543
# 13                  site  5.37556500
# 14        f4a_disp_feces  5.23809895
# 15      f4a_house_agland  4.54095468
# 16          f4a_ms_water  4.45173913
# 17     f4a_drh_bellypain  4.39759912
# 18         f4a_fac_waste  4.31922113
# 19         f4a_drh_vomit  4.13127410
# 20          f4a_dad_live  4.10876420
# 21             f3_drh_iv  4.07222125
# 22        f4a_max_stools  3.86504674
# 23       f4a_water_avail  3.85405057
# 24           f3_drh_hosp  3.61487973
# 25        f4a_trt_method  3.52003277
# 26            f4b_mental  3.45184644
# 27         f4a_breastfed  3.42691887
# 28        f4a_house_elec  3.36538511
# 29             f4b_mouth  3.21444986
# 30    f4a_drh_cough_miss  3.18598179
# 31             f3_gender  3.10607333
# 32          f4a_wash_use  3.03715123
# 33  f4a_drh_lethrgy_miss  2.98394801
# 34             f4b_admit  2.92387620
# 35        f4a_house_bike  2.88889546
# 36       f4a_cur_thirsty  2.87057707
# 37          f4a_ani_goat  2.85749820
# 38         f4a_wash_cook  2.83414380
# 39        f4a_wash_nurse  2.81068526
# 40        f4a_wash_child  2.76235866
# 41           f4a_ani_cat  2.72638138
# 42       f4a_ani_rodents  2.66825359
# 43        f4a_water_bore  2.64507555
# 44          f4a_wash_def  2.63815804
# 45      f4a_hometrt_none  2.61353282
# 46          f4a_ani_fowl  2.54262707
# 47        f4a_house_tele  2.52246526
# 48      f4a_drh_restless  2.51265827
# 49           f4a_ani_dog  2.50859806
# 50      f4a_seek_outside  2.47287131
# 51      f4a_cur_drymouth  2.46828888
# 52      f4a_cur_restless  2.44453661
# 53              f4b_skin  2.44277852
# 54           f4a_ani_cow  2.44099507
# 55       f4a_house_radio  2.43136730
# 56         f4a_trt_water  2.40545891
# 57             f4a_floor  2.38716955
# 58          f4a_cur_skin  2.35560752
# 59         f4a_drh_blood  2.34128455
# 60        f4a_drh_thirst  2.34123365
# 61       f4a_hometrt_ors  2.31174914
# 62    f4a_cur_fastbreath  2.27375332
# 63         f4a_ani_sheep  2.22778578
# 64       f4a_house_phone  2.18519247
# 65     f4a_drh_lessdrink  2.16462359
# 66         f4a_ani_other  2.15554983
# 67     f4a_hometrt_othr1  2.13855381
# 68       f4a_store_water  2.11853556
# 69      f4a_water_pubtap  2.10693385
# 70       f4a_house_scoot  2.09993475
# 71          f4a_wash_eat  2.08942018
# 72      f4b_nature_stool  2.05308603
# 73        f4a_hometrt_ab  2.00725699
# 74     f4a_fuel_charcoal  1.96737034
# 75         f3_drh_turgor  1.88458562
# 76         f4a_fuel_wood  1.88069836
# 77        f4b_under_nutr  1.81990923
# 78     f4a_hometrt_maize  1.73435510
# 79      f4a_house_fridge  1.71464596
# 80        f4a_drh_strain  1.65826825
# 81        f4a_seek_pharm  1.62903908
# 82      f4a_hometrt_herb  1.61806207
# 83              f4b_eyes  1.59820905
# 84    f4a_water_deepwell  1.58871431
# 85       f4a_wash_animal  1.58669683
# 86        f4a_house_cart  1.57878016
# 87     f4a_water_pubwell  1.57652333
# 88         f4a_wash_othr  1.49017044
# 89        f4a_water_well  1.34018196
# 90         f4a_fuel_kero  1.26937157
# 91     f4a_hometrt_othr2  1.26312533
# 92         f4a_fuel_dung  1.25906572
# 93        f4a_seek_remdy  1.24811042
# 94            f4a_ani_no  1.23269762
# 95        f4a_water_yard  1.18875499
# 96       f4a_water_house  1.18428685
# 97         f4a_house_car  1.16254592
# 98        f4a_fuel_grass  1.08661791
# 99       f4b_chest_indrw  1.07757222
# 100  f4a_water_shallwell  1.06439560
# 101    f4a_water_covwell  1.04976963
# 102         f4b_abn_hair  1.04116671
# 103      f4a_fuel_biogas  1.03516063
# 104       f4a_water_rain  1.02223605
# 105     f4a_seek_privdoc  1.00607201
# 106        f4a_fuel_crop  0.97093893
# 107   f4a_water_covpwell  0.96107855
# 108       f4a_water_othr  0.95506129
# 109     f4a_hometrt_zinc  0.93446950
# 110      f4a_fuel_natgas  0.92383851
# 111        f4a_drh_consc  0.87317868
# 112         f4a_seek_doc  0.84368146
# 113         f4a_drh_conv  0.83431859
# 114     f4a_relationship  0.80570516
# 115     f4a_water_bought  0.80377674
# 116      f4a_seek_healer  0.80247627
# 117        f4a_fuel_coal  0.76338045
# 118     f4a_fuel_propane  0.74884267
# 119  f4a_water_prospring  0.73555343
# 120      f4a_water_river  0.70951178
# 121       f4a_water_pond  0.60981685
# 122       f4a_house_none  0.59865474
# 123          f4b_bipedal  0.42493247
# 124       f4a_seek_other  0.41100669
# 125  f4a_hometrt_othrliq  0.40497811
# 126       f4b_skin_flaky  0.36473908
# 127    f4b_observe_stool  0.27978535
# 128     f4a_drh_prolapse  0.21261122
# 129   f4a_water_unspring  0.18690853
# 130       f4a_fuel_other  0.17926182
# 131      f4a_seek_friend  0.17892973
# 132       f4a_house_boat  0.14523969
# 133           f4b_rectal  0.05251546
# 134        f4a_fuel_elec  0.01734944
# 135     f4a_hometrt_milk  0.01115439


# AUC          SE     lower     upper level Model nvar
# 1 0.6570957 0.002832452 0.6515442 0.6626472  0.95    LR    5
# 2 0.6991225 0.002729328 0.6937731 0.7044719  0.95    LR   10
# 3 0.6283448 0.002875257 0.6227094 0.6339802  0.95    RF    5
# 4 0.6874801 0.002767516 0.6820559 0.6929043  0.95    RF   10

# nvar   intc intc_LCI intc_UCI slope slope_LCI slope_UCI
# <dbl>  <dbl>    <dbl>    <dbl> <dbl>     <dbl>     <dbl>
# 1     5 -0.00835   -0.225    0.201 0.969     0.605      1.35
# 2    10 -0.00947   -0.232    0.206 0.881     0.610      1.17

################### main + 0-23(age_4) growth falter ####
main.023 <- CPR.funct(data=cases_gf_age4,outcome="haz_0.5",iter=100,nvars_opts=c(1:10,15,20,30,40,50))
main.023[["df_imps"]]
main.023[["AUC_df"]]
main.023[["calib"]]

# names    var_red
# 1                f4b_haz 76.1918459
# 2               base_age 55.4855972
# 3               f4b_temp 48.1159342
# 4               f4b_resp 47.3401584
# 5          f4a_ppl_house 35.6856124
# 6          f4a_slp_rooms 24.2593053
# 7           f4a_drh_days 23.1580841
# 8          f4a_share_fac 22.3128371
# 9       f4a_yng_children 19.7577778
# 10         f4a_prim_schl 18.1900492
# 11        f4a_offr_drink 15.3123458
# 12         f4b_recommend 13.0666126
# 13        f4a_disp_feces 12.0379764
# 14         f4a_fac_waste 11.9480763
# 15        f4a_max_stools 11.2275644
# 16          f4a_ms_water 11.1233648
# 17          f4a_dad_live 10.9065871
# 18     f4a_drh_bellypain 10.9032418
# 19                  site 10.8668292
# 20         f4a_breastfed 10.8454924
# 21            f4b_mental  9.9674597
# 22       f4a_water_avail  9.3079566
# 23        f4a_trt_method  9.0173813
# 24         f4a_drh_vomit  8.8937407
# 25             f3_gender  8.6150020
# 26             f4b_mouth  8.3955227
# 27          f4a_wash_use  8.3559566
# 28    f4a_drh_cough_miss  8.1933511
# 29       f4a_cur_thirsty  8.0801019
# 30         f4a_wash_cook  7.9358033
# 31        f4a_wash_nurse  7.9247626
# 32        f4a_wash_child  7.8282859
# 33  f4a_drh_lethrgy_miss  7.6733912
# 34        f4a_house_bike  7.4572950
# 35        f4a_drh_thirst  7.4212169
# 36          f4a_wash_def  7.3891600
# 37      f4a_hometrt_none  7.1561618
# 38             f3_drh_iv  7.1286890
# 39      f4a_cur_drymouth  7.0966812
# 40        f4a_house_elec  7.0017606
# 41              f4b_skin  6.9875049
# 42      f4a_cur_restless  6.9874622
# 43          f4a_ani_fowl  6.9820367
# 44        f4a_house_tele  6.9358756
# 45      f4a_drh_restless  6.8815130
# 46       f4a_hometrt_ors  6.8795205
# 47      f4a_seek_outside  6.7641299
# 48       f4a_ani_rodents  6.6839692
# 49       f4a_house_radio  6.6754571
# 50           f4a_ani_dog  6.6590544
# 51       f4a_store_water  6.6258528
# 52           f4a_ani_cat  6.5731245
# 53       f4a_house_phone  6.5511384
# 54          f4a_cur_skin  6.4917455
# 55          f4a_ani_goat  6.4678032
# 56     f4a_drh_lessdrink  6.3535842
# 57          f4a_wash_eat  6.2722652
# 58             f4a_floor  6.2560014
# 59      f4a_house_agland  6.2232367
# 60         f4a_trt_water  5.9552618
# 61         f4a_fuel_wood  5.9113840
# 62           f4a_ani_cow  5.8369073
# 63         f3_drh_turgor  5.6899031
# 64           f3_drh_hosp  5.6392421
# 65      f4a_water_pubtap  5.6276707
# 66     f4a_hometrt_othr1  5.5932570
# 67       f4a_house_scoot  5.5915295
# 68        f4a_drh_strain  5.5821665
# 69     f4a_hometrt_maize  5.3324874
# 70     f4a_fuel_charcoal  5.2037287
# 71             f4b_admit  5.1663795
# 72        f4a_hometrt_ab  5.1308841
# 73         f4a_ani_sheep  5.1296507
# 74         f4a_drh_blood  5.1288931
# 75        f4b_under_nutr  4.9777867
# 76        f4a_seek_pharm  4.9707208
# 77    f4a_cur_fastbreath  4.9425678
# 78       f4a_wash_animal  4.5734457
# 79      f4b_nature_stool  4.5442144
# 80    f4a_water_deepwell  4.4873428
# 81      f4a_hometrt_herb  4.4608937
# 82      f4a_house_fridge  4.3301154
# 83         f4a_ani_other  4.3297172
# 84              f4b_eyes  4.0561208
# 85         f4a_wash_othr  3.8667554
# 86         f4a_fuel_kero  3.8100817
# 87        f4a_water_bore  3.7927681
# 88            f4a_ani_no  3.7778727
# 89        f4a_house_cart  3.7767714
# 90        f4a_water_yard  3.5864980
# 91     f4a_water_pubwell  3.4633694
# 92         f4a_house_car  3.1946596
# 93     f4a_water_covwell  3.0969633
# 94   f4a_water_shallwell  2.9986503
# 95     f4a_hometrt_othr2  2.9781734
# 96      f4a_seek_privdoc  2.8744930
# 97         f4a_fuel_dung  2.8740231
# 98        f4a_fuel_grass  2.8401261
# 99         f4a_fuel_crop  2.8310456
# 100     f4a_water_bought  2.8299537
# 101       f4a_seek_remdy  2.7727045
# 102      f4a_fuel_natgas  2.6185336
# 103      f4a_seek_healer  2.5990313
# 104         f4a_seek_doc  2.5769487
# 105     f4a_relationship  2.5675891
# 106        f4a_drh_consc  2.5605737
# 107     f4a_hometrt_zinc  2.5336644
# 108      f4a_water_house  2.5141575
# 109      f4b_chest_indrw  2.4630562
# 110       f4a_water_rain  2.4255664
# 111        f4a_fuel_coal  2.3376507
# 112       f4a_water_well  2.2789294
# 113         f4b_abn_hair  2.2172066
# 114     f4a_fuel_propane  2.1954635
# 115      f4a_fuel_biogas  2.1793356
# 116      f4a_water_river  1.9960781
# 117       f4a_seek_other  1.8592220
# 118   f4a_water_covpwell  1.7194951
# 119       f4a_house_none  1.6689784
# 120  f4a_water_prospring  1.4944005
# 121       f4a_water_pond  1.3395010
# 122       f4a_water_othr  1.2192443
# 123       f4b_skin_flaky  1.1756939
# 124         f4a_drh_conv  1.1540145
# 125  f4a_hometrt_othrliq  1.1338050
# 126       f4a_fuel_other  0.9827448
# 127     f4a_drh_prolapse  0.9407975
# 128          f4b_bipedal  0.8796204
# 129       f4a_house_boat  0.7545029
# 130     f4a_hometrt_milk  0.6562712
# 131   f4a_water_unspring  0.5565115
# 132      f4a_seek_friend  0.3847815
# 133        f4a_fuel_elec  0.2866154
# 134    f4b_observe_stool  0.2092349
# 135           f4b_rectal  0.1739223

# AUC          SE     lower     upper level Model nvar
# 1  0.6024177 0.001854013 0.5987839 0.6060515  0.95    LR    1
# 2  0.6286434 0.001782145 0.6251505 0.6321363  0.95    LR    2
# 3  0.6356208 0.001775748 0.6321404 0.6391012  0.95    LR    3
# 4  0.6388365 0.001774658 0.6353582 0.6423147  0.95    LR    4
# 5  0.6382623 0.001775555 0.6347823 0.6417423  0.95    LR    5
# 6  0.6379449 0.001776205 0.6344636 0.6414262  0.95    LR    6
# 7  0.6376029 0.001777867 0.6341184 0.6410875  0.95    LR    7
# 8  0.6373253 0.001776272 0.6338439 0.6408068  0.95    LR    8
# 9  0.6368552 0.001777792 0.6333708 0.6403396  0.95    LR    9
# 10 0.6400806 0.001770336 0.6366109 0.6435504  0.95    LR   10
# 11 0.6648981 0.001696316 0.6615734 0.6682228  0.95    LR   15
# 12 0.6727507 0.001677370 0.6694631 0.6760383  0.95    LR   20
# 13 0.6718902 0.001676201 0.6686049 0.6751755  0.95    LR   30
# 14 0.6688872 0.001679849 0.6655948 0.6721797  0.95    LR   40
# 15 0.6679299 0.001682146 0.6646329 0.6712268  0.95    LR   50
# 16 0.5414731 0.001883579 0.5377814 0.5451649  0.95    RF    1
# 17 0.6047728 0.001806396 0.6012323 0.6083132  0.95    RF    2
# 18 0.6040359 0.001807783 0.6004927 0.6075791  0.95    RF    3
# 19 0.6099693 0.001784613 0.6064715 0.6134670  0.95    RF    4
# 20 0.6109739 0.001786410 0.6074726 0.6144752  0.95    RF    5
# 21 0.6168734 0.001785637 0.6133736 0.6203731  0.95    RF    6
# 22 0.6197465 0.001780271 0.6162572 0.6232357  0.95    RF    7
# 23 0.6215727 0.001776104 0.6180916 0.6250538  0.95    RF    8
# 24 0.6202679 0.001776999 0.6167851 0.6237508  0.95    RF    9
# 25 0.6264268 0.001770010 0.6229577 0.6298960  0.95    RF   10
# 26 0.6591605 0.001710626 0.6558078 0.6625133  0.95    RF   15
# 27 0.6704314 0.001686764 0.6671254 0.6737374  0.95    RF   20
# 28 0.6724683 0.001678693 0.6691781 0.6757585  0.95    RF   30
# 29 0.6761136 0.001673672 0.6728333 0.6793939  0.95    RF   40
# 30 0.6784375 0.001670523 0.6751633 0.6817117  0.95    RF   50


# nvar    intc intc_LCI intc_UCI slope slope_LCI slope_UCI
# <dbl>   <dbl>    <dbl>    <dbl> <dbl>     <dbl>     <dbl>
#   1     1 -0.0102   -0.141    0.118 0.972     0.618     1.33 
# 2     2 -0.0110   -0.142    0.118 0.979     0.696     1.27 
# 3     3 -0.0116   -0.143    0.118 0.966     0.701     1.24 
# 4     4 -0.0117   -0.144    0.118 0.968     0.709     1.24 
# 5     5 -0.0118   -0.144    0.118 0.964     0.704     1.23 
# 6     6 -0.0118   -0.144    0.118 0.960     0.701     1.23 
# 7     7 -0.0119   -0.144    0.118 0.955     0.697     1.22 
# 8     8 -0.0121   -0.144    0.118 0.950     0.693     1.21 
# 9     9 -0.0121   -0.144    0.118 0.945     0.688     1.21 
# 10    10 -0.0120   -0.144    0.118 0.933     0.686     1.19 
# 11    15 -0.0108   -0.145    0.121 0.890     0.686     1.10 
# 12    20 -0.0119   -0.147    0.121 0.872     0.681     1.07 
# 13    30 -0.0111   -0.147    0.122 0.819     0.636     1.01 
# 14    40 -0.0112   -0.147    0.123 0.787     0.609     0.971
# 15    50 -0.0113   -0.147    0.123 0.754     0.581     0.934


################### main + 24-59(age_3) growth falter ####
main.2459 <- CPR.funct(data=cases_gf_age3,outcome="haz_0.5",iter=100,nvars_opts=c(5,10))
main.2459[["df_imps"]]
main.2459[["AUC_df"]]
main.2459[["calib"]]


# names      var_red
# 1               f4b_temp 3.560891e+00
# 2               f4b_resp 3.499576e+00
# 3               base_age 3.427866e+00
# 4                f4b_haz 3.301354e+00
# 5          f4a_ppl_house 1.949479e+00
# 6          f4a_share_fac 1.583759e+00
# 7           f4a_drh_days 1.464608e+00
# 8          f4a_slp_rooms 1.379671e+00
# 9       f4a_yng_children 1.077951e+00
# 10         f4a_fac_waste 1.070985e+00
# 11         f4a_prim_schl 1.055323e+00
# 12        f4a_disp_feces 8.716739e-01
# 13        f4a_offr_drink 8.542325e-01
# 14        f4a_trt_method 8.179730e-01
# 15             f4b_admit 8.106868e-01
# 16          f4a_wash_use 7.926390e-01
# 17         f4b_recommend 7.921092e-01
# 18                  site 7.711108e-01
# 19        f4a_max_stools 7.108112e-01
# 20           f3_drh_hosp 6.461989e-01
# 21         f4a_breastfed 5.765864e-01
# 22        f4a_house_bike 5.629933e-01
# 23          f4a_dad_live 5.338499e-01
# 24              f4b_eyes 5.195558e-01
# 25  f4a_drh_lethrgy_miss 5.147505e-01
# 26          f4a_ms_water 5.129289e-01
# 27            f4b_mental 5.126772e-01
# 28           f4a_ani_cow 5.123840e-01
# 29       f4a_ani_rodents 5.111276e-01
# 30      f4a_house_agland 5.101686e-01
# 31              f4b_skin 5.076818e-01
# 32     f4a_drh_bellypain 4.880915e-01
# 33       f4a_hometrt_ors 4.875649e-01
# 34             f4b_mouth 4.857330e-01
# 35             f3_gender 4.852100e-01
# 36      f4a_seek_outside 4.663703e-01
# 37        f4a_wash_child 4.642965e-01
# 38         f4a_drh_vomit 4.619977e-01
# 39        f4a_wash_nurse 4.553206e-01
# 40   f4a_water_prospring 4.537938e-01
# 41         f4a_drh_blood 4.531293e-01
# 42         f4a_wash_cook 4.497603e-01
# 43        f4a_house_tele 4.429621e-01
# 44          f4a_ani_goat 4.392816e-01
# 45       f4a_cur_thirsty 4.291803e-01
# 46           f4a_ani_dog 4.274485e-01
# 47       f4a_house_phone 4.262458e-01
# 48     f4a_hometrt_othr1 4.157590e-01
# 49          f4b_abn_hair 4.154961e-01
# 50    f4a_drh_cough_miss 4.147069e-01
# 51           f4a_ani_cat 4.120666e-01
# 52      f4b_nature_stool 4.108229e-01
# 53         f4a_fuel_crop 4.041207e-01
# 54        f4a_house_elec 4.038444e-01
# 55        f4a_drh_thirst 3.970740e-01
# 56          f4a_ani_fowl 3.965981e-01
# 57          f4a_drh_conv 3.819048e-01
# 58      f4a_drh_restless 3.819022e-01
# 59       f4a_store_water 3.810028e-01
# 60       f4a_house_radio 3.736306e-01
# 61         f4a_fuel_dung 3.706468e-01
# 62   f4a_water_shallwell 3.671344e-01
# 63             f3_drh_iv 3.657761e-01
# 64       f4a_water_avail 3.622560e-01
# 65        f4a_water_yard 3.605168e-01
# 66             f4a_floor 3.589927e-01
# 67      f4a_cur_restless 3.581521e-01
# 68        f4a_hometrt_ab 3.578686e-01
# 69     f4a_hometrt_othr2 3.541929e-01
# 70         f4a_fuel_wood 3.466299e-01
# 71          f4a_cur_skin 3.396887e-01
# 72         f4a_trt_water 3.376535e-01
# 73      f4a_water_pubtap 3.369539e-01
# 74      f4a_hometrt_none 3.304266e-01
# 75     f4a_hometrt_maize 3.235107e-01
# 76        f4a_seek_remdy 3.221500e-01
# 77      f4a_cur_drymouth 3.164679e-01
# 78        f4a_fuel_grass 2.998015e-01
# 79         f4a_drh_consc 2.865016e-01
# 80        f4a_drh_strain 2.829683e-01
# 81        f4a_seek_pharm 2.821870e-01
# 82         f4a_ani_sheep 2.811821e-01
# 83          f4a_wash_def 2.754277e-01
# 84    f4a_water_deepwell 2.658006e-01
# 85      f4a_fuel_propane 2.611460e-01
# 86          f4a_seek_doc 2.599137e-01
# 87     f4a_fuel_charcoal 2.469480e-01
# 88         f4a_fuel_coal 2.441991e-01
# 89         f4a_wash_othr 2.399156e-01
# 90       f4a_wash_animal 2.375173e-01
# 91         f3_drh_turgor 2.304327e-01
# 92        f4a_house_cart 2.300699e-01
# 93        f4a_water_pond 2.273418e-01
# 94      f4a_house_fridge 2.244314e-01
# 95       f4a_house_scoot 2.211406e-01
# 96      f4a_drh_prolapse 2.113521e-01
# 97            f4a_ani_no 2.101622e-01
# 98    f4a_cur_fastbreath 2.067408e-01
# 99       f4a_fuel_biogas 2.034136e-01
# 100        f4a_ani_other 2.003748e-01
# 101       f4a_house_boat 1.952322e-01
# 102         f4a_wash_eat 1.887154e-01
# 103    f4a_drh_lessdrink 1.871728e-01
# 104    f4a_water_pubwell 1.869444e-01
# 105       f4b_under_nutr 1.815522e-01
# 106        f4a_fuel_kero 1.754189e-01
# 107    f4a_water_covwell 1.742085e-01
# 108       f4a_water_rain 1.601085e-01
# 109      f4a_water_river 1.579681e-01
# 110       f4a_seek_other 1.550679e-01
# 111       f4a_house_none 1.512850e-01
# 112       f4a_water_well 1.463966e-01
# 113     f4a_hometrt_zinc 1.432330e-01
# 114     f4a_relationship 1.390430e-01
# 115     f4a_hometrt_herb 1.238964e-01
# 116   f4a_water_unspring 1.190838e-01
# 117      f4a_seek_healer 1.015796e-01
# 118      f4a_water_house 9.558586e-02
# 119      f4a_fuel_natgas 9.178065e-02
# 120       f4a_water_bore 8.631637e-02
# 121        f4a_house_car 8.098350e-02
# 122   f4a_water_covpwell 7.297897e-02
# 123     f4a_seek_privdoc 5.158965e-02
# 124     f4a_water_bought 4.005633e-02
# 125      f4b_chest_indrw 7.385087e-03
# 126           f4b_rectal 5.534925e-03
# 127  f4a_hometrt_othrliq 2.790147e-03
# 128        f4a_fuel_elec 2.375000e-03
# 129       f4a_fuel_other 9.665995e-04
# 130          f4b_bipedal 8.333333e-04
# 131       f4a_water_othr 4.266667e-04
# 132     f4a_hometrt_milk 1.398601e-05
# 133      f4a_seek_friend 0.000000e+00
# 134       f4b_skin_flaky 0.000000e+00
# 135    f4b_observe_stool 0.000000e+00

# AUC          SE     lower     upper level Model nvar
# 1 0.7771496 0.005315955 0.7667305 0.7875687  0.95    LR    5
# 2 0.7610818 0.005423742 0.7504515 0.7717121  0.95    LR   10
# 3 0.7101096 0.005862162 0.6986200 0.7215992  0.95    RF    5
# 4 0.7390029 0.005626804 0.7279745 0.7500312  0.95    RF   10

# nvar  intc intc_LCI intc_UCI slope slope_LCI slope_UCI
# <dbl> <dbl>    <dbl>    <dbl> <dbl>     <dbl>     <dbl>
# 1     5 -0.0279   -0.588    0.457 1.01      0.482      1.61
# 2    10 -0.0161   -0.583    0.476 0.771     0.336      1.26

################### main + month diarrhea growth falter ####
names<-append(x=names, values="month")

main.month <- CPR.funct(data=complete_gf,outcome="haz_0.5",iter=100,nvars_opts=c(5,10))
main.month[["df_imps"]]
main.month[["AUC_df"]]
main.month[["calib"]]

# names    var_red
# 1               base_age 97.9938405
# 2                f4b_haz 81.6886014
# 3               f4b_resp 53.1240884
# 4               f4b_temp 50.0961221
# 5          f4a_ppl_house 37.5065328
# 6                  month 36.5063384
# 7          f4a_slp_rooms 24.7926326
# 8           f4a_drh_days 24.1350434
# 9          f4a_share_fac 23.4606593
# 10         f4a_breastfed 22.8552328
# 11      f4a_yng_children 20.3303948
# 12         f4a_prim_schl 19.4253746
# 13        f4a_offr_drink 16.1206385
# 14         f4b_recommend 13.9364604
# 15        f4a_disp_feces 13.1854731
# 16         f4a_fac_waste 12.6702005
# 17                  site 11.9426076
# 18     f4a_drh_bellypain 11.6510422
# 19        f4a_max_stools 11.3510780
# 20          f4a_ms_water 11.2111055
# 21          f4a_dad_live 11.1527822
# 22        f4a_trt_method 10.0600558
# 23            f4b_mental 10.0264132
# 24         f4a_drh_vomit  9.7735136
# 25       f4a_water_avail  9.6742577
# 26          f4a_wash_use  9.0991580
# 27             f3_gender  8.6537763
# 28             f4b_mouth  8.5945498
# 29    f4a_drh_cough_miss  8.3923552
# 30         f4a_wash_cook  8.2358990
# 31        f4a_wash_nurse  8.1423681
# 32       f4a_cur_thirsty  8.1273816
# 33  f4a_drh_lethrgy_miss  8.0271770
# 34        f4a_wash_child  7.9850845
# 35        f4a_house_bike  7.7662433
# 36        f4a_drh_thirst  7.5889925
# 37          f4a_wash_def  7.5099368
# 38      f4a_hometrt_none  7.4623344
# 39        f4a_house_tele  7.4386094
# 40      f4a_seek_outside  7.4289579
# 41      f4a_cur_drymouth  7.3935154
# 42      f4a_cur_restless  7.2968682
# 43      f4a_drh_restless  7.2802192
# 44        f4a_house_elec  7.2329716
# 45              f4b_skin  7.2090861
# 46             f3_drh_iv  7.1884818
# 47          f4a_ani_fowl  7.1325264
# 48       f4a_hometrt_ors  7.1029979
# 49       f4a_house_radio  6.9362524
# 50       f4a_ani_rodents  6.8706593
# 51      f4a_house_agland  6.8687948
# 52          f4a_cur_skin  6.8574607
# 53       f4a_house_phone  6.8557477
# 54       f4a_store_water  6.8222923
# 55           f4a_ani_dog  6.8078864
# 56           f3_drh_hosp  6.7038253
# 57           f4a_ani_cat  6.6449137
# 58     f4a_drh_lessdrink  6.6225961
# 59          f4a_ani_goat  6.4112778
# 60             f4a_floor  6.3578339
# 61             f4b_admit  6.2991007
# 62           f4a_ani_cow  6.2635911
# 63         f3_drh_turgor  6.2016170
# 64          f4a_wash_eat  6.1424527
# 65         f4a_trt_water  6.1210167
# 66         f4a_fuel_wood  5.8992872
# 67        f4a_drh_strain  5.8035292
# 68      f4a_water_pubtap  5.7654320
# 69     f4a_hometrt_othr1  5.7241516
# 70       f4a_house_scoot  5.7063006
# 71     f4a_hometrt_maize  5.5418884
# 72         f4a_drh_blood  5.4996301
# 73     f4a_fuel_charcoal  5.4963829
# 74    f4a_cur_fastbreath  5.4155967
# 75         f4a_ani_sheep  5.3272583
# 76        f4a_seek_pharm  5.2345961
# 77        f4a_hometrt_ab  5.2143141
# 78      f4b_nature_stool  4.9384577
# 79        f4b_under_nutr  4.8829670
# 80         f4a_ani_other  4.8151057
# 81       f4a_wash_animal  4.7221042
# 82    f4a_water_deepwell  4.6647390
# 83      f4a_house_fridge  4.5268852
# 84      f4a_hometrt_herb  4.4325510
# 85              f4b_eyes  4.3417326
# 86        f4a_house_cart  4.1311563
# 87         f4a_wash_othr  4.0662348
# 88            f4a_ani_no  3.9919818
# 89         f4a_fuel_kero  3.8215115
# 90        f4a_water_yard  3.6622944
# 91     f4a_water_pubwell  3.6555662
# 92        f4a_water_bore  3.6406275
# 93     f4a_hometrt_othr2  3.4240043
# 94   f4a_water_shallwell  3.3015541
# 95         f4a_house_car  3.2557316
# 96         f4a_fuel_dung  3.1909530
# 97        f4a_fuel_grass  3.1013521
# 98        f4a_seek_remdy  3.0899688
# 99         f4a_fuel_crop  3.0748838
# 100    f4a_water_covwell  3.0578238
# 101        f4a_drh_consc  3.0447508
# 102     f4a_seek_privdoc  3.0327145
# 103         f4a_seek_doc  2.9916596
# 104      f4a_fuel_natgas  2.7919018
# 105     f4a_water_bought  2.7742023
# 106     f4a_relationship  2.7620796
# 107      f4a_water_house  2.7052420
# 108       f4a_water_rain  2.6945551
# 109      f4a_seek_healer  2.6745207
# 110      f4b_chest_indrw  2.6435013
# 111     f4a_hometrt_zinc  2.6092073
# 112         f4b_abn_hair  2.5994720
# 113       f4a_water_well  2.5695628
# 114        f4a_fuel_coal  2.4368523
# 115      f4a_fuel_biogas  2.3931781
# 116     f4a_fuel_propane  2.3407361
# 117      f4a_water_river  2.1992057
# 118       f4a_seek_other  2.0805619
# 119       f4a_house_none  1.8531829
# 120  f4a_water_prospring  1.8482442
# 121   f4a_water_covpwell  1.7836295
# 122         f4a_drh_conv  1.5703359
# 123       f4a_water_pond  1.4236446
# 124       f4a_water_othr  1.2527796
# 125  f4a_hometrt_othrliq  1.2222853
# 126       f4b_skin_flaky  1.1589538
# 127     f4a_drh_prolapse  1.1486495
# 128          f4b_bipedal  1.1006216
# 129       f4a_house_boat  1.0143971
# 130       f4a_fuel_other  0.9250642
# 131   f4a_water_unspring  0.7729656
# 132     f4a_hometrt_milk  0.7216479
# 133      f4a_seek_friend  0.3525807
# 134    f4b_observe_stool  0.3052048
# 135        f4a_fuel_elec  0.2839745
# 136           f4b_rectal  0.1689034



# AUC          SE     lower     upper level Model nvar
# 1 0.7197632 0.001419507 0.7169810 0.7225454  0.95    LR    5
# 2 0.7202599 0.001417153 0.7174823 0.7230374  0.95    LR   10
# 3 0.6984382 0.001456699 0.6955831 0.7012932  0.95    RF    5
# 4 0.7167280 0.001414212 0.7139562 0.7194998  0.95    RF   10

# nvar    intc intc_LCI intc_UCI slope slope_LCI slope_UCI
# <dbl>   <dbl>    <dbl>    <dbl> <dbl>     <dbl>     <dbl>
# 1     5 0.000132    -0.128    0.126 0.990     0.823      1.16
# 2    10 0.0000518   -0.128    0.126 0.966     0.804      1.14

################### main + shigella AF 0.5 growth falter ####
names<-append(x=names, values="shigella_afe")

main.shig0.5 <- CPR.funct(data=complete_gf_afe,outcome="haz_0.5",iter=100,nvars_opts=c(5,10))
main.shig0.5[["df_imps"]]
main.shig0.5[["AUC_df"]]
main.shig0.5[["calib"]]

# names    var_red
# 1               base_age 57.0494395
# 2                f4b_haz 40.6342207
# 3               f4b_resp 28.6593058
# 4               f4b_temp 27.2785632
# 5          f4a_ppl_house 20.5890593
# 6           f4a_drh_days 13.2299271
# 7          f4a_breastfed 13.2292377
# 8          f4a_slp_rooms 12.8987425
# 9          f4a_share_fac 12.2757554
# 10      f4a_yng_children 11.2095648
# 11         f4a_prim_schl 10.2693563
# 12        f4a_offr_drink  8.3205887
# 13         f4b_recommend  8.2536779
# 14        f4a_disp_feces  8.0510847
# 15         f4a_fac_waste  7.3059586
# 16                  site  6.8977045
# 17        f4a_max_stools  6.4475252
# 18          f4a_dad_live  6.4385420
# 19          f4a_ms_water  6.1856734
# 20     f4a_drh_bellypain  6.1103278
# 21            f4b_mental  5.9299035
# 22        f4a_trt_method  5.8645556
# 23       f4a_water_avail  5.6841704
# 24         f4a_drh_vomit  5.5558256
# 25         f4a_wash_cook  5.0156114
# 26          f4a_wash_use  4.6933877
# 27          f4a_ani_goat  4.5772603
# 28             f3_gender  4.5629326
# 29  f4a_drh_lethrgy_miss  4.5051004
# 30    f4a_drh_cough_miss  4.4999110
# 31        f4a_wash_nurse  4.4820225
# 32             f4b_mouth  4.4689275
# 33        f4a_wash_child  4.3374662
# 34        f4a_drh_thirst  4.3199537
# 35       f4a_cur_thirsty  4.2441487
# 36      f4a_seek_outside  4.1980417
# 37          f4a_wash_def  4.1699770
# 38          f4a_cur_skin  4.1629597
# 39              f4b_skin  4.1403848
# 40        f4a_house_tele  4.0842141
# 41             f3_drh_iv  4.0751245
# 42           f3_drh_hosp  4.0193687
# 43      f4a_hometrt_none  4.0030044
# 44      f4a_house_agland  3.9823169
# 45      f4a_cur_drymouth  3.9586524
# 46       f4a_hometrt_ors  3.9519272
# 47       f4a_house_phone  3.9417032
# 48           f4a_ani_cat  3.9085297
# 49          shigella_afe  3.8855104
# 50          f4a_ani_fowl  3.8726536
# 51        f4a_house_bike  3.8260261
# 52        f4a_house_elec  3.8187190
# 53      f4a_drh_restless  3.8133297
# 54      f4a_cur_restless  3.7816933
# 55           f4a_ani_cow  3.7740581
# 56       f4a_ani_rodents  3.6985155
# 57           f4a_ani_dog  3.6897787
# 58          f4a_wash_eat  3.6855945
# 59             f4b_admit  3.6559356
# 60         f3_drh_turgor  3.6470477
# 61       f4a_house_radio  3.5665113
# 62       f4a_store_water  3.5543781
# 63        f4a_drh_strain  3.5336602
# 64    f4a_cur_fastbreath  3.5086066
# 65     f4a_drh_lessdrink  3.5081907
# 66         f4a_trt_water  3.4858542
# 67      f4a_water_pubtap  3.3397171
# 68     f4a_hometrt_othr1  3.3147605
# 69             f4a_floor  3.2804186
# 70         f4a_fuel_wood  3.2444138
# 71     f4a_hometrt_maize  3.2314626
# 72         f4a_drh_blood  3.2026283
# 73       f4a_house_scoot  2.8923307
# 74        f4a_seek_pharm  2.8839615
# 75         f4a_ani_sheep  2.8279565
# 76       f4a_wash_animal  2.7302267
# 77     f4a_fuel_charcoal  2.7132817
# 78    f4a_water_deepwell  2.6817765
# 79              f4b_eyes  2.6745297
# 80         f4a_ani_other  2.6691914
# 81        f4a_hometrt_ab  2.6437754
# 82        f4b_under_nutr  2.6203756
# 83      f4b_nature_stool  2.6173362
# 84        f4a_house_cart  2.5856955
# 85      f4a_house_fridge  2.5379582
# 86        f4a_water_bore  2.4024091
# 87      f4a_hometrt_herb  2.3524290
# 88         f4a_wash_othr  2.2957001
# 89            f4a_ani_no  2.1837274
# 90         f4a_fuel_kero  2.0787319
# 91        f4a_water_yard  2.0668014
# 92         f4a_drh_consc  2.0186091
# 93     f4a_water_pubwell  2.0063314
# 94   f4a_water_shallwell  1.9404776
# 95        f4a_fuel_grass  1.9306972
# 96         f4a_fuel_dung  1.8968287
# 97        f4a_seek_remdy  1.8879771
# 98          f4b_abn_hair  1.8726095
# 99        f4a_water_well  1.8652135
# 100         f4a_seek_doc  1.8431490
# 101        f4a_house_car  1.8206770
# 102        f4a_fuel_crop  1.8160310
# 103    f4a_hometrt_othr2  1.7869975
# 104     f4a_seek_privdoc  1.6847182
# 105      f4a_seek_healer  1.5838699
# 106       f4a_seek_other  1.5412276
# 107     f4a_relationship  1.5162807
# 108    f4a_water_covwell  1.5061171
# 109      f4a_fuel_natgas  1.4742158
# 110       f4a_water_rain  1.4612237
# 111     f4a_hometrt_zinc  1.4267250
# 112      f4a_water_house  1.3718378
# 113     f4a_water_bought  1.3643083
# 114      f4a_fuel_biogas  1.2484949
# 115     f4a_fuel_propane  1.2341546
# 116      f4b_chest_indrw  1.2185538
# 117        f4a_fuel_coal  1.2006512
# 118      f4a_water_river  1.1429302
# 119       f4a_house_none  1.0712340
# 120   f4a_water_covpwell  1.0644131
# 121         f4a_drh_conv  1.0349785
# 122     f4a_drh_prolapse  0.9972541
# 123       f4b_skin_flaky  0.8830613
# 124  f4a_water_prospring  0.8700294
# 125  f4a_hometrt_othrliq  0.8172315
# 126       f4a_water_othr  0.6845860
# 127       f4a_water_pond  0.6168120
# 128   f4a_water_unspring  0.5340368
# 129       f4a_house_boat  0.4712394
# 130       f4a_fuel_other  0.4526727
# 131          f4b_bipedal  0.3355860
# 132      f4a_seek_friend  0.2896100
# 133     f4a_hometrt_milk  0.2049610
# 134           f4b_rectal  0.1846762
# 135        f4a_fuel_elec  0.1299797
# 136    f4b_observe_stool  0.0000000


# AUC          SE     lower     upper level Model nvar
# 1 0.7314349 0.001865458 0.7277787 0.7350912  0.95    LR    5
# 2 0.7305062 0.001872962 0.7268353 0.7341772  0.95    LR   10
# 3 0.7097553 0.001924280 0.7059838 0.7135268  0.95    RF    5
# 4 0.7231357 0.001873601 0.7194635 0.7268079  0.95    RF   10


# nvar    intc intc_LCI intc_UCI slope slope_LCI slope_UCI
# <dbl>   <dbl>    <dbl>    <dbl> <dbl>     <dbl>     <dbl>
# 1     5 -0.00466   -0.180    0.167 0.979     0.768      1.21
# 2    10 -0.00367   -0.180    0.168 0.963     0.753      1.19

#intercept <0, overestimation
#intercept >0, underestimation
#slopes <1, estimated risks too extreme in both directions
#slope >1 risk estimates to moderate
#slightly positive slope indicates slight underestimation

################### main + crypto AF 0.5 growth falter ####
names<-append(x=names, values="crypto_afe")

main.crypto0.5 <- CPR.funct(data=complete_gf_afe,outcome="haz_0.5",iter=100,nvars_opts=c(5,10))
main.crypto0.5[["df_imps"]]
main.crypto0.5[["AUC_df"]]
main.crypto0.5[["calib"]]

# names    var_red
# 1               base_age 55.1060617
# 2                f4b_haz 41.0699278
# 3               f4b_resp 28.6586743
# 4               f4b_temp 27.6120472
# 5          f4a_ppl_house 19.8988240
# 6           f4a_drh_days 13.3158726
# 7          f4a_breastfed 13.2867162
# 8          f4a_slp_rooms 12.9260023
# 9          f4a_share_fac 12.3611425
# 10      f4a_yng_children 11.0271423
# 11         f4a_prim_schl 10.3606525
# 12        f4a_offr_drink  8.5532491
# 13         f4b_recommend  8.2269706
# 14        f4a_disp_feces  7.9746150
# 15         f4a_fac_waste  7.3252787
# 16                  site  6.9423449
# 17          f4a_ms_water  6.4704978
# 18          f4a_dad_live  6.3130851
# 19        f4a_max_stools  6.2960153
# 20            crypto_afe  6.1920229
# 21     f4a_drh_bellypain  6.1916933
# 22            f4b_mental  6.0097453
# 23        f4a_trt_method  5.7752189
# 24         f4a_drh_vomit  5.6671054
# 25       f4a_water_avail  5.6537439
# 26         f4a_wash_cook  5.0916558
# 27             f3_gender  4.6295971
# 28          f4a_wash_use  4.6175563
# 29    f4a_drh_cough_miss  4.5275125
# 30             f4b_mouth  4.5060674
# 31          f4a_ani_goat  4.4855377
# 32        f4a_wash_nurse  4.4783186
# 33  f4a_drh_lethrgy_miss  4.4079711
# 34        f4a_wash_child  4.3814145
# 35      f4a_seek_outside  4.3321579
# 36       f4a_cur_thirsty  4.3033473
# 37       f4a_hometrt_ors  4.2823548
# 38              f4b_skin  4.2618088
# 39        f4a_drh_thirst  4.2587975
# 40        f4a_house_tele  4.1644636
# 41             f3_drh_iv  4.1469986
# 42      f4a_cur_drymouth  4.0911349
# 43          f4a_wash_def  4.0882615
# 44      f4a_hometrt_none  4.0779046
# 45           f3_drh_hosp  4.0010797
# 46       f4a_house_phone  3.9939688
# 47          f4a_cur_skin  3.9350789
# 48        f4a_house_bike  3.9344649
# 49        f4a_house_elec  3.9119661
# 50      f4a_cur_restless  3.8729560
# 51      f4a_house_agland  3.8674663
# 52          f4a_ani_fowl  3.8446115
# 53      f4a_drh_restless  3.8213254
# 54           f4a_ani_cow  3.8110144
# 55       f4a_ani_rodents  3.7934390
# 56         f3_drh_turgor  3.7522420
# 57           f4a_ani_cat  3.7356161
# 58             f4b_admit  3.7237986
# 59           f4a_ani_dog  3.6911791
# 60          f4a_wash_eat  3.5928344
# 61       f4a_store_water  3.5162015
# 62    f4a_cur_fastbreath  3.4323109
# 63     f4a_drh_lessdrink  3.4303607
# 64         f4a_trt_water  3.3885186
# 65             f4a_floor  3.3881414
# 66       f4a_house_radio  3.3595048
# 67     f4a_hometrt_othr1  3.3574916
# 68         f4a_drh_blood  3.3218248
# 69        f4a_drh_strain  3.3044419
# 70      f4a_water_pubtap  3.2147586
# 71         f4a_fuel_wood  3.0990622
# 72     f4a_hometrt_maize  3.0701925
# 73       f4a_house_scoot  2.9838874
# 74         f4a_ani_other  2.8911461
# 75         f4a_ani_sheep  2.8796069
# 76        f4a_seek_pharm  2.8776050
# 77      f4b_nature_stool  2.7535815
# 78     f4a_fuel_charcoal  2.7289049
# 79              f4b_eyes  2.6654926
# 80        f4a_hometrt_ab  2.6524897
# 81       f4a_wash_animal  2.6244830
# 82    f4a_water_deepwell  2.6145232
# 83        f4b_under_nutr  2.5493283
# 84        f4a_water_bore  2.4654979
# 85        f4a_house_cart  2.4145908
# 86      f4a_house_fridge  2.3908674
# 87      f4a_hometrt_herb  2.3499059
# 88         f4a_wash_othr  2.2860971
# 89            f4a_ani_no  2.1117052
# 90        f4a_water_yard  2.0617694
# 91         f4a_fuel_kero  1.9907556
# 92         f4a_fuel_crop  1.9406881
# 93          f4a_seek_doc  1.9352717
# 94         f4a_fuel_dung  1.9218041
# 95         f4a_drh_consc  1.9020284
# 96     f4a_water_pubwell  1.8809407
# 97          f4b_abn_hair  1.8484320
# 98        f4a_fuel_grass  1.8324154
# 99        f4a_seek_remdy  1.8289437
# 100        f4a_house_car  1.8289310
# 101  f4a_water_shallwell  1.8146925
# 102       f4a_water_well  1.8133947
# 103     f4a_seek_privdoc  1.7785529
# 104    f4a_hometrt_othr2  1.7392972
# 105    f4a_water_covwell  1.5391369
# 106      f4a_seek_healer  1.5383901
# 107       f4a_water_rain  1.4851387
# 108      f4a_fuel_natgas  1.4648442
# 109       f4a_seek_other  1.4548560
# 110     f4a_relationship  1.4524692
# 111      f4a_water_house  1.4462485
# 112     f4a_hometrt_zinc  1.4418198
# 113     f4a_water_bought  1.3538503
# 114        f4a_fuel_coal  1.2676359
# 115     f4a_fuel_propane  1.2207512
# 116      f4b_chest_indrw  1.2124333
# 117      f4a_water_river  1.1611682
# 118      f4a_fuel_biogas  1.1114474
# 119     f4a_drh_prolapse  1.0765786
# 120       f4a_house_none  1.0511193
# 121   f4a_water_covpwell  1.0096542
# 122         f4a_drh_conv  1.0031642
# 123       f4b_skin_flaky  0.8526488
# 124  f4a_water_prospring  0.7895383
# 125  f4a_hometrt_othrliq  0.7705575
# 126       f4a_water_othr  0.6673161
# 127       f4a_water_pond  0.6321844
# 128   f4a_water_unspring  0.5610474
# 129       f4a_house_boat  0.5024744
# 130       f4a_fuel_other  0.4603426
# 131          f4b_bipedal  0.3398233
# 132      f4a_seek_friend  0.3119968
# 133     f4a_hometrt_milk  0.1794074
# 134           f4b_rectal  0.1564060
# 135        f4a_fuel_elec  0.1367912
# 136    f4b_observe_stool  0.0000000

# AUC          SE     lower     upper level Model nvar
# 1 0.7304815 0.001865060 0.7268260 0.7341369  0.95    LR    5
# 2 0.7297970 0.001872766 0.7261264 0.7334675  0.95    LR   10
# 3 0.7083054 0.001926788 0.7045290 0.7120818  0.95    RF    5
# 4 0.7220771 0.001878686 0.7183949 0.7257593  0.95    RF   10

# nvar   intc intc_LCI intc_UCI slope slope_LCI slope_UCI
# <dbl>  <dbl>    <dbl>    <dbl> <dbl>     <dbl>     <dbl>
# 1     5 -0.0105   -0.186    0.161 0.992     0.777      1.22
# 2    10 -0.0114   -0.187    0.160 0.977     0.764      1.20


################### main + shigella&crypto AF 0.5 growth falter ####
names<-append(x=names, values=c("shigella_afe","crypto_afe"))

main.shigcrypt0.5 <- CPR.funct(data=complete_gf_afe,outcome="haz_0.5",iter=100,nvars_opts=c(5,10))
main.shigcrypt0.5[["df_imps"]]
main.shigcrypt0.5[["AUC_df"]]
main.shigcrypt0.5[["calib"]]

# names    var_red
# 1               base_age 55.1269102
# 2                f4b_haz 40.2859760
# 3               f4b_resp 28.7296293
# 4               f4b_temp 27.4439114
# 5          f4a_ppl_house 20.1059697
# 6          f4a_breastfed 13.2746468
# 7           f4a_drh_days 13.1475185
# 8          f4a_slp_rooms 12.9023443
# 9          f4a_share_fac 12.1194600
# 10      f4a_yng_children 11.0752917
# 11         f4a_prim_schl 10.5677701
# 12         f4b_recommend  8.2618693
# 13        f4a_offr_drink  8.0564437
# 14        f4a_disp_feces  7.9715413
# 15         f4a_fac_waste  7.1625345
# 16                  site  7.0012580
# 17          f4a_dad_live  6.4117366
# 18        f4a_max_stools  6.2789676
# 19          f4a_ms_water  6.2575585
# 20     f4a_drh_bellypain  6.2219136
# 21            f4b_mental  6.1123034
# 22            crypto_afe  5.9205538
# 23        f4a_trt_method  5.6515222
# 24         f4a_drh_vomit  5.5932538
# 25       f4a_water_avail  5.4875112
# 26         f4a_wash_cook  4.7471333
# 27          f4a_wash_use  4.6309650
# 28             f3_gender  4.6006687
# 29    f4a_drh_cough_miss  4.5125529
# 30        f4a_wash_nurse  4.4986793
# 31          f4a_ani_goat  4.4885092
# 32             f4b_mouth  4.4702985
# 33  f4a_drh_lethrgy_miss  4.3961866
# 34              f4b_skin  4.3719462
# 35        f4a_wash_child  4.3718758
# 36       f4a_cur_thirsty  4.3057066
# 37             f3_drh_iv  4.1411814
# 38      f4a_seek_outside  4.1379196
# 39          f4a_wash_def  4.1363454
# 40       f4a_hometrt_ors  4.1092366
# 41        f4a_drh_thirst  4.0856041
# 42        f4a_house_tele  4.0781807
# 43           f3_drh_hosp  4.0673269
# 44       f4a_house_phone  4.0461049
# 45      f4a_hometrt_none  3.9953807
# 46        f4a_house_bike  3.9870369
# 47      f4a_cur_drymouth  3.9773018
# 48          shigella_afe  3.9027592
# 49        f4a_house_elec  3.9024074
# 50          f4a_ani_fowl  3.8686321
# 51      f4a_cur_restless  3.8546189
# 52      f4a_drh_restless  3.8340873
# 53      f4a_house_agland  3.8305467
# 54          f4a_cur_skin  3.7864979
# 55           f4a_ani_dog  3.7801328
# 56           f4a_ani_cat  3.7636173
# 57           f4a_ani_cow  3.7348202
# 58          f4a_wash_eat  3.6932803
# 59             f4b_admit  3.6872358
# 60       f4a_house_radio  3.6000348
# 61         f3_drh_turgor  3.5741396
# 62    f4a_cur_fastbreath  3.5597515
# 63       f4a_ani_rodents  3.5442168
# 64     f4a_drh_lessdrink  3.5105705
# 65         f4a_trt_water  3.4827829
# 66       f4a_store_water  3.4821996
# 67     f4a_hometrt_othr1  3.3912079
# 68        f4a_drh_strain  3.3803398
# 69      f4a_water_pubtap  3.3376875
# 70             f4a_floor  3.3346825
# 71         f4a_drh_blood  3.2619678
# 72         f4a_fuel_wood  3.1745202
# 73     f4a_hometrt_maize  3.1073149
# 74       f4a_house_scoot  3.0019316
# 75         f4a_ani_other  2.8772891
# 76        f4a_seek_pharm  2.8486340
# 77     f4a_fuel_charcoal  2.7772907
# 78         f4a_ani_sheep  2.7728249
# 79       f4a_wash_animal  2.7642379
# 80    f4a_water_deepwell  2.7286187
# 81        f4a_hometrt_ab  2.6284848
# 82      f4b_nature_stool  2.5641501
# 83        f4b_under_nutr  2.5628831
# 84              f4b_eyes  2.5336500
# 85      f4a_house_fridge  2.4872601
# 86        f4a_house_cart  2.4310984
# 87        f4a_water_bore  2.4275644
# 88      f4a_hometrt_herb  2.2391554
# 89         f4a_wash_othr  2.2278342
# 90        f4a_water_yard  2.0867720
# 91            f4a_ani_no  2.0765399
# 92     f4a_water_pubwell  1.9908529
# 93         f4a_fuel_kero  1.9771469
# 94         f4a_fuel_crop  1.9450563
# 95         f4a_drh_consc  1.9325690
# 96        f4a_seek_remdy  1.9227123
# 97          f4b_abn_hair  1.9179659
# 98          f4a_seek_doc  1.9048430
# 99   f4a_water_shallwell  1.8916484
# 100        f4a_fuel_dung  1.8513168
# 101       f4a_water_well  1.8458221
# 102       f4a_fuel_grass  1.7660496
# 103    f4a_hometrt_othr2  1.7172777
# 104        f4a_house_car  1.6977183
# 105     f4a_seek_privdoc  1.6890951
# 106      f4a_seek_healer  1.5533077
# 107      f4a_fuel_natgas  1.5007794
# 108       f4a_seek_other  1.4929757
# 109    f4a_water_covwell  1.4779761
# 110     f4a_water_bought  1.4351686
# 111       f4a_water_rain  1.4082523
# 112     f4a_relationship  1.4062404
# 113      f4a_water_house  1.3659695
# 114     f4a_hometrt_zinc  1.3514128
# 115        f4a_fuel_coal  1.2596229
# 116      f4b_chest_indrw  1.2364130
# 117     f4a_fuel_propane  1.1871295
# 118      f4a_water_river  1.1404078
# 119      f4a_fuel_biogas  1.1387021
# 120       f4a_house_none  1.0791803
# 121         f4a_drh_conv  1.0305487
# 122     f4a_drh_prolapse  0.9487298
# 123   f4a_water_covpwell  0.9034166
# 124       f4b_skin_flaky  0.8434971
# 125  f4a_water_prospring  0.7838021
# 126  f4a_hometrt_othrliq  0.7427696
# 127       f4a_water_othr  0.6113146
# 128       f4a_water_pond  0.5930872
# 129   f4a_water_unspring  0.5116054
# 130       f4a_house_boat  0.4468190
# 131       f4a_fuel_other  0.4414032
# 132          f4b_bipedal  0.3456505
# 133      f4a_seek_friend  0.2760228
# 134     f4a_hometrt_milk  0.1881810
# 135           f4b_rectal  0.1778421
# 136        f4a_fuel_elec  0.1245471
# 137    f4b_observe_stool  0.0000000

# AUC          SE     lower     upper level Model nvar
# 1 0.7325504 0.001865964 0.7288931 0.7362076  0.95    LR    5
# 2 0.7316322 0.001873639 0.7279599 0.7353044  0.95    LR   10
# 3 0.7120650 0.001919332 0.7083032 0.7158268  0.95    RF    5
# 4 0.7251971 0.001871707 0.7215287 0.7288656  0.95    RF   10

# nvar   intc intc_LCI intc_UCI slope slope_LCI slope_UCI
# <dbl>  <dbl>    <dbl>    <dbl> <dbl>     <dbl>     <dbl>
# 1     5 0.00247   -0.173    0.174 0.991     0.778      1.22
# 2    10 0.00338   -0.172    0.175 0.974     0.763      1.20

################### main + viral(any) AF 0.5 growth falter ####
names<-append(x=names, values="viral")

main.viral0.5 <- CPR.funct(data=complete_gf_afe,outcome="haz_0.5",iter=100,nvars_opts=c(5,10))
main.viral0.5[["df_imps"]]
main.viral0.5[["AUC_df"]]
main.viral0.5[["calib"]]

# names    var_red
# 1               base_age 55.7808358
# 2                f4b_haz 40.6829286
# 3               f4b_resp 28.5444883
# 4               f4b_temp 27.6762112
# 5          f4a_ppl_house 20.2416267
# 6           f4a_drh_days 13.1967490
# 7          f4a_breastfed 13.1684771
# 8          f4a_slp_rooms 13.0512310
# 9          f4a_share_fac 12.4845322
# 10      f4a_yng_children 11.2735135
# 11         f4a_prim_schl 10.2428002
# 12         f4b_recommend  8.4052268
# 13        f4a_offr_drink  8.2681945
# 14        f4a_disp_feces  7.7802177
# 15         f4a_fac_waste  7.3086443
# 16                  site  6.9839777
# 17        f4a_max_stools  6.4504413
# 18          f4a_dad_live  6.3409574
# 19          f4a_ms_water  6.2891255
# 20     f4a_drh_bellypain  6.2398535
# 21            f4b_mental  6.0238540
# 22       f4a_water_avail  5.7564567
# 23        f4a_trt_method  5.7467255
# 24         f4a_drh_vomit  5.5622363
# 25         f4a_wash_cook  4.9982737
# 26                 viral  4.8513535
# 27          f4a_wash_use  4.8300480
# 28             f3_gender  4.6822742
# 29             f4b_mouth  4.6228617
# 30    f4a_drh_cough_miss  4.5784984
# 31        f4a_wash_nurse  4.5060836
# 32  f4a_drh_lethrgy_miss  4.4362131
# 33          f4a_wash_def  4.4331569
# 34              f4b_skin  4.4085691
# 35          f4a_ani_goat  4.3882266
# 36       f4a_cur_thirsty  4.3498520
# 37        f4a_wash_child  4.3412019
# 38      f4a_seek_outside  4.2707407
# 39      f4a_hometrt_none  4.1797399
# 40        f4a_house_tele  4.1599457
# 41        f4a_drh_thirst  4.1419039
# 42       f4a_hometrt_ors  4.0298772
# 43             f3_drh_iv  4.0186543
# 44        f4a_house_elec  4.0118002
# 45      f4a_cur_drymouth  3.9982926
# 46          f4a_cur_skin  3.9957119
# 47       f4a_house_phone  3.9940050
# 48           f3_drh_hosp  3.9863389
# 49      f4a_drh_restless  3.9646493
# 50        f4a_house_bike  3.9466337
# 51      f4a_house_agland  3.8797536
# 52          f4a_ani_fowl  3.8700599
# 53       f4a_ani_rodents  3.8284014
# 54      f4a_cur_restless  3.8267743
# 55           f4a_ani_cat  3.8069202
# 56           f4a_ani_dog  3.6805070
# 57       f4a_house_radio  3.6492752
# 58           f4a_ani_cow  3.6444704
# 59          f4a_wash_eat  3.6183592
# 60             f4b_admit  3.6149055
# 61         f3_drh_turgor  3.5829068
# 62    f4a_cur_fastbreath  3.5425337
# 63     f4a_drh_lessdrink  3.5262028
# 64       f4a_store_water  3.4751517
# 65         f4a_trt_water  3.4625563
# 66        f4a_drh_strain  3.3978386
# 67             f4a_floor  3.3695686
# 68     f4a_hometrt_othr1  3.2480335
# 69         f4a_drh_blood  3.2328900
# 70         f4a_fuel_wood  3.2122747
# 71      f4a_water_pubtap  3.1274618
# 72     f4a_hometrt_maize  3.1243351
# 73        f4a_seek_pharm  2.9114976
# 74       f4a_house_scoot  2.8923296
# 75         f4a_ani_sheep  2.8688733
# 76    f4a_water_deepwell  2.7515200
# 77       f4a_wash_animal  2.7514614
# 78     f4a_fuel_charcoal  2.7471336
# 79        f4b_under_nutr  2.7259636
# 80         f4a_ani_other  2.7089451
# 81      f4b_nature_stool  2.6547946
# 82              f4b_eyes  2.6246695
# 83      f4a_house_fridge  2.6042120
# 84        f4a_hometrt_ab  2.5855125
# 85        f4a_house_cart  2.4969025
# 86        f4a_water_bore  2.3930086
# 87      f4a_hometrt_herb  2.3622168
# 88            f4a_ani_no  2.1833395
# 89         f4a_wash_othr  2.1766714
# 90        f4a_water_yard  2.0720373
# 91         f4a_fuel_kero  2.0645206
# 92   f4a_water_shallwell  1.9506148
# 93          f4a_seek_doc  1.9304972
# 94         f4a_fuel_dung  1.9018701
# 95         f4a_fuel_crop  1.8939523
# 96         f4a_drh_consc  1.8842079
# 97     f4a_water_pubwell  1.8804659
# 98        f4a_fuel_grass  1.8722701
# 99        f4a_seek_remdy  1.8515055
# 100    f4a_hometrt_othr2  1.8290679
# 101       f4a_water_well  1.8270238
# 102         f4b_abn_hair  1.8150105
# 103        f4a_house_car  1.7732339
# 104     f4a_seek_privdoc  1.6396397
# 105    f4a_water_covwell  1.5764954
# 106       f4a_seek_other  1.5751816
# 107      f4a_seek_healer  1.5722003
# 108      f4a_fuel_natgas  1.5206362
# 109      f4a_water_house  1.4789532
# 110       f4a_water_rain  1.4287478
# 111     f4a_relationship  1.4237754
# 112     f4a_hometrt_zinc  1.3609545
# 113     f4a_water_bought  1.3429518
# 114     f4a_fuel_propane  1.2754910
# 115      f4a_fuel_biogas  1.2491848
# 116        f4a_fuel_coal  1.2337732
# 117      f4b_chest_indrw  1.2184880
# 118      f4a_water_river  1.1270708
# 119       f4a_house_none  1.1160205
# 120   f4a_water_covpwell  1.0313978
# 121         f4a_drh_conv  0.9918860
# 122       f4b_skin_flaky  0.9546996
# 123     f4a_drh_prolapse  0.9495241
# 124  f4a_water_prospring  0.8155363
# 125  f4a_hometrt_othrliq  0.7380755
# 126       f4a_water_othr  0.6375114
# 127       f4a_water_pond  0.6058321
# 128   f4a_water_unspring  0.5324002
# 129       f4a_house_boat  0.4856359
# 130       f4a_fuel_other  0.4398908
# 131          f4b_bipedal  0.4128322
# 132      f4a_seek_friend  0.3066205
# 133     f4a_hometrt_milk  0.1985053
# 134           f4b_rectal  0.1784710
# 135        f4a_fuel_elec  0.1280094
# 136    f4b_observe_stool  0.0000000


# AUC          SE     lower     upper level Model nvar
# 1 0.7346982 0.001855922 0.7310607 0.7383357  0.95    LR    5
# 2 0.7330519 0.001866122 0.7293944 0.7367094  0.95    LR   10
# 3 0.7109009 0.001927271 0.7071235 0.7146783  0.95    RF    5
# 4 0.7233375 0.001879022 0.7196547 0.7270203  0.95    RF   10

# nvar   intc intc_LCI intc_UCI slope slope_LCI slope_UCI
# <dbl>  <dbl>    <dbl>    <dbl> <dbl>     <dbl>     <dbl>
# 1     5 0.000563   -0.175    0.172 1.00      0.790      1.24
# 2    10 0.000452   -0.175    0.172 0.983     0.771      1.21

################### main + shigella AF 0.3 growth falter ####
names<-append(x=names, values="shigella_afe_0.3")

main.shig0.3 <- CPR.funct(data=complete_gf_afe,outcome="haz_0.5",iter=100,nvars_opts=c(5,10))
main.shig0.3[["df_imps"]]
main.shig0.3[["AUC_df"]]
main.shig0.3[["calib"]]

# names    var_red
# 1               base_age 56.6872000
# 2                f4b_haz 41.4777357
# 3               f4b_resp 28.5465674
# 4               f4b_temp 27.6113061
# 5          f4a_ppl_house 20.0868077
# 6           f4a_drh_days 13.3610351
# 7          f4a_slp_rooms 13.2170273
# 8          f4a_breastfed 13.0823759
# 9          f4a_share_fac 12.3320329
# 10      f4a_yng_children 11.1054546
# 11         f4a_prim_schl 10.3286822
# 12         f4b_recommend  8.4278139
# 13        f4a_offr_drink  8.1197998
# 14        f4a_disp_feces  7.9705005
# 15         f4a_fac_waste  7.1698282
# 16                  site  7.0137285
# 17        f4a_max_stools  6.4893022
# 18          f4a_dad_live  6.4332381
# 19          f4a_ms_water  6.3786896
# 20     f4a_drh_bellypain  6.1786973
# 21            f4b_mental  6.1186123
# 22         f4a_drh_vomit  5.7732632
# 23        f4a_trt_method  5.7626596
# 24       f4a_water_avail  5.5124238
# 25         f4a_wash_cook  4.9938433
# 26             f3_gender  4.7106668
# 27          f4a_ani_goat  4.6139609
# 28          f4a_wash_use  4.5743773
# 29             f4b_mouth  4.5727657
# 30    f4a_drh_cough_miss  4.4652053
# 31  f4a_drh_lethrgy_miss  4.3745535
# 32        f4a_wash_nurse  4.3613399
# 33      shigella_afe_0.3  4.3230928
# 34       f4a_cur_thirsty  4.2673694
# 35        f4a_wash_child  4.2583329
# 36      f4a_seek_outside  4.2360662
# 37       f4a_hometrt_ors  4.2268839
# 38        f4a_house_tele  4.1782875
# 39              f4b_skin  4.1469631
# 40          f4a_wash_def  4.1169058
# 41          f4a_cur_skin  4.0960308
# 42       f4a_house_phone  4.0718704
# 43        f4a_drh_thirst  4.0562872
# 44      f4a_hometrt_none  4.0331944
# 45             f3_drh_iv  4.0022553
# 46           f3_drh_hosp  3.9779907
# 47      f4a_cur_drymouth  3.9440926
# 48        f4a_house_bike  3.9323160
# 49        f4a_house_elec  3.9259176
# 50      f4a_house_agland  3.8682490
# 51       f4a_ani_rodents  3.8662434
# 52      f4a_drh_restless  3.8505138
# 53           f4a_ani_cat  3.8020258
# 54          f4a_ani_fowl  3.7901036
# 55      f4a_cur_restless  3.7855658
# 56           f4a_ani_cow  3.7507558
# 57          f4a_wash_eat  3.7299084
# 58         f3_drh_turgor  3.6998538
# 59     f4a_drh_lessdrink  3.6660376
# 60           f4a_ani_dog  3.5992483
# 61    f4a_cur_fastbreath  3.5989814
# 62             f4b_admit  3.5926970
# 63       f4a_house_radio  3.5743843
# 64       f4a_store_water  3.5537018
# 65         f4a_trt_water  3.4646749
# 66             f4a_floor  3.3585548
# 67     f4a_hometrt_othr1  3.3511181
# 68         f4a_fuel_wood  3.3439318
# 69        f4a_drh_strain  3.3252327
# 70         f4a_drh_blood  3.2184993
# 71      f4a_water_pubtap  3.2112039
# 72     f4a_hometrt_maize  3.2098271
# 73       f4a_house_scoot  3.0102908
# 74        f4a_seek_pharm  2.9085716
# 75     f4a_fuel_charcoal  2.7839173
# 76         f4a_ani_sheep  2.7742811
# 77         f4a_ani_other  2.7649627
# 78    f4a_water_deepwell  2.7312657
# 79      f4b_nature_stool  2.6854456
# 80       f4a_wash_animal  2.6688302
# 81        f4b_under_nutr  2.6597506
# 82        f4a_hometrt_ab  2.6379998
# 83        f4a_house_cart  2.5865297
# 84              f4b_eyes  2.5312488
# 85      f4a_house_fridge  2.4843963
# 86        f4a_water_bore  2.4636869
# 87      f4a_hometrt_herb  2.3394075
# 88            f4a_ani_no  2.2992855
# 89         f4a_wash_othr  2.2375855
# 90   f4a_water_shallwell  2.0614724
# 91        f4a_water_yard  2.0301949
# 92         f4a_fuel_kero  2.0196751
# 93         f4a_drh_consc  1.9975527
# 94          f4b_abn_hair  1.9184131
# 95     f4a_water_pubwell  1.9049300
# 96          f4a_seek_doc  1.9034928
# 97         f4a_fuel_crop  1.8805383
# 98        f4a_seek_remdy  1.8688562
# 99         f4a_fuel_dung  1.8664382
# 100       f4a_water_well  1.8264007
# 101       f4a_fuel_grass  1.8170461
# 102        f4a_house_car  1.7676661
# 103    f4a_hometrt_othr2  1.7674496
# 104     f4a_seek_privdoc  1.6928443
# 105       f4a_seek_other  1.6386350
# 106      f4a_seek_healer  1.5990009
# 107     f4a_relationship  1.5192436
# 108      f4a_fuel_natgas  1.5007710
# 109    f4a_water_covwell  1.4728530
# 110       f4a_water_rain  1.4690521
# 111      f4a_water_house  1.4198063
# 112     f4a_hometrt_zinc  1.4049626
# 113     f4a_water_bought  1.3859253
# 114        f4a_fuel_coal  1.2850350
# 115     f4a_fuel_propane  1.2621335
# 116      f4b_chest_indrw  1.1775084
# 117      f4a_fuel_biogas  1.1140014
# 118      f4a_water_river  1.0990984
# 119       f4a_house_none  1.0431565
# 120         f4a_drh_conv  1.0367163
# 121   f4a_water_covpwell  0.9912256
# 122     f4a_drh_prolapse  0.9724619
# 123       f4b_skin_flaky  0.9004989
# 124  f4a_water_prospring  0.8053723
# 125  f4a_hometrt_othrliq  0.7700664
# 126       f4a_water_othr  0.6444041
# 127       f4a_water_pond  0.6081936
# 128   f4a_water_unspring  0.5083409
# 129       f4a_house_boat  0.4733700
# 130       f4a_fuel_other  0.4669936
# 131          f4b_bipedal  0.3448378
# 132      f4a_seek_friend  0.3263356
# 133     f4a_hometrt_milk  0.1904151
# 134           f4b_rectal  0.1762219
# 135        f4a_fuel_elec  0.1108024
# 136    f4b_observe_stool  0.0000000

# AUC          SE     lower     upper level Model nvar
# 1 0.7334243 0.001857283 0.7297841 0.7370645  0.95    LR    5
# 2 0.7321636 0.001866368 0.7285055 0.7358216  0.95    LR   10
# 3 0.7122453 0.001920228 0.7084818 0.7160089  0.95    RF    5
# 4 0.7255846 0.001866429 0.7219265 0.7292427  0.95    RF   10

# nvar    intc intc_LCI intc_UCI slope slope_LCI slope_UCI
# <dbl>   <dbl>    <dbl>    <dbl> <dbl>     <dbl>     <dbl>
# 1     5 0.00589   -0.169    0.177 1.00      0.789      1.23
# 2    10 0.00615   -0.169    0.177 0.985     0.773      1.21

################### main + crypto AF 0.3 growth falter ####
names<-append(x=names, values="crypto_afe_0.3")

main.crypto0.3 <- CPR.funct(data=complete_gf_afe,outcome="haz_0.5",iter=100,nvars_opts=c(5,10))
main.crypto0.3[["df_imps"]]
main.crypto0.3[["AUC_df"]]
main.crypto0.3[["calib"]]

# names    var_red
# 1               base_age 55.3393759
# 2                f4b_haz 40.5774669
# 3               f4b_resp 28.5474936
# 4               f4b_temp 27.1914612
# 5          f4a_ppl_house 20.1134938
# 6           f4a_drh_days 13.3451915
# 7          f4a_breastfed 13.2696285
# 8          f4a_slp_rooms 12.9632218
# 9          f4a_share_fac 12.4393634
# 10      f4a_yng_children 11.2156612
# 11         f4a_prim_schl 10.3718482
# 12         f4b_recommend  8.3718496
# 13        f4a_offr_drink  8.1804334
# 14        f4a_disp_feces  7.8100621
# 15         f4a_fac_waste  7.2649105
# 16                  site  7.1360512
# 17        f4a_max_stools  6.5334232
# 18        crypto_afe_0.3  6.4163034
# 19          f4a_dad_live  6.3774128
# 20          f4a_ms_water  6.2074387
# 21     f4a_drh_bellypain  6.0060838
# 22            f4b_mental  5.9953262
# 23         f4a_drh_vomit  5.7394620
# 24        f4a_trt_method  5.6056262
# 25       f4a_water_avail  5.5180817
# 26         f4a_wash_cook  4.9065336
# 27             f3_gender  4.7495380
# 28          f4a_wash_use  4.6953691
# 29  f4a_drh_lethrgy_miss  4.5649964
# 30             f4b_mouth  4.4786164
# 31          f4a_ani_goat  4.4661453
# 32        f4a_wash_nurse  4.4468760
# 33    f4a_drh_cough_miss  4.4380293
# 34       f4a_cur_thirsty  4.4199720
# 35        f4a_wash_child  4.3502366
# 36      f4a_seek_outside  4.3410303
# 37              f4b_skin  4.2622323
# 38        f4a_house_tele  4.2417742
# 39       f4a_house_phone  4.2031191
# 40        f4a_drh_thirst  4.1677626
# 41             f3_drh_iv  4.1382643
# 42          f4a_cur_skin  4.1370594
# 43          f4a_wash_def  4.1181978
# 44       f4a_hometrt_ors  4.0957739
# 45      f4a_hometrt_none  4.0340023
# 46      f4a_cur_drymouth  4.0217979
# 47        f4a_house_elec  4.0068152
# 48           f4a_ani_cat  4.0040022
# 49        f4a_house_bike  3.9041259
# 50           f3_drh_hosp  3.8809310
# 51          f4a_ani_fowl  3.8490601
# 52      f4a_house_agland  3.8424773
# 53           f4a_ani_dog  3.8124628
# 54          f4a_wash_eat  3.7751008
# 55      f4a_drh_restless  3.7689358
# 56      f4a_cur_restless  3.7465896
# 57           f4a_ani_cow  3.7395081
# 58       f4a_ani_rodents  3.6594781
# 59       f4a_house_radio  3.6442878
# 60       f4a_store_water  3.6162049
# 61         f3_drh_turgor  3.6023728
# 62             f4b_admit  3.5939552
# 63         f4a_trt_water  3.5572483
# 64     f4a_drh_lessdrink  3.5381022
# 65    f4a_cur_fastbreath  3.4688618
# 66        f4a_drh_strain  3.4154419
# 67             f4a_floor  3.3367690
# 68     f4a_hometrt_othr1  3.3085436
# 69         f4a_drh_blood  3.3038325
# 70         f4a_fuel_wood  3.2772913
# 71      f4a_water_pubtap  3.1233510
# 72     f4a_hometrt_maize  3.1011673
# 73       f4a_house_scoot  2.9765008
# 74        f4a_seek_pharm  2.8845144
# 75         f4a_ani_sheep  2.7254222
# 76    f4a_water_deepwell  2.7197587
# 77      f4b_nature_stool  2.6912215
# 78       f4a_wash_animal  2.6905293
# 79     f4a_fuel_charcoal  2.6882384
# 80              f4b_eyes  2.6354518
# 81         f4a_ani_other  2.6291424
# 82        f4b_under_nutr  2.6028822
# 83        f4a_hometrt_ab  2.5819958
# 84        f4a_house_cart  2.5550407
# 85      f4a_house_fridge  2.5523735
# 86        f4a_water_bore  2.3256401
# 87         f4a_wash_othr  2.3099111
# 88      f4a_hometrt_herb  2.2603849
# 89            f4a_ani_no  2.2180118
# 90         f4a_fuel_kero  2.0782553
# 91         f4a_drh_consc  1.9720369
# 92        f4a_water_yard  1.9539911
# 93   f4a_water_shallwell  1.9524613
# 94     f4a_water_pubwell  1.9340817
# 95          f4b_abn_hair  1.9265486
# 96         f4a_fuel_dung  1.8721279
# 97          f4a_seek_doc  1.8379048
# 98        f4a_fuel_grass  1.8127709
# 99        f4a_seek_remdy  1.8103900
# 100        f4a_fuel_crop  1.7885287
# 101        f4a_house_car  1.7244967
# 102       f4a_water_well  1.7122968
# 103     f4a_seek_privdoc  1.6982935
# 104    f4a_hometrt_othr2  1.6843226
# 105       f4a_seek_other  1.5970031
# 106    f4a_water_covwell  1.5678319
# 107      f4a_fuel_natgas  1.5672496
# 108      f4a_seek_healer  1.5495257
# 109       f4a_water_rain  1.5006816
# 110     f4a_relationship  1.4975735
# 111      f4a_water_house  1.4584930
# 112     f4a_hometrt_zinc  1.4010191
# 113        f4a_fuel_coal  1.3438215
# 114     f4a_water_bought  1.3161448
# 115     f4a_fuel_propane  1.2502073
# 116      f4b_chest_indrw  1.2400745
# 117      f4a_fuel_biogas  1.1835144
# 118      f4a_water_river  1.0921758
# 119         f4a_drh_conv  1.0871477
# 120       f4a_house_none  1.0671384
# 121   f4a_water_covpwell  0.9984805
# 122     f4a_drh_prolapse  0.9655468
# 123       f4b_skin_flaky  0.9213420
# 124  f4a_water_prospring  0.7864083
# 125  f4a_hometrt_othrliq  0.7600368
# 126       f4a_water_othr  0.6527807
# 127       f4a_water_pond  0.6177487
# 128   f4a_water_unspring  0.4954663
# 129       f4a_house_boat  0.4878841
# 130       f4a_fuel_other  0.4265832
# 131          f4b_bipedal  0.2907755
# 132      f4a_seek_friend  0.2558014
# 133     f4a_hometrt_milk  0.1824401
# 134           f4b_rectal  0.1582202
# 135        f4a_fuel_elec  0.1107041
# 136    f4b_observe_stool  0.0000000

# AUC          SE     lower     upper level Model nvar
# 1 0.7316085 0.001858144 0.7279666 0.7352504  0.95    LR    5
# 2 0.7306605 0.001866264 0.7270027 0.7343183  0.95    LR   10
# 3 0.7098813 0.001925313 0.7061078 0.7136549  0.95    RF    5
# 4 0.7236290 0.001878755 0.7199467 0.7273113  0.95    RF   10

# nvar    intc intc_LCI intc_UCI slope slope_LCI slope_UCI
# <dbl>   <dbl>    <dbl>    <dbl> <dbl>     <dbl>     <dbl>
# 1     5 0.00388   -0.172    0.175 0.983     0.772      1.21
# 2    10 0.00392   -0.172    0.175 0.967     0.757      1.19

################### main + shigella&crypto AF 0.3 growth falter ####
names<-append(x=names, values=c("shigella_afe_0.3","crypto_afe_0.3"))


main.shigcrypt0.3 <- CPR.funct(data=complete_gf_afe,outcome="haz_0.5",iter=100,nvars_opts=c(5,10))
main.shigcrypt0.3[["df_imps"]]
main.shigcrypt0.3[["AUC_df"]]
main.shigcrypt0.3[["calib"]]

# names    var_red
# 1               base_age 56.7903283
# 2                f4b_haz 40.1985120
# 3               f4b_resp 28.2163655
# 4               f4b_temp 26.7491671
# 5          f4a_ppl_house 20.2644248
# 6           f4a_drh_days 13.0103298
# 7          f4a_slp_rooms 12.9002979
# 8          f4a_breastfed 12.5553856
# 9          f4a_share_fac 12.4220905
# 10      f4a_yng_children 11.0731515
# 11         f4a_prim_schl 10.1676443
# 12         f4b_recommend  8.5022791
# 13        f4a_offr_drink  8.2670426
# 14        f4a_disp_feces  7.7764004
# 15         f4a_fac_waste  7.2955438
# 16                  site  7.0603770
# 17          f4a_dad_live  6.4379577
# 18        f4a_max_stools  6.3916951
# 19        crypto_afe_0.3  6.3456339
# 20     f4a_drh_bellypain  6.2516109
# 21          f4a_ms_water  6.2366035
# 22            f4b_mental  6.0036330
# 23        f4a_trt_method  5.6696501
# 24         f4a_drh_vomit  5.6243040
# 25       f4a_water_avail  5.5983110
# 26         f4a_wash_cook  4.8817149
# 27          f4a_wash_use  4.6203462
# 28             f3_gender  4.6183366
# 29  f4a_drh_lethrgy_miss  4.5766994
# 30      shigella_afe_0.3  4.5066387
# 31             f4b_mouth  4.4601913
# 32    f4a_drh_cough_miss  4.4164568
# 33          f4a_ani_goat  4.4148221
# 34        f4a_wash_child  4.4077744
# 35       f4a_cur_thirsty  4.3408723
# 36        f4a_wash_nurse  4.2616785
# 37             f3_drh_iv  4.2235292
# 38      f4a_hometrt_none  4.1987246
# 39        f4a_drh_thirst  4.1629543
# 40              f4b_skin  4.1404193
# 41        f4a_house_tele  4.0949133
# 42          f4a_wash_def  4.0826764
# 43      f4a_seek_outside  4.0661682
# 44       f4a_hometrt_ors  4.0577672
# 45        f4a_house_bike  4.0088260
# 46      f4a_cur_drymouth  4.0080986
# 47       f4a_house_phone  3.9674250
# 48          f4a_ani_fowl  3.8773531
# 49      f4a_cur_restless  3.8668119
# 50        f4a_house_elec  3.8303107
# 51           f3_drh_hosp  3.8237615
# 52          f4a_cur_skin  3.8159590
# 53           f4a_ani_dog  3.8157874
# 54      f4a_drh_restless  3.8150764
# 55           f4a_ani_cat  3.7741967
# 56      f4a_house_agland  3.7499244
# 57           f4a_ani_cow  3.7099915
# 58          f4a_wash_eat  3.7059477
# 59       f4a_ani_rodents  3.6770960
# 60             f4b_admit  3.6220062
# 61         f3_drh_turgor  3.6072666
# 62       f4a_store_water  3.5517710
# 63     f4a_drh_lessdrink  3.5489710
# 64    f4a_cur_fastbreath  3.4741029
# 65         f4a_trt_water  3.4607289
# 66        f4a_drh_strain  3.4500623
# 67       f4a_house_radio  3.4492092
# 68     f4a_hometrt_othr1  3.3443073
# 69             f4a_floor  3.3080161
# 70         f4a_fuel_wood  3.2141479
# 71         f4a_drh_blood  3.1957236
# 72      f4a_water_pubtap  3.1700627
# 73     f4a_hometrt_maize  3.0749145
# 74       f4a_house_scoot  3.0276027
# 75        f4a_seek_pharm  2.8522965
# 76         f4a_ani_sheep  2.8460542
# 77       f4a_wash_animal  2.7801640
# 78         f4a_ani_other  2.7767996
# 79    f4a_water_deepwell  2.6707150
# 80     f4a_fuel_charcoal  2.6361322
# 81      f4b_nature_stool  2.5929589
# 82        f4a_house_cart  2.5671630
# 83        f4a_hometrt_ab  2.5531511
# 84              f4b_eyes  2.5445944
# 85        f4b_under_nutr  2.4906591
# 86      f4a_house_fridge  2.4814082
# 87        f4a_water_bore  2.4129482
# 88      f4a_hometrt_herb  2.2286870
# 89         f4a_wash_othr  2.2235032
# 90            f4a_ani_no  2.1789819
# 91         f4a_fuel_kero  2.0425942
# 92        f4a_water_yard  2.0393758
# 93         f4a_fuel_dung  1.9664293
# 94         f4a_fuel_crop  1.9219403
# 95   f4a_water_shallwell  1.9160463
# 96          f4a_seek_doc  1.9097535
# 97         f4a_drh_consc  1.8913199
# 98     f4a_water_pubwell  1.8603750
# 99          f4b_abn_hair  1.8523490
# 100       f4a_seek_remdy  1.8010749
# 101       f4a_water_well  1.7635291
# 102        f4a_house_car  1.7513546
# 103       f4a_fuel_grass  1.7421959
# 104    f4a_hometrt_othr2  1.7041612
# 105      f4a_seek_healer  1.6542766
# 106     f4a_seek_privdoc  1.6541922
# 107    f4a_water_covwell  1.5252210
# 108       f4a_water_rain  1.5181486
# 109     f4a_relationship  1.4969445
# 110      f4a_fuel_natgas  1.4664979
# 111       f4a_seek_other  1.4511190
# 112     f4a_hometrt_zinc  1.4185049
# 113      f4a_water_house  1.3849618
# 114     f4a_water_bought  1.3180094
# 115        f4a_fuel_coal  1.2977190
# 116     f4a_fuel_propane  1.2715576
# 117      f4b_chest_indrw  1.1936223
# 118      f4a_water_river  1.1694946
# 119      f4a_fuel_biogas  1.1689762
# 120       f4a_house_none  1.1131763
# 121         f4a_drh_conv  1.0654055
# 122     f4a_drh_prolapse  0.9784472
# 123   f4a_water_covpwell  0.9274849
# 124       f4b_skin_flaky  0.8621697
# 125  f4a_water_prospring  0.8226092
# 126  f4a_hometrt_othrliq  0.7739523
# 127       f4a_water_othr  0.6553715
# 128       f4a_water_pond  0.5984658
# 129   f4a_water_unspring  0.5173349
# 130       f4a_house_boat  0.5047363
# 131       f4a_fuel_other  0.4473314
# 132          f4b_bipedal  0.3244064
# 133      f4a_seek_friend  0.2748751
# 134     f4a_hometrt_milk  0.2204356
# 135           f4b_rectal  0.1765487
# 136        f4a_fuel_elec  0.1125996
# 137    f4b_observe_stool  0.0000000

#    AUC          SE     lower     upper level Model nvar
# 1 0.7337340 0.001859961 0.7300885 0.7373795  0.95    LR    5
# 2 0.7331742 0.001868666 0.7295117 0.7368367  0.95    LR   10
# 3 0.7100253 0.001927629 0.7062472 0.7138034  0.95    RF    5
# 4 0.7246707 0.001873010 0.7209997 0.7283418  0.95    RF   10

# nvar    intc intc_LCI intc_UCI slope slope_LCI slope_UCI
# <dbl>   <dbl>    <dbl>    <dbl> <dbl>     <dbl>     <dbl>
# 1     5 -0.000653   -0.176    0.170 1.01      0.795      1.24
# 2    10 -0.000345   -0.176    0.171 0.994     0.781      1.22

################### main + viral(any) AF 0.3 growth falter ####
names<-append(x=names, values="viral_0.3")

main.viral0.3 <- CPR.funct(data=complete_gf_afe,outcome="haz_0.5",iter=100,nvars_opts=c(5,10))
main.viral0.3[["df_imps"]]
main.viral0.3[["AUC_df"]]
main.viral0.3[["calib"]]

# names    var_red
# 1               base_age 55.7815830
# 2                f4b_haz 40.3853987
# 3               f4b_resp 28.8516785
# 4               f4b_temp 27.7617004
# 5          f4a_ppl_house 20.4390287
# 6           f4a_drh_days 13.4946176
# 7          f4a_slp_rooms 12.8927251
# 8          f4a_breastfed 12.6914435
# 9          f4a_share_fac 12.2674467
# 10      f4a_yng_children 11.2288079
# 11         f4a_prim_schl 10.2925945
# 12        f4a_offr_drink  8.3736254
# 13         f4b_recommend  8.2591453
# 14        f4a_disp_feces  7.7394828
# 15         f4a_fac_waste  7.4472947
# 16                  site  7.1837835
# 17          f4a_dad_live  6.4240803
# 18        f4a_max_stools  6.3771136
# 19     f4a_drh_bellypain  6.3509736
# 20          f4a_ms_water  6.2757518
# 21            f4b_mental  5.9267839
# 22             viral_0.3  5.8533407
# 23         f4a_drh_vomit  5.6255110
# 24        f4a_trt_method  5.6171518
# 25       f4a_water_avail  5.5796483
# 26         f4a_wash_cook  4.9726425
# 27          f4a_wash_use  4.7410159
# 28             f3_gender  4.6080066
# 29    f4a_drh_cough_miss  4.5706365
# 30  f4a_drh_lethrgy_miss  4.5329524
# 31          f4a_ani_goat  4.5107839
# 32             f4b_mouth  4.4695731
# 33        f4a_wash_nurse  4.4410575
# 34        f4a_wash_child  4.3992828
# 35              f4b_skin  4.3574054
# 36       f4a_cur_thirsty  4.2597289
# 37      f4a_seek_outside  4.2582224
# 38          f4a_wash_def  4.2462627
# 39        f4a_drh_thirst  4.2320701
# 40             f3_drh_iv  4.1516539
# 41       f4a_hometrt_ors  4.1423184
# 42          f4a_cur_skin  4.1366647
# 43      f4a_cur_drymouth  4.0834477
# 44       f4a_house_phone  4.0486772
# 45        f4a_house_tele  4.0402753
# 46      f4a_hometrt_none  4.0362857
# 47          f4a_ani_fowl  3.9545035
# 48        f4a_house_elec  3.9492960
# 49        f4a_house_bike  3.9292238
# 50           f3_drh_hosp  3.9056229
# 51           f4a_ani_cat  3.8666339
# 52      f4a_cur_restless  3.8258672
# 53      f4a_drh_restless  3.8113060
# 54      f4a_house_agland  3.7777201
# 55           f4a_ani_cow  3.7655788
# 56           f4a_ani_dog  3.7348590
# 57       f4a_ani_rodents  3.7145181
# 58          f4a_wash_eat  3.6929163
# 59         f3_drh_turgor  3.6225424
# 60             f4b_admit  3.6018351
# 61    f4a_cur_fastbreath  3.5206199
# 62       f4a_store_water  3.5129026
# 63       f4a_house_radio  3.4939418
# 64     f4a_drh_lessdrink  3.4692079
# 65         f4a_trt_water  3.4631013
# 66             f4a_floor  3.4590252
# 67        f4a_drh_strain  3.3823408
# 68     f4a_hometrt_maize  3.2344087
# 69     f4a_hometrt_othr1  3.2207662
# 70         f4a_fuel_wood  3.2196564
# 71         f4a_drh_blood  3.1675736
# 72      f4a_water_pubtap  3.1225298
# 73        f4a_seek_pharm  2.9626912
# 74       f4a_house_scoot  2.9432316
# 75         f4a_ani_sheep  2.8592536
# 76     f4a_fuel_charcoal  2.7840671
# 77         f4a_ani_other  2.7362156
# 78      f4b_nature_stool  2.7251523
# 79    f4a_water_deepwell  2.7199248
# 80       f4a_wash_animal  2.6738631
# 81        f4a_hometrt_ab  2.6601045
# 82              f4b_eyes  2.6554365
# 83        f4b_under_nutr  2.6179740
# 84        f4a_house_cart  2.5356036
# 85      f4a_house_fridge  2.4657500
# 86      f4a_hometrt_herb  2.3584781
# 87        f4a_water_bore  2.3286311
# 88         f4a_wash_othr  2.2093970
# 89            f4a_ani_no  2.1854375
# 90        f4a_water_yard  2.1578407
# 91         f4a_drh_consc  2.0552981
# 92         f4a_fuel_dung  1.9794875
# 93         f4a_fuel_kero  1.9779014
# 94   f4a_water_shallwell  1.9715311
# 95          f4a_seek_doc  1.9041846
# 96          f4b_abn_hair  1.9008021
# 97        f4a_seek_remdy  1.8923959
# 98         f4a_fuel_crop  1.8852208
# 99     f4a_water_pubwell  1.8695926
# 100    f4a_hometrt_othr2  1.8331009
# 101        f4a_house_car  1.8315921
# 102       f4a_water_well  1.8164761
# 103       f4a_fuel_grass  1.8138698
# 104       f4a_seek_other  1.6070164
# 105      f4a_seek_healer  1.6063371
# 106     f4a_seek_privdoc  1.5548517
# 107    f4a_water_covwell  1.5435603
# 108     f4a_relationship  1.4842433
# 109      f4a_water_house  1.4444444
# 110       f4a_water_rain  1.4376204
# 111      f4a_fuel_natgas  1.4165138
# 112     f4a_water_bought  1.3770053
# 113     f4a_hometrt_zinc  1.3587904
# 114      f4b_chest_indrw  1.2824278
# 115        f4a_fuel_coal  1.2405094
# 116      f4a_fuel_biogas  1.1661958
# 117     f4a_fuel_propane  1.1285736
# 118       f4a_house_none  1.1039887
# 119   f4a_water_covpwell  1.0657970
# 120         f4a_drh_conv  1.0495730
# 121      f4a_water_river  1.0313674
# 122     f4a_drh_prolapse  0.9553604
# 123       f4b_skin_flaky  0.8326413
# 124  f4a_water_prospring  0.8211334
# 125  f4a_hometrt_othrliq  0.7298679
# 126       f4a_water_pond  0.6430135
# 127       f4a_water_othr  0.6190107
# 128       f4a_house_boat  0.5170591
# 129   f4a_water_unspring  0.4875389
# 130       f4a_fuel_other  0.4730941
# 131          f4b_bipedal  0.3766005
# 132      f4a_seek_friend  0.2552981
# 133     f4a_hometrt_milk  0.1921070
# 134           f4b_rectal  0.1533948
# 135        f4a_fuel_elec  0.1111540
# 136    f4b_observe_stool  0.0000000

# AUC          SE     lower     upper level Model nvar
# 1 0.7320414 0.001862033 0.7283918 0.7356909  0.95    LR    5
# 2 0.7312477 0.001870682 0.7275812 0.7349142  0.95    LR   10
# 3 0.7091488 0.001928008 0.7053700 0.7129276  0.95    RF    5
# 4 0.7235474 0.001875386 0.7198717 0.7272231  0.95    RF   10

# nvar   intc intc_LCI intc_UCI slope slope_LCI slope_UCI
# <dbl>  <dbl>    <dbl>    <dbl> <dbl>     <dbl>     <dbl>
# 1     5 0.00219   -0.173    0.173 0.994     0.780      1.22
# 2    10 0.00233   -0.173    0.174 0.978     0.766      1.20

################### main + shigella AF 0.7 growth falter ####
names<-append(x=names, values="shigella_afe_0.7")

main.shig0.7 <- CPR.funct(data=complete_gf_afe,outcome="haz_0.5",iter=100,nvars_opts=c(5,10))
main.shig0.7[["df_imps"]]
main.shig0.7[["AUC_df"]]
main.shig0.7[["calib"]]

# names    var_red
# 1               base_age 57.1994268
# 2                f4b_haz 40.7544319
# 3               f4b_resp 28.8499475
# 4               f4b_temp 27.4061342
# 5          f4a_ppl_house 20.0349515
# 6           f4a_drh_days 13.4124340
# 7          f4a_breastfed 13.0768160
# 8          f4a_slp_rooms 13.0539874
# 9          f4a_share_fac 12.3861415
# 10      f4a_yng_children 11.1462412
# 11         f4a_prim_schl 10.1144947
# 12        f4a_offr_drink  8.3998062
# 13         f4b_recommend  8.1350667
# 14        f4a_disp_feces  7.9260311
# 15         f4a_fac_waste  7.2832162
# 16                  site  7.1405847
# 17        f4a_max_stools  6.4438149
# 18          f4a_dad_live  6.3373458
# 19          f4a_ms_water  6.2994157
# 20     f4a_drh_bellypain  6.2061540
# 21            f4b_mental  5.8519073
# 22        f4a_trt_method  5.7486173
# 23         f4a_drh_vomit  5.6463093
# 24       f4a_water_avail  5.6093415
# 25         f4a_wash_cook  5.0219957
# 26          f4a_wash_use  4.6902912
# 27             f4b_mouth  4.6693555
# 28             f3_gender  4.6131588
# 29        f4a_wash_nurse  4.5516586
# 30    f4a_drh_cough_miss  4.4981028
# 31          f4a_ani_goat  4.4695430
# 32  f4a_drh_lethrgy_miss  4.4197005
# 33       f4a_cur_thirsty  4.3924905
# 34        f4a_drh_thirst  4.2828717
# 35        f4a_wash_child  4.2716267
# 36              f4b_skin  4.2314807
# 37      f4a_seek_outside  4.1989514
# 38        f4a_house_tele  4.1839367
# 39       f4a_hometrt_ors  4.1814853
# 40             f3_drh_iv  4.1669124
# 41          f4a_wash_def  4.1217731
# 42       f4a_house_phone  4.0650725
# 43          f4a_cur_skin  4.0584778
# 44      f4a_cur_drymouth  4.0192495
# 45      f4a_hometrt_none  4.0053941
# 46        f4a_house_bike  3.9853498
# 47           f3_drh_hosp  3.9361253
# 48      f4a_drh_restless  3.8966940
# 49      f4a_house_agland  3.8864220
# 50        f4a_house_elec  3.8745259
# 51       f4a_ani_rodents  3.8622593
# 52      f4a_cur_restless  3.8423742
# 53          f4a_ani_fowl  3.7692076
# 54           f4a_ani_dog  3.7648331
# 55           f4a_ani_cat  3.7027945
# 56             f4b_admit  3.6873787
# 57       f4a_store_water  3.6704225
# 58           f4a_ani_cow  3.6562989
# 59          f4a_wash_eat  3.6470300
# 60      shigella_afe_0.7  3.6353403
# 61       f4a_house_radio  3.6091798
# 62     f4a_drh_lessdrink  3.6051089
# 63    f4a_cur_fastbreath  3.5995804
# 64         f4a_trt_water  3.5842473
# 65             f4a_floor  3.4431171
# 66         f3_drh_turgor  3.4205916
# 67        f4a_drh_strain  3.3599086
# 68     f4a_hometrt_othr1  3.3289471
# 69         f4a_fuel_wood  3.2787216
# 70         f4a_drh_blood  3.2055685
# 71     f4a_hometrt_maize  3.1629920
# 72      f4a_water_pubtap  3.1472454
# 73       f4a_house_scoot  2.9718020
# 74        f4a_seek_pharm  2.8589581
# 75       f4a_wash_animal  2.8422067
# 76         f4a_ani_sheep  2.8214385
# 77         f4a_ani_other  2.7874380
# 78    f4a_water_deepwell  2.7206220
# 79              f4b_eyes  2.6523396
# 80     f4a_fuel_charcoal  2.6489482
# 81      f4b_nature_stool  2.6372320
# 82        f4a_hometrt_ab  2.6368894
# 83        f4b_under_nutr  2.5906240
# 84        f4a_house_cart  2.5079613
# 85      f4a_house_fridge  2.4424111
# 86        f4a_water_bore  2.4225019
# 87      f4a_hometrt_herb  2.3179308
# 88         f4a_wash_othr  2.2390184
# 89            f4a_ani_no  2.1895622
# 90        f4a_water_yard  2.0430474
# 91         f4a_fuel_crop  1.9369291
# 92          f4b_abn_hair  1.9219303
# 93         f4a_drh_consc  1.9171187
# 94         f4a_fuel_kero  1.9123776
# 95     f4a_water_pubwell  1.8837858
# 96        f4a_water_well  1.8631924
# 97          f4a_seek_doc  1.8580119
# 98   f4a_water_shallwell  1.8445752
# 99        f4a_seek_remdy  1.8380446
# 100        f4a_fuel_dung  1.7848328
# 101       f4a_fuel_grass  1.7684969
# 102    f4a_hometrt_othr2  1.7631162
# 103        f4a_house_car  1.7243081
# 104      f4a_seek_healer  1.6783132
# 105     f4a_seek_privdoc  1.6640740
# 106    f4a_water_covwell  1.5410571
# 107       f4a_seek_other  1.4818018
# 108      f4a_fuel_natgas  1.4654334
# 109     f4a_relationship  1.4433876
# 110     f4a_hometrt_zinc  1.3879803
# 111       f4a_water_rain  1.3710624
# 112      f4a_water_house  1.3593999
# 113     f4a_water_bought  1.3491273
# 114      f4b_chest_indrw  1.3222357
# 115     f4a_fuel_propane  1.2414662
# 116        f4a_fuel_coal  1.2247985
# 117      f4a_fuel_biogas  1.2045580
# 118       f4a_house_none  1.1070659
# 119         f4a_drh_conv  1.0597064
# 120      f4a_water_river  1.0513874
# 121   f4a_water_covpwell  0.9919485
# 122     f4a_drh_prolapse  0.9639526
# 123  f4a_water_prospring  0.9132450
# 124       f4b_skin_flaky  0.8934725
# 125  f4a_hometrt_othrliq  0.7876552
# 126       f4a_water_pond  0.6340438
# 127       f4a_water_othr  0.6323842
# 128   f4a_water_unspring  0.5388418
# 129       f4a_house_boat  0.5108982
# 130       f4a_fuel_other  0.4576449
# 131          f4b_bipedal  0.3756391
# 132      f4a_seek_friend  0.3033356
# 133     f4a_hometrt_milk  0.1820623
# 134           f4b_rectal  0.1587979
# 135        f4a_fuel_elec  0.1465467
# 136    f4b_observe_stool  0.0000000

# AUC          SE     lower     upper level Model nvar
# 1 0.7320156 0.001850944 0.7283879 0.7356434  0.95    LR    5
# 2 0.7310630 0.001859746 0.7274180 0.7347081  0.95    LR   10
# 3 0.7110336 0.001915332 0.7072796 0.7147876  0.95    RF    5
# 4 0.7233290 0.001870288 0.7196633 0.7269947  0.95    RF   10

# nvar    intc intc_LCI intc_UCI slope slope_LCI slope_UCI
# <dbl>   <dbl>    <dbl>    <dbl> <dbl>     <dbl>     <dbl>
# 1     5 0.00348   -0.172    0.175 0.989     0.776      1.22
# 2    10 0.00374   -0.172    0.175 0.973     0.762      1.20

################### main + crypto AF 0.7 growth falter ####
names<-append(x=names, values="crypto_afe_0.7")

main.crypto0.7 <- CPR.funct(data=complete_gf_afe,outcome="haz_0.5",iter=100,nvars_opts=c(5,10))
main.crypto0.7[["df_imps"]]
main.crypto0.7[["AUC_df"]]
main.crypto0.7[["calib"]]

# names    var_red
# 1               base_age 55.9127463
# 2                f4b_haz 40.5860600
# 3               f4b_resp 28.6460001
# 4               f4b_temp 27.8055603
# 5          f4a_ppl_house 20.1993346
# 6           f4a_drh_days 13.7105802
# 7          f4a_breastfed 13.4558189
# 8          f4a_slp_rooms 12.9871507
# 9          f4a_share_fac 12.2683495
# 10      f4a_yng_children 11.2334863
# 11         f4a_prim_schl 10.3756672
# 12        f4a_offr_drink  8.3673701
# 13         f4b_recommend  8.3150423
# 14        f4a_disp_feces  7.8890330
# 15         f4a_fac_waste  7.4448285
# 16                  site  6.9936782
# 17        f4a_max_stools  6.2864316
# 18          f4a_dad_live  6.2425777
# 19     f4a_drh_bellypain  6.1247396
# 20          f4a_ms_water  6.1134209
# 21            f4b_mental  6.0674444
# 22        f4a_trt_method  5.8179200
# 23       f4a_water_avail  5.6739093
# 24         f4a_drh_vomit  5.4620007
# 25         f4a_wash_cook  5.0310596
# 26          f4a_wash_use  4.8022847
# 27             f3_gender  4.6381343
# 28  f4a_drh_lethrgy_miss  4.5243028
# 29    f4a_drh_cough_miss  4.4666021
# 30        f4a_wash_nurse  4.4518818
# 31          f4a_ani_goat  4.4193139
# 32             f4b_mouth  4.3764897
# 33        f4a_wash_child  4.2910274
# 34              f4b_skin  4.2588232
# 35      f4a_seek_outside  4.2520969
# 36             f3_drh_iv  4.2111293
# 37       f4a_cur_thirsty  4.1961141
# 38          f4a_wash_def  4.1911693
# 39      f4a_hometrt_none  4.1636702
# 40       f4a_hometrt_ors  4.1471301
# 41        f4a_drh_thirst  4.1170091
# 42        f4a_house_tele  4.0923391
# 43        f4a_house_elec  4.0560577
# 44          f4a_ani_fowl  4.0070398
# 45       f4a_house_phone  3.9941341
# 46      f4a_cur_drymouth  3.9850426
# 47           f3_drh_hosp  3.9802785
# 48        f4a_house_bike  3.9711272
# 49      f4a_drh_restless  3.9659522
# 50           f4a_ani_cat  3.9406667
# 51      f4a_house_agland  3.9396007
# 52          f4a_cur_skin  3.8478241
# 53           f4a_ani_dog  3.8028999
# 54       f4a_ani_rodents  3.8022554
# 55           f4a_ani_cow  3.7846689
# 56        crypto_afe_0.7  3.7661574
# 57      f4a_cur_restless  3.7573788
# 58       f4a_house_radio  3.7121006
# 59          f4a_wash_eat  3.6686532
# 60     f4a_drh_lessdrink  3.6406666
# 61         f3_drh_turgor  3.6036217
# 62             f4b_admit  3.5842381
# 63        f4a_drh_strain  3.5143629
# 64    f4a_cur_fastbreath  3.4868815
# 65       f4a_store_water  3.4709427
# 66         f4a_trt_water  3.4078469
# 67     f4a_hometrt_othr1  3.3704882
# 68         f4a_drh_blood  3.3100924
# 69             f4a_floor  3.2647762
# 70         f4a_fuel_wood  3.2473697
# 71     f4a_hometrt_maize  3.2016041
# 72      f4a_water_pubtap  3.1160378
# 73       f4a_house_scoot  3.0087101
# 74        f4a_seek_pharm  2.7942339
# 75         f4a_ani_sheep  2.7850723
# 76         f4a_ani_other  2.7662611
# 77    f4a_water_deepwell  2.7427875
# 78        f4a_hometrt_ab  2.6942215
# 79       f4a_wash_animal  2.6909640
# 80      f4b_nature_stool  2.6868767
# 81        f4b_under_nutr  2.6865644
# 82     f4a_fuel_charcoal  2.6708627
# 83              f4b_eyes  2.5441235
# 84      f4a_house_fridge  2.5280390
# 85        f4a_house_cart  2.4404567
# 86        f4a_water_bore  2.3978330
# 87      f4a_hometrt_herb  2.3190622
# 88         f4a_wash_othr  2.2384632
# 89            f4a_ani_no  2.1340240
# 90        f4a_water_yard  2.1171944
# 91         f4a_fuel_kero  1.9876695
# 92   f4a_water_shallwell  1.9685044
# 93     f4a_water_pubwell  1.9348405
# 94         f4a_fuel_crop  1.9233353
# 95        f4a_fuel_grass  1.9177666
# 96          f4a_seek_doc  1.9036036
# 97         f4a_drh_consc  1.8996049
# 98         f4a_fuel_dung  1.8946193
# 99        f4a_seek_remdy  1.8710506
# 100         f4b_abn_hair  1.8467205
# 101    f4a_hometrt_othr2  1.7814761
# 102        f4a_house_car  1.7572484
# 103       f4a_water_well  1.7524546
# 104     f4a_seek_privdoc  1.7512584
# 105       f4a_seek_other  1.6171139
# 106      f4a_seek_healer  1.6067936
# 107      f4a_fuel_natgas  1.4734413
# 108     f4a_water_bought  1.4491026
# 109     f4a_relationship  1.4279247
# 110      f4a_water_house  1.4235083
# 111    f4a_water_covwell  1.4186067
# 112       f4a_water_rain  1.3896865
# 113     f4a_hometrt_zinc  1.3874056
# 114      f4b_chest_indrw  1.2915726
# 115     f4a_fuel_propane  1.2699545
# 116        f4a_fuel_coal  1.2442186
# 117      f4a_water_river  1.1633397
# 118      f4a_fuel_biogas  1.1464687
# 119         f4a_drh_conv  1.0383517
# 120   f4a_water_covpwell  1.0008625
# 121       f4a_house_none  0.9992459
# 122     f4a_drh_prolapse  0.9744555
# 123  f4a_water_prospring  0.9035827
# 124       f4b_skin_flaky  0.8841696
# 125  f4a_hometrt_othrliq  0.8439040
# 126       f4a_water_pond  0.6365319
# 127       f4a_water_othr  0.6251349
# 128   f4a_water_unspring  0.5078100
# 129       f4a_house_boat  0.4909659
# 130       f4a_fuel_other  0.4602851
# 131          f4b_bipedal  0.3535259
# 132      f4a_seek_friend  0.2985956
# 133     f4a_hometrt_milk  0.2073795
# 134           f4b_rectal  0.1581125
# 135        f4a_fuel_elec  0.1074578
# 136    f4b_observe_stool  0.0000000

# AUC          SE     lower     upper level Model nvar
# 1 0.7325175 0.001862084 0.7288679 0.7361671  0.95    LR    5
# 2 0.7318110 0.001867906 0.7281500 0.7354721  0.95    LR   10
# 3 0.7089903 0.001921819 0.7052236 0.7127570  0.95    RF    5
# 4 0.7215992 0.001875411 0.7179235 0.7252750  0.95    RF   10

# nvar     intc intc_LCI intc_UCI slope slope_LCI slope_UCI
# <dbl>    <dbl>    <dbl>    <dbl> <dbl>     <dbl>     <dbl>
# 1     5 0.0168   -0.158    0.188 0.998     0.784      1.23
# 2    10 0.0157   -0.159    0.187 0.982     0.770      1.21

################### main + shigella&crypto AF 0.7 growth falter ####
names<-append(x=names, values=c("shigella_afe_0.7","crypto_afe_0.7"))

main.shigcrypt0.7 <- CPR.funct(data=complete_gf_afe,outcome="haz_0.5",iter=100,nvars_opts=c(5,10))
main.shigcrypt0.7[["df_imps"]]
main.shigcrypt0.7[["AUC_df"]]
main.shigcrypt0.7[["calib"]]

# names    var_red
# 1               base_age 56.1375085
# 2                f4b_haz 41.0134682
# 3               f4b_resp 28.4029541
# 4               f4b_temp 27.2674236
# 5          f4a_ppl_house 20.1153236
# 6           f4a_drh_days 13.0947406
# 7          f4a_slp_rooms 13.0890378
# 8          f4a_breastfed 13.0224482
# 9          f4a_share_fac 12.1835488
# 10      f4a_yng_children 10.9949282
# 11         f4a_prim_schl 10.0834751
# 12         f4b_recommend  8.3253614
# 13        f4a_offr_drink  8.3158554
# 14        f4a_disp_feces  7.8147828
# 15                  site  7.2067105
# 16         f4a_fac_waste  7.1168075
# 17          f4a_dad_live  6.4824779
# 18     f4a_drh_bellypain  6.3658598
# 19        f4a_max_stools  6.3334383
# 20          f4a_ms_water  6.2419119
# 21            f4b_mental  5.9437998
# 22        f4a_trt_method  5.7728779
# 23       f4a_water_avail  5.6115149
# 24         f4a_drh_vomit  5.5809270
# 25         f4a_wash_cook  4.9773042
# 26             f3_gender  4.7212624
# 27          f4a_wash_use  4.6939862
# 28          f4a_ani_goat  4.6468845
# 29  f4a_drh_lethrgy_miss  4.5060470
# 30    f4a_drh_cough_miss  4.4601394
# 31        f4a_wash_nurse  4.4343008
# 32             f4b_mouth  4.3552446
# 33        f4a_wash_child  4.3105533
# 34       f4a_cur_thirsty  4.2465937
# 35        f4a_drh_thirst  4.2244553
# 36      f4a_seek_outside  4.1918562
# 37          f4a_wash_def  4.1857012
# 38              f4b_skin  4.1666354
# 39        f4a_house_tele  4.0705607
# 40        f4a_house_bike  4.0551920
# 41      f4a_cur_drymouth  4.0541416
# 42       f4a_house_phone  4.0478602
# 43             f3_drh_iv  4.0418168
# 44      f4a_hometrt_none  4.0416007
# 45       f4a_hometrt_ors  3.9893320
# 46          f4a_ani_fowl  3.9420542
# 47          f4a_cur_skin  3.9318544
# 48      f4a_house_agland  3.9141841
# 49           f3_drh_hosp  3.9046345
# 50           f4a_ani_cat  3.8857608
# 51        f4a_house_elec  3.8802359
# 52        crypto_afe_0.7  3.8508436
# 53       f4a_ani_rodents  3.7724241
# 54      f4a_drh_restless  3.7615244
# 55      f4a_cur_restless  3.7519420
# 56           f4a_ani_cow  3.7465593
# 57           f4a_ani_dog  3.7047911
# 58          f4a_wash_eat  3.6886417
# 59         f3_drh_turgor  3.5703574
# 60       f4a_house_radio  3.5650868
# 61     f4a_drh_lessdrink  3.5580477
# 62      shigella_afe_0.7  3.5440828
# 63             f4b_admit  3.5321532
# 64       f4a_store_water  3.5242563
# 65         f4a_trt_water  3.4559422
# 66    f4a_cur_fastbreath  3.3934831
# 67     f4a_hometrt_othr1  3.3721313
# 68        f4a_drh_strain  3.3708821
# 69             f4a_floor  3.3462064
# 70         f4a_fuel_wood  3.2387998
# 71      f4a_water_pubtap  3.1741773
# 72         f4a_drh_blood  3.1664304
# 73     f4a_hometrt_maize  3.1405215
# 74        f4a_seek_pharm  3.0377040
# 75       f4a_house_scoot  2.9483820
# 76         f4a_ani_sheep  2.8171650
# 77    f4a_water_deepwell  2.7870337
# 78       f4a_wash_animal  2.7646143
# 79         f4a_ani_other  2.7516597
# 80        f4b_under_nutr  2.6640791
# 81     f4a_fuel_charcoal  2.6246005
# 82              f4b_eyes  2.6106186
# 83        f4a_hometrt_ab  2.5956877
# 84        f4a_house_cart  2.5807810
# 85      f4b_nature_stool  2.5757811
# 86      f4a_house_fridge  2.4519464
# 87         f4a_wash_othr  2.3031322
# 88      f4a_hometrt_herb  2.2294097
# 89        f4a_water_bore  2.1995738
# 90            f4a_ani_no  2.1846631
# 91        f4a_water_yard  2.0128258
# 92          f4b_abn_hair  1.9613297
# 93         f4a_fuel_crop  1.9574548
# 94         f4a_fuel_kero  1.9562451
# 95         f4a_drh_consc  1.9443410
# 96         f4a_fuel_dung  1.9247794
# 97        f4a_seek_remdy  1.9197640
# 98   f4a_water_shallwell  1.8951014
# 99          f4a_seek_doc  1.8866980
# 100    f4a_water_pubwell  1.8723009
# 101       f4a_water_well  1.7724924
# 102        f4a_house_car  1.7508592
# 103       f4a_fuel_grass  1.7395945
# 104    f4a_hometrt_othr2  1.7118105
# 105      f4a_seek_healer  1.6721012
# 106     f4a_seek_privdoc  1.6298275
# 107     f4a_relationship  1.5721859
# 108      f4a_fuel_natgas  1.5284795
# 109       f4a_seek_other  1.5136377
# 110    f4a_water_covwell  1.4757261
# 111      f4a_water_house  1.4270206
# 112     f4a_hometrt_zinc  1.4090630
# 113       f4a_water_rain  1.3901070
# 114     f4a_water_bought  1.3802353
# 115     f4a_fuel_propane  1.2310658
# 116        f4a_fuel_coal  1.1937082
# 117      f4a_fuel_biogas  1.1511182
# 118      f4b_chest_indrw  1.1442844
# 119       f4a_house_none  1.1229690
# 120      f4a_water_river  1.0882774
# 121   f4a_water_covpwell  1.0213856
# 122         f4a_drh_conv  0.9718564
# 123     f4a_drh_prolapse  0.9599836
# 124       f4b_skin_flaky  0.9007555
# 125  f4a_water_prospring  0.8506542
# 126  f4a_hometrt_othrliq  0.7492611
# 127       f4a_water_pond  0.6284891
# 128       f4a_water_othr  0.5715524
# 129   f4a_water_unspring  0.4945455
# 130       f4a_fuel_other  0.4765209
# 131       f4a_house_boat  0.4481306
# 132          f4b_bipedal  0.3456714
# 133      f4a_seek_friend  0.2767445
# 134     f4a_hometrt_milk  0.2057230
# 135           f4b_rectal  0.1650917
# 136        f4a_fuel_elec  0.1392285
# 137    f4b_observe_stool  0.0000000

# AUC          SE     lower     upper level Model nvar
# 1 0.7317325 0.001860474 0.7280861 0.7353790  0.95    LR    5
# 2 0.7304120 0.001869022 0.7267488 0.7340752  0.95    LR   10
# 3 0.7110305 0.001921886 0.7072637 0.7147974  0.95    RF    5
# 4 0.7242626 0.001872927 0.7205917 0.7279335  0.95    RF   10

# nvar    intc intc_LCI intc_UCI slope slope_LCI slope_UCI
# <dbl>   <dbl>    <dbl>    <dbl> <dbl>     <dbl>     <dbl>
# 1     5 -0.00696   -0.183    0.165 0.989     0.776      1.22
# 2    10 -0.00799   -0.184    0.164 0.968     0.758      1.19

################### main + viral(any) AF 0.7 growth falter ####
names<-append(x=names, values="viral_0.7")

main.viral0.7 <- CPR.funct(data=complete_gf_afe,outcome="haz_0.5",iter=100,nvars_opts=c(5,10))
main.viral0.7[["df_imps"]]
main.viral0.7[["AUC_df"]]
main.viral0.7[["calib"]]

# names    var_red
# 1               base_age 55.4570149
# 2                f4b_haz 40.7154784
# 3               f4b_resp 28.6238430
# 4               f4b_temp 27.6551921
# 5          f4a_ppl_house 20.0236440
# 6          f4a_breastfed 13.2740217
# 7           f4a_drh_days 13.2572617
# 8          f4a_slp_rooms 13.1981451
# 9          f4a_share_fac 12.2659972
# 10      f4a_yng_children 10.8492038
# 11         f4a_prim_schl 10.4659879
# 12         f4b_recommend  8.4258205
# 13        f4a_offr_drink  8.2990496
# 14        f4a_disp_feces  7.7680935
# 15         f4a_fac_waste  7.2448064
# 16                  site  6.9986631
# 17          f4a_dad_live  6.4964184
# 18          f4a_ms_water  6.4372428
# 19        f4a_max_stools  6.3752542
# 20     f4a_drh_bellypain  6.0978742
# 21            f4b_mental  6.0063527
# 22        f4a_trt_method  5.8059519
# 23         f4a_drh_vomit  5.6804854
# 24       f4a_water_avail  5.5656696
# 25         f4a_wash_cook  5.1086580
# 26             viral_0.7  4.8421277
# 27             f3_gender  4.7672834
# 28          f4a_wash_use  4.7028406
# 29          f4a_ani_goat  4.5848212
# 30    f4a_drh_cough_miss  4.5768480
# 31  f4a_drh_lethrgy_miss  4.5313636
# 32             f4b_mouth  4.3736638
# 33        f4a_wash_nurse  4.3634274
# 34              f4b_skin  4.3627166
# 35       f4a_cur_thirsty  4.3545253
# 36      f4a_seek_outside  4.3393068
# 37        f4a_wash_child  4.3209085
# 38          f4a_wash_def  4.2470018
# 39        f4a_drh_thirst  4.1919172
# 40        f4a_house_tele  4.1052820
# 41      f4a_hometrt_none  4.0801060
# 42        f4a_house_elec  4.0714657
# 43             f3_drh_iv  4.0472223
# 44       f4a_hometrt_ors  4.0425148
# 45       f4a_house_phone  4.0253135
# 46      f4a_cur_drymouth  4.0235483
# 47          f4a_cur_skin  4.0106420
# 48          f4a_ani_fowl  3.9354581
# 49      f4a_house_agland  3.9336948
# 50        f4a_house_bike  3.9322063
# 51           f3_drh_hosp  3.8791196
# 52      f4a_drh_restless  3.8747777
# 53       f4a_ani_rodents  3.7854799
# 54      f4a_cur_restless  3.7780317
# 55          f4a_wash_eat  3.7564745
# 56           f4a_ani_dog  3.7392513
# 57           f4a_ani_cow  3.6991791
# 58           f4a_ani_cat  3.6903850
# 59         f3_drh_turgor  3.6161215
# 60         f4a_trt_water  3.5813972
# 61       f4a_house_radio  3.5294333
# 62     f4a_drh_lessdrink  3.5245666
# 63       f4a_store_water  3.5179386
# 64             f4b_admit  3.5043673
# 65             f4a_floor  3.5032872
# 66    f4a_cur_fastbreath  3.4700463
# 67        f4a_drh_strain  3.4322162
# 68     f4a_hometrt_othr1  3.3614491
# 69     f4a_hometrt_maize  3.2760878
# 70         f4a_drh_blood  3.2652640
# 71         f4a_fuel_wood  3.2641825
# 72      f4a_water_pubtap  3.1567507
# 73       f4a_house_scoot  2.9684015
# 74        f4a_seek_pharm  2.9012210
# 75         f4a_ani_sheep  2.8522181
# 76        f4b_under_nutr  2.7759164
# 77       f4a_wash_animal  2.7435488
# 78    f4a_water_deepwell  2.6973750
# 79         f4a_ani_other  2.6898486
# 80        f4a_hometrt_ab  2.6746642
# 81     f4a_fuel_charcoal  2.6484822
# 82              f4b_eyes  2.6441794
# 83      f4b_nature_stool  2.6234237
# 84        f4a_house_cart  2.5957385
# 85      f4a_house_fridge  2.4858636
# 86        f4a_water_bore  2.3707309
# 87         f4a_wash_othr  2.3096545
# 88      f4a_hometrt_herb  2.2521773
# 89            f4a_ani_no  2.1814367
# 90        f4a_water_yard  2.1403822
# 91         f4a_fuel_kero  1.9784798
# 92         f4a_fuel_crop  1.9645047
# 93   f4a_water_shallwell  1.9507923
# 94         f4a_fuel_dung  1.9479134
# 95         f4a_drh_consc  1.9226151
# 96        f4a_fuel_grass  1.9183675
# 97     f4a_water_pubwell  1.9139505
# 98        f4a_seek_remdy  1.8729039
# 99          f4b_abn_hair  1.8607868
# 100         f4a_seek_doc  1.8597152
# 101    f4a_hometrt_othr2  1.8205686
# 102        f4a_house_car  1.7906177
# 103       f4a_water_well  1.7232757
# 104      f4a_seek_healer  1.6845254
# 105     f4a_seek_privdoc  1.6306359
# 106     f4a_relationship  1.5543981
# 107       f4a_seek_other  1.5346350
# 108    f4a_water_covwell  1.4986173
# 109       f4a_water_rain  1.4521681
# 110     f4a_water_bought  1.4029403
# 111      f4a_fuel_natgas  1.3978128
# 112     f4a_hometrt_zinc  1.3415394
# 113      f4a_water_house  1.3262539
# 114        f4a_fuel_coal  1.3035318
# 115     f4a_fuel_propane  1.1987052
# 116      f4b_chest_indrw  1.1941099
# 117      f4a_fuel_biogas  1.1180818
# 118       f4a_house_none  1.1092934
# 119      f4a_water_river  1.0873648
# 120         f4a_drh_conv  1.0866262
# 121     f4a_drh_prolapse  0.9719061
# 122   f4a_water_covpwell  0.9504399
# 123       f4b_skin_flaky  0.9045620
# 124  f4a_water_prospring  0.8382654
# 125  f4a_hometrt_othrliq  0.7972431
# 126       f4a_water_othr  0.6172862
# 127       f4a_water_pond  0.6101072
# 128   f4a_water_unspring  0.5181236
# 129       f4a_house_boat  0.4454403
# 130       f4a_fuel_other  0.4225057
# 131          f4b_bipedal  0.3439838
# 132      f4a_seek_friend  0.2963571
# 133     f4a_hometrt_milk  0.1631328
# 134           f4b_rectal  0.1534622
# 135        f4a_fuel_elec  0.1394429
# 136    f4b_observe_stool  0.0000000

# AUC          SE     lower     upper level Model nvar
# 1 0.7323692 0.001861581 0.7287205 0.7360178  0.95    LR    5
# 2 0.7311925 0.001870441 0.7275265 0.7348585  0.95    LR   10
# 3 0.7118264 0.001921935 0.7080595 0.7155933  0.95    RF    5
# 4 0.7235604 0.001875734 0.7198840 0.7272367  0.95    RF   10

# nvar   intc intc_LCI intc_UCI slope slope_LCI slope_UCI
# <dbl>  <dbl>    <dbl>    <dbl> <dbl>     <dbl>     <dbl>
# 1     5 -0.00257   -0.178    0.169 0.989     0.776      1.22
# 2    10 -0.00132   -0.177    0.170 0.973     0.762      1.20

################### main + abx antibiotics growth falter ####
names<-append(x=names, values=c("abx_bf","abx_at","abx_home","abx_ever"))

main.abx <- CPR.funct(data=complete_gf,outcome="haz_0.5",iter=100,nvars_opts=c(5,10))
main.abx[["df_imps"]]
main.abx[["AUC_df"]]
main.abx[["calib"]]

# names    var_red
# 1               base_age 99.2880468
# 2                f4b_haz 82.1222778
# 3               f4b_resp 52.7492023
# 4               f4b_temp 49.9649155
# 5          f4a_ppl_house 37.5203175
# 6          f4a_slp_rooms 25.1011788
# 7           f4a_drh_days 24.7699007
# 8          f4a_share_fac 23.7644460
# 9          f4a_breastfed 22.5532511
# 10      f4a_yng_children 20.5511739
# 11         f4a_prim_schl 19.4313038
# 12        f4a_offr_drink 16.1555825
# 13         f4b_recommend 13.8944416
# 14        f4a_disp_feces 13.5506279
# 15         f4a_fac_waste 12.6418602
# 16                  site 12.3589702
# 17     f4a_drh_bellypain 11.8365931
# 18        f4a_max_stools 11.6803494
# 19          f4a_dad_live 11.4416742
# 20          f4a_ms_water 11.3835878
# 21            f4b_mental 10.3904057
# 22        f4a_trt_method 10.2749772
# 23       f4a_water_avail  9.9529916
# 24         f4a_drh_vomit  9.6260793
# 25          f4a_wash_use  9.0785582
# 26             f3_gender  8.9514314
# 27    f4a_drh_cough_miss  8.8737458
# 28             f4b_mouth  8.7130841
# 29         f4a_wash_cook  8.6115889
# 30        f4a_wash_nurse  8.3097434
# 31        f4a_wash_child  8.2777342
# 32  f4a_drh_lethrgy_miss  8.2101819
# 33       f4a_cur_thirsty  8.1870869
# 34        f4a_house_bike  7.8680348
# 35        f4a_drh_thirst  7.8523090
# 36          f4a_wash_def  7.7296332
# 37      f4a_hometrt_none  7.5293273
# 38      f4a_seek_outside  7.4336759
# 39      f4a_drh_restless  7.4298512
# 40      f4a_cur_restless  7.4195013
# 41        f4a_house_tele  7.3966633
# 42       f4a_hometrt_ors  7.3836369
# 43          f4a_ani_fowl  7.3649790
# 44      f4a_cur_drymouth  7.3355760
# 45        f4a_house_elec  7.2303209
# 46              f4b_skin  7.1648950
# 47             f3_drh_iv  7.0633074
# 48       f4a_house_radio  7.0612642
# 49       f4a_house_phone  7.0601849
# 50       f4a_ani_rodents  7.0131706
# 51       f4a_store_water  7.0084740
# 52          f4a_cur_skin  6.9789158
# 53      f4a_house_agland  6.9745307
# 54           f4a_ani_cat  6.9351341
# 55          f4a_ani_goat  6.7944068
# 56     f4a_drh_lessdrink  6.6584608
# 57           f4a_ani_dog  6.6197885
# 58         f4a_trt_water  6.4210794
# 59           f3_drh_hosp  6.3881911
# 60          f4a_wash_eat  6.3829770
# 61           f4a_ani_cow  6.3502560
# 62              abx_home  6.3014098
# 63             f4a_floor  6.2667281
# 64         f3_drh_turgor  6.1279302
# 65         f4a_fuel_wood  6.0020949
# 66        f4a_drh_strain  5.9555758
# 67      f4a_water_pubtap  5.8694128
# 68       f4a_house_scoot  5.7233949
# 69     f4a_hometrt_othr1  5.7147497
# 70         f4a_drh_blood  5.6830807
# 71                abx_at  5.6747668
# 72     f4a_hometrt_maize  5.6702937
# 73             f4b_admit  5.5689799
# 74    f4a_cur_fastbreath  5.4319932
# 75     f4a_fuel_charcoal  5.3752089
# 76        f4a_seek_pharm  5.3047096
# 77         f4a_ani_sheep  5.3044371
# 78              abx_ever  5.0852469
# 79      f4b_nature_stool  4.9425208
# 80       f4a_wash_animal  4.9362944
# 81        f4b_under_nutr  4.9014370
# 82        f4a_hometrt_ab  4.8120312
# 83      f4a_house_fridge  4.7985461
# 84                abx_bf  4.6118482
# 85         f4a_ani_other  4.6097537
# 86    f4a_water_deepwell  4.5560072
# 87      f4a_hometrt_herb  4.4948984
# 88              f4b_eyes  4.3527132
# 89         f4a_wash_othr  4.2269267
# 90        f4a_house_cart  4.1892587
# 91         f4a_fuel_kero  4.0451626
# 92            f4a_ani_no  3.9843374
# 93        f4a_water_bore  3.9429329
# 94        f4a_water_yard  3.9072108
# 95     f4a_water_pubwell  3.8186650
# 96     f4a_hometrt_othr2  3.3949195
# 97         f4a_house_car  3.3830941
# 98     f4a_water_covwell  3.2624303
# 99   f4a_water_shallwell  3.2506387
# 100       f4a_fuel_grass  3.1432763
# 101        f4a_fuel_crop  3.1227413
# 102       f4a_seek_remdy  3.0839111
# 103        f4a_fuel_dung  3.0724625
# 104        f4a_drh_consc  2.9334404
# 105     f4a_seek_privdoc  2.9243198
# 106      f4a_fuel_natgas  2.8548629
# 107         f4a_seek_doc  2.8541246
# 108     f4a_relationship  2.8209780
# 109      f4a_water_house  2.7110904
# 110      f4a_seek_healer  2.7036084
# 111     f4a_water_bought  2.6905407
# 112       f4a_water_well  2.6765647
# 113       f4a_water_rain  2.6540870
# 114     f4a_hometrt_zinc  2.6453642
# 115         f4b_abn_hair  2.5487006
# 116      f4b_chest_indrw  2.5113751
# 117        f4a_fuel_coal  2.4885857
# 118      f4a_fuel_biogas  2.4764687
# 119     f4a_fuel_propane  2.4459855
# 120      f4a_water_river  2.2059339
# 121       f4a_house_none  2.0326239
# 122       f4a_seek_other  2.0194470
# 123   f4a_water_covpwell  1.7685353
# 124         f4a_drh_conv  1.7051366
# 125  f4a_water_prospring  1.6791897
# 126       f4a_water_pond  1.4364524
# 127       f4a_water_othr  1.3097671
# 128  f4a_hometrt_othrliq  1.2119138
# 129       f4b_skin_flaky  1.2085998
# 130     f4a_drh_prolapse  1.2010707
# 131          f4b_bipedal  1.0847393
# 132       f4a_house_boat  1.0170188
# 133       f4a_fuel_other  0.9691191
# 134     f4a_hometrt_milk  0.7304751
# 135   f4a_water_unspring  0.6934942
# 136      f4a_seek_friend  0.4057007
# 137    f4b_observe_stool  0.2935868
# 138        f4a_fuel_elec  0.2744756
# 139           f4b_rectal  0.1750889

# AUC          SE     lower     upper level Model nvar
# 1 0.7209874 0.001417823 0.7182086 0.7237663  0.95    LR    5
# 2 0.7210458 0.001415725 0.7182710 0.7238206  0.95    LR   10
# 3 0.6976940 0.001456887 0.6948386 0.7005495  0.95    RF    5
# 4 0.7130055 0.001417895 0.7102265 0.7157845  0.95    RF   10

# nvar   intc intc_LCI intc_UCI slope slope_LCI slope_UCI
# <dbl>  <dbl>    <dbl>    <dbl> <dbl>     <dbl>     <dbl>
# 1     5 -0.00789   -0.136    0.118 1.01      0.837      1.18
# 2    10 -0.00760   -0.136    0.118 0.996     0.829      1.17

################### main -HAZ +MUAC ####
names<-append(x=names, values=c("f4b_muac"))
names <- names[!names %in% c("f4b_haz")]

main.muac.haz <- CPR.funct(data=complete_gf,outcome="haz_0.5",iter=100,nvars_opts=c(5,10))
main.muac.haz[["df_imps"]]
main.muac.haz[["AUC_df"]]
main.muac.haz[["calib"]]

# names     var_red
# 1               base_age 101.9804358
# 2               f4b_muac  60.0114993
# 3               f4b_resp  55.4001344
# 4               f4b_temp  52.9783919
# 5          f4a_ppl_house  38.9851690
# 6           f4a_drh_days  26.0289932
# 7          f4a_slp_rooms  25.9863975
# 8          f4a_share_fac  24.7963856
# 9          f4a_breastfed  23.9511658
# 10      f4a_yng_children  21.6658396
# 11         f4a_prim_schl  20.3488998
# 12        f4a_offr_drink  16.9732031
# 13         f4b_recommend  13.9905142
# 14        f4a_disp_feces  13.6765680
# 15         f4a_fac_waste  13.3847553
# 16                  site  12.3870267
# 17     f4a_drh_bellypain  12.2468710
# 18        f4a_max_stools  12.1622532
# 19          f4a_dad_live  11.8935722
# 20          f4a_ms_water  11.7319006
# 21        f4a_trt_method  10.6049373
# 22            f4b_mental  10.5954626
# 23         f4a_drh_vomit  10.4662190
# 24       f4a_water_avail  10.0300821
# 25          f4a_wash_use   9.6404628
# 26             f3_gender   9.2043577
# 27         f4a_wash_cook   9.0804463
# 28             f4b_mouth   9.0502238
# 29    f4a_drh_cough_miss   8.8566665
# 30        f4a_wash_nurse   8.8457408
# 31  f4a_drh_lethrgy_miss   8.6961640
# 32       f4a_cur_thirsty   8.4551113
# 33        f4a_wash_child   8.3620776
# 34        f4a_drh_thirst   8.3219051
# 35        f4a_house_bike   8.2231328
# 36      f4a_hometrt_none   7.9576131
# 37          f4a_wash_def   7.9019953
# 38      f4a_cur_drymouth   7.8094962
# 39        f4a_house_tele   7.7875892
# 40      f4a_cur_restless   7.7631536
# 41       f4a_hometrt_ors   7.7284918
# 42          f4a_ani_fowl   7.6912999
# 43      f4a_drh_restless   7.6242050
# 44        f4a_house_elec   7.4122747
# 45      f4a_seek_outside   7.3555539
# 46       f4a_house_phone   7.3283837
# 47       f4a_store_water   7.2891436
# 48       f4a_house_radio   7.2886138
# 49           f4a_ani_cat   7.2610144
# 50       f4a_ani_rodents   7.2139770
# 51              f4b_skin   7.1681166
# 52      f4a_house_agland   7.1654185
# 53             f3_drh_iv   7.0683770
# 54          f4a_cur_skin   7.0568536
# 55           f4a_ani_dog   7.0511736
# 56     f4a_drh_lessdrink   6.9682545
# 57          f4a_wash_eat   6.8103254
# 58          f4a_ani_goat   6.7744837
# 59             f4a_floor   6.6388476
# 60         f4a_trt_water   6.6181659
# 61           f4a_ani_cow   6.5926981
# 62             f4b_admit   6.5050035
# 63           f3_drh_hosp   6.4987655
# 64         f4a_fuel_wood   6.3651694
# 65      f4a_water_pubtap   6.2908911
# 66         f3_drh_turgor   6.1820764
# 67       f4a_house_scoot   6.1139049
# 68     f4a_hometrt_othr1   6.1088818
# 69        f4a_drh_strain   6.0910745
# 70         f4a_drh_blood   5.9645027
# 71     f4a_hometrt_maize   5.8877894
# 72     f4a_fuel_charcoal   5.7450560
# 73    f4a_cur_fastbreath   5.5229119
# 74        f4a_seek_pharm   5.5101185
# 75         f4a_ani_sheep   5.3061133
# 76        f4a_hometrt_ab   5.2884510
# 77      f4b_nature_stool   5.0917948
# 78      f4a_house_fridge   5.0186683
# 79        f4b_under_nutr   4.9551746
# 80       f4a_wash_animal   4.9073522
# 81    f4a_water_deepwell   4.8294993
# 82         f4a_ani_other   4.7682299
# 83              f4b_eyes   4.6522741
# 84      f4a_hometrt_herb   4.5951974
# 85         f4a_wash_othr   4.4258712
# 86        f4a_house_cart   4.3833803
# 87         f4a_fuel_kero   4.1773499
# 88            f4a_ani_no   3.9518324
# 89        f4a_water_bore   3.8533107
# 90        f4a_water_yard   3.8263390
# 91     f4a_water_pubwell   3.6933694
# 92         f4a_house_car   3.5592112
# 93     f4a_water_covwell   3.4326081
# 94     f4a_hometrt_othr2   3.3882439
# 95         f4a_fuel_dung   3.3476172
# 96   f4a_water_shallwell   3.3139947
# 97         f4a_fuel_crop   3.2278193
# 98        f4a_fuel_grass   3.2229598
# 99        f4a_seek_remdy   3.1444353
# 100     f4a_seek_privdoc   3.0428173
# 101         f4a_seek_doc   2.9881437
# 102     f4a_relationship   2.9569316
# 103      f4a_fuel_natgas   2.9369912
# 104        f4a_drh_consc   2.9270521
# 105      f4a_seek_healer   2.7782667
# 106      f4a_water_house   2.7570964
# 107       f4a_water_rain   2.7409042
# 108     f4a_hometrt_zinc   2.7179936
# 109     f4a_water_bought   2.6893535
# 110        f4a_fuel_coal   2.6790880
# 111       f4a_water_well   2.6650350
# 112     f4a_fuel_propane   2.6324393
# 113      f4b_chest_indrw   2.6240892
# 114         f4b_abn_hair   2.6145251
# 115      f4a_fuel_biogas   2.5619694
# 116      f4a_water_river   2.3936701
# 117       f4a_seek_other   2.0860020
# 118       f4a_house_none   2.0059254
# 119   f4a_water_covpwell   1.9931061
# 120  f4a_water_prospring   1.8500086
# 121         f4a_drh_conv   1.7621225
# 122  f4a_hometrt_othrliq   1.4460013
# 123       f4a_water_pond   1.4078428
# 124       f4a_water_othr   1.3458431
# 125     f4a_drh_prolapse   1.1118144
# 126       f4a_house_boat   1.0940653
# 127       f4b_skin_flaky   1.0129295
# 128       f4a_fuel_other   0.9753667
# 129          f4b_bipedal   0.9165199
# 130   f4a_water_unspring   0.7393118
# 131     f4a_hometrt_milk   0.7051053
# 132      f4a_seek_friend   0.4362063
# 133        f4a_fuel_elec   0.3727115
# 134    f4b_observe_stool   0.3136430
# 135           f4b_rectal   0.1946715

# AUC          SE     lower     upper level Model nvar
# 1 0.7002198 0.001427121 0.6974227 0.7030169  0.95    LR    5
# 2 0.7005924 0.001425810 0.6977979 0.7033869  0.95    LR   10
# 3 0.6775393 0.001473840 0.6746506 0.6804279  0.95    RF    5
# 4 0.6912425 0.001438285 0.6884236 0.6940615  0.95    RF   10

# nvar    intc intc_LCI intc_UCI slope slope_LCI slope_UCI
# <dbl>   <dbl>    <dbl>    <dbl> <dbl>     <dbl>     <dbl>
# 1     5 -0.000212   -0.127    0.124 0.992     0.812      1.18
# 2    10 -0.000308   -0.127    0.124 0.981     0.803      1.17

################### main + MUAC ####
names<-append(x=names, values=c("f4b_muac"))

main.muac <- CPR.funct(data=complete_gf,outcome="haz_0.5",iter=100,nvars_opts=c(5,10))
main.muac[["df_imps"]]
main.muac[["AUC_df"]]
main.muac[["calib"]]

# names    var_red
# 1               base_age 96.7549792
# 2                f4b_haz 81.1592046
# 3               f4b_muac 56.6636514
# 4               f4b_resp 52.2258793
# 5               f4b_temp 49.4398587
# 6          f4a_ppl_house 36.2979457
# 7          f4a_slp_rooms 24.4534159
# 8           f4a_drh_days 24.0971507
# 9          f4a_share_fac 22.8893920
# 10         f4a_breastfed 21.5918996
# 11      f4a_yng_children 20.0688344
# 12         f4a_prim_schl 18.7291527
# 13        f4a_offr_drink 15.6517260
# 14         f4b_recommend 14.1787694
# 15        f4a_disp_feces 12.6768317
# 16         f4a_fac_waste 12.5860292
# 17                  site 12.1781837
# 18     f4a_drh_bellypain 11.5834521
# 19          f4a_ms_water 11.3682746
# 20        f4a_max_stools 11.2855128
# 21          f4a_dad_live 11.1091018
# 22            f4b_mental  9.8275042
# 23        f4a_trt_method  9.7068713
# 24         f4a_drh_vomit  9.6913389
# 25       f4a_water_avail  9.4887253
# 26          f4a_wash_use  8.9231055
# 27             f3_gender  8.5760964
# 28             f4b_mouth  8.4055278
# 29    f4a_drh_cough_miss  8.2545511
# 30        f4a_wash_nurse  8.1752383
# 31  f4a_drh_lethrgy_miss  8.0957828
# 32         f4a_wash_cook  8.0688418
# 33       f4a_cur_thirsty  7.9886765
# 34        f4a_wash_child  7.9420263
# 35        f4a_house_bike  7.6573328
# 36        f4a_drh_thirst  7.6113174
# 37          f4a_wash_def  7.3169065
# 38      f4a_hometrt_none  7.3071184
# 39        f4a_house_tele  7.2325565
# 40      f4a_cur_restless  7.1579758
# 41        f4a_house_elec  7.1344655
# 42      f4a_cur_drymouth  7.1185594
# 43      f4a_house_agland  7.0905368
# 44              f4b_skin  7.0421011
# 45      f4a_seek_outside  7.0359416
# 46       f4a_hometrt_ors  7.0270631
# 47          f4a_ani_fowl  6.9881695
# 48      f4a_drh_restless  6.9068075
# 49             f3_drh_iv  6.8135772
# 50           f4a_ani_cat  6.7440100
# 51       f4a_house_radio  6.7412415
# 52       f4a_house_phone  6.7245315
# 53       f4a_store_water  6.6471164
# 54       f4a_ani_rodents  6.6438180
# 55          f4a_cur_skin  6.5939140
# 56           f3_drh_hosp  6.4624889
# 57           f4a_ani_dog  6.4257988
# 58          f4a_ani_goat  6.4198056
# 59     f4a_drh_lessdrink  6.3639763
# 60             f4a_floor  6.2783979
# 61         f4a_trt_water  6.2540201
# 62          f4a_wash_eat  6.1204961
# 63           f4a_ani_cow  6.0951495
# 64             f4b_admit  6.0266164
# 65         f4a_fuel_wood  5.9760309
# 66         f3_drh_turgor  5.9302270
# 67      f4a_water_pubtap  5.8434429
# 68     f4a_hometrt_othr1  5.8351121
# 69        f4a_drh_strain  5.8180427
# 70     f4a_hometrt_maize  5.5413274
# 71         f4a_drh_blood  5.4306063
# 72       f4a_house_scoot  5.4251210
# 73        f4a_hometrt_ab  5.3164606
# 74         f4a_ani_sheep  5.2379758
# 75    f4a_cur_fastbreath  5.1566086
# 76        f4a_seek_pharm  5.1036542
# 77     f4a_fuel_charcoal  5.0832658
# 78      f4b_nature_stool  4.9253997
# 79       f4a_wash_animal  4.7798395
# 80        f4b_under_nutr  4.5708091
# 81      f4a_house_fridge  4.5465622
# 82    f4a_water_deepwell  4.5327729
# 83         f4a_ani_other  4.4350687
# 84      f4a_hometrt_herb  4.3972199
# 85              f4b_eyes  4.3875490
# 86         f4a_wash_othr  4.2221972
# 87        f4a_house_cart  4.0601517
# 88            f4a_ani_no  3.8516729
# 89         f4a_fuel_kero  3.8342464
# 90        f4a_water_yard  3.6476406
# 91        f4a_water_bore  3.6014270
# 92     f4a_water_pubwell  3.5564675
# 93   f4a_water_shallwell  3.3468261
# 94        f4a_fuel_grass  3.2713515
# 95         f4a_house_car  3.2435888
# 96     f4a_hometrt_othr2  3.2431378
# 97     f4a_water_covwell  3.1619642
# 98         f4a_fuel_crop  3.1543471
# 99        f4a_seek_remdy  3.0372814
# 100        f4a_fuel_dung  3.0219441
# 101     f4a_seek_privdoc  2.9102591
# 102      f4a_fuel_natgas  2.7798020
# 103        f4a_drh_consc  2.7529458
# 104     f4a_water_bought  2.7486287
# 105         f4a_seek_doc  2.7435642
# 106      f4a_seek_healer  2.7041297
# 107     f4a_relationship  2.6737737
# 108      f4a_water_house  2.5565968
# 109       f4a_water_rain  2.5545580
# 110     f4a_hometrt_zinc  2.5503385
# 111       f4a_water_well  2.5427631
# 112        f4a_fuel_coal  2.4496622
# 113      f4b_chest_indrw  2.4087055
# 114         f4b_abn_hair  2.3954417
# 115      f4a_fuel_biogas  2.3935528
# 116     f4a_fuel_propane  2.3266941
# 117      f4a_water_river  2.1734746
# 118       f4a_seek_other  1.9682293
# 119       f4a_house_none  1.8383297
# 120   f4a_water_covpwell  1.7419762
# 121  f4a_water_prospring  1.6985068
# 122         f4a_drh_conv  1.5917544
# 123       f4a_water_pond  1.4577744
# 124  f4a_hometrt_othrliq  1.2459389
# 125       f4a_water_othr  1.2149877
# 126     f4a_drh_prolapse  1.0931420
# 127          f4b_bipedal  1.0219336
# 128       f4b_skin_flaky  1.0113648
# 129       f4a_fuel_other  0.9503805
# 130       f4a_house_boat  0.9386592
# 131     f4a_hometrt_milk  0.7164359
# 132   f4a_water_unspring  0.7126824
# 133      f4a_seek_friend  0.4239132
# 134        f4a_fuel_elec  0.3090716
# 135    f4b_observe_stool  0.2562927
# 136           f4b_rectal  0.1690088

# AUC          SE     lower     upper level Model nvar
# 1 0.7233618 0.001410432 0.7205974 0.7261262  0.95    LR    5
# 2 0.7230136 0.001410163 0.7202497 0.7257775  0.95    LR   10
# 3 0.7064496 0.001444295 0.7036189 0.7092804  0.95    RF    5
# 4 0.7176008 0.001411100 0.7148351 0.7203665  0.95    RF   10

# nvar    intc intc_LCI intc_UCI slope slope_LCI slope_UCI
# <dbl>   <dbl>    <dbl>    <dbl> <dbl>     <dbl>     <dbl>
# 1     5 -0.00392   -0.132    0.122 0.993     0.828      1.17
# 2    10 -0.00382   -0.132    0.122 0.983     0.819      1.15

################### HAZ1.0 growth falter ####
main.1 <- CPR.funct(data=complete_gf,outcome="haz_1.0",iter=100,nvars_opts=c(5,10))
main.1[["df_imps"]]
main.1[["AUC_df"]]
main.1[["calib"]]

# names      var_red
# 1               base_age 19.525533869
# 2                f4b_haz 18.403621830
# 3               f4b_resp 12.686697280
# 4               f4b_temp 11.768960108
# 5          f4a_ppl_house  9.159507912
# 6          f4a_share_fac  6.079668944
# 7          f4a_slp_rooms  5.982348392
# 8           f4a_drh_days  5.490394237
# 9       f4a_yng_children  5.244364002
# 10         f4a_breastfed  4.628995823
# 11         f4a_prim_schl  4.389903370
# 12        f4a_disp_feces  3.749287398
# 13        f4a_offr_drink  3.745711939
# 14          f4a_ms_water  3.392397798
# 15     f4a_drh_bellypain  2.916612245
# 16                  site  2.892573075
# 17         f4a_fac_waste  2.735545734
# 18          f4a_dad_live  2.723131979
# 19        f4a_max_stools  2.603034676
# 20            f4b_mental  2.485294481
# 21             f4b_mouth  2.472599147
# 22        f4a_trt_method  2.444691908
# 23         f4b_recommend  2.377497537
# 24       f4a_water_avail  2.318393660
# 25        f4a_wash_nurse  2.267917636
# 26             f3_gender  2.261491533
# 27       f4a_cur_thirsty  2.184188280
# 28        f4a_drh_thirst  2.129740171
# 29         f4a_wash_cook  2.093852776
# 30    f4a_drh_cough_miss  2.085225383
# 31         f4a_drh_vomit  2.062633867
# 32      f4a_cur_drymouth  2.013152238
# 33        f4a_house_tele  1.889121480
# 34          f4a_wash_def  1.869627783
# 35      f4a_cur_restless  1.865541597
# 36      f4a_relationship  1.856794668
# 37  f4a_drh_lethrgy_miss  1.844256047
# 38              f4b_skin  1.817715765
# 39     f4a_drh_lessdrink  1.815947827
# 40      f4a_seek_outside  1.795654043
# 41        f4a_water_bore  1.757441522
# 42          f4a_ani_fowl  1.751766947
# 43        f4a_wash_child  1.751089485
# 44          f4a_cur_skin  1.749701488
# 45          f4a_wash_eat  1.743806633
# 46      f4a_hometrt_none  1.741616908
# 47        f4a_house_bike  1.716477490
# 48       f4a_hometrt_ors  1.706666875
# 49          f4a_ani_goat  1.702133029
# 50           f4a_ani_dog  1.696294758
# 51          f4a_wash_use  1.691419651
# 52    f4a_cur_fastbreath  1.689826026
# 53      f4a_drh_restless  1.662454196
# 54           f4a_ani_cat  1.658935428
# 55       f4a_house_phone  1.603582084
# 56         f3_drh_turgor  1.596253032
# 57       f4a_house_radio  1.594473134
# 58             f4a_floor  1.584484046
# 59        f4a_drh_strain  1.555117543
# 60       f4a_ani_rodents  1.535929411
# 61        f4a_seek_pharm  1.518270603
# 62           f4a_ani_cow  1.511170441
# 63       f4a_store_water  1.496445877
# 64      f4b_nature_stool  1.474757423
# 65       f4a_house_scoot  1.458528519
# 66        f4a_house_elec  1.454090647
# 67         f4a_fuel_wood  1.429219130
# 68         f4a_trt_water  1.425446756
# 69      f4a_hometrt_milk  1.399187920
# 70      f4a_house_agland  1.385157974
# 71      f4a_water_pubtap  1.371197568
# 72     f4a_water_covwell  1.339811962
# 73         f4a_ani_sheep  1.337312832
# 74     f4a_hometrt_maize  1.321698461
# 75         f4a_drh_blood  1.317385747
# 76             f3_drh_iv  1.315021007
# 77       f4a_wash_animal  1.305628872
# 78         f4a_wash_othr  1.284448333
# 79      f4a_house_fridge  1.258050130
# 80             f4b_admit  1.239470741
# 81     f4a_fuel_charcoal  1.217328696
# 82           f3_drh_hosp  1.212760240
# 83        f4b_under_nutr  1.180177964
# 84       f4b_chest_indrw  1.173482019
# 85     f4a_hometrt_othr1  1.167990042
# 86              f4b_eyes  1.167034474
# 87        f4a_hometrt_ab  1.156611539
# 88         f4a_ani_other  1.150465912
# 89         f4a_fuel_kero  1.129868696
# 90            f4a_ani_no  1.074800924
# 91        f4a_house_cart  1.032879785
# 92       f4a_seek_healer  0.987471160
# 93      f4a_hometrt_herb  0.975569172
# 94        f4a_water_rain  0.928637532
# 95        f4a_house_none  0.910987502
# 96         f4a_fuel_coal  0.891079930
# 97      f4a_seek_privdoc  0.881105339
# 98        f4a_seek_other  0.873615477
# 99        f4a_water_yard  0.836864150
# 100        f4a_house_car  0.835572291
# 101   f4a_water_deepwell  0.798976517
# 102      f4a_water_river  0.786795805
# 103      f4a_fuel_natgas  0.775505656
# 104        f4a_drh_consc  0.770891755
# 105  f4a_hometrt_othrliq  0.768930206
# 106   f4a_water_covpwell  0.728298796
# 107        f4a_fuel_crop  0.717645697
# 108         f4a_seek_doc  0.702466969
# 109    f4a_water_pubwell  0.698390875
# 110       f4a_water_othr  0.697591496
# 111         f4b_abn_hair  0.687475193
# 112     f4a_hometrt_zinc  0.680751540
# 113    f4a_hometrt_othr2  0.641878077
# 114         f4a_drh_conv  0.624275906
# 115       f4a_seek_remdy  0.618516278
# 116      f4a_fuel_biogas  0.612667237
# 117       f4a_water_pond  0.601882592
# 118  f4a_water_shallwell  0.601858747
# 119     f4a_water_bought  0.591159985
# 120       f4a_water_well  0.579027119
# 121       f4a_fuel_grass  0.568256550
# 122  f4a_water_prospring  0.562596491
# 123      f4a_water_house  0.554549518
# 124       f4a_house_boat  0.536395850
# 125        f4a_fuel_dung  0.523927079
# 126     f4a_fuel_propane  0.488550146
# 127     f4a_drh_prolapse  0.437161125
# 128          f4b_bipedal  0.402926805
# 129       f4b_skin_flaky  0.362241266
# 130   f4a_water_unspring  0.260775557
# 131       f4a_fuel_other  0.229174999
# 132      f4a_seek_friend  0.156752796
# 133        f4a_fuel_elec  0.007687242
# 134           f4b_rectal  0.002309290
# 135    f4b_observe_stool  0.000000000

# AUC          SE     lower     upper level Model nvar
# 1 0.7983748 0.002061733 0.7943339 0.8024157  0.95    LR    5
# 2 0.7966949 0.002072926 0.7926321 0.8007578  0.95    LR   10
# 3 0.7500509 0.002435032 0.7452783 0.7548235  0.95    RF    5
# 4 0.7599233 0.002382439 0.7552538 0.7645928  0.95    RF   10

# nvar   intc intc_LCI intc_UCI slope slope_LCI slope_UCI
# <dbl>  <dbl>    <dbl>    <dbl> <dbl>     <dbl>     <dbl>
# 1     5 -0.00626   -0.261    0.232 1.00      0.738      1.30
# 2    10 -0.00652   -0.261    0.232 0.973     0.714      1.26

################### by country growth falter - Gambia ####
summary(cases_gf_Gambia$f4a_prim_schl)
cases_gf_Gambia_complete <- cases_gf_Gambia %>% filter(f4a_prim_schl!=7) 
cases_gf_Gambia_complete$f4a_prim_schl <- as.factor(ifelse(cases_gf_Gambia_complete$f4a_prim_schl==5,4,cases_gf_Gambia_complete$f4a_prim_schl))
summary(cases_gf_Gambia_complete$f4a_prim_schl)

GF.Gambia <- CPR.funct(data=cases_gf_Gambia_complete,outcome="haz_0.5",iter=100,nvars_opts=c(5,10))
GF.Gambia[["df_imps"]]
GF.Gambia[["AUC_df"]]
GF.Gambia[["calib"]]

# names      var_red
# 1                f4b_haz 12.081798419
# 2               base_age 10.363533497
# 3               f4b_resp  6.991856813
# 4          f4a_ppl_house  6.947900977
# 5               f4b_temp  6.532431291
# 6          f4a_slp_rooms  5.612991954
# 7       f4a_yng_children  5.245031992
# 8           f4a_drh_days  3.573676866
# 9         f4a_offr_drink  2.282577523
# 10       f4a_water_avail  2.204348401
# 11         f4a_breastfed  2.117688293
# 12         f4a_prim_schl  1.916993410
# 13     f4a_drh_bellypain  1.856825325
# 14          f4a_dad_live  1.724369164
# 15          f4a_ms_water  1.601629969
# 16         f4a_drh_vomit  1.595501416
# 17      f4a_hometrt_none  1.576487169
# 18        f4a_trt_method  1.569619967
# 19         f4a_trt_water  1.502344796
# 20      f4a_seek_outside  1.448387655
# 21             f4b_mouth  1.324214523
# 22       f4a_store_water  1.280962160
# 23        f4a_seek_pharm  1.264150250
# 24           f4a_ani_cow  1.236238240
# 25         f4b_recommend  1.227286987
# 26        f4a_drh_thirst  1.220630785
# 27           f4a_ani_dog  1.201760664
# 28        f4a_wash_nurse  1.186243698
# 29       f4a_cur_thirsty  1.180782335
# 30             f3_gender  1.173796344
# 31    f4a_drh_cough_miss  1.171809190
# 32       f4a_house_scoot  1.171169114
# 33        f4a_house_elec  1.169054617
# 34         f4a_wash_cook  1.168564671
# 35         f4a_ani_other  1.159007361
# 36         f4a_ani_sheep  1.127144874
# 37           f4a_ani_cat  1.125463686
# 38  f4a_drh_lethrgy_miss  1.107145445
# 39        f4a_house_tele  1.104827941
# 40          f4a_wash_use  1.080806943
# 41              f4b_skin  1.068019303
# 42          f4a_wash_def  1.045371731
# 43        f4a_max_stools  1.036147861
# 44       f4a_ani_rodents  1.033934845
# 45         f4a_house_car  1.033871250
# 46     f4a_hometrt_maize  1.032922583
# 47      f4a_cur_drymouth  1.002075734
# 48     f4a_water_pubwell  1.000189598
# 49      f4a_water_pubtap  0.999856410
# 50         f4a_drh_blood  0.985299405
# 51          f4a_ani_goat  0.972714061
# 52      f4a_drh_restless  0.964219346
# 53        f4a_wash_child  0.959226089
# 54        f4b_under_nutr  0.955495146
# 55             f3_drh_iv  0.942002912
# 56        f4a_house_cart  0.933262999
# 57    f4a_water_deepwell  0.926180106
# 58         f3_drh_turgor  0.923460704
# 59          f4a_cur_skin  0.866925133
# 60        f4a_water_well  0.851069991
# 61      f4a_house_fridge  0.842305903
# 62             f4a_floor  0.797110729
# 63           f3_drh_hosp  0.780560181
# 64     f4a_hometrt_othr1  0.777065418
# 65     f4a_drh_lessdrink  0.768551840
# 66       f4a_wash_animal  0.763417432
# 67      f4a_cur_restless  0.756106458
# 68             f4b_admit  0.755191514
# 69       f4a_hometrt_ors  0.750609150
# 70        f4a_house_bike  0.746063126
# 71      f4b_nature_stool  0.732508004
# 72       f4a_house_phone  0.718851080
# 73    f4a_cur_fastbreath  0.697953319
# 74         f4a_share_fac  0.679283798
# 75      f4a_house_agland  0.664772338
# 76      f4a_hometrt_herb  0.650032089
# 77            f4b_mental  0.635317137
# 78          f4b_abn_hair  0.618697034
# 79          f4a_ani_fowl  0.617535347
# 80        f4a_disp_feces  0.583960620
# 81              f4b_eyes  0.572106285
# 82        f4a_water_yard  0.570137875
# 83       f4b_chest_indrw  0.536275815
# 84       f4a_house_radio  0.493069658
# 85        f4a_drh_strain  0.489715504
# 86         f4a_fac_waste  0.473353546
# 87          f4a_wash_eat  0.444587028
# 88        f4a_seek_other  0.407519178
# 89        f4a_hometrt_ab  0.366109032
# 90         f4a_wash_othr  0.305757249
# 91   f4a_hometrt_othrliq  0.275067709
# 92      f4a_relationship  0.234260246
# 93          f4a_seek_doc  0.205065786
# 94     f4a_water_covwell  0.200641211
# 95       f4a_seek_healer  0.196467386
# 96     f4a_hometrt_othr2  0.194909387
# 97    f4a_water_covpwell  0.194743019
# 98        f4a_seek_remdy  0.177957858
# 99      f4a_seek_privdoc  0.166536455
# 100       f4b_skin_flaky  0.160681461
# 101        f4a_drh_consc  0.145354185
# 102    f4a_fuel_charcoal  0.130420215
# 103           f4a_ani_no  0.088537234
# 104        f4a_fuel_wood  0.085442160
# 105      f4a_seek_friend  0.071820772
# 106       f4a_water_bore  0.058252995
# 107     f4a_drh_prolapse  0.050895979
# 108     f4a_hometrt_milk  0.044651932
# 109          f4b_bipedal  0.041454381
# 110       f4a_water_othr  0.040484175
# 111      f4a_water_house  0.034829298
# 112         f4a_drh_conv  0.030242492
# 113           f4b_rectal  0.016947289
# 114       f4a_water_rain  0.012509042
# 115  f4a_water_shallwell  0.011434888
# 116      f4a_fuel_natgas  0.006712751
# 117        f4a_fuel_coal  0.003877631
# 118     f4a_hometrt_zinc  0.003478963
# 119                 site  0.000000000
# 120       f4a_house_boat  0.000000000
# 121       f4a_house_none  0.000000000
# 122        f4a_fuel_elec  0.000000000
# 123      f4a_fuel_biogas  0.000000000
# 124       f4a_fuel_grass  0.000000000
# 125     f4a_fuel_propane  0.000000000
# 126        f4a_fuel_dung  0.000000000
# 127        f4a_fuel_crop  0.000000000
# 128        f4a_fuel_kero  0.000000000
# 129       f4a_fuel_other  0.000000000
# 130  f4a_water_prospring  0.000000000
# 131   f4a_water_unspring  0.000000000
# 132      f4a_water_river  0.000000000
# 133       f4a_water_pond  0.000000000
# 134     f4a_water_bought  0.000000000
# 135    f4b_observe_stool  0.000000000

# AUC          SE     lower     upper level Model nvar
# 1 0.6656235 0.004433991 0.6569331 0.6743140  0.95    LR    5
# 2 0.6871235 0.004423330 0.6784540 0.6957931  0.95    LR   10
# 3 0.6548910 0.004517594 0.6460367 0.6637453  0.95    RF    5
# 4 0.6681199 0.004516757 0.6592672 0.6769726  0.95    RF   10

# nvar   intc intc_LCI intc_UCI slope slope_LCI slope_UCI
# <dbl>  <dbl>    <dbl>    <dbl> <dbl>     <dbl>     <dbl>
#   1     5 0.0128   -0.349    0.362 0.984     0.447      1.58
# 2    10 0.0109   -0.357    0.366 0.771     0.349      1.26

################### by country growth falter - Mali ####
summary(cases_gf_Mali$f4a_prim_schl)
cases_gf_Mali_complete <- cases_gf_Mali %>% filter(f4a_prim_schl!=7) 
summary(cases_gf_Mali_complete$f4a_prim_schl)

GF.Mali <- CPR.funct(data=cases_gf_Mali_complete,outcome="haz_0.5",iter=100,nvars_opts=c(5,10))
GF.Mali[["df_imps"]]
GF.Mali[["AUC_df"]]
GF.Mali[["calib"]]

# names      var_red
# 1               base_age 21.955331133
# 2                f4b_haz 13.408203498
# 3               f4b_resp  9.575536458
# 4               f4b_temp  9.162323342
# 5          f4a_breastfed  8.564195567
# 6          f4a_ppl_house  7.212269971
# 7          f4a_slp_rooms  5.333295977
# 8       f4a_yng_children  4.538280482
# 9          f4a_share_fac  4.115750672
# 10          f4a_drh_days  4.045664111
# 11         f4a_prim_schl  3.379810689
# 12         f4a_drh_vomit  2.402840529
# 13       f4a_store_water  2.359871288
# 14     f4a_drh_bellypain  2.113636963
# 15          f4a_dad_live  1.949838396
# 16     f4a_water_covwell  1.921455626
# 17              f4b_skin  1.818330723
# 18        f4a_max_stools  1.806932755
# 19        f4a_offr_drink  1.806244995
# 20    f4a_drh_cough_miss  1.715072857
# 21            f4b_mental  1.637536075
# 22      f4a_cur_restless  1.589634388
# 23       f4a_water_avail  1.575407016
# 24          f4a_wash_def  1.525300417
# 25             f3_gender  1.471320469
# 26         f4a_wash_cook  1.458691783
# 27        f4a_house_bike  1.449628848
# 28             f4b_mouth  1.442838122
# 29          f4a_ani_fowl  1.411344748
# 30      f4a_cur_drymouth  1.372378503
# 31      f4a_drh_restless  1.367588658
# 32       f4a_hometrt_ors  1.366786985
# 33          f4a_ms_water  1.345129593
# 34  f4a_drh_lethrgy_miss  1.329644163
# 35       f4a_house_scoot  1.324061177
# 36         f4a_fuel_wood  1.320419350
# 37          f4a_cur_skin  1.289710277
# 38        f4a_wash_nurse  1.285020765
# 39      f4a_hometrt_none  1.283303131
# 40       f4a_ani_rodents  1.270773473
# 41         f3_drh_turgor  1.266210812
# 42     f4a_fuel_charcoal  1.256434273
# 43      f4a_seek_outside  1.252898514
# 44        f4a_wash_child  1.247528745
# 45     f4a_hometrt_maize  1.229203470
# 46          f4a_wash_use  1.223922016
# 47      f4a_hometrt_herb  1.202388288
# 48       f4a_seek_healer  1.191989662
# 49      f4a_house_fridge  1.151755474
# 50        f4a_house_tele  1.135781743
# 51        f4a_house_elec  1.127480798
# 52    f4a_water_covpwell  1.110235275
# 53        f4b_under_nutr  1.077216066
# 54         f4a_house_car  1.062083824
# 55       f4a_cur_thirsty  1.039569817
# 56        f4a_drh_strain  1.034485986
# 57         f4a_ani_sheep  1.007190017
# 58      f4a_water_pubtap  0.983046076
# 59            f4a_ani_no  0.977103542
# 60       f4a_house_radio  0.976888350
# 61      f4a_water_bought  0.955363045
# 62        f4a_drh_thirst  0.930816703
# 63     f4a_hometrt_othr1  0.926638718
# 64          f4a_wash_eat  0.905489184
# 65        f4a_hometrt_ab  0.902915874
# 66        f4a_seek_pharm  0.887605173
# 67        f4a_disp_feces  0.869835297
# 68         f4b_recommend  0.851295834
# 69         f4a_drh_blood  0.819787144
# 70        f4a_house_cart  0.806123177
# 71             f3_drh_iv  0.795327216
# 72   f4a_water_prospring  0.753821849
# 73           f4a_ani_dog  0.738827851
# 74     f4a_drh_lessdrink  0.684786924
# 75           f4a_ani_cat  0.656693471
# 76    f4a_cur_fastbreath  0.654166809
# 77        f4a_seek_remdy  0.614176351
# 78        f4a_fuel_other  0.602931990
# 79        f4a_water_yard  0.598783769
# 80      f4a_relationship  0.579227169
# 81     f4a_hometrt_othr2  0.551398491
# 82     f4a_water_pubwell  0.541319372
# 83       f4a_house_phone  0.434983163
# 84      f4b_nature_stool  0.426834581
# 85         f4a_fac_waste  0.383646013
# 86        f4a_water_well  0.378051001
# 87      f4a_seek_privdoc  0.376323459
# 88          f4b_abn_hair  0.366225883
# 89       f4a_water_house  0.330370202
# 90      f4a_fuel_propane  0.326236121
# 91           f4b_bipedal  0.311773834
# 92          f4a_ani_goat  0.310980077
# 93       f4b_chest_indrw  0.296441191
# 94           f3_drh_hosp  0.292313156
# 95       f4a_wash_animal  0.275392897
# 96             f4a_floor  0.273340735
# 97             f4b_admit  0.262957683
# 98      f4a_house_agland  0.252814173
# 99        f4a_seek_other  0.252062862
# 100          f4a_ani_cow  0.250923644
# 101        f4a_ani_other  0.250036622
# 102  f4a_hometrt_othrliq  0.171210452
# 103             f4b_eyes  0.145756423
# 104       f4a_water_bore  0.135656176
# 105        f4a_fuel_elec  0.122150222
# 106        f4a_trt_water  0.122016285
# 107       f4a_trt_method  0.121421449
# 108         f4a_drh_conv  0.121219705
# 109      f4a_fuel_biogas  0.117624353
# 110     f4a_hometrt_milk  0.107915820
# 111       f4a_house_boat  0.103552835
# 112   f4a_water_deepwell  0.103363919
# 113      f4a_fuel_natgas  0.102139808
# 114        f4a_wash_othr  0.101447499
# 115       f4b_skin_flaky  0.097600244
# 116      f4a_seek_friend  0.069374791
# 117         f4a_seek_doc  0.056292530
# 118        f4a_drh_consc  0.014158349
# 119     f4a_drh_prolapse  0.011210369
# 120     f4a_hometrt_zinc  0.009055965
# 121        f4a_fuel_coal  0.007985773
# 122        f4a_fuel_crop  0.006677094
# 123           f4b_rectal  0.003828110
# 124       f4a_house_none  0.002009524
# 125                 site  0.000000000
# 126       f4a_fuel_grass  0.000000000
# 127        f4a_fuel_dung  0.000000000
# 128        f4a_fuel_kero  0.000000000
# 129   f4a_water_unspring  0.000000000
# 130      f4a_water_river  0.000000000
# 131       f4a_water_pond  0.000000000
# 132       f4a_water_rain  0.000000000
# 133  f4a_water_shallwell  0.000000000
# 134       f4a_water_othr  0.000000000
# 135    f4b_observe_stool  0.000000000

# AUC          SE     lower     upper level Model nvar
# 1 0.8237806 0.002596168 0.8186922 0.8288690  0.95    LR    5
# 2 0.8190074 0.002622275 0.8138679 0.8241470  0.95    LR   10
# 3 0.7877022 0.002897924 0.7820224 0.7933821  0.95    RF    5
# 4 0.8019593 0.002812039 0.7964478 0.8074708  0.95    RF   10

# nvar    intc intc_LCI intc_UCI slope slope_LCI slope_UCI
# <dbl>   <dbl>    <dbl>    <dbl> <dbl>     <dbl>     <dbl>
#   1     5 -0.0360   -0.379    0.289 1.00      0.689      1.37
# 2    10 -0.0381   -0.381    0.288 0.958     0.657      1.31

################### by country growth falter - Mozam - abandoned, too small sample size####


################### by country growth falter - Kenya ####
summary(cases_gf_Kenya$f4a_prim_schl)
cases_gf_Kenya_complete <- cases_gf_Kenya %>% filter(f4a_prim_schl!=7) 
cases_gf_Kenya_complete$f4a_prim_schl <- as.factor(ifelse(cases_gf_Kenya_complete$f4a_prim_schl==5,4,cases_gf_Kenya_complete$f4a_prim_schl))
summary(cases_gf_Kenya_complete$f4a_prim_schl)

GF.Kenya <- CPR.funct(data=cases_gf_Kenya_complete,outcome="haz_0.5",iter=100,nvars_opts=c(5,10))
GF.Kenya[["df_imps"]]
GF.Kenya[["AUC_df"]]
GF.Kenya[["calib"]]

# names      var_red
# 1               base_age 1.507445e+01
# 2                f4b_haz 1.245626e+01
# 3               f4b_resp 9.105176e+00
# 4               f4b_temp 8.116625e+00
# 5          f4a_breastfed 5.295267e+00
# 6           f4a_drh_days 4.962560e+00
# 7          f4a_ppl_house 4.749089e+00
# 8          f4a_share_fac 4.415257e+00
# 9         f4a_offr_drink 4.059327e+00
# 10      f4a_yng_children 2.517742e+00
# 11            f4b_mental 2.352309e+00
# 12          f4a_cur_skin 2.327227e+00
# 13          f4a_dad_live 2.172088e+00
# 14        f4a_disp_feces 2.146271e+00
# 15        f4a_trt_method 2.133811e+00
# 16          f4a_ms_water 1.992381e+00
# 17         f4a_prim_schl 1.953535e+00
# 18         f4a_slp_rooms 1.842010e+00
# 19       f4a_cur_thirsty 1.803163e+00
# 20        f4a_max_stools 1.710469e+00
# 21     f4a_drh_lessdrink 1.691265e+00
# 22     f4a_drh_bellypain 1.674480e+00
# 23     f4a_hometrt_othr1 1.633697e+00
# 24           f4a_ani_cat 1.619497e+00
# 25             f3_gender 1.565019e+00
# 26             f4b_mouth 1.558896e+00
# 27       f4a_ani_rodents 1.547404e+00
# 28              f4b_skin 1.531958e+00
# 29        f4a_house_bike 1.517214e+00
# 30  f4a_drh_lethrgy_miss 1.516807e+00
# 31        f4a_wash_nurse 1.516747e+00
# 32         f4a_wash_othr 1.494647e+00
# 33       f4a_house_phone 1.472162e+00
# 34         f4a_drh_vomit 1.461701e+00
# 35      f4a_cur_restless 1.439931e+00
# 36       f4a_hometrt_ors 1.431505e+00
# 37    f4a_drh_cough_miss 1.429850e+00
# 38         f4a_wash_cook 1.422958e+00
# 39           f4a_ani_dog 1.422917e+00
# 40          f4a_ani_goat 1.421825e+00
# 41          f4a_wash_def 1.415765e+00
# 42         f4a_ani_sheep 1.411597e+00
# 43     f4a_hometrt_maize 1.408561e+00
# 44     f4a_fuel_charcoal 1.397749e+00
# 45        f4a_drh_thirst 1.387786e+00
# 46        f4a_water_rain 1.371729e+00
# 47           f4a_ani_cow 1.361894e+00
# 48        f4a_wash_child 1.341028e+00
# 49       f4a_water_river 1.319418e+00
# 50       f4a_house_radio 1.303273e+00
# 51      f4a_cur_drymouth 1.301293e+00
# 52         f3_drh_turgor 1.295659e+00
# 53      f4a_drh_restless 1.269770e+00
# 54         f4a_drh_consc 1.267550e+00
# 55    f4a_cur_fastbreath 1.260611e+00
# 56       f4a_fuel_biogas 1.240118e+00
# 57      f4a_seek_outside 1.203260e+00
# 58          f4a_wash_eat 1.189176e+00
# 59      f4a_hometrt_none 1.186014e+00
# 60         f4a_trt_water 1.172283e+00
# 61        f4a_water_bore 1.144911e+00
# 62        f4a_drh_strain 1.128020e+00
# 63         f4a_drh_blood 1.100098e+00
# 64           f3_drh_hosp 1.084261e+00
# 65        f4b_under_nutr 1.060566e+00
# 66             f4a_floor 1.025278e+00
# 67      f4a_hometrt_herb 9.995912e-01
# 68             f3_drh_iv 9.952964e-01
# 69        f4a_water_pond 9.641911e-01
# 70       f4a_water_avail 9.492973e-01
# 71        f4a_seek_pharm 9.226264e-01
# 72       f4a_store_water 9.092826e-01
# 73          f4a_ani_fowl 8.936630e-01
# 74        f4a_house_tele 8.895841e-01
# 75   f4a_water_prospring 8.866799e-01
# 76         f4a_fuel_kero 8.427637e-01
# 77         f4a_fuel_wood 8.353187e-01
# 78         f4a_ani_other 8.326739e-01
# 79         f4a_fac_waste 7.775633e-01
# 80      f4a_water_pubtap 7.770680e-01
# 81             f4b_admit 7.060110e-01
# 82      f4a_house_agland 7.044261e-01
# 83       f4a_wash_animal 6.972339e-01
# 84     f4a_water_pubwell 6.833829e-01
# 85          f4a_wash_use 6.735692e-01
# 86         f4a_fuel_crop 6.425324e-01
# 87        f4a_hometrt_ab 6.355166e-01
# 88          f4b_abn_hair 5.867222e-01
# 89         f4b_recommend 5.863623e-01
# 90   f4a_water_shallwell 5.667533e-01
# 91    f4a_water_unspring 5.398711e-01
# 92        f4a_house_cart 5.374587e-01
# 93     f4a_hometrt_othr2 4.580684e-01
# 94       f4a_seek_healer 4.573787e-01
# 95          f4a_drh_conv 4.483166e-01
# 96        f4a_seek_remdy 4.254185e-01
# 97           f4b_bipedal 4.089412e-01
# 98      f4b_nature_stool 4.025859e-01
# 99    f4a_water_deepwell 3.867288e-01
# 100     f4a_relationship 3.800404e-01
# 101       f4a_fuel_grass 3.526139e-01
# 102      f4b_chest_indrw 3.270070e-01
# 103       f4b_skin_flaky 3.135690e-01
# 104     f4a_hometrt_milk 3.093536e-01
# 105       f4a_house_elec 2.865720e-01
# 106      f4a_house_scoot 2.576278e-01
# 107     f4a_hometrt_zinc 2.415556e-01
# 108       f4a_seek_other 2.337110e-01
# 109       f4a_water_well 1.995315e-01
# 110         f4a_seek_doc 1.940173e-01
# 111     f4a_drh_prolapse 1.859601e-01
# 112             f4b_eyes 1.822140e-01
# 113       f4a_house_none 1.614337e-01
# 114  f4a_hometrt_othrliq 1.582354e-01
# 115        f4a_house_car 1.474972e-01
# 116      f4a_seek_friend 1.438885e-01
# 117           f4a_ani_no 1.085679e-01
# 118        f4a_fuel_coal 1.056125e-01
# 119      f4a_fuel_natgas 1.030635e-01
# 120           f4b_rectal 1.023864e-01
# 121   f4a_water_covpwell 9.412337e-02
# 122    f4a_water_covwell 8.521324e-02
# 123     f4a_fuel_propane 5.928542e-02
# 124     f4a_house_fridge 5.430299e-02
# 125     f4a_seek_privdoc 3.746243e-02
# 126        f4a_fuel_dung 2.328004e-02
# 127       f4a_water_yard 1.124528e-02
# 128     f4a_water_bought 1.093947e-02
# 129       f4a_fuel_other 7.671561e-03
# 130        f4a_fuel_elec 7.915950e-04
# 131      f4a_water_house 5.142857e-04
# 132                 site 0.000000e+00
# 133       f4a_house_boat 0.000000e+00
# 134       f4a_water_othr 0.000000e+00
# 135    f4b_observe_stool 0.000000e+00

# AUC          SE     lower     upper level Model nvar
# 1 0.7080055 0.003658155 0.7008356 0.7151753  0.95    LR    5
# 2 0.7168951 0.003641203 0.7097585 0.7240317  0.95    LR   10
# 3 0.6906276 0.003779725 0.6832195 0.6980357  0.95    RF    5
# 4 0.7062554 0.003668601 0.6990650 0.7134457  0.95    RF   10

# nvar     intc intc_LCI intc_UCI slope slope_LCI slope_UCI
# <dbl>    <dbl>    <dbl>    <dbl> <dbl>     <dbl>     <dbl>
#   1     5 0.00196    -0.326    0.319 0.994     0.579      1.47
# 2    10 0.000204   -0.332    0.322 0.894     0.529      1.31


################### by country growth falter - India ####
GF.India <- CPR.funct(data=cases_gf_India,outcome="haz_0.5",iter=100,nvars_opts=c(5,10))
GF.India[["df_imps"]]
GF.India[["AUC_df"]]
GF.India[["calib"]]

# names      var_red
# 1                f4b_haz 1.521073e+01
# 2               base_age 1.504872e+01
# 3               f4b_temp 9.329257e+00
# 4               f4b_resp 8.620198e+00
# 5          f4a_share_fac 7.067005e+00
# 6          f4a_ppl_house 5.873352e+00
# 7           f4a_drh_days 4.201690e+00
# 8          f4a_prim_schl 4.112901e+00
# 9          f4a_slp_rooms 3.420494e+00
# 10        f4a_offr_drink 3.156171e+00
# 11        f4a_disp_feces 2.851584e+00
# 12     f4a_drh_bellypain 2.597336e+00
# 13      f4a_yng_children 2.477022e+00
# 14       f4a_store_water 2.403966e+00
# 15            f4b_mental 2.365120e+00
# 16         f4a_breastfed 2.355949e+00
# 17        f4a_max_stools 2.300619e+00
# 18         f4b_recommend 2.129528e+00
# 19         f4a_fac_waste 1.997613e+00
# 20        f4a_wash_nurse 1.873292e+00
# 21             f4b_admit 1.863156e+00
# 22           f3_drh_hosp 1.784268e+00
# 23        f4a_drh_thirst 1.761857e+00
# 24        f4a_trt_method 1.757213e+00
# 25     f4a_drh_lessdrink 1.754423e+00
# 26          f4a_wash_use 1.740666e+00
# 27          f4a_ms_water 1.734895e+00
# 28             f3_gender 1.720478e+00
# 29       f4a_cur_thirsty 1.713658e+00
# 30         f4a_fuel_wood 1.676659e+00
# 31         f4a_trt_water 1.624051e+00
# 32          f4a_wash_eat 1.598030e+00
# 33         f4a_fuel_coal 1.591337e+00
# 34        f4a_wash_child 1.587046e+00
# 35          f4a_ani_fowl 1.569543e+00
# 36         f4a_drh_vomit 1.548184e+00
# 37         f4a_fuel_kero 1.531109e+00
# 38       f4a_house_phone 1.494213e+00
# 39        f4a_house_bike 1.493888e+00
# 40      f4a_seek_outside 1.477646e+00
# 41    f4a_drh_cough_miss 1.469349e+00
# 42       f4a_hometrt_ors 1.457092e+00
# 43           f4a_ani_dog 1.437626e+00
# 44  f4a_drh_lethrgy_miss 1.417704e+00
# 45         f4a_wash_cook 1.413458e+00
# 46          f4a_wash_def 1.405248e+00
# 47      f4a_fuel_propane 1.329878e+00
# 48           f4a_ani_cat 1.325169e+00
# 49       f4a_house_radio 1.323808e+00
# 50        f4a_house_tele 1.321220e+00
# 51      f4a_drh_restless 1.288186e+00
# 52        f4a_water_yard 1.278417e+00
# 53             f4b_mouth 1.271464e+00
# 54        f4a_house_elec 1.267081e+00
# 55      f4a_water_pubtap 1.264919e+00
# 56     f4a_hometrt_maize 1.247431e+00
# 57      f4a_hometrt_none 1.218870e+00
# 58      f4a_cur_restless 1.187947e+00
# 59          f4a_cur_skin 1.181253e+00
# 60             f4a_floor 1.144962e+00
# 61      f4a_seek_privdoc 1.107743e+00
# 62              f4b_skin 1.013737e+00
# 63         f3_drh_turgor 9.803479e-01
# 64         f4a_ani_other 9.502382e-01
# 65          f4a_ani_goat 9.410543e-01
# 66        f4a_hometrt_ab 9.241972e-01
# 67     f4a_hometrt_othr1 9.216239e-01
# 68              f4b_eyes 8.444873e-01
# 69       f4a_house_scoot 8.229946e-01
# 70      f4a_house_fridge 8.183037e-01
# 71      f4a_cur_drymouth 8.082448e-01
# 72             f3_drh_iv 8.060254e-01
# 73         f4a_drh_blood 8.024697e-01
# 74    f4a_cur_fastbreath 7.885411e-01
# 75        f4a_seek_other 7.431479e-01
# 76          f4a_seek_doc 6.727085e-01
# 77     f4a_hometrt_othr2 6.493747e-01
# 78      f4b_nature_stool 6.480466e-01
# 79        f4b_under_nutr 6.378190e-01
# 80        f4a_seek_pharm 6.276165e-01
# 81        f4a_drh_strain 6.149805e-01
# 82          f4a_dad_live 6.139282e-01
# 83        f4a_house_none 5.691069e-01
# 84       f4a_ani_rodents 5.045493e-01
# 85    f4a_water_deepwell 4.987316e-01
# 86       f4a_fuel_natgas 4.382491e-01
# 87         f4a_house_car 4.234732e-01
# 88        f4a_water_othr 4.150893e-01
# 89     f4a_fuel_charcoal 3.936707e-01
# 90       f4a_water_avail 3.909248e-01
# 91       f4a_water_house 3.687936e-01
# 92       f4a_wash_animal 3.571452e-01
# 93           f4a_ani_cow 3.294792e-01
# 94         f4a_fuel_dung 2.790447e-01
# 95   f4a_hometrt_othrliq 2.788285e-01
# 96     f4b_observe_stool 2.469088e-01
# 97      f4a_relationship 2.459320e-01
# 98   f4a_water_shallwell 2.392190e-01
# 99         f4a_wash_othr 2.052254e-01
# 100        f4a_fuel_elec 1.480428e-01
# 101         f4b_abn_hair 1.446651e-01
# 102        f4a_ani_sheep 1.360242e-01
# 103       f4a_seek_remdy 1.343827e-01
# 104     f4a_hometrt_zinc 1.343514e-01
# 105     f4a_hometrt_herb 8.224079e-02
# 106      f4a_seek_friend 7.467944e-02
# 107           f4a_ani_no 7.444196e-02
# 108     f4a_hometrt_milk 4.857174e-02
# 109     f4a_house_agland 4.543482e-02
# 110      f4b_chest_indrw 2.287050e-02
# 111        f4a_drh_consc 1.469187e-02
# 112         f4a_drh_conv 1.005509e-02
# 113           f4b_rectal 8.358881e-03
# 114       f4b_skin_flaky 6.476286e-03
# 115     f4a_drh_prolapse 5.589976e-03
# 116          f4b_bipedal 4.305556e-03
# 117       f4a_water_well 4.026741e-03
# 118     f4a_water_bought 3.203385e-03
# 119       f4a_fuel_grass 9.008586e-04
# 120                 site 0.000000e+00
# 121       f4a_house_cart 0.000000e+00
# 122       f4a_house_boat 0.000000e+00
# 123      f4a_fuel_biogas 0.000000e+00
# 124        f4a_fuel_crop 0.000000e+00
# 125       f4a_fuel_other 0.000000e+00
# 126    f4a_water_covwell 0.000000e+00
# 127   f4a_water_covpwell 0.000000e+00
# 128  f4a_water_prospring 0.000000e+00
# 129   f4a_water_unspring 0.000000e+00
# 130    f4a_water_pubwell 0.000000e+00
# 131      f4a_water_river 0.000000e+00
# 132       f4a_water_pond 0.000000e+00
# 133       f4a_water_rain 0.000000e+00
# 134       f4a_water_bore 0.000000e+00
# 135      f4a_seek_healer 0.000000e+00

# AUC          SE     lower     upper level Model nvar
# 1 0.7551037 0.003409296 0.7484216 0.7617858  0.95    LR    5
# 2 0.7714008 0.003307398 0.7649185 0.7778832  0.95    LR   10
# 3 0.7354566 0.003566921 0.7284656 0.7424477  0.95    RF    5
# 4 0.7607934 0.003383891 0.7541610 0.7674257  0.95    RF   10

# nvar    intc intc_LCI intc_UCI slope slope_LCI slope_UCI
# <dbl>   <dbl>    <dbl>    <dbl> <dbl>     <dbl>     <dbl>
#   1     5 -0.0163   -0.346    0.297 0.965     0.621      1.35
# 2    10 -0.0135   -0.352    0.308 0.884     0.582      1.23

################### by country growth falter - Bang ####
summary(cases_gf_Bang$f4a_fac_waste)
cases_gf_Bang_complete <- cases_gf_Bang
cases_gf_Bang_complete$f4a_fac_waste <- as.factor(ifelse(cases_gf_Bang_complete$f4a_fac_waste==1,6,cases_gf_Bang_complete$f4a_fac_waste))
summary(cases_gf_Bang_complete$f4a_fac_waste)

GF.Bang <- CPR.funct(data=cases_gf_Bang_complete,outcome="haz_0.5",iter=100,nvars_opts=c(5,10))
GF.Bang[["df_imps"]]
GF.Bang[["AUC_df"]]
GF.Bang[["calib"]]

# names      var_red
# 1               base_age 15.992211997
# 2                f4b_haz 15.455265839
# 3               f4b_resp 10.417970594
# 4               f4b_temp  9.927338211
# 5          f4a_ppl_house  7.027169264
# 6           f4a_drh_days  5.434688195
# 7          f4a_fac_waste  4.711101874
# 8          f4a_slp_rooms  4.701627255
# 9          f4a_share_fac  4.371175198
# 10         f4a_prim_schl  3.505505882
# 11        f4a_max_stools  2.893332334
# 12          f4a_dad_live  2.719018338
# 13      f4a_yng_children  2.538019270
# 14        f4a_disp_feces  2.488647357
# 15        f4a_offr_drink  2.138582252
# 16          f4a_wash_use  2.111029667
# 17         f4b_recommend  2.093926832
# 18             f3_gender  1.962319399
# 19        f4a_wash_child  1.955525838
# 20         f4a_wash_cook  1.950384079
# 21        f4a_wash_nurse  1.924674846
# 22          f4a_ani_fowl  1.852258304
# 23           f4a_ani_cow  1.833130567
# 24         f4a_breastfed  1.823930761
# 25       f4a_wash_animal  1.791537018
# 26        f4a_house_elec  1.790629394
# 27        f4a_hometrt_ab  1.761546238
# 28      f4a_drh_restless  1.746548967
# 29         f4a_drh_vomit  1.742260745
# 30         f4a_fuel_dung  1.738637794
# 31       f4a_house_radio  1.732888728
# 32      f4b_nature_stool  1.731673642
# 33   f4a_water_shallwell  1.731432359
# 34        f4a_house_tele  1.718576971
# 35     f4a_drh_bellypain  1.687180778
# 36        f4a_house_bike  1.678059056
# 37    f4a_drh_cough_miss  1.670595591
# 38    f4a_water_deepwell  1.660793875
# 39         f4a_fuel_crop  1.648096991
# 40        f4a_seek_pharm  1.637227292
# 41         f4a_drh_blood  1.584206826
# 42       f4a_hometrt_ors  1.577001237
# 43             f4b_admit  1.551080113
# 44        f4a_drh_strain  1.549353411
# 45       f4a_cur_thirsty  1.548480258
# 46           f3_drh_hosp  1.531459476
# 47      f4a_cur_drymouth  1.516467728
# 48       f4a_house_phone  1.510175764
# 49  f4a_drh_lethrgy_miss  1.508095242
# 50          f4a_wash_def  1.496999370
# 51      f4a_house_agland  1.489696877
# 52      f4a_cur_restless  1.480930094
# 53        f4a_fuel_grass  1.466623967
# 54         f4a_wash_othr  1.454377707
# 55          f4a_wash_eat  1.448042656
# 56      f4a_hometrt_none  1.434816813
# 57         f4a_fuel_wood  1.431227707
# 58           f4a_ani_dog  1.406762054
# 59       f4a_ani_rodents  1.375417698
# 60      f4a_seek_outside  1.366602701
# 61        f4a_seek_remdy  1.345468278
# 62        f4a_drh_thirst  1.315718838
# 63             f4a_floor  1.250969044
# 64     f4a_hometrt_othr1  1.212436775
# 65     f4a_hometrt_othr2  1.180862153
# 66           f4a_ani_cat  1.156374746
# 67      f4a_hometrt_zinc  1.152232112
# 68          f4a_cur_skin  1.117868441
# 69          f4a_ani_goat  1.020014927
# 70          f4a_seek_doc  1.002018117
# 71              f4b_eyes  0.974981392
# 72            f4b_mental  0.947357513
# 73             f3_drh_iv  0.885386427
# 74        f4a_house_none  0.781154500
# 75       f4a_house_scoot  0.738342849
# 76         f4a_ani_sheep  0.737210771
# 77      f4a_house_fridge  0.706603957
# 78          f4a_drh_conv  0.636100969
# 79        f4a_trt_method  0.590455474
# 80      f4a_drh_prolapse  0.584047755
# 81            f4a_ani_no  0.522625739
# 82         f4a_trt_water  0.509555072
# 83         f3_drh_turgor  0.490030064
# 84       f4a_fuel_natgas  0.469329239
# 85        f4b_under_nutr  0.437232347
# 86    f4a_cur_fastbreath  0.435410887
# 87              f4b_skin  0.394641348
# 88        f4a_house_cart  0.383408453
# 89             f4b_mouth  0.358175440
# 90      f4a_hometrt_herb  0.343583628
# 91        f4a_house_boat  0.332705985
# 92     f4a_drh_lessdrink  0.320411080
# 93         f4a_drh_consc  0.318961118
# 94      f4a_fuel_propane  0.274776385
# 95     f4a_hometrt_maize  0.259525937
# 96         f4a_house_car  0.235459178
# 97       f4a_seek_healer  0.234802980
# 98      f4a_seek_privdoc  0.229832983
# 99     f4a_water_covwell  0.228528803
# 100      f4b_chest_indrw  0.210565077
# 101       f4a_water_well  0.165708834
# 102         f4a_ms_water  0.159096424
# 103    f4a_fuel_charcoal  0.139392926
# 104      f4a_water_house  0.110308276
# 105       f4a_fuel_other  0.092426599
# 106          f4b_bipedal  0.087102852
# 107     f4a_relationship  0.021910565
# 108      f4a_store_water  0.019517809
# 109           f4b_rectal  0.016624236
# 110      f4a_seek_friend  0.012850846
# 111        f4a_fuel_elec  0.012231162
# 112  f4a_hometrt_othrliq  0.009629447
# 113        f4a_fuel_kero  0.007805464
# 114      f4a_water_avail  0.007553802
# 115    f4a_water_pubwell  0.003715285
# 116        f4a_ani_other  0.001087912
# 117                 site  0.000000000
# 118      f4a_fuel_biogas  0.000000000
# 119        f4a_fuel_coal  0.000000000
# 120       f4a_water_yard  0.000000000
# 121   f4a_water_covpwell  0.000000000
# 122     f4a_water_pubtap  0.000000000
# 123  f4a_water_prospring  0.000000000
# 124   f4a_water_unspring  0.000000000
# 125      f4a_water_river  0.000000000
# 126       f4a_water_pond  0.000000000
# 127       f4a_water_rain  0.000000000
# 128     f4a_water_bought  0.000000000
# 129       f4a_water_othr  0.000000000
# 130       f4a_water_bore  0.000000000
# 131     f4a_hometrt_milk  0.000000000
# 132       f4a_seek_other  0.000000000
# 133         f4b_abn_hair  0.000000000
# 134       f4b_skin_flaky  0.000000000
# 135    f4b_observe_stool  0.000000000

# AUC          SE     lower     upper level Model nvar
# 1 0.7121055 0.003543283 0.7051608 0.7190502  0.95    LR    5
# 2 0.7035398 0.003534854 0.6966116 0.7104680  0.95    LR   10
# 3 0.6831493 0.003620424 0.6760534 0.6902452  0.95    RF    5
# 4 0.6916552 0.003621528 0.6845572 0.6987533  0.95    RF   10

# nvar    intc intc_LCI intc_UCI slope slope_LCI slope_UCI
# <dbl>   <dbl>    <dbl>    <dbl> <dbl>     <dbl>     <dbl>
#   1     5 -0.0395   -0.353    0.261 1.00      0.602      1.45
# 2    10 -0.0380   -0.354    0.265 0.805     0.453      1.19

################### by country growth falter - Pak ####
GF.Pak <- CPR.funct(data=cases_gf_Pak,outcome="haz_0.5",iter=100,nvars_opts=c(5,10))
GF.Pak[["df_imps"]]
GF.Pak[["AUC_df"]]
GF.Pak[["calib"]]

# names      var_red
# 1               base_age 1.063968e+01
# 2                f4b_haz 9.030872e+00
# 3               f4b_temp 7.157936e+00
# 4               f4b_resp 6.895398e+00
# 5          f4a_ppl_house 5.310283e+00
# 6           f4a_drh_days 3.415322e+00
# 7       f4a_yng_children 3.017623e+00
# 8          f4a_slp_rooms 2.929742e+00
# 9          f4a_prim_schl 2.779773e+00
# 10             f3_drh_iv 2.362648e+00
# 11         f4b_recommend 2.168605e+00
# 12         f4a_share_fac 2.162022e+00
# 13        f4a_offr_drink 2.141527e+00
# 14         f4a_breastfed 2.007453e+00
# 15        f4a_trt_method 1.964544e+00
# 16        f4a_max_stools 1.894542e+00
# 17       f4a_water_avail 1.878792e+00
# 18     f4a_drh_bellypain 1.751672e+00
# 19            f4b_mental 1.697971e+00
# 20          f4a_ms_water 1.697682e+00
# 21             f4b_mouth 1.668625e+00
# 22        f4a_disp_feces 1.504898e+00
# 23         f4a_fac_waste 1.503561e+00
# 24    f4a_drh_cough_miss 1.314603e+00
# 25         f4a_drh_vomit 1.262012e+00
# 26             f3_gender 1.250074e+00
# 27  f4a_drh_lethrgy_miss 1.230022e+00
# 28         f3_drh_turgor 1.202978e+00
# 29       f4a_house_phone 1.164557e+00
# 30         f4a_trt_water 1.156389e+00
# 31      f4a_relationship 1.155844e+00
# 32        f4a_house_tele 1.153765e+00
# 33          f4a_wash_use 1.152076e+00
# 34        f4a_wash_child 1.152031e+00
# 35    f4a_cur_fastbreath 1.126550e+00
# 36      f4a_cur_drymouth 1.119658e+00
# 37       f4a_cur_thirsty 1.112085e+00
# 38          f4a_wash_def 1.060936e+00
# 39              f4b_skin 1.060243e+00
# 40      f4a_seek_outside 1.051916e+00
# 41            f4a_ani_no 1.050806e+00
# 42       f4a_store_water 1.049925e+00
# 43      f4a_cur_restless 1.036974e+00
# 44        f4a_wash_nurse 1.033645e+00
# 45        f4a_drh_thirst 1.025225e+00
# 46      f4a_hometrt_none 1.020620e+00
# 47       f4a_water_house 1.019899e+00
# 48        f4a_drh_strain 1.013854e+00
# 49          f4a_cur_skin 1.013093e+00
# 50       f4a_hometrt_ors 9.901571e-01
# 51        f4b_under_nutr 9.868859e-01
# 52     f4a_drh_lessdrink 9.716423e-01
# 53             f4a_floor 9.500475e-01
# 54      f4a_house_fridge 9.437988e-01
# 55       f4a_house_scoot 9.392284e-01
# 56          f4a_wash_eat 9.248429e-01
# 57      f4a_drh_restless 9.182633e-01
# 58          f4a_ani_goat 9.074394e-01
# 59         f4a_wash_cook 9.015962e-01
# 60      f4a_seek_privdoc 8.899935e-01
# 61      f4a_water_bought 8.783368e-01
# 62         f4a_drh_blood 8.287467e-01
# 63       f4a_fuel_natgas 8.078868e-01
# 64          f4a_ani_fowl 7.937874e-01
# 65              f4b_eyes 7.822678e-01
# 66     f4a_hometrt_othr1 7.808812e-01
# 67         f4a_fuel_wood 7.460616e-01
# 68      f4b_nature_stool 7.314007e-01
# 69        f4a_seek_pharm 7.272726e-01
# 70         f4a_fuel_kero 7.221161e-01
# 71        f4a_house_bike 7.111921e-01
# 72        f4a_hometrt_ab 7.104600e-01
# 73           f4a_ani_cow 7.103457e-01
# 74      f4a_water_pubtap 6.880563e-01
# 75          f4a_seek_doc 6.855219e-01
# 76      f4a_hometrt_zinc 6.817475e-01
# 77        f4a_water_yard 6.332967e-01
# 78       f4a_house_radio 6.000122e-01
# 79         f4a_drh_consc 5.955145e-01
# 80           f4a_ani_cat 5.861217e-01
# 81       f4a_wash_animal 5.831491e-01
# 82        f4a_water_othr 5.604023e-01
# 83     f4a_hometrt_maize 5.544374e-01
# 84          f4a_dad_live 5.531853e-01
# 85         f4a_wash_othr 5.491048e-01
# 86        f4a_fuel_grass 5.453877e-01
# 87     f4a_hometrt_othr2 5.035765e-01
# 88           f4a_ani_dog 5.018777e-01
# 89        f4a_house_boat 4.325379e-01
# 90        f4a_house_none 4.314414e-01
# 91      f4a_hometrt_herb 4.289553e-01
# 92       f4a_fuel_biogas 4.196083e-01
# 93       f4b_chest_indrw 4.173780e-01
# 94        f4a_house_elec 4.033030e-01
# 95          f4b_abn_hair 3.872357e-01
# 96        f4b_skin_flaky 3.830558e-01
# 97        f4a_seek_other 3.609889e-01
# 98   f4a_hometrt_othrliq 3.470029e-01
# 99           f3_drh_hosp 3.274687e-01
# 100       f4a_house_cart 3.060559e-01
# 101     f4a_drh_prolapse 2.990660e-01
# 102        f4a_ani_other 2.414255e-01
# 103        f4a_house_car 2.135963e-01
# 104     f4a_house_agland 2.037362e-01
# 105      f4a_seek_healer 2.034082e-01
# 106        f4a_fuel_dung 1.974180e-01
# 107        f4a_ani_sheep 1.926353e-01
# 108       f4a_water_bore 1.823860e-01
# 109       f4a_seek_remdy 1.322235e-01
# 110     f4a_hometrt_milk 1.024878e-01
# 111            f4b_admit 8.501591e-02
# 112    f4a_water_covwell 7.213989e-02
# 113           f4b_rectal 7.056180e-02
# 114         f4a_drh_conv 4.043283e-02
# 115     f4a_fuel_propane 1.306946e-02
# 116          f4b_bipedal 5.963814e-03
# 117      f4a_ani_rodents 3.726625e-03
# 118      f4a_seek_friend 2.169604e-03
# 119       f4a_water_pond 1.943644e-03
# 120        f4a_fuel_elec 1.743034e-03
# 121   f4a_water_covpwell 9.425483e-04
# 122                 site 0.000000e+00
# 123        f4a_fuel_coal 0.000000e+00
# 124    f4a_fuel_charcoal 0.000000e+00
# 125        f4a_fuel_crop 0.000000e+00
# 126       f4a_fuel_other 0.000000e+00
# 127  f4a_water_prospring 0.000000e+00
# 128       f4a_water_well 0.000000e+00
# 129   f4a_water_unspring 0.000000e+00
# 130    f4a_water_pubwell 0.000000e+00
# 131      f4a_water_river 0.000000e+00
# 132   f4a_water_deepwell 0.000000e+00
# 133       f4a_water_rain 0.000000e+00
# 134  f4a_water_shallwell 0.000000e+00
# 135    f4b_observe_stool 0.000000e+00

# AUC          SE     lower     upper level Model nvar
# 1 0.6768255 0.004193596 0.6686062 0.6850448  0.95    LR    5
# 2 0.6783018 0.004219604 0.6700315 0.6865721  0.95    LR   10
# 3 0.6244680 0.004461196 0.6157242 0.6332118  0.95    RF    5
# 4 0.6457302 0.004484582 0.6369406 0.6545198  0.95    RF   10

# nvar    intc intc_LCI intc_UCI slope slope_LCI slope_UCI
# <dbl>   <dbl>    <dbl>    <dbl> <dbl>     <dbl>     <dbl>
#   1     5 0.00806   -0.357    0.356 0.995     0.483      1.60
# 2    10 0.00804   -0.365    0.363 0.725     0.339      1.17

################### by country growth falter - Africa, val in Asia ####
GF.Afr <- CPR.funct(data=cases_gf_Afr,outcome="haz_0.5",iter=100,nvars_opts=c(1:10))
GF.Afr[["df_imps"]]
GF.Afr[["AUC_df"]]
GF.Afr[["calib"]]

# names     var_red
# 1               base_age 57.10262405
# 2                f4b_haz 44.24762393
# 3               f4b_resp 29.75596682
# 4               f4b_temp 26.72987026
# 5          f4a_ppl_house 21.59905392
# 6          f4a_breastfed 18.25355763
# 7          f4a_slp_rooms 15.60292624
# 8           f4a_drh_days 13.59066709
# 9       f4a_yng_children 13.51014807
# 10         f4a_share_fac 10.63364086
# 11         f4a_prim_schl  9.66344186
# 12        f4a_offr_drink  9.41890877
# 13          f4a_dad_live  7.56507403
# 14          f4a_ms_water  6.33785443
# 15         f4a_drh_vomit  6.33605711
# 16                  site  6.27818967
# 17        f4a_disp_feces  6.25546209
# 18     f4a_drh_bellypain  6.12204399
# 19       f4a_water_avail  5.95895111
# 20         f4b_recommend  5.68003276

# AUC          SE     lower     upper level Model nvar
# 1  0.6960657 0.001932004 0.6922790 0.6998523  0.95    LR    1
# 2  0.7217561 0.001898149 0.7180358 0.7254764  0.95    LR    2
# 3  0.7219368 0.001896142 0.7182204 0.7256532  0.95    LR    3
# 4  0.7269166 0.001885593 0.7232209 0.7306123  0.95    LR    4
# 5  0.7266771 0.001888584 0.7229756 0.7303787  0.95    LR    5
# 6  0.7266815 0.001886555 0.7229840 0.7303791  0.95    LR    6
# 7  0.7259301 0.001887654 0.7222303 0.7296298  0.95    LR    7
# 8  0.7255394 0.001889684 0.7218357 0.7292431  0.95    LR    8
# 9  0.7254812 0.001890899 0.7217751 0.7291873  0.95    LR    9
# 10 0.7263290 0.001887481 0.7226296 0.7300284  0.95    LR   10
# 11 0.6937189 0.001970490 0.6898568 0.6975810  0.95    RF    1
# 12 0.6942569 0.002001299 0.6903344 0.6981794  0.95    RF    2
# 13 0.6962220 0.001982624 0.6923361 0.7001078  0.95    RF    3
# 14 0.7000508 0.001977154 0.6961756 0.7039259  0.95    RF    4
# 15 0.7006481 0.001964094 0.6967985 0.7044976  0.95    RF    5
# 16 0.7087007 0.001941394 0.7048956 0.7125058  0.95    RF    6
# 17 0.7134216 0.001936496 0.7096262 0.7172171  0.95    RF    7
# 18 0.7147910 0.001929779 0.7110087 0.7185733  0.95    RF    8
# 19 0.7169556 0.001925247 0.7131822 0.7207290  0.95    RF    9
# 20 0.7185634 0.001916599 0.7148069 0.7223198  0.95    RF   10

# nvar      intc intc_LCI intc_UCI slope slope_LCI slope_UCI
# <int>     <dbl>    <dbl>    <dbl> <dbl>     <dbl>     <dbl>
#   1     1 -0.000188   -0.174    0.170 1.01      0.766      1.28
# 2     2 -0.00294    -0.178    0.169 1.02      0.793      1.27
# 3     3 -0.00305    -0.179    0.169 1.02      0.790      1.27
# 4     4 -0.00396    -0.180    0.168 1.02      0.791      1.26
# 5     5 -0.00373    -0.180    0.169 1.01      0.787      1.25
# 6     6 -0.00333    -0.180    0.169 1.01      0.785      1.25
# 7     7 -0.00360    -0.180    0.169 1.00      0.780      1.24
# 8     8 -0.00352    -0.180    0.169 0.998     0.777      1.24
# 9     9 -0.00346    -0.180    0.169 0.995     0.774      1.23
# 10    10 -0.00335    -0.180    0.169 0.993     0.773      1.23


GEMS_glm_gf_Afr_2var <- glm(haz_0.5~base_age+f4b_haz,
                        data=cases_gf_Afr,family="binomial",control=glm.control(maxit=50))
summary(GEMS_glm_gf_Afr_2var)
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  0.247666   0.075115   3.297 0.000977 ***
#   base_age    -0.080911   0.004982 -16.239  < 2e-16 ***
#   f4b_haz      0.240613   0.033870   7.104 1.21e-12 ***

round(exp(coef(GEMS_glm_gf_Afr_2var)),2)
# (Intercept)    base_age     f4b_haz 
# 1.28        0.92        1.27 
round(exp(confint(GEMS_glm_gf_Afr_2var)),2)
# 2.5 % 97.5 %
#   (Intercept)  1.11   1.49
# base_age     0.91   0.93
# f4b_haz      1.19   1.36

Afr_2var_AfrFit<-cases_gf_Afr %>% select(haz_0.5,base_age,f4b_haz)
Afr_2var_AfrFit$Afr_pred_glm <- as.numeric(predict(GEMS_glm_gf_Afr_2var,newdata=Afr_2var_AfrFit,type="response"))
Afr_2var_AfrFit_AUC <- roc(response=Afr_2var_AfrFit$haz_0.5,predictor=Afr_2var_AfrFit$Afr_pred_glm)
paste(round(Afr_2var_AfrFit_AUC$auc,2)," (",
      round(ci.auc(Afr_2var_AfrFit_AUC)[1],2),", ",
      round(ci.auc(Afr_2var_AfrFit_AUC)[3],2),")",sep="")
# "0.72 (0.7, 0.74)"

Asia_2var_AfrFit<-cases_gf_Asia %>% select(haz_0.5,base_age,f4b_haz)
Asia_2var_AfrFit$Afr_pred_glm <- as.numeric(predict(GEMS_glm_gf_Afr_2var,newdata=Asia_2var_AfrFit,type="response"))
Asia_2var_AfrFit_AUC <- roc(response=Asia_2var_AfrFit$haz_0.5,predictor=Asia_2var_AfrFit$Afr_pred_glm)
paste(round(Asia_2var_AfrFit_AUC$auc,2)," (",
      round(ci.auc(Asia_2var_AfrFit_AUC)[1],2),", ",
      round(ci.auc(Asia_2var_AfrFit_AUC)[3],2),")",sep="")
# "0.7 (0.68, 0.72)"

################### by country growth falter - Asia, val in Africa ####
GF.Asia <- CPR.funct(data=cases_gf_Asia,outcome="haz_0.5",iter=100,nvars_opts=c(1:10))
GF.Asia[["df_imps"]]
GF.Asia[["AUC_df"]]
GF.Asia[["calib"]]

# names      var_red
# 1               base_age 43.394018396
# 2                f4b_haz 39.162187310
# 3               f4b_resp 26.198910692
# 4               f4b_temp 25.746587237
# 5          f4a_ppl_house 17.552034280
# 6          f4a_share_fac 13.849865054
# 7           f4a_drh_days 12.476471870
# 8          f4a_slp_rooms 10.627188462
# 9          f4a_prim_schl 10.098786483
# 10         f4a_fac_waste  8.854699781
# 11         f4b_recommend  7.855568018
# 12      f4a_yng_children  7.770585683
# 13        f4a_offr_drink  7.230399947
# 14        f4a_max_stools  6.898982695
# 15        f4a_disp_feces  6.437183038
# 16         f4a_breastfed  6.236767163
# 17     f4a_drh_bellypain  5.929644952
# 18          f4a_wash_use  5.389219430
# 19            f4b_mental  5.077336505
# 20          f4a_ms_water  4.731808133
# 21             f3_gender  4.694774205
# 22        f4a_trt_method  4.544110761
# 23        f4a_wash_child  4.405502868
# 24        f4a_drh_thirst  4.358823224
# 25        f4a_wash_nurse  4.338956269
# 26    f4a_drh_cough_miss  4.268903451
# 27       f4a_cur_thirsty  4.250888501
# 28         f4a_drh_vomit  4.187900495
# 29             f3_drh_iv  4.056148882
# 30        f4a_house_tele  4.036770545
# 31          f4a_ani_fowl  3.986750978
# 32         f4a_wash_cook  3.973577607
# 33          f4a_dad_live  3.913027829
# 34  f4a_drh_lethrgy_miss  3.906574167
# 35       f4a_hometrt_ors  3.892174404
# 36        f4a_house_bike  3.849774094
# 37       f4a_house_phone  3.842224422
# 38          f4a_wash_eat  3.841517159
# 39          f4a_wash_def  3.838995223
# 40      f4a_hometrt_none  3.744705302
# 41      f4a_drh_restless  3.729994916
# 42      f4a_cur_restless  3.647107191
# 43       f4a_water_avail  3.629096738
# 44         f4a_fuel_wood  3.619510458
# 45       f4a_house_radio  3.601355729
# 46      f4a_seek_outside  3.593278671
# 47             f4b_admit  3.537389239
# 48      f4a_cur_drymouth  3.455451307
# 49             f4b_mouth  3.450135631
# 50          f4a_cur_skin  3.373349817
# 51             f4a_floor  3.315650668
# 52        f4a_house_elec  3.281507321
# 53           f4a_ani_dog  3.279967615
# 54        f4a_hometrt_ab  3.252335878
# 55     f4a_drh_lessdrink  3.173847565
# 56      f4b_nature_stool  3.157606149
# 57           f3_drh_hosp  3.134613197
# 58           f4a_ani_cat  3.131021629
# 59         f4a_trt_water  2.999275206
# 60        f4a_drh_strain  2.996284004
# 61       f4a_store_water  2.984713334
# 62       f4a_wash_animal  2.906434962
# 63     f4a_hometrt_othr1  2.900442362
# 64           f4a_ani_cow  2.823392627
# 65        f4a_seek_pharm  2.776757580
# 66         f4a_drh_blood  2.758771237
# 67          f4a_ani_goat  2.685885247
# 68         f3_drh_turgor  2.664442325
# 69    f4a_water_deepwell  2.594359652
# 70       f4a_ani_rodents  2.573042059
# 71         f4a_fuel_kero  2.491627510
# 72         f4a_fuel_dung  2.474566719
# 73       f4a_house_scoot  2.449228501
# 74              f4b_skin  2.437066802
# 75                  site  2.413726135
# 76      f4a_house_fridge  2.395771859
# 77   f4a_water_shallwell  2.373143358
# 78              f4b_eyes  2.348074643
# 79    f4a_cur_fastbreath  2.325223144
# 80      f4a_house_agland  2.303167978
# 81      f4a_seek_privdoc  2.295216390
# 82         f4a_wash_othr  2.257062787
# 83     f4a_hometrt_othr2  2.205117605
# 84      f4a_water_pubtap  2.200490857
# 85          f4a_seek_doc  2.199549726
# 86        f4a_fuel_grass  2.188993248
# 87      f4a_hometrt_zinc  2.146074536
# 88        f4a_water_yard  2.100557971
# 89         f4a_fuel_crop  2.090813991
# 90            f4a_ani_no  2.046911534
# 91     f4a_hometrt_maize  1.995187088
# 92       f4a_fuel_natgas  1.977060982
# 93         f4a_fuel_coal  1.947177045
# 94       f4a_water_house  1.915465733
# 95        f4b_under_nutr  1.904088828
# 96      f4a_fuel_propane  1.850175534
# 97        f4a_seek_remdy  1.765773519
# 98        f4a_house_none  1.650215981
# 99      f4a_relationship  1.476377637
# 100     f4a_water_bought  1.279484651
# 101        f4a_ani_other  1.266889919
# 102       f4a_seek_other  1.164722439
# 103       f4a_water_othr  1.108734044
# 104        f4a_ani_sheep  1.009671159
# 105     f4a_drh_prolapse  0.970743836
# 106        f4a_house_car  0.881226351
# 107        f4a_drh_consc  0.819511902
# 108       f4a_house_cart  0.785340911
# 109     f4a_hometrt_herb  0.767702936
# 110         f4a_drh_conv  0.748712632
# 111       f4a_house_boat  0.730504696
# 112      f4b_chest_indrw  0.669674385
# 113  f4a_hometrt_othrliq  0.635331587
# 114    f4a_fuel_charcoal  0.496573095
# 115         f4b_abn_hair  0.474782998
# 116      f4a_seek_healer  0.457038360
# 117      f4a_fuel_biogas  0.454031572
# 118       f4b_skin_flaky  0.383333216
# 119    f4a_water_covwell  0.310059634
# 120    f4b_observe_stool  0.265099741
# 121       f4a_water_well  0.224994066
# 122     f4a_hometrt_milk  0.157637340
# 123        f4a_fuel_elec  0.152432067
# 124       f4a_water_bore  0.144163456
# 125      f4a_seek_friend  0.103116884
# 126          f4b_bipedal  0.092291793
# 127           f4b_rectal  0.082326273
# 128       f4a_fuel_other  0.070772376
# 129       f4a_water_pond  0.002835225
# 130    f4a_water_pubwell  0.001376963
# 131   f4a_water_covpwell  0.000000000
# 132  f4a_water_prospring  0.000000000
# 133   f4a_water_unspring  0.000000000
# 134      f4a_water_river  0.000000000
# 135       f4a_water_rain  0.000000000

# AUC          SE     lower     upper level Model nvar
# 1  0.6780355 0.002205548 0.6737127 0.6823583  0.95    LR    1
# 2  0.7002125 0.002162853 0.6959734 0.7044517  0.95    LR    2
# 3  0.7017679 0.002156911 0.6975404 0.7059954  0.95    LR    3
# 4  0.7151814 0.002142659 0.7109818 0.7193809  0.95    LR    4
# 5  0.7168104 0.002134363 0.7126272 0.7209937  0.95    LR    5
# 6  0.7162167 0.002136637 0.7120290 0.7204044  0.95    LR    6
# 7  0.7161437 0.002147764 0.7119342 0.7203533  0.95    LR    7
# 8  0.7160615 0.002148139 0.7118512 0.7202717  0.95    LR    8
# 9  0.7177553 0.002145614 0.7135500 0.7219606  0.95    LR    9
# 10 0.7204542 0.002132374 0.7162748 0.7246336  0.95    LR   10
# 11 0.6614264 0.002242712 0.6570308 0.6658221  0.95    RF    1
# 12 0.6593439 0.002269015 0.6548967 0.6637911  0.95    RF    2
# 13 0.6684152 0.002236634 0.6640315 0.6727990  0.95    RF    3
# 14 0.6873872 0.002194607 0.6830859 0.6916886  0.95    RF    4
# 15 0.6943380 0.002168187 0.6900885 0.6985876  0.95    RF    5
# 16 0.6996902 0.002148439 0.6954794 0.7039011  0.95    RF    6
# 17 0.6998748 0.002150576 0.6956598 0.7040899  0.95    RF    7
# 18 0.7051347 0.002141973 0.7009365 0.7093329  0.95    RF    8
# 19 0.7071173 0.002141621 0.7029198 0.7113148  0.95    RF    9
# 20 0.7106430 0.002128009 0.7064721 0.7148138  0.95    RF   10

# nvar   intc intc_LCI intc_UCI slope slope_LCI slope_UCI
# <int>  <dbl>    <dbl>    <dbl> <dbl>     <dbl>     <dbl>
#   1     1 0.0132   -0.171    0.192 1.01      0.715      1.33
# 2     2 0.0129   -0.173    0.194 1.01      0.747      1.30
# 3     3 0.0132   -0.173    0.194 0.999     0.738      1.28
# 4     4 0.0158   -0.172    0.198 1.01      0.760      1.28
# 5     5 0.0148   -0.173    0.197 1.01      0.759      1.27
# 6     6 0.0145   -0.173    0.197 1.00      0.756      1.27
# 7     7 0.0147   -0.173    0.197 0.994     0.749      1.26
# 8     8 0.0145   -0.173    0.197 0.982     0.740      1.24
# 9     9 0.0151   -0.173    0.199 0.953     0.720      1.20
# 10    10 0.0140   -0.175    0.198 0.928     0.703      1.17

GEMS_glm_gf_Asia_2var <- glm(haz_0.5~base_age+f4b_haz,
                            data=cases_gf_Asia,family="binomial",control=glm.control(maxit=50))
summary(GEMS_glm_gf_Asia_2var)
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  0.025252   0.082180   0.307    0.759    
# base_age    -0.069413   0.005223 -13.290  < 2e-16 ***
#   f4b_haz      0.271430   0.036264   7.485 7.17e-14 ***

round(exp(coef(GEMS_glm_gf_Asia_2var)),2)
# (Intercept)    base_age     f4b_haz 
# 1.03        0.93        1.31 
round(exp(confint(GEMS_glm_gf_Asia_2var)),2)
# 2.5 % 97.5 %
#   (Intercept)  0.87   1.21
# base_age     0.92   0.94
# f4b_haz      1.22   1.41

Asia_2var_AsiaFit<-cases_gf_Asia %>% select(haz_0.5,base_age,f4b_haz)
Asia_2var_AsiaFit$Asia_pred_glm <- as.numeric(predict(GEMS_glm_gf_Asia_2var,newdata=Asia_2var_AsiaFit,type="response"))
Asia_2var_AsiaFit_AUC <- roc(response=Asia_2var_AsiaFit$haz_0.5,predictor=Asia_2var_AsiaFit$Asia_pred_glm)
paste(round(Asia_2var_AsiaFit_AUC$auc,2)," (",
      round(ci.auc(Asia_2var_AsiaFit_AUC)[1],2),", ",
      round(ci.auc(Asia_2var_AsiaFit_AUC)[3],2),")",sep="")
# [1] "0.7 (0.68, 0.72)"

Afr_2var_AsiaFit<-cases_gf_Afr %>% select(haz_0.5,base_age,f4b_haz)
Afr_2var_AsiaFit$Asia_pred_glm <- as.numeric(predict(GEMS_glm_gf_Asia_2var,newdata=Afr_2var_AsiaFit,type="response"))
Afr_2var_AsiaFit_AUC <- roc(response=Afr_2var_AsiaFit$haz_0.5,predictor=Afr_2var_AsiaFit$Asia_pred_glm)
paste(round(Afr_2var_AsiaFit_AUC$auc,2)," (",
      round(ci.auc(Afr_2var_AsiaFit_AUC)[1],2),", ",
      round(ci.auc(Afr_2var_AsiaFit_AUC)[3],2),")",sep="")
# [1] "0.72 (0.71, 0.74)"

################### growth falter - not stunted at enrollment (HAZ>=-2) ####
GF.notStunt <- CPR.funct(data=cases_gf_notStunt,outcome="haz_0.5",iter=10,nvars_opts=c(5,10))
GF.notStunt[["df_imps"]]
GF.notStunt[["AUC_df"]]
GF.notStunt[["calib"]]

################### predict stunting at f-up, not growth faltering ####
post.stunt <- CPR.funct(data=complete_gf,outcome="post.stunt",iter=10,nvars_opts=c(5,10))
post.stunt[["df_imps"]]
post.stunt[["AUC_df"]]
post.stunt[["calib"]]


################### predict stunting at f-up among not stunted at enrollment ####
post.stunt2 <- CPR.funct(data=cases_gf_notStunt,outcome="post.stunt",iter=10,nvars_opts=c(5,10))
post.stunt2[["df_imps"]]
post.stunt2[["AUC_df"]]
post.stunt2[["calib"]]



################### external validation: 2-variable growth faltering 0-23mo ####
GEMS_glm_gf_age4_2var <- glm(haz_0.5~base_age+f4b_haz,
                   data=cases_gf_age4,family="binomial",control=glm.control(maxit=50))
summary(GEMS_glm_gf_age4_2var)
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -0.059304   0.066154  -0.896     0.37    
# base_age    -0.051969   0.005421  -9.587   <2e-16 ***
#   f4b_haz      0.270460   0.025452  10.626   <2e-16 ***
round(coef(GEMS_glm_gf_age4),4)
# (Intercept)    base_age     f4b_haz 
# -0.0593     -0.0520      0.2705
#save(GEMS_glm_gf_age4, file = "/GEMS_glm_gf_age4.Rdata")

round(exp(coef(GEMS_glm_gf_age4_2var)),2)
# (Intercept)    base_age     f4b_haz 
# 0.94        0.95        1.31 
round(exp(confint(GEMS_glm_gf_age4_2var)),2)
# 2.5 % 97.5 %
#   (Intercept)  0.83   1.07
# base_age     0.94   0.96
# f4b_haz      1.25   1.38


GEMS_2var_age4<-cases_gf_age4 %>% select(haz_0.5,base_age,f4b_haz)
GEMS_2var_age4$pred_glm <- as.numeric(predict(GEMS_glm_gf_age4,newdata=GEMS_2var_age4,type="response"))
# GEMS_AUC_age4 <- AUC(predictions=GEMS_2var_age4$pred_glm,labels=GEMS_2var_age4$haz_0.5)
# GEMS_AUC_age4
# # [1] 0.6315417
#using pROC package since need CI
GEMS_AUC_age4 <- roc(response=GEMS_2var_age4$haz_0.5,predictor=GEMS_2var_age4$pred_glm)
paste(round(GEMS_AUC_age4$auc,2)," (",
      round(ci.auc(GEMS_AUC_age4)[1],2),", ",
      round(ci.auc(GEMS_AUC_age4)[3],2),")",sep="")
# "0.63 (0.62, 0.65)"

# summary(GEMS_2var_age4$haz_0.5)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0000  0.0000  0.0000  0.2856  1.0000  1.0000 
# summary(GEMS_2var_age4$base_age)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00    7.00   11.00   11.29   16.00   23.00 
# summary(GEMS_2var_age4$f4b_haz)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -5.830  -1.930  -1.150  -1.185  -0.360   5.520

GEMS_decilesCC_age4 <- GEMS_2var_age4 %>% mutate(decile_glm=ntile(pred_glm,10)) %>%
  group_by(decile_glm) %>% summarize(mean(haz_0.5),mean(pred_glm))
#save(GEMS_decilesCC_age4, file = "/GEMS_decilesCC_age4.Rdata")

#will be slightly different every time since data is a random sample in MAL-ED
load(file = "/MALED_data_age023.Rdata")
MALED_2var_age4<-data_age4 %>% select(haz_0.5,age,HAZ_1) %>% mutate(age_mo=age/30) %>% #GEMS age in months, MALED age is in days
  rename(f4b_haz = HAZ_1,base_age = age_mo)
load(file = "/GEMS_glm_gf_age4.Rdata")
MALED_2var_age4$GEMS_pred_glm <- as.numeric(predict(GEMS_glm_gf_age4,newdata=MALED_2var_age4,type="response"))
# MALED_AUC_age4 <- AUC(predictions=MALED_2var_age4$GEMS_pred_glm,labels=MALED_2var_age4$haz_0.5)
# MALED_AUC_age4
# # [1] 0.6847495
MALED_AUC_age4 <- roc(response=MALED_2var_age4$haz_0.5,predictor=MALED_2var_age4$GEMS_pred_glm)
paste(round(MALED_AUC_age4$auc,2)," (",
      round(ci.auc(MALED_AUC_age4)[1],2),", ",
      round(ci.auc(MALED_AUC_age4)[3],2),")",sep="")
# "0.68 (0.63, 0.74)"

# summary(MALED_2var_age4$haz_0.5)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0000  0.0000  0.0000  0.1846  0.0000  1.0000 
# summary(MALED_2var_age4$age)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 7.0   204.0   342.0   386.6   537.0  1076.0 
# summary(MALED_2var_age4$base_age)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.2333  6.8000 11.4000 12.8853 17.9000 35.8667 
# summary(MALED_2var_age4$f4b_haz)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -4.930  -2.040  -1.340  -1.341  -0.660   3.020


MALED_decilesCC_age4 <- MALED_2var_age4 %>% mutate(decile_glm=ntile(GEMS_pred_glm,10)) %>%
  group_by(decile_glm) %>% summarize(mean(haz_0.5),mean(GEMS_pred_glm))
#save(MALED_decilesCC_age4, file = "/MALED_decilesCC_age4.Rdata")



load(file = "/GEMS_decilesCC_age4.Rdata")
load(file = "/MALED_decilesCC_age4.Rdata")

#jpeg("/GF_CC_GEMS023_MALED.jpg",width=600,height=480,quality=400)
#width and height in pixels (600/72)*300 (480/72)*300. need min 10cm across for eLife
  #((600*1.2)/72)*300 ((480*1.2)/72)*300
#png("/GF_CC_GEMS023_MALED_highres.png",units="px",width=3000,height=2400,res=300)
plot(x=seq(0,1,by=0.1),y=seq(0,1,by=0.1),type="l",
     xlab="Predicted Probability",ylab="Observed Proportion",
     main=expression(paste("Calibration Curve:","">=0.5,Delta,"HAZ in diarrhea cases 0-23mo,")))
title("GEMS-derived CPR",line=0.7,font.main=1)
points(GEMS_decilesCC_age4$`mean(pred_glm)`,GEMS_decilesCC_age4$`mean(haz_0.5)`,col="red",pch=1)
points(MALED_decilesCC_age4$`mean(GEMS_pred_glm)`,MALED_decilesCC_age4$`mean(haz_0.5)`,col="blue",pch=2)
legend("topleft",col=c("red","blue"),c("GEMS data, AUC=0.63 (0.62, 0.65)","MAL-ED data, AUC=0.68 (0.63, 0.74)"),pch=c(1,2))
dev.off()



#calib
intercept <- glm(haz_0.5~1,offset=log(GEMS_pred_glm/(1-GEMS_pred_glm)),family="binomial",data=MALED_2var_age4)
summary(intercept)
confint(intercept)
slope <- glm(haz_0.5~log(GEMS_pred_glm/(1-GEMS_pred_glm)),family="binomial",data=MALED_2var_age4)
summary(slope)
confint(slope)

# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)   
# (Intercept)  -0.3192     0.1080  -2.956  0.00312 **
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 2.5 %     97.5 % 
# -0.5351989 -0.1114307 
# 
# Coefficients:
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                              0.1896     0.2663   0.712    0.476    
# log(GEMS_pred_glm/(1 - GEMS_pred_glm))   1.5491     0.2687   5.765 8.16e-09 ***
  # 2.5 %    97.5 %
#   (Intercept)                            -0.3322444 0.7137498
# log(GEMS_pred_glm/(1 - GEMS_pred_glm))  1.0343467 2.0896629

####################
#Publication - GEMS CONTROLS all duplicates of code lower below
################### define variables ####
controls <- gems1 %>% filter(type=="Control")
#13128

controls=controls %>% mutate(haz_dif = f7_haz - f5_haz) #dat_joined$haz_dif <- dat_joined$f4b_haz - dat_joined$f5_haz
# summary(controls$haz_dif)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# -12.8100  -0.1400   0.0800   0.1034   0.3400  22.8600      814 
controls=controls %>% mutate(haz_1.0=(case_when(haz_dif>=1.0 ~ 1, TRUE~0))) #dat_joined$haz_0.1 <- ifelse(dat_joined$haz_dif<=-1.0,1,0)
# table(controls$haz_1.0)
# 0     1 
# 12641   487
controls=controls %>% mutate(haz_0.5=(case_when(haz_dif>=0.5 ~ 1, TRUE~0))) #dat_joined$haz_0.5 <- ifelse(dat_joined$haz_dif<=-0.5,1,0)
# table(controls$haz_0.5)
# 0     1 
# 11097  2031 
#see pg 51 of notebook for manual checks

controls=controls %>% mutate(change_ht = f5_height - f7_height)
# summary(controls$change_ht)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# -48.933   1.000   1.733   1.907   2.600  33.000     814 

controls$f7_date_date <- as.Date(as.character(controls$f7_date))
controls$f5_date_date <- as.Date(as.character(controls$f5_date))
controls$fup_days <- as.numeric(controls$f5_date_date - controls$f7_date_date)

# #control enrollment antibiotic variables
# abx_controls <- c("f7_med_cotr","f7_med_gent","f7_med_chlor",
#                   "f7_med_eryth","f7_med_azith","f7_med_omacr",
#                   "f7_med_peni","f7_med_amoxy","f7_med_ampi",
#                   "f7_med_nalid","f7_med_cipro","f7_med_sele",
#                   "f7_med_otherant")

#too few so combine
controls$f7_floor <- as.character(controls$f7_floor)
controls <- controls %>% mutate(f7_floor=case_when((f7_floor=="3")~"10",(f7_floor=="4")~"10",TRUE~f7_floor))

#combine to fewer categories: f4a_ms_water

table(controls$f7_ms_water)
#   1    2    3    4    5    6    7    8    9   10   11   12   13   14   15   16   17   18 
# 702 1628 4273  105  387   92 1917 1077   43  166  104   90  407   18  755  742  497  125 
controls=controls %>% mutate(f7_ms_water=(case_when((f7_ms_water==6 | f7_ms_water==13 | f7_ms_water==14) ~ 0, #surface 
                                                         (f7_ms_water==4 | f7_ms_water==5 | f7_ms_water==12 | f7_ms_water==16) ~ 1, #unimproved
                                                         (f7_ms_water==3 | f7_ms_water==7 | f7_ms_water==8 | f7_ms_water==9 |
                                                            f7_ms_water==10 | f7_ms_water==11 | f7_ms_water==15 | f7_ms_water==17) ~ 2, #other improved
                                                         (f7_ms_water==1 | f7_ms_water==2) ~ 3, #piped
                                                         TRUE~4))) #other
# table(controls$test)
# 0    1    2    3    4 
# 517 1324 8832 2330  125 
# #use JMP drinking water services ladder
# #surface (subset of unimproved)
# 6-pond/lake
# 13-river/stream
# 14-dam/earth pan
# #unimproved
# 4-open well in house/yard
# 5-open public well
# 12-unprotected spring
# 16-bought
# #other improved
# 3-public tap
# 7-deep tube well
# 8-shallow tube well
# 9-covered well in house/yard
# 10-covered public well
# 11-protected spring
# 15-rainwater
# 17-bore hole
# #safely managed/ piped into dwelling/plot/yard
# 1-piped into house
# 2-piped into yard
# #other
# 18-other

# table(controls$f7_relation)
#     1     2     3     4     5     6     7     8     9    10 
# 12604   103    45     3   215     8   124     2    12    12  
#creating a combined category for non-father male relation OR non relation (4,6,8,9)
controls$f7_relation <- as.numeric(controls$f7_relation)
controls=controls %>% mutate(f7_relation=(case_when((f7_relation==4 | f7_relation==6 | f7_relation==8 | f7_relation==9 | f7_relation==10) ~ 9, #non-father male relation, no relation, other 
                                                             TRUE~f7_relation))) #what was originally
# table(controls$f7_relation)
# 1     2     3     5     7     9 
# 12604   103    45   215   124    37

#convert to factor:
vars <- c("site","f7_relation","f7_dad_live",
          "f7_prim_schl","f7_floor","f7_ms_water",
          "f7_water_avail","f7_trt_method","f7_disp_feces",
          "f7_fac_waste","f7_wash_use","f7_breastfed",
          "f7_seekcare"
)
controls[vars] <- lapply(controls[vars], factor)
#this works too: test <- cases_orig %>% mutate_at(vars, list(~factor(.)))

################### inclusion/exclusion for growth faltering ####
#no f4b_outcome equivalent for controls

#from Brader_2019:
# Children presenting with prolonged (> 7 days' duration) and 
# persistent (> 14days' duration) diarrhea were excluded
# >>> per Ben 4/8/2020 this already taken care of (prolonged and persistent diarrhea kiddos not in dataset)
# We also excluded children with implausible length/LAZ values
# (LAZ > 6 or < - 6 and change in (delta) LAZ>3; 
# a length gain of > 8 cm for follow-up periods 49-60 days and
# > 10 cm for periods 61-91 days among infants <= 6 months, 
# a length gain of > 4 cm for follow-up periods 49-60 days and 
# > 6 cm for periods 61-91 days among children > 6 months, or 
# length values that were > 1.5 cm lower at follow-up than at enrollment.

controls$keep <- ifelse((controls$base_age<=6 & controls$fup_days>=49 & controls$fup_days<=60 & controls$change_ht<=8),1,
                  ifelse((controls$base_age<=6 & controls$fup_days>=61 & controls$fup_days<=91 & controls$change_ht<=10),1,
                  ifelse((controls$base_age>6 & controls$fup_days>=49 & controls$fup_days<=60 & controls$change_ht<=4),1,
                  ifelse((controls$base_age>6 & controls$fup_days>=61 & controls$fup_days<=91 & controls$change_ht<=6),1,
                  #ifelse((controls$fup_days<49 | controls$fup_days>91),1,0)
                  0))))
# summary(controls$keep)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  0.0000  1.0000  1.0000  0.9737  1.0000  1.0000     793 
# table(controls$keep)
# 0     1 
# 325 12010
controls <- controls %>% filter(keep==1)
#12010 observations
#keep missing for some, only keeping keep==1 drops missing change_ht too

controls <- controls %>% filter((fup_days>=49 & fup_days<=91)& #n=; f-up time period criteria
                                    (f7_haz_f==0)& #n=; f4b_haz_f: Flag for _ZLEN<-6 or _ZLEN>6
                                    (abs(haz_dif)<=3.0)& #n=
                                    (keep==1)& #n=
                                    (change_ht>=-1.5)& #
                                    (f5_status==1)& #n=; (==1 60d f-up conducted, 0==not conducted)
                                    (!is.na(haz_dif))) #n=

#cases_gf is now those who meet HAZ plausability and have follow-up HAZ measurement
#table(controls$f4b_outcome_miss,is.na(controls$f5_haz),controls$death_all)


################### drop missing since can't have missing in RF, define "names" variables interested in ####
controls <- controls %>% filter(!is.na(f7_dad_live)&!is.na(f7_prim_schl)&!is.na(f7_ppl_house)&!is.na(f7_yng_childrn)&
                                  !is.na(f7_slp_rooms)&!is.na(f7_house_elec)&!is.na(f7_house_bike)&!is.na(f7_house_phone)&
                                  !is.na(f7_house_tele)&!is.na(f7_house_car)&!is.na(f7_house_cart)&!is.na(f7_house_scoot)&
                                  !is.na(f7_house_fridge)&!is.na(f7_house_agland)&!is.na(f7_house_radio)&!is.na(f7_house_boat)&
                                  !is.na(f7_house_none)&!is.na(f7_ani_goat)&!is.na(f7_ani_no)&!is.na(f7_store_water)&
                                  !is.na(f7_trt_water)&!is.na(f7_temp)&!is.na(f7_bipedal)&!is.na(f7_under_nutr)&
                                  !is.na(f7_skin_flaky)&!is.na(f5_ms_water)&!is.na(f5_treat_water)&
                                  !is.na(f5_feces_visible)&!is.na(f7_disp_feces)&!is.na(f7_wash_use)&!is.na(f7_muac)&
                                  !is.na(f5_exp_rectal)&!is.na(f5_exp_convul)&!is.na(f5_exp_arthritis)&
                                  !is.na(f5_child_feces)&!is.na(f7_share_fac))
#goes from 11889 to 11849 observations

names <- c("site",
           "f7_relation","f7_dad_live",
           "f7_prim_schl","f7_ppl_house","f7_yng_childrn",
           "f7_slp_rooms","f7_floor","f7_house_elec",
           "f7_house_bike","f7_house_phone","f7_house_tele",    
           "f7_house_car","f7_house_cart","f7_house_scoot",    
           "f7_house_fridge","f7_house_agland","f7_house_radio",   
           "f7_house_boat","f7_house_none","f7_fuel_elec",   
           "f7_fuel_biogas","f7_fuel_grass","f7_fuel_propane",   
           "f7_fuel_coal","f7_fuel_dung","f7_fuel_natgas", 
           "f7_fuel_charcoal","f7_fuel_crop","f7_fuel_kero",   
           "f7_fuel_wood","f7_fuel_other","f7_ani_goat",     
           "f7_ani_sheep","f7_ani_dog","f7_ani_cat",      
           "f7_ani_cow","f7_ani_rodents","f7_ani_fowl",       
           "f7_ani_other","f7_ani_no","f7_water_house",    
           "f7_water_covwell","f7_water_yard","f7_water_covpwell", 
           "f7_water_pubtap","f7_water_prospring","f7_water_well",
           "f7_water_unspring","f7_water_pubwell","f7_water_river",    
           "f7_water_pond","f7_water_deepwell","f7_water_rain",   
           "f7_water_shallwell","f7_water_bought","f7_water_othr",    
           "f7_water_bore","f7_ms_water","f7_water_avail",   
           "f7_store_water","f7_trt_water","f7_trt_method",   
           "f7_disp_feces",
           "f7_fac_waste","f7_share_fac","f7_wash_eat",    
           "f7_wash_cook","f7_wash_nurse","f7_wash_def",      
           "f7_wash_animal","f7_wash_child","f7_wash_othr",      
           "f7_wash_use","f7_breastfed",   
           "f7_seekcare",
           #"f7_height",
           #"f7_muac",
           "f7_haz",  
           "f7_temp","f7_resp",   
           "f7_bipedal","f7_abn_hair","f7_under_nutr",     
           "f7_skin_flaky",
           #"f9_memory_aid","wealth_index",
           "base_age")
           #these all at 60d post enroll, therefore not useful at enrollment:
           # 'f5_exp_drh', 'f5_exp_dys', 'f5_exp_cou', 'f5_exp_fever', 
           # 'f5_diag_typ', 'f5_diag_mal', 'f5_diag_pne',
           # 'f5_exp_rectal', 'f5_exp_convul', 'f5_exp_arthritis',
           # 'f5_rectal', 'f5_bipedal', 'f5_abn_hair', 'f5_under_nutr', 'f5_skin_flaky',
           # 'f5_ms_water', 'f5_main_cont', 'f5_treat_water', 'f5_trt_meth', 
           # 'f5_wash_where',
           # 'f5_child_feces', 'f5_feces_visible', 'f5_house_feces'

################### controls growth falter ####
control <- CPR.funct(data=controls,outcome="haz_0.5",iter=100,nvars_opts=c(1:10,15,20,30,40,50))
control[["df_imps"]]
control[["AUC_df"]]
control[["calib"]]

# names     var_red
# 1            base_age 158.3537919
# 2              f7_haz 121.8220899
# 3             f7_resp  80.6945709
# 4             f7_temp  68.7851104
# 5        f7_ppl_house  61.3933800
# 6        f7_breastfed  41.3638452
# 7        f7_slp_rooms  41.0217526
# 8      f7_yng_childrn  34.2225455
# 9        f7_prim_schl  31.9029722
# 10       f7_share_fac  31.7198246
# 11        f7_seekcare  31.3489705
# 12      f7_disp_feces  18.3067833
# 13               site  17.5685799
# 14       f7_fac_waste  17.0306000
# 15        f7_dad_live  15.8772405
# 16        f7_wash_use  15.5951344
# 17           f7_floor  15.3194986
# 18        f7_ms_water  15.2893148
# 19     f7_water_avail  14.9800303
# 20      f7_wash_child  14.2186948
# 21      f7_trt_method  14.1939337
# 22      f7_wash_nurse  13.1095371
# 23       f7_wash_cook  12.4349516
# 24        f7_wash_def  12.2479585
# 25      f7_house_bike  12.1123025
# 26        f7_ani_fowl  11.9908577
# 27      f7_house_tele  11.8318633
# 28     f7_house_phone  11.4378712
# 29        f7_wash_eat  11.0489753
# 30     f7_ani_rodents  10.8132885
# 31         f7_ani_dog  10.5543439
# 32     f7_store_water  10.4039317
# 33     f7_house_radio  10.3062957
# 34        f7_ani_goat  10.1390135
# 35    f7_house_agland   9.8630846
# 36         f7_ani_cat   9.7312887
# 37      f7_house_elec   9.3217240
# 38     f7_house_scoot   9.3023695
# 39       f7_fuel_wood   8.8495953
# 40    f7_water_pubtap   8.7964085
# 41         f7_ani_cow   8.6205684
# 42    f7_house_fridge   8.4836246
# 43  f7_water_deepwell   8.2241051
# 44       f7_trt_water   8.1725366
# 45     f7_wash_animal   8.0391716
# 46       f7_wash_othr   7.7170003
# 47   f7_fuel_charcoal   7.1132046
# 48       f7_ani_sheep   7.0114847
# 49      f7_water_yard   6.0549501
# 50          f7_ani_no   5.5769121
# 51       f7_fuel_kero   5.5638041
# 52      f7_house_cart   5.2962219
# 53    f7_water_bought   5.2636171
# 54       f7_ani_other   5.2566270
# 55       f7_house_car   5.1701974
# 56 f7_water_shallwell   5.0567895
# 57     f7_water_house   4.8249896
# 58      f7_water_well   4.6571547
# 59    f7_fuel_propane   4.6411272
# 60        f7_relation   4.6378829
# 61   f7_water_pubwell   4.5311524
# 62       f7_fuel_crop   4.3610020
# 63       f7_fuel_dung   4.1769142
# 64      f7_water_bore   4.1704672
# 65      f7_under_nutr   4.1646506
# 66     f7_fuel_natgas   4.1571231
# 67      f7_fuel_grass   4.0175205
# 68      f7_water_rain   3.4526555
# 69  f7_water_covpwell   3.4102350
# 70       f7_fuel_coal   3.3919509
# 71   f7_water_covwell   2.7661067
# 72 f7_water_prospring   2.4727767
# 73     f7_water_river   2.2596058
# 74      f7_water_othr   2.2468025
# 75      f7_house_none   2.1045967
# 76      f7_water_pond   1.8082917
# 77        f7_abn_hair   1.4501499
# 78      f7_fuel_other   1.4356621
# 79      f7_skin_flaky   1.1533249
# 80      f7_house_boat   0.8496132
# 81       f7_fuel_elec   0.7472145
# 82  f7_water_unspring   0.7381070
# 83         f7_bipedal   0.6805382
# 84     f7_fuel_biogas   0.5198811

# AUC          SE     lower     upper level Model nvar
# 1  0.7624776 0.001165527 0.7601932 0.7647620  0.95    LR    1
# 2  0.7803823 0.001120172 0.7781868 0.7825778  0.95    LR    2
# 3  0.7808922 0.001120434 0.7786962 0.7830882  0.95    LR    3
# 4  0.7823023 0.001115755 0.7801154 0.7844891  0.95    LR    4
# 5  0.7852275 0.001108578 0.7830547 0.7874003  0.95    LR    5
# 6  0.7852461 0.001109426 0.7830717 0.7874206  0.95    LR    6
# 7  0.7852127 0.001109741 0.7830376 0.7873877  0.95    LR    7
# 8  0.7851301 0.001109857 0.7829549 0.7873054  0.95    LR    8
# 9  0.7862382 0.001109338 0.7840639 0.7884124  0.95    LR    9
# 10 0.7874418 0.001109001 0.7852682 0.7896154  0.95    LR   10
# 11 0.7946085 0.001101475 0.7924496 0.7967673  0.95    LR   15
# 12 0.7930901 0.001111725 0.7909112 0.7952691  0.95    LR   20
# 13 0.7930195 0.001115292 0.7908336 0.7952055  0.95    LR   30
# 14 0.7937860 0.001112944 0.7916047 0.7959673  0.95    LR   40
# 15 0.7937777 0.001113959 0.7915944 0.7959611  0.95    LR   50
# 16 0.7590985 0.001184341 0.7567773 0.7614198  0.95    RF    1
# 17 0.7523940 0.001187332 0.7500669 0.7547212  0.95    RF    2
# 18 0.7606396 0.001183919 0.7583192 0.7629600  0.95    RF    3
# 19 0.7594930 0.001187609 0.7571653 0.7618207  0.95    RF    4
# 20 0.7712183 0.001160818 0.7689432 0.7734935  0.95    RF    5
# 21 0.7724299 0.001158249 0.7701598 0.7747001  0.95    RF    6
# 22 0.7751419 0.001154052 0.7728800 0.7774038  0.95    RF    7
# 23 0.7757156 0.001151806 0.7734581 0.7779731  0.95    RF    8
# 24 0.7773207 0.001147942 0.7750708 0.7795707  0.95    RF    9
# 25 0.7814371 0.001138919 0.7792049 0.7836694  0.95    RF   10
# 26 0.7938329 0.001123891 0.7916301 0.7960357  0.95    RF   15
# 27 0.7971071 0.001118474 0.7949149 0.7992992  0.95    RF   20
# 28 0.8002248 0.001110771 0.7980477 0.8024018  0.95    RF   30
# 29 0.8020687 0.001105843 0.7999013 0.8042361  0.95    RF   40
# 30 0.8025294 0.001104203 0.8003652 0.8046936  0.95    RF   50

# nvar    intc intc_LCI intc_UCI slope slope_LCI slope_UCI
# <dbl>   <dbl>    <dbl>    <dbl> <dbl>     <dbl>     <dbl>
#   1     1 0.00966   -0.112    0.129 1.00      0.857      1.15
# 2     2 0.0102    -0.113    0.131 1.00      0.867      1.14
# 3     3 0.00997   -0.114    0.131 1.00      0.866      1.14
# 4     4 0.0103    -0.113    0.131 0.999     0.866      1.14
# 5     5 0.00986   -0.114    0.131 0.999     0.867      1.14
# 6     6 0.00985   -0.114    0.131 0.997     0.865      1.14
# 7     7 0.00970   -0.114    0.131 0.996     0.865      1.13
# 8     8 0.00977   -0.114    0.131 0.994     0.863      1.13
# 9     9 0.0102    -0.114    0.132 0.989     0.860      1.13
# 10    10 0.00958   -0.115    0.132 0.986     0.858      1.12
# 11    15 0.00864   -0.117    0.132 0.964     0.842      1.09
# 12    20 0.00963   -0.116    0.133 0.930     0.811      1.05
# 13    30 0.00936   -0.117    0.133 0.917     0.800      1.04
# 14    40 0.00936   -0.117    0.133 0.908     0.792      1.03
# 15    50 0.00925   -0.117    0.133 0.900     0.786      1.02

temp <- control[["decilesCC"]][c("1","2","3","4","5","6","7","8","9","10","15","20","30","40","50")]
names(temp) <- c("1-var","2-var","3-var","4-var","5-var","6-var","7-var","8-var","9-var","10-var","15-var","20-var","30-var","40-var","50-var")  #renaming
#jpeg("/GF_CC_GEMS059_controls.jpg",width=600,height=480,quality=400)
plot(x=seq(0,1,by=0.1),y=seq(0,1,by=0.1),type="l",
     xlab="Predicted Probability",ylab="Observed Proportion",
     main=expression(paste("Calibration Curve:","">=0.5,Delta,"HAZ in controls 0-59mo in GEMS")),
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5)
points(temp$`5-var`$`mean(pred_glm)`,temp$`5-var`$`mean(true)`,col="red",pch=1,cex=2,lwd=2)
points(temp$`10-var`$`mean(pred_glm)`,temp$`10-var`$`mean(true)`,col="blue",pch=2,cex=2,lwd=2)
legend("topleft",col=c("red","blue"),c("5-variable","10-variable"),pch=c(1,2),cex=1.5)
dev.off()

AUC_df <- control[["AUC_df"]]
#save(AUC_df, file = "/AUC_df_GEMS059_controls.Rdata")
#load(file = "/AUC_df_GEMS059_controls.Rdata")

#jpeg(/GF_AUCs_GEMS059_controls.jpg",width=600,height=480,quality=400)
#png("/GF_AUCs_GEMS059_controls.png",units="px",width=3000,height=2400,res=300)
par(mar=c(5,5,4,2))
plot(AUC_df$nvar[1:length(control[["nvars_opts"]])[1]],AUC_df$AUC[1:length(control[["nvars_opts"]])[1]],
     xlab="number of variables",ylab="AUC",
     main=expression(paste("">=0.5,Delta,"HAZ in controls 0-59mo in GEMS")),
     ylim=c(0.6,0.9),
     pch=1,col="red",cex=2,lwd=2,cex.axis=1.5,cex.lab=1.5,cex.main=1.5)
points(AUC_df$nvar[1:length(control[["nvars_opts"]])[1]],AUC_df$AUC[(length(control[["nvars_opts"]])[1]+1):dim(AUC_df)[1]],
       pch=2,col="blue",cex=2,lwd=2)
legend("topleft",c("logistic regression","random forest regression"),col=c("red","blue"),pch=c(1,2),cex=1.5)
dev.off()

####################
#Publication - MAL-ED for rederive growth faltering prediction
################### define study pop - one obs per diarrhea episode that has b/f and after HAZ and meets criteria ####
# ontologies <- fread("/ISASimple_Gates_MAL-ED_0-60m_RSRC_ontologyMetadata.txt")
# #write.csv(ontologies,"/ontologies.csv")
# maled_orig <- fread("/ISASimple_Gates_MAL-ED_0-60m_RSRC.txt")

# maled_orig_renamed <- rename(maled_orig, c("id"="Participant_Id"))
# maled_orig_renamed <- rename(maled_orig_renamed, c("hh_id"="Household_Id"))
# maled_orig_renamed <- rename(maled_orig_renamed, c("obs_date"="Observation date [EUPATH_0004991]"))
# maled_orig_renamed <- rename(maled_orig_renamed, c("diarrhea"="Study-defined diarrhea [EUPATH_0000665]"))
# maled_orig_renamed <- rename(maled_orig_renamed, c("first_diar_day"="1st day of diarrheal episode [EUPATH_0010473]"))
# maled_orig_renamed <- rename(maled_orig_renamed, c("diar_epi_ct"="Cumulative diarrheal episode count [EUPATH_0010478]")) #how many episodes have ever had, just need fir or last obs per episode
# maled_orig_renamed <- rename(maled_orig_renamed, c("length"="Recumbent length (cm) [EUPATH_0011918]"))
# maled_orig_renamed <- rename(maled_orig_renamed, c("HAZ_orig"="Length- or height-for-age z-score [EUPATH_0015710]"))

# temp <- maled_orig_renamed %>% select(id, hh_id, obs_date, diarrhea, first_diar_day, diar_epi_ct, length, HAZ_orig)
# study_pop <- temp
# save(study_pop, file = "/study_pop.Rdata")
load(file = "/study_pop.Rdata")
temp <- study_pop

temp$obs_date_character <- temp$obs_date
temp$obs_date <- as.Date(temp$obs_date, "%d-%m-%Y") #25-04-2012. run out of memory if do this to the full dataset
#summary(as.factor(temp$obs_date))
#drop observations without a date (is <1% of all)
temp <- temp %>% drop_na(obs_date) #1853368/1870909*100 = 99.1%
#subset to only observations that are first obs of a diarrhea episode, or have HAZ or both
temp <- temp %>% filter((diarrhea=="Yes" & first_diar_day=="Yes")| !is.na(HAZ_orig))

#create list of id's and HAZs
HAZ_data <- temp %>% select(id,length,HAZ_orig,obs_date,obs_date_character) %>% filter(!is.na(HAZ_orig))
HAZ_data <- unique(HAZ_data) #dplyr distinct
#single observation for first day of each diarrhea episode, the date of that first day of diarrhea
diar_data <- temp %>% select(id,hh_id,diarrhea,obs_date,obs_date_character,first_diar_day,diar_epi_ct) %>% filter(first_diar_day=="Yes")
diar_data <-unique(diar_data)
length(unique(diar_data$id)) #1716/11906

# HAZ was assessed monthly up to and including 36mo, then every 3 mo until 5yr.
# diarrhea episodes separated by 2 diarrhea-free days
# only want HAZ measurement from BEFORE diarrhea, up to and including 31 days ago,
#   (this will exclude lots of 3-5year olds if diarrhea too far from last HAZ measurement)
#   want to further exclude to only first obs of diarrhea if multi episodes w/in month of HAZ1 measurement
# then want the HAZ measurement that's closest to 75 days after start of diarrhea,
#   but has to be between (and including) 49 and 91 days (so like GEMS)
#   additional diarrhea episodes after index episode are fine (e.g. if another episode at day 30 is fine, include it)
temp2 <- diar_data %>% rename(diar_date=obs_date,diar_date_character=obs_date_character) %>%
  left_join(HAZ_data,by="id") %>% rename(HAZ_date=obs_date,HAZ_date_character=obs_date_character) %>%
  mutate(diff1=diar_date-HAZ_date,target_date=diar_date+75,diff2=HAZ_date-target_date,diff3=HAZ_date-diar_date) %>% #timeline: haz diar haz. diff1 is first to second, diff3 is second to third, diff2 is target to haz
  group_by(id,diar_date) %>%
  mutate(closest0=diff1==min(diff1[which(diff1>=0)]),closest75=(abs(diff2))==min(abs(diff2)))  %>%
  #mutate(row=row_number(),closest0=diff1==min(diff1[which(diff1>=0)]),closest75=(abs(diff2))==min(abs(diff2)))  %>%
  filter((closest0==T & diff1<=31) | (closest75==T & diff3>=49 & diff3<=91)) %>%
    #HAZ1 must be before diar, but no more than 31 days beforehand
    #HAZ2 is closest to 75 days from start of diar, but has to be w/in 49 and 91 days inclusive
  rename(HAZ=HAZ_orig) %>%
  select(id,hh_id,diar_date,diar_epi_ct,length,HAZ,HAZ_date,target_date,diff1,diff2,diff3,closest0,closest75) %>%
  arrange(id,diar_date,HAZ_date) %>%
  mutate(obs=1:n()) %>% #numbering each observation now that ordered
# table(temp2$obs)
# # 1    2    3 
# # 8062 7604  118 
# #examine all the relevant observations for each diarrhea episode that has more than two associated HAZ measurements
# test <- temp2 %>% filter(any(obs==3)) #118*3=354
# test2 <- test %>% filter(closest75==T)
# table(test2$diff3)
# #>>> all has to do with two equidistance around HAZ2, so just taking the second one for all
  filter(row_number()==1 | row_number()==n()) %>% #keep first and last obs for each diarrhea episode. 
  #means if there are two HAZ2 equidistance around 75days post diarrhea start, will keep the later one
  mutate(obs=1:n()) %>% #numbering on if is first/second observation for that diarrhea episode
  ungroup 
length(unique(temp2$id)) #1441/15666
#this is what want
# #closest0 production leads to Inf: p_007c8df165091d3b
# diar_test <- diar_data %>% filter(id=="p_007c8df165091d3b")
# HAZ_test <- HAZ_data %>% filter(id=="p_007c8df165091d3b")
# combo_test <- temp1 %>% filter(id=="p_007c8df165091d3b")
# diar_test <- diar_data %>% filter(id=="p_00933f3d08aef1ce")
# HAZ_test <- HAZ_data %>% filter(id=="p_00933f3d08aef1ce")
# combo_test <- temp1 %>% filter(id=="p_00933f3d08aef1ce")
# #>>> no HAZ measurements for these kids, so will end up dropping anyway
temp3 <- temp2 %>% pivot_wider(names_from=obs,values_from=c(length,HAZ,HAZ_date,diff1,diff2,diff3,closest0,closest75)) %>% #creating wide data
  filter(!is.na(HAZ_1)&!is.na(HAZ_2)) %>% #dropping obs where no f-up HAZ
  mutate(difHAZ=HAZ_2-HAZ_1) %>%
  #only want the first case of diarrhea for each HAZ_1 per kid
  group_by(id,HAZ_date_1) %>%
  mutate(ct=1:n()) %>% #ct for each time the same HAZ1 used by the same child for different diarrhea episodes
  filter(row_number()==1) %>% #only want the first use of each HAZ1 for each child
  ungroup() %>% select(id,hh_id,diar_date,diar_epi_ct,length_1,length_2,HAZ_1,HAZ_2,HAZ_date_1,HAZ_date_2,difHAZ) #only keep variables need moving forward
length(unique(temp3$id)) #1390/6617
#1 obs for each diar episode. diar_date is the first day of diarrhea for each episode. 

# #manual spot-check
# diar_test <- diar_data %>% filter(id=="p_041c6ba92b55e570")
# HAZ_test <- HAZ_data %>% filter(id=="p_041c6ba92b55e570")
# combo_test <- data %>% filter(id=="p_041c6ba92b55e570")
# 
# diar_test <- diar_data %>% filter(id=="p_00c80941c87d98ab")
# HAZ_test <- HAZ_data %>% filter(id=="p_00c80941c87d98ab")
# combo_test <- data %>% filter(id=="p_00c80941c87d98ab")
# 
# diar_test <- diar_data %>% filter(id=="p_019c47c245dc13cb")
# HAZ_test <- HAZ_data %>% filter(id=="p_019c47c245dc13cb")
# combo_test <- data %>% filter(id=="p_019c47c245dc13cb")

################### observation-level predictors ####
maled_orig_obs <- fread("/ISASimple_Gates_MAL-ED_0-60m_RSRC_observations.txt")
#subset to only those observations with id's want

#take last obs per diarrhea episode of each of these
obs_per_episode_last <- maled_orig_obs %>% rename(id="Participant_Id",
                                            obs_date="Observation date [EUPATH_0004991]",
                                            diarrhea="Study-defined diarrhea [EUPATH_0000665]",
                                            first_diar_day="1st day of diarrheal episode [EUPATH_0010473]", #Yes/No
                                            diar_dur="Diarrheal episode duration (days) [EUPATH_0010238]", #the final total num days of diarrhea in this episode, just take first or last obs of this per episode
                                            diar_epi_ct="Cumulative diarrheal episode count [EUPATH_0010478]", #how many episodes have ever had, just need fir or last obs per episode
                                            diar_days_sum="Cumulative days within diarrheal episodes [EUPATH_0010480]", #running total num of days of diar across all episodes >>> take last obs from each episode
                                            diar_dur_cat="Diarrhea duration categorization [EUPATH_0010495]", #category of how long diarhea lasted in this episode. just need first or last obs per episode
                                            diar_GEMS_MSD="Moderate-to-severe diarrhea by GEMS criteria [EUPATH_0010524]", #just need first or last obs per episode
                                            loose_stool_max="Max loose stools at episode [EUPATH_0010240]", #is per episode, so just take first or last obs per episode
                                            blood="Blood in stool at episode [EUPATH_0010487]",
                                            Vesi_Clark_score="Combined Vesikari and Clark severity at episode [EUPATH_0010500]", #is same for all days in episode, just take first or last obs per episode. may still not want to consider since is a combo variable
                                            vomit_dur="Days with vomiting at episode [EUPATH_0010486]", #same for entire episode, just take the first of last obs per episode
                                            app_dec_dur="Days with decreased appetite at episode [EUPATH_0010489]", #num days of dec app in each episode. just take first or last obs per episode
                                            dehyd_max_cat="Max dehydration categorization [EUPATH_0010498]", #seems to be the same for all days in episode, therefore just take first or last obs per episode test1 <- maled_obs_symp %>% group_by(id,diar_epi_ct) %>% filter(diarrhea=="Yes") %>% filter(any(!is.na(dehyd_max_cat)))
                                            fev_max="Fever at episode, max temp (C) [EUPATH_0010492]", #the max temp measured per diar episode. just need first or last obs per diarrhea episode
                                            fev_bin="Fever at episode categorization [EUPATH_0010499]", #none, mother report, measured confirmed. just need first or last obs per diarrhea
                                            ALRI_dur="Cumulative days in this ALRI episode [EUPATH_0010518]", #how long this ALRI episode lasted, is per obs. can be bookended with field worker defined at start and stop, this count continues increasing in-between see id==p_010e6df71b4eafe0. use this at the last day of diarrhea episode as indicator of if and severity of ALRI overlapping w/ diarrhea
) %>%
select(id,obs_date,diarrhea,first_diar_day,diar_dur,diar_epi_ct,
       diar_days_sum,diar_dur_cat,diar_GEMS_MSD,loose_stool_max,
       blood,Vesi_Clark_score,vomit_dur,app_dec_dur,dehyd_max_cat,
       fev_max,fev_bin,ALRI_dur)
obs_per_episode_last <- obs_per_episode_last[obs_per_episode_last$id %in% temp3$id,]
obs_per_episode_last$obs_date_character <- obs_per_episode_last$obs_date
obs_per_episode_last$obs_date <- as.Date(obs_per_episode_last$obs_date, "%d-%m-%Y") #25-04-2012
obs_per_episode_last <- obs_per_episode_last %>% arrange(id,obs_date) %>% group_by(id,diar_epi_ct) %>% 
  mutate(diar_start_date=case_when(first_diar_day=="Yes" ~ obs_date)) %>%
  # table(obs_per_episode_last$first_diar_day,is.na(obs_per_episode_last$diar_start_date))
  # FALSE    TRUE
  # No        0 1287453
  # Yes    8007       0
  fill(diar_start_date) %>% #this fills diar_start_date down rows until next new entry
  # table(obs_per_episode_last$first_diar_day)
  # No     Yes 
  # 1287453    8007 
  filter(diarrhea=="Yes") %>%
  filter(row_number()==n()) %>% #n=8007
  select(-obs_date,-diarrhea,-first_diar_day,-obs_date_character) %>%
  ungroup()
#each row is now a single episode of diarrhea, each child can have multiple episodes.
#diar_start_date is the first day of diarrhea in that episode


#take the first obs per diar episode of each of these
obs_per_episode_first <- maled_orig_obs %>% rename(id="Participant_Id",
                                             obs_date="Observation date [EUPATH_0004991]",
                                             age="Age (days) [EUPATH_0000579]",
                                             diarrhea="Study-defined diarrhea [EUPATH_0000665]",
                                             first_diar_day="1st day of diarrheal episode [EUPATH_0010473]", #Yes/No
                                             diar_dur="Diarrheal episode duration (days) [EUPATH_0010238]", #the final total num days of diarrhea in this episode, just take first or last obs of this per episode
                                             diar_epi_ct="Cumulative diarrheal episode count [EUPATH_0010478]", #how many episodes have ever had, just need fir or last obs per episode
                                             diar_days_since_temp="Days since last diarrheal episode [EUPATH_0010482]", #take the first obs of this from each episode
                                             breast_excl="Cumulative days exclusively breastfed [EUPATH_0011014]",
                                             breast_not="Cumulative days not breastfed [EUPATH_0011015]",
                                             breast_part="Cumulative days partially breastfed [EUPATH_0011017]",
                                             breast_predom="Cumulative days predominantly breastfed [EUPATH_0011019]",
                                             #>>>breastfeeding variables all seem to be per obs, mutually exclusive, running counts across all episodes. so each observation, one of the four variables increase by 1
) %>%
  select(id,obs_date,age,diarrhea,first_diar_day,diar_dur,diar_epi_ct,
         diar_days_since_temp,breast_excl,breast_not,breast_part,breast_predom)
obs_per_episode_first <- obs_per_episode_first[obs_per_episode_first$id %in% temp3$id,]
obs_per_episode_first$obs_date_character <- obs_per_episode_first$obs_date
obs_per_episode_first$obs_date <- as.Date(obs_per_episode_first$obs_date, "%d-%m-%Y") #25-04-2012
obs_per_episode_first <- obs_per_episode_first %>% arrange(id,obs_date) %>% group_by(id,diar_epi_ct) %>% 
  mutate(diar_start_date=case_when(first_diar_day=="Yes" ~ obs_date)) %>%
  # table(obs_per_episode_first$first_diar_day,is.na(obs_per_episode_first$diar_start_date))
  # FALSE    TRUE
  # No        0 1287453
  # Yes    8007       0
  fill(diar_start_date) %>% #this fills diar_start_date down rows until next new entry
  # table(obs_per_episode_first$first_diar_day)
  # No     Yes 
  # 1287453    8007 
  ungroup() %>% #need to ungroup so can do lag for days since last diarrhea, otherwise first obs of each episode blank since no lag obs in that group
#  mutate(diar_days_since2=case_when(first_diar_day=="Yes" ~ lag(diar_days_since))) %>%
  mutate(diar_days_since=lag(diar_days_since_temp),
         diar_days_since=(case_when(
           is.na(diar_days_since) ~ 0,
           TRUE ~ as.numeric(diar_days_since)))) %>% #if missing means was first obs of diarrhea for that kid, set to 0 days since
  group_by(id,diar_epi_ct) %>% 
  filter(diarrhea=="Yes") %>%
  filter(row_number()==1) %>% #n=8007
  select(-obs_date,-diarrhea,-first_diar_day,-obs_date_character,-diar_days_since_temp) %>%
  ungroup()
#each row is now a single episode of diarrhea, each child can have multiple episodes.
#diar_start_date is the first day of diarrhea in that episode


#need if any during diar episode 
obs_per_episode_any <- maled_orig_obs %>% rename(id="Participant_Id",
                                                   obs_date="Observation date [EUPATH_0004991]",
                                                   diarrhea="Study-defined diarrhea [EUPATH_0000665]",
                                                   first_diar_day="1st day of diarrheal episode [EUPATH_0010473]", #Yes/No
                                                   diar_dur="Diarrheal episode duration (days) [EUPATH_0010238]", #the final total num days of diarrhea in this episode, just take first or last obs of this per episode
                                                   diar_epi_ct="Cumulative diarrheal episode count [EUPATH_0010478]", #how many episodes have ever had, just need fir or last obs per episode
                                                   indraw="Indrawing, fieldworker assessment [EUPATH_0010540]", #is per obs. seems to be based on field worker assessment. just have if ever during diarrhea episode
                                                   activity="Activity level, caregiver report [EUPATH_0010526]", #seems to be per obs. do if any during diar episode
                                                   abx_caregiverYN="Use of antibiotics, caregiver report [EUPATH_0010530]", #seems to be per obs. use if any Y during diar episode
                                                   ORT_caregiver="ORT administered, caregiver report [EUPATH_0010536]", #seems to be per obs. use if any during diar episode
                                                   hosp="Hospitalized [EUPATH_0010995]" #per obs if was hospitalized that day. want ever per diar episode, also want number of days per episode
                                                   
) %>%
  select(id,obs_date,diarrhea,first_diar_day,diar_dur,diar_epi_ct,
         indraw,activity,abx_caregiverYN,ORT_caregiver,hosp)
obs_per_episode_any <- obs_per_episode_any[obs_per_episode_any$id %in% temp3$id,]
obs_per_episode_any$obs_date_character <- obs_per_episode_any$obs_date
obs_per_episode_any$obs_date <- as.Date(obs_per_episode_any$obs_date, "%d-%m-%Y") #25-04-2012
obs_per_episode_any <- obs_per_episode_any %>% arrange(id,obs_date) %>% group_by(id,diar_epi_ct) %>%
  mutate(diar_start_date=case_when(first_diar_day=="Yes" ~ obs_date)) %>%
  # table(obs_per_episode_any$first_diar_day,is.na(obs_per_episode_any$diar_start_date))
  # FALSE    TRUE
  # No        0 1287453
  # Yes    8007       0
  fill(diar_start_date) %>% #this fills diar_start_date down rows until next new entry
  # table(obs_per_episode_any$first_diar_day)
  # No     Yes 
  # 1287453    8007 
  mutate(indraw_any=case_when((diarrhea=="Yes" & indraw=="Yes") ~ "Yes"),
         sleepy_any=case_when((diarrhea=="Yes" & activity=="Sleepy") ~ "Yes"),
         unawake_any=case_when((diarrhea=="Yes" & activity=="Difficult to awaken") ~ "Yes"),
         abx_any=case_when((diarrhea=="Yes" & abx_caregiverYN=="Yes") ~ "Yes"),
         ORT_any=case_when((diarrhea=="Yes" & ORT_caregiver=="Yes") ~ "Yes"),
         hosp_any=case_when((diarrhea=="Yes" & hosp=="Yes") ~ "Yes")) %>%
  # id==p_344d8fd5e2527884 (2010-09-12), p_4ed14776c9d156b3 (2013-02-13) for checks for indraw
  # id==p_00541d8f6796dd38 (2011-03-28) for checks for activity sleepy
  # id==p_1ac7ab69e73110a0 (2010-10-19) for checks for activity unawake
  # id==p_00541d8f6796dd38 (2011-03-27) for checks for abx_caregiverYN
  # id==p_00541d8f6796dd38 (2011-05-26) for checks for ORT_caregiver
  # id==p_00541d8f6796dd38 (2011-06-04) for checks for hosp
  fill(indraw_any,sleepy_any,unawake_any,abx_any,ORT_any,hosp_any) %>% #fills until end of group, not up. therefore take last obs per group
  # test <- obs_per_episode_any %>% filter(id=="p_344d8fd5e2527884" | id=="p_4ed14776c9d156b3" |
  #                                          id=="p_00541d8f6796dd38" | id=="p_1ac7ab69e73110a0")
  filter(diarrhea=="Yes") %>%
  filter(row_number()==n()) %>% #n=8007
  select(-obs_date,-diarrhea,-first_diar_day,-obs_date_character) %>%
  ungroup()
#each row is now a single episode of diarrhea, each child can have multiple episodes.
#diar_start_date is the first day of diarrhea in that episode


#these only assessed once a month, rest missing. subset to only the non-missing diet observations, then take join closest diet before diarrhea
obs_diet_ct <- maled_orig_obs %>% rename(id="Participant_Id",
                                       obs_date="Observation date [EUPATH_0004991]",
                                       diarrhea="Study-defined diarrhea [EUPATH_0000665]",
                                       first_diar_day="1st day of diarrheal episode [EUPATH_0010473]", #Yes/No
                                       #monthly 24-hr food recall up to 24mo, optional extension up to 60mo
                                       meal_ct="Meal count during recall [EUPATH_0011067]",
                                       dairy_ct="Dairy count [EUPATH_0011074]",
                                       veg_ct="Dark green leafy vegetable count [EUPATH_0011075]",
                                       egg_ct="Egg count [EUPATH_0011076]",
                                       fish_ct="Fish or shellfish count [EUPATH_0011077]",
                                       legume_ct="Legume count [EUPATH_0011078]",
                                       meat_ct="Meat count [EUPATH_0011079]",
                                       fruit_ct="Other fruit or vegetable count [EUPATH_0011080]",
                                       organ_ct="Organ meat count [EUPATH_0011081]",
                                       sweet_ct="Sweets count [EUPATH_0011082]",
                                       #qualitative diet assessed monthly until 8mo of age. lots missing, not assessed same obs as 24-hr recall so doesn't make sense
                                       diet_score_05="Diet diversity score for children 0 to 5 months [EUPATH_0011625]",
                                       diet_score_68="Diet diversity score for children 6 to 8 months [EUPATH_0011626]"

) %>%
  select(id,obs_date,diarrhea,first_diar_day,
         meal_ct,dairy_ct,veg_ct,egg_ct,fish_ct,legume_ct,
         meat_ct,fruit_ct,organ_ct,sweet_ct)
obs_diet_ct <- obs_diet_ct[obs_diet_ct$id %in% temp3$id,]
obs_diet_ct$obs_date_character <- obs_diet_ct$obs_date
obs_diet_ct$obs_date <- as.Date(obs_diet_ct$obs_date, "%d-%m-%Y") #25-04-2012
obs_diet_ct <- obs_diet_ct %>% filter(!is.na(dairy_ct))
#each row is an observation of what have. merge based on date and keep if close enough
# summary(obs_diet_ct)
# # id               obs_date            diarrhea         first_diar_day        meal_ct           dairy_ct      
# # Length:1316746     Min.   :2009-10-13   Length:1316746     Length:1316746     Min.   : 0.0      Min.   : 0.0     
# # Class :character   1st Qu.:2011-09-17   Class :character   Class :character   1st Qu.: 4.0      1st Qu.: 0.0     
# # Mode  :character   Median :2012-06-06   Mode  :character   Mode  :character   Median : 6.0      Median : 1.0     
# # Mean   :2012-06-15                                         Mean   : 5.8      Mean   : 1.6     
# # 3rd Qu.:2013-02-26                                         3rd Qu.: 7.0      3rd Qu.: 3.0     
# # Max.   :2017-04-14                                         Max.   :20.0      Max.   :15.0     
# # NA's   :1289289   NA's   :1286727  
# # veg_ct            egg_ct           fish_ct          legume_ct          meat_ct           fruit_ct      
# # Min.   :0.0       Min.   :0.0       Min.   :0.0       Min.   :0.0       Min.   :0.0       Min.   : 0.0     
# # 1st Qu.:0.0       1st Qu.:0.0       1st Qu.:0.0       1st Qu.:0.0       1st Qu.:0.0       1st Qu.: 1.0     
# # Median :0.0       Median :0.0       Median :0.0       Median :1.0       Median :0.0       Median : 2.0     
# # Mean   :0.3       Mean   :0.3       Mean   :0.2       Mean   :1.2       Mean   :0.6       Mean   : 2.3     
# # 3rd Qu.:0.0       3rd Qu.:1.0       3rd Qu.:0.0       3rd Qu.:2.0       3rd Qu.:1.0       3rd Qu.: 3.0     
# # Max.   :6.0       Max.   :6.0       Max.   :6.0       Max.   :9.0       Max.   :6.0       Max.   :14.0     
# # NA's   :1286727   NA's   :1286727   NA's   :1286727   NA's   :1286727   NA's   :1286727   NA's   :1286727  
# # organ_ct          sweet_ct       diet_score_05     diet_score_68     obs_date_character
# # Min.   :0.0       Min.   :0.0       Min.   :0.0       Min.   :1         Length:1316746    
# # 1st Qu.:0.0       1st Qu.:0.0       1st Qu.:3.0       1st Qu.:4         Class :character  
# # Median :0.0       Median :0.0       Median :4.0       Median :4         Mode  :character  
# # Mean   :0.1       Mean   :0.7       Mean   :3.4       Mean   :4                           
# # 3rd Qu.:0.0       3rd Qu.:1.0       3rd Qu.:4.0       3rd Qu.:4                           
# # Max.   :6.0       Max.   :8.0       Max.   :4.0       Max.   :6                           
# # NA's   :1286727   NA's   :1286727   NA's   :1309879   NA's   :1312678  
# table(is.na(obs_diet_ct$dairy_ct)&is.na(obs_diet_ct$veg_ct))
# # FALSE    TRUE 
# # 30019 1286727
#>>> food count variables mostly missing for the same observations, just drop based on missing for one of them

obs_diet_score <- maled_orig_obs %>% rename(id="Participant_Id",
                                         obs_date="Observation date [EUPATH_0004991]",
                                         diarrhea="Study-defined diarrhea [EUPATH_0000665]",
                                         first_diar_day="1st day of diarrheal episode [EUPATH_0010473]", #Yes/No
                                         #qualitative diet assessed monthly until 8mo of age. lots missing, not assessed same obs as 24-hr recall so doesn't make sense
                                         diet_score_05="Diet diversity score for children 0 to 5 months [EUPATH_0011625]",
                                         diet_score_68="Diet diversity score for children 6 to 8 months [EUPATH_0011626]"
                                         
) %>%
  select(id,obs_date,diarrhea,first_diar_day,
         diet_score_05,diet_score_68)
obs_diet_score <- obs_diet_score[obs_diet_score$id %in% temp3$id,]
obs_diet_score$obs_date_character <- obs_diet_score$obs_date
obs_diet_score$obs_date <- as.Date(obs_diet_score$obs_date, "%d-%m-%Y") #25-04-2012
obs_diet_score <- obs_diet_score %>% filter(!is.na(diet_score_05) | !is.na(diet_score_68))
#each row is an observation of what have. merge based on date and keep if close enough


obs_resp <- maled_orig_obs %>% rename(id="Participant_Id",
                                      obs_date="Observation date [EUPATH_0004991]",
                                      diarrhea="Study-defined diarrhea [EUPATH_0000665]",
                                      first_diar_day="1st day of diarrheal episode [EUPATH_0010473]", #Yes/No
                                      diar_dur="Diarrheal episode duration (days) [EUPATH_0010238]", #the final total num days of diarrhea in this episode, just take first or last obs of this per episode
                                      diar_epi_ct="Cumulative diarrheal episode count [EUPATH_0010478]", #how many episodes have ever had, just need fir or last obs per episode
                                      #GEMS: fever of at least 38C or parental perception
                                      resp1="Respiratory rate (breaths/min) [CMO_0000289]", #is per obs
                                      resp2="Respiratory rate (breaths/min), 2nd [EUPATH_0010097]", #is per obs
                                      
) %>%
  select(id,obs_date,diarrhea,first_diar_day,diar_dur,diar_epi_ct,
         resp1,resp2) 
#subset to only those observations with id's want
obs_resp <- obs_resp[obs_resp$id %in% temp3$id,]
obs_resp$obs_date_character <- obs_resp$obs_date
obs_resp$obs_date <- as.Date(obs_resp$obs_date, "%d-%m-%Y") #25-04-2012
#cleaning symptoms so can symplify
# table(is.na(obs_resp$resp1),is.na(obs_resp$resp2))
# # FALSE    TRUE
# # FALSE   42846       4
# # TRUE        0 1273896
obs_resp$resp <- ifelse((is.na(obs_resp$resp1) & !is.na(obs_resp$resp2)),obs_resp$resp2,
                        ifelse((!is.na(obs_resp$resp1) & is.na(obs_resp$resp2)),obs_resp$resp1,
                               ifelse((!is.na(obs_resp$resp1) & !is.na(obs_resp$resp2)),((obs_resp$resp1 + obs_resp$resp2)/2),
                                      NA)))
# table(is.na(obs_resp$resp))
# # FALSE    TRUE 
# # 42850 1273896 
obs_resp <- obs_resp %>% filter(!is.na(resp) & diarrhea=="Yes") %>% select(-obs_date_character,-resp1,-resp2) %>%
    # #are there multiple resp observations per diarrhea episode?
    # test2 <- obs_resp %>% group_by(id,diar_epi_ct) %>% mutate(ct=1:n())
    # table(test2$ct)
    # # 1    2    3    4    5 
    # # 1613  375   73   14    4 
    # test3 <- test2 %>% filter(any(ct>1))
  group_by(id,diar_epi_ct) %>% mutate(avg_resp=mean(resp)) %>% #want an average resp rate per diarrhea episode
  filter(row_number()==n()) %>% #only need 1 obs per diar episode
  ungroup()
#each row is a diar episode, resp is the average resp rate during that illness


#merging observation-level observations that have been wrangled back into full analytic dataset
#merge these by diarrhea episode
temp4 <- temp3 %>% left_join(obs_per_episode_last,by=c("id","diar_epi_ct"))
# table(temp3$diar_date==temp3$diar_start_date)
# # TRUE 
# # 6617
# test2 <- test %>% group_by(id,diar_date) %>% mutate(ct=1:n()) #also grouped by (id,diar_epi_ct)
# table(test2$ct)
# 1 
# 6617 
temp5 <- temp4 %>% left_join(obs_per_episode_first,by=c("id","diar_epi_ct")) %>%
  select(-diar_dur.y,-diar_start_date.y) %>% rename(diar_dur="diar_dur.x", diar_start_date="diar_start_date.x")
# table(temp3$diar_date==temp3$diar_start_date.y)
# # TRUE 
# # 6617 
# test2 <- test %>% group_by(id,diar_date) %>% mutate(ct=1:n()) #also grouped by (id,diar_epi_ct)
# table(test2$ct)
# 1 
# 6617 
# table(test$diar_dur.x==test$diar_dur.y)
# # TRUE 
# # 6617 
# table(test$diar_start_date.x==test$diar_start_date.y)
# # TRUE 
# # 6617 
temp6 <- temp5 %>% left_join(obs_per_episode_any,by=c("id","diar_epi_ct")) %>%
  select(-diar_dur.y,-diar_start_date.y) %>% rename(diar_dur="diar_dur.x", diar_start_date="diar_start_date.x")
# test2 <- test %>% group_by(id,diar_date) %>% mutate(ct=1:n()) #also grouped by (id,diar_epi_ct)
# table(test2$ct)
# 1 
# 6617 
# table(test$diar_dur.x==test$diar_dur.y)
# # TRUE 
# # 6617 
# table(test$diar_start_date.x==test$diar_start_date.y)
# # TRUE 
# # 6617 
temp7 <- temp6 %>% left_join(obs_resp,by=c("id","diar_epi_ct")) %>%
  select(-diar_dur.y,-obs_date) %>% rename(diar_dur="diar_dur.x")
# test2 <- test %>% group_by(id,diar_date) %>% mutate(ct=1:n()) #also grouped by (id,diar_epi_ct)
# table(test2$ct)
# # 1
# # 6617


#merge these by closest date within some threshold
temp8 <- temp7 %>% left_join(obs_diet_ct,by="id") %>%
  mutate(obs_diff_diet_ct=abs(diar_date-obs_date)) %>%
  group_by(id,diar_date) %>%
  mutate(row=row_number(),closest0=(obs_diff_diet_ct==min(obs_diff_diet_ct))) %>%
  filter(closest0==T | is.na(closest0)) %>% #want to keep observations for which diet is closest or no diet info for that id. all remaining duplicates are when multiple diet obs equidistance from start of diarrhea, want the earlier diet obs 
    # test2 <- test %>% group_by(id,diar_date) %>% mutate(ct=1:n()) #also grouped by (id,diar_epi_ct)
    # table(test2$ct)
    # # 1    2 
    # # 6617   63 
    # test3 <- test2 %>% filter(any(ct>1))
    # table(test3$closest0)
    # # TRUE 
    # # 126
  arrange(id,diar_date,obs_date) %>%
  group_by(id,diar_epi_ct) %>%
  mutate(ct=1:n()) %>% #ct for each time the same diar_epi_ct used by the same child for different diet observations
    # test2 <- test %>% group_by(id,diar_date) %>% mutate(ct=1:n()) #also grouped by (id,diar_epi_ct)
    # table(test2$ct)
    # # 1    2 
    # # 6617   63
    # test3 <- test2 %>% filter(any(ct>1))
    # table(test3$closest0)
    # # TRUE 
    # # 126
  filter(row_number()==1) %>% #only want the first use of each diet obs for each diarrhea episode
  ungroup %>% rename(obs_date_diet_ct="obs_date") %>% 
  select(-obs_date_character,-row,-closest0,-ct) #drop variables don't need/don't make sense anymore
# test2 <- test %>% group_by(id,diar_date) %>% mutate(ct=1:n()) #also grouped by (id,diar_epi_ct)
# table(test2$ct)


temp9 <- temp8 %>% left_join(obs_diet_score,by="id") %>%
  mutate(obs_diff_diet_score=abs(diar_date-obs_date)) %>%
  group_by(id,diar_date) %>%
  mutate(row=row_number(),closest0=(obs_diff_diet_score==min(obs_diff_diet_score))) %>%
  filter(closest0==T | is.na(closest0)) %>% #want to keep observations for which diet is closest or no diet info for that id. all remaining duplicates are when multiple diet obs equidistance from start of diarrhea, want the earlier diet obs 
    # test2 <- test %>% group_by(id,diar_date) %>% mutate(ct=1:n()) #also grouped by (id,diar_epi_ct)
    # table(test2$ct)
    # # 1    2
    # # 6617   24
    # test3 <- test2 %>% filter(any(ct>1))
    # table(test3$closest0)
    # # TRUE
    # # 48
  arrange(id,diar_date,obs_date) %>%
  group_by(id,diar_epi_ct) %>%
  mutate(ct=1:n()) %>% #ct for each time the same diar_epi_ct used by the same child for different diet observations
    # test2 <- test %>% group_by(id,diar_date) %>% mutate(ct=1:n()) #also grouped by (id,diar_epi_ct)
    # table(test2$ct)
    # # 1    2
    # # 6617   24
    # test3 <- test2 %>% filter(any(ct>1))
    # table(test3$closest0)
    # # TRUE
    # # 48
  filter(row_number()==1) %>% #only want the first use of each diet obs for each diarrhea episode
  ungroup %>% rename(obs_date_diet_score="obs_date") %>% 
  select(-first_diar_day.x,-first_diar_day.y,-row,-closest0,-ct,-obs_date_character) #drop variables that don't mean anything anymore
    # test2 <- test %>% group_by(id,diar_date) %>% mutate(ct=1:n()) #also grouped by (id,diar_epi_ct)
    # table(test2$ct)
    # # 1 
    # # 6617 


################### person-level predictors ####
#participant variables want as predictors, start cleaning those and will need to merge to this data
maled_orig_ppl <- fread("/ISASimple_Gates_MAL-ED_0-60m_RSRC_participants.txt")

maled_ppl <- maled_orig_ppl %>% rename(id="Participant_Id",
                                       breast_24="Breastfed within 1st 24hrs [EUPATH_0011009]",
                                       time_to_breast="Time between childbirth and 1st breastfeeding [EUPATH_0011010]",
                                       colostrum="Fed colostrum [EUPATH_0011011]",
                                       prelacteal="Prelacteal feeding [EUPATH_0011012]",
                                       sex="Sex [PATO_0000047]",
                                       tot_diar="Total days in all diarrheal episodes [EUPATH_0000742]") %>%
  select(id,breast_24,time_to_breast,colostrum,prelacteal,sex,tot_diar) 
#(no observation date since these are time-insensitive descriptors of participant)
#subset to only those observations with id's want
maled_ppl <- maled_ppl[maled_ppl$id %in% temp3$id,] #2145 to 1390 which is what want. this step probably superfluous w/ left-join
#dim(unique(maled_ppl)) #no repeates
temp10 <- temp9 %>% left_join(maled_ppl,by="id")
length(unique(temp10$id)) #1390/6617


################### HH-level predictors ####
#HH variables want as potential predictors, start cleaning those and will need to merge to this data
maled_orig_hh <- fread("/ISASimple_Gates_MAL-ED_0-60m_RSRC_households.txt")

maled_orig_hh_renamed <- maled_orig_hh %>% rename(hh_id="Household_Id",
                                                  hh_date="Household data collection date [EUPATH_0021085]",
                                                  country="Country [ENVO_00000004]",
                                                  water_source="Drinking water source [ENVO_00003064]",
                                                  wealth_index="Household wealth index, numerical [EUPATH_0000014]",
                                                  ppl_slp="Persons sleeping in dwelling [EUPATH_0000714]",
                                                  mean_ppl="Mean people per room [EUPATH_0011604]",
                                                  sani_score="Sanitation score [EUPATH_0011607]",
                                                  water_score="Drinking water score [EUPATH_0011603]",
                                                  bed="Bed [ENVO_00000501]",
                                                  tv="Television [ENVO_01000579]",
                                                  fridge="Refrigerator [ENVO_01000583]",
                                                  table="Table [ENVO_01000584]",
                                                  chair="Chair [ENVO_01000586]",
                                                  roof="Roof material [EUPATH_0000003]",
                                                  floor="Floor material [EUPATH_0000006]",
                                                  wall="Wall material [EUPATH_0000009]",
                                                  bank="Bank account [EUPATH_0000167]",
                                                  food_24wo="Went 24hrs without food last month [EUPATH_0011135]",
                                                  food_not_want="Ate foods not wanted last month [EUPATH_0011136]",
                                                  food_not_able="Not able to eat preferred foods last month [EUPATH_0011137]",
                                                  food_fewer="Ate fewer meals due to lack of food last month [EUPATH_0011138]",
                                                  sleep_hungry="Went to sleep hungry last month [EUPATH_0011139]",
                                                  food_variety="Had to eat a limited variety of foods last month [EUPATH_0011141]",
                                                  food_none="No food of any kind last month [EUPATH_0011142]",
                                                  food_worried="Worried that there was not enough food last month [EUPATH_0011143]",
                                                  food_small="Ate smaller meal than needed last month [EUPATH_0011144]",
                                                  kitchen="Separate kitchen [EUPATH_0011586]",
                                                  edu="Maternal education (years) [EUPATH_0011605]",
                                                  edu2="Maternal education [EUPATH_0011590]",
                                                  edu_score="Maternal education score [EUPATH_0011591]",
                                                  rm_ct="Room count [EUPATH_0011593]",
                                                  income_score="Income score [EUPATH_0011601]",
                                                  sani_water_score="Sanitation and drinking water score [EUPATH_0011617]",
                                                  two_ppl_rm="Fewer than 2 people per room [EUPATH_0011609]",
                                                  sani="Toilet or latrine [EUPATH_0011611]",
                                                  sani_concrete="Toilet or latrine has concrete floor [EUPATH_0011612]",
                                                  sani_type="Toilet or latrine type [EUPATH_0011744]",
                                                  latrine_current="Latrine type, current [EUPATH_0011701]",
                                                  latrine_previous="Latrine type, previous [EUPATH_0011702]"
)
maled_orig_hh_renamed$hh_date_character <- maled_orig_hh_renamed$hh_date
maled_orig_hh_renamed$hh_date <- as.Date(maled_orig_hh_renamed$hh_date, "%d-%m-%Y") #25-04-2012
maled_hh <- maled_orig_hh_renamed %>% select(hh_id,hh_date_character,hh_date,country,water_source,
                                             wealth_index,ppl_slp,mean_ppl,sani_score,water_score,
                                             bed,tv,fridge,table,chair,roof,floor,wall,bank,food_24wo,
                                             food_not_want,food_not_able,food_fewer,sleep_hungry,
                                             food_variety,food_none,food_worried,food_small,kitchen,
                                             edu,edu2,edu_score,rm_ct,income_score,sani_water_score,
                                             two_ppl_rm,sani,sani_concrete,sani_type,latrine_current,
                                             latrine_previous)
#are multiple observations per household.
# length(unique(maled_hh$hh_id))
# [1] 2145
#observations with country recorded don't have any other info recorded
maled_hh_country <- maled_hh %>% select(hh_id,country) %>% filter(!is.na(country)) 
#rest of the other observations, no longer need country
#if water_source missing, everything but food insecurity Q's are also missing, so drop those observations
maled_hh_repeated <- maled_hh %>% select(-country) %>% filter(!is.na(hh_date)&!(is.na(water_source)))
# test<-maled_hh_repeated %>% filter(is.na(water_source))
# str(test)

#for hh variables that repeated, selected closest date
temp11 <- temp10 %>% 
  left_join(maled_hh_repeated,by="hh_id") %>%
  mutate(hh_diff=abs(diar_date-hh_date)) %>%
  group_by(id,diar_date) %>%
  mutate(row=row_number(),closest=(hh_diff==min(hh_diff)))  %>% #1390 uniq at this step length(unique(temp3$id)); this row labels each observation as T/F is it the row with the smallest difference in dates
  filter(closest==T | is.na(closest)) %>% ##1390 uniq at this step length(unique(temp3$id)). are some diar episodes for which are two equidistance HH observations from either side
    # test1 <- temp11 %>% mutate(ct=1:n()) %>% filter(any(ct>1)) %>% select(id,hh_id,diar_date,diar_epi_ct,ct)
    # table(test1$ct)
    # # 1  2 
    # # 15 15
  arrange(id,diar_date,hh_date) %>%
  group_by(id,diar_epi_ct) %>%
  mutate(ct=1:n()) %>% #ct for each time the same diar_epi_ct used by the same child for different HH observations
  # test1 <- temp11 %>% group_by(id,diar_date) %>% mutate(ct=1:n()) #also grouped by (id,diar_epi_ct)
  # table(test1$ct)
  # # 1    2
  # # 6617   15
  # test2 <- test1 %>% filter(any(ct>1))
  # table(test2$closest)
  # # TRUE
  # # 30
  filter(row_number()==1) %>% #only want the first use of each HH obs for each diarrhea episode
  ungroup %>% select(-diarrhea.x,-diarrhea.y,-row,-closest,-ct)
length(unique(temp11$id)) #1390
# test <- temp11 %>% group_by(id,diar_date) %>% mutate(ct=1:n()) #also grouped by (id,diar_epi_ct)
# table(test$ct)

#add country
temp12 <- temp11 %>% left_join(maled_hh_country,by="hh_id")
length(unique(temp12$id)) #1390/6617

unclean_analytic <- temp12
#save(unclean_analytic, file = "/unclean_analytic.Rdata")


################### START here for analysis: analytic file, define variables ####
load(file = "/unclean_analytic.Rdata")
temp1 <- unclean_analytic
#this is an observation for each case of diarrhea. kids with multiple distinct episodes of diarrhea are in here multiple times

#create 0.5HAZ outcome variable
temp2 <- temp1 %>% mutate(HAZ_dif=HAZ_1 - HAZ_2, #in GEMS: haz_dif = f4b_haz - f5_haz
  haz_0.5=(case_when(HAZ_dif>=0.5 ~ 1, TRUE~0)),
  haz_1.0=(case_when(HAZ_dif>=1.0 ~ 1, TRUE~0)))
#cbind(test$HAZ_1,test$HAZ_2,test$HAZ_dif,test$haz_0.5)

temp3<-temp2
#convert these to factors
vars <- c("diar_GEMS_MSD","blood","country","breast_24",
          "colostrum","prelacteal","sex","roof","floor",
          "wall","kitchen","edu2","two_ppl_rm"
          )
temp3[vars] <- lapply(temp3[vars], factor)

#going from categories to ordinal (linear numeric)
  #remember case_when default if not satisfied is NA
temp4 <- temp3 %>% mutate(diar_dur_cat=(case_when(
                    diar_dur_cat=="0 points - 0-1 day" ~ 0,
                    diar_dur_cat=="1 point - 2-4 days" ~ 1,
                    diar_dur_cat=="2 points - 5-7 days" ~ 2,
                    diar_dur_cat=="3 points - 8+ days" ~ 3)),
                  dehyd_max_cat=(case_when(
                    dehyd_max_cat=="0 points - No dehydration" ~ 0,
                    dehyd_max_cat=="2 points - Some dehydration" ~ 1,
                    dehyd_max_cat=="3 points - Severe dehydration" ~ 2)),
                  fev_bin=(case_when(
                    fev_bin=="0 points - No fever reported" ~ 0,
                    fev_bin=="1 point - Mother reported fever" ~ 1,
                    fev_bin=="2 points - Fever >37.5C confirmed by field worker" ~ 2)),
                  ALRI_dur=(case_when(
                    is.na(ALRI_dur) ~ 0,
                    TRUE ~ as.numeric(ALRI_dur))),
                  time_to_breast=(case_when(
                    time_to_breast=="1. Within 1 hour" ~ 0,
                    time_to_breast=="2. 1 hour to 24 hours" ~ 1,
                    time_to_breast=="3. 1 day to 3 days" ~ 2,
                    time_to_breast=="4. 4+ days" ~ 3)),
                  water_source=(case_when(
                    water_source=="Unimproved: surface water (river, dam, lake, pond, stream, canal, or irrigation canal)" ~ 0,
                    water_source=="Unimproved: unprotected well (dug well that is unprotected from runoff water and/or unprotected from bird droppings and animals)" ~ 0,
                    #is.na(water_source)~99,
                    is.na(water_source)~as.numeric(NA),
                    TRUE ~ 1)),
                  sani_score=(case_when(
                    sani_score=="0 points - Unimproved" ~ 0,
                    sani_score=="4 points - Improved" ~ 1,
                    is.na(sani_score)~as.numeric(NA))),
                  water_score=(case_when(
                    water_score=="0 points - Unimproved" ~ 0,
                    water_score=="4 points - Improved" ~ 1,
                    is.na(water_score)~as.numeric(NA))),
                  food_24wo=(case_when(
                    food_24wo=="0 points - No" ~ 0,
                    food_24wo=="1 point - Rarely" ~ 1,
                    food_24wo=="2 points - Sometimes" ~ 2,
                    food_24wo=="3 points - Often" ~ 3,
                    is.na(food_24wo)~as.numeric(NA))),
                  food_not_want=(case_when(
                    food_not_want=="0 points - No" ~ 0,
                    food_not_want=="1 point - Rarely" ~ 1,
                    food_not_want=="2 points - Sometimes" ~ 2,
                    food_not_want=="3 points - Often" ~ 3,
                    is.na(food_not_want)~as.numeric(NA))),
                  food_not_able=(case_when(
                    food_not_able=="0 points - No" ~ 0,
                    food_not_able=="1 point - Rarely" ~ 1,
                    food_not_able=="2 points - Sometimes" ~ 2,
                    food_not_able=="3 points - Often" ~ 3,
                    is.na(food_not_able)~as.numeric(NA))),
                  food_fewer=(case_when(
                    food_fewer=="0 points - No" ~ 0,
                    food_fewer=="1 point - Rarely" ~ 1,
                    food_fewer=="2 points - Sometimes" ~ 2,
                    food_fewer=="3 points - Often" ~ 3,
                    is.na(food_fewer)~as.numeric(NA))),
                  sleep_hungry=(case_when(
                    sleep_hungry=="0 points - No" ~ 0,
                    sleep_hungry=="1 point - Rarely" ~ 1,
                    sleep_hungry=="2 points - Sometimes" ~ 2,
                    sleep_hungry=="3 points - Often" ~ 3,
                    is.na(sleep_hungry)~as.numeric(NA))),
                  food_variety=(case_when(
                    food_variety=="0 points - No" ~ 0,
                    food_variety=="1 point - Rarely" ~ 1,
                    food_variety=="2 points - Sometimes" ~ 2,
                    food_variety=="3 points - Often" ~ 3,
                    is.na(food_variety)~as.numeric(NA))),
                  food_none=(case_when(
                    food_none=="0 points - No" ~ 0,
                    food_none=="1 point - Rarely" ~ 1,
                    food_none=="2 points - Sometimes" ~ 2,
                    food_none=="3 points - Often" ~ 3,
                    is.na(food_none)~as.numeric(NA))),
                  food_worried=(case_when(
                    food_worried=="0 points - No" ~ 0,
                    food_worried=="1 point - Rarely" ~ 1,
                    food_worried=="2 points - Sometimes" ~ 2,
                    food_worried=="3 points - Often" ~ 3,
                    is.na(food_worried)~as.numeric(NA))),
                  food_small=(case_when(
                    food_small=="0 points - No" ~ 0,
                    food_small=="1 point - Rarely" ~ 1,
                    food_small=="2 points - Sometimes" ~ 2,
                    food_small=="3 points - Often" ~ 3,
                    is.na(food_small)~as.numeric(NA))),
                  sani=(case_when(
                    sani=="No facility" ~ 0,
                    sani=="Pit latrine or flush toilet" ~ 1,
                    is.na(sani)~as.numeric(NA))),
                  sani_type=(case_when(
                    sani_type=="Unimproved: no facility, bush, field, or bucket toilet" ~ 0,
                    is.na(sani_type)~as.numeric(NA),
                    TRUE~1)))
              
#set missing to no, becomes numeric 0/1
temp5 <- temp4 %>% mutate(across(c(indraw_any,sleepy_any,unawake_any,
                                 abx_any,ORT_any,hosp_any,
                                 bed,tv,fridge,table,chair,
                                 bank,sani_concrete),
                               ~ case_when(is.na(.) ~ 0,
                                           .=="No" ~ 0,
                                           .=="Yes" ~ 1)))
# test <- data %>% mutate(indraw_any_test=(case_when(
#                     is.na(indraw_any) ~ 0,
#                     indraw_any=="Yes" ~ 1)))

#diet_score_05, diet_score_68 - only eligible for one of these based on age, combine into a single var based on age at that diarrhea episode
temp6 <- temp5 %>% mutate(diet_score=(case_when(
                      is.na(diet_score_05) & is.na(diet_score_68) ~ as.numeric(NA),
                      !is.na(diet_score_05) & is.na(diet_score_68) ~ as.numeric(diet_score_05),
                      is.na(diet_score_05) & !is.na(diet_score_68) ~ as.numeric(diet_score_68))))
# table(is.na(data$diet_score_05),is.na(data$diet_score_68))
# # FALSE TRUE
# # FALSE     0 1240
# # TRUE   5377    0

#create a single combined breastfeeding variable
#these four variables are number of days since brith that exclusive/predominant/partial/no breastfeeding.
#assume more is always better, assign points for each
temp6.2 <- temp6 %>% mutate(breast_totdays=(breast_excl*1)+(breast_predom*0.75)+(breast_part*0.5)+(breast_not*0))
#cbind(temp6.2$test,temp6.2$breast_excl,temp6.2$breast_predom,temp6.2$breast_part,temp6.2$breast_not)


################### inclusion/exclusion for growth faltering ####
#look at dates for each merge, makes sure things are within an acceptable date range
#if diet info was collected >=90 days from start of diarrhea, then set diet info to missing (no diet info available from close enough date)
temp7 <- temp6.2 %>% mutate(across(c(meal_ct,dairy_ct,veg_ct,
                                   egg_ct,fish_ct,legume_ct,
                                   meat_ct,fruit_ct,organ_ct,sweet_ct),
                                 ~ifelse(as.numeric(obs_diff_diet_ct)>=90,NA,.)))
# temp7$obs_diff_diet_ct[1:15]
# # Time differences in days
# # [1] 224 147  91 213 176 127 117  91  30  12   1  10   1   7  11
# temp6$meal_ct[1:15]
# # [1] 5 5 5 5 5 5 5 5 5 5 4 5 6 4 4
# temp7$meal_ct[1:15]
# #[1] NA NA  5 NA NA NA NA  5  5  5  4  5  6  4  4

temp8 <- temp7 %>% mutate(across(c(diet_score),
                                 ~ifelse(as.numeric(obs_diff_diet_score)>=90,NA,.)))
# temp8$obs_diff_diet_score[1:15]
# # Time differences in days
# # [1]   7  10   7   2   8   4   6   2   2  44  91 111 153 206 224
# temp7$diet_score[1:15]
# # [1] 4 3 5 2 2 3 3 4 4 4 4 4 4 4 4
# temp8$diet_score[1:15]
# #[1] 4  3  5  2  2  3  3  4  4  4 NA NA NA NA NA

#if HH info more than 6 months old then missing
temp9 <- temp8 %>% mutate(across(c(water_source,wealth_index,
                                   ppl_slp,mean_ppl,sani_score,
                                   water_score,bed,tv,fridge,
                                   table,chair,roof,floor,wall,
                                   bank,food_24wo,food_not_want,
                                   food_not_able,food_fewer,
                                   sleep_hungry,food_variety,
                                   food_none,food_worried,food_small,
                                   kitchen,edu,edu2,edu_score,
                                   rm_ct,income_score,sani_water_score,
                                   two_ppl_rm,sani,sani_concrete,
                                   sani_type,latrine_current,
                                   latrine_previous),
                                 ~ifelse(as.numeric(hh_diff)>=183,NA,.)))
# temp9$hh_diff[20:30]
# # Time differences in days
# # [1]  47  81 405 168 112  68  15  73  45   0  13
# temp8$water_source[20:30]
# # [1] 1 1 0 1 1 1 1 1 1 1 1
# temp9$water_source[20:30]
# # [1]  1  1 NA  1  1  1  1  1  1  1  1

#keep based on Brader_2019 HAZ plausability
# Children presenting with prolonged (> 7 days' duration) and 
# persistent (> 14days' duration) diarrhea were excluded
# We also excluded children with implausible length/LAZ values
# (LAZ > 6 or < - 6 and change in (delta) LAZ>3;
# a length gain of > 8 cm for follow-up periods 49-60 days and
# > 10 cm for periods 61-91 days among infants <= 6 months,
# a length gain of > 4 cm for follow-up periods 49-60 days and
# > 6 cm for periods 61-91 days among children > 6 months, or
# length values that were > 1.5 cm lower at follow-up than at enrollment.
# >>>>> MAL-ED has huge number of follow-ups at just slightly longer than 91 days so pushing out a bit for larger sample size table(as.numeric(temp10$fup_days))
temp10 <- temp9 %>% mutate(change_ht = length_2 - length_1,
                           fup_days = HAZ_date_2 - HAZ_date_1)
# cases_orig=cases_orig %>% mutate(change_ht = f5_height - f4b_height)
# cases_orig$fup_days <- as.numeric(cases_orig$f5_date_date - cases_orig$f4b_date_date)
# cases_orig=cases_orig %>% mutate(haz_dif = f4b_haz - f5_haz) 
# temp2 <- temp1 %>% mutate(HAZ_dif=HAZ_1 - HAZ_2, #in GEMS: haz_dif = f4b_haz - f5_haz

temp10$keep <- ifelse((temp10$age<=183 & temp10$fup_days>=49 & temp10$fup_days<=60 & temp10$change_ht<=8),1,
                ifelse((temp10$age<=183 & temp10$fup_days>=61 & temp10$fup_days<=95 & temp10$change_ht<=10),1,
                ifelse((temp10$age>183 & temp10$fup_days>=49 & temp10$fup_days<=60 & temp10$change_ht<=4),1,
                ifelse((temp10$age>183 & temp10$fup_days>=61 & temp10$fup_days<=95 & temp10$change_ht<=6),1,
                0))))
#n=6617
table(temp10$keep)
# 0    1 
# 209 6340 
table(temp10$fup_days)
length(unique(temp10$id))

#temp10 n=6617
temp11 <- temp10 %>% filter((diar_dur<=7) & #n=6051
         (!is.na(HAZ_dif)) & #n=6051
         (fup_days>=49 & fup_days<=95) & #n=5926
         (HAZ_1>=-6 & HAZ_1<=6) & #n=5926
         (abs(HAZ_dif)<=3.0) & #n=5925
         (keep==1) & #n=5793
         (change_ht>=-1.5)) #n=5788 
length(unique(temp11$id))
#[1] 1350

################### drop missing since can't have missing in RF, define "names" variables interested in ####
complete <- temp11 %>% filter(!is.na(Vesi_Clark_score)&!is.na(fev_bin)&!is.na(time_to_breast)&!is.na(water_source)&
                                           !is.na(wealth_index)&!is.na(sani_score)&!is.na(wall)&!is.na(kitchen)&
                                           !is.na(edu)
)
#5788 to 5683 observations
length(unique(complete$id))
# [1] 1322
complete$index=1:dim(complete)[1] #SMA creating an ID for each observation

# select variables we're interested in. have checked all appropriate ones are factor
names <- c("diar_epi_ct","HAZ_1","diar_dur",
            "diar_days_sum","diar_dur_cat","loose_stool_max",
            "blood",
           #"Vesi_Clark_score",
            "vomit_dur","app_dec_dur",
            "dehyd_max_cat","fev_bin","ALRI_dur",
            "age",
           #"breast_excl","breast_not","breast_part","breast_predom",
            "breast_totdays",
            "diar_days_since",
            "ORT_caregiver",
            "indraw_any","sleepy_any","unawake_any","abx_any",
            "ORT_any","hosp_any",
            "first_diar_day",
            "breast_24","time_to_breast",
            "colostrum","prelacteal","sex","tot_diar",
            "water_source",
           #"wealth_index",
            "ppl_slp","mean_ppl","sani_score","water_score",
            "bed","tv","fridge","table",
            "chair","roof","floor","wall",
            "bank","kitchen","edu",
            "edu2",
           #"edu_score",
            "rm_ct",
            "income_score",
           #"sani_water_score",
            "two_ppl_rm","sani_concrete",
            "sani_type","country"
              # "diet_score",
           # "meal_ct", "dairy_ct","veg_ct",
           # "egg_ct","fish_ct","legume_ct","meat_ct",
           # "fruit_ct","organ_ct","sweet_ct",
               # "food_24wo","food_not_want","food_not_able",
               # "food_fewer","sleep_hungry","food_variety","food_none",
               # "food_worried","food_small",
)

################### cases into age groups, random sample ####
# dim(complete)
# #[1] 5683  118
# length(unique(complete$id))
# #[1] 1322
#
# dim(complete[which(complete$age<365),])
# #[1] 2586  118
# length(unique(complete[which(complete$age<365),]$id))
# #[1] 1093
# dim(complete[which(complete$age>=365 & complete$age<730),])
# #[1] 2316  118
# length(unique(complete[which(complete$age>=365 & complete$age<730),]$id))
# #[1] 958
# dim(complete[which(complete$age>=730 & complete$age<1825),])
# #[1] 781 118
# length(unique(complete[which(complete$age>=730  & complete$age<1825),]$id))
# #[1] 380

#want 1 observation per ID in each age group
data <- complete %>% group_by(id) %>% sample_n(1) %>% ungroup #1322
data_age1 <- complete %>% filter(age<365) %>% #n=2597
  group_by(id) %>% sample_n(1) %>% ungroup() 
data_age2 <- complete %>% filter(age>=365 & age<730) %>% #n=2325
  group_by(id) %>% sample_n(1) %>% ungroup()
data_age3 <- complete %>% filter(age>=730 & age<1825) %>% #n=781
  group_by(id) %>% sample_n(1) %>% ungroup()
data_age4 <- complete %>% filter(age<730) %>% #n=1301
  group_by(id) %>% sample_n(1) %>% ungroup()

length(unique(data_age1$id)) #1093
length(unique(data_age2$id)) #958
length(unique(data_age3$id)) #380

################### descriptive for pub ####
table(complete$haz_0.5)
# 0    1 
# 4722  961 
961/5683

table(complete$haz_1.0)
# 0    1 
# 5522  161 
161/5683

#dates 
# participants <- fread("/ISASimple_Gates_MAL-ED_0-60m_RSRC_participants.txt")
options(max.print=1500)
participants$`Initial illness surveillance date [EUPATH_0010484]` <- as.Date(participants$`Initial illness surveillance date [EUPATH_0010484]`, "%d-%m-%Y") #25-04-2012
summary(participants$`Initial illness surveillance date [EUPATH_0010484]`)
# Min.      1st Qu.       Median         Mean      3rd Qu.         Max.         NA's 
# "2009-10-26" "2010-08-13" "2011-02-04" "2011-02-05" "2011-08-15" "2012-03-04"         "11" 



################### MAIN growth falter all cases groups RF ####
main <- CPR.funct(data=data,outcome="haz_0.5",iter=100,nvars_opts=c(1:10,15,20,30,40,50))
#save(main, file = "/main_df_MALED023.Rdata")
main[["df_imps"]]
main[["AUC_df"]]
main[["calib"]]

# names     var_red
# 1            HAZ_1 17.15548750
# 2              age 15.50661939
# 3   breast_totdays 12.79204226
# 4         tot_diar  8.76128359
# 5         mean_ppl  7.47745973
# 6    diar_days_sum  7.42376649
# 7              edu  6.67591932
# 8  diar_days_since  6.48343683
# 9          ppl_slp  5.95643232
# 10 loose_stool_max  5.86598395
# 11    income_score  5.58613098
# 12           rm_ct  5.16075226
# 13        diar_dur  4.38668112
# 14         country  4.23577203
# 15     diar_epi_ct  4.23109687
# 16     app_dec_dur  3.44942767
# 17            roof  3.03078851
# 18       vomit_dur  2.80410640
# 19           floor  2.31403043
# 20            wall  2.27049095
# 21    diar_dur_cat  2.09299489
# 22             sex  2.07871338
# 23         fev_bin  1.82574419
# 24  time_to_breast  1.80126278
# 25         kitchen  1.59794107
# 26           table  1.53412880
# 27      sleepy_any  1.44662428
# 28           chair  1.40965542
# 29         abx_any  1.38427728
# 30         ORT_any  1.35100872
# 31              tv  1.30261504
# 32            bank  1.26639115
# 33    water_source  1.24083515
# 34   ORT_caregiver  1.20181736
# 35             bed  1.19780353
# 36      sani_score  1.18110189
# 37       sani_type  1.15189870
# 38           blood  1.08401722
# 39          fridge  1.06845823
# 40     water_score  1.05415581
# 41      two_ppl_rm  0.94176620
# 42      prelacteal  0.85445677
# 43   dehyd_max_cat  0.80864804
# 44   sani_concrete  0.79981774
# 45        ALRI_dur  0.76403188
# 46            edu2  0.73767757
# 47       colostrum  0.69423916
# 48        hosp_any  0.67397402
# 49  first_diar_day  0.64334767
# 50       breast_24  0.27349967
# 51     unawake_any  0.06211842
# 52      indraw_any  0.01032156

# AUC          SE     lower     upper level Model nvar
# 1  0.6353862 0.004385074 0.6267916 0.6439808  0.95    LR    1
# 2  0.6765409 0.003905191 0.6688869 0.6841950  0.95    LR    2
# 3  0.6761881 0.003918361 0.6685082 0.6838679  0.95    LR    3
# 4  0.6753073 0.003917341 0.6676295 0.6829852  0.95    LR    4
# 5  0.6748224 0.003931324 0.6671172 0.6825277  0.95    LR    5
# 6  0.6747928 0.003941062 0.6670685 0.6825172  0.95    LR    6
# 7  0.6779168 0.003916447 0.6702407 0.6855929  0.95    LR    7
# 8  0.6817474 0.003901717 0.6741002 0.6893946  0.95    LR    8
# 9  0.6817740 0.003916293 0.6740982 0.6894498  0.95    LR    9
# 10 0.6815009 0.003940089 0.6737785 0.6892234  0.95    LR   10
# 11 0.7223710 0.003889108 0.7147485 0.7299935  0.95    LR   15
# 12 0.7221000 0.003883646 0.7144882 0.7297118  0.95    LR   20
# 13 0.7125105 0.003950359 0.7047679 0.7202531  0.95    LR   30
# 14 0.7070599 0.003997367 0.6992252 0.7148946  0.95    LR   40
# 15 0.7049490 0.004005229 0.6970989 0.7127991  0.95    LR   50
# 16 0.5008319 0.004648273 0.4917214 0.5099423  0.95    RF    1
# 17 0.6695756 0.003999626 0.6617365 0.6774147  0.95    RF    2
# 18 0.6771758 0.003902893 0.6695263 0.6848253  0.95    RF    3
# 19 0.6777841 0.003865944 0.6702070 0.6853612  0.95    RF    4
# 20 0.6750835 0.003897662 0.6674442 0.6827227  0.95    RF    5
# 21 0.6845977 0.003871610 0.6770095 0.6921859  0.95    RF    6
# 22 0.6861093 0.003872633 0.6785191 0.6936995  0.95    RF    7
# 23 0.6868113 0.003899378 0.6791686 0.6944539  0.95    RF    8
# 24 0.6844021 0.003918316 0.6767223 0.6920818  0.95    RF    9
# 25 0.6860423 0.003915757 0.6783676 0.6937171  0.95    RF   10
# 26 0.7082861 0.003789964 0.7008580 0.7157143  0.95    RF   15
# 27 0.7087931 0.003800752 0.7013437 0.7162424  0.95    RF   20
# 28 0.7080553 0.003795178 0.7006169 0.7154937  0.95    RF   30
# 29 0.7083161 0.003822820 0.7008235 0.7158087  0.95    RF   40
# 30 0.7117281 0.003808015 0.7042645 0.7191917  0.95    RF   50

# nvar    intc intc_LCI intc_UCI slope slope_LCI slope_UCI
# <dbl>   <dbl>    <dbl>    <dbl> <dbl>     <dbl>     <dbl>
#   1     1 0.00958   -0.323    0.321 1.02      0.351     1.72 
# 2     2 0.0111    -0.326    0.328 0.998     0.510     1.53 
# 3     3 0.0114    -0.326    0.329 0.991     0.506     1.53 
# 4     4 0.0114    -0.326    0.329 0.967     0.492     1.49 
# 5     5 0.0111    -0.327    0.329 0.950     0.482     1.47 
# 6     6 0.0118    -0.326    0.330 0.936     0.474     1.45 
# 7     7 0.0115    -0.327    0.330 0.923     0.471     1.42 
# 8     8 0.0116    -0.328    0.331 0.915     0.472     1.40 
# 9     9 0.0122    -0.327    0.332 0.905     0.466     1.39 
# 10    10 0.0118    -0.328    0.332 0.893     0.458     1.37 
# 11    15 0.0181    -0.329    0.346 0.876     0.515     1.27 
# 12    20 0.0188    -0.331    0.349 0.828     0.486     1.20 
# 13    30 0.0193    -0.333    0.352 0.745     0.425     1.09 
# 14    40 0.0181    -0.337    0.354 0.628     0.334     0.948
# 15    50 0.0211    -0.336    0.360 0.533     0.264     0.828

temp <- main[["decilesCC"]][c("1","2","3","4","5","6","7","8","9","10","15","20","30","40","50")]
names(temp) <- c("1-var","2-var","3-var","4-var","5-var","6-var","7-var","8-var","9-var","10-var","15-var","20-var","30-var","40-var","50-var")  #renaming
#jpeg("/GF_CC_MALED023.jpg",width=600,height=480,quality=400)
#png("/GF_CC_MALED023.png",units="px",width=3000,height=2400,res=300)
plot(x=seq(0,1,by=0.1),y=seq(0,1,by=0.1),type="l",
     xlab="Predicted Probability",ylab="Observed Proportion",
     main=expression(paste("Calibration Curve:","">=0.5,Delta,"HAZ in cases 0-23mo in MAL-ED")),
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5)
points(temp$`5-var`$`mean(pred_glm)`,temp$`5-var`$`mean(true)`,col="red",pch=1,cex=2,lwd=2)
points(temp$`10-var`$`mean(pred_glm)`,temp$`10-var`$`mean(true)`,col="blue",pch=2,cex=2,lwd=2)
legend("topleft",col=c("red","blue"),c("5-variable","10-variable"),pch=c(1,2),cex=1.5)
dev.off()

AUC_df <- main[["AUC_df"]]
#save(AUC_df, file = "/AUC_df_MALED023.Rdata")
#load(file = "/AUC_df_MALED023.Rdata")

#jpeg("/GF_AUCs_MALED023.jpg",width=600,height=480,quality=400)
#png("/GF_AUCs_MALED023.png",units="px",width=3000,height=2400,res=300)
par(mar=c(5,5,4,2))
plot(AUC_df$nvar[1:length(main[["nvars_opts"]])[1]],AUC_df$AUC[1:length(main[["nvars_opts"]])[1]],
     xlab="number of variables",ylab="AUC",
     main=expression(paste("">=0.5,Delta,"HAZ in cases 0-23mo in MAL-ED")),
     ylim=c(0.5,0.85),
     pch=1,col="red",cex=2,lwd=2,cex.axis=1.5,cex.lab=1.5,cex.main=1.5)
points(AUC_df$nvar[1:length(main[["nvars_opts"]])[1]],AUC_df$AUC[(length(main[["nvars_opts"]])[1]+1):dim(AUC_df)[1]],
       pch=2,col="blue",cex=2,lwd=2)
legend("topleft",c("logistic regression","random forest regression"),col=c("red","blue"),pch=c(1,2),cex=1.5)
dev.off()

glm_gf <- glm(haz_0.5~HAZ_1+age+breast_totdays+tot_diar+mean_ppl+
                diar_days_sum+edu+diar_days_since+ppl_slp+
                loose_stool_max,
              data=data,family="binomial",control=glm.control(maxit=50))
summary(glm_gf)
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)      0.2470802  0.3784190   0.653  0.51380    
# HAZ_1            0.2977146  0.0720466   4.132 3.59e-05 ***
#   age             -0.0026698  0.0008772  -3.044  0.00234 ** 
#   breast_totdays  -0.0001476  0.0016043  -0.092  0.92671    
# tot_diar        -0.0182925  0.0070910  -2.580  0.00989 ** 
#   mean_ppl         0.0458076  0.0520188   0.881  0.37854    
# diar_days_sum    0.0167046  0.0118128   1.414  0.15733    
# edu             -0.0470744  0.0221693  -2.123  0.03372 *  
#   diar_days_since -0.0005146  0.0009246  -0.557  0.57782    
# ppl_slp          0.0632154  0.0269691   2.344  0.01908 *  
#   loose_stool_max -0.0771935  0.0386871  -1.995  0.04601 *  

round(exp(coef(glm_gf)),2)
# (Intercept)     HAZ_1           age             breast_totdays 
# 1.28            1.35            1.00            1.00 
# tot_diar        mean_ppl        diar_days_sum   edu 
# 0.98            1.05            1.02            0.95 
# diar_days_since ppl_slp         loose_stool_max 
# 1.00            1.07            0.93 

round(exp(confint(glm_gf)),2)
# 2.5 % 97.5 %
# (Intercept)      0.61   2.70
# HAZ_1            1.17   1.55
# age              1.00   1.00
# breast_totdays   1.00   1.00
# tot_diar         0.97   1.00
# mean_ppl         0.94   1.16
# diar_days_sum    0.99   1.04
# edu              0.91   1.00
# diar_days_since  1.00   1.00
# ppl_slp          1.01   1.12
# loose_stool_max  0.86   1.00

################### main + 0-23(age_4) growth falter all cases groups RF ####
main.023 <- CPR.funct(data=data_age1,outcome="haz_0.5",iter=100,nvars_opts=c(5,10))
main.023[["df_imps"]]
main.023[["AUC_df"]]
main.023[["calib"]]

# names    var_red
# 1            HAZ_1 20.6326727
# 2              age 12.7294852
# 3   breast_totdays 12.0690382
# 4         tot_diar 10.5562264
# 5         mean_ppl  8.6923600
# 6    diar_days_sum  7.8463434
# 7          ppl_slp  7.5703386
# 8              edu  7.1289399
# 9  diar_days_since  6.7119577
# 10    income_score  6.5169071
# 11 loose_stool_max  6.4650697
# 12           rm_ct  5.6617140
# 13        diar_dur  4.8989131
# 14         country  4.5594597
# 15     diar_epi_ct  4.2036325
# 16            roof  3.3738426
# 17       vomit_dur  3.2947984
# 18     app_dec_dur  3.0185697
# 19            wall  2.6941961
# 20  time_to_breast  2.6204379
# 21           floor  2.5580734
# 22    diar_dur_cat  2.2324349
# 23         fev_bin  1.7868039
# 24             sex  1.7367707
# 25         abx_any  1.6505323
# 26      sleepy_any  1.5433360
# 27              tv  1.5212632
# 28            bank  1.5143284
# 29           chair  1.5140572
# 30         ORT_any  1.5095013
# 31           table  1.4873554
# 32             bed  1.4619446
# 33   ORT_caregiver  1.4554739
# 34         kitchen  1.4547752
# 35      two_ppl_rm  1.4442493
# 36      sani_score  1.2980108
# 37       sani_type  1.2919316
# 38          fridge  1.1142311
# 39           blood  1.0640437
# 40      prelacteal  0.9261701
# 41    water_source  0.9195793
# 42     water_score  0.8694123
# 43     unawake_any  0.8491754
# 44            edu2  0.8058054
# 45   sani_concrete  0.7920688
# 46       colostrum  0.7812904
# 47   dehyd_max_cat  0.6917662
# 48  first_diar_day  0.6073528
# 49        hosp_any  0.4712800
# 50       breast_24  0.3769985
# 51      indraw_any  0.2921180
# 52        ALRI_dur  0.2878996

# AUC          SE     lower     upper level Model nvar
# 1 0.6231256 0.004354048 0.6145918 0.6316593  0.95    LR    5
# 2 0.6322478 0.004344662 0.6237324 0.6407631  0.95    LR   10
# 3 0.5943818 0.004518869 0.5855250 0.6032386  0.95    RF    5
# 4 0.6120419 0.004428254 0.6033627 0.6207211  0.95    RF   10

# nvar     intc intc_LCI intc_UCI slope slope_LCI slope_UCI
# <dbl>    <dbl>    <dbl>    <dbl> <dbl>     <dbl>     <dbl>
#   1     5  0.00171   -0.318    0.307 0.981     0.311      1.69
# 2    10 -0.00306   -0.326    0.305 0.876     0.319      1.47


################### main + 0-11(age_1) growth falter all cases groups RF ####
main.011 <- CPR.funct(data=data_age1,outcome="haz_0.5",iter=100,nvars_opts=c(5,10))
main.011[["df_imps"]]
main.011[["AUC_df"]]
main.011[["calib"]]

# names    var_red
# 1            HAZ_1 18.5498730
# 2   breast_totdays 12.7360167
# 3              age 12.2237852
# 4         tot_diar 11.2264007
# 5         mean_ppl  9.0536302
# 6    diar_days_sum  7.8695446
# 7          ppl_slp  6.8512724
# 8  loose_stool_max  6.7402528
# 9              edu  6.7326711
# 10 diar_days_since  6.6043171
# 11    income_score  5.8240771
# 12           rm_ct  5.3535171
# 13         country  5.1524558
# 14        diar_dur  4.9002644
# 15     diar_epi_ct  4.2662959
# 16            roof  4.0849423
# 17     app_dec_dur  3.4743863
# 18       vomit_dur  3.4050298
# 19            wall  2.8445240
# 20  time_to_breast  2.5714970
# 21    diar_dur_cat  2.3742932
# 22           floor  2.2896480
# 23              tv  1.8940723
# 24            bank  1.8907087
# 25             sex  1.7805307
# 26         fev_bin  1.7677955
# 27           chair  1.6896639
# 28           table  1.6064208
# 29         abx_any  1.5979120
# 30         ORT_any  1.4827757
# 31         kitchen  1.4537968
# 32             bed  1.4048966
# 33   ORT_caregiver  1.3773763
# 34    water_source  1.3640630
# 35     water_score  1.3180330
# 36      sleepy_any  1.3159382
# 37      two_ppl_rm  1.2871423
# 38      sani_score  1.2155697
# 39           blood  1.2136642
# 40       sani_type  1.1858869
# 41          fridge  1.1549382
# 42       colostrum  1.0138133
# 43      prelacteal  0.9787242
# 44            edu2  0.7882533
# 45  first_diar_day  0.7213862
# 46   sani_concrete  0.7095852
# 47     unawake_any  0.5559937
# 48   dehyd_max_cat  0.5416113
# 49        hosp_any  0.5246189
# 50       breast_24  0.4640246
# 51        ALRI_dur  0.1452111
# 52      indraw_any  0.1259864


# AUC          SE     lower     upper level Model nvar
# 1 0.6048450 0.004445400 0.5961322 0.6135578  0.95    LR    5
# 2 0.6257824 0.004428864 0.6171020 0.6344628  0.95    LR   10
# 3 0.5807371 0.004494092 0.5719288 0.5895453  0.95    RF    5
# 4 0.6095810 0.004370582 0.6010148 0.6181472  0.95    RF   10

# nvar     intc intc_LCI intc_UCI slope slope_LCI slope_UCI
# <dbl>    <dbl>    <dbl>    <dbl> <dbl>     <dbl>     <dbl>
#   1     5  0.00105   -0.317    0.304 0.890     0.156      1.66
# 2    10 -0.00285   -0.324    0.304 0.813     0.246      1.42

################### main + 12-23(age_2) growth falter all cases groups RF ####
main.1223 <- CPR.funct(data=data_age2,outcome="haz_0.5",iter=100,nvars_opts=c(5,10))
main.1223[["df_imps"]]
main.1223[["AUC_df"]]
main.1223[["calib"]]

# names      var_red
# 1              age 8.2937880198
# 2            HAZ_1 7.8855494584
# 3   breast_totdays 7.1996865799
# 4         tot_diar 5.3444582135
# 5  diar_days_since 5.2048583417
# 6    diar_days_sum 4.9236248335
# 7         mean_ppl 4.6271590633
# 8              edu 4.0509801417
# 9          ppl_slp 3.8857319124
# 10     diar_epi_ct 3.3352324530
# 11           rm_ct 3.1744898812
# 12 loose_stool_max 2.9908127888
# 13        diar_dur 2.9382719602
# 14    income_score 2.8284109862
# 15         country 2.6058126856
# 16     app_dec_dur 2.2292128387
# 17            wall 1.5614129463
# 18            roof 1.5514350710
# 19       vomit_dur 1.3730209129
# 20    diar_dur_cat 1.3541890575
# 21           floor 1.2967057327
# 22         fev_bin 1.1073459266
# 23  time_to_breast 1.0952644220
# 24         kitchen 1.0011911163
# 25         abx_any 0.9138210079
# 26             sex 0.8307115737
# 27              tv 0.7884088513
# 28           table 0.7722142038
# 29           chair 0.7704606865
# 30      sleepy_any 0.7615448851
# 31         ORT_any 0.7227166495
# 32          fridge 0.7004043486
# 33   ORT_caregiver 0.6715892991
# 34            bank 0.6588709960
# 35      two_ppl_rm 0.6547677154
# 36             bed 0.6206634742
# 37     water_score 0.6187056228
# 38       sani_type 0.5833158323
# 39   sani_concrete 0.5484374579
# 40           blood 0.5394083593
# 41  first_diar_day 0.5391696911
# 42    water_source 0.5182375417
# 43      sani_score 0.4993685019
# 44            edu2 0.4982157077
# 45      prelacteal 0.4408430608
# 46   dehyd_max_cat 0.3546290689
# 47        ALRI_dur 0.2983967834
# 48       breast_24 0.2969230909
# 49       colostrum 0.2173548954
# 50        hosp_any 0.2081909263
# 51     unawake_any 0.0143349784
# 52      indraw_any 0.0006805556

# AUC          SE     lower     upper level Model nvar
# 1 0.6516903 0.005397801 0.6411108 0.6622698  0.95    LR    5
# 2 0.6363299 0.005596540 0.6253608 0.6472989  0.95    LR   10
# 3 0.6388091 0.005493855 0.6280414 0.6495769  0.95    RF    5
# 4 0.6326459 0.005747351 0.6213813 0.6439105  0.95    RF   10

# nvar     intc intc_LCI intc_UCI slope slope_LCI slope_UCI
# <dbl>    <dbl>    <dbl>    <dbl> <dbl>     <dbl>     <dbl>
# 1     5 -0.0235   -0.469    0.381 0.987     0.257      1.80
# 2    10 -0.0279   -0.475    0.378 0.802     0.130      1.54
