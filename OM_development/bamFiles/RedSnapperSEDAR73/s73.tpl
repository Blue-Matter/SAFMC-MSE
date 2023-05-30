//##--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>
//##
//##  SEDAR 41  Red Snapper assessment November 2015
//##  NMFS, Beaufort Lab, Sustainable Fisheries Branch
//##
//##--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>
DATA_SECTION

!!cout << "Starting Beaufort Assessment Model" << endl;
!!cout << endl;
!!cout << "                BAM!" << endl;
!!cout << endl;

//--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>
//-- BAM DATA_SECTION: set-up section
//--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>

// Starting and ending year of the model (year data starts)
init_int styr;
init_int endyr;

//Starting year to estimate recruitment deviation from S-R curve
init_int styr_rec_dev;
//Ending year to estimate recruitment deviation from S-R curve
init_int endyr_rec_dev;
//possible 3 phases of constraints on recruitment deviations
init_int endyr_rec_phase1;
init_int endyr_rec_phase2;

// ending years for selectivity blocks
init_int endyr_selex_phase1;
init_int endyr_selex_phase2;
init_int endyr_selex_phase3;

//number assessment years
number nyrs;
number nyrs_rec;

//this section MUST BE INDENTED!!!
 LOCAL_CALCS
   nyrs=endyr-styr+1.;
   nyrs_rec=endyr_rec_dev-styr_rec_dev+1.;
 END_CALCS
 
//Total number of ages in population model
init_int nages;
// Vector of ages for age bins in population model
init_vector agebins(1,nages);
 
//Total number of ages used to match age comps: plus group may differ from popn, first age must not
init_int nages_agec;
init_int nages_agec_10p;
//Vector of ages for age bins in age comps
init_vector agebins_agec(1,nages_agec);
init_vector agebins_agec_10p(1,nages_agec_10p);

//Total number of length bins for each matrix and width of bins)
init_int nlenbins;          //used to match data
init_number lenbins_width;  //width of length bins (mm)

//Vector of lengths for length bins (mm)(midpoint) 
init_vector lenbins(1,nlenbins);
 
//Max F used in spr and msy calcs
init_number max_F_spr_msy;
//Total number of iterations for spr calcs
init_int n_iter_spr;
//Total number of iterations for msy calcs
int n_iter_msy;
 LOCAL_CALCS
		n_iter_msy=n_iter_spr; 
 END_CALCS

//Starting year to compute arithmetic average recruitment for SPR-related values
init_int styr_rec_spr;
//Ending year to compute arithmetic average recruitment for SPR-related values
init_int endyr_rec_spr;
number nyrs_rec_spr;
 LOCAL_CALCS
   nyrs_rec_spr=endyr_rec_spr-styr_rec_spr+1.;
 END_CALCS 

//Switch for recruitment type in spawner per recruit function (1=observed average during styr_rec_spr to endyr_rec_spr, 2=expected average from R0)  
init_int SPR_rec_switch;
//Switch for spawner-recruit function (integer 1=Beverton-Holt, 2=Ricker, 3=None (constant average recruitment))
init_int SR_switch; 
 
//Number years at end of time series over which to average sector F's, for weighted selectivities
init_int selpar_n_yrs_wgted;
//bias correction (set to 1.0 for no bias correction or a negative value to compute from rec variance)
init_number set_BiasCor;

//Number years at end of time series over which to average sector F's, for weighted selectivities
init_int E_age_min;


//--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>
//-- BAM DATA_SECTION: observed data section
//--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>

//################Commercial handline fleet #######################################
// Comm HL CPUE
init_int styr_cH_cpue;                                             
init_int endyr_cH_cpue;                                            
init_vector obs_cH_cpue(styr_cH_cpue,endyr_cH_cpue); //Observed CPUE
init_vector cH_cpue_cv(styr_cH_cpue,endyr_cH_cpue);  //CV of cpue

// Comm HL  Landings (1000 lb whole weight)
init_int styr_cH_L;
init_int endyr_cH_L;
init_vector obs_cH_L(styr_cH_L,endyr_cH_L);
init_vector cH_L_cv(styr_cH_L,endyr_cH_L);

// Comm HL length Compositions (3 cm bins)
init_int nyr_cH_lenc;
init_ivector yrs_cH_lenc(1,nyr_cH_lenc);
init_vector nsamp_cH_lenc(1,nyr_cH_lenc);
init_vector nfish_cH_lenc(1,nyr_cH_lenc);
init_matrix obs_cH_lenc(1,nyr_cH_lenc,1,nlenbins);
 
// Comm HL age compositions
init_int nyr_cH_agec;
init_ivector yrs_cH_agec(1,nyr_cH_agec);
init_vector nsamp_cH_agec(1,nyr_cH_agec);
init_vector nfish_cH_agec(1,nyr_cH_agec);
init_matrix obs_cH_agec(1,nyr_cH_agec,1,nages_agec);

// Comm HL  Discards (1000 fish)
init_int styr_cH_D;
init_int endyr_cH_D;
init_vector obs_cH_released(styr_cH_D,endyr_cH_D);
init_vector cH_D_cv(styr_cH_D,endyr_cH_D);

//Comm HL discards length compositions
// Number and vector of years of length compositions to be pooled in first pooled comp (2007-2009)
init_int nyr_cH_D_lenc_pool1;
init_ivector yrs_cH_D_lenc_pool1(1,nyr_cH_D_lenc_pool1);
//Annual sample size (ntrips) of length comp data; used to weight years for pooling
init_vector nsamp_cH_D_lenc_pool1(1,nyr_cH_D_lenc_pool1);

// Number and vector of years of length compositions to be pooled in second pooled comp (2010-2019)
init_int nyr_cH_D_lenc_pool2;
init_ivector yrs_cH_D_lenc_pool2(1,nyr_cH_D_lenc_pool2);
//Annual sample sizes (ntrips) used to weight years for pooling
init_vector nsamp_cH_D_lenc_pool2(1,nyr_cH_D_lenc_pool2);

//Pooled comps
init_int nyr_cH_D_lenc;
init_ivector yrs_cH_D_lenc(1,nyr_cH_D_lenc);
init_vector nsamp_cH_D_lenc(1,nyr_cH_D_lenc);
init_vector nfish_cH_D_lenc(1,nyr_cH_D_lenc);
init_matrix obs_cH_D_lenc(1,nyr_cH_D_lenc,1,nlenbins);

//###################Headboat fleet ##########################################
// HB CPUE
init_int styr_HB_cpue;                                             
init_int endyr_HB_cpue;                                            
init_vector obs_HB_cpue(styr_HB_cpue,endyr_HB_cpue);//Observed CPUE
init_vector HB_cpue_cv(styr_HB_cpue,endyr_HB_cpue); //CV of cpue

// HB Landings (1000s fish)
init_int styr_HB_L;
init_int endyr_HB_L;
init_vector obs_HB_L(styr_HB_L,endyr_HB_L);   //vector of observed landings by year 
init_vector HB_L_cv(styr_HB_L,endyr_HB_L);    //vector of CV of landings by year

// HB age compositions
init_int nyr_HB_agec;
init_ivector yrs_HB_agec(1,nyr_HB_agec);
init_vector nsamp_HB_agec(1,nyr_HB_agec);
init_vector nfish_HB_agec(1,nyr_HB_agec);
init_matrix obs_HB_agec(1,nyr_HB_agec,1,nages_agec_10p);

// HB discards CPUE
init_int styr_HB_D_cpue;                                             
init_int endyr_HB_D_cpue;                                            
init_vector obs_HB_D_cpue(styr_HB_D_cpue,endyr_HB_D_cpue);//Observed CPUE
init_vector HB_D_cpue_cv(styr_HB_D_cpue,endyr_HB_D_cpue); //CV of cpue

// HB  Discards (1000 fish)
init_int styr_HB_D;
init_int endyr_HB_D;
init_vector obs_HB_released(styr_HB_D,endyr_HB_D);
init_vector HB_D_cv(styr_HB_D,endyr_HB_D);

//HB discards length compositions
init_int nyr_HB_D_lenc;
init_ivector yrs_HB_D_lenc(1,nyr_HB_D_lenc);
init_vector nsamp_HB_D_lenc(1,nyr_HB_D_lenc);
init_vector nfish_HB_D_lenc(1,nyr_HB_D_lenc);
init_matrix obs_HB_D_lenc(1,nyr_HB_D_lenc,1,nlenbins);

//###################Private/Charterboat Recreational fleet ##########################################
// Landings (1000s fish)
init_int styr_GR_L;
init_int endyr_GR_L;
init_vector obs_GR_L(styr_GR_L,endyr_GR_L);   //vector of observed landings by year 
init_vector GR_L_cv(styr_GR_L,endyr_GR_L);    //vector of CV of landings by year

//  Discards (1000 fish)
init_int styr_GR_D;
init_int endyr_GR_D;
init_vector obs_GR_released(styr_GR_D,endyr_GR_D);
init_vector GR_D_cv(styr_GR_D,endyr_GR_D);

//  Age compositions
init_int nyr_GR_agec;
init_ivector yrs_GR_agec(1,nyr_GR_agec);
init_vector nsamp_GR_agec(1,nyr_GR_agec);
init_vector nfish_GR_agec(1,nyr_GR_agec);
init_matrix obs_GR_agec(1,nyr_GR_agec,1,nages_agec_10p);

//GR discards length compositions
init_int nyr_GR_D_lenc;
init_ivector yrs_GR_D_lenc(1,nyr_GR_D_lenc);
init_vector nsamp_GR_D_lenc(1,nyr_GR_D_lenc);
init_vector nfish_GR_D_lenc(1,nyr_GR_D_lenc);
init_matrix obs_GR_D_lenc(1,nyr_GR_D_lenc,1,nlenbins);

//###SERFS CVT################# 
//CPUE
init_int styr_CVT_cpue;
init_int endyr_CVT_cpue;
init_vector obs_CVT_cpue(styr_CVT_cpue,endyr_CVT_cpue);   //Observed CPUE
init_vector CVT_cpue_cv(styr_CVT_cpue,endyr_CVT_cpue);    //CV of cpue

// Age Compositions 
init_int nyr_CVT_agec;
init_ivector yrs_CVT_agec(1,nyr_CVT_agec);
init_vector nsamp_CVT_agec(1,nyr_CVT_agec);
init_vector nfish_CVT_agec(1,nyr_CVT_agec);
init_matrix obs_CVT_agec(1,nyr_CVT_agec,1,nages_agec_10p);

//###SERFS VID################# 
//CPUE
init_int styr_VID_cpue;
init_int endyr_VID_cpue;
init_vector obs_VID_cpue(styr_VID_cpue,endyr_VID_cpue);   //Observed CPUE
init_vector VID_cpue_cv(styr_VID_cpue,endyr_VID_cpue);    //CV of cpue

//--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>
//-- BAM DATA_SECTION: parameter section
//--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>
//##################Single Parameter values and initial guesses #################################
// von Bert parms in TL mm population
init_vector set_Linf(1,7);
init_vector set_K(1,7);
init_vector set_t0(1,7);
init_vector set_len_cv(1,7);

//von Bert parms for all fisheries in periods 1 and 3
init_vector set_Linf_L(1,7);
init_vector set_K_L(1,7);
init_vector set_t0_L(1,7);
init_vector set_len_cv_L(1,7);

// von Bert parms in TL mm all fleets, 20in size limit
init_vector set_Linf_20(1,7);
init_vector set_K_20(1,7);
init_vector set_t0_20(1,7);
init_vector set_len_cv_20(1,7);

//Scalar used only for computing MSST. For gag, this might be eventually be based on 0.25: MSST=(1-0.25)SSBmsy
init_vector set_M_constant(1,7);     
//Spawner-recruit parameters (Initial guesses or fixed values)
init_vector set_steep(1,7);         //recruitment steepness
init_vector set_log_R0(1,7);        //recruitment R0
init_vector set_R_autocorr(1,7);    //recruitment autocorrelation
init_vector set_rec_sigma(1,7);     //recruitment standard deviation in log space

//Dirichlet-multinomial overdispersion parameters
init_vector set_log_dm_cH_lc(1,7);    
init_vector set_log_dm_cH_D_lc(1,7);    
init_vector set_log_dm_HB_D_lc(1,7); 
init_vector set_log_dm_GR_D_lc(1,7);    
init_vector set_log_dm_cH_ac(1,7);    
init_vector set_log_dm_HB_ac(1,7);  
init_vector set_log_dm_CVT_ac(1,7);   
init_vector set_log_dm_GR_ac(1,7);    


//Initial guesses or fixed values of estimated selectivity parameters
init_vector set_selpar_A50_cH1(1,7);
init_vector set_selpar_slope_cH1(1,7);

init_vector set_selpar_A50_cH2(1,7);
init_vector set_selpar_slope_cH2(1,7);

init_vector set_selpar_A50_cH3(1,7); 
init_vector set_selpar_slope_cH3(1,7);

init_vector set_selpar_A50_HB1(1,7);
init_vector set_selpar_slope_HB1(1,7);
init_vector set_selpar_A502_HB1(1,7);
init_vector set_selpar_slope2_HB1(1,7);

init_vector set_selpar_A50_HB2(1,7);
init_vector set_selpar_slope_HB2(1,7);
init_vector set_selpar_A502_HB2(1,7);
init_vector set_selpar_slope2_HB2(1,7);

init_vector set_selpar_A50_HB3(1,7); 
init_vector set_selpar_slope_HB3(1,7);
init_vector set_selpar_A502_HB3(1,7);
init_vector set_selpar_slope2_HB3(1,7);

init_vector set_selpar_A50_GR2(1,7); 
init_vector set_selpar_slope_GR2(1,7);
init_vector set_selpar_A502_GR2(1,7);
init_vector set_selpar_slope2_GR2(1,7);

init_vector set_selpar_A50_GR3(1,7); 
init_vector set_selpar_slope_GR3(1,7);

init_vector set_selpar_A50_CVT(1,7); 
init_vector set_selpar_slope_CVT(1,7);
init_vector set_selpar_A502_CVT(1,7); 
init_vector set_selpar_slope2_CVT(1,7);

init_vector set_selpar_A50_cH2_D(1,7);  
init_vector set_selpar_slope_cH2_D(1,7);
init_vector set_selpar_A502_cH2_D(1,7);
init_vector set_selpar_slope2_cH2_D(1,7);

init_vector set_selpar_A50_cH3_D(1,7);  
init_vector set_selpar_slope_cH3_D(1,7);

init_vector set_selpar_A50_HB2_D(1,7); 
init_vector set_selpar_slope_HB2_D(1,7);
init_vector set_selpar_A502_HB2_D(1,7);
init_vector set_selpar_slope2_HB2_D(1,7);

init_vector set_selpar_A50_HB3_D(1,7);  
init_vector set_selpar_slope_HB3_D(1,7);
init_vector set_selpar_A502_HB3_D(1,7);
init_vector set_selpar_slope2_HB3_D(1,7);

init_vector set_selpar_A50_GR3_D(1,7);  
init_vector set_selpar_slope_GR3_D(1,7);
init_vector set_selpar_A502_GR3_D(1,7);
init_vector set_selpar_slope2_GR3_D(1,7);

//--index catchability-----------------------------------------------------------------------------------
init_vector set_log_q_cH(1,7);      //catchability coefficient (log) for comm handline index
init_vector set_log_q_HB(1,7);      //catchability coefficient (log) for headboat index
init_vector set_log_q_HB_D(1,7);      //catchability coefficient (log) for headboat discard index
init_vector set_log_q_CVT(1,7);      //catchability coefficient (log) for SERFS CVT index  
init_vector set_log_q_VID(1,7);      //catchability coefficient (log) for SERFS VID index  

//initial F
init_vector set_F_init(1,7);  //scales initial F 
//--mean F's in log space --------------------------------
init_vector set_log_avg_F_cH(1,7);
init_vector set_log_avg_F_HB(1,7);
init_vector set_log_avg_F_GR(1,7);
init_vector set_log_avg_F_cH_D(1,7);
init_vector set_log_avg_F_HB_D(1,7);
init_vector set_log_avg_F_GR_D(1,7);

//##################Dev Vector Parameter values (vals) and bounds #################################
//--F vectors---------------------------
init_vector set_log_F_dev_cH(1,3); 
init_vector set_log_F_dev_HB(1,3);
init_vector set_log_F_dev_GR(1,3);
init_vector set_log_F_dev_cH_D(1,3); 
init_vector set_log_F_dev_HB_D(1,3);
init_vector set_log_F_dev_GR_D(1,3);
init_vector set_log_rec_dev(1,3);
init_vector set_log_Nage_dev(1,3);

init_vector set_log_F_dev_cH_vals(styr_cH_L,endyr_cH_L);
init_vector set_log_F_dev_HB_vals(styr_HB_L,endyr_HB_L);
init_vector set_log_F_dev_GR_vals(styr_GR_L,endyr_GR_L);
init_vector set_log_F_dev_cH_D_vals(styr_cH_D,endyr_cH_D);
init_vector set_log_F_dev_HB_D_vals(styr_HB_D,endyr_HB_D);
init_vector set_log_F_dev_GR_D_vals(styr_GR_D,endyr_GR_D);
init_vector set_log_rec_dev_vals(styr_rec_dev,endyr_rec_dev);
init_vector set_log_Nage_dev_vals(2,nages);             

//--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>
//-- BAM DATA_SECTION: likelihood weights section
//--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>

init_number set_w_L;
init_number set_w_D;            //weight for discards
init_number set_w_I_cH;         //weight for comm handline index
init_number set_w_I_HB;         //weight for headboat index
init_number set_w_I_HB_D;       //weight for hb discard index
init_number set_w_I_CVT;		//weight for SERFS CVT index
init_number set_w_I_VID;		//weight for SERFS VID index
init_number set_w_I_CVT_mult;   //weight for SERFS CVT index, multiplier on likelihood component
init_number set_w_I_VID_mult;   //weight for SERFS VID index, multiplier on likelihood component

init_number set_w_lc_cH;        //weight for comm handline len comps
init_number set_w_lc_cH_D;
init_number set_w_lc_HB_D;		//weight for the headboat discard length comps 
init_number set_w_lc_GR_D;		//weight for the gen rec discard length comps 
init_number set_w_ac_cH;        //weight for comm handline age comps
init_number set_w_ac_HB;        //weight for headboat age comps
init_number set_w_ac_CVT;		//weight for the SERFS age comps  
init_number set_w_ac_GR;		//weight for the Recreational age comps  
init_number set_w_Nage_init;    //for fitting initial abundance at age (excluding first age)
init_number set_w_rec;          //for fitting S-R curve
init_number set_w_rec_early;    //additional constraint on early years recruitment
init_number set_w_rec_end;      //additional constraint on ending years recruitment 
init_number set_w_fullF;        //penalty for any Fapex>3(removed in final phase of optimization)
init_number set_w_Ftune;        //weight applied to tuning F (removed in final phase of optimization)
//--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>
//-- BAM DATA_SECTION: miscellaneous stuff section
//--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>

//TL(mm)-weight(whole weight in kg) relationship: W=aL^b
init_number wgtpar_a;
init_number wgtpar_b;

//weight(whole weight)-gonad weight (units=g) relationship: GW=a+b*W
init_number ww2gw;

//Maturity and proportion female at age
init_vector maturity_f_obs(1,nages);            //proportion females mature at age

//Fecundity-length relationship
init_vector prop_f_obs(1,nages);
init_vector set_fecpar_batches(1,nages);
init_number set_fecpar_a;
init_number set_fecpar_b;
init_number set_fecpar_c;
init_number set_fecpar_thresh;
init_number set_fecpar_min;
init_number fecpar_scale;
init_number spawn_time_frac; //time of year of peak spawning, as a fraction of the year

// Natural mortality
init_vector set_M(1,nages);     //age-dependent: used in model
init_number max_obs_age;        //max observed age, used to scale M, if estimated

//Define discard mortality time blocks
init_int endyr_cH_Dmort1;
init_int endyr_HB_Dmort1;
init_int endyr_GR_Dmort1;
init_int endyr_cH_Dmort2;
init_int endyr_HB_Dmort2;
init_int endyr_GR_Dmort2;
//discard mortality constants
init_number set_Dmort_cH1;
init_number set_Dmort_HB1;
init_number set_Dmort_GR1;
init_number set_Dmort_cH2;
init_number set_Dmort_HB2;
init_number set_Dmort_GR2;
init_number set_Dmort_cH3;
init_number set_Dmort_HB3;
init_number set_Dmort_GR3;

//scalar multiple applied to observed historic recreational landings in MCB analysis
init_number historic_Lrec_scale;


//rate of increase on q
init_int set_q_rate_phase;  //value sets estimation phase of rate increase, negative value turns it off
init_number set_q_rate;
//density dependence on fishery q's 
init_int set_q_DD_phase;      //value sets estimation phase of random walk, negative value turns it off
init_number set_q_DD_beta;    //value of 0.0 is density indepenent
init_number set_q_DD_beta_se;
init_int set_q_DD_stage;      //age to begin counting biomass, should be near full exploitation

//random walk on fishery q's 
init_int set_q_RW_phase;         //value sets estimation phase of random walk, negative value turns it off
init_number set_q_RW_rec_var;     //assumed variance of RW q

//Tune Fapex (tuning removed in final year of optimization)
init_number set_Ftune;
init_int set_Ftune_yr;

//threshold sample sizes for length comps 
init_number minSS_cH_lenc;
init_number minSS_cH_D_lenc;
init_number minSS_HB_D_lenc;
init_number minSS_GR_D_lenc;

//threshold sample sizes for age comps
init_number minSS_cH_agec;
init_number minSS_HB_agec;
init_number minSS_CVT_agec;
init_number minSS_GR_agec;
//ageing error matrix (columns are true ages, rows are ages as read for age comps: columns should sum to one)
init_matrix age_error(1,nages,1,nages);

// #######Indexing integers for year(iyear), age(iage),length(ilen) ###############
int iyear;
int iage;
int ilen;
int ff;

number sqrt2pi;
number g2mt;                    //conversion of grams to metric tons 
number g2kg;                    //conversion of grams to kg   
number g2klb;                   //conversion of grams to 1000 lb   
number mt2klb;                  //conversion of metric tons to 1000 lb
number mt2lb;                   //conversion of metric tons to lb
number dzero;                   //small additive constant to prevent division by zero
number huge_number;             //huge number, to avoid irregular parameter space

init_number end_of_data_file;
//this section MUST BE INDENTED!!!
 LOCAL_CALCS
   if(end_of_data_file!=999)
   {
       cout << "*** WARNING: Data File NOT READ CORRECTLY ****" << endl;
       exit(0);  
   }
   else
   {cout << "Data File read correctly" << endl;} 
 END_CALCS   


PARAMETER_SECTION

 LOCAL_CALCS
  const double Linf_LO=set_Linf(2); const double Linf_HI=set_Linf(3); const double Linf_PH=set_Linf(4);
  const double K_LO=set_K(2); const double K_HI=set_K(3); const double K_PH=set_K(4);
  const double t0_LO=set_t0(2); const double t0_HI=set_t0(3); const double t0_PH=set_t0(4);  
  const double len_cv_LO=set_len_cv(2); const double len_cv_HI=set_len_cv(3); const double len_cv_PH=set_len_cv(4); 
  
  const double Linf_L_LO=set_Linf_L(2); const double Linf_L_HI=set_Linf_L(3); const double Linf_L_PH=set_Linf_L(4);
  const double K_L_LO=set_K_L(2); const double K_L_HI=set_K_L(3); const double K_L_PH=set_K_L(4);
  const double t0_L_LO=set_t0_L(2); const double t0_L_HI=set_t0_L(3); const double t0_L_PH=set_t0_L(4);  
  const double len_cv_L_LO=set_len_cv_L(2); const double len_cv_L_HI=set_len_cv_L(3); const double len_cv_L_PH=set_len_cv_L(4);
  
  const double Linf_20_LO=set_Linf_20(2); const double Linf_20_HI=set_Linf_20(3); const double Linf_20_PH=set_Linf_20(4);
  const double K_20_LO=set_K_20(2); const double K_20_HI=set_K_20(3); const double K_20_PH=set_K_20(4);
  const double t0_20_LO=set_t0_20(2); const double t0_20_HI=set_t0_20(3); const double t0_20_PH=set_t0_20(4);  
  const double len_cv_20_LO=set_len_cv_20(2); const double len_cv_20_HI=set_len_cv_20(3); const double len_cv_20_PH=set_len_cv_20(4);
   
  const double M_constant_LO=set_M_constant(2); const double M_constant_HI=set_M_constant(3); const double M_constant_PH=set_M_constant(4);        
  const double steep_LO=set_steep(2); const double steep_HI=set_steep(3); const double steep_PH=set_steep(4);
  const double log_R0_LO=set_log_R0(2); const double log_R0_HI=set_log_R0(3); const double log_R0_PH=set_log_R0(4);
  const double R_autocorr_LO=set_R_autocorr(2); const double R_autocorr_HI=set_R_autocorr(3); const double R_autocorr_PH=set_R_autocorr(4);
  const double rec_sigma_LO=set_rec_sigma(2); const double rec_sigma_HI=set_rec_sigma(3); const double rec_sigma_PH=set_rec_sigma(4);

  const double log_dm_cH_lc_LO=set_log_dm_cH_lc(2); const double log_dm_cH_lc_HI=set_log_dm_cH_lc(3); const double log_dm_cH_lc_PH=set_log_dm_cH_lc(4);
  const double log_dm_cH_D_lc_LO=set_log_dm_cH_D_lc(2); const double log_dm_cH_D_lc_HI=set_log_dm_cH_D_lc(3); const double log_dm_cH_D_lc_PH=set_log_dm_cH_D_lc(4);
  const double log_dm_HB_D_lc_LO=set_log_dm_HB_D_lc(2); const double log_dm_HB_D_lc_HI=set_log_dm_HB_D_lc(3); const double log_dm_HB_D_lc_PH=set_log_dm_HB_D_lc(4);
  const double log_dm_GR_D_lc_LO=set_log_dm_GR_D_lc(2); const double log_dm_GR_D_lc_HI=set_log_dm_GR_D_lc(3); const double log_dm_GR_D_lc_PH=set_log_dm_GR_D_lc(4);
  const double log_dm_cH_ac_LO=set_log_dm_cH_ac(2); const double log_dm_cH_ac_HI=set_log_dm_cH_ac(3); const double log_dm_cH_ac_PH=set_log_dm_cH_ac(4);
  const double log_dm_HB_ac_LO=set_log_dm_HB_ac(2); const double log_dm_HB_ac_HI=set_log_dm_HB_ac(3); const double log_dm_HB_ac_PH=set_log_dm_HB_ac(4);
  const double log_dm_CVT_ac_LO=set_log_dm_CVT_ac(2); const double log_dm_CVT_ac_HI=set_log_dm_CVT_ac(3); const double log_dm_CVT_ac_PH=set_log_dm_CVT_ac(4);
  const double log_dm_GR_ac_LO=set_log_dm_GR_ac(2); const double log_dm_GR_ac_HI=set_log_dm_GR_ac(3); const double log_dm_GR_ac_PH=set_log_dm_GR_ac(4);

  
  const double selpar_A50_cH1_LO=set_selpar_A50_cH1(2); const double selpar_A50_cH1_HI=set_selpar_A50_cH1(3); const double selpar_A50_cH1_PH=set_selpar_A50_cH1(4);
  const double selpar_slope_cH1_LO=set_selpar_slope_cH1(2); const double selpar_slope_cH1_HI=set_selpar_slope_cH1(3); const double selpar_slope_cH1_PH=set_selpar_slope_cH1(4);
  const double selpar_A50_cH2_LO=set_selpar_A50_cH2(2); const double selpar_A50_cH2_HI=set_selpar_A50_cH2(3); const double selpar_A50_cH2_PH=set_selpar_A50_cH2(4);
  const double selpar_slope_cH2_LO=set_selpar_slope_cH2(2); const double selpar_slope_cH2_HI=set_selpar_slope_cH2(3); const double selpar_slope_cH2_PH=set_selpar_slope_cH2(4);
  const double selpar_A50_cH3_LO=set_selpar_A50_cH3(2); const double selpar_A50_cH3_HI=set_selpar_A50_cH3(3); const double selpar_A50_cH3_PH=set_selpar_A50_cH3(4);
  const double selpar_slope_cH3_LO=set_selpar_slope_cH3(2); const double selpar_slope_cH3_HI=set_selpar_slope_cH3(3); const double selpar_slope_cH3_PH=set_selpar_slope_cH3(4);
  
  const double selpar_A50_HB1_LO=set_selpar_A50_HB1(2); const double selpar_A50_HB1_HI=set_selpar_A50_HB1(3); const double selpar_A50_HB1_PH=set_selpar_A50_HB1(4);
  const double selpar_slope_HB1_LO=set_selpar_slope_HB1(2); const double selpar_slope_HB1_HI=set_selpar_slope_HB1(3); const double selpar_slope_HB1_PH=set_selpar_slope_HB1(4);
  const double selpar_A502_HB1_LO=set_selpar_A502_HB1(2); const double selpar_A502_HB1_HI=set_selpar_A502_HB1(3); const double selpar_A502_HB1_PH=set_selpar_A502_HB1(4);
  const double selpar_slope2_HB1_LO=set_selpar_slope2_HB1(2); const double selpar_slope2_HB1_HI=set_selpar_slope2_HB1(3); const double selpar_slope2_HB1_PH=set_selpar_slope2_HB1(4);
  const double selpar_A50_HB2_LO=set_selpar_A50_HB2(2); const double selpar_A50_HB2_HI=set_selpar_A50_HB2(3); const double selpar_A50_HB2_PH=set_selpar_A50_HB2(4);
  const double selpar_slope_HB2_LO=set_selpar_slope_HB2(2); const double selpar_slope_HB2_HI=set_selpar_slope_HB2(3); const double selpar_slope_HB2_PH=set_selpar_slope_HB2(4);
  const double selpar_A502_HB2_LO=set_selpar_A502_HB2(2); const double selpar_A502_HB2_HI=set_selpar_A502_HB2(3); const double selpar_A502_HB2_PH=set_selpar_A502_HB2(4);
  const double selpar_slope2_HB2_LO=set_selpar_slope2_HB2(2); const double selpar_slope2_HB2_HI=set_selpar_slope2_HB2(3); const double selpar_slope2_HB2_PH=set_selpar_slope2_HB2(4);
  const double selpar_A50_HB3_LO=set_selpar_A50_HB3(2); const double selpar_A50_HB3_HI=set_selpar_A50_HB3(3); const double selpar_A50_HB3_PH=set_selpar_A50_HB3(4);
  const double selpar_slope_HB3_LO=set_selpar_slope_HB3(2); const double selpar_slope_HB3_HI=set_selpar_slope_HB3(3); const double selpar_slope_HB3_PH=set_selpar_slope_HB3(4);
  const double selpar_A502_HB3_LO=set_selpar_A502_HB3(2); const double selpar_A502_HB3_HI=set_selpar_A502_HB3(3); const double selpar_A502_HB3_PH=set_selpar_A502_HB3(4);
  const double selpar_slope2_HB3_LO=set_selpar_slope2_HB3(2); const double selpar_slope2_HB3_HI=set_selpar_slope2_HB3(3); const double selpar_slope2_HB3_PH=set_selpar_slope2_HB3(4);
  
  const double selpar_A50_GR2_LO=set_selpar_A50_GR2(2); const double selpar_A50_GR2_HI=set_selpar_A50_GR2(3); const double selpar_A50_GR2_PH=set_selpar_A50_GR2(4);
  const double selpar_slope_GR2_LO=set_selpar_slope_GR2(2); const double selpar_slope_GR2_HI=set_selpar_slope_GR2(3); const double selpar_slope_GR2_PH=set_selpar_slope_GR2(4);
  const double selpar_A502_GR2_LO=set_selpar_A502_GR2(2); const double selpar_A502_GR2_HI=set_selpar_A502_GR2(3); const double selpar_A502_GR2_PH=set_selpar_A502_GR2(4);
  const double selpar_slope2_GR2_LO=set_selpar_slope2_GR2(2); const double selpar_slope2_GR2_HI=set_selpar_slope2_GR2(3); const double selpar_slope2_GR2_PH=set_selpar_slope2_GR2(4);

  const double selpar_A50_GR3_LO=set_selpar_A50_GR3(2); const double selpar_A50_GR3_HI=set_selpar_A50_GR3(3); const double selpar_A50_GR3_PH=set_selpar_A50_GR3(4);
  const double selpar_slope_GR3_LO=set_selpar_slope_GR3(2); const double selpar_slope_GR3_HI=set_selpar_slope_GR3(3); const double selpar_slope_GR3_PH=set_selpar_slope_GR3(4);
  
  const double selpar_A50_CVT_LO=set_selpar_A50_CVT(2); const double selpar_A50_CVT_HI=set_selpar_A50_CVT(3); const double selpar_A50_CVT_PH=set_selpar_A50_CVT(4);
  const double selpar_slope_CVT_LO=set_selpar_slope_CVT(2); const double selpar_slope_CVT_HI=set_selpar_slope_CVT(3); const double selpar_slope_CVT_PH=set_selpar_slope_CVT(4);
  const double selpar_A502_CVT_LO=set_selpar_A502_CVT(2); const double selpar_A502_CVT_HI=set_selpar_A502_CVT(3); const double selpar_A502_CVT_PH=set_selpar_A502_CVT(4);
  const double selpar_slope2_CVT_LO=set_selpar_slope2_CVT(2); const double selpar_slope2_CVT_HI=set_selpar_slope2_CVT(3); const double selpar_slope2_CVT_PH=set_selpar_slope2_CVT(4);
  
  const double selpar_A50_HB2_D_LO=set_selpar_A50_HB2_D(2); const double selpar_A50_HB2_D_HI=set_selpar_A50_HB2_D(3); const double selpar_A50_HB2_D_PH=set_selpar_A50_HB2_D(4);
  const double selpar_slope_HB2_D_LO=set_selpar_slope_HB2_D(2); const double selpar_slope_HB2_D_HI=set_selpar_slope_HB2_D(3); const double selpar_slope_HB2_D_PH=set_selpar_slope_HB2_D(4);
  const double selpar_A502_HB2_D_LO=set_selpar_A502_HB2_D(2); const double selpar_A502_HB2_D_HI=set_selpar_A502_HB2_D(3); const double selpar_A502_HB2_D_PH=set_selpar_A502_HB2_D(4);
  const double selpar_slope2_HB2_D_LO=set_selpar_slope2_HB2_D(2); const double selpar_slope2_HB2_D_HI=set_selpar_slope2_HB2_D(3); const double selpar_slope2_HB2_D_PH=set_selpar_slope2_HB2_D(4);
  
  const double selpar_A50_HB3_D_LO=set_selpar_A50_HB3_D(2); const double selpar_A50_HB3_D_HI=set_selpar_A50_HB3_D(3); const double selpar_A50_HB3_D_PH=set_selpar_A50_HB3_D(4);
  const double selpar_slope_HB3_D_LO=set_selpar_slope_HB3_D(2); const double selpar_slope_HB3_D_HI=set_selpar_slope_HB3_D(3); const double selpar_slope_HB3_D_PH=set_selpar_slope_HB3_D(4);
  const double selpar_A502_HB3_D_LO=set_selpar_A502_HB3_D(2); const double selpar_A502_HB3_D_HI=set_selpar_A502_HB3_D(3); const double selpar_A502_HB3_D_PH=set_selpar_A502_HB3_D(4);
  const double selpar_slope2_HB3_D_LO=set_selpar_slope2_HB3_D(2); const double selpar_slope2_HB3_D_HI=set_selpar_slope2_HB3_D(3); const double selpar_slope2_HB3_D_PH=set_selpar_slope2_HB3_D(4);

  const double selpar_A50_GR3_D_LO=set_selpar_A50_GR3_D(2); const double selpar_A50_GR3_D_HI=set_selpar_A50_GR3_D(3); const double selpar_A50_GR3_D_PH=set_selpar_A50_GR3_D(4);
  const double selpar_slope_GR3_D_LO=set_selpar_slope_GR3_D(2); const double selpar_slope_GR3_D_HI=set_selpar_slope_GR3_D(3); const double selpar_slope_GR3_D_PH=set_selpar_slope_GR3_D(4);
  const double selpar_A502_GR3_D_LO=set_selpar_A502_GR3_D(2); const double selpar_A502_GR3_D_HI=set_selpar_A502_GR3_D(3); const double selpar_A502_GR3_D_PH=set_selpar_A502_GR3_D(4);
  const double selpar_slope2_GR3_D_LO=set_selpar_slope2_GR3_D(2); const double selpar_slope2_GR3_D_HI=set_selpar_slope2_GR3_D(3); const double selpar_slope2_GR3_D_PH=set_selpar_slope2_GR3_D(4);
  
  const double selpar_A50_cH2_D_LO=set_selpar_A50_cH2_D(2); const double selpar_A50_cH2_D_HI=set_selpar_A50_cH2_D(3); const double selpar_A50_cH2_D_PH=set_selpar_A50_cH2_D(4);
  const double selpar_slope_cH2_D_LO=set_selpar_slope_cH2_D(2); const double selpar_slope_cH2_D_HI=set_selpar_slope_cH2_D(3); const double selpar_slope_cH2_D_PH=set_selpar_slope_cH2_D(4);
  const double selpar_A502_cH2_D_LO=set_selpar_A502_cH2_D(2); const double selpar_A502_cH2_D_HI=set_selpar_A502_cH2_D(3); const double selpar_A502_cH2_D_PH=set_selpar_A502_cH2_D(4);
  const double selpar_slope2_cH2_D_LO=set_selpar_slope2_cH2_D(2); const double selpar_slope2_cH2_D_HI=set_selpar_slope2_cH2_D(3); const double selpar_slope2_cH2_D_PH=set_selpar_slope2_cH2_D(4);
  
  const double selpar_A50_cH3_D_LO=set_selpar_A50_cH3_D(2); const double selpar_A50_cH3_D_HI=set_selpar_A50_cH3_D(3); const double selpar_A50_cH3_D_PH=set_selpar_A50_cH3_D(4);
  const double selpar_slope_cH3_D_LO=set_selpar_slope_cH3_D(2); const double selpar_slope_cH3_D_HI=set_selpar_slope_cH3_D(3); const double selpar_slope_cH3_D_PH=set_selpar_slope_cH3_D(4);
  
  const double log_q_cH_LO=set_log_q_cH(2); const double log_q_cH_HI=set_log_q_cH(3); const double log_q_cH_PH=set_log_q_cH(4);
  const double log_q_HB_LO=set_log_q_HB(2); const double log_q_HB_HI=set_log_q_HB(3); const double log_q_HB_PH=set_log_q_HB(4);
  const double log_q_HB_D_LO=set_log_q_HB_D(2); const double log_q_HB_D_HI=set_log_q_HB_D(3); const double log_q_HB_D_PH=set_log_q_HB_D(4);
  const double log_q_CVT_LO=set_log_q_CVT(2); const double log_q_CVT_HI=set_log_q_CVT(3); const double log_q_CVT_PH=set_log_q_CVT(4);
  const double log_q_VID_LO=set_log_q_VID(2); const double log_q_VID_HI=set_log_q_VID(3); const double log_q_VID_PH=set_log_q_VID(4);
  
  const double F_init_LO=set_F_init(2); const double F_init_HI=set_F_init(3); const double F_init_PH=set_F_init(4);
  const double log_avg_F_cH_LO=set_log_avg_F_cH(2); const double log_avg_F_cH_HI=set_log_avg_F_cH(3); const double log_avg_F_cH_PH=set_log_avg_F_cH(4);
  const double log_avg_F_HB_LO=set_log_avg_F_HB(2); const double log_avg_F_HB_HI=set_log_avg_F_HB(3); const double log_avg_F_HB_PH=set_log_avg_F_HB(4); 
  const double log_avg_F_GR_LO=set_log_avg_F_GR(2); const double log_avg_F_GR_HI=set_log_avg_F_GR(3); const double log_avg_F_GR_PH=set_log_avg_F_GR(4); 
  const double log_avg_F_cH_D_LO=set_log_avg_F_cH_D(2); const double log_avg_F_cH_D_HI=set_log_avg_F_cH_D(3); const double log_avg_F_cH_D_PH=set_log_avg_F_cH_D(4);
  const double log_avg_F_HB_D_LO=set_log_avg_F_HB_D(2); const double log_avg_F_HB_D_HI=set_log_avg_F_HB_D(3); const double log_avg_F_HB_D_PH=set_log_avg_F_HB_D(4); 
  const double log_avg_F_GR_D_LO=set_log_avg_F_GR_D(2); const double log_avg_F_GR_D_HI=set_log_avg_F_GR_D(3); const double log_avg_F_GR_D_PH=set_log_avg_F_GR_D(4); 
  
  //-dev vectors-----------------------------------------------------------------------------------------------------------  
  const double log_F_dev_cH_LO=set_log_F_dev_cH(1); const double log_F_dev_cH_HI=set_log_F_dev_cH(2); const double log_F_dev_cH_PH=set_log_F_dev_cH(3);   
  const double log_F_dev_HB_LO=set_log_F_dev_HB(1); const double log_F_dev_HB_HI=set_log_F_dev_HB(2); const double log_F_dev_HB_PH=set_log_F_dev_HB(3);   
  const double log_F_dev_GR_LO=set_log_F_dev_GR(1); const double log_F_dev_GR_HI=set_log_F_dev_GR(2); const double log_F_dev_GR_PH=set_log_F_dev_GR(3);   
  
  const double log_F_dev_cH_D_LO=set_log_F_dev_cH_D(1); const double log_F_dev_cH_D_HI=set_log_F_dev_cH_D(2); const double log_F_dev_cH_D_PH=set_log_F_dev_cH_D(3);   
  const double log_F_dev_HB_D_LO=set_log_F_dev_HB_D(1); const double log_F_dev_HB_D_HI=set_log_F_dev_HB_D(2); const double log_F_dev_HB_D_PH=set_log_F_dev_HB_D(3);   
  const double log_F_dev_GR_D_LO=set_log_F_dev_GR_D(1); const double log_F_dev_GR_D_HI=set_log_F_dev_GR_D(2); const double log_F_dev_GR_D_PH=set_log_F_dev_GR_D(3);   
  
  const double log_rec_dev_LO=set_log_rec_dev(1); const double log_rec_dev_HI=set_log_rec_dev(2); const double log_rec_dev_PH=set_log_rec_dev(3);          
  const double log_Nage_dev_LO=set_log_Nage_dev(1); const double log_Nage_dev_HI=set_log_Nage_dev(2); const double log_Nage_dev_PH=set_log_Nage_dev(3);          
  
 END_CALCS
 
////--------------Growth--------------------------------------------------------------------------- 
  //Population growth parms and conversions
  init_bounded_number Linf(Linf_LO,Linf_HI,Linf_PH);
  init_bounded_number K(K_LO,K_HI,K_PH);
  init_bounded_number t0(t0_LO,t0_HI,t0_PH);
  init_bounded_number len_cv_val(len_cv_LO,len_cv_HI,len_cv_PH);  
  vector Linf_out(1,8);
  vector K_out(1,8);
  vector t0_out(1,8);
  vector len_cv_val_out(1,8);

  vector meanlen_TL(1,nages);   //mean total length (mm) at age all fish
  vector fecundity(1,nages);         // annual fec at age per mat female: fecpar_batches*(fecpar_a+(fecpar_b*TL^fecpar_c)), modified for age 1  
  vector fecpar_batches(1,nages);    //number of batches by age
  number fecpar_a;
  number fecpar_b;
  number fecpar_c;
  number fecpar_thresh; //smaller than this length, apply fecpar_min instead of the model-based value
  number fecpar_min;
   
  vector wgt_g(1,nages);        //whole wgt in g
  vector wgt_kg(1,nages);       //whole wgt in kg
  vector wgt_mt(1,nages);       //whole wgt in mt
  vector wgt_klb(1,nages);      //whole wgt in 1000 lb
  vector wgt_lb(1,nages);       //whole wgt in lb  
  
  init_bounded_number Linf_L(Linf_L_LO,Linf_L_HI,Linf_L_PH);
  init_bounded_number K_L(K_L_LO,K_L_HI,K_L_PH);
  init_bounded_number t0_L(t0_L_LO,t0_L_HI,t0_L_PH);
  init_bounded_number len_cv_val_L(len_cv_L_LO,len_cv_L_HI,len_cv_L_PH);  
  vector Linf_L_out(1,8);
  vector K_L_out(1,8);
  vector t0_L_out(1,8);
  vector len_cv_val_L_out(1,8);
  vector meanlen_TL_L(1,nages);   //mean total length (mm) at age all fish
  
  vector wgt_g_L(1,nages);        //whole wgt in g
  vector wgt_kg_L(1,nages);       //whole wgt in kg
  vector wgt_mt_L(1,nages);       //whole wgt in mt
  vector wgt_klb_L(1,nages);      //whole wgt in 1000 lb
  vector wgt_lb_L(1,nages);       //whole wgt in lb  
  vector wgt_klb_gut_L(1,nages);  //gutted wgt in 1000 lb  
  vector wgt_lb_gut_L(1,nages);   //gutted wgt in lb
     
  init_bounded_number Linf_20(Linf_20_LO,Linf_20_HI,Linf_20_PH);
  init_bounded_number K_20(K_20_LO,K_20_HI,K_20_PH);
  init_bounded_number t0_20(t0_20_LO,t0_20_HI,t0_20_PH);
  init_bounded_number len_cv_val_20(len_cv_20_LO,len_cv_20_HI,len_cv_20_PH);  
  vector Linf_20_out(1,8);
  vector K_20_out(1,8);
  vector t0_20_out(1,8);
  vector len_cv_val_20_out(1,8);
  vector meanlen_TL_20(1,nages);   //mean total length (mm) at age all fish
  
  vector wgt_g_20(1,nages);        //whole wgt in g
  vector wgt_kg_20(1,nages);       //whole wgt in kg
  vector wgt_mt_20(1,nages);       //whole wgt in mt
  vector wgt_klb_20(1,nages);      //whole wgt in 1000 lb
  vector wgt_lb_20(1,nages);       //whole wgt in lb  
    
  matrix len_cH_mm(styr,endyr,1,nages);          //mean length at age of commercial handline landings in mm (may differ from popn mean)
  matrix wholewgt_cH_klb(styr,endyr,1,nages);    //whole wgt of commercial handline landings in 1000 lb   
  matrix len_HB_mm(styr,endyr,1,nages);          //mean length at age of HB landings in mm (may differ from popn mean)    
  matrix wholewgt_HB_klb(styr,endyr,1,nages);    //whole wgt of HB landings in 1000 lb  
  matrix len_GR_mm(styr,endyr,1,nages);          //mean length at age of GR landings in mm (may differ from popn mean)    
  matrix wholewgt_GR_klb(styr,endyr,1,nages);    //whole wgt of GR landings in 1000 lb  

  matrix len_cH_D_mm(styr,endyr,1,nages);          //mean length at age of commercial handline discards in mm (may differ from popn mean)
  matrix wholewgt_cH_D_klb(styr,endyr,1,nages);    //whole wgt of commercial handline discards in 1000 lb   
  matrix len_HB_D_mm(styr,endyr,1,nages);          //mean length at age of HB discards in mm (may differ from popn mean)    
  matrix wholewgt_HB_D_klb(styr,endyr,1,nages);    //whole wgt of HB discards in 1000 lb  
  matrix len_GR_D_mm(styr,endyr,1,nages);          //mean length at age of GR discards in mm (may differ from popn mean)    
  matrix wholewgt_GR_D_klb(styr,endyr,1,nages);    //whole wgt of GR discards in 1000 lb  
  
  matrix lenprob(1,nages,1,nlenbins);           //distn of size at age (age-length key, 3 cm bins) in population
  number zscore_len;                            //standardized normal values used for computing lenprob
  vector cprob_lenvec(1,nlenbins);              //cumulative probabilities used for computing lenprob
  number zscore_lzero;                          //standardized normal values for length = 0
  number cprob_lzero;                           //length probability mass below zero, used for computing lenprob
  
  number zscore_len_L;                            //standardized normal values used for computing lenprob
  vector cprob_lenvec_L(1,nlenbins);              //cumulative probabilities used for computing lenprob
  number zscore_lzero_L;                          //standardized normal values for length = 0
  number cprob_lzero_L;                           //length probability mass below zero, used for computing lenprob
  
  number zscore_len_20;                            //standardized normal values used for computing lenprob
  vector cprob_lenvec_20(1,nlenbins);              //cumulative probabilities used for computing lenprob
  number zscore_lzero_20;                          //standardized normal values for length = 0
  number cprob_lzero_20;                           //length probability mass below zero, used for computing lenprob
  
  //matrices below are used to match length comps
  matrix lenprob_cH(1,nages,1,nlenbins);     //distn of size at age in cH
  matrix lenprob_cH_D(1,nages,1,nlenbins);     //distn of size at age in cH discards  
  matrix lenprob_HB(1,nages,1,nlenbins);     //distn of size at age in HB (rec)
  matrix lenprob_HB_D(1,nages,1,nlenbins);     //distn of size at age in HB discards  
  matrix lenprob_GR_D(1,nages,1,nlenbins);     //distn of size at age in GR discards  
  matrix lenprob_CVT(1,nages,1,nlenbins);     //distn of size at age in CVT  
  matrix lenprob_GR(1,nages,1,nlenbins);     //distn of size at age in GR  
  matrix lenprob_20(1,nages,1,nlenbins);     //distn of size at age under 20" size limit  
  matrix lenprob_L(1,nages,1,nlenbins);
  
  //init_bounded_dev_vector log_len_cv_dev(1,nages,-2,2,3)
  vector len_sd(1,nages);
  vector len_cv(1,nages); //for fishgraph 
  //All Fishery-dependent
  vector len_sd_L(1,nages);
  vector len_cv_L(1,nages); //for fishgraph 
  //20 in size limit
  vector len_sd_20(1,nages);
  vector len_cv_20(1,nages);

//----Predicted length and age compositions
  matrix pred_cH_lenc(1,nyr_cH_lenc,1,nlenbins);
  matrix pred_cH_D_lenc_yr1(1,nyr_cH_D_lenc_pool1,1,nlenbins); //annual predicted length comps, used for pooling first pooled comp
  matrix pred_cH_D_lenc_yr2(1,nyr_cH_D_lenc_pool2,1,nlenbins); //annual predicted length comps, used for pooling second pooled comp
  matrix pred_cH_D_lenc(1,nyr_cH_D_lenc,1,nlenbins);
  matrix pred_HB_D_lenc(1,nyr_HB_D_lenc,1,nlenbins); 
  matrix pred_GR_D_lenc(1,nyr_GR_D_lenc,1,nlenbins); 
  
  matrix pred_cH_agec(1,nyr_cH_agec,1,nages_agec);
  matrix pred_cH_agec_allages(1,nyr_cH_agec,1,nages);
  matrix ErrorFree_cH_agec(1,nyr_cH_agec,1,nages);  
  matrix pred_HB_agec(1,nyr_HB_agec,1,nages_agec_10p);
  matrix pred_HB_agec_allages(1,nyr_HB_agec,1,nages);  
  matrix ErrorFree_HB_agec(1,nyr_HB_agec,1,nages);
  matrix pred_CVT_agec(1,nyr_CVT_agec,1,nages_agec_10p);  
  matrix pred_CVT_agec_allages(1,nyr_CVT_agec,1,nages);  
  matrix ErrorFree_CVT_agec(1,nyr_CVT_agec,1,nages);  
  matrix pred_GR_agec(1,nyr_GR_agec,1,nages_agec_10p);  
  matrix pred_GR_agec_allages(1,nyr_GR_agec,1,nages);  
  matrix ErrorFree_GR_agec(1,nyr_GR_agec,1,nages);  
 
//effective sample size applied in multinomial distributions
  vector nsamp_cH_lenc_allyr(styr,endyr);
  vector nsamp_cH_D_lenc_allyr(styr,endyr);  
  vector nsamp_HB_D_lenc_allyr(styr,endyr);  
  vector nsamp_GR_D_lenc_allyr(styr,endyr);  
  vector nsamp_cH_agec_allyr(styr,endyr);
  vector nsamp_HB_agec_allyr(styr,endyr);
  vector nsamp_CVT_agec_allyr(styr,endyr);  
  vector nsamp_GR_agec_allyr(styr,endyr);  

//Nfish used in MCB analysis (not used in fitting)
  vector nfish_cH_lenc_allyr(styr,endyr);
  vector nfish_cH_D_lenc_allyr(styr,endyr);   
  vector nfish_HB_D_lenc_allyr(styr,endyr);   
  vector nfish_GR_D_lenc_allyr(styr,endyr);   
  vector nfish_cH_agec_allyr(styr,endyr);
  vector nfish_HB_agec_allyr(styr,endyr);
  vector nfish_CVT_agec_allyr(styr,endyr);   
  vector nfish_GR_agec_allyr(styr,endyr);   

//Computed effective sample size for output (not used in fitting)
  vector neff_cH_lenc_allyr_out(styr,endyr);
  vector neff_HB_D_lenc_allyr_out(styr,endyr); 
  vector neff_GR_D_lenc_allyr_out(styr,endyr);    
  vector neff_cH_agec_allyr_out(styr,endyr);
  vector neff_HB_agec_allyr_out(styr,endyr);
  vector neff_CVT_agec_allyr_out(styr,endyr);  
  vector neff_GR_agec_allyr_out(styr,endyr);  

//-----Population-----------------------------------------------------------------------------------
  matrix N(styr,endyr+1,1,nages);           //Population numbers by year and age at start of yr
  matrix N_mdyr(styr,endyr,1,nages);        //Population numbers by year and age at mdpt of yr: used for comps and cpue
  matrix N_spawn(styr,endyr,1,nages);     //Population numbers by year and age at peaking spawning: used for SSB  
  init_bounded_vector log_Nage_dev(2,nages,log_Nage_dev_LO,log_Nage_dev_HI,log_Nage_dev_PH);
  vector log_Nage_dev_output(1,nages);      //used in output. equals zero for first age
  matrix B(styr,endyr+1,1,nages);           //Population biomass by year and age at start of yr
  vector totB(styr,endyr+1);                //Total biomass by year
  vector totN(styr,endyr+1);                //Total abundance by year
  vector SSB(styr,endyr);                 //Total spawning biomass by year (female + male mature biomass) 
  vector rec(styr,endyr+1);                 //Recruits by year
  vector prop_f(1,nages);
  vector maturity_f(1,nages);
  vector reprod(1,nages);
  
//---Stock-Recruit Function (Beverton-Holt, steepness parameterization)----------
  init_bounded_number log_R0(log_R0_LO,log_R0_HI,log_R0_PH);        //log(virgin Recruitment)
  vector log_R0_out(1,8);
  number R0;                                  //virgin recruitment
  init_bounded_number steep(steep_LO,steep_HI,steep_PH); //steepness
  vector steep_out(1,8);
  init_bounded_number rec_sigma(rec_sigma_LO,rec_sigma_HI,rec_sigma_PH);  //sd recruitment residuals  //KC comment out; this should fix it at the initial value
  vector rec_sigma_out(1,8);
  init_bounded_number R_autocorr(R_autocorr_LO,R_autocorr_HI,R_autocorr_PH);  //autocorrelation in SR  KC commented out since not estimated
  vector R_autocorr_out(1,8);

  number rec_sigma_sq;                        //square of rec_sigma      
  number rec_logL_add;                        //additive term in -logL term   
 
  init_bounded_dev_vector log_rec_dev(styr_rec_dev,endyr_rec_dev,log_rec_dev_LO,log_rec_dev_HI,log_rec_dev_PH);
  vector log_rec_dev_output(styr,endyr+1);             //used in t.series output. equals zero except for yrs in log_rec_dev
  vector log_rec_dev_out(styr_rec_dev,endyr_rec_dev);  //used in output for bound checking
  
  number var_rec_dev;                                //variance of log recruitment deviations, from yrs with unconstrainted S-R(XXXX-XXXX)
  number sigma_rec_dev;                              //sample SD of log residuals (may not equal rec_sigma 
                                             
  number BiasCor;                               //Bias correction in equilibrium recruits
  number S0;                                    //equal to spr_F0*R0 = virgin SSB
  number B0;                                    //equal to bpr_F0*R0 = virgin B  
  number R1;                                    //Recruits in styr
  number R_virgin;                              //unfished recruitment with bias correction
  vector SdS0(styr,endyr);                      //SSB relative to virgin SSB              

//Dirichlet-multinomial parameters
  init_bounded_number log_dm_cH_lc(log_dm_cH_lc_LO,log_dm_cH_lc_HI,log_dm_cH_lc_PH);
  init_bounded_number log_dm_cH_D_lc(log_dm_cH_D_lc_LO,log_dm_cH_D_lc_HI,log_dm_cH_D_lc_PH);
  init_bounded_number log_dm_HB_D_lc(log_dm_HB_D_lc_LO,log_dm_HB_D_lc_HI,log_dm_HB_D_lc_PH);
  init_bounded_number log_dm_GR_D_lc(log_dm_GR_D_lc_LO,log_dm_GR_D_lc_HI,log_dm_GR_D_lc_PH);
  init_bounded_number log_dm_cH_ac(log_dm_cH_ac_LO,log_dm_cH_ac_HI,log_dm_cH_ac_PH);
  init_bounded_number log_dm_HB_ac(log_dm_HB_ac_LO,log_dm_HB_ac_HI,log_dm_HB_ac_PH);
  init_bounded_number log_dm_CVT_ac(log_dm_CVT_ac_LO,log_dm_CVT_ac_HI,log_dm_CVT_ac_PH);
  init_bounded_number log_dm_GR_ac(log_dm_GR_ac_LO,log_dm_GR_ac_HI,log_dm_GR_ac_PH);
   
  vector log_dm_cH_lc_out(1,8);
  vector log_dm_cH_D_lc_out(1,8);
  vector log_dm_HB_D_lc_out(1,8);
  vector log_dm_GR_D_lc_out(1,8);
  vector log_dm_cH_ac_out(1,8);
  vector log_dm_HB_ac_out(1,8);
  vector log_dm_CVT_ac_out(1,8);
  vector log_dm_GR_ac_out(1,8);  
  
//-----------------------------------------------------------------------------------------------------------------------------------------------
////---Selectivity-------------------------------------------------------------------------

//Commercial handline-------------------------------------------------
  matrix sel_cH(styr,endyr,1,nages);
  init_bounded_number selpar_A50_cH1(selpar_A50_cH1_LO,selpar_A50_cH1_HI,selpar_A50_cH1_PH);
  init_bounded_number selpar_slope_cH1(selpar_slope_cH1_LO,selpar_slope_cH1_HI,selpar_slope_cH1_PH);
  init_bounded_number selpar_A50_cH2(selpar_A50_cH2_LO,selpar_A50_cH2_HI,selpar_A50_cH2_PH);
  init_bounded_number selpar_slope_cH2(selpar_slope_cH2_LO,selpar_slope_cH2_HI,selpar_slope_cH2_PH);
  init_bounded_number selpar_A50_cH3(selpar_A50_cH3_LO,selpar_A50_cH3_HI,selpar_A50_cH3_PH);
  init_bounded_number selpar_slope_cH3(selpar_slope_cH3_LO,selpar_slope_cH3_HI,selpar_slope_cH3_PH);
  
  vector selpar_A50_cH1_out(1,8);
  vector selpar_slope_cH1_out(1,8);
  vector selpar_A50_cH2_out(1,8);
  vector selpar_slope_cH2_out(1,8);
  vector selpar_A50_cH3_out(1,8);
  vector selpar_slope_cH3_out(1,8);

//Recreational -------------------------------------------------
  matrix sel_HB(styr,endyr,1,nages);
  vector sel_HB_block1(1,nages);
  vector sel_HB_block2(1,nages);
  vector sel_HB_block3(1,nages);
  init_bounded_number selpar_A50_HB1(selpar_A50_HB1_LO,selpar_A50_HB1_HI,selpar_A50_HB1_PH);
  init_bounded_number selpar_slope_HB1(selpar_slope_HB1_LO,selpar_slope_HB1_HI,selpar_slope_HB1_PH);
  init_bounded_number selpar_A502_HB1(selpar_A502_HB1_LO,selpar_A502_HB1_HI,selpar_A502_HB1_PH);
  init_bounded_number selpar_slope2_HB1(selpar_slope2_HB1_LO,selpar_slope2_HB1_HI,selpar_slope2_HB1_PH);
  init_bounded_number selpar_A50_HB2(selpar_A50_HB2_LO,selpar_A50_HB2_HI,selpar_A50_HB2_PH);
  init_bounded_number selpar_slope_HB2(selpar_slope_HB2_LO,selpar_slope_HB2_HI,selpar_slope_HB2_PH);
  init_bounded_number selpar_A502_HB2(selpar_A502_HB2_LO,selpar_A502_HB2_HI,selpar_A502_HB2_PH);
  init_bounded_number selpar_slope2_HB2(selpar_slope2_HB2_LO,selpar_slope2_HB2_HI,selpar_slope2_HB2_PH);
    
  init_bounded_number selpar_A50_HB3(selpar_A50_HB3_LO,selpar_A50_HB3_HI,selpar_A50_HB3_PH);
  init_bounded_number selpar_slope_HB3(selpar_slope_HB3_LO,selpar_slope_HB3_HI,selpar_slope_HB3_PH);
  init_bounded_number selpar_A502_HB3(selpar_A502_HB3_LO,selpar_A502_HB3_HI,selpar_A502_HB3_PH);
  init_bounded_number selpar_slope2_HB3(selpar_slope2_HB3_LO,selpar_slope2_HB3_HI,selpar_slope2_HB3_PH);
  
  vector selpar_A50_HB1_out(1,8);
  vector selpar_slope_HB1_out(1,8);
  vector selpar_A502_HB1_out(1,8);
  vector selpar_slope2_HB1_out(1,8);
  vector selpar_A50_HB2_out(1,8);
  vector selpar_slope_HB2_out(1,8);
  vector selpar_A502_HB2_out(1,8);
  vector selpar_slope2_HB2_out(1,8);
  vector selpar_A50_HB3_out(1,8);
  vector selpar_slope_HB3_out(1,8);
  vector selpar_A502_HB3_out(1,8);
  vector selpar_slope2_HB3_out(1,8);

  //General Rec in the last two time blocks 
  matrix sel_GR(styr,endyr,1,nages);
  vector sel_GR_block1(1,nages);
  vector sel_GR_block2(1,nages);
  vector sel_GR_block3(1,nages);
 
  init_bounded_number selpar_A50_GR2(selpar_A50_GR2_LO,selpar_A50_GR2_HI,selpar_A50_GR2_PH);
  init_bounded_number selpar_slope_GR2(selpar_slope_GR2_LO,selpar_slope_GR2_HI,selpar_slope_GR2_PH);
  init_bounded_number selpar_A502_GR2(selpar_A502_GR2_LO,selpar_A502_GR2_HI,selpar_A502_GR2_PH);
  init_bounded_number selpar_slope2_GR2(selpar_slope2_GR2_LO,selpar_slope2_GR2_HI,selpar_slope2_GR2_PH);

  init_bounded_number selpar_A50_GR3(selpar_A50_GR3_LO,selpar_A50_GR3_HI,selpar_A50_GR3_PH);
  init_bounded_number selpar_slope_GR3(selpar_slope_GR3_LO,selpar_slope_GR3_HI,selpar_slope_GR3_PH);
 
  vector selpar_A50_GR2_out(1,8);
  vector selpar_slope_GR2_out(1,8);
  vector selpar_A502_GR2_out(1,8);
  vector selpar_slope2_GR2_out(1,8);

  vector selpar_A50_GR3_out(1,8);
  vector selpar_slope_GR3_out(1,8);

  
//Discard selectivities
  matrix sel_cH_D(styr,endyr,1,nages);
  matrix sel_HB_D(styr,endyr,1,nages);  //GR uses HB discard selex, except for block 3
  matrix sel_GR_D(styr,endyr,1,nages); 
  vector sel_HB_D_block3(1,nages);
  vector sel_GR_D_block3(1,nages);
  
  init_bounded_number selpar_A50_HB2_D(selpar_A50_HB2_D_LO,selpar_A50_HB2_D_HI,selpar_A50_HB2_D_PH);
  init_bounded_number selpar_slope_HB2_D(selpar_slope_HB2_D_LO,selpar_slope_HB2_D_HI,selpar_slope_HB2_D_PH);
  init_bounded_number selpar_A502_HB2_D(selpar_A502_HB2_D_LO,selpar_A502_HB2_D_HI,selpar_A502_HB2_D_PH);
  init_bounded_number selpar_slope2_HB2_D(selpar_slope2_HB2_D_LO,selpar_slope2_HB2_D_HI,selpar_slope2_HB2_D_PH);
  
  init_bounded_number selpar_A50_HB3_D(selpar_A50_HB3_D_LO,selpar_A50_HB3_D_HI,selpar_A50_HB3_D_PH);
  init_bounded_number selpar_slope_HB3_D(selpar_slope_HB3_D_LO,selpar_slope_HB3_D_HI,selpar_slope_HB3_D_PH);
  init_bounded_number selpar_A502_HB3_D(selpar_A502_HB3_D_LO,selpar_A502_HB3_D_HI,selpar_A502_HB3_D_PH);
  init_bounded_number selpar_slope2_HB3_D(selpar_slope2_HB3_D_LO,selpar_slope2_HB3_D_HI,selpar_slope2_HB3_D_PH);

  init_bounded_number selpar_A50_GR3_D(selpar_A50_GR3_D_LO,selpar_A50_GR3_D_HI,selpar_A50_GR3_D_PH);
  init_bounded_number selpar_slope_GR3_D(selpar_slope_GR3_D_LO,selpar_slope_GR3_D_HI,selpar_slope_GR3_D_PH);
  init_bounded_number selpar_A502_GR3_D(selpar_A502_GR3_D_LO,selpar_A502_GR3_D_HI,selpar_A502_GR3_D_PH);
  init_bounded_number selpar_slope2_GR3_D(selpar_slope2_GR3_D_LO,selpar_slope2_GR3_D_HI,selpar_slope2_GR3_D_PH);
  
  init_bounded_number selpar_A50_cH2_D(selpar_A50_cH2_D_LO,selpar_A50_cH2_D_HI,selpar_A50_cH2_D_PH);
  init_bounded_number selpar_slope_cH2_D(selpar_slope_cH2_D_LO,selpar_slope_cH2_D_HI,selpar_slope_cH2_D_PH);
  init_bounded_number selpar_A502_cH2_D(selpar_A502_cH2_D_LO,selpar_A502_cH2_D_HI,selpar_A502_cH2_D_PH);
  init_bounded_number selpar_slope2_cH2_D(selpar_slope2_cH2_D_LO,selpar_slope2_cH2_D_HI,selpar_slope2_cH2_D_PH);
  
  init_bounded_number selpar_A50_cH3_D(selpar_A50_cH3_D_LO,selpar_A50_cH3_D_HI,selpar_A50_cH3_D_PH);
  init_bounded_number selpar_slope_cH3_D(selpar_slope_cH3_D_LO,selpar_slope_cH3_D_HI,selpar_slope_cH3_D_PH);
   
  vector selpar_A50_HB2_D_out(1,8);
  vector selpar_slope_HB2_D_out(1,8);
  vector selpar_A502_HB2_D_out(1,8);
  vector selpar_slope2_HB2_D_out(1,8);
  
  vector selpar_A50_HB3_D_out(1,8);
  vector selpar_slope_HB3_D_out(1,8);
  vector selpar_A502_HB3_D_out(1,8);
  vector selpar_slope2_HB3_D_out(1,8);

  vector selpar_A50_GR3_D_out(1,8);
  vector selpar_slope_GR3_D_out(1,8);
  vector selpar_A502_GR3_D_out(1,8);
  vector selpar_slope2_GR3_D_out(1,8);
  
  vector selpar_A50_cH2_D_out(1,8);
  vector selpar_slope_cH2_D_out(1,8);
  vector selpar_A502_cH2_D_out(1,8);
  vector selpar_slope2_cH2_D_out(1,8);
  
  vector selpar_A50_cH3_D_out(1,8);
  vector selpar_slope_cH3_D_out(1,8);
     
//Index selectivity (CVT and VID)  
  matrix sel_CVT(styr,endyr,1,nages);
  matrix sel_VID(styr,endyr,1,nages); 
  vector sel_CVT_vec(1,nages);
  vector sel_VID_vec(1,nages);
  
  init_bounded_number selpar_A50_CVT(selpar_A50_CVT_LO,selpar_A50_CVT_HI,selpar_A50_CVT_PH);
  init_bounded_number selpar_slope_CVT(selpar_slope_CVT_LO,selpar_slope_CVT_HI,selpar_slope_CVT_PH);
  init_bounded_number selpar_A502_CVT(selpar_A502_CVT_LO,selpar_A502_CVT_HI,selpar_A502_CVT_PH);
  init_bounded_number selpar_slope2_CVT(selpar_slope2_CVT_LO,selpar_slope2_CVT_HI,selpar_slope2_CVT_PH);
   
  vector selpar_A50_CVT_out(1,8);
  vector selpar_slope_CVT_out(1,8);
  vector selpar_A502_CVT_out(1,8);
  vector selpar_slope2_CVT_out(1,8);
  number selpar_switch_VID;
  
       
//Weighted total selectivity--------------------------------------------  
  //effort-weighted, recent selectivities
  vector sel_wgted_L(1,nages);  //toward landings 
  vector sel_wgted_D(1,nages);  //toward discards    
  vector sel_wgted_tot(1,nages);//toward Z, landings plus deads discards

//-----------------------------------------------------------------------------------------------------------------------------------------------
//-------CPUE Predictions--------------------------------
  vector pred_cH_cpue(styr_cH_cpue,endyr_cH_cpue);                  //predicted cH index (weight fish per effort)
  matrix N_cH(styr_cH_cpue,endyr_cH_cpue,1,nages);                  //used to compute cH index
  vector pred_HB_cpue(styr_HB_cpue,endyr_HB_cpue);                  //predicted HB index (number fish per effort)
  matrix N_HB(styr_HB_cpue,endyr_HB_cpue,1,nages);                  //used to compute HB index
  vector pred_HB_D_cpue(styr_HB_D_cpue,endyr_HB_D_cpue);            //predicted HB disc index (number fish per effort)
  matrix N_HB_D(styr_HB_D_cpue,endyr_HB_D_cpue,1,nages);            //used to compute HB disc index
  vector pred_CVT_cpue(styr_CVT_cpue,endyr_CVT_cpue);               //predicted SERFS CVT index (number fish per effort)
  matrix N_CVT(styr_CVT_cpue,endyr_CVT_cpue,1,nages);               //used to compute CVT index
  vector pred_VID_cpue(styr_VID_cpue,endyr_VID_cpue);               //predicted SERFS VID index (number fish per effort)
  matrix N_VID(styr_VID_cpue,endyr_VID_cpue,1,nages);               //used to compute VID index
  
//---Catchability (CPUE q's)----------------------------------------------------------
  init_bounded_number log_q_cH(log_q_cH_LO,log_q_cH_HI,log_q_cH_PH);
  init_bounded_number log_q_HB(log_q_HB_LO,log_q_HB_HI,log_q_HB_PH);
  init_bounded_number log_q_HB_D(log_q_HB_D_LO,log_q_HB_D_HI,log_q_HB_D_PH);
  init_bounded_number log_q_CVT(log_q_CVT_LO,log_q_CVT_HI,log_q_CVT_PH);  
  init_bounded_number log_q_VID(log_q_VID_LO,log_q_VID_HI,log_q_VID_PH); 
  vector log_q_cH_out(1,8);
  vector log_q_HB_out(1,8);
  vector log_q_HB_D_out(1,8);
  vector log_q_CVT_out(1,8);  
  vector log_q_VID_out(1,8); 
  
  number q_rate;
  vector q_rate_fcn_cH(styr_cH_cpue,endyr_cH_cpue);         //increase due to technology creep (saturates in 2003) 
  vector q_rate_fcn_HB(styr_HB_cpue,endyr_HB_cpue);         //increase due to technology creep (saturates in 2003) 
  vector q_rate_fcn_HB_D(styr_HB_D_cpue,endyr_HB_D_cpue);         //increase due to technology creep (saturates in 2003) 
  
//  init_bounded_number q_DD_beta(0.1,0.9,set_q_DD_phase);    //not estimated so commented out and declared as number (below)
  number q_DD_beta;
  vector q_DD_fcn(styr,endyr);    //density dependent function as a multiple of q (scaled a la Katsukawa and Matsuda. 2003)
  number B0_q_DD;                 //B0 of ages q_DD_age plus
  vector B_q_DD(styr,endyr);      //annual biomass of ages q_DD_age plus

//Fishery dependent random walk catchability
 vector q_RW_log_dev_cH(styr_cH_cpue,endyr_cH_cpue-1); 
 vector q_RW_log_dev_HB(styr_HB_cpue,endyr_HB_cpue-1); 
 vector q_RW_log_dev_HB_D(styr_HB_D_cpue,endyr_HB_D_cpue-1); 
 vector q_RW_log_dev_CVT(styr_CVT_cpue,endyr_CVT_cpue-1); 
 vector q_RW_log_dev_VID(styr_VID_cpue,endyr_VID_cpue-1); 

//Fishery dependent catchability over time, may be constant
 vector q_cH(styr_cH_cpue,endyr_cH_cpue); 
 vector q_HB(styr_HB_cpue,endyr_HB_cpue); 
 vector q_HB_D(styr_HB_D_cpue,endyr_HB_D_cpue); 
 vector q_CVT(styr_CVT_cpue,endyr_CVT_cpue); 
 vector q_VID(styr_VID_cpue,endyr_VID_cpue); 

//----------------------------------------------------------------------------------------------------------------------------------------------- 
//---Landings in numbers (total or 1000 fish) and in wgt (whole klb)--------------------------------------------------
  matrix L_cH_num(styr,endyr,1,nages);   //landings (numbers) at age
  matrix L_cH_klb(styr,endyr,1,nages);   //landings (1000 lb whole weight) at age    
  vector pred_cH_L_knum(styr,endyr);     //yearly landings in 1000 fish summed over ages  
  vector pred_cH_L_klb(styr,endyr);      //yearly landings in 1000 lb whole summed over ages

  matrix L_HB_num(styr,endyr,1,nages);   //landings (numbers) at age
  matrix L_HB_klb(styr,endyr,1,nages);   //landings (1000 lb whole weight) at age
  vector pred_HB_L_knum(styr,endyr);     //yearly landings in 1000 fish summed over ages
  vector pred_HB_L_klb(styr,endyr);      //yearly landings in 1000 lb whole summed over ages

  matrix L_GR_num(styr,endyr,1,nages);   //landings (numbers) at age
  matrix L_GR_klb(styr,endyr,1,nages);   //landings (1000 lb whole weight) at age
  vector pred_GR_L_knum(styr,endyr);     //yearly landings in 1000 fish summed over ages
  vector pred_GR_L_klb(styr,endyr);      //yearly landings in 1000 lb whole summed over ages

  matrix L_total_num(styr,endyr,1,nages);//total landings in number at age
  matrix L_total_klb(styr,endyr,1,nages);//landings in klb whole wgt at age 
  vector L_total_knum_yr(styr,endyr);    //total landings in 1000 fish by yr summed over ages  
  vector L_total_klb_yr(styr,endyr);     //total landings (klb whole wgt) by yr summed over ages
  
//---Discards (number dead fish) --------------------------------------------------
  matrix D_cH_num(styr,endyr,1,nages);             //discards (numbers) at age
  matrix D_cH_klb(styr,endyr,1,nages);             //discards (1000 lb whole) at age
  vector pred_cH_D_knum(styr_cH_D,endyr_cH_D);     //yearly dead discards summed over ages
  vector obs_cH_D(styr_cH_D,endyr_cH_D);           //observed releases multiplied by discard mortality
  vector pred_cH_D_klb(styr_cH_D,endyr_cH_D);      //yearly dead discards in klb whole wgt summed over ages
  
  matrix D_HB_num(styr,endyr,1,nages);             //discards (numbers) at age
  matrix D_HB_klb(styr,endyr,1,nages);             //discards (1000 lb) at age
  vector pred_HB_D_knum(styr_HB_D,endyr_HB_D);     //yearly dead discards summed over ages
  vector obs_HB_D(styr_HB_D,endyr_HB_D);           //observed releases multiplied by discard mortality
  vector pred_HB_D_klb(styr_HB_D,endyr_HB_D);      //yearly dead discards in klb whole wgt summed over ages

  matrix D_GR_num(styr,endyr,1,nages);             //discards (numbers) at age
  matrix D_GR_klb(styr,endyr,1,nages);             //discards (1000 lb) at age
  vector pred_GR_D_knum(styr_GR_D,endyr_GR_D);     //yearly dead discards summed over ages
  vector obs_GR_D(styr_GR_D,endyr_GR_D);           //observed releases multiplied by discard mortality
  vector pred_GR_D_klb(styr_GR_D,endyr_GR_D);      //yearly dead discards in klb whole wgt summed over ages
  
  matrix D_total_num(styr,endyr,1,nages);          //total discards in number at age
  matrix D_total_klb(styr,endyr,1,nages);          //discards in klb wgt at age 
  vector D_total_knum_yr(styr,endyr);              //total discards in 1000 fish by yr summed over ages  
  vector D_total_klb_yr(styr,endyr);               //total discards (klb whole wgt) by yr summed over ages

  number Dmort_cH1;
  number Dmort_HB1;
  number Dmort_GR1;  
  number Dmort_cH2;
  number Dmort_HB2;
  number Dmort_GR2;
  number Dmort_cH3;
  number Dmort_HB3;
  number Dmort_GR3; 

////---MSY calcs----------------------------------------------------------------------------
  number F_cH_prop;       //proportion of F_sum attributable to cH, last X=selpar_n_yrs_wgted yrs
  number F_HB_prop;       //proportion of F_sum attributable to HB, last X=selpar_n_yrs_wgted yrs 
  number F_GR_prop;       //proportion of F_sum attributable to GR, last X=selpar_n_yrs_wgted yrs
  number F_cH_D_prop;     //proportion of F_sum attributable to cH discards, last X=selpar_n_yrs_wgted yrs
  number F_HB_D_prop;     //proportion of F_sum attributable to HB, last X=selpar_n_yrs_wgted yrs
  number F_GR_D_prop;     //proportion of F_sum attributable to GR, last X=selpar_n_yrs_wgted yrs
  
  number F_init_cH_prop;  //proportion of F_init attributable to cH, first X yrs, No diving or discards in initial yrs
  number F_init_HB_prop;  //proportion of F_init attributable to HB, first X yrs
  number F_init_GR_prop;  //proportion of F_init attributable to GR, first X yrs
  
  number F_temp_sum;      //sum of geom mean Fsum's in last X yrs, used to compute F_fishery_prop

  vector F_end(1,nages);
  vector F_end_L(1,nages);  
  vector F_end_D(1,nages);    
  number F_end_apex;
  
  number SSB_msy_out;           //SSB (total mature biomass) at msy
  number F_msy_out;             //F at msy
  number msy_klb_out;           //max sustainable yield (1000 lb whole wgt)
  number msy_knum_out;          //max sustainable yield (1000 fish)  
  number D_msy_klb_out;         //discards associated with msy (1000 lb whole wgt)
  number D_msy_knum_out;        //discards associated with msy (1000 fish)  
  number B_msy_out;             //total biomass at MSY 
  number R_msy_out;             //equilibrium recruitment at F=Fmsy
  number spr_msy_out;           //spr at F=Fmsy

  number F20_dum;				//intermediate calculation for F20
  number F30_dum;				//intermediate calculation for F30
  number F40_dum;				//intermediate calculation for F40
  number F20_out;              	//F20
  number F30_out;              	//F30
  number F40_out;              	//F40
  number SSB_F30_out;  
  number B_F30_out;
  number R_F30_out;
  number L_F30_knum_out;
  number L_F30_klb_out;
  number D_F30_knum_out;
  number D_F30_klb_out;       
  number rec_mean;  			//arithemetic average recruitment used in SPR-related quantities  

  vector N_age_msy(1,nages);         //numbers at age for MSY calculations: beginning of yr
  vector N_age_msy_spawn(1,nages);   //numbers at age for MSY calculations: time of peak spawning  
  vector L_age_msy(1,nages);         //landings at age for MSY calculations
  vector D_age_msy(1,nages);         //discard mortality (dead discards) at age for MSY calculations
  vector Z_age_msy(1,nages);         //total mortality at age for MSY calculations
  vector F_L_age_msy(1,nages);       //fishing mortality landings (not discards) at age for MSY calculations
  vector F_D_age_msy(1,nages);       //fishing mortality of discards at age for MSY calculations
  vector F_msy(1,n_iter_msy);        //values of full F to be used in equilibrium calculations
  vector spr_msy(1,n_iter_msy);      //reproductive capacity-per-recruit values corresponding to F values in F_msy
  vector R_eq(1,n_iter_msy);         //equilibrium recruitment values corresponding to F values in F_msy
  vector L_eq_klb(1,n_iter_msy);     //equilibrium landings(klb whole wgt) values corresponding to F values in F_msy
  vector L_eq_knum(1,n_iter_msy);    //equilibrium landings(1000 fish) values corresponding to F values in F_msy
  vector D_eq_klb(1,n_iter_msy);     //equilibrium discards(klb whole wgt) values corresponding to F values in F_msy
  vector D_eq_knum(1,n_iter_msy);    //equilibrium discards(1000 fish) values corresponding to F values in F_msy
  vector SSB_eq(1,n_iter_msy);       //equilibrium reproductive capacity values corresponding to F values in F_msy
  vector B_eq(1,n_iter_msy);         //equilibrium biomass values corresponding to F values in F_msy
  
  vector FdF_msy(styr,endyr);
  vector FdF30(styr,endyr);
  vector SdSSB_msy(styr,endyr);	 
  number SdSSB_msy_end;
  number FdF_msy_end;
  number FdF_msy_end_mean;           //geometric mean of last X yrs  
  vector SdSSB_F30(styr,endyr);	 
  vector Sdmsst_F30(styr,endyr);	 
  number SdSSB_F30_end;
  number Sdmsst_F30_end;
  number FdF30_end_mean;             //geometric mean of last selpar_n_yrs_wgted yrs  
  number Fend_mean_temp;			 //intermediate calc for geometric mean of last selpar_n_yrs_wgted yrs
  number Fend_mean;					 //geometric mean of last selpar_n_yrs_wgted yrs
  vector L_age_F30(1,nages);         //landings at age for F30 calculations
  vector D_age_F30(1,nages);         //discard mortality (dead discards) at age for F30 calculations
  
  vector wgt_wgted_L_klb(1,nages);   //fishery-weighted average weight at age of landings in whole weight
  vector wgt_wgted_D_klb(1,nages);   //fishery-weighted average weight at age of discards in whole weight  
  number wgt_wgted_L_denom;          //used in intermediate calculations
  number wgt_wgted_D_denom;          //used in intermediate calculations

  number iter_inc_msy;               //increments used to compute msy, equals 1/(n_iter_msy-1)
 
////--------Exploitation rate------------------------------------------------------------------
  vector E_total_wgt(styr,endyr); //exploitation in units of biomass (landings+dead discards)/standing biomass     
  vector E_L_wgt(styr,endyr);
  vector E_D_wgt(styr,endyr);
  vector E_total_num(styr,endyr);  //exploitation in units of numbers (landings+dead discards)/standing abundance  
  vector E_L_num(styr,endyr);
  vector E_D_num(styr,endyr);
  vector E_eq_wgt(1,n_iter_msy);     //equilibrium exploitation rate (in weight) values corresponding to F values in F_msy
  vector E_eq_num(1,n_iter_msy);    //equilibrium exploitaiton rate (in numbers) values corresponding to F values in F_msy
  number E_msy_wgt;
  number E_msy_num;
  number E_F30_wgt;
  number E_F30_num;
  vector EdEmsy_wgt(styr,endyr);    //Time series of exploitation rate relative to that at MSY, in weight
  vector EdEmsy_num(styr,endyr);    //Time series of exploitation rate relative to that at MSY, in numbers
  number Eend_mean_temp_wgt;		 //intermediate calc for geometric mean of last selpar_n_yrs_wgted yrs
  number E_wgt_end_mean;			 //geometric mean of last selpar_n_yrs_wgted yrs
  number Eend_mean_temp_num;		 //intermediate calc for geometric mean of last selpar_n_yrs_wgted yrs
  number E_num_end_mean;			 //geometric mean of last selpar_n_yrs_wgted yrs
  number EdEmsy_wgt_end_mean;        //terminal status of exploitation rate, in weight
  number EdEmsy_num_end_mean;        //termainal status of exploitation rate, in numbers 
  vector EdEF30_wgt(styr,endyr);     //Time series of exploitation rate relative to that at E.F30, in weight
  vector EdEF30_num(styr,endyr);     //Time series of exploitation rate relative to that at E.F30, in numbers
  number EdEF30_wgt_end_mean;        //terminal status of exploitation rate, in weight
  number EdEF30_num_end_mean;        //termainal status of exploitation rate, in numbers

  ////--------Mortality------------------------------------------------------------------

  vector M(1,nages);                         //age-dependent natural mortality
  init_bounded_number M_constant(M_constant_LO,M_constant_HI,M_constant_PH);                         //age-indpendent: used only for MSST
  vector M_constant_out(1,8);
  number smsy2msst;                           //scales Smsy to get msst using (1-M). Used only in output.
  number smsy2msst75;                         //scales Smsy to get msst using 75%. Used only in output.  
  
  matrix F(styr,endyr,1,nages);
  vector Fsum(styr,endyr);                   //Full fishing mortality rate by year
  vector Fapex(styr,endyr);                  //Max across ages, fishing mortality rate by year (may differ from Fsum bc of dome-shaped sel 
  matrix Z(styr,endyr,1,nages);

  init_bounded_number log_avg_F_cH(log_avg_F_cH_LO,log_avg_F_cH_HI,log_avg_F_cH_PH);
  vector log_avg_F_cH_out(1,8);  
  init_bounded_dev_vector log_F_dev_cH(styr_cH_L,endyr_cH_L,log_F_dev_cH_LO,log_F_dev_cH_HI,log_F_dev_cH_PH);
  vector log_F_dev_cH_out(styr_cH_L,endyr_cH_L);
  matrix F_cH(styr,endyr,1,nages);
  vector F_cH_out(styr,endyr);        //used for intermediate calculations in fcn get_mortality
  number log_F_dev_init_cH;
  number log_F_dev_end_cH; 

  init_bounded_number log_avg_F_HB(log_avg_F_HB_LO,log_avg_F_HB_HI,log_avg_F_HB_PH);
  vector log_avg_F_HB_out(1,8); 
  init_bounded_dev_vector log_F_dev_HB(styr_HB_L,endyr_HB_L,log_F_dev_HB_LO,log_F_dev_HB_HI,log_F_dev_HB_PH);    
  vector log_F_dev_HB_out(styr_HB_L,endyr_HB_L);
  matrix F_HB(styr,endyr,1,nages);
  vector F_HB_out(styr,endyr); //used for intermediate calculations in fcn get_mortality
  number log_F_dev_init_HB;    
  number log_F_dev_end_HB;  

  init_bounded_number log_avg_F_GR(log_avg_F_GR_LO,log_avg_F_GR_HI,log_avg_F_GR_PH);
  vector log_avg_F_GR_out(1,8); 
  init_bounded_dev_vector log_F_dev_GR(styr_GR_L,endyr_GR_L,log_F_dev_GR_LO,log_F_dev_GR_HI,log_F_dev_GR_PH);    
  vector log_F_dev_GR_out(styr_GR_L,endyr_GR_L);
  matrix F_GR(styr,endyr,1,nages);
  vector F_GR_out(styr,endyr); //used for intermediate calculations in fcn get_mortality
  number log_F_dev_init_GR;    
  number log_F_dev_end_GR;  

  init_bounded_number log_avg_F_cH_D(log_avg_F_cH_D_LO,log_avg_F_cH_D_HI,log_avg_F_cH_D_PH);
  vector log_avg_F_cH_D_out(1,8);  
  init_bounded_dev_vector log_F_dev_cH_D(styr_cH_D,endyr_cH_D,log_F_dev_cH_D_LO,log_F_dev_cH_D_HI,log_F_dev_cH_D_PH);
  vector log_F_dev_cH_D_out(styr_cH_D,endyr_cH_D);
  matrix F_cH_D(styr,endyr,1,nages);
  vector F_cH_D_out(styr,endyr);        //used for intermediate calculations in fcn get_mortality
  number log_F_dev_end_cH_D; 

  init_bounded_number log_avg_F_HB_D(log_avg_F_HB_D_LO,log_avg_F_HB_D_HI,log_avg_F_HB_D_PH);
  vector log_avg_F_HB_D_out(1,8);  
  init_bounded_dev_vector log_F_dev_HB_D(styr_HB_D,endyr_HB_D,log_F_dev_HB_D_LO,log_F_dev_HB_D_HI,log_F_dev_HB_D_PH);
  vector log_F_dev_HB_D_out(styr_HB_D,endyr_HB_D);
  matrix F_HB_D(styr,endyr,1,nages);
  vector F_HB_D_out(styr,endyr);        //used for intermediate calculations in fcn get_mortality
  number log_F_dev_end_HB_D; 

  init_bounded_number log_avg_F_GR_D(log_avg_F_GR_D_LO,log_avg_F_GR_D_HI,log_avg_F_GR_D_PH);
  vector log_avg_F_GR_D_out(1,8);  
  init_bounded_dev_vector log_F_dev_GR_D(styr_GR_D,endyr_GR_D,log_F_dev_GR_D_LO,log_F_dev_GR_D_HI,log_F_dev_GR_D_PH);
  vector log_F_dev_GR_D_out(styr_GR_D,endyr_GR_D);
  matrix F_GR_D(styr,endyr,1,nages);
  vector F_GR_D_out(styr,endyr);        //used for intermediate calculations in fcn get_mortality
  number log_F_dev_end_GR_D; 

  init_bounded_number F_init(F_init_LO,F_init_HI,F_init_PH); //scales early F for initialization
  vector F_init_out(1,8); 
  number F_init_denom;  //interim calculation

//---Per-recruit stuff----------------------------------------------------------------------------------
  vector N_age_spr(1,nages);         //numbers at age for SPR calculations: beginning of year
  vector N_age_spr_spawn(1,nages);   //numbers at age for SPR calculations: time of peak spawning  
  vector L_age_spr(1,nages);         //catch at age for SPR calculations
  vector Z_age_spr(1,nages);         //total mortality at age for SPR calculations
  vector spr_static(styr,endyr);     //vector of static SPR values by year
  vector F_L_age_spr(1,nages);       //fishing mortality of landings (not discards) at age for SPR calculations
  vector F_D_age_spr(1,nages);       //fishing mortality of discards at age for SPR calculations
  vector F_spr(1,n_iter_spr);        //values of full F to be used in per-recruit calculations
  vector spr_spr(1,n_iter_spr);      //reproductive capacity-per-recruit values corresponding to F values in F_spr
  vector spr_ratio(1,n_iter_spr);    //reproductive capacity-per-recruit relative to spr_F0 values corresponding to F values in F_spr
  vector L_spr(1,n_iter_spr);        //landings(lb)-per-recruit (ypr) values corresponding to F values in F_spr

  vector N_spr_F0(1,nages);          //Used to compute spr at F=0: at time of peak spawning
  vector N_bpr_F0(1,nages);          //Used to compute bpr at F=0: at start of year  
  vector N_spr_initial(1,nages);     //Initial spawners per recruit at age given initial F
  vector N_initial_eq(1,nages);      //Initial equilibrium abundance at age
  vector F_initial(1,nages);         //initial F at age
  vector Z_initial(1,nages);         //initial Z at age
  number spr_initial;                //initial spawners per recruit
  number spr_F0;                     //Spawning biomass per recruit at F=0
  number bpr_F0;                     //Biomass per recruit at F=0

  number iter_inc_spr;               //increments used to compute msy, equals max_F_spr_msy/(n_iter_spr-1)

////-------SDNR output-----------------------------------------------------------------------------
 
  number sdnr_lc_cH;
  number sdnr_lc_cH_D;
  number sdnr_lc_HB;
  number sdnr_lc_HB_D;  
  number sdnr_lc_CVT;  
  number sdnr_lc_GR;  
  number sdnr_lc_GR_D;
  
  number sdnr_ac_cH;
  number sdnr_ac_HB;
  number sdnr_ac_CVT;  
  number sdnr_ac_GR;  
    
  number sdnr_I_cH;
  number sdnr_I_HB;
  number sdnr_I_HB_D;
  number sdnr_I_CVT;  
  number sdnr_I_VID;
  
    
////-------Objective function components-----------------------------------------------------------------------------
  number w_L;
  number w_D;
  
  number w_I_cH;
  number w_I_HB;
  number w_I_HB_D;
  number w_I_CVT; 
  number w_I_VID; 
  number w_I_CVT_mult; 
  number w_I_VID_mult; 
  
  number w_lc_cH;
  number w_lc_cH_D; 
  number w_lc_HB_D;  
  number w_lc_GR_D;
  
  number w_ac_cH; 
  number w_ac_HB;
  number w_ac_CVT;  
  number w_ac_GR;  
  
  number w_Nage_init;  
  number w_rec;
  number w_rec_early;
  number w_rec_end;
  number w_fullF;  
  number w_Ftune;

  number f_cH_L; 
  number f_HB_L; 
  number f_GR_L; 

  number f_cH_D; 
  number f_HB_D; 
  number f_GR_D; 

  number f_cH_cpue;
  number f_HB_cpue;
  number f_HB_D_cpue;
  number f_CVT_cpue;  
  number f_VID_cpue;  
   
  number f_cH_lenc;
  number f_cH_D_lenc;  
  number f_HB_lenc;
  number f_HB_D_lenc;  
  number f_CVT_lenc;  
  number f_GR_lenc;  
  number f_GR_D_lenc; 

  number f_cH_agec;
  number f_HB_agec;
  number f_CVT_agec;  
  number f_GR_agec;  

//  Penalties and constraints. Not all are used.
  number f_Nage_init;              //weight on log devs to estimate initial abundance (excluding first age)
  number f_rec_dev;                //weight on recruitment deviations to fit S-R curve
  number f_rec_dev_early;          //extra weight on deviations in first recruitment stanza
  number f_rec_dev_end;            //extra weight on deviations in ending recruitment stanza
  number f_fullF_constraint;       //penalty for Fapex>X
  number f_Ftune;                  //penalty for tuning F in Ftune yr.  Not applied in final optimization phase.
  number f_priors;                 //prior information on parameters
  
  //init_number xdum;
  objective_function_value fval;
  number fval_data;
  number grad_max;
  
//--Dummy variables ----
  number denom;                   //denominator used in some calculations
  number numer;                   //numerator used in some calculations
   
//##--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>
//##--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>
//INITIALIZATION_SECTION
//##--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>
//##--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>
GLOBALS_SECTION
  #include "admodel.h"          // Include AD class definitions
  #include "admb2r.cpp"    // Include S-compatible output functions (needs preceding)
  #include <time.h>
	time_t start,finish;
	long hour,minute,second;	
	double elapsed_time;
	
//##--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>
RUNTIME_SECTION
 maximum_function_evaluations 1000, 2000, 3000, 5000; 5000; 5000; 10000; 
 convergence_criteria 1e-2, 1e-2, 1e-3, 1e-3; 1e-4; 1e-4; 1e-4; 
 
//##--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>
//##--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>
PRELIMINARY_CALCS_SECTION

// Set values of fixed parameters or set initial guess of estimated parameters
  Dmort_cH1=set_Dmort_cH1; Dmort_HB1=set_Dmort_HB1; Dmort_GR1=set_Dmort_GR1;
  Dmort_cH2=set_Dmort_cH2; Dmort_HB2=set_Dmort_HB2; Dmort_GR2=set_Dmort_GR2;
  Dmort_cH3=set_Dmort_cH3; Dmort_HB3=set_Dmort_HB3; Dmort_GR3=set_Dmort_GR3;
  
  for(iyear=styr_cH_D; iyear<=endyr_cH_Dmort1; iyear++)
	{obs_cH_D(iyear)=Dmort_cH1*obs_cH_released(iyear);
	}
  for(iyear=endyr_cH_Dmort1+1; iyear<=endyr_cH_Dmort2; iyear++)
	{obs_cH_D(iyear)=Dmort_cH2*obs_cH_released(iyear);
	}
  for(iyear=endyr_cH_Dmort2+1; iyear<=endyr_cH_D; iyear++)
	{obs_cH_D(iyear)=Dmort_cH3*obs_cH_released(iyear);
	}
	
  for(iyear=styr_HB_D; iyear<=endyr_HB_Dmort1; iyear++)
	{obs_HB_D(iyear)=Dmort_HB1*obs_HB_released(iyear);
	}
  for(iyear=endyr_HB_Dmort1+1; iyear<=endyr_HB_Dmort2; iyear++)
	{obs_HB_D(iyear)=Dmort_HB2*obs_HB_released(iyear);
	}
  for(iyear=endyr_HB_Dmort2+1; iyear<=endyr_HB_D; iyear++)
	{obs_HB_D(iyear)=Dmort_HB3*obs_HB_released(iyear);
	}
	
  for(iyear=styr_GR_D; iyear<=endyr_GR_Dmort1; iyear++)
	{obs_GR_D(iyear)=Dmort_GR1*obs_GR_released(iyear);
	}
  for(iyear=endyr_GR_Dmort1+1; iyear<=endyr_GR_Dmort2; iyear++)
	{obs_GR_D(iyear)=Dmort_GR2*obs_GR_released(iyear);
	}
  for(iyear=endyr_GR_Dmort2+1; iyear<=endyr_GR_D; iyear++)
	{obs_GR_D(iyear)=Dmort_GR3*obs_GR_released(iyear);
	}
	
  //Population		
  Linf=set_Linf(1);
  K=set_K(1);
  t0=set_t0(1);
  len_cv_val=set_len_cv(1);
  //All fisheries
  Linf_L=set_Linf_L(1);  
  K_L=set_K_L(1);
  t0_L=set_t0_L(1);
  len_cv_val_L=set_len_cv_L(1);
  //20" size limit
  Linf_20=set_Linf_20(1);  
  K_20=set_K_20(1);
  t0_20=set_t0_20(1);
  len_cv_val_20=set_len_cv_20(1);
  
  M=set_M; 
  M_constant=set_M_constant(1);
  smsy2msst=1.0-M_constant;
  smsy2msst75=0.75;  
  
  log_R0=set_log_R0(1);
  steep=set_steep(1);
  R_autocorr=set_R_autocorr(1);
  rec_sigma=set_rec_sigma(1);
  
  log_dm_cH_lc=set_log_dm_cH_lc(1);
  log_dm_cH_D_lc=set_log_dm_cH_D_lc(1);
  log_dm_HB_D_lc=set_log_dm_HB_D_lc(1);
  log_dm_GR_D_lc=set_log_dm_GR_D_lc(1);
  log_dm_cH_ac=set_log_dm_cH_ac(1);
  log_dm_HB_ac=set_log_dm_HB_ac(1);
  log_dm_CVT_ac=set_log_dm_CVT_ac(1);
  log_dm_GR_ac=set_log_dm_GR_ac(1);  
  
  log_q_cH=set_log_q_cH(1);
  log_q_HB=set_log_q_HB(1);
  log_q_HB_D=set_log_q_HB_D(1);
  log_q_CVT=set_log_q_CVT(1);
  log_q_VID=set_log_q_VID(1);
  
  q_rate=set_q_rate;
  q_rate_fcn_cH=1.0;   
  q_rate_fcn_HB=1.0;   
  q_rate_fcn_HB_D=1.0; 
  q_DD_beta=set_q_DD_beta;
  q_DD_fcn=1.0;

  q_RW_log_dev_cH.initialize(); 
  q_RW_log_dev_HB.initialize(); 
  q_RW_log_dev_HB_D.initialize();
  q_RW_log_dev_CVT.initialize();
  q_RW_log_dev_VID.initialize();
  
   if (set_q_rate_phase<0 & q_rate!=0.0)
  {
    for (iyear=styr_cH_cpue; iyear<=endyr_cH_cpue; iyear++)
      {   if (iyear>styr_cH_cpue & iyear <=2003) 
          {//q_rate_fcn_cH(iyear)=(1.0+q_rate)*q_rate_fcn_cH(iyear-1); //compound
             q_rate_fcn_cH(iyear)=(1.0+(iyear-styr_cH_cpue)*q_rate)*q_rate_fcn_cH(styr_cH_cpue);  //linear
          }
          if (iyear>2003) {q_rate_fcn_cH(iyear)=q_rate_fcn_cH(iyear-1);} 
      }   
    for (iyear=styr_HB_cpue; iyear<=endyr_HB_cpue; iyear++)
      {   if (iyear>styr_HB_cpue & iyear <=2003) 
          {//q_rate_fcn_HB(iyear)=(1.0+q_rate)*q_rate_fcn_HB(iyear-1); //compound
             q_rate_fcn_HB(iyear)=(1.0+(iyear-styr_HB_cpue)*q_rate)*q_rate_fcn_HB(styr_HB_cpue);  //linear
          }
          if (iyear>2003) {q_rate_fcn_HB(iyear)=q_rate_fcn_HB(iyear-1);} 
      }   
    for (iyear=styr_HB_D_cpue; iyear<=endyr_HB_D_cpue; iyear++)
      {   if (iyear>styr_HB_D_cpue & iyear <=2003) 
          {//q_rate_fcn_HB_D(iyear)=(1.0+q_rate)*q_rate_fcn_HB_D(iyear-1); //compound
             q_rate_fcn_HB_D(iyear)=(1.0+(iyear-styr_HB_D_cpue)*q_rate)*q_rate_fcn_HB_D(styr_HB_D_cpue);  //linear
          }
          if (iyear>2003) {q_rate_fcn_HB_D(iyear)=q_rate_fcn_HB_D(iyear-1);} 
      }   
  } //end q_rate conditional      

  w_L=set_w_L;
  w_D=set_w_D;
  
  w_I_cH=set_w_I_cH;
  w_I_HB=set_w_I_HB;
  w_I_HB_D=set_w_I_HB_D;
  w_I_CVT=set_w_I_CVT;
  w_I_VID=set_w_I_VID;
  w_I_CVT_mult=set_w_I_CVT_mult;
  w_I_VID_mult=set_w_I_VID_mult;
  
  w_lc_cH=set_w_lc_cH;
  w_lc_cH_D=set_w_lc_cH_D;  
  w_lc_HB_D=set_w_lc_HB_D;
  w_lc_GR_D=set_w_lc_GR_D;  
    
  w_ac_cH=set_w_ac_cH;
  w_ac_HB=set_w_ac_HB;
  w_ac_CVT=set_w_ac_CVT;  
  w_ac_GR=set_w_ac_GR;  
  
  w_Nage_init=set_w_Nage_init;
  w_rec=set_w_rec;
  w_rec_early=set_w_rec_early;
  w_rec_end=set_w_rec_end;
  w_fullF=set_w_fullF;
  w_Ftune=set_w_Ftune;

  F_init=set_F_init(1);

  log_avg_F_cH=set_log_avg_F_cH(1);
  log_avg_F_HB=set_log_avg_F_HB(1); 
  log_avg_F_GR=set_log_avg_F_GR(1); 
  log_avg_F_cH_D=set_log_avg_F_cH_D(1);
  log_avg_F_HB_D=set_log_avg_F_HB_D(1); 
  log_avg_F_GR_D=set_log_avg_F_GR_D(1); 
    
  log_F_dev_cH=set_log_F_dev_cH_vals;
  log_F_dev_HB=set_log_F_dev_HB_vals;
  log_F_dev_GR=set_log_F_dev_GR_vals;
  log_F_dev_cH_D=set_log_F_dev_cH_D_vals;
  log_F_dev_HB_D=set_log_F_dev_HB_D_vals;
  log_F_dev_GR_D=set_log_F_dev_GR_D_vals;
 
  selpar_A50_cH1=set_selpar_A50_cH1(1);
  selpar_slope_cH1=set_selpar_slope_cH1(1);
  selpar_A50_cH2=set_selpar_A50_cH2(1);
  selpar_slope_cH2=set_selpar_slope_cH2(1);
  selpar_A50_cH3=set_selpar_A50_cH3(1);
  selpar_slope_cH3=set_selpar_slope_cH3(1);

  selpar_A50_HB1=set_selpar_A50_HB1(1);
  selpar_slope_HB1=set_selpar_slope_HB1(1);
  selpar_A502_HB1=set_selpar_A502_HB1(1);
  selpar_slope2_HB1=set_selpar_slope2_HB1(1);
  selpar_A50_HB2=set_selpar_A50_HB2(1);
  selpar_slope_HB2=set_selpar_slope_HB2(1);
  selpar_A502_HB2=set_selpar_A502_HB2(1);
  selpar_slope2_HB2=set_selpar_slope2_HB2(1);
  selpar_A50_HB3=set_selpar_A50_HB3(1);
  selpar_slope_HB3=set_selpar_slope_HB3(1);
  selpar_A502_HB3=set_selpar_A502_HB3(1);
  selpar_slope2_HB3=set_selpar_slope2_HB3(1);
  
  selpar_A50_GR2=set_selpar_A50_GR2(1);
  selpar_slope_GR2=set_selpar_slope_GR2(1);
  selpar_A502_GR2=set_selpar_A502_GR2(1);
  selpar_slope2_GR2=set_selpar_slope2_GR2(1);

  selpar_A50_GR3=set_selpar_A50_GR3(1);
  selpar_slope_GR3=set_selpar_slope_GR3(1);
  
  selpar_A50_HB2_D=set_selpar_A50_HB2_D(1);
  selpar_slope_HB2_D=set_selpar_slope_HB2_D(1);
  selpar_A502_HB2_D=set_selpar_A502_HB2_D(1);
  selpar_slope2_HB2_D=set_selpar_slope2_HB2_D(1);
  
  selpar_A50_HB3_D=set_selpar_A50_HB3_D(1);
  selpar_slope_HB3_D=set_selpar_slope_HB3_D(1);
  selpar_A502_HB3_D=set_selpar_A502_HB3_D(1);
  selpar_slope2_HB3_D=set_selpar_slope2_HB3_D(1);

  selpar_A50_GR3_D=set_selpar_A50_GR3_D(1);
  selpar_slope_GR3_D=set_selpar_slope_GR3_D(1);
  selpar_A502_GR3_D=set_selpar_A502_GR3_D(1);
  selpar_slope2_GR3_D=set_selpar_slope2_GR3_D(1);  
 
  selpar_A50_cH2_D=set_selpar_A50_cH2_D(1);
  selpar_slope_cH2_D=set_selpar_slope_cH2_D(1);
  selpar_A502_cH2_D=set_selpar_A502_cH2_D(1);
  selpar_slope2_cH2_D=set_selpar_slope2_cH2_D(1);
  
  selpar_A50_cH3_D=set_selpar_A50_cH3_D(1);
  selpar_slope_cH3_D=set_selpar_slope_cH3_D(1);
    
  selpar_A50_CVT=set_selpar_A50_CVT(1);
  selpar_slope_CVT=set_selpar_slope_CVT(1);
  selpar_A502_CVT=set_selpar_A502_CVT(1);
  selpar_slope2_CVT=set_selpar_slope2_CVT(1);


 sqrt2pi=sqrt(2.*3.14159265);
 g2mt=0.000001;         //conversion of grams to metric tons
 g2kg=0.001;            //conversion of grams to kg 
 mt2klb=2.20462;        //conversion of metric tons to 1000 lb 
 mt2lb=mt2klb*1000.0;   //conversion of metric tons to lb
 g2klb=g2mt*mt2klb;     //conversion of grams to 1000 lb 
 dzero=0.00001;         
 huge_number=1.0e+10;   
 
 SSB_msy_out=0.0;

 iter_inc_msy=max_F_spr_msy/(n_iter_msy-1);
 iter_inc_spr=max_F_spr_msy/(n_iter_spr-1); 

 maturity_f=maturity_f_obs;
 
 prop_f=prop_f_obs;

 fecpar_a=set_fecpar_a;
 fecpar_b=set_fecpar_b;
 fecpar_c=set_fecpar_c;
 fecpar_thresh=set_fecpar_thresh;
 fecpar_min=set_fecpar_min;
 fecpar_batches=set_fecpar_batches;
 
//Fill in sample sizes of comps, possibly sampled in nonconsec yrs 
//Used primarily for output in R object   

      nsamp_cH_lenc_allyr=missing;
	  nsamp_cH_D_lenc_allyr=missing;  
      nsamp_HB_D_lenc_allyr=missing;  
	  nsamp_GR_D_lenc_allyr=missing; 
      nsamp_cH_agec_allyr=missing;
      nsamp_HB_agec_allyr=missing;
	  nsamp_CVT_agec_allyr=missing; 
	  nsamp_GR_agec_allyr=missing;  
      
      nfish_cH_lenc_allyr=missing;
	  nfish_cH_D_lenc_allyr=missing;  
      nfish_HB_D_lenc_allyr=missing; 
	  nfish_GR_D_lenc_allyr=missing;
      nfish_cH_agec_allyr=missing;
      nfish_HB_agec_allyr=missing;
	  nfish_CVT_agec_allyr=missing;  
	  nfish_GR_agec_allyr=missing;  
   
      for (iyear=1; iyear<=nyr_cH_lenc; iyear++)
         {if (nsamp_cH_lenc(iyear)>=minSS_cH_lenc)
           {nsamp_cH_lenc_allyr(yrs_cH_lenc(iyear))=nsamp_cH_lenc(iyear);
            nfish_cH_lenc_allyr(yrs_cH_lenc(iyear))=nfish_cH_lenc(iyear);}}
	  for (iyear=1; iyear<=nyr_cH_D_lenc; iyear++)                           
         {if (nsamp_cH_D_lenc(iyear)>=minSS_cH_D_lenc)
            {nsamp_cH_D_lenc_allyr(yrs_cH_D_lenc(iyear))=nsamp_cH_D_lenc(iyear);
             nfish_cH_D_lenc_allyr(yrs_cH_D_lenc(iyear))=nfish_cH_D_lenc(iyear);}}
      for (iyear=1; iyear<=nyr_HB_D_lenc; iyear++)                          
         {if (nsamp_HB_D_lenc(iyear)>=minSS_HB_D_lenc)
            {nsamp_HB_D_lenc_allyr(yrs_HB_D_lenc(iyear))=nsamp_HB_D_lenc(iyear);
             nfish_HB_D_lenc_allyr(yrs_HB_D_lenc(iyear))=nfish_HB_D_lenc(iyear);}}
	  for (iyear=1; iyear<=nyr_GR_D_lenc; iyear++)                          
         {if (nsamp_GR_D_lenc(iyear)>=minSS_GR_D_lenc)
            {nsamp_GR_D_lenc_allyr(yrs_GR_D_lenc(iyear))=nsamp_GR_D_lenc(iyear);
             nfish_GR_D_lenc_allyr(yrs_GR_D_lenc(iyear))=nfish_GR_D_lenc(iyear);}}
	  for (iyear=1; iyear<=nyr_cH_agec; iyear++)
         {if (nsamp_cH_agec(iyear)>=minSS_cH_agec)
           {nsamp_cH_agec_allyr(yrs_cH_agec(iyear))=nsamp_cH_agec(iyear);
            nfish_cH_agec_allyr(yrs_cH_agec(iyear))=nfish_cH_agec(iyear);}}
      for (iyear=1; iyear<=nyr_HB_agec; iyear++)
          {if (nsamp_HB_agec(iyear)>=minSS_HB_agec)
            {nsamp_HB_agec_allyr(yrs_HB_agec(iyear))=nsamp_HB_agec(iyear);
             nfish_HB_agec_allyr(yrs_HB_agec(iyear))=nfish_HB_agec(iyear);}} 
	  for (iyear=1; iyear<=nyr_CVT_agec; iyear++)  
          {if (nsamp_CVT_agec(iyear)>=minSS_CVT_agec)
            {nsamp_CVT_agec_allyr(yrs_CVT_agec(iyear))=nsamp_CVT_agec(iyear);
             nfish_CVT_agec_allyr(yrs_CVT_agec(iyear))=nfish_CVT_agec(iyear);}} 
      for (iyear=1; iyear<=nyr_GR_agec; iyear++)  
         {if (nsamp_GR_agec(iyear)>=minSS_GR_agec)
           {nsamp_GR_agec_allyr(yrs_GR_agec(iyear))=nsamp_GR_agec(iyear);
             nfish_GR_agec_allyr(yrs_GR_agec(iyear))=nfish_GR_agec(iyear);}}  

             
//fill in Fs for msy and per-recruit analyses
  F_msy(1)=0.0;  
  for (ff=2;ff<=n_iter_msy;ff++) {F_msy(ff)=F_msy(ff-1)+iter_inc_msy;}
  F_spr(1)=0.0;  
  for (ff=2;ff<=n_iter_spr;ff++) {F_spr(ff)=F_spr(ff-1)+iter_inc_spr;}


//fill in F's, Catch matrices, and log rec dev with zero's
  F_cH.initialize(); L_cH_num.initialize();
  F_HB.initialize(); L_HB_num.initialize();
  F_GR.initialize(); L_GR_num.initialize();
  F_cH_D.initialize(); D_cH_num.initialize();
  F_HB_D.initialize(); D_HB_num.initialize();
  F_GR_D.initialize(); D_GR_num.initialize();

  F_cH_out.initialize();
  F_HB_out.initialize();
  F_GR_out.initialize();
  F_cH_D_out.initialize();
  F_HB_D_out.initialize();
  F_GR_D_out.initialize();

      
  sel_cH.initialize();
  sel_HB.initialize();
  sel_GR.initialize();
  sel_cH_D.initialize();
  sel_HB_D.initialize();
  sel_GR_D.initialize();
  sel_CVT.initialize();
  sel_VID.initialize();
  
  sel_HB_block1.initialize();
  sel_HB_block2.initialize();
  sel_HB_block3.initialize();
  sel_GR_block1.initialize();
  sel_GR_block2.initialize();
  sel_GR_block3.initialize();
  sel_HB_D_block3.initialize();
  sel_GR_D_block3.initialize();  
  sel_CVT_vec.initialize();
  sel_VID_vec.initialize();
 
  log_rec_dev_output.initialize();  
  log_rec_dev=set_log_rec_dev_vals;
  log_Nage_dev_output.initialize();
  log_Nage_dev=set_log_Nage_dev_vals;
 
 
//##--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>
//##--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>
TOP_OF_MAIN_SECTION
  time(&start);
  arrmblsize=20000000;
  gradient_structure::set_MAX_NVAR_OFFSET(1600);
  gradient_structure::set_GRADSTACK_BUFFER_SIZE(2000000);
  gradient_structure::set_CMPDIF_BUFFER_SIZE(2000000);
  gradient_structure::set_NUM_DEPENDENT_VARIABLES(10000);

//>--><>--><>--><>--><>
//##--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>--><>
PROCEDURE_SECTION
  
 //cout<<"start"<<endl;
 
 //get_M_at_age(); //Needed only if M is estimated
 
 get_length_weight_at_age(); 
  //cout << "got length, weight, fecundity transitions" <<endl;
   get_reprod();
  //cout << "got repro stuff" << endl;
   get_length_at_age_dist(); 
  //cout<< "got predicted length at age distribution"<<endl;
   get_weight_at_age_landings();
  //cout<< "got weight at age of landings"<<endl; 
   get_spr_F0();
  //cout << "got F0 spr" << endl;
   get_selectivity(); 
  //cout << "got selectivity" << endl;
   get_mortality(); 
 // cout << "got mortalities" << endl;
   get_bias_corr(); 
  //cout<< "got recruitment bias correction" << endl;
   get_numbers_at_age(); 
  //cout << "got numbers at age" << endl;
   get_landings_numbers();
  //cout << "got landings in numbers" << endl;
   get_landings_wgt();
  //cout << "got landings in wgt" << endl;
   get_dead_discards(); 
  //cout << "got dead discards in num and wgt" << endl;
   get_catchability_fcns(); 
  //cout << "got catchability_fcns" << endl;
   get_indices();
  //cout << "got indices" << endl;
   get_length_comps();
  //cout<< "got length comps"<< endl;
   get_age_comps();
  //cout<< "got age comps"<< endl;
   evaluate_objective_function();
  //cout << "objective function calculations complete" << endl;
 
 
FUNCTION get_length_weight_at_age
	//population total length in mm
    //compute mean length (mm TL) and weight (whole) at age
    meanlen_TL=Linf*(1.0-mfexp(-K*(agebins-t0+0.5)));     
    wgt_kg=wgtpar_a*pow(meanlen_TL,wgtpar_b);             //whole wgt in kg 
    wgt_g=wgt_kg/g2kg;                                    //convert wgt in kg to weight in g    
    wgt_mt=wgt_g*g2mt;                                    //convert weight in g to weight in mt
    wgt_klb=mt2klb*wgt_mt;                                //1000 lb of whole wgt
    wgt_lb=mt2lb*wgt_mt;                                  //lb of whole wgt
    fecundity=fecpar_a+fecpar_b*pow(meanlen_TL,fecpar_c); //relationship provided by Wyanski et al.  
    for (iage=1; iage<=nages; iage++)
		{ if(meanlen_TL(iage)<fecpar_thresh){fecundity(iage)=fecpar_min;}	
          else {break;}			
		}
	fecundity=fecundity/fecpar_scale; //batch fecundity of a mature female at age in units of fecpar_scale
	
	//20 in size limit
	meanlen_TL_20=Linf_20*(1.0-mfexp(-K_20*(agebins-t0_20+0.5)));    //Landings total length in mm
    wgt_kg_20=wgtpar_a*pow(meanlen_TL_20,wgtpar_b);             //whole wgt in kg 
    wgt_g_20=wgt_kg_20/g2kg;                                    //convert wgt in kg to weight in g    
    wgt_mt_20=wgt_g_20*g2mt;                                    //convert weight in g to weight in mt
    wgt_klb_20=mt2klb*wgt_mt_20;                               //1000 lb of whole wgt
    wgt_lb_20=mt2lb*wgt_mt_20;                                 //1000 lb of whole wgt
	
	//All fisheries
	meanlen_TL_L=Linf_L*(1.0-mfexp(-K_L*(agebins-t0_L+0.5)));    //Landings total length in mm
    wgt_kg_L=wgtpar_a*pow(meanlen_TL_L,wgtpar_b);             //whole wgt in kg 
    wgt_g_L=wgt_kg_L/g2kg;                                    //convert wgt in kg to weight in g    
    wgt_mt_L=wgt_g_L*g2mt;                                    //convert weight in g to weight in mt
    wgt_klb_L=mt2klb*wgt_mt_L;                               //1000 lb of whole wgt
    wgt_lb_L=mt2lb*wgt_mt_L;                                 //1000 lb of whole wgt

FUNCTION get_reprod 
 
	reprod=elem_prod(elem_prod(elem_prod(prop_f,maturity_f),fecundity),fecpar_batches);
 
FUNCTION get_length_at_age_dist
  //compute matrix of length at age, based on the normal distribution
    //population
	for (iage=1;iage<=nages;iage++)
   {len_cv(iage)=len_cv_val;
    len_sd(iage)=meanlen_TL(iage)*len_cv(iage);
	zscore_lzero=(0.0-meanlen_TL(iage))/len_sd(iage); 
	cprob_lzero=cumd_norm(zscore_lzero);
	
	//20 in size limit
	//len_cv_20(iage)=mfexp(log_len_cv_20+log_len_cv_dev_20(iage));
    len_cv_20(iage)=len_cv_val_20;
    len_sd_20(iage)=meanlen_TL_20(iage)*len_cv_20(iage);
	zscore_lzero_20=(0.0-meanlen_TL_20(iage))/len_sd_20(iage); 
	cprob_lzero_20=cumd_norm(zscore_lzero_20);
	
	//All fishery dependent
	//len_cv_L(iage)=mfexp(log_len_cv_L+log_len_cv_dev_L(iage));
    len_cv_L(iage)=len_cv_val_L;
    len_sd_L(iage)=meanlen_TL_L(iage)*len_cv_L(iage);
	zscore_lzero_L=(0.0-meanlen_TL_L(iage))/len_sd_L(iage); 
	cprob_lzero_L=cumd_norm(zscore_lzero_L);
    
    //first length bin
	//population
     zscore_len=((lenbins(1)+0.5*lenbins_width)-meanlen_TL(iage)) / len_sd(iage);
    cprob_lenvec(1)=cumd_norm(zscore_len);          //includes any probability mass below zero
    lenprob(iage,1)=cprob_lenvec(1)-cprob_lzero;    //removes any probability mass below zero

	//20 in size limit
	 zscore_len_20=((lenbins(1)+0.5*lenbins_width)-meanlen_TL_20(iage)) / len_sd_20(iage);
    cprob_lenvec_20(1)=cumd_norm(zscore_len_20);          //includes any probability mass below zero
    lenprob_20(iage,1)=cprob_lenvec_20(1)-cprob_lzero_20;    //removes any probability mass below zero

	//All fishery dependent
    zscore_len_L=((lenbins(1)+0.5*lenbins_width)-meanlen_TL_L(iage)) / len_sd_L(iage);
    cprob_lenvec_L(1)=cumd_norm(zscore_len_L);          //includes any probability mass below zero
    lenprob_L(iage,1)=cprob_lenvec_L(1)-cprob_lzero_L;    //removes any probability mass below zero
         
    //most other length bins  
    //population
    for (ilen=2;ilen<nlenbins;ilen++)
      {
        zscore_len=((lenbins(ilen)+0.5*lenbins_width)-meanlen_TL(iage)) / len_sd(iage); 
		cprob_lenvec(ilen)=cumd_norm(zscore_len);
        lenprob(iage,ilen)=cprob_lenvec(ilen)-cprob_lenvec(ilen-1);
      }
	
	//20 in size limit
	for (ilen=2;ilen<nlenbins;ilen++)
      {
        zscore_len_20=((lenbins(ilen)+0.5*lenbins_width)-meanlen_TL_20(iage)) / len_sd_20(iage); 
		cprob_lenvec_20(ilen)=cumd_norm(zscore_len_20);
        lenprob_20(iage,ilen)=cprob_lenvec_20(ilen)-cprob_lenvec_20(ilen-1);
      }
	//All fishery dependent	
    for (ilen=2;ilen<nlenbins;ilen++)
      {
        zscore_len_L=((lenbins(ilen)+0.5*lenbins_width)-meanlen_TL_L(iage)) / len_sd_L(iage); 
		cprob_lenvec_L(ilen)=cumd_norm(zscore_len_L);
        lenprob_L(iage,ilen)=cprob_lenvec_L(ilen)-cprob_lenvec_L(ilen-1);
      }
    //last length bin is a plus group
	//population
    zscore_len=((lenbins(nlenbins)-0.5*lenbins_width)-meanlen_TL(iage)) / len_sd(iage); 
	lenprob(iage,nlenbins)=1.0-cumd_norm(zscore_len);
    lenprob(iage)=lenprob(iage)/(1.0-cprob_lzero);  //renormalize to account for any prob mass below size=0
  
	//20 in size limit
	zscore_len_20=((lenbins(nlenbins)-0.5*lenbins_width)-meanlen_TL_20(iage)) / len_sd_20(iage); 
	lenprob_20(iage,nlenbins)=1.0-cumd_norm(zscore_len_20);
    lenprob_20(iage)=lenprob_20(iage)/(1.0-cprob_lzero_20);  //renormalize to account for any prob mass below size=0
  
	//All fishery dependent
    zscore_len_L=((lenbins(nlenbins)-0.5*lenbins_width)-meanlen_TL_L(iage)) / len_sd_L(iage); 
	lenprob_L(iage,nlenbins)=1.0-cumd_norm(zscore_len_L);
    lenprob_L(iage)=lenprob_L(iage)/(1.0-cprob_lzero_L);  //renormalize to account for any prob mass below size=0
  }
  
  //fleet and survey specific length probs, all assumed here to equal the popn or to the landings
  lenprob_cH=lenprob_L;
  lenprob_cH_D=lenprob; //population
  lenprob_HB=lenprob_L;
  lenprob_HB_D=lenprob; //population
  lenprob_GR_D=lenprob; //population
  lenprob_CVT=lenprob; //population
  lenprob_GR=lenprob_L;  
  
FUNCTION get_weight_at_age_landings  ///***in whole weight
  
  //for (iyear=styr; iyear<=endyr; iyear++)
  for (iyear=styr; iyear<=endyr_selex_phase2; iyear++) //start through 1991
  {
    len_cH_mm(iyear)=meanlen_TL_L;  //using all fishery dependent growth for fisheries until 20 in size limit
    wholewgt_cH_klb(iyear)=wgt_klb_L; 
    len_HB_mm(iyear)=meanlen_TL_L;
    wholewgt_HB_klb(iyear)=wgt_klb_L;
    len_GR_mm(iyear)=meanlen_TL_L;
    wholewgt_GR_klb(iyear)=wgt_klb_L;

    len_cH_D_mm(iyear)=meanlen_TL;  //using the population growth to define discards
    wholewgt_cH_D_klb(iyear)=wgt_klb;
    len_HB_D_mm(iyear)=meanlen_TL;
    wholewgt_HB_D_klb(iyear)=wgt_klb;
    len_GR_D_mm(iyear)=meanlen_TL;
    wholewgt_GR_D_klb(iyear)=wgt_klb;
  }  
  for (iyear=endyr_selex_phase2+1; iyear<=endyr_selex_phase3; iyear++)  //1992 through 2009
  {
    len_cH_mm(iyear)=meanlen_TL_20;
    wholewgt_cH_klb(iyear)=wgt_klb_20; 
    len_HB_mm(iyear)=meanlen_TL_20;
    wholewgt_HB_klb(iyear)=wgt_klb_20;
    len_GR_mm(iyear)=meanlen_TL_20;
    wholewgt_GR_klb(iyear)=wgt_klb_20;

    len_cH_D_mm(iyear)=meanlen_TL;
    wholewgt_cH_D_klb(iyear)=wgt_klb;   //using population growth curve for the discard weight
    len_HB_D_mm(iyear)=meanlen_TL;
    wholewgt_HB_D_klb(iyear)=wgt_klb;
    len_GR_D_mm(iyear)=meanlen_TL;
    wholewgt_GR_D_klb(iyear)=wgt_klb;
  }  
  for (iyear=endyr_selex_phase3+1; iyear<=endyr; iyear++)  //2010 through 2014 save in case we end up treating as a different growth curve
  {
    len_cH_mm(iyear)=meanlen_TL_L;
    wholewgt_cH_klb(iyear)=wgt_klb_L; 
    len_HB_mm(iyear)=meanlen_TL_L;
    wholewgt_HB_klb(iyear)=wgt_klb_L;
    len_GR_mm(iyear)=meanlen_TL_L;
    wholewgt_GR_klb(iyear)=wgt_klb_L;

    len_cH_D_mm(iyear)=meanlen_TL;
    wholewgt_cH_D_klb(iyear)=wgt_klb;
    len_HB_D_mm(iyear)=meanlen_TL;
    wholewgt_HB_D_klb(iyear)=wgt_klb;
    len_GR_D_mm(iyear)=meanlen_TL;
    wholewgt_GR_D_klb(iyear)=wgt_klb;
  }  
 
FUNCTION get_spr_F0
  //at mdyr, apply half this yr's mortality, half next yr's
  N_spr_F0(1)=1.0*mfexp(-1.0*M(1)*spawn_time_frac); //at peak spawning time
  N_bpr_F0(1)=1.0;      //at start of year
  for (iage=2; iage<=nages; iage++)
  { N_spr_F0(iage)=N_spr_F0(iage-1)*mfexp(-1.0*(M(iage-1)*(1.0-spawn_time_frac) + M(iage)*spawn_time_frac)); 
    N_bpr_F0(iage)=N_bpr_F0(iage-1)*mfexp(-1.0*(M(iage-1)));    
  }
  N_spr_F0(nages)=N_spr_F0(nages)/(1.0-mfexp(-1.0*M(nages))); //plus group (sum of geometric series)
  N_bpr_F0(nages)=N_bpr_F0(nages)/(1.0-mfexp(-1.0*M(nages)));
  
  spr_F0=sum(elem_prod(N_spr_F0,reprod)); 
  bpr_F0=sum(elem_prod(N_bpr_F0,wgt_mt));    

FUNCTION get_selectivity

  sel_HB_block1=logistic_double(agebins, selpar_A50_HB1, selpar_slope_HB1, selpar_A502_HB1, selpar_slope2_HB1);
  sel_HB_block2=logistic_double(agebins, selpar_A50_HB2, selpar_slope_HB2, selpar_A502_HB2,selpar_slope2_HB2);
  sel_HB_block3=logistic_double(agebins, selpar_A50_HB3, selpar_slope_HB3, selpar_A502_HB3, selpar_slope2_HB3);
  sel_GR_block1=sel_HB_block1;
  sel_GR_block2=logistic_double(agebins, selpar_A50_GR2, selpar_slope_GR2, selpar_A502_GR2,selpar_slope2_GR2);
  sel_GR_block3=logistic(agebins, selpar_A50_GR3, selpar_slope_GR3);
  sel_HB_D_block3=logistic_double(agebins, selpar_A50_HB3_D, selpar_slope_HB3_D, selpar_A502_HB3_D, selpar_slope2_HB3_D);
  sel_GR_D_block3=logistic_double(agebins, selpar_A50_GR3_D, selpar_slope_GR3_D, selpar_A502_GR3_D, selpar_slope2_GR3_D); 
  sel_CVT_vec=logistic_double(agebins, selpar_A50_CVT, selpar_slope_CVT, selpar_A502_CVT, selpar_slope2_CVT);
  sel_CVT_vec((nages_agec_10p+1),nages)=sel_CVT_vec(nages_agec_10p);
  
  selpar_switch_VID=0.0;
  for (iage=1; iage<=nages; iage++)
  {if (selpar_switch_VID==1.0 | sel_CVT_vec(iage)==max(sel_CVT_vec))
    {sel_VID_vec(iage)=1.0; 
	 selpar_switch_VID=1.0;
	} else {sel_VID_vec(iage)=sel_CVT_vec(iage);}
  }
  
    
  //BLOCK 1 for selex. No limit through to 20" size limit  
  for (iyear=styr; iyear<=endyr_selex_phase2; iyear++)
   {     
    sel_cH(iyear)=logistic(agebins, selpar_A50_cH1, selpar_slope_cH1);
    sel_HB(iyear)(1,nages_agec_10p)=sel_HB_block1(1,nages_agec_10p);
	sel_HB(iyear)((nages_agec_10p+1),nages)=sel_HB_block1(nages_agec_10p);
    sel_GR(iyear)(1,nages_agec_10p)=sel_GR_block1(1,nages_agec_10p); //NOTE: uses nages_agec_10p bc this selex mirrors HB
	sel_GR(iyear)((nages_agec_10p+1),nages)=sel_GR_block1(nages_agec_10p);
	sel_cH_D(iyear)=logistic_double(agebins, selpar_A50_cH2_D, selpar_slope_cH2_D, selpar_A502_cH2_D, selpar_slope2_cH2_D);
	sel_HB_D(iyear)=logistic_double(agebins, selpar_A50_HB2_D, selpar_slope_HB2_D, selpar_A502_HB2_D, selpar_slope2_HB2_D);
	sel_GR_D(iyear)=sel_HB_D(iyear);
   }
   
  //BLOCK 2 for selex. 20" size limit until 2010
  
  for (iyear=(endyr_selex_phase2+1); iyear<=endyr_selex_phase3; iyear++)
   {
    sel_cH(iyear)=logistic(agebins, selpar_A50_cH2, selpar_slope_cH2);
    sel_HB(iyear)(1,nages_agec_10p)=sel_HB_block2(1,nages_agec_10p);
	sel_HB(iyear)((nages_agec_10p+1),nages)=sel_HB_block2(nages_agec_10p);
    sel_GR(iyear)(1,nages_agec_10p)=sel_GR_block2(1,nages_agec_10p); 
	sel_GR(iyear)((nages_agec_10p+1),nages)=sel_GR_block2(nages_agec_10p);
    sel_cH_D(iyear)=logistic_double(agebins, selpar_A50_cH2_D, selpar_slope_cH2_D, selpar_A502_cH2_D, selpar_slope2_cH2_D);  
	sel_HB_D(iyear)=logistic_double(agebins, selpar_A50_HB2_D, selpar_slope_HB2_D, selpar_A502_HB2_D, selpar_slope2_HB2_D);  
	sel_GR_D(iyear)=sel_HB_D(iyear);
   }
  
  //BLOCK 3 for selex.  Moratorium and mini-seasons with no size limit during the mini-season
   for (iyear=(endyr_selex_phase3+1); iyear<=endyr; iyear++)
   {   
    sel_cH(iyear)=logistic(agebins, selpar_A50_cH3, selpar_slope_cH3);
	sel_HB(iyear)(1,nages_agec_10p)=sel_HB_block3(1,nages_agec_10p);
	sel_HB(iyear)((nages_agec_10p+1),nages)=sel_HB_block3(nages_agec_10p);
    sel_GR(iyear)(1,nages_agec)=sel_GR_block3(1,nages_agec);
	sel_GR(iyear)((nages_agec+1),nages)=sel_GR_block3(nages_agec);
    sel_cH_D(iyear)=logistic(agebins, selpar_A50_cH3_D, selpar_slope_cH3_D);
	sel_HB_D(iyear)(1,nages_agec_10p)=sel_HB_D_block3(1,nages_agec_10p);
	sel_HB_D(iyear)((nages_agec_10p+1),nages)=sel_HB_D_block3(nages_agec_10p);
	sel_GR_D(iyear)(1,nages_agec_10p)=sel_GR_D_block3(1,nages_agec_10p);
	sel_GR_D(iyear)((nages_agec_10p+1),nages)=sel_GR_D_block3(nages_agec_10p);
   }  
   //sel_GR_D=sel_HB_D;
   for (iyear=styr; iyear<=endyr; iyear++)
     {     
       sel_CVT(iyear)=sel_CVT_vec;
	   sel_VID(iyear)=sel_VID_vec;
     }

FUNCTION get_mortality
  Fsum.initialize();
  Fapex.initialize();
  F.initialize();
  //initialization F is avg from first 3 yrs of observed landings
  log_F_dev_init_cH=sum(log_F_dev_cH(styr_cH_L,(styr_cH_L+2)))/3.0;         
  log_F_dev_init_HB=sum(log_F_dev_HB(styr_HB_L,(styr_HB_L+2)))/3.0;         
  log_F_dev_init_GR=sum(log_F_dev_GR(styr_GR_L,(styr_GR_L+2)))/3.0;         
  
  for (iyear=styr; iyear<=endyr; iyear++) 
  {
    if(iyear>=styr_cH_L & iyear<=endyr_cH_L)
    {  F_cH_out(iyear)=mfexp(log_avg_F_cH+log_F_dev_cH(iyear)); //}    
       F_cH(iyear)=sel_cH(iyear)*F_cH_out(iyear);
       Fsum(iyear)+=F_cH_out(iyear);
    }

    if(iyear>=styr_HB_L & iyear<=endyr_HB_L)
    {  F_HB_out(iyear)=mfexp(log_avg_F_HB+log_F_dev_HB(iyear)); //}    
       F_HB(iyear)=sel_HB(iyear)*F_HB_out(iyear);
       Fsum(iyear)+=F_HB_out(iyear);
    }
   
    if(iyear>=styr_GR_L & iyear<=endyr_GR_L) 
    {  F_GR_out(iyear)=mfexp(log_avg_F_GR+log_F_dev_GR(iyear)); //}    
       F_GR(iyear)=sel_GR(iyear)*F_GR_out(iyear); //(general rec shares headboat selex) fixed 
       Fsum(iyear)+=F_GR_out(iyear);
    }

    if(iyear>=styr_cH_D & iyear<=endyr_cH_D)
    {  F_cH_D_out(iyear)=mfexp(log_avg_F_cH_D+log_F_dev_cH_D(iyear)); //}    
       F_cH_D(iyear)=sel_cH_D(iyear)*F_cH_D_out(iyear);
       Fsum(iyear)+=F_cH_D_out(iyear);
    }

    if(iyear>=styr_HB_D & iyear<=endyr_HB_D)
    {  F_HB_D_out(iyear)=mfexp(log_avg_F_HB_D+log_F_dev_HB_D(iyear)); //}    
       F_HB_D(iyear)=sel_HB_D(iyear)*F_HB_D_out(iyear);
       Fsum(iyear)+=F_HB_D_out(iyear);
    }

    if(iyear>=styr_GR_D & iyear<=endyr_GR_D)
    {  F_GR_D_out(iyear)=mfexp(log_avg_F_GR_D+log_F_dev_GR_D(iyear)); //}    
       F_GR_D(iyear)=sel_GR_D(iyear)*F_GR_D_out(iyear); //general rec shares headboat selex
       Fsum(iyear)+=F_GR_D_out(iyear);
    }
 
    //Total F at age
    F(iyear)=F_cH(iyear);  //first in additive series (NO +=)
    F(iyear)+=F_HB(iyear);
    F(iyear)+=F_GR(iyear);
    F(iyear)+=F_cH_D(iyear);
    F(iyear)+=F_HB_D(iyear);
    F(iyear)+=F_GR_D(iyear);
    
    Fapex(iyear)=max(F(iyear));
    Z(iyear)=M+F(iyear);
    
  }  //end iyear 

FUNCTION get_bias_corr
  var_rec_dev=norm2(log_rec_dev(styr_rec_dev,endyr_rec_dev)-
              sum(log_rec_dev(styr_rec_dev,endyr_rec_dev))/nyrs_rec)
              /(nyrs_rec-1.0);                           
  //if (set_BiasCor <= 0.0) {BiasCor=mfexp(var_rec_dev/2.0);}   //bias correction based on empirical residuals
  rec_sigma_sq=square(rec_sigma);
  if (set_BiasCor <= 0.0) {BiasCor=mfexp(rec_sigma_sq/2.0);}   //bias correction based on Rsigma               
  else {BiasCor=set_BiasCor;}

FUNCTION get_numbers_at_age
//Initialization
  R0=mfexp(log_R0);
  S0=spr_F0*R0;
  R_virgin=SR_eq_func(R0, steep, spr_F0, spr_F0, BiasCor, SR_switch);
 
  B0=bpr_F0*R_virgin;   
  B0_q_DD=R_virgin*sum(elem_prod(N_bpr_F0(set_q_DD_stage,nages),wgt_mt(set_q_DD_stage,nages))); 
  
  F_init_denom=mfexp(log_avg_F_cH+log_F_dev_init_cH)+mfexp(log_avg_F_HB+log_F_dev_init_HB)+mfexp(log_avg_F_GR+log_F_dev_init_GR);
  F_init_cH_prop= 1.0; //mfexp(log_avg_F_cH+log_F_dev_init_cH)/F_init_denom;  //Leave this machinery in, in case a sensitivity is run with a later start year.
  F_init_HB_prop= 0.0; //mfexp(log_avg_F_HB+log_F_dev_init_HB)/F_init_denom;
  F_init_GR_prop= 0.0; //mfexp(log_avg_F_GR+log_F_dev_init_GR)/F_init_denom;
  
  F_initial=sel_cH(styr)*F_init*F_init_cH_prop+
            sel_HB(styr)*F_init*F_init_HB_prop+
            sel_GR(styr)*F_init*F_init_GR_prop; //(GR uses HB selex)  
  Z_initial=M+F_initial;

//Initial equilibrium age structure
  N_spr_initial(1)=1.0*mfexp(-1.0*Z_initial(1)*spawn_time_frac); //at peak spawning time;
  for (iage=2; iage<=nages; iage++)
    {
      N_spr_initial(iage)=N_spr_initial(iage-1)*
                   mfexp(-1.0*(Z_initial(iage-1)*(1.0-spawn_time_frac) + Z_initial(iage)*spawn_time_frac)); 
    }
  N_spr_initial(nages)=N_spr_initial(nages)/(1.0-mfexp(-1.0*Z_initial(nages))); //plus group
  spr_initial=sum(elem_prod(N_spr_initial,reprod));
  if (styr==styr_rec_dev) {R1=SR_eq_func(R0, steep, spr_F0, spr_initial, 1.0, SR_switch);} //without bias correction (deviation added later)
  else {R1=SR_eq_func(R0, steep, spr_F0, spr_initial, BiasCor, SR_switch);} //with bias correction
  if(R1<10.0) {R1=10.0;} //Avoid unrealistically low popn sizes during search algorithm

//Compute equilibrium age structure for first year
  N_initial_eq(1)=R1;
  for (iage=2; iage<=nages; iage++)
  {
    N_initial_eq(iage)=N_initial_eq(iage-1)*
        mfexp(-1.0*(Z_initial(iage-1)));    
  }
  //plus group calculation
  N_initial_eq(nages)=N_initial_eq(nages)/(1.0-mfexp(-1.0*Z_initial(nages))); //plus group
  
//Add deviations to initial equilibrium N
  N(styr)(2,nages)=elem_prod(N_initial_eq(2,nages),mfexp(log_Nage_dev));
   
  if (styr==styr_rec_dev) {N(styr,1)=N_initial_eq(1)*mfexp(log_rec_dev(styr_rec_dev));}
  else {N(styr,1)=N_initial_eq(1);}
  
  N_mdyr(styr)(1,nages)=elem_prod(N(styr)(1,nages),(mfexp(-1.*(Z_initial(1,nages))*0.5))); //mid year 
  N_spawn(styr)(1,nages)=elem_prod(N(styr)(1,nages),(mfexp(-1.*(Z_initial(1,nages))*spawn_time_frac))); //peak spawning time 

  SSB(styr)=sum(elem_prod(N_spawn(styr),reprod));
  B_q_DD(styr)=sum(elem_prod(N(styr)(set_q_DD_stage,nages),wgt_mt(set_q_DD_stage,nages)));
    
//Rest of years 
  for (iyear=styr; iyear<endyr; iyear++)
  {
    if(iyear<(styr_rec_dev-1)||iyear>(endyr_rec_dev-1)) //recruitment follows S-R curve (with bias correction) exactly
    {
        N(iyear+1,1)=BiasCor*SR_func(R0, steep, spr_F0, SSB(iyear),SR_switch);
        N(iyear+1)(2,nages)=++elem_prod(N(iyear)(1,nages-1),(mfexp(-1.*Z(iyear)(1,nages-1))));
        N(iyear+1,nages)+=N(iyear,nages)*mfexp(-1.*Z(iyear,nages)); //plus group
        N_mdyr(iyear+1)(1,nages)=elem_prod(N(iyear+1)(1,nages),(mfexp(-1.*(Z(iyear+1)(1,nages))*0.5))); //mid year 
        N_spawn(iyear+1)(1,nages)=elem_prod(N(iyear+1)(1,nages),(mfexp(-1.*(Z(iyear+1)(1,nages))*spawn_time_frac))); //peak spawning time 
        SSB(iyear+1)=sum(elem_prod(N_spawn(iyear+1),reprod));
		B_q_DD(iyear+1)=sum(elem_prod(N(iyear+1)(set_q_DD_stage,nages),wgt_mt(set_q_DD_stage,nages)));       
    }
    else   //recruitment follows S-R curve with lognormal deviation
    {
        N(iyear+1,1)=SR_func(R0, steep, spr_F0, SSB(iyear),SR_switch)*mfexp(log_rec_dev(iyear+1));
        N(iyear+1)(2,nages)=++elem_prod(N(iyear)(1,nages-1),(mfexp(-1.*Z(iyear)(1,nages-1))));
        N(iyear+1,nages)+=N(iyear,nages)*mfexp(-1.*Z(iyear,nages)); //plus group
        N_mdyr(iyear+1)(1,nages)=elem_prod(N(iyear+1)(1,nages),(mfexp(-1.*(Z(iyear+1)(1,nages))*0.5))); //mid year 
        N_spawn(iyear+1)(1,nages)=elem_prod(N(iyear+1)(1,nages),(mfexp(-1.*(Z(iyear+1)(1,nages))*spawn_time_frac))); //peak spawning time 
        SSB(iyear+1)=sum(elem_prod(N_spawn(iyear+1),reprod));
        B_q_DD(iyear+1)=sum(elem_prod(N(iyear+1)(set_q_DD_stage,nages),wgt_mt(set_q_DD_stage,nages)));
    }
  }
  
  //last year (projection) has no recruitment variability
  N(endyr+1,1)=BiasCor*SR_func(R0, steep, spr_F0, SSB(endyr),SR_switch);
  N(endyr+1)(2,nages)=++elem_prod(N(endyr)(1,nages-1),(mfexp(-1.*Z(endyr)(1,nages-1))));
  N(endyr+1,nages)+=N(endyr,nages)*mfexp(-1.*Z(endyr,nages)); //plus group

  
FUNCTION get_landings_numbers //Baranov catch eqn
  for (iyear=styr; iyear<=endyr; iyear++)
  {
    for (iage=1; iage<=nages; iage++)
    {
      L_cH_num(iyear,iage)=N(iyear,iage)*F_cH(iyear,iage)*
        (1.-mfexp(-1.*Z(iyear,iage)))/Z(iyear,iage);
      L_HB_num(iyear,iage)=N(iyear,iage)*F_HB(iyear,iage)*
        (1.-mfexp(-1.*Z(iyear,iage)))/Z(iyear,iage);
      L_GR_num(iyear,iage)=N(iyear,iage)*F_GR(iyear,iage)*
        (1.-mfexp(-1.*Z(iyear,iage)))/Z(iyear,iage);        
    }          
    pred_cH_L_knum(iyear)=sum(L_cH_num(iyear))/1000.0;
    pred_HB_L_knum(iyear)=sum(L_HB_num(iyear))/1000.0;
    pred_GR_L_knum(iyear)=sum(L_GR_num(iyear))/1000.0;
  }

 
FUNCTION get_landings_wgt
  for (iyear=styr; iyear<=endyr; iyear++)
  {    
    L_cH_klb(iyear)=elem_prod(L_cH_num(iyear),wholewgt_cH_klb(iyear));     //in 1000 lb whole weight
    L_HB_klb(iyear)=elem_prod(L_HB_num(iyear),wholewgt_HB_klb(iyear));     //in 1000 lb whole weight
    L_GR_klb(iyear)=elem_prod(L_GR_num(iyear),wholewgt_GR_klb(iyear));     //in 1000 lb whole weight
    
    pred_cH_L_klb(iyear)=sum(L_cH_klb(iyear));
    pred_HB_L_klb(iyear)=sum(L_HB_klb(iyear));
    pred_GR_L_klb(iyear)=sum(L_GR_klb(iyear));    
  }
 
FUNCTION get_dead_discards
  //dead discards at age (number fish) 

  for (iyear=styr_cH_D; iyear<=endyr_cH_D; iyear++)
  {
    for (iage=1; iage<=nages; iage++)
    {
      D_cH_num(iyear,iage)=N(iyear,iage)*F_cH_D(iyear,iage)*
        (1.-mfexp(-1.*Z(iyear,iage)))/Z(iyear,iage);
    }
    pred_cH_D_knum(iyear)=sum(D_cH_num(iyear))/1000.0;            //pred annual dead discards in 1000s (for matching data)
    pred_cH_D_klb(iyear)=sum(elem_prod(D_cH_num(iyear),wholewgt_cH_D_klb(iyear))); //annual dead discards in 1000 lb whole (for output only)
  }

  for (iyear=styr_HB_D; iyear<=endyr_HB_D; iyear++)
  {
    for (iage=1; iage<=nages; iage++)
    {
      D_HB_num(iyear,iage)=N(iyear,iage)*F_HB_D(iyear,iage)*
        (1.-mfexp(-1.*Z(iyear,iage)))/Z(iyear,iage);
    }
    pred_HB_D_knum(iyear)=sum(D_HB_num(iyear))/1000.0;            //pred annual dead discards in 1000s (for matHBing data)
    pred_HB_D_klb(iyear)=sum(elem_prod(D_HB_num(iyear),wholewgt_HB_D_klb(iyear))); //annual dead discards in 1000 lb whole (for output only)
  }

  for (iyear=styr_GR_D; iyear<=endyr_GR_D; iyear++)
  {
    for (iage=1; iage<=nages; iage++)
    {
      D_GR_num(iyear,iage)=N(iyear,iage)*F_GR_D(iyear,iage)*
        (1.-mfexp(-1.*Z(iyear,iage)))/Z(iyear,iage);
    }
    pred_GR_D_knum(iyear)=sum(D_GR_num(iyear))/1000.0;            //pred annual dead discards in 1000s (for matGRing data)
    pred_GR_D_klb(iyear)=sum(elem_prod(D_GR_num(iyear),wholewgt_GR_D_klb(iyear))); //annual dead discards in 1000 lb whole (for output only)
  }
   
FUNCTION get_catchability_fcns    
 //Get rate increase if estimated, otherwise fixed above
  if (set_q_rate_phase>0.0)
  {

      for (iyear=styr_cH_cpue; iyear<=endyr_cH_cpue; iyear++)
      {   if (iyear>styr_cH_cpue & iyear <=2003) 
          {//q_rate_fcn_cH(iyear)=(1.0+q_rate)*q_rate_fcn_cH(iyear-1); //compound
             q_rate_fcn_cH(iyear)=(1.0+(iyear-styr_cH_cpue)*q_rate)*q_rate_fcn_cH(styr_cH_cpue);  //linear
          }
          if (iyear>2003) {q_rate_fcn_cH(iyear)=q_rate_fcn_cH(iyear-1);} 
      }   
      for (iyear=styr_HB_cpue; iyear<=endyr_HB_cpue; iyear++)
      {   if (iyear>styr_HB_cpue & iyear <=2003) 
          {//q_rate_fcn_HB(iyear)=(1.0+q_rate)*q_rate_fcn_HB(iyear-1); //compound
             q_rate_fcn_HB(iyear)=(1.0+(iyear-styr_HB_cpue)*q_rate)*q_rate_fcn_HB(styr_HB_cpue);  //linear
          }
          if (iyear>2003) {q_rate_fcn_HB(iyear)=q_rate_fcn_HB(iyear-1);} 
      }   
      for (iyear=styr_HB_D_cpue; iyear<=endyr_HB_D_cpue; iyear++)
      {   if (iyear>styr_HB_D_cpue & iyear <=2003) 
          {//q_rate_fcn_HB_D(iyear)=(1.0+q_rate)*q_rate_fcn_HB_D(iyear-1); //compound
             q_rate_fcn_HB_D(iyear)=(1.0+(iyear-styr_HB_D_cpue)*q_rate)*q_rate_fcn_HB_D(styr_HB_D_cpue);  //linear
          }
          if (iyear>2003) {q_rate_fcn_HB_D(iyear)=q_rate_fcn_HB_D(iyear-1);} 
      }   
      
  } //end q_rate conditional      

 //Get density dependence scalar (=1.0 if density independent model is used)   
  if (q_DD_beta>0.0) 
  {
    B_q_DD+=dzero;
    for (iyear=styr;iyear<=endyr;iyear++)
        {q_DD_fcn(iyear)=pow(B0_q_DD,q_DD_beta)*pow(B_q_DD(iyear),-q_DD_beta);}
          //{q_DD_fcn(iyear)=1.0+4.0/(1.0+mfexp(0.75*(B_q_DD(iyear)-0.1*B0_q_DD))); }
  }  

     
FUNCTION get_indices
//---Predicted CPUEs------------------------

 //cH  cpue
  q_cH(styr_cH_cpue)=mfexp(log_q_cH); 
  for (iyear=styr_cH_cpue; iyear<=endyr_cH_cpue; iyear++)
  {//index in weight units. original index in lb and re-scaled. predicted in klb whole weight, but difference in lb and klb is absorbed by q
      N_cH(iyear)=elem_prod(elem_prod(N_mdyr(iyear),sel_cH(iyear)),wholewgt_cH_klb(iyear));   
      pred_cH_cpue(iyear)=q_cH(iyear)*q_rate_fcn_cH(iyear)*q_DD_fcn(iyear)*sum(N_cH(iyear));
      if (iyear<endyr_cH_cpue){q_cH(iyear+1)=q_cH(iyear)*mfexp(q_RW_log_dev_cH(iyear));}
  }

 //HB  cpue
  q_HB(styr_HB_cpue)=mfexp(log_q_HB); 
  for (iyear=styr_HB_cpue; iyear<=endyr_HB_cpue; iyear++)
  {   
      N_HB(iyear)=elem_prod(N_mdyr(iyear),sel_HB(iyear)); 
      pred_HB_cpue(iyear)=q_HB(iyear)*q_rate_fcn_HB(iyear)*q_DD_fcn(iyear)*sum(N_HB(iyear));
      if (iyear<endyr_HB_cpue){q_HB(iyear+1)=q_HB(iyear)*mfexp(q_RW_log_dev_HB(iyear));}
  }

 //HB disc cpue         
  q_HB_D(styr_HB_D_cpue)=mfexp(log_q_HB_D); 
  for (iyear=styr_HB_D_cpue; iyear<=endyr_HB_D_cpue; iyear++)
  {   
      N_HB_D(iyear)=elem_prod(N_mdyr(iyear),sel_HB_D(endyr_selex_phase3));  //selectivity from block with 20inch size limit, as in the index
      pred_HB_D_cpue(iyear)=q_HB_D(iyear)*q_rate_fcn_HB_D(iyear)*q_DD_fcn(iyear)*sum(N_HB_D(iyear));
      if (iyear<endyr_HB_D_cpue){q_HB_D(iyear+1)=q_HB_D(iyear)*mfexp(q_RW_log_dev_HB_D(iyear));}
  } 
 
 //SERFS CVT cpue
  q_CVT(styr_CVT_cpue)=mfexp(log_q_CVT); 
  for (iyear=styr_CVT_cpue; iyear<=endyr_CVT_cpue; iyear++)
  {   
    N_CVT(iyear)=elem_prod(N_mdyr(iyear),sel_CVT(iyear)); 
	pred_CVT_cpue(iyear)=q_CVT(iyear)*q_DD_fcn(iyear)*sum(N_CVT(iyear));
    if (iyear<endyr_CVT_cpue){q_CVT(iyear+1)=q_CVT(iyear)*mfexp(q_RW_log_dev_CVT(iyear));}
  } 

 //SERFS CVT cpue
  q_VID(styr_VID_cpue)=mfexp(log_q_VID); 
  for (iyear=styr_VID_cpue; iyear<=endyr_VID_cpue; iyear++)
  {   
    N_VID(iyear)=elem_prod(N_mdyr(iyear),sel_VID(iyear)); 
	pred_VID_cpue(iyear)=q_VID(iyear)*q_DD_fcn(iyear)*sum(N_VID(iyear));
    if (iyear<endyr_VID_cpue){q_VID(iyear+1)=q_VID(iyear)*mfexp(q_RW_log_dev_VID(iyear));}
  } 
  
FUNCTION get_length_comps

  //------comm handline 
  for (iyear=1;iyear<=nyr_cH_lenc;iyear++)
  {pred_cH_lenc(iyear)=(L_cH_num(yrs_cH_lenc(iyear))*lenprob_cH)/sum(L_cH_num(yrs_cH_lenc(iyear)));}
  
  //------comm discards  
  //for (iyear=1;iyear<=nyr_cH_D_lenc;iyear++) 
  //{pred_cH_D_lenc(iyear)=(D_cH_num(yrs_cH_D_lenc(iyear))*lenprob_cH_D)/sum(D_cH_num(yrs_cH_D_lenc(iyear)));}
 
   for (iyear=1;iyear<=nyr_cH_D_lenc_pool1;iyear++)
	{pred_cH_D_lenc_yr1(iyear)=(D_cH_num(yrs_cH_D_lenc_pool1(iyear))*lenprob_cH_D)/sum(D_cH_num(yrs_cH_D_lenc_pool1(iyear)));}

   for (iyear=1;iyear<=nyr_cH_D_lenc_pool2;iyear++)
	{pred_cH_D_lenc_yr2(iyear)=(D_cH_num(yrs_cH_D_lenc_pool2(iyear))*lenprob_cH_D)/sum(D_cH_num(yrs_cH_D_lenc_pool2(iyear)));}
	
   pred_cH_D_lenc.initialize();
   for (iyear=1;iyear<=nyr_cH_D_lenc_pool1;iyear++)
     {pred_cH_D_lenc(1) += nsamp_cH_D_lenc_pool1(iyear) * pred_cH_D_lenc_yr1(iyear);}
   pred_cH_D_lenc(1)=pred_cH_D_lenc(1)/sum(nsamp_cH_D_lenc_pool1);
   
   for (iyear=1;iyear<=nyr_cH_D_lenc_pool2;iyear++)
     {pred_cH_D_lenc(2) += nsamp_cH_D_lenc_pool2(iyear) * pred_cH_D_lenc_yr2(iyear);}
   pred_cH_D_lenc(2)=pred_cH_D_lenc(2)/sum(nsamp_cH_D_lenc_pool2);

 
 
 
  //------headboat discards  
  for (iyear=1;iyear<=nyr_HB_D_lenc;iyear++) 
  {pred_HB_D_lenc(iyear)=(D_HB_num(yrs_HB_D_lenc(iyear))*lenprob_HB_D)/sum(D_HB_num(yrs_HB_D_lenc(iyear)));}
 
  //------gen rec discards  
  for (iyear=1;iyear<=nyr_GR_D_lenc;iyear++) 
  {pred_GR_D_lenc(iyear)=(D_GR_num(yrs_GR_D_lenc(iyear))*lenprob_GR_D)/sum(D_GR_num(yrs_GR_D_lenc(iyear)));}
  
FUNCTION get_age_comps
   
  //Commercial handline
  for (iyear=1;iyear<=nyr_cH_agec;iyear++) 
  {
    ErrorFree_cH_agec(iyear)=L_cH_num(yrs_cH_agec(iyear))/sum(L_cH_num(yrs_cH_agec(iyear)));  
    //ErrorFree_cH_agec(iyear)=elem_prod(N(yrs_cH_agec(iyear)),sel_cH(yrs_cH_agec(iyear)));
    pred_cH_agec_allages(iyear)=age_error*(ErrorFree_cH_agec(iyear)/sum(ErrorFree_cH_agec(iyear)));   
    for (iage=1; iage<=nages_agec; iage++) {pred_cH_agec(iyear,iage)=pred_cH_agec_allages(iyear,iage);} 
    for (iage=(nages_agec+1); iage<=nages; iage++) {pred_cH_agec(iyear,nages_agec)+=pred_cH_agec_allages(iyear,iage);} //plus group                             
  }
 
  //Headboat
 for (iyear=1;iyear<=nyr_HB_agec;iyear++)
  {
    ErrorFree_HB_agec(iyear)=L_HB_num(yrs_HB_agec(iyear))/sum(L_HB_num(yrs_HB_agec(iyear)));
    pred_HB_agec_allages(iyear)=age_error*ErrorFree_HB_agec(iyear); 
    for (iage=1; iage<=nages_agec_10p; iage++) {pred_HB_agec(iyear,iage)=pred_HB_agec_allages(iyear,iage);} 
    for (iage=(nages_agec_10p+1); iage<=nages; iage++) {pred_HB_agec(iyear,nages_agec_10p)+=pred_HB_agec_allages(iyear,iage);} //plus group                        
  }
    
   //General Recreational  
 for (iyear=1;iyear<=nyr_GR_agec;iyear++)
  {
    ErrorFree_GR_agec(iyear)=L_GR_num(yrs_GR_agec(iyear))/sum(L_GR_num(yrs_GR_agec(iyear)));
    pred_GR_agec_allages(iyear)=age_error*ErrorFree_GR_agec(iyear); 
    for (iage=1; iage<=nages_agec_10p; iage++) {pred_GR_agec(iyear,iage)=pred_GR_agec_allages(iyear,iage);} 
    for (iage=(nages_agec_10p+1); iage<=nages; iage++) {pred_GR_agec(iyear,nages_agec_10p)+=pred_GR_agec_allages(iyear,iage);} //plus group                        
  }
 

    //SERFS CVT
 for (iyear=1;iyear<=nyr_CVT_agec;iyear++)
  {
    ErrorFree_CVT_agec(iyear)=N_CVT(yrs_CVT_agec(iyear))/sum(N_CVT(yrs_CVT_agec(iyear)));
    pred_CVT_agec_allages(iyear)=age_error*ErrorFree_CVT_agec(iyear); 
    for (iage=1; iage<=nages_agec_10p; iage++) {pred_CVT_agec(iyear,iage)=pred_CVT_agec_allages(iyear,iage);} 
    for (iage=(nages_agec_10p+1); iage<=nages; iage++) {pred_CVT_agec(iyear,nages_agec_10p)+=pred_CVT_agec_allages(iyear,iage);} //plus group                        
  }
 
  
////--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------   
FUNCTION get_weighted_current 
  F_temp_sum=0.0;
  F_temp_sum+=mfexp((selpar_n_yrs_wgted*log_avg_F_cH+
        sum(log_F_dev_cH((endyr-selpar_n_yrs_wgted+1),endyr)))/selpar_n_yrs_wgted);  
  F_temp_sum+=mfexp((selpar_n_yrs_wgted*log_avg_F_HB+
        sum(log_F_dev_HB((endyr-selpar_n_yrs_wgted+1),endyr)))/selpar_n_yrs_wgted);
  F_temp_sum+=mfexp((selpar_n_yrs_wgted*log_avg_F_GR+
        sum(log_F_dev_GR((endyr-selpar_n_yrs_wgted+1),endyr)))/selpar_n_yrs_wgted);
  F_temp_sum+=mfexp((selpar_n_yrs_wgted*log_avg_F_cH_D+
        sum(log_F_dev_cH_D((endyr-selpar_n_yrs_wgted+1),endyr)))/selpar_n_yrs_wgted);
  F_temp_sum+=mfexp((selpar_n_yrs_wgted*log_avg_F_HB_D+
        sum(log_F_dev_HB_D((endyr-selpar_n_yrs_wgted+1),endyr)))/selpar_n_yrs_wgted);
  F_temp_sum+=mfexp((selpar_n_yrs_wgted*log_avg_F_GR_D+
        sum(log_F_dev_GR_D((endyr-selpar_n_yrs_wgted+1),endyr)))/selpar_n_yrs_wgted);

  F_cH_prop=mfexp((selpar_n_yrs_wgted*log_avg_F_cH+
        sum(log_F_dev_cH((endyr-selpar_n_yrs_wgted+1),endyr)))/selpar_n_yrs_wgted)/F_temp_sum;
  F_HB_prop=mfexp((selpar_n_yrs_wgted*log_avg_F_HB+
        sum(log_F_dev_HB((endyr-selpar_n_yrs_wgted+1),endyr)))/selpar_n_yrs_wgted)/F_temp_sum;
  F_GR_prop=mfexp((selpar_n_yrs_wgted*log_avg_F_GR+
        sum(log_F_dev_GR((endyr-selpar_n_yrs_wgted+1),endyr)))/selpar_n_yrs_wgted)/F_temp_sum;
  F_cH_D_prop=mfexp((selpar_n_yrs_wgted*log_avg_F_cH_D+
        sum(log_F_dev_cH_D((endyr-selpar_n_yrs_wgted+1),endyr)))/selpar_n_yrs_wgted)/F_temp_sum;
  F_HB_D_prop=mfexp((selpar_n_yrs_wgted*log_avg_F_HB_D+
        sum(log_F_dev_HB_D((endyr-selpar_n_yrs_wgted+1),endyr)))/selpar_n_yrs_wgted)/F_temp_sum;
  F_GR_D_prop=mfexp((selpar_n_yrs_wgted*log_avg_F_GR_D+
        sum(log_F_dev_GR_D((endyr-selpar_n_yrs_wgted+1),endyr)))/selpar_n_yrs_wgted)/F_temp_sum;
  
  log_F_dev_end_cH=sum(log_F_dev_cH((endyr-selpar_n_yrs_wgted+1),endyr))/selpar_n_yrs_wgted;
  log_F_dev_end_HB=sum(log_F_dev_HB((endyr-selpar_n_yrs_wgted+1),endyr))/selpar_n_yrs_wgted;  
  log_F_dev_end_GR=sum(log_F_dev_GR((endyr-selpar_n_yrs_wgted+1),endyr))/selpar_n_yrs_wgted;  

  log_F_dev_end_cH_D=sum(log_F_dev_cH_D((endyr-selpar_n_yrs_wgted+1),endyr))/selpar_n_yrs_wgted;
  log_F_dev_end_HB_D=sum(log_F_dev_HB_D((endyr-selpar_n_yrs_wgted+1),endyr))/selpar_n_yrs_wgted;
  log_F_dev_end_GR_D=sum(log_F_dev_GR_D((endyr-selpar_n_yrs_wgted+1),endyr))/selpar_n_yrs_wgted;

  F_end_L=sel_cH(endyr)*mfexp(log_avg_F_cH+log_F_dev_end_cH)+
          sel_HB(endyr)*mfexp(log_avg_F_HB+log_F_dev_end_HB)+
          sel_GR(endyr)*mfexp(log_avg_F_GR+log_F_dev_end_GR);   // (GR uses HB selex ) 

  F_end_D=sel_cH_D(endyr)*mfexp(log_avg_F_cH_D+log_F_dev_end_cH_D)+
          sel_HB_D(endyr)*mfexp(log_avg_F_HB_D+log_F_dev_end_HB_D)+
          sel_GR_D(endyr)*mfexp(log_avg_F_GR_D+log_F_dev_end_GR_D);  // (GR uses HB selex )  
    
  F_end=F_end_L+F_end_D;
  F_end_apex=max(F_end);
  
  sel_wgted_tot=F_end/F_end_apex;
  sel_wgted_L=elem_prod(sel_wgted_tot, elem_div(F_end_L,F_end));
  sel_wgted_D=elem_prod(sel_wgted_tot, elem_div(F_end_D,F_end));
  
  wgt_wgted_L_denom=F_cH_prop+F_HB_prop+F_GR_prop;  
  wgt_wgted_L_klb=F_cH_prop/wgt_wgted_L_denom*wholewgt_cH_klb(endyr)+  
                  F_HB_prop/wgt_wgted_L_denom*wholewgt_HB_klb(endyr)+
                  F_GR_prop/wgt_wgted_L_denom*wholewgt_GR_klb(endyr);                          

  wgt_wgted_D_denom=F_cH_D_prop+F_HB_D_prop+F_GR_D_prop;  
  wgt_wgted_D_klb=F_cH_D_prop/wgt_wgted_D_denom*wholewgt_cH_D_klb(endyr)+ 
                  F_HB_D_prop/wgt_wgted_D_denom*wholewgt_HB_D_klb(endyr)+
                  F_GR_D_prop/wgt_wgted_D_denom*wholewgt_GR_D_klb(endyr);                
  
FUNCTION get_msy
  
  //compute values as functions of F
  for(ff=1; ff<=n_iter_msy; ff++)
  {
    //uses fishery-weighted F's
    Z_age_msy=0.0;
    F_L_age_msy=0.0;
    F_D_age_msy=0.0;
          
    F_L_age_msy=F_msy(ff)*sel_wgted_L;
    F_D_age_msy=F_msy(ff)*sel_wgted_D;    
    Z_age_msy=M+F_L_age_msy+F_D_age_msy;         
    
    N_age_msy(1)=1.0;
    for (iage=2; iage<=nages; iage++)
      {N_age_msy(iage)=N_age_msy(iage-1)*mfexp(-1.*Z_age_msy(iage-1));}
    N_age_msy(nages)=N_age_msy(nages)/(1.0-mfexp(-1.*Z_age_msy(nages)));
    N_age_msy_spawn(1,(nages-1))=elem_prod(N_age_msy(1,(nages-1)),
                                   mfexp((-1.*Z_age_msy(1,(nages-1)))*spawn_time_frac));                 
    N_age_msy_spawn(nages)=(N_age_msy_spawn(nages-1)*(mfexp(-1.*(Z_age_msy(nages-1)*(1.0-spawn_time_frac) + 
                            Z_age_msy(nages)*spawn_time_frac) )))/(1.0-mfexp(-1.*Z_age_msy(nages)));
                     
    spr_msy(ff)=sum(elem_prod(N_age_msy_spawn,reprod));
	        
    R_eq(ff)=SR_eq_func(R0, steep, spr_msy(1), spr_msy(ff), BiasCor, SR_switch);
    
    if (R_eq(ff)<dzero) {R_eq(ff)=dzero;}    
    N_age_msy*=R_eq(ff);
    N_age_msy_spawn*=R_eq(ff);
    
    for (iage=1; iage<=nages; iage++)
    {
      L_age_msy(iage)=N_age_msy(iage)*(F_L_age_msy(iage)/Z_age_msy(iage))*
                      (1.-mfexp(-1.*Z_age_msy(iage)));
      D_age_msy(iage)=N_age_msy(iage)*(F_D_age_msy(iage)/Z_age_msy(iage))*
                      (1.-mfexp(-1.0*Z_age_msy(iage)));                      
    }
    
    SSB_eq(ff)=sum(elem_prod(N_age_msy_spawn,reprod));
	B_eq(ff)=sum(elem_prod(N_age_msy,wgt_mt));
    L_eq_klb(ff)=sum(elem_prod(L_age_msy,wgt_wgted_L_klb)); //in whole weight
    L_eq_knum(ff)=sum(L_age_msy)/1000.0;  
    D_eq_klb(ff)=sum(elem_prod(D_age_msy,wgt_wgted_D_klb)); //in whole weight   
    D_eq_knum(ff)=sum(D_age_msy)/1000.0;    
	
	E_eq_wgt(ff)=(sum(elem_prod(L_age_msy(E_age_min, nages),wgt_wgted_L_klb(E_age_min, nages)))+
				 sum(elem_prod(D_age_msy(E_age_min, nages),wgt_wgted_D_klb(E_age_min, nages))) )/
				 (mt2klb*sum(elem_prod(N_age_msy(E_age_min, nages),wgt_mt(E_age_min, nages))));

	E_eq_num(ff)=(sum(L_age_msy(E_age_min, nages))+sum(D_age_msy(E_age_min, nages)) )/
				 (sum(N_age_msy(E_age_min, nages)));
				 
  }  
  
  msy_klb_out=max(L_eq_klb); //msy in whole weight  
  
  for(ff=1; ff<=n_iter_msy; ff++)
  {
   if(L_eq_klb(ff) == msy_klb_out) 
      {    
        SSB_msy_out=SSB_eq(ff);
        B_msy_out=B_eq(ff);
        R_msy_out=R_eq(ff);
        msy_knum_out=L_eq_knum(ff);
        D_msy_knum_out=D_eq_knum(ff);
        D_msy_klb_out=D_eq_klb(ff);        
        F_msy_out=F_msy(ff);  
        spr_msy_out=spr_msy(ff);      
		E_msy_wgt=E_eq_wgt(ff);
		E_msy_num=E_eq_num(ff);
      }
  }

//--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------   
FUNCTION get_per_recruit_stuff

  //static per-recruit stuff
 
  for(iyear=styr; iyear<=endyr; iyear++)
  {
    N_age_spr(1)=1.0;
    for(iage=2; iage<=nages; iage++)
     {N_age_spr(iage)=N_age_spr(iage-1)*mfexp(-1.*Z(iyear,iage-1));}
    N_age_spr(nages)=N_age_spr(nages)/(1.0-mfexp(-1.*Z(iyear,nages)));    
    N_age_spr_spawn(1,(nages-1))=elem_prod(N_age_spr(1,(nages-1)),
                                mfexp(-1.*Z(iyear)(1,(nages-1))*spawn_time_frac));
    N_age_spr_spawn(nages)=(N_age_spr_spawn(nages-1)*
                          (mfexp(-1.*(Z(iyear)(nages-1)*(1.0-spawn_time_frac) + Z(iyear)(nages)*spawn_time_frac) )))
                          /(1.0-mfexp(-1.*Z(iyear)(nages)));           
    spr_static(iyear)=sum(elem_prod(N_age_spr_spawn,reprod))/spr_F0;
  }
 
  //compute SSB/R and YPR as functions of F
  for(ff=1; ff<=n_iter_spr; ff++)
  {
    //uses fishery-weighted F's, same as in MSY calculations
    Z_age_spr=0.0;
    F_L_age_spr=0.0;
    F_D_age_spr=0.0;
    
	F_L_age_spr=F_spr(ff)*sel_wgted_L;
    F_D_age_spr=F_spr(ff)*sel_wgted_D;
    Z_age_spr=M+F_L_age_spr+F_D_age_spr;    

    N_age_spr(1)=1.0;
    for (iage=2; iage<=nages; iage++)
     {N_age_spr(iage)=N_age_spr(iage-1)*mfexp(-1.*Z_age_spr(iage-1));}
    N_age_spr(nages)=N_age_spr(nages)/(1-mfexp(-1.*Z_age_spr(nages)));
    N_age_spr_spawn(1,(nages-1))=elem_prod(N_age_spr(1,(nages-1)),
                                   mfexp((-1.*Z_age_spr(1,(nages-1)))*spawn_time_frac));                 
    N_age_spr_spawn(nages)=(N_age_spr_spawn(nages-1)*
                          (mfexp(-1.*(Z_age_spr(nages-1)*(1.0-spawn_time_frac) + Z_age_spr(nages)*spawn_time_frac) )))
                          /(1.0-mfexp(-1.*Z_age_spr(nages)));
    spr_spr(ff)=sum(elem_prod(N_age_spr_spawn,reprod));
	L_spr(ff)=0.0;
    for (iage=1; iage<=nages; iage++)
    {
      L_age_spr(iage)=N_age_spr(iage)*(F_L_age_spr(iage)/Z_age_spr(iage))*
                      (1.-mfexp(-1.*Z_age_spr(iage)));
      L_spr(ff)+=L_age_spr(iage)*wgt_wgted_L_klb(iage)*1000.0; //in lb whole wgt
    }   
  }
  spr_ratio=spr_spr/spr_F0;
  F20_dum=min(fabs(spr_ratio-0.2));
  F30_dum=min(fabs(spr_ratio-0.3));
  F40_dum=min(fabs(spr_ratio-0.4));
  for(ff=1; ff<=n_iter_spr; ff++)
  {   
      if (fabs(spr_ratio(ff)-0.2)==F20_dum) {F20_out=F_spr(ff);}	  
	  if (fabs(spr_ratio(ff)-0.3)==F30_dum) {F30_out=F_spr(ff);}
	  if (fabs(spr_ratio(ff)-0.4)==F40_dum) {F40_out=F_spr(ff);}	
  }
    
	rec=column(N,1);	  
    switch(SPR_rec_switch){
       case 1: //Observed mean over years specified
          rec_mean=sum(rec(styr_rec_spr, endyr_rec_spr))/nyrs_rec_spr;
          break;
       case 2: //Expected recruitment is R0 with bias correction (if that feature is turned on)
          rec_mean=BiasCor*R0;
          break;
       default: // no such switch available
          cout << "Error in input: SPR_rec_switch must be set to 1 or 2." << endl;
          cout << "Presently it is set to " << SPR_rec_switch <<"."<< endl;
          exit(0);          
	}	  
   

  R_F30_out=rec_mean;
  F_L_age_spr=F30_out*sel_wgted_L;
  F_D_age_spr=F30_out*sel_wgted_D;
  Z_age_spr=M+F_L_age_spr+F_D_age_spr;

  N_age_spr(1)=R_F30_out;
  for (iage=2; iage<=nages; iage++)
     {N_age_spr(iage)=N_age_spr(iage-1)*mfexp(-1.*Z_age_spr(iage-1));}
  N_age_spr(nages)=N_age_spr(nages)/(1-mfexp(-1.*Z_age_spr(nages)));
  N_age_spr_spawn(1,(nages-1))=elem_prod(N_age_spr(1,(nages-1)),
                                   mfexp((-1.*Z_age_spr(1,(nages-1)))*spawn_time_frac));                 
  N_age_spr_spawn(nages)=(N_age_spr_spawn(nages-1)*
                          (mfexp(-1.*(Z_age_spr(nages-1)*(1.0-spawn_time_frac) + Z_age_spr(nages)*spawn_time_frac) )))
                          /(1.0-mfexp(-1.*Z_age_spr(nages)));

  for (iage=1; iage<=nages; iage++)
    {
      L_age_F30(iage)=N_age_spr(iage)*(F_L_age_spr(iage)/Z_age_spr(iage))*
                      (1.-mfexp(-1.*Z_age_spr(iage)));
      D_age_F30(iage)=N_age_spr(iage)*(F_D_age_spr(iage)/Z_age_spr(iage))*
                      (1.-mfexp(-1.0*Z_age_spr(iage)));                      
    }
		
  SSB_F30_out=sum(elem_prod(N_age_spr_spawn,reprod));
  B_F30_out=sum(elem_prod(N_age_spr,wgt_mt));
  L_F30_klb_out=sum(elem_prod(L_age_F30,wgt_wgted_L_klb)); //in whole weight
  L_F30_knum_out=sum(L_age_F30)/1000.0;  
  D_F30_klb_out=sum(elem_prod(D_age_F30,wgt_wgted_D_klb)); //in whole weight   
  D_F30_knum_out=sum(D_age_F30)/1000.0;    
 
  E_F30_wgt=(sum(elem_prod(L_age_F30(E_age_min, nages),wgt_wgted_L_klb(E_age_min, nages)))+
				 sum(elem_prod(D_age_F30(E_age_min, nages),wgt_wgted_D_klb(E_age_min, nages))) )/
				 (mt2klb*sum(elem_prod(N_age_spr(E_age_min, nages),wgt_mt(E_age_min, nages))));

  E_F30_num=(sum(L_age_F30(E_age_min, nages))+sum(D_age_F30(E_age_min, nages)) )/
				 (sum(N_age_spr(E_age_min, nages))); 
  
//--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------   
FUNCTION get_miscellaneous_stuff

//switch here if var_rec_dev <=dzero 
  if(var_rec_dev>0.0)
   {sigma_rec_dev=sqrt(var_rec_dev);} //pow(var_rec_dev,0.5);  //sample SD of predicted residuals (may not equal rec_sigma)  
   else{sigma_rec_dev=0.0;}

  len_cv=elem_div(len_sd,meanlen_TL);
  len_cv_L=elem_div(len_sd_L,meanlen_TL_L);
  len_cv_20=elem_div(len_sd_20,meanlen_TL_20);
  
 //Time series of interest  
  B(endyr+1)=elem_prod(N(endyr+1),wgt_mt);
  totN(endyr+1)=sum(N(endyr+1));
  totB(endyr+1)=sum(B(endyr+1));  
  SdS0=SSB/S0;
   
  
  //compute total landings- and discards-at-age in 1000 fish and klb whole weight
  L_total_num.initialize();
  L_total_klb.initialize();
  L_total_knum_yr.initialize();
  L_total_klb_yr.initialize();  
  D_total_num.initialize();
  D_total_klb.initialize();
  D_total_knum_yr.initialize();
  D_total_klb_yr.initialize();
  D_cH_klb.initialize();
  D_HB_klb.initialize();
  D_GR_klb.initialize();
  E_total_wgt.initialize();
  E_L_wgt.initialize();
  E_D_wgt.initialize();
  E_total_num.initialize();
  E_L_num.initialize();
  E_D_num.initialize();
  
  for(iyear=styr; iyear<=endyr; iyear++)
  {
        L_total_klb_yr(iyear)=pred_cH_L_klb(iyear)+pred_HB_L_klb(iyear)+pred_GR_L_klb(iyear);
        L_total_knum_yr(iyear)=pred_cH_L_knum(iyear)+pred_HB_L_knum(iyear)+pred_GR_L_knum(iyear);
                
        B(iyear)=elem_prod(N(iyear),wgt_mt);
        totN(iyear)=sum(N(iyear));
        totB(iyear)=sum(B(iyear));   
        
        if (iyear>=styr_cH_D && iyear<=endyr_cH_D)
        {
         D_total_knum_yr(iyear)+=pred_cH_D_knum(iyear);
         D_total_klb_yr(iyear)+=pred_cH_D_klb(iyear);
         D_cH_klb(iyear)=elem_prod(D_cH_num(iyear),wholewgt_cH_D_klb(iyear));     //in 1000 lb 
        }
                
        if (iyear>=styr_HB_D && iyear<=endyr_HB_D)
        {
         D_total_knum_yr(iyear)+=pred_HB_D_knum(iyear);
         D_total_klb_yr(iyear)+=pred_HB_D_klb(iyear);
         D_HB_klb(iyear)=elem_prod(D_HB_num(iyear),wholewgt_HB_D_klb(iyear));     //in 1000 lb 
        }    
    	
        if (iyear>=styr_GR_D && iyear<=endyr_GR_D)
        {
         D_total_knum_yr(iyear)+=pred_GR_D_knum(iyear);
         D_total_klb_yr(iyear)+=pred_GR_D_klb(iyear);
         D_GR_klb(iyear)=elem_prod(D_GR_num(iyear),wholewgt_GR_D_klb(iyear));     //in 1000 lb 
        }                          
      
	E_L_num(iyear)= (sum(L_cH_num(iyear)(E_age_min, nages)) + 
	                 sum(L_HB_num(iyear)(E_age_min, nages)) + 
					 sum(L_GR_num(iyear)(E_age_min, nages)))/
					 sum(N(iyear)(E_age_min, nages));
	E_D_num(iyear)= (sum(D_cH_num(iyear)(E_age_min, nages)) + 
					 sum(D_HB_num(iyear)(E_age_min, nages)) + 
					 sum(D_GR_num(iyear)(E_age_min, nages)))/
					 sum(N(iyear)(E_age_min, nages));
	
    E_total_num=E_L_num+E_D_num;	
					 

	E_L_wgt(iyear)= (sum(L_cH_klb(iyear)(E_age_min, nages)) + 
	                 sum(L_HB_klb(iyear)(E_age_min, nages)) + 
					 sum(L_GR_klb(iyear)(E_age_min, nages)))/
					 (mt2klb*sum(B(iyear)(E_age_min, nages)));		
	E_D_wgt(iyear)= (sum(D_cH_klb(iyear)(E_age_min, nages)) + 
	                 sum(D_HB_klb(iyear)(E_age_min, nages)) + 
					 sum(D_GR_klb(iyear)(E_age_min, nages)))/
					 (mt2klb*sum(B(iyear)(E_age_min, nages)));		
					 
    E_total_wgt=E_L_wgt+E_D_wgt;	
   
  }
  
  
  L_total_num=L_cH_num+L_HB_num+L_GR_num;   //landings at age in number fish
  L_total_klb=L_cH_klb+L_HB_klb+L_GR_klb;   //landings at age in klb whole weight
 
  D_total_num=(D_cH_num+D_HB_num+D_GR_num);          //discards at age in number fish
  D_total_klb=D_cH_klb+D_HB_klb+D_GR_klb;            //discards at age in klb whole weight
 
  
  
  Fend_mean_temp=1.0;
  for (iyear=1; iyear<=selpar_n_yrs_wgted; iyear++) {Fend_mean_temp*=Fapex(endyr-iyear+1);}
  Fend_mean=pow(Fend_mean_temp,(1.0/selpar_n_yrs_wgted));	  
  
  Eend_mean_temp_wgt=1.0;
  for (iyear=1; iyear<=selpar_n_yrs_wgted; iyear++) {Eend_mean_temp_wgt*=E_total_wgt(endyr-iyear+1);}
  E_wgt_end_mean=pow(Eend_mean_temp_wgt,(1.0/selpar_n_yrs_wgted));	  
  
  Eend_mean_temp_num=1.0;
  for (iyear=1; iyear<=selpar_n_yrs_wgted; iyear++) {Eend_mean_temp_num*=E_total_num(endyr-iyear+1);}
  E_num_end_mean=pow(Eend_mean_temp_num,(1.0/selpar_n_yrs_wgted));	  
    
  
  if(F_msy_out>0)
    {
      FdF_msy=Fapex/F_msy_out;
      FdF_msy_end=FdF_msy(endyr);
      FdF_msy_end_mean=Fend_mean/F_msy_out;
    }
  if(SSB_msy_out>0)
    {
      SdSSB_msy=SSB/SSB_msy_out;
      SdSSB_msy_end=SdSSB_msy(endyr);
    }  

	if(F30_out>0)
    {
	  FdF30=Fapex/F30_out;
	  FdF30_end_mean=Fend_mean/F30_out;
	}
  if(SSB_F30_out>0)
    {
      SdSSB_F30=SSB/SSB_F30_out;
	  Sdmsst_F30=SSB/(smsy2msst75*SSB_F30_out);
      SdSSB_F30_end=SdSSB_F30(endyr);
	  Sdmsst_F30_end=Sdmsst_F30(endyr);
    }  	
   if (E_msy_wgt>0)
    {
	  EdEmsy_wgt=E_total_wgt/E_msy_wgt;
	  EdEF30_wgt=E_total_wgt/E_F30_wgt;
	  EdEmsy_wgt_end_mean=E_wgt_end_mean/E_msy_wgt;
	  EdEF30_wgt_end_mean=E_wgt_end_mean/E_F30_wgt;
    }	   

   if (E_msy_num>0)
    {
	  EdEmsy_num=E_total_num/E_msy_num;
	  EdEF30_num=E_total_wgt/E_F30_num;
	  EdEmsy_num_end_mean=E_num_end_mean/E_msy_num;
	  EdEF30_num_end_mean=E_num_end_mean/E_F30_num;
	}	   
   
   //fill in log recruitment deviations for yrs they are nonzero
   for(iyear=styr_rec_dev; iyear<=endyr_rec_dev; iyear++)
     {log_rec_dev_output(iyear)=log_rec_dev(iyear);}
   //fill in log Nage deviations for ages they are nonzero (ages2+)
   for(iage=2; iage<=nages; iage++)
     {log_Nage_dev_output(iage)=log_Nage_dev(iage);}

//--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------     
FUNCTION get_effective_sample_sizes
      neff_cH_lenc_allyr_out=missing;
	  neff_HB_D_lenc_allyr_out=missing; 
	  neff_GR_D_lenc_allyr_out=missing; 
   
      neff_cH_agec_allyr_out=missing;
      neff_HB_agec_allyr_out=missing;
	  neff_CVT_agec_allyr_out=missing;
	  neff_GR_agec_allyr_out=missing;
        
      for (iyear=1; iyear<=nyr_cH_lenc; iyear++)
         {if (nsamp_cH_lenc(iyear)>=minSS_cH_lenc)
            {neff_cH_lenc_allyr_out(yrs_cH_lenc(iyear))=multinom_eff_N(pred_cH_lenc(iyear),obs_cH_lenc(iyear));} 
          else {neff_cH_lenc_allyr_out(yrs_cH_lenc(iyear))=-99;}
         }

      for (iyear=1; iyear<=nyr_HB_D_lenc; iyear++)
         {if (nsamp_HB_D_lenc(iyear)>=minSS_HB_D_lenc)
            {neff_HB_D_lenc_allyr_out(yrs_HB_D_lenc(iyear))=multinom_eff_N(pred_HB_D_lenc(iyear),obs_HB_D_lenc(iyear));} 
          else {neff_HB_D_lenc_allyr_out(yrs_HB_D_lenc(iyear))=-99;}
         }

     for (iyear=1; iyear<=nyr_GR_D_lenc; iyear++)
         {if (nsamp_GR_D_lenc(iyear)>=minSS_GR_D_lenc)
            {neff_GR_D_lenc_allyr_out(yrs_GR_D_lenc(iyear))=multinom_eff_N(pred_GR_D_lenc(iyear),obs_GR_D_lenc(iyear));} 
          else {neff_GR_D_lenc_allyr_out(yrs_GR_D_lenc(iyear))=-99;}
         }
		 
      for (iyear=1; iyear<=nyr_cH_agec; iyear++)
         {if (nsamp_cH_agec(iyear)>=minSS_cH_agec)
            {neff_cH_agec_allyr_out(yrs_cH_agec(iyear))=multinom_eff_N(pred_cH_agec(iyear),obs_cH_agec(iyear));}                            
          else {neff_cH_agec_allyr_out(yrs_cH_agec(iyear))=-99;}
         }    
         
      for (iyear=1; iyear<=nyr_HB_agec; iyear++)
         {if (nsamp_HB_agec(iyear)>=minSS_HB_agec)
            {neff_HB_agec_allyr_out(yrs_HB_agec(iyear))=multinom_eff_N(pred_HB_agec(iyear),obs_HB_agec(iyear));}                            
          else {neff_HB_agec_allyr_out(yrs_HB_agec(iyear))=-99;}
         }
		 
	  for (iyear=1; iyear<=nyr_CVT_agec; iyear++)  
           {if (nsamp_CVT_agec(iyear)>=minSS_CVT_agec)
              {neff_CVT_agec_allyr_out(yrs_CVT_agec(iyear))=multinom_eff_N(pred_CVT_agec(iyear),obs_CVT_agec(iyear));}                            
            else {neff_CVT_agec_allyr_out(yrs_CVT_agec(iyear))=-99;}
           }
	   
	  for (iyear=1; iyear<=nyr_GR_agec; iyear++)  
           {if (nsamp_GR_agec(iyear)>=minSS_GR_agec)
              {neff_GR_agec_allyr_out(yrs_GR_agec(iyear))=multinom_eff_N(pred_GR_agec(iyear),obs_GR_agec(iyear));}                            
            else {neff_GR_agec_allyr_out(yrs_GR_agec(iyear))=-99;}
           }
                          
//--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------   


FUNCTION evaluate_objective_function
  //fval=square(xdum-9.0);
  
  fval=0.0;
  fval_data=0.0;  
//---likelihoods---------------------------

//---Indices-------------------------------

  f_cH_cpue=0.0;
  f_cH_cpue=lk_lognormal(pred_cH_cpue, obs_cH_cpue, cH_cpue_cv, w_I_cH);
  fval+=f_cH_cpue;
  fval_data+=f_cH_cpue;  
	
  f_HB_cpue=0.0;
  f_HB_cpue=lk_lognormal(pred_HB_cpue, obs_HB_cpue, HB_cpue_cv, w_I_HB);
  fval+=f_HB_cpue;
  fval_data+=f_HB_cpue;  
   
  f_HB_D_cpue=0.0;
  f_HB_D_cpue=lk_lognormal(pred_HB_D_cpue, obs_HB_D_cpue, HB_D_cpue_cv, w_I_HB_D);
  fval+=f_HB_D_cpue;
  fval_data+=f_HB_D_cpue;  
  
  f_CVT_cpue=0.0;
  f_CVT_cpue=lk_lognormal(pred_CVT_cpue, obs_CVT_cpue, CVT_cpue_cv, w_I_CVT);
  fval+=w_I_CVT_mult*f_CVT_cpue;
  fval_data+=f_CVT_cpue;  

  f_VID_cpue=0.0;
  f_VID_cpue=lk_lognormal(pred_VID_cpue, obs_VID_cpue, VID_cpue_cv, w_I_VID);
  fval+=w_I_VID_mult*f_VID_cpue;
  fval_data+=f_VID_cpue;  
  
//---Landings-------------------------------
  

  //f_cH_L in 1000 lb whole wgt  
  f_cH_L=lk_lognormal(pred_cH_L_klb(styr_cH_L,endyr_cH_L), obs_cH_L(styr_cH_L,endyr_cH_L),
                      cH_L_cv(styr_cH_L,endyr_cH_L), w_L);
  fval+=f_cH_L;
  fval_data+=f_cH_L;

  //f_HB_L in 1000 fish
  f_HB_L=lk_lognormal(pred_HB_L_knum(styr_HB_L,endyr_HB_L), obs_HB_L(styr_HB_L,endyr_HB_L), 
                      HB_L_cv(styr_HB_L,endyr_HB_L), w_L);
  fval+=f_HB_L;
  fval_data+=f_HB_L;  

  //f_GR_L in 1000 fish
  f_GR_L=lk_lognormal(pred_GR_L_knum(styr_GR_L,endyr_GR_L), obs_GR_L(styr_GR_L,endyr_GR_L), 
                      GR_L_cv(styr_GR_L,endyr_GR_L), w_L);
  fval+=f_GR_L;
  fval_data+=f_GR_L;  


//---Discards-------------------------------
  
  //f_cH_D in 1000 fish
  f_cH_D=lk_lognormal(pred_cH_D_knum(styr_cH_D,endyr_cH_D), obs_cH_D(styr_cH_D,endyr_cH_D), 
                      cH_D_cv(styr_cH_D,endyr_cH_D), w_D);
  fval+=f_cH_D;
  fval_data+=f_cH_D;  

  //f_HB_D in 1000 fish
  f_HB_D=lk_lognormal(pred_HB_D_knum(styr_HB_D,endyr_HB_D), obs_HB_D(styr_HB_D,endyr_HB_D), 
                      HB_D_cv(styr_HB_D,endyr_HB_D), w_D);
  fval+=f_HB_D;
  fval_data+=f_HB_D;  

  //f_GR_D in 1000 fish
  f_GR_D=lk_lognormal(pred_GR_D_knum(styr_GR_D,endyr_GR_D), obs_GR_D(styr_GR_D,endyr_GR_D), 
                      GR_D_cv(styr_GR_D,endyr_GR_D), w_D);
  fval+=f_GR_D;
  fval_data+=f_GR_D;  
  
//---Length comps-------------------------------

  //f_cH_lenc
  //f_cH_lenc=lk_robust_multinomial(nsamp_cH_lenc, pred_cH_lenc, obs_cH_lenc, nyr_cH_lenc, double(nlenbins), minSS_cH_lenc, w_lc_cH);
  f_cH_lenc=lk_dirichlet_multinomial(nsamp_cH_lenc, pred_cH_lenc, obs_cH_lenc, nyr_cH_lenc, double(nlenbins), minSS_cH_lenc, log_dm_cH_lc);
  fval+=f_cH_lenc;
  fval_data+=f_cH_lenc;
 
  //f_cH_D_lenc
  //f_cH_D_lenc=lk_robust_multinomial(nsamp_cH_D_lenc, pred_cH_D_lenc, obs_cH_D_lenc, nyr_cH_D_lenc, double(nlenbins), minSS_cH_D_lenc, w_lc_cH_D);
  f_cH_D_lenc=lk_dirichlet_multinomial(nsamp_cH_D_lenc, pred_cH_D_lenc, obs_cH_D_lenc, nyr_cH_D_lenc, double(nlenbins), minSS_cH_D_lenc, log_dm_cH_D_lc);
  fval+=f_cH_D_lenc;
  fval_data+=f_cH_D_lenc;
  
  //f_HB_D_lenc
  //f_HB_D_lenc=lk_robust_multinomial(nsamp_HB_D_lenc, pred_HB_D_lenc, obs_HB_D_lenc, nyr_HB_D_lenc, double(nlenbins), minSS_HB_D_lenc, w_lc_HB_D);
  f_HB_D_lenc=lk_dirichlet_multinomial(nsamp_HB_D_lenc, pred_HB_D_lenc, obs_HB_D_lenc, nyr_HB_D_lenc, double(nlenbins), minSS_HB_D_lenc, log_dm_HB_D_lc); 
  fval+=f_HB_D_lenc;
  fval_data+=f_HB_D_lenc;

  //f_GR_D_lenc
  //f_GR_D_lenc=lk_robust_multinomial(nsamp_GR_D_lenc, pred_GR_D_lenc, obs_GR_D_lenc, nyr_GR_D_lenc, double(nlenbins), minSS_GR_D_lenc, w_lc_GR_D);
  f_GR_D_lenc=lk_dirichlet_multinomial(nsamp_GR_D_lenc, pred_GR_D_lenc, obs_GR_D_lenc, nyr_GR_D_lenc, double(nlenbins), minSS_GR_D_lenc, log_dm_GR_D_lc); 
  fval+=f_GR_D_lenc;
  fval_data+=f_GR_D_lenc;
  
//---Age comps-------------------------------

  //f_cH_agec
  //f_cH_agec=lk_robust_multinomial(nsamp_cH_agec, pred_cH_agec, obs_cH_agec, nyr_cH_agec, double(nages_agec), minSS_cH_agec, w_ac_cH);
  f_cH_agec=lk_dirichlet_multinomial(nsamp_cH_agec, pred_cH_agec, obs_cH_agec, nyr_cH_agec, double(nages_agec), minSS_cH_agec, log_dm_cH_ac);
  fval+=f_cH_agec;
  fval_data+=f_cH_agec;

  //f_HB_agec
  //f_HB_agec=lk_robust_multinomial(nsamp_HB_agec, pred_HB_agec, obs_HB_agec, nyr_HB_agec, double(nages_agec_10p), minSS_HB_agec, w_ac_HB);
  f_HB_agec=lk_dirichlet_multinomial(nsamp_HB_agec, pred_HB_agec, obs_HB_agec, nyr_HB_agec, double(nages_agec_10p), minSS_HB_agec, log_dm_HB_ac);
  fval+=f_HB_agec;
  fval_data+=f_HB_agec;
  
  //f_CVT_agec
  //f_CVT_agec=lk_robust_multinomial(nsamp_CVT_agec, pred_CVT_agec, obs_CVT_agec, nyr_CVT_agec, double(nages_agec_10p), minSS_CVT_agec, w_ac_CVT);
  f_CVT_agec=lk_dirichlet_multinomial(nsamp_CVT_agec, pred_CVT_agec, obs_CVT_agec, nyr_CVT_agec, double(nages_agec_10p), minSS_CVT_agec, log_dm_CVT_ac);  
  fval+=f_CVT_agec;
  fval_data+=f_CVT_agec;
  
  //f_GR_agec
  //f_GR_agec=lk_robust_multinomial(nsamp_GR_agec, pred_GR_agec, obs_GR_agec, nyr_GR_agec, double(nages_agec_10p), minSS_GR_agec, w_ac_GR);
  f_GR_agec=lk_dirichlet_multinomial(nsamp_GR_agec, pred_GR_agec, obs_GR_agec, nyr_GR_agec, double(nages_agec_10p), minSS_GR_agec, log_dm_GR_ac);  
  fval+=f_GR_agec;
  fval_data+=f_GR_agec;

//-----------Constraints and penalties--------------------------------
  
  //Light penalty applied to log_Nage_dev for deviation from zero. If not estimated, this penalty equals zero.
  f_Nage_init=norm2(log_Nage_dev);        
  fval+=w_Nage_init*f_Nage_init;
  
  f_rec_dev=0.0;
  //rec_sigma_sq=square(rec_sigma);
  rec_logL_add=nyrs_rec*log(rec_sigma);
  f_rec_dev=(square(log_rec_dev(styr_rec_dev) + rec_sigma_sq/2.0)/(2.0*rec_sigma_sq));
  for(iyear=(styr_rec_dev+1); iyear<=endyr_rec_dev; iyear++)
  {f_rec_dev+=(square(log_rec_dev(iyear)-R_autocorr*log_rec_dev(iyear-1) + rec_sigma_sq/2.0)/
               (2.0*rec_sigma_sq));}
  f_rec_dev+=rec_logL_add;            
  fval+=w_rec*f_rec_dev;
   
  f_rec_dev_early=0.0; //possible extra constraint on early rec deviations
  if (w_rec_early>0.0)
    { if (styr_rec_dev<endyr_rec_phase1)
        {  
          for(iyear=styr_rec_dev; iyear<=endyr_rec_phase1; iyear++)
          //{f_rec_dev_early+=(square(log_rec_dev(iyear)-R_autocorr*log_rec_dev(iyear-1) + rec_sigma_sq/2.0)/
          //                  (2.0*rec_sigma_sq)) + rec_logL_add;}
          {f_rec_dev_early+=square(log_rec_dev(iyear));}
        }
  fval+=w_rec_early*f_rec_dev_early;
  }
  
  f_rec_dev_end=0.0; //possible extra constraint on ending rec deviations
  if (w_rec_end>0.0)
  { if (endyr_rec_phase2<endyr_rec_dev)
        {  
          for(iyear=(endyr_rec_phase2+1); iyear<=endyr_rec_dev; iyear++)
          //{f_rec_dev_end+=(square(log_rec_dev(iyear)-R_autocorr*log_rec_dev(iyear-1) + rec_sigma_sq/2.0)/
          //                 (2.0*rec_sigma_sq)) + rec_logL_add;}
          {f_rec_dev_end+=square(log_rec_dev(iyear));}
        }
      fval+=w_rec_end*f_rec_dev_end;
   }  

  //Ftune penalty: does not apply in last phase
  f_Ftune=0.0; 
  if (w_Ftune>0.0)
  {if (set_Ftune>0.0 && !last_phase()) {f_Ftune=square(Fapex(set_Ftune_yr)-set_Ftune);}
   fval+=w_Ftune*f_Ftune;
  }

  //Penalty if apical F exceeds 3.0
  f_fullF_constraint=0.0;
  if (w_fullF>0.0)
  {for (iyear=styr; iyear<=endyr; iyear++)
        {if(Fapex(iyear)>3.0) {f_fullF_constraint+=(mfexp(Fapex(iyear)-3.0)-1.0);}}
   fval+=w_fullF*f_fullF_constraint;
  }
  
//  //Random walk components of fishery dependent indices
//  f_HB_RW_cpue=0.0;
//  for (iyear=styr_HB_cpue; iyear<endyr_HB_cpue; iyear++)
//      {f_HB_RW_cpue+=square(q_RW_log_dev_HB(iyear))/(2.0*set_q_RW_HB_var);}
//  fval+=f_HB_RW_cpue;   

  
//---Priors---------------------------------------------------
//neg_log_prior arguments: estimate, prior mean, prior var/-CV, pdf type
//Variance input as a negative value is considered to be CV in arithmetic space (CV=-1 implies loose prior) 
//pdf type 1=none, 2=lognormal, 3=normal, 4=beta 
  f_priors=0.0; 
  f_priors+=neg_log_prior(len_cv_val,set_len_cv(5),set_len_cv(6),set_len_cv(7));
  f_priors+=neg_log_prior(len_cv_val_20,set_len_cv_20(5),set_len_cv_20(6),set_len_cv_20(7));
  f_priors+=neg_log_prior(len_cv_val_L,set_len_cv_L(5),set_len_cv_L(6),set_len_cv_L(7));
   
  //f_priors+=neg_log_prior(steep,set_steep(5),set_log_R0(6),set_log_R0(7)); 
  f_priors+=neg_log_prior(log_R0,set_log_R0(5),set_log_R0(6),set_log_R0(7)); 
  f_priors+=neg_log_prior(R_autocorr,set_R_autocorr(5),set_R_autocorr(6),set_R_autocorr(7));
  f_priors+=neg_log_prior(rec_sigma,set_rec_sigma(5),set_rec_sigma(6),set_rec_sigma(7));
 
  f_priors+=neg_log_prior(selpar_A50_cH1,set_selpar_A50_cH1(5), set_selpar_A50_cH1(6), set_selpar_A50_cH1(7));
  f_priors+=neg_log_prior(selpar_slope_cH1,set_selpar_slope_cH1(5), set_selpar_slope_cH1(6), set_selpar_slope_cH1(7));
  f_priors+=neg_log_prior(selpar_A50_cH2,set_selpar_A50_cH2(5), set_selpar_A50_cH2(6), set_selpar_A50_cH2(7));
  f_priors+=neg_log_prior(selpar_slope_cH2,set_selpar_slope_cH2(5), set_selpar_slope_cH2(6), set_selpar_slope_cH2(7));
  f_priors+=neg_log_prior(selpar_A50_cH3,set_selpar_A50_cH3(5), set_selpar_A50_cH3(6), set_selpar_A50_cH3(7));
  f_priors+=neg_log_prior(selpar_slope_cH3,set_selpar_slope_cH3(5), set_selpar_slope_cH3(6), set_selpar_slope_cH3(7));

  f_priors+=neg_log_prior(selpar_A50_HB1,set_selpar_A50_HB1(5), set_selpar_A50_HB1(6), set_selpar_A50_HB1(7));
  f_priors+=neg_log_prior(selpar_slope_HB1,set_selpar_slope_HB1(5), set_selpar_slope_HB1(6), set_selpar_slope_HB1(7));
  f_priors+=neg_log_prior(selpar_A502_HB1,set_selpar_A502_HB1(5), set_selpar_A502_HB1(6), set_selpar_A502_HB1(7));
  f_priors+=neg_log_prior(selpar_slope2_HB1,set_selpar_slope2_HB1(5), set_selpar_slope2_HB1(6), set_selpar_slope2_HB1(7));
  f_priors+=neg_log_prior(selpar_A50_HB2,set_selpar_A50_HB2(5), set_selpar_A50_HB2(6), set_selpar_A50_HB2(7));
  f_priors+=neg_log_prior(selpar_slope_HB2,set_selpar_slope_HB2(5), set_selpar_slope_HB2(6), set_selpar_slope_HB2(7));
  f_priors+=neg_log_prior(selpar_A502_HB2,set_selpar_A502_HB2(5), set_selpar_A502_HB2(6), set_selpar_A502_HB2(7));
  f_priors+=neg_log_prior(selpar_slope2_HB2,set_selpar_slope2_HB2(5), set_selpar_slope2_HB2(6), set_selpar_slope2_HB2(7));
  f_priors+=neg_log_prior(selpar_A50_HB3,set_selpar_A50_HB3(5), set_selpar_A50_HB3(6), set_selpar_A50_HB3(7));
  f_priors+=neg_log_prior(selpar_slope_HB3,set_selpar_slope_HB3(5), set_selpar_slope_HB3(6), set_selpar_slope_HB3(7));
  f_priors+=neg_log_prior(selpar_A502_HB3,set_selpar_A502_HB3(5), set_selpar_A502_HB3(6), set_selpar_A502_HB3(7));
  f_priors+=neg_log_prior(selpar_slope2_HB3,set_selpar_slope2_HB3(5), set_selpar_slope2_HB3(6), set_selpar_slope2_HB3(7));

  f_priors+=neg_log_prior(selpar_A50_GR2,set_selpar_A50_GR2(5), set_selpar_A50_GR2(6), set_selpar_A50_GR2(7));
  f_priors+=neg_log_prior(selpar_slope_GR2,set_selpar_slope_GR2(5), set_selpar_slope_GR2(6), set_selpar_slope_GR2(7));
  f_priors+=neg_log_prior(selpar_A502_GR2,set_selpar_A502_GR2(5), set_selpar_A502_GR2(6), set_selpar_A502_GR2(7));
  f_priors+=neg_log_prior(selpar_slope2_GR2,set_selpar_slope2_GR2(5), set_selpar_slope2_GR2(6), set_selpar_slope2_GR2(7));

  f_priors+=neg_log_prior(selpar_A50_GR3,set_selpar_A50_GR3(5), set_selpar_A50_GR3(6), set_selpar_A50_GR3(7));
  f_priors+=neg_log_prior(selpar_slope_GR3,set_selpar_slope_GR3(5), set_selpar_slope_GR3(6), set_selpar_slope_GR3(7));
  
  f_priors+=neg_log_prior(selpar_A50_cH2_D,set_selpar_A50_cH2_D(5), set_selpar_A50_cH2_D(6), set_selpar_A50_cH2_D(7));
  f_priors+=neg_log_prior(selpar_slope_cH2_D,set_selpar_slope_cH2_D(5), set_selpar_slope_cH2_D(6), set_selpar_slope_cH2_D(7));
  f_priors+=neg_log_prior(selpar_A502_cH2_D,set_selpar_A502_cH2_D(5), set_selpar_A502_cH2_D(6), set_selpar_A502_cH2_D(7));
  f_priors+=neg_log_prior(selpar_slope2_cH2_D,set_selpar_slope2_cH2_D(5), set_selpar_slope2_cH2_D(6), set_selpar_slope2_cH2_D(7));
  
  f_priors+=neg_log_prior(selpar_A50_cH3_D,set_selpar_A50_cH3_D(5), set_selpar_A50_cH3_D(6), set_selpar_A50_cH3_D(7));
  f_priors+=neg_log_prior(selpar_slope_cH3_D,set_selpar_slope_cH3_D(5), set_selpar_slope_cH3_D(6), set_selpar_slope_cH3_D(7));
  
  f_priors+=neg_log_prior(selpar_A50_HB2_D,set_selpar_A50_HB2_D(5), set_selpar_A50_HB2_D(6), set_selpar_A50_HB2_D(7));
  f_priors+=neg_log_prior(selpar_slope_HB2_D,set_selpar_slope_HB2_D(5), set_selpar_slope_HB2_D(6), set_selpar_slope_HB2_D(7));
  f_priors+=neg_log_prior(selpar_A502_HB2_D,set_selpar_A502_HB2_D(5), set_selpar_A502_HB2_D(6), set_selpar_A502_HB2_D(7));
  f_priors+=neg_log_prior(selpar_slope2_HB2_D,set_selpar_slope2_HB2_D(5), set_selpar_slope2_HB2_D(6), set_selpar_slope2_HB2_D(7));
  
  f_priors+=neg_log_prior(selpar_A50_HB3_D,set_selpar_A50_HB3_D(5), set_selpar_A50_HB3_D(6), set_selpar_A50_HB3_D(7));
  f_priors+=neg_log_prior(selpar_slope_HB3_D,set_selpar_slope_HB3_D(5), set_selpar_slope_HB3_D(6), set_selpar_slope_HB3_D(7));
  f_priors+=neg_log_prior(selpar_A502_HB3_D,set_selpar_A502_HB3_D(5), set_selpar_A502_HB3_D(6), set_selpar_A502_HB3_D(7));
  f_priors+=neg_log_prior(selpar_slope2_HB3_D,set_selpar_slope2_HB3_D(5), set_selpar_slope2_HB3_D(6), set_selpar_slope2_HB3_D(7));

  f_priors+=neg_log_prior(selpar_A50_GR3_D,set_selpar_A50_GR3_D(5), set_selpar_A50_GR3_D(6), set_selpar_A50_GR3_D(7));
  f_priors+=neg_log_prior(selpar_slope_GR3_D,set_selpar_slope_GR3_D(5), set_selpar_slope_GR3_D(6), set_selpar_slope_GR3_D(7));
  f_priors+=neg_log_prior(selpar_A502_GR3_D,set_selpar_A502_GR3_D(5), set_selpar_A502_GR3_D(6), set_selpar_A502_GR3_D(7));
  f_priors+=neg_log_prior(selpar_slope2_GR3_D,set_selpar_slope2_GR3_D(5), set_selpar_slope2_GR3_D(6), set_selpar_slope2_GR3_D(7));
  
  f_priors+=neg_log_prior(selpar_A50_CVT,set_selpar_A50_CVT(5), set_selpar_A50_CVT(6), set_selpar_A50_CVT(7));
  f_priors+=neg_log_prior(selpar_slope_CVT,set_selpar_slope_CVT(5), set_selpar_slope_CVT(6), set_selpar_slope_CVT(7));
  f_priors+=neg_log_prior(selpar_A502_CVT,set_selpar_A502_CVT(5), set_selpar_A502_CVT(6), set_selpar_A502_CVT(7));
  f_priors+=neg_log_prior(selpar_slope2_CVT,set_selpar_slope2_CVT(5), set_selpar_slope2_CVT(6), set_selpar_slope2_CVT(7));

  f_priors+=neg_log_prior(log_q_cH,set_log_q_cH(5),set_log_q_cH(6),set_log_q_cH(7));
  f_priors+=neg_log_prior(log_q_HB,set_log_q_HB(5),set_log_q_HB(6),set_log_q_HB(7));
  f_priors+=neg_log_prior(log_q_HB_D,set_log_q_HB_D(5),set_log_q_HB_D(6),set_log_q_HB_D(7));
  f_priors+=neg_log_prior(log_q_CVT,set_log_q_CVT(5),set_log_q_CVT(6),set_log_q_CVT(7));
  f_priors+=neg_log_prior(log_q_VID,set_log_q_VID(5),set_log_q_VID(6),set_log_q_VID(7));
    
  f_priors+=neg_log_prior(F_init,set_F_init(5),set_F_init(6),set_F_init(7));
  //f_priors+=neg_log_prior(log_avg_F_cH,set_log_avg_F_cH(5),set_log_avg_F_cH(6),set_log_avg_F_cH(7));
  //f_priors+=neg_log_prior(log_avg_F_cL,set_log_avg_F_cL(5),set_log_avg_F_cL(6),set_log_avg_F_cL(7));
  //f_priors+=neg_log_prior(log_avg_F_HB,set_log_avg_F_HB(5),set_log_avg_F_HB(6),set_log_avg_F_HB(7));
  //f_priors+=neg_log_prior(log_avg_F_GR,set_log_avg_F_GR(5),set_log_avg_F_GR(6),set_log_avg_F_GR(7));
  
  f_priors+=neg_log_prior(log_dm_cH_lc,set_log_dm_cH_lc(5),set_log_dm_cH_lc(6),set_log_dm_cH_lc(7));
  f_priors+=neg_log_prior(log_dm_cH_D_lc,set_log_dm_cH_D_lc(5),set_log_dm_cH_D_lc(6),set_log_dm_cH_D_lc(7));
  f_priors+=neg_log_prior(log_dm_HB_D_lc,set_log_dm_HB_D_lc(5),set_log_dm_HB_D_lc(6),set_log_dm_HB_D_lc(7));
  f_priors+=neg_log_prior(log_dm_GR_D_lc,set_log_dm_GR_D_lc(5),set_log_dm_GR_D_lc(6),set_log_dm_GR_D_lc(7));
  f_priors+=neg_log_prior(log_dm_cH_ac,set_log_dm_cH_ac(5),set_log_dm_cH_ac(6),set_log_dm_cH_ac(7));
  f_priors+=neg_log_prior(log_dm_HB_ac,set_log_dm_HB_ac(5),set_log_dm_HB_ac(6),set_log_dm_HB_ac(7));
  f_priors+=neg_log_prior(log_dm_CVT_ac,set_log_dm_CVT_ac(5),set_log_dm_CVT_ac(6),set_log_dm_CVT_ac(7));
  f_priors+=neg_log_prior(log_dm_GR_ac,set_log_dm_GR_ac(5),set_log_dm_GR_ac(6),set_log_dm_GR_ac(7));
    
  
  fval+=f_priors;

//----------------------------------------------------------------------------------
//Logistic function: 2 parameters
FUNCTION dvar_vector logistic(const dvar_vector& ages, const dvariable& A50, const dvariable& slope)
  //ages=vector of ages, A50=age at 50% selectivity, slope=rate of increase
  RETURN_ARRAYS_INCREMENT();
  dvar_vector Sel_Tmp(ages.indexmin(),ages.indexmax());
  Sel_Tmp=1./(1.+mfexp(-1.*slope*(ages-A50))); //logistic;  
  RETURN_ARRAYS_DECREMENT();
  return Sel_Tmp;

//-----------------------------------------------------------------------------------                                                                           
//Logistic-exponential: 4 parameters (but 1 is fixed)
FUNCTION dvar_vector logistic_exponential(const dvar_vector& ages, const dvariable& A50, const dvariable& slope, const dvariable& sigma, const dvariable& joint)
  //ages=vector of ages, A50=age at 50% sel (ascending limb), slope=rate of increase, sigma=controls rate of descent (descending)                               
  //joint=age to join curves                                                                                                                                    
  RETURN_ARRAYS_INCREMENT();                                                                                                                                    
  dvar_vector Sel_Tmp(ages.indexmin(),ages.indexmax());                                                                                                         
  Sel_Tmp=1.0;                                                                                                                                                  
  for (iage=1; iage<=nages; iage++)                                                                                                                             
  {                                                                                                                                                             
   if (ages(iage)<joint) {Sel_Tmp(iage)=1./(1.+mfexp(-1.*slope*(ages(iage)-A50)));}                                                                             
   if (ages(iage)>joint){Sel_Tmp(iage)=mfexp(-1.*square((ages(iage)-joint)/sigma));}                                                                            
  }                                                                                                                                                             
  Sel_Tmp=Sel_Tmp/max(Sel_Tmp);                                                                                                                                 
  RETURN_ARRAYS_DECREMENT();                                                                                                                                    
  return Sel_Tmp;   

//-----------------------------------------------------------------------------------
//Logistic function: 4 parameters
FUNCTION dvar_vector logistic_double(const dvar_vector& ages, const dvariable& A501, const dvariable& slope1, const dvariable& A502, const dvariable& slope2)
  //ages=vector of ages, A50=age at 50% selectivity, slope=rate of increase, A502=age at 50% decrease additive to A501, slope2=slope of decrease
  RETURN_ARRAYS_INCREMENT();
  dvar_vector Sel_Tmp(ages.indexmin(),ages.indexmax());
  Sel_Tmp=elem_prod( (1./(1.+mfexp(-1.*slope1*(ages-A501)))),(1.-(1./(1.+mfexp(-1.*slope2*(ages-(A501+A502)))))) );     
  Sel_Tmp=Sel_Tmp/max(Sel_Tmp);
  RETURN_ARRAYS_DECREMENT();
  return Sel_Tmp;

//-----------------------------------------------------------------------------------
//Jointed logistic function: 6 parameters (increasing and decreasing logistics joined at peak selectivity)
FUNCTION dvar_vector logistic_joint(const dvar_vector& ages, const dvariable& A501, const dvariable& slope1, const dvariable& A502, const dvariable& slope2, const dvariable& satval, const dvariable& joint)
  //ages=vector of ages, A501=age at 50% sel (ascending limb), slope1=rate of increase,A502=age at 50% sel (descending), slope1=rate of increase (ascending), 
  //satval=saturation value of descending limb, joint=location in age vector to join curves (may equal age or age + 1 if age-0 is included)
  RETURN_ARRAYS_INCREMENT();
  dvar_vector Sel_Tmp(ages.indexmin(),ages.indexmax());
  Sel_Tmp=1.0; 
  for (iage=1; iage<=nages; iage++)
  {
   if (double(iage)<joint) {Sel_Tmp(iage)=1./(1.+mfexp(-1.*slope1*(ages(iage)-A501)));}  
   if (double(iage)>joint){Sel_Tmp(iage)=1.0-(1.0-satval)/(1.+mfexp(-1.*slope2*(ages(iage)-A502)));}  
  }  
  Sel_Tmp=Sel_Tmp/max(Sel_Tmp);
  RETURN_ARRAYS_DECREMENT();
  return Sel_Tmp;

//-----------------------------------------------------------------------------------  
//Double Gaussian function: 6 parameters (as in SS3)
FUNCTION dvar_vector gaussian_double(const dvar_vector& ages, const dvariable& peak, const dvariable& top, const dvariable& ascwid, const dvariable& deswid, const dvariable& init, const dvariable& final)
  //ages=vector of ages, peak=ascending inflection location (as logistic), top=width of plateau, ascwid=ascent width (as log(width))
  //deswid=descent width (as log(width))
  RETURN_ARRAYS_INCREMENT();
  dvar_vector Sel_Tmp(ages.indexmin(),ages.indexmax());
  dvar_vector sel_step1(ages.indexmin(),ages.indexmax());
  dvar_vector sel_step2(ages.indexmin(),ages.indexmax());
  dvar_vector sel_step3(ages.indexmin(),ages.indexmax());
  dvar_vector sel_step4(ages.indexmin(),ages.indexmax());
  dvar_vector sel_step5(ages.indexmin(),ages.indexmax());
  dvar_vector sel_step6(ages.indexmin(),ages.indexmax());
  dvar_vector pars_tmp(1,6); dvar_vector sel_tmp_iq(1,2);
  
  pars_tmp(1)=peak;
  pars_tmp(2)=peak+1.0+(0.99*ages(nages)-peak-1.0)/(1.0+mfexp(-top));
  pars_tmp(3)=mfexp(ascwid);
  pars_tmp(4)=mfexp(deswid);
  pars_tmp(5)=1.0/(1.0+mfexp(-init));
  pars_tmp(6)=1.0/(1.0+mfexp(-final));
       
  sel_tmp_iq(1)=mfexp(-(square(ages(1)-pars_tmp(1))/pars_tmp(3)));
  sel_tmp_iq(2)=mfexp(-(square(ages(nages)-pars_tmp(2))/pars_tmp(4)));
  
  sel_step1=mfexp(-(square(ages-pars_tmp(1))/pars_tmp(3)));
  sel_step2=pars_tmp(5)+(1.0-pars_tmp(5))*(sel_step1-sel_tmp_iq(1))/(1.0-sel_tmp_iq(1));  
  sel_step3=mfexp(-(square(ages-pars_tmp(2))/pars_tmp(4)));
  sel_step4=1.0+(pars_tmp(6)-1.0)*(sel_step3-1.0)/(sel_tmp_iq(2)-1.0);
  sel_step5=1.0/ (1.0+mfexp(-(20.0* elem_div((ages-pars_tmp(1)), (1.0+sfabs(ages-pars_tmp(1)))) )));
  sel_step6=1.0/(1.0+mfexp(-(20.0*elem_div((ages-pars_tmp(2)),(1.0+sfabs(ages-pars_tmp(2)))) )));  

  Sel_Tmp=elem_prod(sel_step2,(1.0-sel_step5))+ 
          elem_prod(sel_step5,((1.0-sel_step6)+ elem_prod(sel_step4,sel_step6)) ); 
 
  Sel_Tmp=Sel_Tmp/max(Sel_Tmp);
  RETURN_ARRAYS_DECREMENT();
  return Sel_Tmp;
    
//-----------------------------------------------------------------------------------    
//Spawner-recruit function (Beverton-Holt or Ricker)
FUNCTION dvariable SR_func(const dvariable& R0, const dvariable& h, const dvariable& spr_F0, const dvariable& SSB, int func)
  //R0=virgin recruitment, h=steepness, spr_F0=spawners per recruit @ F=0, SSB=spawning biomass
  //func=1 for Beverton-Holt, 2 for Ricker
  RETURN_ARRAYS_INCREMENT();
  dvariable Recruits_Tmp;
  switch(func) {
    case 1: //Beverton-Holt
      Recruits_Tmp=((0.8*R0*h*SSB)/(0.2*R0*spr_F0*(1.0-h)+(h-0.2)*SSB));       
    break;
    case 2: //Ricker
      Recruits_Tmp=((SSB/spr_F0)*mfexp(h*(1-SSB/(R0*spr_F0))));       
    break;
	case 3: //None
      Recruits_Tmp=R0;       
    break;
  }
  RETURN_ARRAYS_DECREMENT();
  return Recruits_Tmp;
  
//-----------------------------------------------------------------------------------    
//Spawner-recruit equilibrium function (Beverton-Holt or Ricker)
FUNCTION dvariable SR_eq_func(const dvariable& R0, const dvariable& h, const dvariable& spr_F0, const dvariable& spr_F, const dvariable& BC, int func)
  //R0=virgin recruitment, h=steepness, spr_F0=spawners per recruit @ F=0, spr_F=spawners per recruit @ F, BC=bias correction
  //func=1 for Beverton-Holt, 2 for Ricker
  RETURN_ARRAYS_INCREMENT();
  dvariable Recruits_Tmp;
  switch(func) {
    case 1: //Beverton-Holt
      Recruits_Tmp=(R0/((5.0*h-1.0)*spr_F))*(BC*4.0*h*spr_F-spr_F0*(1.0-h));    
    break;
    case 2: //Ricker
      Recruits_Tmp=R0/(spr_F/spr_F0)*(1.0+log(BC*spr_F/spr_F0)/h);      
    break;
    case 3: //None
      Recruits_Tmp=BC*R0;      
    break;	
  }
  RETURN_ARRAYS_DECREMENT();
  return Recruits_Tmp;
  
//-----------------------------------------------------------------------------------
//compute multinomial effective sample size for a single yr
FUNCTION dvariable multinom_eff_N(const dvar_vector& pred_comp, const dvar_vector& obs_comp)
  //pred_comp=vector of predicted comps, obscomp=vector of observed comps
  dvariable EffN_Tmp; dvariable numer; dvariable denom;
  RETURN_ARRAYS_INCREMENT();
  numer=sum( elem_prod(pred_comp,(1.0-pred_comp)) );
  denom=sum( square(obs_comp-pred_comp) );
  if (denom>0.0) {EffN_Tmp=numer/denom;}
  else {EffN_Tmp=-missing;}                            
  RETURN_ARRAYS_DECREMENT();
  return EffN_Tmp;

//-----------------------------------------------------------------------------------
//Likelihood contribution: lognormal
FUNCTION dvariable lk_lognormal(const dvar_vector& pred, const dvar_vector& obs, const dvar_vector& cv, const dvariable& wgt_dat)
  //pred=vector of predicted vals, obs=vector of observed vals, cv=vector of CVs in arithmetic space, wgt_dat=constant scaling of CVs
  //small_number is small value to avoid log(0) during search
  RETURN_ARRAYS_INCREMENT();
  dvariable LkvalTmp;
  dvariable small_number=0.00001;
  dvar_vector var(cv.indexmin(),cv.indexmax()); //variance in log space
  var=log(1.0+square(cv/wgt_dat));   // convert cv in arithmetic space to variance in log space
  LkvalTmp=sum(0.5*elem_div(square(log(elem_div((pred+small_number),(obs+small_number)))),var) );
  RETURN_ARRAYS_DECREMENT();
  return LkvalTmp;

//-----------------------------------------------------------------------------------
//Likelihood contribution: multinomial
FUNCTION dvariable lk_multinomial(const dvar_vector& nsamp, const dvar_matrix& pred_comp, const dvar_matrix& obs_comp, const double& ncomp, const double& minSS, const dvariable& wgt_dat)
  //nsamp=vector of N's, pred_comp=matrix of predicted comps, obs_comp=matrix of observed comps, ncomp = number of yrs in matrix, minSS=min N threshold, wgt_dat=scaling of N's
  RETURN_ARRAYS_INCREMENT();
  dvariable LkvalTmp;
  dvariable small_number=0.00001;
  LkvalTmp=0.0;
  for (int ii=1; ii<=ncomp; ii++)
  {if (nsamp(ii)>=minSS)
    {LkvalTmp-=wgt_dat*nsamp(ii)*sum(elem_prod((obs_comp(ii)+small_number),
               log(elem_div((pred_comp(ii)+small_number), (obs_comp(ii)+small_number)))));
    }
  }  
  RETURN_ARRAYS_DECREMENT();
  return LkvalTmp;

//-----------------------------------------------------------------------------------
//Likelihood contribution: multinomial
FUNCTION dvariable lk_robust_multinomial(const dvar_vector& nsamp, const dvar_matrix& pred_comp, const dvar_matrix& obs_comp, const double& ncomp, const dvariable& mbin, const double& minSS, const dvariable& wgt_dat)
  //nsamp=vector of N's, pred_comp=matrix of predicted comps, obs_comp=matrix of observed comps, ncomp = number of yrs in matrix, mbin=number of bins, minSS=min N threshold, wgt_dat=scaling of N's
  RETURN_ARRAYS_INCREMENT();
  dvariable LkvalTmp;
  dvariable small_number=0.00001;
  LkvalTmp=0.0;
  dvar_matrix Eprime=elem_prod((1.0-obs_comp), obs_comp)+0.1/mbin; //E' of Francis 2011, p.1131  
  dvar_vector nsamp_wgt=nsamp*wgt_dat;
  //cout<<nsamp_wgt<<endl;
  for (int ii=1; ii<=ncomp; ii++)
  {if (nsamp(ii)>=minSS)
    {LkvalTmp+= sum(0.5*log(Eprime(ii))-log(small_number+mfexp(elem_div((-square(obs_comp(ii)-pred_comp(ii))) , (Eprime(ii)*2.0/nsamp_wgt(ii)) ))) );
    }
  }  
  RETURN_ARRAYS_DECREMENT();
  return LkvalTmp;

//-----------------------------------------------------------------------------------
//-----------------------------------------------------------------------------------
//Likelihood contribution: Dirichlet-multinomial
FUNCTION dvariable lk_dirichlet_multinomial(const dvar_vector& nsamp, const dvar_matrix& pred_comp, const dvar_matrix& obs_comp, const double& ncomp, const dvariable& mbin, const double& minSS, const dvariable& log_dir_par)
  //nsamp=vector of N's, pred_comp=matrix of predicted comps, obs_comp=matrix of observed comps, ncomp = number of yrs in matrix, mbin=number of bins, minSS=min N threshold, log_dir_par=variance inflation factor
  RETURN_ARRAYS_INCREMENT();
  dvariable LkvalTmp;
  LkvalTmp=0.0; 
  dvar_vector nsamp_adjust=nsamp*mfexp(log_dir_par);
  //dvar_vector nsamp_adjust=mfexp(log_dir_par);
  for (int ii=1; ii<=ncomp; ii++)
  {
	if (nsamp(ii)>=minSS)
    {
		LkvalTmp-=gammln(nsamp_adjust(ii))-gammln(nsamp(ii)+nsamp_adjust(ii));
		LkvalTmp-=sum(gammln(nsamp(ii)*obs_comp(ii)+nsamp_adjust(ii)*pred_comp(ii)));
        LkvalTmp+=sum(gammln(nsamp_adjust(ii)*pred_comp(ii)));		
    }
  }  
  RETURN_ARRAYS_DECREMENT();
  return LkvalTmp;

//-----------------------------------------------------------------------------------
//Likelihood contribution: logistic normal (aka multivariate logistic in iSCAM; logistic normal in Francis' terminology)
FUNCTION dvariable lk_logistic_normal(const dvar_vector& nsamp, const dvar_matrix& pred_comp, const dvar_matrix& obs_comp, const double& ncomp, const dvariable& mbin, const double& minSS)
  //nsamp=vector of N's, pred_comp=matrix of predicted comps, obs_comp=matrix of observed comps, ncomp = number of yrs in matrix, mbin=number of bins, minSS=min N threshold
  RETURN_ARRAYS_INCREMENT();
  dvariable LkvalTmp;
  dvariable small_number=0.0001;
  LkvalTmp=0.0;
  dvar_matrix nu=pred_comp+0.0;
  dvar_matrix pred_plus=pred_comp+small_number;
  dvar_matrix obs_plus=obs_comp+small_number;

  dvariable nu_mean;
  dvariable nu_sum_sq;
  dvariable tau_hat_sq;
  dvariable year_count; //keeps track of years included in likelihood (i.e., that meet the sample size requirement)
   
  LkvalTmp=0.0;
  nu_sum_sq=0.0;
  year_count=0.0;
  for (int ii=1; ii<=ncomp; ii++)
  {if (nsamp(ii)>=minSS)
    {
		year_count+=1.0;
		nu_mean=sum( log(obs_plus(ii))-log(pred_plus(ii))  )/mbin;	//year-specific mean log residual
		for (int jj=1; jj<=mbin;jj++)
		{
			nu(ii,jj) = log(obs_plus(ii,jj)) - log(pred_plus(ii,jj)) - nu_mean;
			nu_sum_sq += square(nu(ii,jj));
		}
    }
  }  
  if (year_count>0.0)
  {
	  tau_hat_sq = nu_sum_sq/((mbin-1.0)*year_count);
	  LkvalTmp = (mbin-1.0)*year_count*log(tau_hat_sq);
  }
  RETURN_ARRAYS_DECREMENT();
  return LkvalTmp;
  
//-----------------------------------------------------------------------------------
//Likelihood contribution: priors
FUNCTION  dvariable neg_log_prior(dvariable pred, const double& prior, dvariable var, int pdf)
  //prior=prior point estimate, var=variance (if negative, treated as CV in arithmetic space), pred=predicted value, pdf=prior type (1=none, 2=lognormal, 3=normal, 4=beta)
    dvariable LkvalTmp;
    dvariable alpha, beta, ab_iq;
    dvariable big_number=1e10;
    LkvalTmp=0.0;
    // compute generic pdf's
    switch(pdf) {
        case 1: //option to turn off prior
          LkvalTmp=0.0;
          break;
        case 2: // lognormal
          if(prior<=0.0) cout << "YIKES: Don't use a lognormal distn for a negative prior" << endl;
          else if(pred<=0) LkvalTmp=big_number=1e10;
          else {
            if(var<0.0) var=log(1.0+var*var) ;      // convert cv to variance on log scale
            LkvalTmp= 0.5*( square(log(pred/prior))/var + log(var) );
          }
        break;
        case 3: // normal
          if(var<0.0 && prior!=0.0) var=square(var*prior);       // convert cv to variance on observation scale
          else if(var<0.0 && prior==0.0) var=-var;               // cv not really appropriate if prior value equals zero
          LkvalTmp= 0.5*( square(pred-prior)/var + log(var) );
          break;
        case 4: // beta
          if(var<0.0) var=square(var*prior);          // convert cv to variance on observation scale
          if(prior<=0.0 || prior>=1.0) cout << "YIKES: Don't use a beta distn for a prior outside (0,1)" << endl;
          ab_iq=prior*(1.0-prior)/var - 1.0; alpha=prior*ab_iq; beta=(1.0-prior)*ab_iq;
          if(ab_iq<=0) {
			cout << "Parameter input error: For beta priors, mu*(1-mu)/var must be greater than one. Try decreasing var." << endl;
            exit(0);
		  }
		  if(pred>=0 && pred<=1) LkvalTmp= (1.0-alpha)*log(pred)+(1.0-beta)*log(1.0-pred)-gammln(alpha+beta)+gammln(alpha)+gammln(beta);
          else LkvalTmp=big_number;
          break;
        default: // no such prior pdf currently available
          cout << "Parameter input error: Prior must be either 1(none), 2(lognormal), 3(normal), or 4(beta)." << endl;
          cout << "Presently at least one is " << pdf << endl;
          exit(0);
    }
    return LkvalTmp;
//-----------------------------------------------------------------------------------
//SDNR: age comp likelihood (assumes fits are done with the robust multinomial function)
FUNCTION dvariable sdnr_multinomial(const double& ncomp, const dvar_vector& ages, const dvar_vector& nsamp, 
                                    const dvar_matrix& pred_comp, const dvar_matrix& obs_comp, const dvariable& wgt_dat)
  //ncomp=number of years of data, ages=vector of ages, nsamp=vector of N's, 
  //pred_comp=matrix of predicted comps, obs_comp=matrix of observed comps, wgt_dat=likelihood weight for data source
  RETURN_ARRAYS_INCREMENT();
  dvariable SdnrTmp;
  dvar_vector o(1,ncomp);  
  dvar_vector p(1,ncomp);  
  dvar_vector ose(1,ncomp);  
  dvar_vector res(1,ncomp);
  SdnrTmp=0.0;
  for (int ii=1; ii<=ncomp; ii++)
  {
    o(ii)=sum(elem_prod(ages,obs_comp(ii)));
    p(ii)=sum(elem_prod(ages,pred_comp(ii)));
    ose(ii)=sqrt((sum(elem_prod(square(ages),pred_comp(ii)))-square(p(ii)))/(nsamp(ii)*wgt_dat));
  }
  res=elem_div((o-p),ose); 
  SdnrTmp=sqrt(sum(square(res-(sum(res)/ncomp))/(ncomp-1.0))); 
  RETURN_ARRAYS_DECREMENT();
  return SdnrTmp;

//-----------------------------------------------------------------------------------
//SDNR: lognormal likelihood
FUNCTION dvariable sdnr_lognormal(const dvar_vector& pred, const dvar_vector& obs, const dvar_vector& cv, const dvariable& wgt_dat)
  //nyr=number of years of data, pred=vector of predicted data, obs=vector of observed data, cv=vector of cv's, wgt_dat=likelihood weight for data source
  RETURN_ARRAYS_INCREMENT();
  dvariable SdnrTmp;
  dvariable small_number=0.00001;
  dvariable n;
  dvar_vector res(cv.indexmin(),cv.indexmax());
  SdnrTmp=0.0;
  res=elem_div(log(elem_div(obs+small_number,pred+small_number)),sqrt(log(1+square(cv/wgt_dat))));
  n=cv.indexmax()-cv.indexmin()+1;
  SdnrTmp=sqrt(sum(square(res-(sum(res)/n))/(n-1.0))); 
  RETURN_ARRAYS_DECREMENT();
  return SdnrTmp; 

//-----------------------------------------------------------------------------------
REPORT_SECTION

  if (last_phase())  
  {
      cout<<"start report"<<endl;
      get_weighted_current();
      cout<<"got weighted"<<endl;
      get_msy();
      cout<<"got msy"<<endl;
      get_per_recruit_stuff();
      cout<<"got per recruit"<<endl;  
      get_miscellaneous_stuff();
      cout<<"got misc stuff"<<endl;
	  get_effective_sample_sizes();
      cout<<"got effective samples sizes"<<endl;
      grad_max=objective_function_value::pobjfun->gmax;
      time(&finish);
	  elapsed_time=difftime(finish,start);
	  hour=long(elapsed_time)/3600;
	  minute=long(elapsed_time)%3600/60;
	  second=(long(elapsed_time)%3600)%60;
	  cout<<endl<<endl<<"*******************************************"<<endl;
	  cout<<"--Start time: "<<ctime(&start)<<endl;
	  cout<<"--Finish time: "<<ctime(&finish)<<endl;
	  cout<<"--Runtime: ";
	  cout<<hour<<" hours, "<<minute<<" minutes, "<<second<<" seconds"<<endl;
	  cout << "--TotalLikelihood: " << fval << endl;
      cout<<"--Final gradient: "<<objective_function_value::pobjfun->gmax << endl;
	  cout<<"*******************************************"<<endl;
      
      cout <<endl;     
      cout << "><>--><>--><>--><>--><>--><>--><>--><>--><>--><>"  <<endl;
      //cout << "BC Fmsy=" << F_msy_out<< "   BC SSBmsy=" << SSB_msy_out <<endl;
      cout <<"F status="<<FdF_msy_end<<endl;
      cout <<"Pop status="<<SdSSB_msy_end<<endl;
      cout << "R0="<<R0<<endl;
      //cout << "len_cv = "<<len_cv_val<<endl;
      //cout << "xdum " << xdum << endl;
      cout << "><>--><>--><>--><>--><>--><>--><>--><>--><>--><>"  <<endl;  
     // cout << F_initial << endl;
      
      report << "TotalLikelihood " << fval << endl;
      report << "N" << endl;
      report << N<<endl;
      report << "F" << endl;
      report << F <<endl;

      sdnr_lc_cH=sdnr_multinomial(nyr_cH_lenc, lenbins, nsamp_cH_lenc, pred_cH_lenc, obs_cH_lenc, w_lc_cH);  
      sdnr_lc_cH_D=sdnr_multinomial(nyr_cH_D_lenc, lenbins, nsamp_cH_D_lenc, pred_cH_D_lenc, obs_cH_D_lenc, w_lc_cH_D); 
      sdnr_lc_HB_D=sdnr_multinomial(nyr_HB_D_lenc, lenbins, nsamp_HB_D_lenc, pred_HB_D_lenc, obs_HB_D_lenc, w_lc_HB_D); 
	  sdnr_lc_GR_D=sdnr_multinomial(nyr_GR_D_lenc, lenbins, nsamp_GR_D_lenc, pred_GR_D_lenc, obs_GR_D_lenc, w_lc_GR_D); 
       
      sdnr_ac_cH=sdnr_multinomial(nyr_cH_agec, agebins_agec, nsamp_cH_agec, pred_cH_agec, obs_cH_agec, w_ac_cH);  
      sdnr_ac_HB=sdnr_multinomial(nyr_HB_agec, agebins_agec_10p, nsamp_HB_agec, pred_HB_agec, obs_HB_agec, w_ac_HB);  
      sdnr_ac_CVT=sdnr_multinomial(nyr_CVT_agec, agebins_agec_10p, nsamp_CVT_agec, pred_CVT_agec, obs_CVT_agec, w_ac_CVT);  
      sdnr_ac_GR=sdnr_multinomial(nyr_GR_agec, agebins_agec_10p, nsamp_GR_agec, pred_GR_agec, obs_GR_agec, w_ac_GR);  
       
      sdnr_I_cH=sdnr_lognormal(pred_cH_cpue, obs_cH_cpue, cH_cpue_cv, w_I_cH);
      sdnr_I_HB=sdnr_lognormal(pred_HB_cpue, obs_HB_cpue, HB_cpue_cv, w_I_HB);
      sdnr_I_HB_D=sdnr_lognormal(pred_HB_D_cpue, obs_HB_D_cpue, HB_D_cpue_cv, w_I_HB_D);
      sdnr_I_CVT=sdnr_lognormal(pred_CVT_cpue, obs_CVT_cpue, CVT_cpue_cv, w_I_CVT);  
      sdnr_I_VID=sdnr_lognormal(pred_VID_cpue, obs_VID_cpue, VID_cpue_cv, w_I_VID);   
      
      //#################################################################################################
      //##  Passing parameters to vector for bounds check plotting
      //################################################################################################# 
       Linf_out(8)=Linf; Linf_out(1,7)=set_Linf; 
       K_out(8)=K; K_out(1,7)=set_K;
       t0_out(8)=t0; t0_out(1,7)=set_t0;
       len_cv_val_out(8)=len_cv_val; len_cv_val_out(1,7)=set_len_cv;
	   
	   Linf_L_out(8)=Linf_L; Linf_L_out(1,7)=set_Linf_L; 
       K_L_out(8)=K_L; K_L_out(1,7)=set_K_L;
       t0_L_out(8)=t0_L; t0_L_out(1,7)=set_t0_L;
       len_cv_val_L_out(8)=len_cv_val_L; len_cv_val_L_out(1,7)=set_len_cv_L;
	   
	   Linf_20_out(8)=Linf_20; Linf_20_out(1,7)=set_Linf_20; 
       K_20_out(8)=K_20; K_20_out(1,7)=set_K_20;
       t0_20_out(8)=t0_20; t0_20_out(1,7)=set_t0_20;
       len_cv_val_20_out(8)=len_cv_val_20; len_cv_val_20_out(1,7)=set_len_cv_20;
	   
       log_R0_out(8)=log_R0; log_R0_out(1,7)=set_log_R0;
       M_constant_out(8)=M_constant; M_constant_out(1,7)=set_M_constant;
       steep_out(8)=steep; steep_out(1,7)=set_steep;
       rec_sigma_out(8)=rec_sigma; rec_sigma_out(1,7)=set_rec_sigma;
       R_autocorr_out(8)=R_autocorr; R_autocorr_out(1,7)=set_R_autocorr;
    
       log_dm_cH_lc_out(8)=log_dm_cH_lc; log_dm_cH_lc_out(1,7)=set_log_dm_cH_lc;
	   log_dm_cH_D_lc_out(8)=log_dm_cH_D_lc; log_dm_cH_D_lc_out(1,7)=set_log_dm_cH_D_lc;
	   log_dm_HB_D_lc_out(8)=log_dm_HB_D_lc; log_dm_HB_D_lc_out(1,7)=set_log_dm_HB_D_lc;
	   log_dm_GR_D_lc_out(8)=log_dm_GR_D_lc; log_dm_GR_D_lc_out(1,7)=set_log_dm_GR_D_lc;
	   log_dm_cH_ac_out(8)=log_dm_cH_ac; log_dm_cH_ac_out(1,7)=set_log_dm_cH_ac;  
	   log_dm_HB_ac_out(8)=log_dm_HB_ac; log_dm_HB_ac_out(1,7)=set_log_dm_HB_ac;  
	   log_dm_CVT_ac_out(8)=log_dm_CVT_ac; log_dm_CVT_ac_out(1,7)=set_log_dm_CVT_ac;  
	   log_dm_GR_ac_out(8)=log_dm_GR_ac; log_dm_GR_ac_out(1,7)=set_log_dm_GR_ac;  
	
       selpar_A50_cH1_out(8)=selpar_A50_cH1; selpar_A50_cH1_out(1,7)=set_selpar_A50_cH1;
       selpar_slope_cH1_out(8)=selpar_slope_cH1; selpar_slope_cH1_out(1,7)=set_selpar_slope_cH1;
       selpar_A50_cH2_out(8)=selpar_A50_cH2; selpar_A50_cH2_out(1,7)=set_selpar_A50_cH2;
       selpar_slope_cH2_out(8)=selpar_slope_cH2; selpar_slope_cH2_out(1,7)=set_selpar_slope_cH2;
       selpar_A50_cH3_out(8)=selpar_A50_cH3; selpar_A50_cH3_out(1,7)=set_selpar_A50_cH3;
       selpar_slope_cH3_out(8)=selpar_slope_cH3; selpar_slope_cH3_out(1,7)=set_selpar_slope_cH3;

       selpar_A50_HB1_out(8)=selpar_A50_HB1; selpar_A50_HB1_out(1,7)=set_selpar_A50_HB1;
       selpar_slope_HB1_out(8)=selpar_slope_HB1; selpar_slope_HB1_out(1,7)=set_selpar_slope_HB1;
       selpar_A502_HB1_out(8)=selpar_A502_HB1; selpar_A502_HB1_out(1,7)=set_selpar_A502_HB1;
       selpar_slope2_HB1_out(8)=selpar_slope2_HB1; selpar_slope2_HB1_out(1,7)=set_selpar_slope2_HB1;
       selpar_A50_HB2_out(8)=selpar_A50_HB2; selpar_A50_HB2_out(1,7)=set_selpar_A50_HB2;
       selpar_slope_HB2_out(8)=selpar_slope_HB2; selpar_slope_HB2_out(1,7)=set_selpar_slope_HB2;
       selpar_A502_HB2_out(8)=selpar_A502_HB2; selpar_A502_HB2_out(1,7)=set_selpar_A502_HB2;
       selpar_slope2_HB2_out(8)=selpar_slope2_HB2; selpar_slope2_HB2_out(1,7)=set_selpar_slope2_HB2;
       selpar_A50_HB3_out(8)=selpar_A50_HB3; selpar_A50_HB3_out(1,7)=set_selpar_A50_HB3;
       selpar_slope_HB3_out(8)=selpar_slope_HB3; selpar_slope_HB3_out(1,7)=set_selpar_slope_HB3;
       selpar_A502_HB3_out(8)=selpar_A502_HB3; selpar_A502_HB3_out(1,7)=set_selpar_A502_HB3;
       selpar_slope2_HB3_out(8)=selpar_slope2_HB3; selpar_slope2_HB3_out(1,7)=set_selpar_slope2_HB3;
       
       selpar_A50_HB2_D_out(8)=selpar_A50_HB2_D; selpar_A50_HB2_D_out(1,7)=set_selpar_A50_HB2_D;
       selpar_slope_HB2_D_out(8)=selpar_slope_HB2_D; selpar_slope_HB2_D_out(1,7)=set_selpar_slope_HB2_D;
       selpar_A502_HB2_D_out(8)=selpar_A502_HB2_D; selpar_A502_HB2_D_out(1,7)=set_selpar_A502_HB2_D;
       selpar_slope2_HB2_D_out(8)=selpar_slope2_HB2_D; selpar_slope2_HB2_D_out(1,7)=set_selpar_slope2_HB2_D;
       
	   selpar_A50_HB3_D_out(8)=selpar_A50_HB3_D; selpar_A50_HB3_D_out(1,7)=set_selpar_A50_HB3_D;
	   selpar_slope_HB3_D_out(8)=selpar_slope_HB3_D; selpar_slope_HB3_D_out(1,7)=set_selpar_slope_HB3_D;
	   selpar_A502_HB3_D_out(8)=selpar_A502_HB3_D; selpar_A502_HB3_D_out(1,7)=set_selpar_A502_HB3_D;
	   selpar_slope2_HB3_D_out(8)=selpar_slope2_HB3_D; selpar_slope2_HB3_D_out(1,7)=set_selpar_slope2_HB3_D;

	   selpar_A50_GR3_D_out(8)=selpar_A50_GR3_D; selpar_A50_GR3_D_out(1,7)=set_selpar_A50_GR3_D;
	   selpar_slope_GR3_D_out(8)=selpar_slope_GR3_D; selpar_slope_GR3_D_out(1,7)=set_selpar_slope_GR3_D;
	   selpar_A502_GR3_D_out(8)=selpar_A502_GR3_D; selpar_A502_GR3_D_out(1,7)=set_selpar_A502_GR3_D;
	   selpar_slope2_GR3_D_out(8)=selpar_slope2_GR3_D; selpar_slope2_GR3_D_out(1,7)=set_selpar_slope2_GR3_D;	   
	    
	   selpar_A50_cH2_D_out(8)=selpar_A50_cH2_D; selpar_A50_cH2_D_out(1,7)=set_selpar_A50_cH2_D;
	   selpar_slope_cH2_D_out(8)=selpar_slope_cH2_D; selpar_slope_cH2_D_out(1,7)=set_selpar_slope_cH2_D;
	   selpar_A502_cH2_D_out(8)=selpar_A502_cH2_D; selpar_A502_cH2_D_out(1,7)=set_selpar_A502_cH2_D;
	   selpar_slope2_cH2_D_out(8)=selpar_slope2_cH2_D; selpar_slope2_cH2_D_out(1,7)=set_selpar_slope2_cH2_D;
	    
	   selpar_A50_cH3_D_out(8)=selpar_A50_cH3_D; selpar_A50_cH3_D_out(1,7)=set_selpar_A50_cH3_D;
	   selpar_slope_cH3_D_out(8)=selpar_slope_cH3_D; selpar_slope_cH3_D_out(1,7)=set_selpar_slope_cH3_D;
	   
	   selpar_A50_GR2_out(8)=selpar_A50_GR2; selpar_A50_GR2_out(1,7)=set_selpar_A50_GR2;
       selpar_slope_GR2_out(8)=selpar_slope_GR2; selpar_slope_GR2_out(1,7)=set_selpar_slope_GR2;
       selpar_A502_GR2_out(8)=selpar_A502_GR2; selpar_A502_GR2_out(1,7)=set_selpar_A502_GR2;
       selpar_slope2_GR2_out(8)=selpar_slope2_GR2; selpar_slope2_GR2_out(1,7)=set_selpar_slope2_GR2;

       selpar_A50_GR3_out(8)=selpar_A50_GR3; selpar_A50_GR3_out(1,7)=set_selpar_A50_GR3;
       selpar_slope_GR3_out(8)=selpar_slope_GR3; selpar_slope_GR3_out(1,7)=set_selpar_slope_GR3;
       //selpar_A502_GR3_out(8)=selpar_A502_GR3; selpar_A502_GR3_out(1,7)=set_selpar_A502_GR3;
       //selpar_slope2_GR3_out(8)=selpar_slope2_GR3; selpar_slope2_GR3_out(1,7)=set_selpar_slope2_GR3;

       selpar_A50_CVT_out(8)=selpar_A50_CVT; selpar_A50_CVT_out(1,7)=set_selpar_A50_CVT;
       selpar_slope_CVT_out(8)=selpar_slope_CVT; selpar_slope_CVT_out(1,7)=set_selpar_slope_CVT;
	   selpar_A502_CVT_out(8)=selpar_A502_CVT; selpar_A502_CVT_out(1,7)=set_selpar_A502_CVT;
       selpar_slope2_CVT_out(8)=selpar_slope2_CVT; selpar_slope2_CVT_out(1,7)=set_selpar_slope2_CVT;

       log_q_cH_out(8)=log_q_cH; log_q_cH_out(1,7)=set_log_q_cH;
       log_q_HB_out(8)=log_q_HB; log_q_HB_out(1,7)=set_log_q_HB;
       log_q_HB_D_out(8)=log_q_HB_D; log_q_HB_D_out(1,7)=set_log_q_HB_D;
       log_q_CVT_out(8)=log_q_CVT; log_q_CVT_out(1,7)=set_log_q_CVT;
	   log_q_VID_out(8)=log_q_VID; log_q_VID_out(1,7)=set_log_q_VID;
                     
       log_avg_F_cH_out(8)=log_avg_F_cH; log_avg_F_cH_out(1,7)=set_log_avg_F_cH;
       log_avg_F_HB_out(8)=log_avg_F_HB; log_avg_F_HB_out(1,7)=set_log_avg_F_HB;
       log_avg_F_GR_out(8)=log_avg_F_GR; log_avg_F_GR_out(1,7)=set_log_avg_F_GR;       
       log_avg_F_cH_D_out(8)=log_avg_F_cH_D; log_avg_F_cH_D_out(1,7)=set_log_avg_F_cH_D;
       log_avg_F_HB_D_out(8)=log_avg_F_HB_D; log_avg_F_HB_D_out(1,7)=set_log_avg_F_HB_D;
       log_avg_F_GR_D_out(8)=log_avg_F_GR_D; log_avg_F_GR_D_out(1,7)=set_log_avg_F_GR_D;
       F_init_out(8)=F_init; F_init_out(1,7)=set_F_init;
       
       log_rec_dev_out(styr_rec_dev, endyr_rec_dev)=log_rec_dev;
       log_F_dev_cH_out(styr_cH_L,endyr_cH_L)=log_F_dev_cH;
       log_F_dev_HB_out(styr_HB_L,endyr_HB_L)=log_F_dev_HB;
       log_F_dev_GR_out(styr_GR_L,endyr_GR_L)=log_F_dev_GR;
       log_F_dev_cH_D_out(styr_cH_D,endyr_cH_D)=log_F_dev_cH_D;
       log_F_dev_HB_D_out(styr_HB_D,endyr_HB_D)=log_F_dev_HB_D;
       log_F_dev_GR_D_out(styr_GR_D,endyr_GR_D)=log_F_dev_GR_D;
           
  #include "s73_make_Robject.cxx"   // write the R-compatible report

  } //endl last phase loop     
  
 
