// Create a file with an R object from AD Model Builder

// open the file using the default AD Model Builder file name, and
// 6 digits of precision
open_r_file(adprogram_name + ".rdat", 6);

// Example of an INFO object
open_r_info_list("info", true);
	wrt_r_item("title", "SEDAR 71 Assessment");
	wrt_r_item("species", "SA Gag Grouper");
	wrt_r_item("model", "Statistical Catch at Age");
	if(SR_switch==1)
	    {wrt_r_item("rec.model", "BH-steep");}
    if(SR_switch==2)
	    {wrt_r_item("rec.model", "Ricker-steep");}
	wrt_r_item("base.run", "RGX.tpl");
	wrt_r_item("units.length", "mm");
	wrt_r_item("units.weight", "lb");
	wrt_r_item("units.biomass", "metric tons");
	wrt_r_item("units.ssb", "metric tons");
	wrt_r_item("units.ypr", "lb gutted");			//KC changed from 'lb whole' to 'lb gutted'
	wrt_r_item("units.landings", "1000 lb gutted");		//KC changed from '1000 lb whole' to '1000 lb gutted'
	wrt_r_item("units.discards", "1000 dead fish");
    wrt_r_item("units.numbers", "number fish");
    wrt_r_item("units.naa", "number fish");
	wrt_r_item("units.rec", "number fish");
close_r_info_list();


// VECTOR object of parameters and estimated quantities
open_r_info_list("parms", false);
	wrt_r_item("styr", styr);
	wrt_r_item("endyr", endyr);
	wrt_r_item("styrR", styr_rec_dev);
	wrt_r_item("M.constant", M_constant);
	wrt_r_item("smsy2msstM", smsy2msstM);
    wrt_r_item("smsy2msst75", smsy2msst75);
	wrt_r_item("Linf", Linf);
	wrt_r_item("K", K);
	wrt_r_item("t0", t0);
	wrt_r_item("len_cv_val",len_cv_val);
    wrt_r_item("Linf_L", Linf_L);			//KC
	wrt_r_item("K_L", K_L);				//KC
	wrt_r_item("t0_L", t0_L);			//KC
	wrt_r_item("len_cv_val_L",len_cv_val_L);	//KC
	wrt_r_item("wgt.a", wgtpar_a);
	wrt_r_item("wgt.b", wgtpar_b);
    wrt_r_item("spawn.time", spawn_time_frac);
	wrt_r_item("D.mort.cH", Dmort_cH);
	wrt_r_item("D.mort.HB", Dmort_HB);
	wrt_r_item("D.mort.GR", Dmort_GR);
	wrt_r_item("q.cH", mfexp(log_q_cH));
	wrt_r_item("q.HB", mfexp(log_q_HB));
	//wrt_r_item("q.GR", mfexp(log_q_GR));
	wrt_r_item("q.VID", mfexp(log_q_VID));

    wrt_r_item("q.rate",q_rate);
	wrt_r_item("q.beta",q_DD_beta);
	wrt_r_item("q.DD.B0.exploitable", B0_q_DD);
	wrt_r_item("F.init", F_init);			//KC
	wrt_r_item("F.prop.cH", F_cH_prop);
	wrt_r_item("F.prop.cD", F_cD_prop);	
	wrt_r_item("F.prop.HB", F_HB_prop);
	wrt_r_item("F.prop.GR", F_GR_prop);
	wrt_r_item("F.prop.cH.D", F_cH_D_prop);
	wrt_r_item("F.prop.HB.D", F_HB_D_prop);
	wrt_r_item("F.prop.GR.D", F_GR_D_prop);
	wrt_r_item("Fend.mean", Fend_mean);

	wrt_r_item("B0", B0);
	wrt_r_item("Bstyr.B0", totB(styr)/B0);
	wrt_r_item("SSB0", S0);
	wrt_r_item("SSBstyr.SSB0", SSB(styr)/S0);
	wrt_r_item("Rstyr.R0", rec(styr)/R0);
	if(SR_switch==1)
	{
      wrt_r_item("BH.biascorr",BiasCor);
	  wrt_r_item("BH.Phi0", spr_F0);
	  wrt_r_item("BH.R0", R0);
	  wrt_r_item("BH.steep", steep);
    }
    if(SR_switch==2)
	{
      wrt_r_item("Ricker.biascorr",BiasCor);
	  wrt_r_item("Ricker.Phi0", spr_F0);
	  wrt_r_item("Ricker.R0", R0);
	  wrt_r_item("Ricker.steep", steep);
    }
	wrt_r_item("R.sigma.logdevs", sigma_rec_dev);
	wrt_r_item("R.sigma.par",rec_sigma);
	wrt_r_item("R.autocorr",R_autocorr);
	wrt_r_item("R0", R0); //same as BH.R0, but used in BSR.time.plots
	wrt_r_item("R.virgin.bc", R_virgin); //bias-corrected virgin recruitment
	wrt_r_item("rec.lag", 1.0);
	wrt_r_item("msy.klb", msy_klb_out);
	wrt_r_item("msy.knum", msy_knum_out);
	wrt_r_item("Dmsy.klb", D_msy_klb_out);
	wrt_r_item("Dmsy.knum", D_msy_knum_out);	
	wrt_r_item("Fmsy", F_msy_out);
	wrt_r_item("SSBmsy", SSB_msy_out);
	//wrt_r_item("msst.msy", smsy2msst75*SSB_msy_out);
	wrt_r_item("msst", smsy2msst75*SSB_msy_out);
	wrt_r_item("Bmsy", B_msy_out);
	wrt_r_item("Rmsy", R_msy_out);
	wrt_r_item("sprmsy",spr_msy_out);
	wrt_r_item("Fend.Fmsy", FdF_msy_end);
	wrt_r_item("Fend.Fmsy.mean", FdF_msy_end_mean);
	wrt_r_item("SSBend.SSBmsy", SdSSB_msy_end);
	wrt_r_item("SSBend.MSST", SdSSB_msy_end/smsy2msst75);
	wrt_r_item("F30", F30_out); //For FishGraph
	wrt_r_item("SSB.F30", SSB_F30_out); //For FishGraph
	wrt_r_item("msst.F30", smsy2msst75*SSB_F30_out);
	//wrt_r_item("msst", smsy2msst75*SSB_F30_out); //For FishGraph
	 wrt_r_item("B.F30", B_F30_out);
	wrt_r_item("R.F30", R_F30_out);
    wrt_r_item("L.F30.knum", L_F30_knum_out);
    wrt_r_item("L.F30.klb", L_F30_klb_out);
    wrt_r_item("D.F30.knum", D_F30_knum_out);
    wrt_r_item("D.F30.klb", D_F30_klb_out);
 close_r_info_list();
	
 // VECTOR object of parameters and estimated quantities
open_r_info_list("spr.brps", false);
	wrt_r_item("F20", F20_out);
	wrt_r_item("F30", F30_out);
	wrt_r_item("F40", F40_out);
	wrt_r_item("SSB.F30", SSB_F30_out);
    wrt_r_item("msst.F30", smsy2msst75*SSB_F30_out);   
    wrt_r_item("B.F30", B_F30_out);
	wrt_r_item("R.F30", R_F30_out);
    wrt_r_item("L.F30.knum", L_F30_knum_out);
    wrt_r_item("L.F30.klb", L_F30_klb_out);
    wrt_r_item("D.F30.knum", D_F30_knum_out);
    wrt_r_item("D.F30.klb", D_F30_klb_out);        	 	
	wrt_r_item("Fend.F30.mean", FdF30_end_mean);
    wrt_r_item("SSBend.SSBF30", SdSSB_F30_end);
	wrt_r_item("SSBend.MSSTF30", Sdmsst_F30_end);	
 close_r_info_list();

// MATRIX object of parameter constraints
open_r_df("parm.cons",1,8,2);
    wrt_r_namevector(1,8);
    wrt_r_df_col("Linf",Linf_out);
    wrt_r_df_col("K",K_out);
    wrt_r_df_col("t0",t0_out);
    wrt_r_df_col("len_cv_val",len_cv_val_out);
    wrt_r_df_col("Linf_L", Linf_L_out);		//KC
    wrt_r_df_col("K_L", K_L_out);		//KC
    wrt_r_df_col("t0_L", t0_L_out);		//KC
    wrt_r_df_col("len_cv_val_L",len_cv_val_L_out); //KC

    wrt_r_df_col("log_R0",log_R0_out);
    wrt_r_df_col("steep",steep_out);
    wrt_r_df_col("rec_sigma",rec_sigma_out);
    wrt_r_df_col("R_autocorr",R_autocorr_out);
    //KC wrt_r_df_col("log_dm_cH_lc",log_dm_cH_lc_out);
    //KC wrt_r_df_col("log_dm_cD_lc",log_dm_cD_lc_out);
	wrt_r_df_col("log_dm_HB_lc",log_dm_HB_lc_out);
    //KC wrt_r_df_col("log_dm_GR_lc",log_dm_GR_lc_out);
        wrt_r_df_col("log_dm_HB_D_lc",log_dm_HB_D_lc_out);
    //KC wrt_r_df_col("log_dm_GR_D_lc",log_dm_GR_D_lc_out);	//KC
    //KC wrt_r_df_col("log_dm_CVT_lc",log_dm_CVT_lc_out);
	wrt_r_df_col("log_dm_cH_ac",log_dm_cH_ac_out);
	wrt_r_df_col("log_dm_cD_ac",log_dm_cD_ac_out);	//KC
	wrt_r_df_col("log_dm_HB_ac",log_dm_HB_ac_out);
	wrt_r_df_col("log_dm_GR_ac",log_dm_GR_ac_out);
	wrt_r_df_col("log_dm_CVT_ac",log_dm_CVT_ac_out);

    wrt_r_df_col("selpar_A50_cH1",selpar_A50_cH1_out);		//KC
    wrt_r_df_col("selpar_slope_cH1",selpar_slope_cH1_out);	//KC	
    wrt_r_df_col("selpar_A50_cH2",selpar_A50_cH2_out);
    wrt_r_df_col("selpar_slope_cH2",selpar_slope_cH2_out);
    wrt_r_df_col("selpar_A50_cH3",selpar_A50_cH3_out);
    wrt_r_df_col("selpar_slope_cH3",selpar_slope_cH3_out);
	
    wrt_r_df_col("selpar_A50_cD2",selpar_A50_cD2_out);
    //KC wrt_r_df_col("selpar_A50_cD3",selpar_A50_cD3_out);	
    wrt_r_df_col("selpar_slope1_cD2",selpar_slope_cD2_out);
	wrt_r_df_col("selpar_A502_cD2",selpar_A502_cD2_out);
	wrt_r_df_col("selpar_slope2_cD2",selpar_slope2_cD2_out);
	
    wrt_r_df_col("selpar_A50_HB1",selpar_A50_HB1_out);
    wrt_r_df_col("selpar_slope_HB1",selpar_slope_HB1_out);
    wrt_r_df_col("selpar_A501_HB2",selpar_A50_HB2_out);
    wrt_r_df_col("selpar_slope1_HB2",selpar_slope_HB2_out);
    wrt_r_df_col("selpar_A501_HB3",selpar_A50_HB3_out);
    wrt_r_df_col("selpar_slope1_HB3",selpar_slope_HB3_out);

    wrt_r_df_col("selpar_A50_GR2",selpar_A50_GR2_out);		//KC
    wrt_r_df_col("selpar_slope_GR2",selpar_slope_GR2_out);	//KC
    wrt_r_df_col("selpar_A50_GR3",selpar_A50_GR3_out);
    wrt_r_df_col("selpar_slope_GR3",selpar_slope_GR3_out);
		
    wrt_r_df_col("selpar_A501_CVT",selpar_A50_CVT_out);
    wrt_r_df_col("selpar_slope1_CVT",selpar_slope_CVT_out);
    wrt_r_df_col("selpar_A502_CVT",selpar_A502_CVT_out);
    wrt_r_df_col("selpar_slope2_CVT",selpar_slope2_CVT_out);
	
    wrt_r_df_col("selpar_age1logit_D",selpar_age1logit_D_out);
    
    wrt_r_df_col("log_q_cH",log_q_cH_out);
    wrt_r_df_col("log_q_HB",log_q_HB_out);
  //wrt_r_df_col("log_q_GR",log_q_GR_out);
    wrt_r_df_col("log_q_VID",log_q_VID_out);
    wrt_r_df_col("M_constant",M_constant_out);
    wrt_r_df_col("log_avg_F_cH",log_avg_F_cH_out);
    wrt_r_df_col("log_avg_F_cD",log_avg_F_cD_out);
    wrt_r_df_col("log_avg_F_HB",log_avg_F_HB_out);
    wrt_r_df_col("log_avg_F_GR",log_avg_F_GR_out);
    wrt_r_df_col("log_avg_F_cH_D",log_avg_F_cH_D_out);
    wrt_r_df_col("log_avg_F_HB_D",log_avg_F_HB_D_out);
    wrt_r_df_col("log_avg_F_GR_D",log_avg_F_GR_D_out);

    wrt_r_df_col("F_init",F_init_out);				//KC uncommented
close_r_df();

// DATA FRAME of time series deviation vector estimates
// names used in this object must match the names used in the "parm.tvec.cons" object
open_r_df("parm.tvec", styr, endyr, 2);
	wrt_r_namevector(styr,endyr);
	wrt_r_df_col("year", styr,endyr);
    	wrt_r_df_col("log.rec.dev", log_rec_dev_out);
    	wrt_r_df_col("log.F.dev.cH", log_F_dev_cH_out);
	wrt_r_df_col("log.F.dev.cD", log_F_dev_cD_out);
    	wrt_r_df_col("log.F.dev.HB", log_F_dev_HB_out);
    	wrt_r_df_col("log.F.dev.GR", log_F_dev_GR_out);
    	wrt_r_df_col("log.F.dev.cH.D", log_F_dev_cH_D_out);
    	wrt_r_df_col("log.F.dev.HB.D", log_F_dev_HB_D_out);
    	wrt_r_df_col("log.F.dev.GR.D", log_F_dev_GR_D_out);
	wrt_r_df_col("log.q.dev.HB.RWq", q_RW_log_dev_HB);
	wrt_r_df_col("log.q.dev.cH.RWq", q_RW_log_dev_cH);
	
close_r_df();

// MATRIX object of deviation vector constraints
// names used in this object must match the names used in the "parm.tvec" object
open_r_df("parm.tvec.cons",1,3,2);
    wrt_r_namevector(1,3);
    wrt_r_df_col("log.rec.dev",set_log_rec_dev);
    wrt_r_df_col("log.F.dev.cH",set_log_F_dev_cH);
    wrt_r_df_col("log.F.dev.cD",set_log_F_dev_cD);
    wrt_r_df_col("log.F.dev.HB",set_log_F_dev_HB);
    wrt_r_df_col("log.F.dev.GR",set_log_F_dev_GR);
    wrt_r_df_col("log.F.dev.cH.D",set_log_F_dev_cH_D);
    wrt_r_df_col("log.F.dev.HB.D",set_log_F_dev_HB_D);
    wrt_r_df_col("log.F.dev.GR.D",set_log_F_dev_GR_D);
    wrt_r_df_col("log.q.dev.HB.RWq", set_log_RWq_dev);
    wrt_r_df_col("log.q.dev.cH.RWq", set_log_RWq_dev);
close_r_df();

// DATA FRAME of age vector deviation estimates
// names used in this object must match the names used in the "parm.avec.cons" object
open_r_df("parm.avec", 1, nages, 2);
	wrt_r_namevector(1,nages);
	wrt_r_df_col("age", agebins); //deviations for first age not estimated
    wrt_r_df_col("log.Nage.dev", log_Nage_dev_output);
close_r_df();

// MATRIX object of age vector deviation constraints
// names used in this object must match the names used in the "parm.avec" object
open_r_df("parm.avec.cons",1,3,2);
    wrt_r_namevector(1,3);
    wrt_r_df_col("log.Nage.dev",set_log_Nage_dev);
close_r_df();

// VECTOR object of SDNR calculations
open_r_vector("sdnr");
    wrt_r_item("sdnr.U.cH", sdnr_I_cH);
    wrt_r_item("sdnr.U.HB", sdnr_I_HB);
    //wrt_r_item("sdnr.U.GR", sdnr_I_GR);
	wrt_r_item("sdnr.U.VID", sdnr_I_VID);
	
    //KC wrt_r_item("sdnr.lc.cH", sdnr_lc_cH);
    //KC wrt_r_item("sdnr.lc.cD", sdnr_lc_cD);	
    wrt_r_item("sdnr.lc.HB", sdnr_lc_HB);
    //KC wrt_r_item("sdnr.lc.GR", sdnr_lc_GR);	
    wrt_r_item("sdnr.lc.HB.D", sdnr_lc_HB_D);
    //KC wrt_r_item("sdnr.lc.GR.D", sdnr_lc_GR_D);	//KC
    //KC wrt_r_item("sdnr.lc.CVT", sdnr_lc_CVT);
	
    wrt_r_item("sdnr.ac.cH", sdnr_ac_cH);
    wrt_r_item("sdnr.ac.cD", sdnr_ac_cD);	//KC
    wrt_r_item("sdnr.ac.HB", sdnr_ac_HB);
    //KC wrt_r_item("sdnr.ac.GR", sdnr_ac_GR);
    wrt_r_item("sdnr.ac.CVT", sdnr_ac_CVT);	
	
 close_r_vector();

// VECTOR object of likelihood contributions
open_r_vector("like");
    wrt_r_item("lk.total", fval); //weighted likelihood
    wrt_r_item("lk.unwgt.data", fval_data);  //likelihood of just data components

    wrt_r_item("lk.L.cH", f_cH_L);
    wrt_r_item("lk.L.cD", f_cD_L);	
    wrt_r_item("lk.L.HB", f_HB_L);
    wrt_r_item("lk.L.GR", f_GR_L);
    wrt_r_item("lk.D.cH", f_cH_D);
    wrt_r_item("lk.D.HB", f_HB_D);
    wrt_r_item("lk.D.GR", f_GR_D);

    //KC wrt_r_item("lk.U.cH", f_cH_cpue);
    wrt_r_item("lk.U.HB", f_HB_cpue);
  //wrt_r_item("lk.U.GR", f_GR_cpue);
    wrt_r_item("lk.U.VID", f_VID_cpue);
    //KC wrt_r_item("lk.lenc.cH", f_cH_lenc);
    //KC wrt_r_item("lk.lenc.cD", f_cD_lenc);	
    wrt_r_item("lk.lenc.HB", f_HB_lenc);
    //KC wrt_r_item("lk.lenc.GR", f_GR_lenc);	
    wrt_r_item("lk.lenc.HB.D", f_HB_D_lenc);
    //KC wrt_r_item("lk.lenc.GR.D", f_GR_D_lenc);	//KC
    //KC wrt_r_item("lk.lenc.CVT", f_CVT_lenc);	
    wrt_r_item("lk.agec.cH", f_cH_agec);
    wrt_r_item("lk.agec.cD", f_cD_agec);		//KC
    wrt_r_item("lk.agec.HB", f_HB_agec);
    wrt_r_item("lk.agec.GR", f_GR_agec);
    wrt_r_item("lk.agec.CVT", f_CVT_agec);	
    wrt_r_item("lk.Nage.init", f_Nage_init);
    wrt_r_item("lk.SRfit", f_rec_dev);
    wrt_r_item("lk.SRearly", f_rec_dev_early);
    wrt_r_item("lk.SRend", f_rec_dev_end);
    wrt_r_item("lk.fullF", f_fullF_constraint);
    wrt_r_item("lk.Ftune", f_Ftune);
    wrt_r_item("lk.U.HB.RWq", f_HB_RWq_cpue);
    wrt_r_item("lk.U.cH.RWq", f_cH_RWq_cpue);
    wrt_r_item("lk.priors",f_priors);
    wrt_r_item("gradient.max",grad_max);

    wrt_r_item("w.L", w_L);
    wrt_r_item("w.D", w_D);
    //KC wrt_r_item("w.U.cH", w_I_cH);
    wrt_r_item("w.U.HB", w_I_HB);
  //wrt_r_item("w.U.GR", w_I_GR);
    wrt_r_item("w.U.VID", w_I_VID);
	
    //KC wrt_r_item("w.lc.cH", w_lc_cH);
    //KC wrt_r_item("w.lc.cD", w_lc_cD);	
    wrt_r_item("w.lc.HB", w_lc_HB);
    //KC wrt_r_item("w.lc.GR", w_lc_GR);
    wrt_r_item("w.lc.HB.D", w_lc_HB_D);
    //KC wrt_r_item("w.lc.GR.D", w_lc_GR_D);	//KC
    //KC wrt_r_item("w.lc.CVT", w_lc_CVT);
	
    wrt_r_item("w.ac.cH", w_ac_cH);
    wrt_r_item("w.ac.cD", w_ac_cD);  //KC
    wrt_r_item("w.ac.HB", w_ac_HB);
    wrt_r_item("w.ac.GR", w_ac_GR);
    wrt_r_item("w.ac.CVT", w_ac_CVT);

    wrt_r_item("w.Nage.init", w_Nage_init);
    wrt_r_item("w.R", w_rec);
    wrt_r_item("w.R.init", w_rec_early);
    wrt_r_item("w.R.end", w_rec_end);
    wrt_r_item("w.F.early.phases", w_fullF);
    wrt_r_item("w.Ftune.early.phases", w_Ftune);
    wrt_r_item("var.RWq", set_RWq_var);

 close_r_vector();

    open_r_matrix("N.age");
    wrt_r_matrix(N, 2, 2);
    wrt_r_namevector(styr, (endyr+1));
    wrt_r_namevector(agebins);
    close_r_matrix();

    open_r_matrix("N.age.mdyr");
    wrt_r_matrix(N_mdyr, 2, 2);
    wrt_r_namevector(styr, endyr);
    wrt_r_namevector(agebins);
    close_r_matrix();

    open_r_matrix("N.age.spawn");
    wrt_r_matrix(N_spawn, 2, 2);
    wrt_r_namevector(styr, endyr); 
    wrt_r_namevector(agebins);
    close_r_matrix();

    open_r_matrix("B.age");
    wrt_r_matrix(B, 2, 2);
    wrt_r_namevector(styr, (endyr+1));
    wrt_r_namevector(agebins);
    close_r_matrix();

    open_r_matrix("F.age");
    wrt_r_matrix(F, 2, 2);
    wrt_r_namevector(styr,endyr);
    wrt_r_namevector(agebins);
    close_r_matrix();

    open_r_matrix("Z.age");
    wrt_r_matrix(Z, 2, 2);
    wrt_r_namevector(styr,endyr);
    wrt_r_namevector(agebins);
    close_r_matrix();
	
    open_r_matrix("L.age.pred.knum");
    wrt_r_matrix(L_total_num/1000.0, 2, 2);
    wrt_r_namevector(styr,endyr);
    wrt_r_namevector(agebins);   
    close_r_matrix();
    
    open_r_matrix("L.age.pred.klb");
    wrt_r_matrix(L_total_klb, 2, 2);
    wrt_r_namevector(styr,endyr);
    wrt_r_namevector(agebins);   
    close_r_matrix();
	
    open_r_matrix("D.age.pred.knum");
    wrt_r_matrix(D_total_num/1000.0, 2, 2);
    wrt_r_namevector(styr,endyr);
    wrt_r_namevector(agebins);   
    close_r_matrix();
    
    open_r_matrix("D.age.pred.klb");
    wrt_r_matrix(D_total_klb, 2, 2);
    wrt_r_namevector(styr,endyr);
    wrt_r_namevector(agebins);   
    close_r_matrix();
 
// LIST object with annual selectivity at age by fishery

open_r_list("size.age.fishery");

    open_r_matrix("len.cH.mm");
    wrt_r_matrix(len_cH_mm, 2, 2);
    wrt_r_namevector(styr,endyr);
    wrt_r_namevector(agebins);
    close_r_matrix();

    open_r_matrix("len.cD.mm");
    wrt_r_matrix(len_cD_mm, 2, 2);
    wrt_r_namevector(styr,endyr);
    wrt_r_namevector(agebins);
    close_r_matrix();
	
    open_r_matrix("len.HB.mm");
    wrt_r_matrix(len_HB_mm, 2, 2);
    wrt_r_namevector(styr,endyr);
    wrt_r_namevector(agebins);
    close_r_matrix();

    open_r_matrix("len.GR.mm");
    wrt_r_matrix(len_GR_mm, 2, 2);
    wrt_r_namevector(styr,endyr);
    wrt_r_namevector(agebins);
    close_r_matrix();

    open_r_matrix("len.cH.D.mm");
    wrt_r_matrix(len_cH_D_mm, 2, 2);
    wrt_r_namevector(styr,endyr);
    wrt_r_namevector(agebins);
    close_r_matrix();

    open_r_matrix("len.HB.D.mm");
    wrt_r_matrix(len_HB_D_mm, 2, 2);
    wrt_r_namevector(styr,endyr);
    wrt_r_namevector(agebins);
    close_r_matrix();

    open_r_matrix("len.GR.D.mm");
    wrt_r_matrix(len_GR_D_mm, 2, 2);
    wrt_r_namevector(styr,endyr);
    wrt_r_namevector(agebins);
    close_r_matrix();

    open_r_matrix("gutwgt.cH.lb");		//KC changed from wholewgt. to gutwgt.
    wrt_r_matrix(gutwgt_cH_klb*1000.0, 2, 2);	//KC changed from wholewgt. to gutwgt.
    wrt_r_namevector(styr,endyr);
    wrt_r_namevector(agebins);
    close_r_matrix();

    open_r_matrix("gutwgt.cD.lb");		//KC changed from wholewgt. to gutwgt.
    wrt_r_matrix(gutwgt_cD_klb*1000.0, 2, 2);	//KC changed from wholewgt. to gutwgt.
    wrt_r_namevector(styr,endyr);
    wrt_r_namevector(agebins);
    close_r_matrix();

    open_r_matrix("gutwgt.HB.lb");		//KC changed from wholewgt. to gutwgt.
    wrt_r_matrix(gutwgt_HB_klb*1000.0, 2, 2);	//KC changed from wholewgt. to gutwgt.
    wrt_r_namevector(styr,endyr);
    wrt_r_namevector(agebins);
    close_r_matrix();

    open_r_matrix("gutwgt.GR.lb");		//KC changed from wholewgt. to gutwgt.
    wrt_r_matrix(gutwgt_GR_klb*1000.0, 2, 2);	//KC changed from wholewgt. to gutwgt.
    wrt_r_namevector(styr,endyr);
    wrt_r_namevector(agebins);
    close_r_matrix();

    open_r_matrix("gutwgt.cH.D.lb");		//KC changed from wholewgt. to gutwgt.
    wrt_r_matrix(gutwgt_cH_D_klb*1000.0, 2, 2);	//KC changed from wholewgt. to gutwgt.
    wrt_r_namevector(styr,endyr);
    wrt_r_namevector(agebins);
    close_r_matrix();

    open_r_matrix("gutwgt.HB.D.lb");		//KC changed from wholewgt. to gutwgt.
    wrt_r_matrix(gutwgt_HB_D_klb*1000.0, 2, 2);	//KC changed from wholewgt. to gutwgt.
    wrt_r_namevector(styr,endyr);
    wrt_r_namevector(agebins);
    close_r_matrix();

    open_r_matrix("gutwgt.GR.D.lb");		//KC changed from wholewgt. to gutwgt.
    wrt_r_matrix(gutwgt_GR_D_klb*1000.0, 2, 2);	//KC changed from wholewgt. to gutwgt.
    wrt_r_namevector(styr,endyr);
    wrt_r_namevector(agebins);
    close_r_matrix();

 close_r_list();

open_r_list("sel.age");

    wrt_r_complete_vector("sel.v.wgted.L",sel_wgted_L, agebins);
    wrt_r_complete_vector("sel.v.wgted.D",sel_wgted_D, agebins);
    wrt_r_complete_vector("sel.v.wgted.tot",sel_wgted_tot, agebins);

    open_r_matrix("sel.m.cH");
    wrt_r_matrix(sel_cH, 2, 2);
    wrt_r_namevector(styr,endyr);
    wrt_r_namevector(agebins);
    close_r_matrix();

    open_r_matrix("sel.m.cD");
    wrt_r_matrix(sel_cD, 2, 2);
    wrt_r_namevector(styr,endyr);
    wrt_r_namevector(agebins);
    close_r_matrix();

    open_r_matrix("sel.m.HB");
    wrt_r_matrix(sel_HB, 2, 2);
    wrt_r_namevector(styr,endyr);
    wrt_r_namevector(agebins);
    close_r_matrix();

    open_r_matrix("sel.m.GR");
    wrt_r_matrix(sel_GR, 2, 2);
    wrt_r_namevector(styr,endyr);
    wrt_r_namevector(agebins);
    close_r_matrix();

    open_r_matrix("sel.m.D");
    wrt_r_matrix(sel_D, 2, 2);
    wrt_r_namevector(styr,endyr);
    wrt_r_namevector(agebins);
    close_r_matrix();
	
    open_r_matrix("sel.m.CVT");
    wrt_r_matrix(sel_CVT, 2, 2);
    wrt_r_namevector(styr,endyr);
    wrt_r_namevector(agebins);
    close_r_matrix();

    open_r_matrix("sel.m.VID");		//KC
    wrt_r_matrix(sel_VID, 2, 2);
    wrt_r_namevector(styr,endyr);
    wrt_r_namevector(agebins);
    close_r_matrix();
	
 close_r_list();


//LIST object with predicted and observed composition data
open_r_list("comp.mats");

    //KC open_r_matrix("lcomp.cH.ob");
    //KC wrt_r_matrix(obs_cH_lenc, 2, 2);
    //KC wrt_r_namevector(yrs_cH_lenc);
    //KC wrt_r_namevector(lenbins);
    //KC close_r_matrix();

    //KC open_r_matrix("lcomp.cH.pr");
    //KC wrt_r_matrix(pred_cH_lenc, 2, 2);
    //KC wrt_r_namevector(yrs_cH_lenc);
    //KC wrt_r_namevector(lenbins);
    //KC close_r_matrix();

    //KC open_r_matrix("lcomp.cD.ob");
    //KC wrt_r_matrix(obs_cD_lenc, 2, 2);
    //KC wrt_r_namevector(yrs_cD_lenc);
    //KC wrt_r_namevector(lenbins);
    //KC close_r_matrix();

    //KC open_r_matrix("lcomp.cD.pr");
    //KC wrt_r_matrix(pred_cD_lenc, 2, 2);
    //KC wrt_r_namevector(yrs_cD_lenc);
    //KC wrt_r_namevector(lenbins);
    //KC close_r_matrix();

    open_r_matrix("lcomp.HB.ob");
    wrt_r_matrix(obs_HB_lenc, 2, 2);
    wrt_r_namevector(yrs_HB_lenc);
    wrt_r_namevector(lenbins);
    close_r_matrix();

    open_r_matrix("lcomp.HB.pr");
    wrt_r_matrix(pred_HB_lenc, 2, 2);
    wrt_r_namevector(yrs_HB_lenc);
    wrt_r_namevector(lenbins);
    close_r_matrix();

    //KC open_r_matrix("lcomp.GR.ob");
    //KC wrt_r_matrix(obs_GR_lenc, 2, 2);
    //KC wrt_r_namevector(yrs_GR_lenc);
    //KC wrt_r_namevector(lenbins);
    //KC close_r_matrix();

    //KC open_r_matrix("lcomp.GR.pr");
    //KC wrt_r_matrix(pred_GR_lenc, 2, 2);
    //KC wrt_r_namevector(yrs_GR_lenc);
    //KC wrt_r_namevector(lenbins);
    //KC close_r_matrix();
	
    open_r_matrix("lcomp.HB.D.ob");
    wrt_r_matrix(obs_HB_D_lenc, 2, 2);
    wrt_r_namevector(yrs_HB_D_lenc);
    wrt_r_namevector(lenbins);
    close_r_matrix();

    open_r_matrix("lcomp.HB.D.pr");
    wrt_r_matrix(pred_HB_D_lenc, 2, 2);
    wrt_r_namevector(yrs_HB_D_lenc);
    wrt_r_namevector(lenbins);
    close_r_matrix();
	
    //KC open_r_matrix("lcomp.GR.D.ob");		//KC added section below
    //KC wrt_r_matrix(obs_GR_D_lenc, 2, 2);
    //KC wrt_r_namevector(yrs_GR_D_lenc);
    //KC wrt_r_namevector(lenbins);
    //KC close_r_matrix();

    //KC open_r_matrix("lcomp.GR.D.pr");		//KC added section below
    //KC wrt_r_matrix(pred_GR_D_lenc, 2, 2);
    //KC wrt_r_namevector(yrs_GR_D_lenc);
    //KC wrt_r_namevector(lenbins);
    //KC close_r_matrix();
	
    //KC open_r_matrix("lcomp.VID.ob");
    //KC wrt_r_matrix(obs_VID_lenc, 2, 2);
    //KC wrt_r_namevector(yrs_VID_lenc);
    //KC wrt_r_namevector(lenbins);
    //KC close_r_matrix();

    //KC open_r_matrix("lcomp.CVT.pr");
    //KC wrt_r_matrix(pred_CVT_lenc, 2, 2);
    //KC wrt_r_namevector(yrs_CVT_lenc);
    //KC wrt_r_namevector(lenbins);
    //KC close_r_matrix();
	
    open_r_matrix("acomp.cH.ob");
    wrt_r_matrix(obs_cH_agec, 2, 2);
    wrt_r_namevector(yrs_cH_agec);
    wrt_r_namevector(agebins_agec);
    close_r_matrix();

    open_r_matrix("acomp.cH.pr");
    wrt_r_matrix(pred_cH_agec, 2, 2);
    wrt_r_namevector(yrs_cH_agec);
    wrt_r_namevector(agebins_agec);
    close_r_matrix();

    open_r_matrix("acomp.cD.ob");		//KC added section below
    wrt_r_matrix(obs_cD_agec, 2, 2);
    wrt_r_namevector(yrs_cD_agec);
    wrt_r_namevector(agebins_agec);
    close_r_matrix();

    open_r_matrix("acomp.cD.pr");		//KC added section below
    wrt_r_matrix(pred_cD_agec, 2, 2);
    wrt_r_namevector(yrs_cD_agec);
    wrt_r_namevector(agebins_agec);
    close_r_matrix();
	
    open_r_matrix("acomp.HB.ob");
    wrt_r_matrix(obs_HB_agec, 2, 2);
    wrt_r_namevector(yrs_HB_agec);
    wrt_r_namevector(agebins_agec);
    close_r_matrix();

    open_r_matrix("acomp.HB.pr");
    wrt_r_matrix(pred_HB_agec, 2, 2);
    wrt_r_namevector(yrs_HB_agec);
    wrt_r_namevector(agebins_agec);
    close_r_matrix();
		
    //KC open_r_matrix("acomp.GR.ob");
    //KC wrt_r_matrix(obs_GR_agec, 2, 2);
    //KC wrt_r_namevector(yrs_GR_agec);
    //KC wrt_r_namevector(agebins_agec);
    //KC close_r_matrix();

    //KC open_r_matrix("acomp.GR.pr");
    //KC wrt_r_matrix(pred_GR_agec, 2, 2);
    //KC wrt_r_namevector(yrs_GR_agec);
    //KC wrt_r_namevector(agebins_agec);
    //KC close_r_matrix();

    open_r_matrix("acomp.CVT.ob");
    wrt_r_matrix(obs_CVT_agec, 2, 2);
    wrt_r_namevector(yrs_CVT_agec);
    wrt_r_namevector(agebins_agec);
    close_r_matrix();

    open_r_matrix("acomp.CVT.pr");
    wrt_r_matrix(pred_CVT_agec, 2, 2);
    wrt_r_namevector(yrs_CVT_agec);
    wrt_r_namevector(agebins_agec);
    close_r_matrix();

 close_r_list();

// DATA FRAME of time series
open_r_df("t.series", styr, (endyr+1), 2);
    wrt_r_namevector(styr,(endyr+1));
    wrt_r_df_col("year", styr,(endyr+1));
    wrt_r_df_col("F.Fmsy", FdF_msy);
//KC	wrt_r_df_col("F.F30.ratio", FdF30);	//*.ratio extension is for FishGraph
    wrt_r_df_col("F.full", Fapex);
    wrt_r_df_col("F.cH", F_cH_out);
    wrt_r_df_col("F.cD", F_cD_out);	
    wrt_r_df_col("F.HB", F_HB_out);
    wrt_r_df_col("F.GR", F_GR_out);
    wrt_r_df_col("F.cH.D", F_cH_D_out);
    wrt_r_df_col("F.HB.D", F_HB_D_out);
    wrt_r_df_col("F.GR.D", F_GR_D_out);
    wrt_r_df_col("Fsum", Fsum);
    wrt_r_df_col("N", totN); //abundance at start of year
    wrt_r_df_col("recruits", rec);
    wrt_r_df_col("logR.dev", log_rec_dev_output); //places zeros in yrs deviations not estimated  //KWS
    wrt_r_df_col("SSB", SSB);
    wrt_r_df_col("SSB.SSBmsy", SdSSB_msy);
    //wrt_r_df_col("SSB.msst.M", SdSSB_msy/smsy2msstM);
    wrt_r_df_col("SSB.msst", SdSSB_msy/smsy2msst75);
	wrt_r_df_col("SSB.SSBF30", SdSSB_F30);
    wrt_r_df_col("SSB.msstF30", Sdmsst_F30);
    wrt_r_df_col("B", totB);
    wrt_r_df_col("B.B0", totB/B0);
    wrt_r_df_col("SPR.static", spr_static);

    wrt_r_df_col("total.L.klb", L_total_klb_yr);
    wrt_r_df_col("total.L.knum", L_total_knum_yr);
    wrt_r_df_col("total.D.klb", D_total_klb_yr);
    wrt_r_df_col("total.D.knum", D_total_knum_yr);

    //KC wrt_r_df_col("U.cH.ob", obs_cH_cpue);
    //KC wrt_r_df_col("U.cH.pr", pred_cH_cpue);
    //KC wrt_r_df_col("cv.U.cH", cH_cpue_cv/w_I_cH);  //applied CV after weighting
    //KC wrt_r_df_col("cv.unwgted.U.cH", cH_cpue_cv); //CV before weighting

    wrt_r_df_col("U.HB.ob", obs_HB_cpue);
    wrt_r_df_col("U.HB.pr", pred_HB_cpue);
    wrt_r_df_col("cv.U.HB", HB_cpue_cv/w_I_HB);
    wrt_r_df_col("cv.unwgted.U.HB", HB_cpue_cv);

    // wrt_r_df_col("U.GR.ob", obs_GR_cpue);
    // wrt_r_df_col("U.GR.pr", pred_GR_cpue);
    // wrt_r_df_col("cv.U.GR", GR_cpue_cv/w_I_GR);
    // wrt_r_df_col("cv.unwgted.U.GR", GR_cpue_cv);
	
    wrt_r_df_col("U.VID.ob", obs_VID_cpue);
    wrt_r_df_col("U.VID.pr", pred_VID_cpue);
    wrt_r_df_col("cv.U.VID", VID_cpue_cv/w_I_VID);
    wrt_r_df_col("cv.unwgted.U.VID", VID_cpue_cv);
	
    //KC wrt_r_df_col("q.cH", q_cH);
    //KC wrt_r_df_col("q.cH.rate.mult",q_rate_fcn_cH);
    //KC wrt_r_df_col("q.cH.RW.log.dev",q_RW_log_dev_cH);

    wrt_r_df_col("q.HB", q_HB);
    wrt_r_df_col("q.HB.rate.mult",q_rate_fcn_HB);
    wrt_r_df_col("q.HB.RW.log.dev",q_RW_log_dev_HB);

    // wrt_r_df_col("q.GR", q_GR);
    // wrt_r_df_col("q.GR.rate.mult",q_rate_fcn_GR);
    // wrt_r_df_col("q.GR.RW.log.dev",q_RW_log_dev_GR);

    wrt_r_df_col("q.CVID", q_VID);
    wrt_r_df_col("q.VID.RW.log.dev",q_RW_log_dev_VID);
	
    wrt_r_df_col("q.DD.mult", q_DD_fcn);
    wrt_r_df_col("q.DD.B.exploitable", B_q_DD);

    wrt_r_df_col("L.cH.ob", obs_cH_L);
    wrt_r_df_col("L.cH.pr", pred_cH_L_klb);
    wrt_r_df_col("cv.L.cH", cH_L_cv);

    wrt_r_df_col("L.cD.ob", obs_cD_L);
    wrt_r_df_col("L.cD.pr", pred_cD_L_klb);
    wrt_r_df_col("cv.L.cD", cD_L_cv);

    wrt_r_df_col("L.HB.ob", obs_HB_L);
    wrt_r_df_col("L.HB.pr", pred_HB_L_knum);
    wrt_r_df_col("cv.L.HB", HB_L_cv);

    wrt_r_df_col("L.GR.ob", obs_GR_L);
    wrt_r_df_col("L.GR.pr", pred_GR_L_knum);
    wrt_r_df_col("cv.L.GR", GR_L_cv);

    wrt_r_df_col("D.cH.ob", obs_cH_D);
    wrt_r_df_col("D.cH.pr", pred_cH_D_knum);
    wrt_r_df_col("cv.D.cH", cH_D_cv);

    wrt_r_df_col("D.HB.ob", obs_HB_D);
    wrt_r_df_col("D.HB.pr", pred_HB_D_knum);
    wrt_r_df_col("cv.D.HB", HB_D_cv);

    wrt_r_df_col("D.GR.ob", obs_GR_D);
    wrt_r_df_col("D.GR.pr", pred_GR_D_knum);
    wrt_r_df_col("cv.D.GR", GR_D_cv);

    //comp sample sizes
	//KC wrt_r_df_col("lcomp.cH.n", nsamp_cH_lenc_allyr);
	//KC wrt_r_df_col("lcomp.cD.n", nsamp_cD_lenc_allyr);
	wrt_r_df_col("lcomp.HB.n", nsamp_HB_lenc_allyr);
	//KCwrt_r_df_col("lcomp.GR.n", nsamp_GR_lenc_allyr);
    	wrt_r_df_col("lcomp.HB.D.n", nsamp_HB_D_lenc_allyr);
	//KC wrt_r_df_col("lcomp.GR.D.n", nsamp_GR_D_lenc_allyr);	//KC
	//KC wrt_r_df_col("lcomp.CVT.n", nsamp_CVT_lenc_allyr);
	
    	wrt_r_df_col("acomp.cH.n", nsamp_cH_agec_allyr);
	wrt_r_df_col("acomp.cD.n", nsamp_cD_agec_allyr);  //KC
   	wrt_r_df_col("acomp.HB.n", nsamp_HB_agec_allyr);
	//KC wrt_r_df_col("acomp.GR.n", nsamp_GR_agec_allyr);
	wrt_r_df_col("acomp.CVT.n", nsamp_CVT_agec_allyr);

	//KC wrt_r_df_col("lcomp.cH.nfish", nfish_cH_lenc_allyr);
	//KC wrt_r_df_col("lcomp.cD.nfish", nfish_cD_lenc_allyr);
	wrt_r_df_col("lcomp.HB.nfish", nfish_HB_lenc_allyr);
	//KC wrt_r_df_col("lcomp.GR.nfish", nfish_GR_lenc_allyr);
    	wrt_r_df_col("lcomp.HB.D.nfish", nfish_HB_D_lenc_allyr);
	//KC wrt_r_df_col("lcomp.GR.D.nfish", nfish_GR_D_lenc_allyr);		//KC
	//KC wrt_r_df_col("lcomp.CVT.nfish", nfish_CVT_lenc_allyr);
	
    	wrt_r_df_col("acomp.cH.nfish", nfish_cH_agec_allyr);
	wrt_r_df_col("acomp.cD.nfish", nfish_cD_agec_allyr);	//KC
   	wrt_r_df_col("acomp.HB.nfish", nfish_HB_agec_allyr);
	//KC wrt_r_df_col("acomp.GR.nfish", nfish_GR_agec_allyr);
	wrt_r_df_col("acomp.CVT.nfish", nfish_CVT_agec_allyr);
	
	//KC wrt_r_df_col("lcomp.cH.neff", (1+nsamp_cH_lenc_allyr*exp(log_dm_cH_lc_out(8)))/(1+exp(log_dm_cH_lc_out(8))) );
	//KC wrt_r_df_col("lcomp.cD.neff", (1+nsamp_cD_lenc_allyr*exp(log_dm_cD_lc_out(8)))/(1+exp(log_dm_cD_lc_out(8))) );
	wrt_r_df_col("lcomp.HB.neff", (1+nsamp_HB_lenc_allyr*exp(log_dm_HB_lc_out(8)))/(1+exp(log_dm_HB_lc_out(8))) );
	//KC wrt_r_df_col("lcomp.GR.neff", (1+nsamp_GR_lenc_allyr*exp(log_dm_GR_lc_out(8)))/(1+exp(log_dm_GR_lc_out(8))) );
    	wrt_r_df_col("lcomp.HB.D.neff", (1+nsamp_HB_D_lenc_allyr*exp(log_dm_HB_D_lc_out(8)))/(1+exp(log_dm_HB_D_lc_out(8))) );
	//KC wrt_r_df_col("lcomp.GR.D.neff", (1+nsamp_GR_D_lenc_allyr*exp(log_dm_GR_D_lc_out(8)))/(1+exp(log_dm_GR_D_lc_out(8))) );		//KC
	//KC wrt_r_df_col("lcomp.CVT.neff", (1+nsamp_CVT_lenc_allyr*exp(log_dm_CVT_lc_out(8)))/(1+exp(log_dm_CVT_lc_out(8))) );
	
    	wrt_r_df_col("acomp.cH.neff", (1+nsamp_cH_agec_allyr*exp(log_dm_cH_ac_out(8)))/(1+exp(log_dm_cH_ac_out(8))) );
	wrt_r_df_col("acomp.cD.neff", (1+nsamp_cD_agec_allyr*exp(log_dm_cD_ac_out(8)))/(1+exp(log_dm_cD_ac_out(8))) );		//KC
    	wrt_r_df_col("acomp.HB.neff", (1+nsamp_HB_agec_allyr*exp(log_dm_HB_ac_out(8)))/(1+exp(log_dm_HB_ac_out(8))) );
	//KC wrt_r_df_col("acomp.GR.neff", (1+nsamp_GR_agec_allyr*exp(log_dm_GR_ac_out(8)))/(1+exp(log_dm_GR_ac_out(8))) );
	wrt_r_df_col("acomp.CVT.neff", (1+nsamp_CVT_agec_allyr*exp(log_dm_CVT_ac_out(8)))/(1+exp(log_dm_CVT_ac_out(8))) );    

 close_r_df();

// DATA FRAME of L and D time series by fishery
open_r_df("LD.pr.tseries", styr, endyr, 2);
    wrt_r_namevector(styr,endyr);
    wrt_r_df_col("year", styr, endyr);

    wrt_r_df_col("L.cH.gutklb", pred_cH_L_klb);		//KC	changed .klb to .gutklb (i.e., L.cH.klb now L.cH.gutklb)
    wrt_r_df_col("L.cH.knum", pred_cH_L_knum);		//KC	changed .klb to .gutklb
    wrt_r_df_col("L.cD.gutklb", pred_cD_L_klb);		//KC	changed .klb to .gutklb
    wrt_r_df_col("L.cD.knum", pred_cD_L_knum);		//KC	changed .klb to .gutklb
    wrt_r_df_col("L.HB.gutklb", pred_HB_L_klb);		//KC	changed .klb to .gutklb
    wrt_r_df_col("L.HB.knum", pred_HB_L_knum);		//KC	changed .klb to .gutklb
    wrt_r_df_col("L.GR.gutklb", pred_GR_L_klb);		//KC	changed .klb to .gutklb
    wrt_r_df_col("L.GR.knum", pred_GR_L_knum);		//KC	changed .klb to .gutklb

    wrt_r_df_col("D.cH.gutklb", pred_cH_D_klb);		//KC	changed .klb to .gutklb
    wrt_r_df_col("D.cH.knum", pred_cH_D_knum);		//KC	changed .klb to .gutklb
    wrt_r_df_col("D.HB.gutklb", pred_HB_D_klb);		//KC	changed .klb to .gutklb
    wrt_r_df_col("D.HB.knum", pred_HB_D_knum);		//KC	changed .klb to .gutklb
    wrt_r_df_col("D.GR.gutklb", pred_GR_D_klb);		//KC	changed .klb to .gutklb
    wrt_r_df_col("D.GR.knum", pred_GR_D_knum);		//KC	changed .klb to .gutklb

close_r_df();

 	open_r_df("a.series", 1, nages, 2);
 	wrt_r_namevector(1,nages);
 	wrt_r_df_col("age", agebins);
 	wrt_r_df_col("length", meanlen_TL);
 	wrt_r_df_col("length.cv", len_cv);
 	wrt_r_df_col("length.sd", len_sd);
 	wrt_r_df_col("weight", wgt_lb);     //for FishGraph
 	wrt_r_df_col("weight.gut", wgt_lb_gut);     //for FishGraph	//KC added
	wrt_r_df_col("wgt.klb", wgt_klb);
	wrt_r_df_col("wgt.klb.gut", wgt_klb_gut);			//KC added
 	wrt_r_df_col("wgt.mt", wgt_mt);
	wrt_r_df_col("gutwgt.wgted.L.klb", wgt_wgted_L_klb);		//KC added
 	wrt_r_df_col("gutwgt.wgted.D.klb", wgt_wgted_D_klb);		//KC added
 //KC   wrt_r_df_col("wholewgt.wgted.L.klb", wgt_wgted_L_klb);
 //KC	wrt_r_df_col("wholewgt.wgted.D.klb", wgt_wgted_D_klb);
        wrt_r_df_col("prop.female", prop_f);
        wrt_r_df_col("prop.male", prop_m);	
   	wrt_r_df_col("mat.female", maturity_f);
   	wrt_r_df_col("mat.male", maturity_m);	
 	wrt_r_df_col("reprod", reprod);
 	
 	wrt_r_df_col("M", M);
 	wrt_r_df_col("F.initial", F_initial);
 	wrt_r_df_col("Z.initial", Z_initial);
 	wrt_r_df_col("Nage.eq.init",N_initial_eq);
 	wrt_r_df_col("log.Nage.init.dev",log_Nage_dev_output);
 close_r_df();


 open_r_df("eq.series", 1, n_iter_msy, 2);
 	wrt_r_namevector(1,n_iter_msy);
 	wrt_r_df_col("F.eq", F_msy);
 	wrt_r_df_col("spr.eq", spr_msy);
 	wrt_r_df_col("R.eq", R_eq);
 	wrt_r_df_col("SSB.eq", SSB_eq);
 	wrt_r_df_col("B.eq", B_eq);
 	wrt_r_df_col("L.eq.gutklb", L_eq_klb);		//KC changed .klb to .gutklb
 	wrt_r_df_col("L.eq.knum", L_eq_knum);
 	wrt_r_df_col("D.eq.gutklb", D_eq_klb);		//KC changed .klb to .gutklb
 	wrt_r_df_col("D.eq.knum", D_eq_knum);
 close_r_df();

 open_r_df("pr.series", 1, n_iter_spr, 2);
 	wrt_r_namevector(1,n_iter_spr);
 	wrt_r_df_col("F.spr", F_spr);
 	wrt_r_df_col("spr", spr_spr);
 	wrt_r_df_col("SPR", spr_ratio);
//KC	wrt_r_df_col("ypr.lb", L_spr); //whole weight
 	wrt_r_df_col("ypr.lb.gutted", L_spr); //gutted weight		//KC added
 close_r_df();


 open_r_list("CLD.est.mats");

     open_r_matrix("Lw.cH");
         wrt_r_matrix(L_cH_klb, 1,1);
     close_r_matrix();

     open_r_matrix("Lw.cD");
         wrt_r_matrix(L_cD_klb, 1,1);
     close_r_matrix();
	 
     open_r_matrix("Lw.HB");
         wrt_r_matrix(L_HB_klb, 1,1);
     close_r_matrix();

     open_r_matrix("Lw.GR");
         wrt_r_matrix(L_GR_klb, 1,1);
     close_r_matrix();

     open_r_matrix("Lw.total");
         wrt_r_matrix(L_total_klb, 1,1);
     close_r_matrix();

     open_r_matrix("Ln.cH");
         wrt_r_matrix(L_cH_num, 1,1);
     close_r_matrix();

     open_r_matrix("Ln.cD");
         wrt_r_matrix(L_cD_num, 1,1);
     close_r_matrix();
	 
     open_r_matrix("Ln.HB");
         wrt_r_matrix(L_HB_num, 1,1);
     close_r_matrix();

     open_r_matrix("Ln.GR");
         wrt_r_matrix(L_GR_num, 1,1);
     close_r_matrix();

     open_r_matrix("Ln.total");
         wrt_r_matrix(L_total_num, 1,1);
     close_r_matrix();

     open_r_matrix("Dw.cH");
         wrt_r_matrix(D_cH_klb, 1,1);
     close_r_matrix();

     open_r_matrix("Dw.HB");
         wrt_r_matrix(D_HB_klb, 1,1);
     close_r_matrix();

     open_r_matrix("Dw.GR");
         wrt_r_matrix(D_GR_klb, 1,1);
     close_r_matrix();

     open_r_matrix("Dw.total");
         wrt_r_matrix(D_total_klb, 1,1);
     close_r_matrix();

     open_r_matrix("Dn.cH");
         wrt_r_matrix(D_cH_num, 1,1);
     close_r_matrix();

     open_r_matrix("Dn.HB");
         wrt_r_matrix(D_HB_num, 1,1);
     close_r_matrix();

     open_r_matrix("Dn.GR");
         wrt_r_matrix(D_GR_num, 1,1);
     close_r_matrix();

     open_r_matrix("Dn.total");
         wrt_r_matrix(D_total_num, 1,1);
     close_r_matrix();

  close_r_list();

  //LIST object of age error matrix
open_r_list("age.error");

    open_r_matrix("error.mat");		
    wrt_r_matrix(age_error, 2, 2);
    wrt_r_namevector(agebins);
    wrt_r_namevector(agebins);
    close_r_matrix();
  
close_r_list();

// DATA FRAME of projection time series
open_r_df("projection", styr_proj, endyr_proj, 2);
    wrt_r_namevector(styr_proj,endyr_proj);
    wrt_r_df_col("year", styr_proj,endyr_proj);
    wrt_r_df_col("F.proj", F_proj);
    wrt_r_df_col("SSB.proj", SSB_proj);
    wrt_r_df_col("B.proj", B_proj);
    wrt_r_df_col("R.proj", R_proj);
	wrt_r_df_col("L.knum.proj", L_knum_proj);
	wrt_r_df_col("L.klb.proj", L_klb_proj);
	wrt_r_df_col("D.knum.proj", D_knum_proj);
	wrt_r_df_col("D.klb.proj", D_klb_proj);	
 close_r_df();

close_r_file();
