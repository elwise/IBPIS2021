#V3.30.11.00-trans;_2018_04_11;_Stock_Synthesis_by_Richard_Methot_(NOAA)_using_ADMB_11.6
#This is a work of the U.S. Government and is not subject to copyright protection in the United States.
#Foreign copyrights may apply. See copyright.txt for more information.
#_user_support_available_at:NMFS.Stock.Synthesis@noaa.gov
#_user_info_available_at:https://vlab.ncep.noaa.gov/group/stock-synthesis
#C Sardine in 8c and 9a: ASSESSMENT 2020
#C growth parameters are estimated spawner-recruitment bias adjustment Not tuned For optimality
#_data_and_control_files: sardine.dat // sardine.ctl
1  # 0 means do not read wtatage.ss; 1 means read and use wtatage.ss and also read and use growth parameters
1  #_N_Growth_Patterns
1 #_N_platoons_Within_GrowthPattern 
#_Cond 1 #_Morph_between/within_stdev_ratio (no read if N_morphs=1)
#_Cond  1 #vector_Morphdist_(-1_in_first_val_gives_normal_approx)
#
4 # recr_dist_method for parameters:  2=main effects for GP, Settle timing, Area; 3=each Settle entity; 4=none when N_GP*Nsettle*pop==1
1 # not yet implemented; Future usage: Spawner-Recruitment: 1=global; 2=by area
1 #  number of recruitment settlement assignments 
0 # unused option
#GPattern month  area  age (for each settlement assignment)
 1 1 1 0
#
#_Cond 0 # N_movement_definitions goes here if Nareas > 1
#_Cond 1.0 # first age that moves (real age at begin of season, not integer) also cond on do_migration>0
#_Cond 1 1 1 2 4 10 # example move definition for seas=1, morph=1, source=1 dest=2, age1=4, age2=10
#
0 #_Nblock_Patterns
# 2 1 #_blocks_per_pattern 
# begin and end years of blocks
# 1988 2005 2006 2020
# 1977 1977
#
# controls for all timevary parameters 
1 #_env/block/dev_adjust_method for all time-vary parms (1=warn relative to base parm bounds; 3=no bound check)
#  autogen
1 1 1 1 1 # autogen: 1st element for biology, 2nd for SR, 3rd for Q, 4th reserved, 5th for selex
# where: 0 = autogen all time-varying parms; 1 = read each time-varying parm line; 2 = read then autogen if parm min==-12345
# 
#
# setup for M, growth, maturity, fecundity, recruitment distibution, movement 
#
3 #_natM_type:_0=1Parm; 1=N_breakpoints;_2=Lorenzen;_3=agespecific;_4=agespec_withseasinterpolate
 #_Age_natmort_by sex x growthpattern
 0.98 0.61 0.47 0.4 0.36 0.35 0.32
1 # GrowthModel: 1=vonBert with L1&L2; 2=Richards with L1&L2; 3=age_specific_K; 4=not implemented
0 #_Age(post-settlement)_for_L1;linear growth below this
6 #_Growth_Age_for_L2 (999 to use as Linf)
-999 #_exponential decay for growth above maxage (fixed at 0.2 in 3.24; value should approx initial Z; -999 replicates 3.24)
0  #_placeholder for future growth feature
0 #_SD_add_to_LAA (set to 0.1 for SS2 V1.x compatibility)
0 #_CV_Growth_Pattern:  0 CV=f(LAA); 1 CV=F(A); 2 SD=F(LAA); 3 SD=F(A); 4 logSD=F(A)
5 #_maturity_option:  1=length logistic; 2=age logistic; 3=read age-maturity matrix by growth_pattern; 4=read age-fecundity; 5=disabled; 6=read length-maturity
#_Age_Fecundity by growth pattern from wt-at-age.ss now invoked by read bodywt flag
1 #_First_Mature_Age
1 #_fecundity option:(1)eggs=Wt*(a+b*Wt);(2)eggs=a*L^b;(3)eggs=a*Wt^b; (4)eggs=a+b*L; (5)eggs=a+b*W
0 #_hermaphroditism option:  0=none; 1=female-to-male age-specific fxn; -1=male-to-female age-specific fxn
1 #_parameter_offset_approach (1=none, 2= M, G, CV_G as offset from female-GP1, 3=like SS2 V1.x)
#
#_growth_parms
#_ LO HI INIT PRIOR PR_SD PR_type PHASE env_var&link dev_link dev_minyr dev_maxyr dev_PH Block Block_Fxn
 8 18 14 0 0 0 -2 0 0 0 0 0 0 0 # L_at_Amin_Fem_GP_1
 20 25 23 0 0 0 -4 0 0 0 0 0 0 0 # L_at_Amax_Fem_GP_1
 0.2 0.8 0.4 0 0 0 -4 0 0 0 0 0 0 0 # VonBert_K_Fem_GP_1
 0.05 0.25 0.1 0 0 0 -3 0 0 0 0 0 0 0 # CV_young_Fem_GP_1
 0.05 0.25 0.1 0 0 0 -3 0 0 0 0 0 0 0 # CV_old_Fem_GP_1
 -3 3 2 0 0 0 -3 0 0 0 0 0 0 0 # Wtlen_1_Fem
 -3 4 3 0 0 0 -3 0 0 0 0 0 0 0 # Wtlen_2_Fem
 50 60 55 0 0 0 -3 0 0 0 0 0 0 0 # Mat50%_Fem
 -3 3 -0.25 0 0 0 -3 0 0 0 0 0 0 0 # Mat_slope_Fem
 -3 3 1 0 0 0 -3 0 0 0 0 0 0 0 # Eggs/kg_inter_Fem
 -3 3 0 0 0 0 -3 0 0 0 0 0 0 0 # Eggs/kg_slope_wt_Fem
# 0 0 0 0 0 0 -4 0 0 0 0 0 0 0 # RecrDist_GP_1
# 0 0 0 0 0 0 -4 0 0 0 0 0 0 0 # RecrDist_Area_1
# 0 0 0 0 0 0 -4 0 0 0 0 0 0 0 # RecrDist_timing_1
 1 1 1 1 1 0 -1 0 0 0 0 0 0 0 # CohortGrowDev
 0.000001 0.999999 0.5 0.5  0.5 0 -99 0 0 0 0 0 0 0 # FracFemale_GP_1
#
#_no timevary MG parameters
#
#_seasonal_effects_on_biology_parms
 0 0 0 0 0 0 0 0 0 0 #_femwtlen1,femwtlen2,mat1,mat2,fec1,fec2,Malewtlen1,malewtlen2,L1,K
#_ LO HI INIT PRIOR PR_SD PR_type PHASE
#_Cond -2 2 0 0 -1 99 -2 #_placeholder when no seasonal MG parameters
#
3 #_Spawner-Recruitment; Options: 2=Ricker; 3=std_B-H; 4=SCAA; 5=Hockey; 6=B-H_flattop; 7=survival_3Parm; 8=Shepherd_3Parm; 9=RickerPower_3parm
0  # 0/1 to use steepness in initial equ recruitment calculation
0  #  future feature:  0/1 to make realized sigmaR a function of SR curvature
#_          LO            HI          INIT         PRIOR         PR_SD       PR_type      PHASE    env-var    use_dev   dev_mnyr   dev_mxyr     dev_PH      Block    Blk_Fxn #  parm_name
             1            20            16           4.5             5             0          1          0          0          0          0          0          0          0 # SR_LN(R0)
             0             2          0.71           0.7          0.05             0         -1          0          0          0          0          0          0          0 # SR_BH_steep
             0             4           0.7           0.6           0.8             0         -4          0          0          0          0          0          0          0 # SR_sigmaR
            -5             5             0             0             1             0         -2          0          0          0          0          0          0          0 # SR_regime
             0             0             0             0             0             0        -99          0          0          0          0          0          0          0 # SR_autocorr
1 #do_recdev:  0=none; 1=devvector; 2=simple deviations
1978 # first year of main recr_devs; early devs can preceed this era
2020 # last year of main recr_devs; forecast devs start in following year
3 #_recdev phase 
1 # (0/1) to read 13 advanced options
 -5 #_recdev_early_start (0=none; neg value makes relative to recdev_start)
 4 #_recdev_early_phase
 -1 #_forecast_recruitment phase (incl. late recr) (0 value resets to maxphase+1)
 1 #_lambda for Fcast_recr_like occurring before endyr+1
 1971.7 #_last_yr_nobias_adj_in_MPD; begin of ramp
 1976.5 #_first_yr_fullbias_adj_in_MPD; begin of plateau
 2019.3 #_last_yr_fullbias_adj_in_MPD
 2028.7 #_end_yr_for_ramp_in_MPD (can be in forecast to shape ramp, but SS sets bias_adj to 0.0 for fcast yrs)
 0.9557 #_max_bias_adj_in_MPD (-1 to override ramp and set biasadj=1.0 for all estimated recdevs)
 0 #_period of cycles in recruitment (N parms read below)
 -5 #min rec_dev
 5 #max rec_dev
 0 #_read_recdevs
#_end of advanced SR options
#
#_placeholder for full parameter lines for recruitment cycles
# read specified recr devs
#_Yr Input_value
#
# all recruitment deviations
#  1974E 1975E 1976E 1977E 1978R 1979R 1980R 1981R 1982R 1983R 1984R 1985R 1986R 1987R 1988R 1989R 1990R 1991R 1992R 1993R 1994R 1995R 1996R 1997R 1998R 1999R 2000R 2001R 2002R 2003R 2004R 2005R 2006R 2007R 2008R 2009R 2010R 2011R 2012R 2013R 2014R 2015R 2016R 2017F 2018F 2019F
#  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
# implementation error by year in forecast:  0
#
#Fishing Mortality info 
0.3 # F ballpark
-2001 # F ballpark year (neg value to disable)
3 # F_Method:  1=Pope; 2=instan. F; 3=hybrid (hybrid is recommended)
7 # max F or harvest rate, depends on F_Method
# no additional F input needed for Fmethod 1
# if Fmethod=2; read overall start F value; overall phase; N detailed inputs to read
# if Fmethod=3; read N iterations for tuning for Fmethod 3
7  # N iterations for tuning F in hybrid method (recommend 3 to 7)
#
# if Fmethod=2; read overall start F value; overall phase; N detailed inputs to read
#0.2 2 0

#_initial_F_parms; count = 1
#_ LO HI INIT PRIOR PR_SD  PR_type  PHASE
 -1 2 0.3 0.3 0.2 0 1 # InitF_seas_1_flt_1purse_seine
#2019 2038
# F rates by fleet
# Yr:  1978 1979 1980 1981 1982 1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019
# seas:  1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
# purse_seine 0.946533 1.81208 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1.92086 0.933049 0.565168 0.274869 0.136794 0.13563 0.118369 0.118369
#
#_Q_setup for fleets with cpue or survey data
#_1:  link type: (1=simple q, 1 parm; 2=mirror simple q, 1 mirrored parm; 3=q and power, 2 parm)
#_2:  extra input for link, i.e. mirror fleet
#_3:  0/1 to select extra sd parameter
#_4:  0/1 for biasadj or not
#_5:  0/1 to float
#_   fleet      link link_info  extra_se   biasadj     float  #  fleetname
         2         1         0         1         0         1  #  Acoustic_survey
         3         1         0         1         0         1  #  DEPM_survey
         4         1         0         0         0         0  #  Rec_survey
-9999 0 0 0 0 0
#
#_Q_parms(if_any);Qunits_are_ln(q)
#_          LO            HI          INIT         PRIOR         PR_SD       PR_type      PHASE    env-var    use_dev   dev_mnyr   dev_mxyr     dev_PH      Block    Blk_Fxn  #  parm_name
            -3             3           0.75             0             1             0         -1          0          0          0          0          0          0          0  #  LnQ_base_Acoustic_survey(2)
             0             1           0.3              0.1           0.1           0          3          0          0          0          0          0          0          0  #  Q_extraSD_Acoustic(2)
            -3             3           0.26             0             1             0         -1          0          0          0          0          0          0          0  #  LnQ_base_DEPM_survey(3)
             0             1           0.3              0.1           0.1           0          3          0          0          0          0          0          0          0  #  Q_extraSD_Acoustic(2)
            -3             3             0             0             1             0          1          0          0          0          0          0          0          0  #  LnQ_base_Rec_survey(4)
#_no timevary Q parameters
#
#_size_selex_patterns
#Pattern:_0; parm=0; selex=1.0 for all sizes
#Pattern:_1; parm=2; logistic; with 95% width specification
#Pattern:_5; parm=2; mirror another size selex; PARMS pick the min-max bin to mirror
#Pattern:_15; parm=0; mirror another age or length seller
#Pattern:_6; parm=2+special; non-parm len selex
#Pattern:_43; parm=2+special+2;  like 6, with 2 additional param for scaling (average over bin range)
#Pattern:_8; parm=8; New doublelogistic with smooth transitions and constant above Linf option
#Pattern:_9; parm=6; simple 4-parm double logistic with starting length; parm 5 is first length; parm 6=1 does desc as offset
#Pattern:_21; parm=2+special; non-parm len selex, read as pairs of size, then selex
#Pattern:_22; parm=4; double_normal as in CASAL
#Pattern:_23; parm=6; double_normal where final value is directly equal to sp(6) so can be >1.0
#Pattern:_24; parm=6; double_normal with sel(minL) and sel(maxL), using joiners
#Pattern:_25; parm=3; exponential-logistic in size
#Pattern:_27; parm=3+special; cubic spline 
#Pattern:_42; parm=2+special+3; // like 27, with 2 additional param for scaling (average over bin range)
#_discard_options:_0=none;_1=define_retention;_2=retention&mortality;_3=all_discarded_dead;_4=define_dome-shaped_retention
#_Pattern Discard Male Special
 0 0 0 0 # 1 purse_seine
 0 0 0 0 # 2 Acoustic_survey
 0 0 0 0 # 3 DEPM_survey
 0 0 0 0 # 4 Rec_survey
#
#_age_selex_types
#Pattern:_0; parm=0; selex=1.0 for ages 0 to maxage
#Pattern:_10; parm=0; selex=1.0 for ages 1 to maxage
#Pattern:_11; parm=2; selex=1.0  for specified min-max age
#Pattern:_12; parm=2; age logistic
#Pattern:_13; parm=8; age double logistic
#Pattern:_14; parm=nages+1; age empirical
#Pattern:_15; parm=0; mirror another age or length selex
#Pattern:_16; parm=2; Coleraine - Gaussian
#Pattern:_17; parm=nages+1; empirical as random walk  N parameters to read can be overridden by setting special to non-zero
#Pattern:_41; parm=2+nages+1; // like 17, with 2 additional param for scaling (average over bin range)
#Pattern:_18; parm=8; double logistic - smooth transition
#Pattern:_19; parm=6; simple 4-parm double logistic with starting age
#Pattern:_20; parm=6; double_normal,using joiners
#Pattern:_26; parm=3; exponential-logistic in age
#Pattern:_27; parm=3+special; cubic spline in age
#Pattern:_42; parm=2+nages+1; // cubic spline; with 2 additional param for scaling (average over bin range)
#_Pattern Discard Male Special
 17 0 0 0 # 1 purse_seine
 17 0 0 0 # 2 Acoustic_survey
 11 0 0 0 # 3 DEPM_survey
 11 0 0 0 # 4 Rec_survey
#
#_          LO            HI          INIT         PRIOR         PR_SD       PR_type      PHASE    env-var    use_dev   dev_mnyr   dev_mxyr     dev_PH      Block    Blk_Fxn  #  parm_name
            -4             4             0             0          0.01             0         -2          0          0          0          0          0          0          0  #  AgeSel_P1_purse_seine(1)
            -3             3           0.9           0.5          0.01             0          5          0          23          1978          2020          5          0          0  #  AgeSel_P2_purse_seine(1)
            -4             4           0.4           0.5          0.01             0          5          0          23          1978          2020         5          0          0  #  AgeSel_P3_purse_seine(1)
            -4             4           0.1           0.3          0.01             0          4          0          0          0          0          0          0          0  #  AgeSel_P4_purse_seine(1)
            -4             4             0           0.1          0.01             0          -4          0          0          0          0          0          0          0  #  AgeSel_P5_purse_seine(1)
            -4             4             0           0.1          0.01             0          -4          0          0          0          0          0          0          0  #  AgeSel_P6_purse_seine(1)
            -4             4             0           0.5          0.01             0          -4          0          0          0          0          0          0          0  #  AgeSel_P7_purse_seine(1)

         -1000            -4         -1000           -6          0.01              0         -2          0          0          0          0          0          0          0  #  AgeSel_P1_Acoustic_survey(2)
            -4             4             0           0.5          0.01             0         -4          0          0          0          0          0          0          0  #  AgeSel_P2_Acoustic_survey(2)
            -4             4             0             0          0.01             0          4          0          0          0          0          0          0          0  #  AgeSel_P3_Acoustic_survey(2)
            -4             4             0             0          0.01             0          4          0          0          0          0          0          0          0  #  AgeSel_P4_Acoustic_survey(2)
            -4             4             0             0          0.01             0          -4          0          0          0          0          0          0          0  #  AgeSel_P5_Acoustic_survey(2)
            -4             4             0             0          0.01             0          -4          0          0          0          0          0          0          0  #  AgeSel_P6_Acoustic_survey(2)
            -4             4             0            -1          0.01             0          -4          0          0          0          0          0          0          0  #  AgeSel_P7_Acoustic_survey(2)

#DEPM
0            12            1             -2          0.01             0         -3          0          0          0          0          0          0          0  #  AgeSel_P1_Active(3)
0            12            6              0          0.01             0         -3          0          0          0          0          0          0          0  #  AgeSel_P2_Active(3)

#Rec_survey
0            12            0             -2          0.01             0         -3          0          0          0          0          0          0          0  #  AgeSel_P1_Active(3)
0            12            0              0          0.01             0         -3          0          0          0          0          0          0          0  #  AgeSel_P2_Active(3)


# Dirichlet-Multinomial parameters controlling age-comp weights   RICK  always estimate in a very late phase!!!
#           -5            5            3             0            99             0             5          0          0          0          0          0          0          0  #  ln(EffN_mult)_1
#           -5            5            3             0            99             0             5          0          0          0          0          0          0          0  #  ln(EffN_mult)_1 
#           -5            5            3             0            99             0             5          0          0          0          0          0          0          0  #  ln(EffN_mult)_1
#           -5            5            3             0            99             0             5          0          0          0          0          0          0          0  #  ln(EffN_mult)_1

# timevary selex parameters 
#_          LO            HI          INIT         PRIOR         PR_SD       PR_type    PHASE  #  parm_name
	5 		100		1 		5 		99 		0 	-4 #FL1(1)_dev_se
        -0.99             0.99           0           0           0.5             6      -5  # FL1(1)_dev_autocorr

	5 		100		1 		5 		99 		0 	-4 #FL1(1)_dev_se
        -0.99             0.99           0           0           0.5             6      -5  # FL1(1)_dev_autocorr

# timevary selex parameters 
#_          LO            HI          INIT         PRIOR         PR_SD       PR_type    PHASE  #  parm_name
#            -4             4           0.9             1          0.01             0      4  # AgeSel_P2_purse_seine(1)_BLK1delta_1988
#            -4             4           0.9             1          0.01             0      4  # AgeSel_P2_purse_seine(1)_BLK1delta_2006
#            -4             4           0.4             1          0.01             0      4  # AgeSel_P3_purse_seine(1)_BLK1delta_1988
#            -4             4           0.4             1          0.01             0      4  # AgeSel_P3_purse_seine(1)_BLK1delta_2006
#            -4             4           0.1             1          0.01             0      4  # AgeSel_P4_purse_seine(1)_BLK1delta_1988
#            -4             4           0.1             1          0.01             0      4  # AgeSel_P4_purse_seine(1)_BLK1delta_2006
#            -4             4          -0.5             1          0.01             0      4  # AgeSel_P7_purse_seine(1)_BLK1delta_1988
#            -4             4          -0.5             1          0.01             0      4  # AgeSel_P7_purse_seine(1)_BLK1delta_2006
# info on dev vectors created for selex parms are reported with other devs after tag parameter section 
#
0   #  use 2D_AR1 selectivity(0/1):  experimental feature
#_no 2D_AR1 selex offset used
#
# Tag loss and Tag reporting parameters go next
0  # TG_custom:  0=no read; 1=read if tags exist
#_Cond -6 6 1 1 2 0.01 -4 0 0 0 0 0 0 0  #_placeholder if no parameters
#
# deviation vectors for timevary parameters
#  base   base first block   block  env  env   dev   dev   dev   dev   dev
#  type  index  parm trend pattern link  var  vectr link _mnyr  mxyr phase  dev_vector
#      5     2     1     1     3     2     0     0     0     0     0     0
#      5     3     3     1     3     2     0     0     0     0     0     0
#      5     4     5     1     3     2     0     0     0     0     0     0
#      5     7     7     1     3     2     0     0     0     0     0     0
     #
# Input variance adjustments factors: 
 #_1=add_to_survey_CV
 #_2=add_to_discard_stddev
 #_3=add_to_bodywt_CV
 #_4=mult_by_lencomp_N
 #_5=mult_by_agecomp_N
 #_6=mult_by_size-at-age_N
 #_7=mult_by_generalized_sizecomp
#_Factor  Fleet  Value
      5      1         1
      5      2         1
      4      3         1
      5      4         1
 -9999   1    0  # terminator
#
4 #_maxlambdaphase
1 #_sd_offset; must be 1 if any growthCV, sigmaR, or survey extraSD is an estimated parameter
# read 3 changes to default Lambdas (default value is 1.0)
# Like_comp codes:  1=surv; 2=disc; 3=mnwt; 4=length; 5=age; 6=SizeFreq; 7=sizeage; 8=catch; 9=init_equ_catch; 
# 10=recrdev; 11=parm_prior; 12=parm_dev; 13=CrashPen; 14=Morphcomp; 15=Tag-comp; 16=Tag-negbin; 17=F_ballpark
#like_comp fleet  phase  value  sizefreq_method
 9 1 1 1 1
 4 2 2 1 1
 4 2 3 1 1
 4 2 4 1 1
-9999  1  1  1  1  #  terminator
#
# lambdas (for info only; columns are phases)
#  0 0 0 0 #_CPUE/survey:_1
#  1 1 1 1 #_CPUE/survey:_2
#  1 1 1 1 #_CPUE/survey:_3
#  1 1 1 1 #_agecomp:_1
#  1 1 1 1 #_agecomp:_2
#  0 0 0 0 #_agecomp:_3
#  1 1 1 1 #_init_equ_catch
#  1 1 1 1 #_recruitments
#  1 1 1 1 #_parameter-priors
#  1 1 1 1 #_parameter-dev-vectors
#  1 1 1 1 #_crashPenLambda
#  0 0 0 0 # F_ballpark_lambda
1 # (0/1) read specs for more stddev reporting 
 0 2 -1 7 0 0 -1 2020 6 # selex type, len/age, year, N selex bins, Growth pattern, N growth ages, NatAge_area(-1 for all), NatAge_yr, N Natages
 1 2 3 4 5 6 # vector with NatAge std bin picks (-1 in first bin to self-generate)
999

