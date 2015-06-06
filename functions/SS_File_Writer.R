writeCtl.om <- function (ctl,y) 
{   
    selec.master <- matrix(c(    
    #_LO    HI   INIT   PRIOR  PR_type SD  PHASE   env  use_dev      dev_minyr   dev_maxyr   dev_stddev  Block     Block_Fxn
    #25,     60,  fsp1.vec[1],  fsp1.vec[1],   -1,     1,   4,    0,   0,           0,          0,         0.20,    1, 2,   "#Inflection",
    #-5,     5,   fsp2,  fsp2,   -1,     1,  -9,    0,   0,           0,          0,          0.5,        0,        0,           "#Slope",
    ##Survey                     
    #25,     60,  ssp1,  ssp1,   -1,     1,    4,   0,   0,           0,          0,          0.5,        0,        0,           "#Peak",                           
    #-5,     5,   ssp2,  ssp2,   -1,     1,   -9,   0,   0,           0,          0,          0.5,        0,        0,           "#Top (Width)"),          
    #ncol=15, byrow=T)
    #_LO    HI   INIT   PRIOR  PR_type SD  PHASE   env  use_dev      dev_minyr   dev_maxyr   dev_stddev  Block     Block_Fxn
    25,     60,  fsp1.vec[1],  fsp1.vec[1],   -1,     1,   4,    0,   0,           0,          0,         0.20,    1, 2,   "#Peak",
    -5,     5,   fsp2,  fsp2,   -1,     1,  -9,    0,   0,           0,          0,          0.5,        0,        0,           "#Top (Width)",                            
    -10,    10,  fsp3,  fsp3,   -1,     1,   5,    0,   0,           0,          0,          0.5,        0,        0,           "#Asc_Width" ,                         
    -2,     20,  fsp4,  fsp4,   -1,     1,  -9,    0,   0,           0,          0,          0.5,        0,        0,           "#Desc_Width",                         
    -15,    15,  fsp5,  fsp5,   -1,     1,  -9,    0,   0,           0,          0,          0.5,        0,        0,           "#Init",                           
    -999,  100,  -999,  -999,   -1,     1,  -9,    0,   0,           0,          0,          0.5,        0,        0,           "#Final",   
    ##Survey                     
    25,     60,  ssp1,  ssp1,   -1,     1,    4,   0,   0,           0,          0,          0.5,        0,        0,           "#Peak",                           
    -5,     5,   ssp2,  ssp2,   -1,     1,   -9,   0,   0,           0,          0,          0.5,        0,        0,           "#Top (Width)",                            
    -10,    10,  ssp3,  ssp3,   -1,     1,    5,   0,   0,           0,          0,          0.5,        0,        0,           "#Asc_Width",                          
    -2,     20,  ssp4,  ssp4,   -1,     1,   -9,   0,   0,           0,          0,          0.5,        0,        0,           "#Desc_Width",                         
    -15,    15,  ssp5,  ssp5,   -1,     1,   -9,   0,   0,           0,          0,          0.5,        0,        0,           "#Init",                           
    -999,  100,  -999,  -999,   -1,     1,   -9,   0,   0,           0,          0,          0.5,        0,        0,           "#Final"),    
    ncol=15, byrow=T) 

    ageselec.mat<- matrix(c(
    #_LO    HI        INIT    PRIOR         R_type   SD       PHASE       env-var use_dev dev_minyr   dev_maxyr   dev_stddev  Block   Block_Fxn    
    0,      0,         0,     0,            0,       50,      -3,        c(rep(0,4), 0.5, 0, 0), "#MIN AGE", 
    0,      ages+10,   ages,  ages,         0,       50,      -3,        c(rep(0,4), 0.5, 0, 0), "#MAX AGE"),
    byrow=T, ncol=15)

    block.selec <- matrix(0, y-1, 8)
    for (i in 2:y){
        block.selec[i-1,] <- c(25, 60,  fsp1.vec[i], fsp1.vec[i] ,  -1,     1,  3,  paste("#Selec", i))
    }
    #block.selec <- matrix(c(
    ##_LO    HI   INIT   PRIOR  PR_type SD  PHASE
    #25,     60,  fsp1.start + selec.adj,  fsp1.start + selec.adj,  -1,     1,  3,  "#Peak Block"),
    #ncol = 8, byrow = F)  

    bio.mat <-matrix(c( 
    #_LO    HI          INIT          PRIOR              PR_type    SD    PHASE      env-var use_dev dev_minyr   dev_maxyr   dev_stddev  Block   Block_Fxn
    0.01,   0.6,        m.vec[1], round(log(m.vec[1]),4),  3,        .20,    -1,        c(rep(0,4), 0.5, 1,2), "#NatM_p_1_Fem_GP_1",
    3,      35,         L1,           L1,                 -1,         10,     3,        c(rep(0,4), 0.5, 0,0), "#L_at_Amin_Fem_GP_1_",
    45,     70,         L2f,          L2f,                -1,         10,     3,        c(rep(0,4), 0.5, 0,0), "#L_at_Amax_Fem_GP_1_",
    0.00,   0.2,        kf,           kf,                 -1,        .25,     -3,       c(rep(0,4), 0.5, 0,0), "#VonBert_K_Fem_GP_1_",
    0.01,   0.2,        CV1,          CV1,                -1,         99,    -99,       c(rep(0,4), 0.5, 0,0), "#CV_young_Fem_GP_1_",
    0.01,   0.1,        CV2,          CV2,                -1,         99,    -99,       c(rep(0,4), 0.5, 0,0), "#CV_old_Fem_GP_1_",
    #0.01,   m.m.est*3,  m.m.est,      round(log(m.m.est),4), 3,      .25,   do.est,     c(rep(0,4), 0.5, 0,0), "#NatM_p_1_Mal_GP_1",
    #3,      35,         L1,           L1,                 -1,         99,    -99,       c(rep(0,4), 0.5, 0,0), "#L_at_Amin_Mal_GP_1_",
    #35,     70,         L2m,          L2m,                -1,         99,    -99,       c(rep(0,4), 0.5, 0,0), "#L_at_Amax_Mal_GP_1_",
    #0.01,   0.2,        km,           km,                 -1,         99,    -99,       c(rep(0,4), 0.5, 0,0), "#VonBert_K_Mal_GP_1_",
    #0.01,   0.2,        CV1,          CV1,                -1,         99,    -99,       c(rep(0,4), 0.5, 0,0), "#CV_young_Mal_GP_1_",
    #0.01,   0.1,        CV2,          CV2,                -1,         99,    -99,       c(rep(0,4), 0.5, 0,0), "#CV_old_Mal_GP_1_",
    0,      0.1,        0,            0,                  -1,        .25,     -2,       c(rep(0,4), 0.5, 0,0), "#NatM_p_1_Mal_GP_1",
    0,      0.1,        0,            0,                  -1,         99,    -99,       c(rep(0,4), 0.5, 0,0), "#L_at_Amin_Mal_GP_1_",
    0,      0.1,        0,            0,                  -1,         99,    -99,       c(rep(0,4), 0.5, 0,0), "#L_at_Amax_Mal_GP_1_",
    0,      0.2,        0,            0,                  -1,         99,    -99,       c(rep(0,4), 0.5, 0,0), "#VonBert_K_Mal_GP_1_",
    0,      0.2,        0,            0,                  -1,         99,    -99,       c(rep(0,4), 0.5, 0,0), "#CV_young_Mal_GP_1_",
    0,      0.1,        0,            0,                  -1,         99,    -99,       c(rep(0,4), 0.5, 0,0), "#CV_old_Mal_GP_1_",
    0,      0.1,        wght.coef.f,  wght.coef.f,        -1,         99,    -99,       c(rep(0,4), 0.5, 0,0), "# Wtlen1_Fem",
    0,      4,          wght.exp.f,   wght.exp.f,         -1,         99,    -99,       c(rep(0,4), 0.5, 0,0), "# Wtlen2_Fem",
    20,     40,         ohm4,         ohm4,               -1,         99,    -99,       c(rep(0,4), 0.5, 0,0), "# Mat50_Fem",
    -3,     3,          ohm3,         ohm3,               -1,         99,    -99,       c(rep(0,4), 0.5, 0, 0), "#Mat_slope_Fem",
    0,      3,          ohm5,         ohm5,               -1,         99,    -99,       c(rep(0,4), 0.5, 0, 0), "# Eggs1_Fem",
    0,      3,          ohm6,         ohm6,               -1,         99,    -99,       c(rep(0,4), 0.5, 0, 0), "# Eggs2_Fem",
    0,      0.1,        wght.coef.m,  wght.coef.m,        -1,         99,    -99,       c(rep(0,4), 0.5, 0, 0), "# Wtlen1_Mal",
    0,      4,          wght.exp.m,   wght.exp.m,         -1,         99,    -99,       c(rep(0,4), 0.5, 0,0),  "# Wtlen2_Mal",
    0,      1,          1,            1,                  -1,         99,    -99,       c(rep(0,4), 0.5, 0, 0), "# RecrDist_GP_1_",
    0,      1,          1,            1,                  -1,         99,    -99,       c(rep(0,4), 0.5, 0, 0), "# RecrDist_Area_1_",
    0,      1,          1,            1,                  -1,         99,    -99,       c(rep(0,4), 0.5, 0, 0), "# RecrDist_Seas_1_",
    0,      1,          0,            0,                  -1,         99,    -99,       c(rep(0,4), 0.5, 0, 0), "# CohortGrowDev"), 
    ncol=15, byrow=T)

    m.matrix <- matrix(0, y-1, 8)
    for (i in 2:y){
        m.matrix[i-1,] <- c(0.01, 0.60,  m.vec[i], round(log(m.vec[i]),4) ,  3,  0.20,  3,  paste("#M", i))
    }

    sigma.set = ifelse(sigmaR == 0, 0.01, sigmaR)
    rec.mat <- matrix(c(
    #_LO    HI     INIT     PRIOR   PR_type     SD      PHASE
    2,      15,    log(R0), log(R0), -1,        10,      est.R0, "# log(R0)",  
    0.20,   1,     steep,   steep,   1,       0.09,     -2, "# SR_steep ",
    0,      1.5,   sigma.set,  sigma.set,   -1,         99,     -99,"#SR_sigmaR",
    -5,     5,     0,       0,      -1,         99,     -99,"# SR_envlink",
    -5,     5,     0,       0,      -1,         99,     -99,"# SR_R1_offset",
    0,   0.99,     0,       0,      -1,         99,     -99,"# SR_autocorr"), 
    ncol=8, byrow=T)
                
    
    cat("#Stock Synthesis\n",
    "#\n",
    1,          " #Number of Growth Morphs\n",
    1,          " #Number of Sub-Morphs\n",
    file =ctl, sep = " ")

    #Define the blocks
    cat(
    1,                " #Number of Block Patterns\n",
    y-1,                " #Blocks per Pattern\n",
    sort(rep(2:y, 2))," #Block Years for Time Varying Parameters\n",
    file = ctl, append = T, sep= " ")
    
    cat(
    0.5,        " #Fraction Female\n",
    0,          " #Natural Mortality Method\n",
    1,          " #Growth Method\n",
    a3,         " #Growth Amin\n",
    a4,         " #Growth Amax\n",
    0,          " #SD add to LAA\n",
    0,          " #CV pattern (CV=f(LAA))\n",
    1,          " #Maturity Method (1=length logistic)\n",
    mat.age,    " #First Mature Age\n",
    1,          " #Fecundity Option (fecundity=wt*(a+b*wt) with a=1 and b=0)\n",
    0,          " #Hermaphroditism option\n",
    2,          " #Offset Method (Direct Assignment, 2 = Males are an offset of females)\n",
    1,          " #Time-varying adjustment\n",
    file=ctl,append = T, sep=" ")
    
    cat("#Maturity & Growth Parameters\n",
    "#LO   HI   INIT   PRIOR   PRIOR_TYPE   SD   PHASE   ENV\n",
    file=ctl, append=T, sep="")
        
    write.table(bio.mat,file=ctl, append=T,row.names=FALSE, col.names=FALSE, quote=FALSE)
    cat(
    y-1, " #MG custom parameters\n",
    file = ctl, append = T, sep = "")
    write.table(m.matrix, file = ctl, append = T, row.names = FALSE, col.names = FALSE, quote = FALSE)
   
    cat("#Seasonal Effects\n",
    rep(0,10), " #\n",
    "#Recruitment\n",
    3, " #Recruitment Method, 3= standard BH\n",
    "#LO   HI   INIT   PRIOR   PRIOR_TYPE   SD   PHASE\n",
    file=ctl, append=T)

    write.table(rec.mat,file=ctl, append=T,row.names=FALSE, col.names=FALSE, quote=FALSE)
    
    if (determ == TRUE){
        cat(0, " #SR env link\n", 
        0,     " #SR env target\n",
        0,     " #Do rec dev (0=none, 1=devvector, 2= simple dev)\n",
        1,     " #main recr dev begin yr\n",
        #y-pre.fishery.yrs,           " #main recr devs end yr\n",
        y,     " #main recr devs end yr\n",
        -2,    " #main recr dev phas\n",
        0,     " #advanced options (0=default values)ALL SET AT DEFAULT VALUES\n",   
        file=ctl,append=T)  
    }

    if (determ == FALSE){
        cat(0, " #SR env link\n", 
        0,                      " #SR env target\n",
        1,                      " #Do rec dev (0=none, 1=devvector, 2= simple dev)\n",
        main.rec.start,         " #main recr dev begin yr\n",
        main.rec.end,           " #main recr devs end yr\n",
        3,                      " #main recr dev phas\n",
        1,                      " #advanced options (0=default values)ALL SET AT DEFAULT VALUES\n",                                                 
        start.devs,         " #_recdev_early_start    (0=none;    neg value   makes   relative    to  recdev_start)\n",    
        pre.dev.phase,      " #_recdev_early_phase\n",                                                                                
        0,                  " #_forecast_recruitment  phase   (incl.  late    recr)   (0  value   resets  to  maxphase+1)\n",                                         
        1000,               " #_lambda    for prior_fore_recr occurring   before  endyr+1\n",                                                         
        start.bias,         " #_last_early_yr_nobias_adj_in_MPD\n",                                                                               
        full.bias,          " #_first_yr_fullbias_adj_in_MPD\n",                                                                              
        last.bias,          " #_last_yr_fullbias_adj_in_MPD\n",                                                                               
        last.no.bias,       " #_first_recent_yr_nobias_adj_in_MPD\n",                                                                             
        max.bias.adj,       " #_max_bias_adj_in_MPD\n",                                                                               
        0,                  " #period of cyle in recruitment\n",                                                             
        -15,                 " #min    rec_dev \n",                                                                        
        15,                  " #max    rec_dev \n",  
        n.devs,             " #_read_recdevs\n",
        " #end of advanced options\n",
        file=ctl,append=T)   
    }
    
    if (!determ){
        write.table(write.devs,file=ctl,append=T,row.names=F, col.names=F, quote=F) 
    }                                                            
    
    cat(
    " #Fishing Mortaltity\n",
    0.04,       " #\n",
    -99,        " #\n",
    1,          " #F Method (1=discrete pope's)\n",
    0.9,        " #Max F\n",
    #4,          " #N iterations for tuning hybrid F\n",
    file=ctl, append=T)
    
    cat(
    0, 4, 0, 0, 0, 99, -1, " #Initial F setup\n", 
    file=ctl,append=T) 
    
    cat(" #Catchability Spec\n",
    "# Q_type options:  <0=mirror, 0/1=float, 2=parameter, 3=parm_w_random_dev, 4=parm_w_randwalk)\n",
    "# A: Do_Power, B: Do_Env_Link, C: Do_extra_sd, D: Q type\n",
    0, 0, 0, 0,  " #Fishery\n",
    0, 0, 0, fix.q,  " #Survey\n",
    file = ctl, append = T)

    if (OM.run.1) { cat( 0, 0, 0, 2, " #Depl\n",
              -6, 6, 0, 0, -1, 99, -1,  " #Depletion\n",
              file = ctl, append = T) }

    if (fix.q > 0){ cat(-6, 6, 0, 0, -1, 99, -1,
                file = ctl, append = T) }

    cat(" #Size Selectivity Spec\n",
    24, 0, 0, 0, " #Dbl Normal Fishery\n",
    24, 0, 0, 0, " #Dbl Normal Survey\n",
    file = ctl, append = T)

    if (OM.run.1) { cat( 34, 0, 0, 0, " #Depl\n",
              file = ctl, append = T )}

    cat(" #Age Selectivity Spec\n",
    10, 0, 0, 0, " #Fishery\n",
    10, 0, 0, 0, " #Survey\n",
    file = ctl, append = T)

    if (OM.run.1) { cat(11, 0, 0, 0, " #Depl\n",
              file = ctl, append = T) }

    cat(" #Selectivity Parameters\n",
    "#LO HI INIT PRIOR PRIOR_TYPE SD PHASE\n",
    file=ctl, append=T)

    write.table(selec.master,file=ctl, append=T,row.names=FALSE, col.names=FALSE, quote=FALSE)
    if (OM.run.1) { write.table(ageselec.mat,file=ctl, append=T,row.names=FALSE, col.names=FALSE, quote=FALSE) }

    cat(
    1,      " #Custom Block Setup\n",
    file = ctl, append = T, sep = " ")

    write.table(block.selec, file = ctl, append=T, row.names = F, col.names = F, quote=F) 

    cat(
    1,      " #seleparm_adjust\n",
    file = ctl, append = T, sep = " ")
 
    cat(
    0,      " #No Tag Parameters\n",
    0,      " #Variance Adjustments\n",
    2,      " #Max_lambda_phase\n",
    0,      " #sd_offset\n",
    0,      " #Number Lambda Changes\n",
    "#Place holder for lambdas\n",
    #5, 1, 1, 0, 1, "#Fishery Ages\n",
    #4, 1, 1, 0, 1, "#Fishery Lengths\n",
    0,      " #Option for Variance Estimates\n",
    999, file=ctl, append=T)
}    


writeCtl <- function (ctl,y) 
{   
    selec.master <- matrix(c(    
    #_LO    HI   INIT   PRIOR  PR_type SD  PHASE   env  use_dev      dev_minyr   dev_maxyr   dev_stddev  Block     Block_Fxn
    #25,     60,  fsp1.vec[1],  fsp1.vec[1],   -1,     1,   4,    0,   0,           0,          0,         0.20,      block.num, block.fxn,   "#Inflection",
    #-5,     5,   fsp2,  fsp2,   -1,     1,  -9,    0,   0,           0,          0,          0.5,        0,        0,           "#Slope",
    ##Survey                     
    #25,     60,  ssp1,  ssp1,   -1,     1,    4,   0,   0,           0,          0,          0.5,        0,        0,           "#Peak",                           
    #-5,     5,   ssp2,  ssp2,   -1,     1,   -9,   0,   0,           0,          0,          0.5,        0,        0,           "#Top (Width)"),          
    #ncol=15, byrow=T)
    #_LO    HI   INIT   PRIOR  PR_type SD  PHASE   env  use_dev      dev_minyr   dev_maxyr   dev_stddev  Block     Block_Fxn
    25,     60,  fsp1.start,   fsp1.start,   -1,     1,   4,    0,   0,           0,          0,         0.20,      block.num, block.fxn,   "#Peak",
    -5,     5,   fsp2,  fsp2,   -1,     1,  -9,    0,   0,           0,          0,          0.5,        0,        0,           "#Top (Width)",                            
    -10,    10,  fsp3,  fsp3,   -1,     1,   5,    0,   0,           0,          0,          0.5,        0,        0,           "#Asc_Width" ,                         
    -2,     20,  fsp4,  fsp4,   -1,     1,  -9,    0,   0,           0,          0,          0.5,        0,        0,           "#Desc_Width",                         
    -15,    15,  fsp5,  fsp5,   -1,     1,  -9,    0,   0,           0,          0,          0.5,        0,        0,           "#Init",                           
    -999,  100,  -999,  -999,   -1,     1,  -9,    0,   0,           0,          0,          0.5,        0,        0,           "#Final",   
    ##Survey                     
    25,     60,  ssp1,  ssp1,   -1,     1,    4,   0,   0,           0,          0,          0.5,        0,        0,           "#Peak",                           
    -5,     5,   ssp2,  ssp2,   -1,     1,   -9,   0,   0,           0,          0,          0.5,        0,        0,           "#Top (Width)",                            
    -10,    10,  ssp3,  ssp3,   -1,     1,    5,   0,   0,           0,          0,          0.5,        0,        0,           "#Asc_Width",                          
    -2,     20,  ssp4,  ssp4,   -1,     1,   -9,   0,   0,           0,          0,          0.5,        0,        0,           "#Desc_Width",                         
    -15,    15,  ssp5,  ssp5,   -1,     1,   -9,   0,   0,           0,          0,          0.5,        0,        0,           "#Init",                           
    -999,  100,  -999,  -999,   -1,     1,   -9,   0,   0,           0,          0,          0.5,        0,        0,           "#Final"),    
    ncol=15, byrow=T) 

    ageselec.mat<- matrix(c(
    #_LO    HI        INIT    PRIOR         R_type   SD       PHASE       env-var use_dev dev_minyr   dev_maxyr   dev_stddev  Block   Block_Fxn    
    0,      0,         0,     0,            0,       50,      -3,        c(rep(0,4), 0.5, 0, 0), "#MIN AGE", 
    0,      ages+10,   ages,  ages,         0,       50,      -3,        c(rep(0,4), 0.5, 0, 0), "#MAX AGE"),
    byrow=T, ncol=15)

    block.selec <- matrix(c(
    #_LO    HI   INIT   PRIOR  PR_type SD  PHASE
    25,     60,  fsp1.start + selec.adj,  fsp1.start + selec.adj,  -1,     1,  3,  "#Peak Block"),
    ncol = 8, byrow = F)  

    bio.mat <-matrix(c( 
    #_LO    HI          INIT          PRIOR              PR_type    SD    PHASE      env-var use_dev dev_minyr   dev_maxyr   dev_stddev  Block   Block_Fxn
    0.01,   0.4,        m,            round(log(m),4),     3,        .20,     m.phase,        c(rep(0,4), 0.5, 0,0), "#NatM_p_1_Fem_GP_1",
    3,      35,         L1,           L1,                 -1,         10,     3,        c(rep(0,4), 0.5, 0,0), "#L_at_Amin_Fem_GP_1_",
    45,     70,         L2f,          L2f,                -1,         10,     3,        c(rep(0,4), 0.5, 0,0), "#L_at_Amax_Fem_GP_1_",
    0.00,   0.2,        kf,           kf,                 -1,        .25,     -3,       c(rep(0,4), 0.5, 0,0), "#VonBert_K_Fem_GP_1_",
    0.01,   0.2,        CV1,          CV1,                -1,         99,    -99,       c(rep(0,4), 0.5, 0,0), "#CV_young_Fem_GP_1_",
    0.01,   0.1,        CV2,          CV2,                -1,         99,    -99,       c(rep(0,4), 0.5, 0,0), "#CV_old_Fem_GP_1_",
    #0.01,   m.m.est*3,  m.m.est,      round(log(m.m.est),4), 3,      .25,   do.est,     c(rep(0,4), 0.5, 0,0), "#NatM_p_1_Mal_GP_1",
    #3,      35,         L1,           L1,                 -1,         99,    -99,       c(rep(0,4), 0.5, 0,0), "#L_at_Amin_Mal_GP_1_",
    #35,     70,         L2m,          L2m,                -1,         99,    -99,       c(rep(0,4), 0.5, 0,0), "#L_at_Amax_Mal_GP_1_",
    #0.01,   0.2,        km,           km,                 -1,         99,    -99,       c(rep(0,4), 0.5, 0,0), "#VonBert_K_Mal_GP_1_",
    #0.01,   0.2,        CV1,          CV1,                -1,         99,    -99,       c(rep(0,4), 0.5, 0,0), "#CV_young_Mal_GP_1_",
    #0.01,   0.1,        CV2,          CV2,                -1,         99,    -99,       c(rep(0,4), 0.5, 0,0), "#CV_old_Mal_GP_1_",
    0,      0.1,        0,            0,                  -1,        .25,     -2,       c(rep(0,4), 0.5, 0,0), "#NatM_p_1_Mal_GP_1",
    0,      0.1,        0,            0,                  -1,         99,    -99,       c(rep(0,4), 0.5, 0,0), "#L_at_Amin_Mal_GP_1_",
    0,      0.1,        0,            0,                  -1,         99,    -99,       c(rep(0,4), 0.5, 0,0), "#L_at_Amax_Mal_GP_1_",
    0,      0.2,        0,            0,                  -1,         99,    -99,       c(rep(0,4), 0.5, 0,0), "#VonBert_K_Mal_GP_1_",
    0,      0.2,        0,            0,                  -1,         99,    -99,       c(rep(0,4), 0.5, 0,0), "#CV_young_Mal_GP_1_",
    0,      0.1,        0,            0,                  -1,         99,    -99,       c(rep(0,4), 0.5, 0,0), "#CV_old_Mal_GP_1_",
    0,      0.1,        wght.coef.f,  wght.coef.f,        -1,         99,    -99,       c(rep(0,4), 0.5, 0,0), "# Wtlen1_Fem",
    0,      4,          wght.exp.f,   wght.exp.f,         -1,         99,    -99,       c(rep(0,4), 0.5, 0,0), "# Wtlen2_Fem",
    20,     40,         ohm4,         ohm4,               -1,         99,    -99,       c(rep(0,4), 0.5, 0,0), "# Mat50_Fem",
    -3,     3,          ohm3,         ohm3,               -1,         99,    -99,       c(rep(0,4), 0.5, 0, 0), "#Mat_slope_Fem",
    0,      3,          ohm5,         ohm5,               -1,         99,    -99,       c(rep(0,4), 0.5, 0, 0), "# Eggs1_Fem",
    0,      3,          ohm6,         ohm6,               -1,         99,    -99,       c(rep(0,4), 0.5, 0, 0), "# Eggs2_Fem",
    0,      0.1,        wght.coef.m,  wght.coef.m,        -1,         99,    -99,       c(rep(0,4), 0.5, 0, 0), "# Wtlen1_Mal",
    0,      4,          wght.exp.m,   wght.exp.m,         -1,         99,    -99,       c(rep(0,4), 0.5, 0,0),  "# Wtlen2_Mal",
    0,      1,          1,            1,                  -1,         99,    -99,       c(rep(0,4), 0.5, 0, 0), "# RecrDist_GP_1_",
    0,      1,          1,            1,                  -1,         99,    -99,       c(rep(0,4), 0.5, 0, 0), "# RecrDist_Area_1_",
    0,      1,          1,            1,                  -1,         99,    -99,       c(rep(0,4), 0.5, 0, 0), "# RecrDist_Seas_1_",
    0,      1,          0,            0,                  -1,         99,    -99,       c(rep(0,4), 0.5, 0, 0), "# CohortGrowDev"), 
    ncol=15, byrow=T)

    sigma.set = ifelse(sigmaR == 0, 0.01, sigmaR)
    rec.mat <- matrix(c(
    #_LO    HI     INIT     PRIOR   PR_type     SD      PHASE
    2,      15,    log(R0), log(R0), -1,        10,      est.R0, "# log(R0)",  
    0.20,   1,     steep,   steep,   1,       0.09,     -2, "# SR_steep ",
    0,      1.5,   sigma.set,  sigma.set,   -1,         99,     -99,"#SR_sigmaR",
    -5,     5,     0,       0,      -1,         99,     -99,"# SR_envlink",
    -5,     5,     0,       0,      -1,         99,     -99,"# SR_R1_offset",
    0,   0.99,     0,       0,      -1,         99,     -99,"# SR_autocorr"), 
    ncol=8, byrow=T)
                
    
    cat("#Stock Synthesis\n",
    "#\n",
    1,          " #Number of Growth Morphs\n",
    1,          " #Number of Sub-Morphs\n",
    file =ctl, sep = " ")

    if (need.blocks == T){
    cat(
    1,                " #Number of Block Patterns\n",
    block.pattern,    " #Blocks per Pattern\n",
    block.yrs,        " #Block Years for Selectivity\n",
    file = ctl, append = T, sep= " ")
    }

    if (need.blocks == F){
    cat(
    0,          " #Number of Block Patterns\n",
    file = ctl, append = T, sep = " ")
    }
    
    cat(
    0.5,        " #Fraction Female\n",
    0,          " #Natural Mortality Method\n",
    1,          " #Growth Method\n",
    a3,         " #Growth Amin\n",
    a4,         " #Growth Amax\n",
    0,          " #SD add to LAA\n",
    0,          " #CV pattern (CV=f(LAA))\n",
    1,          " #Maturity Method (1=length logistic)\n",
    mat.age,    " #First Mature Age\n",
    1,          " #Fecundity Option (fecundity=wt*(a+b*wt) with a=1 and b=0)\n",
    0,          " #Hermaphroditism option\n",
    2,          " #Offset Method (Direct Assignment, 2 = Males are an offset of females)\n",
    1,          " #Time-varying adjustment\n",
    file=ctl,append = T, sep=" ")
    
    cat("#Maturity & Growth Parameters\n",
    "#LO   HI   INIT   PRIOR   PRIOR_TYPE   SD   PHASE   ENV\n",
    file=ctl, append=T, sep="")
        
    write.table(bio.mat,file=ctl, append=T,row.names=FALSE, col.names=FALSE, quote=FALSE)
   
    cat("#Seasonal Effects\n",
    rep(0,10), " #\n",
    "#Recruitment\n",
    3, " #Recruitment Method, 3= standard BH\n",
    "#LO   HI   INIT   PRIOR   PRIOR_TYPE   SD   PHASE\n",
    file=ctl, append=T)

    write.table(rec.mat,file=ctl, append=T,row.names=FALSE, col.names=FALSE, quote=FALSE)
    
    if (determ == TRUE){
        cat(0, " #SR env link\n", 
        0,     " #SR env target\n",
        0,     " #Do rec dev (0=none, 1=devvector, 2= simple dev)\n",
        1,     " #main recr dev begin yr\n",
        #y-pre.fishery.yrs,           " #main recr devs end yr\n",
        y,     " #main recr devs end yr\n",
        -2,    " #main recr dev phas\n",
        0,     " #advanced options (0=default values)ALL SET AT DEFAULT VALUES\n",   
        file=ctl,append=T)  
    }

    if (determ == FALSE){
        cat(0, " #SR env link\n", 
        0,                      " #SR env target\n",
        1,                      " #Do rec dev (0=none, 1=devvector, 2= simple dev)\n",
        main.rec.start,         " #main recr dev begin yr\n",
        main.rec.end,           " #main recr devs end yr\n",
        3,                      " #main recr dev phas\n",
        1,                      " #advanced options (0=default values)ALL SET AT DEFAULT VALUES\n",                                                 
        start.devs,         " #_recdev_early_start    (0=none;    neg value   makes   relative    to  recdev_start)\n",    
        pre.dev.phase,      " #_recdev_early_phase\n",                                                                                
        0,                  " #_forecast_recruitment  phase   (incl.  late    recr)   (0  value   resets  to  maxphase+1)\n",                                         
        1000,               " #_lambda    for prior_fore_recr occurring   before  endyr+1\n",                                                         
        start.bias,         " #_last_early_yr_nobias_adj_in_MPD\n",                                                                               
        full.bias,          " #_first_yr_fullbias_adj_in_MPD\n",                                                                              
        last.bias,          " #_last_yr_fullbias_adj_in_MPD\n",                                                                               
        last.no.bias,       " #_first_recent_yr_nobias_adj_in_MPD\n",                                                                             
        max.bias.adj,       " #_max_bias_adj_in_MPD\n",                                                                               
        0,                  " #period of cyle in recruitment\n",                                                             
        -15,                 " #min    rec_dev \n",                                                                        
        15,                  " #max    rec_dev \n",  
        0,             " #_read_recdevs\n",
        " #end of advanced options\n",
        file=ctl,append=T)   
    }
    
    #if (!determ){
    #    if(OM || get.forecast) { write.table(write.devs,file=ctl,append=T,row.names=F, col.names=F, quote=F) }
    #}                                                            
    
    cat(
    " #Fishing Mortaltity\n",
    0.04,       " #\n",
    -99,        " #\n",
    1,          " #F Method (1=discrete pope's)\n",
    0.9,        " #Max F\n",
    #4,          " #N iterations for tuning hybrid F\n",
    file=ctl, append=T)
    
    cat(
    0, 4, 0, 0, 0, 99, -1, " #Initial F setup\n", 
    file=ctl,append=T) 
    
    cat(" #Catchability Spec\n",
    "# Q_type options:  <0=mirror, 0/1=float, 2=parameter, 3=parm_w_random_dev, 4=parm_w_randwalk)\n",
    "# A: Do_Power, B: Do_Env_Link, C: Do_extra_sd, D: Q type\n",
    0, 0, 0, 0,  " #Fishery\n",
    0, 0, 0, fix.q,  " #Survey\n",
    file = ctl, append = T)

    if (fix.q > 0){ cat(-6, 6, 0, 0, -1, 99, -1,
                file = ctl, append = T) }

    cat(" #Size Selectivity Spec\n",
    24, 0, 0, 0, " #Dbl Normal Fishery\n",
    24, 0, 0, 0, " #Dbl Normal Survey\n",
    file = ctl, append = T)

    cat(" #Age Selectivity Spec\n",
    10, 0, 0, 0, " #Fishery\n",
    10, 0, 0, 0, " #Survey\n",
    file = ctl, append = T)

    cat(" #Selectivity Parameters\n",
    "#LO HI INIT PRIOR PRIOR_TYPE SD PHASE\n",
    file=ctl, append=T)

    write.table(selec.master,file=ctl, append=T,row.names=FALSE, col.names=FALSE, quote=FALSE)
    
    if(need.blocks == T){
        cat(
        1,      " #Custom Block Setup\n",
        file = ctl, append = T, sep = " ")
        for (z in 1:block.pattern){
           write.table(block.selec, file = ctl, append=T, row.names = F, col.names = F, quote=F) }
    }

    if (need.blocks == T){
        #if(data.scenario == "ds2" || data.scenario == "ds3" ){
            cat(
            1,      " #seleparm_adjust\n",
            file = ctl, append = T, sep = " ")
        #} 
    }

    #if(data.scenario == "ds0" || data.scenario == "ds1" ){
    #   cat(
    #    4,      " #selectivity dev phase\n",
    #    1,      " #seleparm_adjust\n",
    #    file = ctl, append = T, sep = " ")
    #}
 
    cat(
    0,      " #No Tag Parameters\n",
    0,      " #Variance Adjustments\n",
    2,      " #Max_lambda_phase\n",
    0,      " #sd_offset\n",
    0,      " #Number Lambda Changes\n",
    "#Place holder for lambdas\n",
    #5, 1, 1, 0, 1, "#Fishery Ages\n",
    #4, 1, 1, 0, 1, "#Fishery Lengths\n",
    0,      " #Option for Variance Estimates\n",
    999, file=ctl, append=T)
}    
        
#------Starter File----------------------------------------------------------------------------------------------------------------------------
writeStarter <- function (starter)
{
    cat("#\n",
    "#Stock Synthesis Version 3.0.11\n",
    "#\n",
    dat.file, "\n",
    ctl.file, "\n",
    0,      " #0=use init values in ctl file, 1= use ss3.par\n",
    0,      " #run display detail (0,1,2)\n",
    0,      " #detailed age-structured reports in REPORT.SSO (0,1)\n",
    0,      " #write detailed checkup.sso file (0,1)\n",
    0,      " #write parm values to ParmTrace.SSO\n",
    0,      " #report level in CUMREPORT.SSO (0,1,2)\n",
    0,      " #include prior_like for non-estimated parameteres (0,1)\n",
    1,      " #use soft boundaries to aid convergence\n",
    boot.files,      " #number of boostrap datafiles to produce\n",
    end.phase, " #turn off estimation for parameters entering after this phase\n",
    1,      " #MCMC burn interval\n",
    1,      " #MCMC thin interval\n",
    0,      " #jitter initial parm value by this fraction\n",
    -1,     " #min yr for sdreport outputs (-1 for styr)\n",
    -2,     " #max yr for sdreport outputs (-1 for endyr; -2 for endyr+Nforecastyrs\n",
    0,      " #N individual STD years\n",
    0.0001, " #final convergence criteria\n",
    0,      " #retrospective year relative to end year\n",
    1,      " #min age for calc of summary biomass\n",
    1,      " #Depletion basis:  denom is: 0=skip; 1=rel X*B0; 2=rel X*Bmsy; 3=rel X*B_styr\n",
    1,      " #Fraction (X) for Depletion denominator\n",
    1,      " #(1-SPR)_reporting:  0=skip; 1=rel(1-SPR); 2=rel(1-SPR_MSY); 3=rel(1-SPR_Btarget); 4=notrel\n",    
    1,      " #F_std reporting: 0=skip; 1=exploit(Bio); 2=exploit(Num); 3=sum(frates)0 # F_report_basis: 0=raw; 1=rel Fspr; 2=rel Fmsy ; 3=rel Fbtgt\n",
    0,      " #F_report_basis: 0=raw; 1=rel Fspr; 2=rel Fmsy ; 3=rel Fbtgt\n",
    999, append=FALSE, file=starter, sep="")
}

#-----------------Forcast File--------------------------------------------------------------------------------------

writeForecast <- function (forecast,y)
{
    cat(
    "#\n",
    "#Stock Synthesis\n",
    "#\n",
    1,          " # Benchmarks: 0=skip; 1=calc F_spr,F_btgt,F_msy\n", 
    2,          " # MSY: 1= set to F(SPR); 2=calc F(MSY); 3=set to F(Btgt); 4=set to F(endyr)\n",
    spr.target, " #  SPR target (e.g. 0.40)\n",
    bio.target, " # Biomass target (e.g. 0.40)\n",
    0, 0, 0, 0, 0, 0, " # Benchmark years: beg_bio, end_bio, beg_selex, end_selex, bef_relF, end_relF\n",
    1,          " # Bmark_relF_Basis: 1= use year range; 2= set relF same as forecast below\n",
    do.forecast," #Forecast: 0=none; 1=F(SPR); 2=F(MSY) 3=F(Btgt); 4=F(endyr); 5=Ave F (enter yrs); 6=read Fmult\n",
    4,          " # N forecast years\n",
    1,          " #F scalar (only used for Do_Forecast == 5)\n",
    0, 0, 0, 0, " #Fcast_years:  beg_selex, end_selex, beg_relF, end_relF\n",
    1,          " # Control rule method (1=west coast adjust catch; 2=adjust F)\n",
    #ctl.rule.tgt," # Control rule Biomass level for constant F (as frac of Bzero, e.g. 0.40)\n",
    #ctl.rule.tgt-0.001," # Control rule Biomass level for no F (as frac of Bzero, e.g. 0.10)\n",
    #buffer,     " # Control rule fraction of Flimit (e.g. 0.75)\n",
    0.01,       " # Control rule Biomass level for constant F (as frac of Bzero, e.g. 0.40)\n",
    0,          " # Control rule Biomass level for no F (as frac of Bzero, e.g. 0.10)\n",
    1,          " # Control rule fraction of Flimit (e.g. 0.75)\n",
    3,          " # N forecast loops (1=OFL only; 2=ABC; 3=get F from forecast ABC catch with allocations applied)\n",
    3,          " #_First forecast loop with stochastic recruitment\n",
    0,          " #_Forecast loop control #3 (reserved for future bells&whistles)\n", 
    0,          " #_Forecast loop control #4 (reserved for future bells&whistles)\n", 
    0,          " #_Forecast loop control #5 (reserved for future bells&whistles)\n", 
    y-2,        " #FirstYear for caps and allocations (should be after years with fixed inputs)\n", 
    0,          " # stddev of log(realized catch/target catch) in forecast (set value>0.0 to cause active impl_error)\n",
    0,          " # Do West Coast gfish rebuilder output (0/1)\n", 
    -1,         " # Rebuilder:  first year catch could have been set to zero (Ydecl)(-1 to set to 1999)\n",
    -1,         " # Rebuilder:  year for current age structure (Yinit) (-1 to set to endyear+1)\n",
    1,          " # fleet relative F:  1=use first-last alloc year; 2=read seas(row) x fleet(col) below\n",
    "# Note that fleet allocation is used directly as average F if Do_Forecast=4\n", 
    2,          " # basis for fcast catch tuning and for fcast catch caps and allocation  (2=deadbio; 3=retainbio; 5=deadnum; 6=retainnum)\n",
    " # Conditional input if relative F choice = 2\n",
    " # Fleet relative F:  rows are seasons, columns are fleets\n",
    " # max totalcatch by fleet (-1 to have no max) must enter value for each fleet\n",
    -1,         "\n",
    " # max totalcatch by area (-1 to have no max); must enter value for each fleet\n", 
    -1,         "\n",
    " # fleet assignment to allocation group (enter group ID# for each fleet, 0 for not included in an alloc group)\n",
    0,          "\n", 
    append = FALSE, file=forecast, sep=" ") 
      
    cat(
    4,      " # Number of forecast catch levels to input (else calc catch from forecast F)\n", 
    99,     " # basis for input Fcast catch:  2=dead catch; 3=retained catch; 99=input Hrate(F) (units are from fleetunits; note new codes in SSV3.20)\n",
    y + 1, 1, 1, fspr.input, "\n", 
    y + 2, 1, 1, fspr.input, "\n", 
    y + 3, 1, 1, fspr.input, "\n", 
    y + 4, 1, 1, fspr.input, "\n", 
    999, append=TRUE, file=forecast, sep=" ")
    

}

#--------.dat file ---------------------------------------------------------------------------------------------------------------------------------
writeDat<-function(dat, y, survey, fore.catch)
{

    landings <- cbind(
        #Value of Catch                          #Year                              #Fleet          
        #fore.catch[(pre.fishery.yrs+1):y],   seq(1,(y-pre.fishery.yrs),1),    rep(1,(y-pre.fishery.yrs)) )
        fore.catch[(pre.fishery.yrs+1):y],   seq(ages,y,1),    rep(1,(y-pre.fishery.yrs)) )

    ss.survey.data = cbind(
        #start.survey:(y-pre.fishery.yrs), rep(1, length(survey)), rep(2, length(survey)), 
        start.survey:y, rep(1, length(survey)), rep(2, length(survey)), 
        survey, rep(ss.survey.cv, length(survey)))
        
    depl.survey    = cbind(y, 1, 3, final.depl, 0.01)
                    #cbind(y-pre.fishery.yrs, 1, 3, final.depl, 0.01)

    #data.yrs = (start.fishery.len.samp - pre.fishery.yrs):(y-pre.fishery.yrs)
    data.yrs = start.fishery.len.samp:y
    data.matrix = matrix(0, length(data.yrs), 2*length(len.step))
    data.matrix[,1] = f.len.samp[data.yrs]
    fishery.length.data = cbind(data.yrs, rep(1, length(data.yrs)), rep(1, length(data.yrs)), rep(3, length(data.yrs)), rep(2, length(data.yrs)),
                    f.len.samp[data.yrs], data.matrix)  

    #data.yrs = (start.survey.len.samp - pre.fishery.yrs):(y-pre.fishery.yrs)
    data.yrs = start.survey.len.samp:y
    data.matrix = matrix(0, length(data.yrs), 2*length(len.step))
    data.matrix[,1] = s.len.samp[data.yrs]
    survey.length.data = cbind(data.yrs, rep(1, length(data.yrs)), rep(2, length(data.yrs)), rep(3, length(data.yrs)), rep(2, length(data.yrs)),
                    s.len.samp[data.yrs], data.matrix)    

    #data.yrs = (start.fishery.age.samp- pre.fishery.yrs) :(y-pre.fishery.yrs)
    data.yrs =  start.fishery.age.samp :y
    data.matrix = matrix(0, length(data.yrs), 2*ages - 2)
    data.matrix[,1] = f.age.samp[data.yrs]
    fishery.age.data = cbind(data.yrs, rep(1, length(data.yrs)), rep(1, length(data.yrs)), rep(3, length(data.yrs)), rep(2, length(data.yrs)),
                    rep(1, length(data.yrs)), rep(-1, length(data.yrs)), rep(-1, length(data.yrs)),
                    f.age.samp[data.yrs], data.matrix)  

    #data.yrs = (start.survey.age.samp- pre.fishery.yrs) :(y-pre.fishery.yrs)
    data.yrs =  start.survey.age.samp : y
    data.matrix = matrix(0, length(data.yrs), 2*ages - 2)
    data.matrix[,1] = s.age.samp[data.yrs]
    survey.age.data = cbind(data.yrs, rep(1, length(data.yrs)), rep(2, length(data.yrs)), rep(3, length(data.yrs)), rep(2, length(data.yrs)),
                    rep(1, length(data.yrs)), rep(-1, length(data.yrs)), rep(-1, length(data.yrs)),
                    s.age.samp[data.yrs], data.matrix)    

    n.length.obs = dim(fishery.length.data)[1] + dim(survey.length.data)[1]
    n.age.obs = dim(fishery.age.data)[1] + dim(survey.age.data)[1]

    cat(
    "#\n",
    "#Stock Synthesis\n",
    "#\n",
    1,                  " #Start Year\n",
    #y-pre.fishery.yrs,  " #End Year\n",
    y,                  " #End Year\n",
    1,                  " #Number Seasons per Year\n",
    12,                 " #Months per Season/season\n",
    1,                  " #Spawning Season\n",
    1,                  " #Nfleet\n",
    2,                  " #Nsurveys\n",
    1,                  " #Nareas\n",
    fleets,             " \n",
    0.5,                " #Fleet Timing\n",
    0.50,               " #Survey Timing\n",
    0.50,               " #Depl Timing\n",
    rep(1,n.fleet),     " #_area_assignments_for_each_fishery_and_survey\n",
    1,                  " #Catch Units (Biomass=1)\n",
    0.0001,             " #SE of log(catch)\n", 
    2,                  " #Ngenders\n",
    max.age,            " #Nages\n",
    0 ,                 " #Initial Equilibrium Catch\n",
    dim(landings)[1],   " #Number of Catch Observations\n",
    "#catch year season\n", file=dat,sep=" ") 
    
    write.table(landings, file=dat, append=T, row.names=FALSE, col.names=FALSE)
    
    cat("#\n",
    (dim(ss.survey.data)[1] + dim(depl.survey)[1]), " #Number of Survey Observations\n",
    "#_Units:  0=numbers; 1=biomass; 2=F\n",                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
    "#_Errtype:  -1=normal; 0=lognormal; >0=T\n",                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
    "#_Fleet Units   Errtype\n",
    1, 1, 0, "#Fishery\n",
    2, 1, 0, "#Survey\n",
    file = dat, append = T, sep = " ")

    #if(OM.run.1) {
        cat(3, 1, 0, "#Depl\n",
        file = dat, append = T, sep = " ") #}
    
    write.table(ss.survey.data, file = dat, append = T,row.names = FALSE, col.names = FALSE)
    #if (OM.run.1) { 
        write.table(depl.survey, file = dat, append = T, row.names = FALSE, col.names = FALSE) #}
    
    cat("#\n",
    0,                  " #N_fleets with discard\n", 
    0,                  " #Number of Discard Observations\n",
    0,                  " #Number of Mean Body Weight Observations\n",
    30,                 " #DF_meanwt\n",
    "#Population Length Structure\n",
    3,                  " #length bin method: 1=use databins; 2=generate from binwidth,min,max below; 3=read vector\n",
    length(len.step),   " #Number of Population Length Bins\n",
    file=dat,append=T,sep=" ")
    
    cat(len.step,"\n",
    file=dat,append=T,sep=" ")
    
    cat(-1,             " #Compress tails\n",
    add.const,          " #Constant added to all proportions\n", 
    0,                  " #Combine males into females at or below this bin number\n",
    length(len.step),   " #_N_LengthBins\n",
    file=dat,append=T,sep="")
    
    cat(len.step,"\n",
    file=dat,append=T,sep=" ")
   
    if (data.scenario != "ds5") {
        cat(n.length.obs,  " #Number of Length Observations\n", 
        "#Year Seas Fleet Gender Partition Nsamp\n",
        file=dat,append=T,sep=" ") 
        
        write.table(fishery.length.data,file=dat, append=T,row.names=FALSE, col.names=FALSE, quote=FALSE)  
        write.table(survey.length.data,file=dat, append=T,row.names=FALSE, col.names=FALSE, quote=FALSE)  
    
        cat("#\n",
        #num.ages,           " #Number of Ages\n",
        #seq(1,num.ages,1),  " #Age Bins\n",
        max.age,             " #Number of Ages\n",
        seq(1,max.age,1),    " #Age Bins\n",
        1,                   " #Number of Ageing Error Sets\n",
        "#Age Error Matrix\n",
        rep(-1, ages),      " #True Ages\n",
        #rep(-1,max.age + 1),  " #True Ages\n",
        rep(0.05,ages),     " #Age Error StDev\n",
        #(1:ages-0.5)*0.05,    " #Age Error StDev\n",
        #(1:(max.age+1)-0.5)*0.05, " #Age Error StDev\n",
        "#\n",
        n.age.obs,          " #Number Age Observations\n",
        3,                  " #Age-Length Bin Option: 1=poplenbins; 2=datalenbins; 3=lengths\n",
        0,                  " #Combine males into females at or below this bin number\n",
        "#Year Seas Fleet Gender Partition Ageerr Lbinlo Lbinhi Nsamp\n",  
        file=dat,append=T,sep=" ")
    
        write.table(fishery.age.data, file=dat, append=T, row.names=FALSE, col.names=FALSE, quote=FALSE) 
        write.table(survey.age.data,  file=dat, append=T, row.names=FALSE, col.names=FALSE, quote=FALSE) 
    }

    if (data.scenario == "ds5") {
        cat(0,  " #Number of Length Observations\n", 
        "#Year Seas Fleet Gender Partition Nsamp\n",
        file=dat,append=T,sep=" ") 
        cat("#\n",
        num.ages,           " #Number of Ages\n",
        seq(1,num.ages,1),  " #Age Bins\n",
        1,                  " #Number of Ageing Error Sets\n",
        "#Age Error Matrix\n",
        rep(-1, ages),      " #True Ages\n",
        #rep(0.01,ages),     " #Age Error StDev\n",
        (1:ages-0.5)*0.05,    " #Age Error StDev\n",
        "#\n",
        0,                  " #Number Age Observations\n",
        3,                  " #Age-Length Bin Option: 1=poplenbins; 2=datalenbins; 3=lengths\n",
        0,                  " #Combine males into females at or below this bin number\n",
        "#Year Seas Fleet Gender Partition Ageerr Lbinlo Lbinhi Nsamp\n",  
        file=dat,append=T,sep=" ")
    }

    cat("#\n", 
    0, " #Number of Mean Size at Age Observations\n",
    0, " #Number of Environmental Variables \n",
    0, " #Number of Environmental Observations\n",
    0, " #Number of Weight Frequency Observations\n",
    0, " #Number of Taggindg Data Observations\n", 
    0, " #Number of Morph Composition Observation\n", 
    999,"\n",
    file=dat, append=T,sep="")
 
}
