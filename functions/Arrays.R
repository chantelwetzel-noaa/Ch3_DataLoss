#Arrays----------------------------------------------------------------------------------------------------
  age.bins <- seq(1,num.ages,1)
  numbers <- array(0,dim=c((total.yrs+1), ages, sexes)) #; rownames(numbers)<-years
  biomass <- array(0,dim=c(total.yrs, ages, sexes)) 
  
  #Matrix Storage for output to R list
  vul.total.obs        <- matrix(0, total.yrs, 1) #fishery.yrs, 1)
  index.expect         <- matrix(0, total.yrs, 1) #fishery.yrs, 1)
  survey.catch.age.len <- array(NA,c(total.yrs, ages, length(len.step), sexes)) #fishery.yrs, ages, length(len.step), sexes))
  fsp1.om <- numeric(total.yrs)
  recovered.om <- numeric(total.yrs)

  #Storage matrices for data samples
  f.lengths            <- matrix(0, total.yrs, (2*length(len.step))) #fishery.yrs, (2*length(len.step)))
  s.lengths            <- matrix(0, total.yrs, (2*length(len.step))) #fishery.yrs, (2*length(len.step)))
  #f.a.ca               <- matrix(0, fishery.yrs, (ages*2-2))
  #s.a.ca               <- matrix(0, fishery.yrs, (ages*2-2))
  f.a.ca               <- matrix(0, total.yrs, (max.age*2)) #fishery.yrs, (max.age*2))
  s.a.ca               <- matrix(0, total.yrs, (max.age*2)) #fishery.yrs, (max.age*2))
  f.sample.size        <- rep(0, total.yrs) #fishery.yrs)
  s.sample.size        <- rep(0, total.yrs) #fishery.yrs)

  #Arrays to Store the Estimation Results from SS
  SB          <- array(NA, dim=c(total.yrs, ass.num)) #fishery.yrs, ass.num))
  Bratio      <- array(NA, dim=c(total.yrs, ass.num)) #fishery.yrs, ass.num))
  TotBio      <- array(NA, dim=c(total.yrs, ass.num)) #fishery.yrs, ass.num))
  VulBioEst   <- array(NA, dim=c(total.yrs, ass.num)) #fishery.yrs, ass.num))
  Recruits    <- array(NA, dim=c(total.yrs, ass.num)) #fishery.yrs, ass.num))
  OFL         <- array(NA, dim=c(total.yrs + 5))
  ForeCat     <- array(NA, dim=c(total.yrs + 5))
  FSPR        <- array(NA, dim=c(1, ass.num))
  Fmult       <- array(NA, dim=c(1, ass.num))
  LLsurvey    <- array(NA, dim=c(1, ass.num))
  CrashPen    <- array(NA, dim=c(1, ass.num))
  R0.out      <- array(NA, dim=c(1, ass.num))
  Gradiant.out<- array(NA, dim=c(1, ass.num))
  M.store     <- array(NA, dim=c(2, ass.num))
  Lmin.store  <- array(NA, dim=c(1, ass.num))
  Lmax.store  <- array(NA, dim=c(1, ass.num))
  k.store     <- array(NA, dim=c(1, ass.num))
  h.store     <- array(NA, dim=c(1, ass.num))
  cv.y.store  <- array(NA, dim=c(1, ass.num))
  cv.old.store<- array(NA, dim=c(1, ass.num))
  F.selex     <- array(NA, dim=c(6, ass.num))
  fsp1.est    <- array(NA, dim=c(1, ass.num))
  fsp2.est    <- array(NA, dim=c(1, ass.num))
  S.selex     <- array(NA, dim=c(6, ass.num))
  recovered.est <- numeric(total.yrs)
  fspr.est.vec<- numeric(total.yrs)
  grad.check  <- numeric(ass.num) 
  grad.out    <- numeric(ass.num)
  
  #Dynamics
  Ry           <- matrix(0, total.yrs+1, 1)#;  rownames(Ry) <-years
  SSB          <- matrix(0, total.yrs+1, 1)#;  rownames(SSB)<-years
  depl         <- matrix(0, total.yrs+1, 1)
  ofl.true     <- matrix(0, total.yrs+1, 1)
  acl.true     <- matrix(0, total.yrs+1, 1)
  catch.at.age <- array(NA, dim=c(total.yrs, ages, sexes))
  catch.at.len <- array(NA, dim=c(total.yrs, length(len.step), sexes))
  z.rate       <- array(NA,dim = c(total.yrs, ages, sexes)) ; rownames(z.rate) <- years
  f.values     <- rep(0,(total.yrs-1))
  catch.wght.values <- rep(0,(total.yrs-1))
  selec        <- array(0, dim=c(total.yrs, length(len.step), sexes))
  selec.age    <- array(0, dim=c(total.yrs, ages, sexes)) 
  fspr.vec     <- numeric(total.yrs)
  
 #Selectivity Adjustment Parameter
  fsp1.realized <- numeric(total.yrs)

