#########################################################
##            Reads Report Files                       ##
##      and pulls out required quantities              ##
##         Written by:  Chantel Wetzel                 ##
##              Date: 4-18-2014                        ##
#########################################################


Rep_Summary<- function(rep.new, y, pre.fishery.yrs, do.forecast)
{
  tot.yrs <- 1:y #1:(y-pre.fishery.yrs)

  if (do.forecast > 0){
    ofl.yrs <- (tot.yrs[length(tot.yrs)]+1):(tot.yrs[length(tot.yrs)] + 4)
    OFL = mapply(function(x) OFL = as.numeric(strsplit(rep.new[grep(paste("OFLCatch_",x,sep=""),rep.new)], " ")[[1]][3]), x = ofl.yrs)
    ACL = mapply(function(x) OFL = as.numeric(strsplit(rep.new[grep(paste("ForeCatch_",x,sep=""),rep.new)], " ")[[1]][3]), x = ofl.yrs)
    ForeCatch = mapply(function(x) ForeCatch = as.numeric(strsplit(rep.new[grep(paste("ForeCatch_",x,sep=""),rep.new)], " ")[[1]][3]), x = ofl.yrs)
  }
  
  SB = mapply(function(x) SB = as.numeric(strsplit(rep.new[grep(paste("SPB_",x,sep=""),rep.new)]," ")[[1]][3]), x = tot.yrs)
  SB.virgin = as.numeric(strsplit(rep.new[grep("SPB_Virgin",rep.new)]," ")[[1]][3])
  
  Recruits = mapply(function(x) TotBio = as.numeric(strsplit(rep.new[grep(paste(1, x,"TIME",sep=" "),rep.new)]," ")[[1]][8]),
                                        x = 1:tot.yrs[length(tot.yrs)-1])
  TotBio= mapply(function(x) TotBio = as.numeric(strsplit(rep.new[grep(paste(1, x,"TIME",sep=" "),rep.new)]," ")[[1]][5]),
                                        x = 1:tot.yrs[length(tot.yrs)-1])
  FMSY = as.numeric(strsplit(rep.new[grep("Fstd_MSY",rep.new)], " ")[[1]][3])
  FSPR = as.numeric(strsplit(rep.new[grep("Fstd_SPR",rep.new)], " ")[[1]][3])
  #LLsurvey = as.numeric(strsplit(rep.new[grep("TOTAL",rep.new)+2], " ")[[1]][2])
  #CrashPen = as.numeric(strsplit(rep.new[grep("TOTAL",rep.new)+8], " ")[[1]][2])#NEW HERE
  R0 = as.numeric(strsplit(rep.new[grep("R0",rep.new)], " ")[[1]][3])
  M.f = as.numeric(strsplit(rep.new[grep("NatM_p_1_Fem_GP",rep.new)], " ")[[1]][3])
  M.m = as.numeric(strsplit(rep.new[grep("NatM_p_1_Mal_GP",rep.new)], " ")[[1]][3])
  Lmin = as.numeric(strsplit(rep.new[grep("L_at_Amin_Fem_GP",rep.new)], " ")[[1]][3])
  Lmax = as.numeric(strsplit(rep.new[grep("L_at_Amax_Fem_GP",rep.new)], " ")[[1]][3])
  k    = as.numeric(strsplit(rep.new[grep("VonBert_K_Fem_GP",rep.new)], " ")[[1]][3])
  cv.young  = as.numeric(strsplit(rep.new[grep("CV_young",rep.new)], " ")[[1]][3])
  cv.old    = as.numeric(strsplit(rep.new[grep("CV_old",rep.new)], " ")[[1]][3])
  h    = as.numeric(strsplit(rep.new[grep("SR_BH_steep",rep.new)], " ")[[1]][3])

  #Selectivity
  F.Selex.1 = as.numeric(strsplit(rep.new[grep("SizeSel_1P_1_Fishery",rep.new)], " ")[[1]][3])
  F.Selex.2 = as.numeric(strsplit(rep.new[grep("SizeSel_1P_2_Fishery",rep.new)], " ")[[1]][3])
  F.Selex.3 = as.numeric(strsplit(rep.new[grep("SizeSel_1P_3_Fishery",rep.new)], " ")[[1]][3])
  F.Selex.4 = as.numeric(strsplit(rep.new[grep("SizeSel_1P_4_Fishery",rep.new)], " ")[[1]][3])
  F.Selex.5 = as.numeric(strsplit(rep.new[grep("SizeSel_1P_5_Fishery",rep.new)], " ")[[1]][3])
  F.Selex.6 = as.numeric(strsplit(rep.new[grep("SizeSel_1P_6_Fishery",rep.new)], " ")[[1]][3])
  S.Selex.1 = as.numeric(strsplit(rep.new[grep("SizeSel_2P_1_Survey",rep.new)], " ")[[1]][3])
  S.Selex.2 = as.numeric(strsplit(rep.new[grep("SizeSel_2P_2_Survey",rep.new)], " ")[[1]][3])
  S.Selex.3 = as.numeric(strsplit(rep.new[grep("SizeSel_2P_3_Survey",rep.new)], " ")[[1]][3])
  S.Selex.4 = as.numeric(strsplit(rep.new[grep("SizeSel_2P_4_Survey",rep.new)], " ")[[1]][3])
  S.Selex.5 = as.numeric(strsplit(rep.new[grep("SizeSel_2P_5_Survey",rep.new)], " ")[[1]][3])
  S.Selex.6 = as.numeric(strsplit(rep.new[grep("SizeSel_2P_6_Survey",rep.new)], " ")[[1]][3])

  if (need.blocks == TRUE){
    #F.Selex.1.adj = ifelse(overfished.counter == 0, 0,
    #          as.numeric(strsplit(rep.new[grep("SizeSel_1P_1_Fishery_BLK1",rep.new)], " ")[[1]][3]) )
    F.Selex.2.adj = ifelse(overfished.counter == 0, 0,
              as.numeric(strsplit(rep.new[grep("SizeSel_1P_2_Fishery_BLK1",rep.new)], " ")[[1]][3]) )
  }
  
  #x = start.survey:(y-pre.fishery.yrs - 1)
  x = seq(start.survey,(y - 1), 2)
  VulBioEst <- mapply(function(x)
               VulBioEst = as.numeric(strsplit(rep.new[grep(paste(2,"Survey",x,sep=" "),rep.new)], " ")[[1]][6]), x = x)
  
  Depl = SB/SB.virgin
  
  RepSummary <- list()
  RepSummary$SB <- SB
  RepSummary$SB.virgin <- SB.virgin
  RepSummary$TotBio <- TotBio
  if (do.forecast > 0 ){
    RepSummary$OFL <- OFL
    RepSummary$ACL <- ACL
    RepSummary$ForeCatch <- ForeCatch
  }
  RepSummary$VulBioEst <- VulBioEst
  RepSummary$Depl <- Depl
  RepSummary$FSPR <- FSPR
  RepSummary$FMSY <-FMSY
  RepSummary$FSelex<- c(F.Selex.1, F.Selex.2, F.Selex.3, F.Selex.4, F.Selex.5, F.Selex.6)
  RepSummary$R0<- R0
  RepSummary$SSelex<- c(S.Selex.1, S.Selex.2, S.Selex.3, S.Selex.4, S.Selex.5, S.Selex.6)
  RepSummary$M<- cbind(M.f,M.m)
  RepSummary$Recruits<- Recruits
  RepSummary$Lmin<- Lmin
  RepSummary$Lmax<- Lmax
  RepSummary$k   <- k
  RepSummary$cv.young <- cv.young
  RepSummary$cv.old <- cv.old
  RepSummary$h <- h
  if(need.blocks == TRUE){
    #RepSummary$F.selex.1.adj <- F.Selex.1.adj
    RepSummary$F.selex.2.adj <- F.Selex.2.adj
  }  
  return(RepSummary)
}
