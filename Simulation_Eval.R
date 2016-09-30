############################################
#   Chapter 3: Loss of Data & Rebuilding   #
#Load in the files from the Operating Model#
#and the estimation model.  Calculates REs #
#                                          #
#    Created July 31, 2014 by              #
#         Chantel Wetzel                   #
############################################

drive = "C:"
run.name = "Final_wo_survey/"#smallN_noAE/"#"April16_PreCPUE/"#"CPUE_smallN_AE/"#"Ass_Freq/"#"April16_PreCPUE/"
AE = TRUE
AgeError = AE
LH = "rockfish"
ds.list = c("ds1","ds2","ds4", "ds3", "ds6", "ds5") 
ds.list = c("ds1","ds4","ds6", "ds2", "ds3", "ds5")
#ds.list = c("ds1", "ds2") 
#ds.list = c("ds1", "ds4", "ds6", "ds8")
#ds.list = c("full", "reduced", "eliminated", "tv_full", "tv_reduced", "tv_eliminated")

sim.range = c(1, 100) #c(1, 100)
max.sim = 100
if (max.sim != sim.range[2]){print ("Only working up a subset")}
order = c(1,2,3,4,5,6) 
data.scenario = ""
setup.yrs = 50

set.quant = c(0.10, 0.50, 0.90)
do.survey = ifelse(run.name =="No_Survey", FALSE, TRUE)


#Dimensions by life-history
git.wd = "C:/Users/Chantell.Wetzel/Documents/GitHub/Ch3_DataLoss/"
source(paste(git.wd, "/functions/LH_parameter_values.R", sep=""))
source(paste(git.wd, "/functions/Data_Scenarios.R", sep=""))

if (file.exists( file = paste(drive,"/PhD/Chapter3/", run.name, "/output", sep = "") ) == FALSE ){
  dir.create(paste0(drive,"/PhD/Chapter3/", run.name, "/output"))
}

set.ass.freq = ass.freq 
first.ass.yr <- total.yrs - project.yrs - 1
end.catch = first.ass.yr + 25
ass.yr = seq(first.ass.yr, total.yrs, ass.freq)
new.ass.num = length(ass.yr)
ass.num = new.ass.num

med.ssb         = array(0, dim = c(length(ds.list), total.yrs, 3))
med.ry          = array(0, dim = c(length(ds.list), total.yrs, 3))
med.depl        = array(0, dim = c(length(ds.list), total.yrs, 3))
med.catch       = array(0, dim = c(length(ds.list), total.yrs, 3))
med.ofl         = array(0, dim = c(length(ds.list), total.yrs, 3))
med.acl         = array(0, dim = c(length(ds.list), total.yrs, 3))
med.ssb.est     = array(0, dim = c(length(ds.list), total.yrs, ass.num, 3))
med.ry.est      = array(0, dim = c(length(ds.list), total.yrs, ass.num, 3))
med.depl.est    = array(0, dim = c(length(ds.list), total.yrs, ass.num, 3))
med.acl.est     = array(0, dim = c(length(ds.list), total.yrs, 3))
med.ofl.est     = array(0, dim = c(length(ds.list), total.yrs, 3))
med.m.est       = array(0, dim = c(length(ds.list), ass.num, 3))
med.s.selex.est = array(0, dim = c(length(ds.list), 2, ass.num, 3))
med.f.selex.est = array(0, dim = c(length(ds.list), 2, ass.num, 3))
m.est.all       = array(0, dim = c(length(ds.list), ass.num, max.sim))
s.selex.est.all = array(0, dim = c(length(ds.list), 6, ass.num, max.sim))
f.selex.est.all = array(0, dim = c(length(ds.list), 6, ass.num, max.sim))
mare.depl       = array(NA,dim = c(length(ds.list), ass.num, total.yrs, max.sim))
mare.ssb        = array(NA,dim = c(length(ds.list), ass.num, total.yrs, max.sim))
mare.ssb0       = array(NA,dim = c(length(ds.list), ass.num, max.sim))
re.depl         = array(NA,dim = c(length(ds.list), ass.num, total.yrs, max.sim))
re.ssb          = array(NA,dim = c(length(ds.list), ass.num, total.yrs, max.sim))
re.ssb0         = array(NA,dim = c(length(ds.list), ass.num, max.sim))
re.m            = array(NA,dim = c(length(ds.list), ass.num, max.sim))
re.h            = array(NA,dim = c(length(ds.list), ass.num, max.sim))
re.k            = array(NA,dim = c(length(ds.list), ass.num, max.sim))
re.lmin         = array(NA,dim = c(length(ds.list), ass.num, max.sim))
re.lmax         = array(NA,dim = c(length(ds.list), ass.num, max.sim))
re.catch        = array(NA,dim = c(length(ds.list), project.yrs + 2 , max.sim))
re.ofl          = array(NA,dim = c(length(ds.list), project.yrs + 2 , max.sim))
re.s.selex      = array(0, dim = c(length(ds.list), 2, ass.num, max.sim))
re.f.selex      = array(0, dim = c(length(ds.list), 4, ass.num, max.sim))
re.f.selex.adj  = array(0, dim = c(length(ds.list), 1, ass.num, max.sim))
acl.min         = array(NA,dim = c(length(ds.list), total.yrs, max.sim))
rmse.sb0        = array(NA,dim = c(length(ds.list), ass.num))
rmse.depl       = array(NA,dim = c(length(ds.list), ass.num))
rmse.catch      = array(NA,dim = c(length(ds.list), ass.num))
rmse.ssb.ass    = array(NA,dim = c(length(ds.list), ass.num))

re.time.over          = matrix(0, length(ds.list), max.sim)
catch.median          = matrix(0, length(ds.list), max.sim)

#==============================================================================================
failed.to.detect.over.all <- array(0, dim = c(length(ds.list), ass.num))
failed.to.detect.rec.all  <- array(0, dim = c(length(ds.list), ass.num))
incorrect.rebuild.all     <- array(0, dim = c(length(ds.list), ass.num))

yrs.declared.rec.early.all<- array(0, dim = c(length(ds.list), max.sim))
yrs.declared.rec.late.all <- array(0, dim = c(length(ds.list), max.sim))
yrs.declared.all          <- array(0, dim = c(length(ds.list), max.sim))

aav                       <- array(0, dim = c(length(ds.list), max.sim))
aav.over                  <- array(0, dim = c(length(ds.list), max.sim))

#operating model values storage arrays ============================================================
ssb   = array(0, dim = c(length(ds.list), total.yrs, max.sim))
ry    = array(0, dim = c(length(ds.list), total.yrs, max.sim))
depl  = array(0, dim = c(length(ds.list), total.yrs, max.sim))
catch = array(0, dim = c(length(ds.list), total.yrs, max.sim))
ofl   = array(0, dim = c(length(ds.list), total.yrs, max.sim))
acl   = array(0, dim = c(length(ds.list), total.yrs, max.sim))
f.lens= array(0, dim = c(length(ds.list), total.yrs, max.sim))
s.lens= array(0, dim = c(length(ds.list), total.yrs, max.sim))
f.ages= array(0, dim = c(length(ds.list), total.yrs, max.sim))
s.ages= array(0, dim = c(length(ds.list), total.yrs, max.sim))
peak  = array(0, dim=  c(length(ds.list), total.yrs, max.sim))
dome  = array(0, dim=  c(length(ds.list), total.yrs, max.sim))
recovery.yr = array(0, dim=  c(length(ds.list), 1,   max.sim))
om.time.over= matrix(0, length(ds.list), max.sim)

#estimation values storage arrays  ================================================================
ssb.est     =  array(0, dim = c(length(ds.list), total.yrs, ass.num, max.sim))
ssb0.est    =  array(0, dim = c(length(ds.list),            ass.num, max.sim))
ry.est      =  array(0, dim = c(length(ds.list), total.yrs, ass.num, max.sim))
depl.est    =  array(0, dim = c(length(ds.list), total.yrs, ass.num, max.sim))
acl.est     =  array(0, dim = c(length(ds.list), total.yrs,          max.sim))
ofl.est     =  array(0, dim = c(length(ds.list), total.yrs,          max.sim))
m.est       =  array(0, dim = c(length(ds.list),            ass.num, max.sim))
h.est       =  array(0, dim = c(length(ds.list),            ass.num, max.sim))
k.est       =  array(0, dim = c(length(ds.list),            ass.num, max.sim))
lmin.est    =  array(0, dim = c(length(ds.list),            ass.num, max.sim))
lmax.est    =  array(0, dim = c(length(ds.list),            ass.num, max.sim))
cv.young.est=  array(0, dim = c(length(ds.list),            ass.num, max.sim))
cv.old.est  =  array(0, dim = c(length(ds.list),            ass.num, max.sim))
s.selex.est =  array(0, dim = c(length(ds.list), 6,         ass.num, max.sim))
f.selex.est =  array(0, dim = c(length(ds.list), 6,         ass.num, max.sim))
f.selex.adj.est =  array(0, dim = c(length(ds.list), 1,     ass.num, max.sim))

ave.over.catch  = matrix(0, length(ds.list), max.sim)
over.catch      = matrix(0, length(ds.list), max.sim)
yr.decl.est     = array(0, dim = c(length(ds.list), 3, max.sim))
yr.recr.est     = array(0, dim = c(length(ds.list), 3, max.sim))


n.overfished = matrix(0, length(ds.list), ass.num)
om.n.overfished = matrix(0, length(ds.list), ass.num)
n.overfished.split = matrix(0, length(ds.list), ass.num)
time.over    = matrix(0, length(ds.list), max.sim)

filt.re.time.over = list()

#Load up the operating model values============================================================
for (spec in 1:length(ds.list))
{
  j = order[spec]
  data.scenario = ds.list[j]

  #dir = paste(drive,"/PhD/Chapter3/", run.name ,LH,"_",data.scenario,"_multinom_boot_AE_TRUE_",
  #      sim.range[1],"_",sim.range[2],
  #      "/save/", sep = "")
  dir = paste(drive,"/PhD/Chapter3/", run.name ,LH,"_",data.scenario,"_multinom_boot_AE_",AE,"_",
        sim.range[1],"_", sim.range[2],
        "/save/", sep = "")
  
  for (i in sim.range[1]:max.sim) {
      dat = paste(dir, "om_proj_",i,sep ="")
      load(dat)
      ssb[j,,i]   = Proj$SSB[1:total.yrs]
      ry[j,,i]    = Proj$Ry[1:total.yrs]*2
      depl[j,,i]  = Proj$Depl[1:total.yrs]
      acl[j,,i]   = Proj$acl[1:total.yrs]
      ofl[j,,i]   = Proj$ofl.true[1:total.yrs]
      f.lens[j,,i]= Proj$f.len.samp
      s.lens[j,,i]= Proj$s.len.sam
      f.ages[j,,i]= Proj$f.age.samp
      s.ages[j,,i]= Proj$s.age.samp
      dome[j,,i]  = Proj$dome
      peak[j,,i]  = Proj$peak
      ind = Proj$recovered.om > 0
      recovery.yr[j,,i] = ifelse(sum(ind) != 0, Proj$recovered.om[ind], 0)
      om.time.over[j,i] = max(Proj$recovered.om) - first.ass.yr
      if(om.time.over[j,i] < 0) { om.time.over[j,i] = ifelse(LH == "rockfish", 101, 51) }
  }

  #Save as an output file 
  om.all <- list()
  om.out <- paste(drive,"/PhD/Chapter3/", run.name, "/output/",LH,"_om_all", sep = "")
  om.all$ssb  <- ssb
  om.all$ry   <- ry
  om.all$depl <- depl
  om.all$catch<- catch
  om.all$ofl  <- ofl
  om.all$acl  <- acl
  om.all$f.lens   <- f.lens
  om.all$s.lens   <- s.lens
  om.all$f.ages   <- f.ages
  om.all$s.ages   <- s.ages
  om.all$peak     <- peak
  om.all$dome     <- dome
  om.all$om.time.over <- om.time.over
  save (om.all, file = om.out)

  #save.index = save.index.ov = 1:sim.range[2]
  
  for (i in sim.range[1]:max.sim){
    est = paste(dir,"ss_ests_",i,sep="")
    load(est)
  
    ssb.est[j,,,i]     = Est$SB[,1:ass.num]
    ry.est[j,,,i]      = Est$Recruit[,1:ass.num]
    depl.est[j,,,i]    = Est$Bratio[,1:ass.num]
    acl.est[j,,i]      = Est$ACL[1:total.yrs]
    ofl.est[j,,i]      = Est$OFL[1:total.yrs]
    m.est[j,,i]        = Est$M.store[1,1:ass.num]
    h.est[j,,i]        = Est$h[1,1:ass.num]
    k.est[j,,i]        = Est$k.store[1,1:ass.num]
    lmin.est[j,,i]     = Est$Lmin.store[1,1:ass.num]
    lmax.est[j,,i]     = Est$Lmax.store[1,1:ass.num]
    s.selex.est[j,,,i] = Est$S.selex[,1:ass.num]
    f.selex.est[j,,,i] = Est$F.selex[,1:ass.num]
    f.selex.adj.est[j,,,i] = Est$fsp2.est[,1:ass.num]
    ssb0.est[j,,i]     = Est$SB[1,1:ass.num]/Est$Bratio[1,1:ass.num]
    ind = Est$recovered.est > 0 
    index = Est$recovered.est[ind]
    yr.decl.est[j,,i]  = c(index[1], ifelse(length(index)> 2, index[3],0), ifelse(length(index)> 2, index[5],0))
    yr.recr.est[j,,i]  = c(index[2], ifelse(length(index)> 2, index[4],0), ifelse(length(index)> 2, index[6],0))

    if (sum(Est$recovered.est) != 0){
      ind = Est$recovered.est>0
      values = unique(sort(Est$recovered.est[ind]))
      time.over[j,i] = values[2] -values[1] + 1
    }
    if (length(values) == 1) { time.over[j,i] = 101 }
    
    ind = ifelse(is.na(yr.recr.est[j,1,i]), first.ass.yr + 100, yr.recr.est[j,1,i])
    over.catch[j,i] = sum(Est$ACL[first.ass.yr:ind])
    ave.over.catch[j,i] = over.catch[j,i]/ind
  } 

  #Save as an output file 
  est.all <- list()
  est.out <- paste(drive,"/PhD/Chapter3/", run.name,"/output/",LH,"_est_all", sep = "")
  est.all$ssb.est  <- ssb.est
  est.all$ry.est   <- ry.est
  est.all$depl.est <- depl.est
  est.all$acl.est  <- acl.est
  est.all$ofl.est  <- ofl.est
  est.all$m.est    <- m.est
  est.all$h.est    <- h.est
  est.all$k.est    <- k.est
  est.all$lmin.est <- lmin.est
  est.all$lmax.est <- lmax.est
  est.all$s.selex.est <- s.selex.est
  est.all$f.selex.est <- f.selex.est
  est.all$f.selex.adj.est <- f.selex.adj.est
  est.all$ssb0.est    <- ssb0.est
  est.all$yr.decl.est <- yr.decl.est
  est.all$yr.recr.est <- yr.recr.est
  est.all$time.over   <- time.over
  est.all$over.catch  <- over.catch
  est.all$ave.over.catch <- ave.over.catch
  save(est.all, file = est.out)
   

  #Calculate when the stock is rebuilt but not estimated as such=============================================================================  
  failed.to.detect.over <- matrix(0, ass.num, max.sim)
  failed.to.detect.rec  <- matrix(0, ass.num, max.sim)
  incorrect.rebuild     <- matrix(0, ass.num, max.sim)

  for(a in sim.range[1]:max.sim){
    for(b in 1:new.ass.num){
      temp = first.ass.yr + b*4 - 4
      ind  = first.ass.yr + b*4 - 4
      past.state = FALSE
      if (b == 1) {
        if (depl.est[j,temp,b,a] > over.thres){
          failed.to.detect.over[b,a] <- 1
          overfished = FALSE
        }
        if (depl.est[j,temp,b,a] < over.thres){
          failed.to.detect.over[b,a] <- 0
          overfished = TRUE
        }
      }
    
      if (b > 1 && depl.est[j,temp,b,a] > over.thres && depl[j,ind,a] < over.thres && overfished == FALSE){
          failed.to.detect.over[b,a] <- 1
          overfished = FALSE
      }
      
      if (overfished == FALSE) {
        if(depl.est[j,temp,b,a] > over.thres && depl[j,ind,a] > over.thres){
          overfished = FALSE
        }
        if(depl.est[j,temp,b,a] < over.thres && depl[j,ind,a] < over.thres){
          overfished = TRUE
        }
      }
  
      if(overfished == TRUE){
        if(depl.est[j,temp,b,a] < ctl.rule.tgt && depl[j,ind,a] < ctl.rule.tgt){
          overfished = TRUE
          if(b > 1 && failed.to.detect.rec[b-1,a] == 1){
            failed.to.detect.rec[b,a] = 1
          }
        }
        if(b > 1 && depl.est[j,temp,b,a] < ctl.rule.tgt && depl[j,ind,a] < ctl.rule.tgt && incorrect.rebuild[b-1,a] == 1){
          incorrect.rebuild[b,a] = 1
          overfished = TRUE
        }
        if(depl.est[j,temp,b,a] < ctl.rule.tgt && depl[j,ind,a] > ctl.rule.tgt){
          failed.to.detect.rec[b,a] <- 1
          overfished = TRUE
        }
        if(b > 1 && depl.est[j,temp,b,a] < ctl.rule.tgt && depl[j,ind,a] > ctl.rule.tgt && failed.to.detect.rec[b-1,a] == 1){
          failed.to.detect.rec[b,a] <- 1
          overfished = TRUE
        }
        if(b > 1 && depl.est[j,temp,b,a] < ctl.rule.tgt && depl[j,ind,a] > ctl.rule.tgt && incorrect.rebuild[b-1,a] == 1){
          failed.to.detect.rec[b,a] <- 0
          incorrect.rebuild[b,a] <- 0
          overfished = FALSE
        }
  
        if(depl.est[j,temp,b,a] > ctl.rule.tgt && depl[j,ind,a] < ctl.rule.tgt){
          incorrect.rebuild[b,a] <- 1
          overfished = TRUE
        }
        if(depl.est[j,temp,b,a] > ctl.rule.tgt && depl[j,ind,a] > ctl.rule.tgt){
          overfished = FALSE
        }
      }
  
      if(overfished == FALSE && past.state == TRUE){
        if(depl.est[j,temp,b,a] > ctl.rule.tgt && depl[j,ind,a] < ctl.rule.tgt){
          incorrect.rebuild[b,a] <- 1
        }
        if(depl.est[j,temp,b,a] > ctl.rule.tgt && depl[j,ind,a] > ctl.rule.tgt){
          incorrect.rebuild[b,a] <- 0
        }
      }
  
      past.state = overfished
    }
  }
  
  # Not sure about this calculation..................................................
  failed.to.detect.rec.all[j,]     = apply(failed.to.detect.rec, 1, sum)
  failed.to.detect.over.all[j,]    = apply(failed.to.detect.over, 1, sum)
  incorrect.rebuild.all[j,]        = apply(incorrect.rebuild,1, sum)

  # Calculates the number of estimated overfished stocks over the 26 assessments called n.overfished
  depl.temp = depl.est[,,,]
  n.overfished[j,1] = 100
  for(a in sim.range[1]:max.sim) { 
    overfished = TRUE
    for(b in 2:new.ass.num){
      temp = first.ass.yr + b*ass.freq - ass.freq

      if(depl.temp[j,temp,b,a] < ctl.rule.tgt && overfished == TRUE){
        n.overfished[j,b] = n.overfished[j,b] + 1
      }
      #if (depl.temp[j,temp,b,a] < over.thres && overfished == FALSE){
      #  overfished = TRUE
      #  n.overfished[j,b] = n.overfished[j,b] + 1
      #}
      if(depl.temp[j,temp,b,a] > ctl.rule.tgt && overfished == TRUE){
        overfished = FALSE
      }
    }
  }

  depl.temp = depl[,,]
  om.n.overfished[j,1] = 100
  for(a in sim.range[1]:max.sim) { 
    overfished = TRUE
    for(b in 2:new.ass.num){
      temp = first.ass.yr + b*ass.freq - ass.freq
      if(depl.temp[j,temp,a] < ctl.rule.tgt && overfished == TRUE){
        om.n.overfished[j,b] = om.n.overfished[j,b] + 1
      }
      #if (depl.temp[j,temp,a] < over.thres && overfished == FALSE){
      #  overfished = TRUE
      #  om.n.overfished[j,b] = om.n.overfished[j,b] + 1
      #}
      if(depl.temp[j,temp,a] > ctl.rule.tgt && overfished == TRUE){
        overfished = FALSE
      }
    }
  }

  #Determine if any of the stocks fall below the overfished thresh after being "rebuilt"==================
  overfished.again = array(0, c(length(ds.list), total.yrs+1, max.sim))
  for (a in 1:max.sim){
    for (b in 1:new.ass.num){
      temp = first.ass.yr + b*4 - 4
      ind  = first.ass.yr + b*4 - 4
      yr.rebuilt = (depl.est[j,temp,b,a] > ctl.rule.tgt) 
      if (yr.rebuilt == TRUE) { break() }
    }
    overfished.again[j, ind:dim(depl)[2], a] = (depl[j, ind:dim(depl)[2], a] < over.thres)
  }
 
  re.time.over[j,] = (time.over[j,] - om.time.over[j,])/ om.time.over[j,]

  #filter to only calculate the ones that recovered in both the OM and EM
  keep = time.over[j,] != 101
  filtered.time.over = time.over[j, keep]
  filtered.om.time.over = om.time.over[j, keep]
  keep = filtered.om.time.over != 101
  filtered.time.over = filtered.time.over[keep]
  filtered.om.time.over = filtered.om.time.over[keep]
  filt.re.time.over[[j]] = (filtered.time.over - filtered.om.time.over) / filtered.om.time.over

  # AAV for the overfished period
  find = time.over[j,] == 101
  filtered.time.over = time.over[j, ] + first.ass.yr
  filtered.time.over[find] = filtered.time.over[find] #+ 101 + 101
  filtered.acl.est = acl.est[j,,]
  temp.abs =temp.sum= NULL
  for (z in 1:max.sim){
    temp.abs = c(temp.abs, sum( abs(filtered.acl.est[(first.ass.yr+1):(filtered.time.over[z] -1),z] - 
                filtered.acl.est[(first.ass.yr+2):(filtered.time.over[z]),z])))
    temp.sum = c(temp.sum, sum(filtered.acl.est[(first.ass.yr+1):(filtered.time.over[z]), z]) )
  }
  
  aav.over[j,] = 100*temp.abs/temp.sum

  #AAV in Catch ==========================================================================================
  abs.catch = mapply(function(x) abs(acl.est[j,x,] - acl.est[j,x+1,]), x=(first.ass.yr+1):(total.yrs-1))
  sum.catch.by.sim = apply(acl.est[j,(first.ass.yr + 1):(total.yrs),],2,sum)
  aav[j,] = 100*(apply(abs.catch,1,sum))/sum.catch.by.sim

  #Catch over fixed period ===============================================================================
  catch.median[j,] = apply(acl.est[j,first.ass.yr:end.catch,],2,median)

  #Calculate medians and relative errors==================================================================
  
  # Operating model medians
  med.ssb[j,,]    = t(apply(ssb[j,,],   1, quantile, set.quant))
  med.ry[j,,]     = t(apply(ry[j,,],    1, quantile, set.quant))
  med.depl[j,,]   = t(apply(depl[j,,],  1, quantile, set.quant))
  med.ofl[j,,]    = t(apply(ofl[j,,],   1, quantile, set.quant))
  med.acl[j,,]    = t(apply(acl[j,,],   1, quantile, set.quant))
  
  # Estimation model medians
  for (i in 1:length(ass.yr))
  {
     y = 1:ass.yr[i]
     med.ssb.est[j,y,i,]     = t(apply( ssb.est[j,  y, i, 1:max.sim], 1, quantile, set.quant ))
     med.depl.est[j,y,i,]    = t(apply( depl.est[j, y, i, 1:max.sim], 1, quantile, set.quant ))

     y = 1:(ass.yr[i]-1) 
     med.ry.est[j,y,i,]      = t(apply( ry.est[j,   y, i, 1:max.sim], 1, quantile, set.quant, na.rm = T))      
  }
  
  med.m.est[j,,]         = t(apply(m.est        [j,,1:max.sim], 1, quantile, set.quant))
  med.s.selex.est[j,1,,] = t(apply(s.selex.est[j,1,,1:max.sim], 1, quantile, set.quant))
  med.s.selex.est[j,2,,] = t(apply(s.selex.est[j,3,,1:max.sim], 1, quantile, set.quant))
  med.f.selex.est[j,1,,] = t(apply(f.selex.est[j,1,,1:max.sim], 1, quantile, set.quant))
  med.f.selex.est[j,2,,] = t(apply(f.selex.est[j,3,,1:max.sim], 1, quantile, set.quant))
  med.acl.est[j,,]       = t(apply(acl.est[j,,],                     1, quantile, set.quant, na.rm = T))
  med.ofl.est[j,,]       = t(apply(ofl.est[j,,],                     1, quantile, set.quant, na.rm = T))

  for (a in 1:new.ass.num){
    mare.depl[j,a,,] <- abs(depl.est[j,,a,] - depl[j,1:total.yrs,]) / depl[j,1:total.yrs,]
    mare.ssb[j,a,,]  <- abs(ssb.est[j,,a,]  - ssb[j, 1:total.yrs,]) / ssb[j, 1:total.yrs,]
    mare.ssb0[j,a,]  <- abs(ssb0.est[j,a,]  - ssb[j,1,])/ ssb[j,1,]
    re.depl[j,a,,]   <- (depl.est[j,,a,] - depl[j,1:total.yrs,]) / depl[j,1:total.yrs,]
    re.ssb[j,a,,]    <- (ssb.est[j,,a,]  - ssb[j, 1:total.yrs,]) / ssb[j, 1:total.yrs,]
    re.ssb0[j,a,]    <- (ssb0.est[j,a,]  - ssb[j,1,])/ ssb[j,1,]
    re.m[j,a,]       <- (m.est[j,a,] - m) / m
    re.h[j,a,]       <- (h.est[j,a,] - steep) / steep
    re.k[j,a,]       <- (k.est[j,a,] - kf) / kf
    re.lmin[j,a,]    <- (lmin.est[j,a,] - L1) / L1
    re.lmax[j,a,]    <- (lmax.est[j,a,] - L2f) / L2f
  }

  ind = (first.ass.yr) :(total.yrs)
  for (a in 1:max.sim){
    for (b in 1:length(ind)){
      index = ind[b]
      re.catch[j,b,a] <- (acl.est[j,index,a] - max(acl[j,index,a], 5))/(max(acl[j,index,a], 5))
    }
  }
  
  re.ofl[j,,]   <- (ofl.est[j,ind,] - ofl[j,ind,])/ofl[j,ind,]

  for (a in 1:new.ass.num){   
    re.f.selex[j,1,a,] <- (f.selex.est[j,1,a,] - fsp1.start) / fsp1.start
    re.f.selex[j,2,a,] <- (f.selex.est[j,2,a,] - fsp2 )/ fsp2
    re.f.selex[j,3,a,] <- (f.selex.adj.est[j,1,a,] - (fsp2 + dome.adj) )/ (fsp2 + dome.adj)
    re.f.selex[j,4,a,] <- (f.selex.est[j,3,a,] - fsp3) / fsp3
    re.s.selex[j,1,a,] <- (s.selex.est[j,1,a,] - ssp1) / ssp1
    re.s.selex[j,2,a,] <- (s.selex.est[j,3,a,] - ssp3) / ssp3
  }
  
  for(a in 1:new.ass.num){
    index = ass.yr[a]
    rmse.sb0[j,a]   =  100 * sqrt((1 / max.sim) * 
                      sum(((ssb0.est[j,a,] - ssb[j, 1,])^2) / (ssb[j, 1 ,]^2)))
    square = ((depl.est[j,index,a,] - depl[j,index,])^2)/(depl[j,index ,]^2)
    if (j == 6) { square = square[-68] }
    #rmse.depl[j,a]  =  100 * sqrt((1/max.sim)*sum(((depl.est[j,index,a,] - depl[j,index ,])^2)/
    #                    (depl[j,index ,]^2)))
    rmse.depl[j,a]  =  100 * sqrt((1/length(square))*sum(square))
    rmse.ssb.ass[j,a] = 100 * sqrt((1 / max.sim) * 
                      sum(((ssb.est[j,index,a,] - ssb[j, index,])^2) / (ssb[j, index,]^2)))
  }
  
  meds.all <- list()
  meds.out <- paste(drive,"/PhD/Chapter3/", run.name,"/output/",LH,"_meds_all", sep = "")

  meds.all$med.ssb      <- med.ssb
  meds.all$med.ry       <- med.ry
  meds.all$med.depl     <- med.depl
  meds.all$med.catch    <- med.catch
  meds.all$med.ofl      <- med.ofl
  meds.all$med.acl      <- med.acl
  meds.all$med.ssb.est  <- med.ssb.est
  meds.all$med.ry.est   <- med.ry.est
  meds.all$med.depl.est <- med.depl.est
  meds.all$med.acl.est  <- med.acl.est
  meds.all$med.ofl.est  <- med.ofl.est
  meds.all$med.m.est    <- med.m.est
  meds.all$mare.depl    <- mare.depl
  meds.all$mare.ssb     <- mare.ssb
  meds.all$mare.ssb0    <- mare.ssb0
  meds.all$re.depl      <- re.depl
  meds.all$re.ssb       <- re.ssb
  meds.all$re.ssb0      <- re.ssb0
  meds.all$re.m         <- re.m
  meds.all$re.h         <- re.h
  meds.all$re.k         <- re.k
  meds.all$re.lmin      <- re.lmin
  meds.all$re.lmax      <- re.lmax
  meds.all$re.catch     <- re.catch
  meds.all$re.ofl       <- re.ofl
  meds.all$re.f.selex   <- re.f.selex
  meds.all$re.s.selex   <- re.s.selex
  meds.all$rmse.depl    <- rmse.depl
  meds.all$rmse.sb0     <- rmse.sb0
  meds.all$rmse.ssb.ass <- rmse.ssb.ass
  meds.all$re.time.over <- re.time.over
  meds.all$catch.median <- catch.median
  meds.all$aav          <- aav
  meds.all$aav.over     <- aav.over
  meds.all$filt.re.time.over          <- filt.re.time.over
  meds.all$failed.to.detect.rec.all   <- failed.to.detect.rec.all
  meds.all$failed.to.detect.over      <- failed.to.detect.over
  meds.all$incorrect.rebuild          <- incorrect.rebuild  
  meds.all$yrs.declared.rec.late.all  <- yrs.declared.rec.late.all
  meds.all$yrs.declared.rec.early.all <- yrs.declared.rec.early.all
  meds.all$yrs.declared.all           <- yrs.declared.all
  meds.all$overfished.again           <- overfished.again
  meds.all$n.overfished    <- n.overfished
  meds.all$om.n.overfished <- om.n.overfished
  save (meds.all, file = meds.out)  
} 
