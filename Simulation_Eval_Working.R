############################################
#   Chapter 3: Loss of Data & Rebuilding   #
#Load in the files from the Operating Model#
#and the estimation model.  Calculates REs #
#                                          #
#    Created July 31, 2014 by              #
#         Chantel Wetzel                   #
############################################

drive = "G"
LH = "flatfish"
ds.list = c("ds1", "ds2", "ds3", "ds4")
sim.range = c(1,100)
order = c(3,1,2,4)
#ds.list = c("greatall")
#sim.range = c(1,50)

#Dimensions by life-history
source(paste(drive,":/PhD/Chapter3/code/functions/LH_parameter_values.R",sep=""))
pre.fishery.yrs <- ages - 1 
setup.yrs   <- 49
first.ass.yr <- 50
project.yrs <- 100
if (LH == "flatfish") { project.yrs = 52 }
fishery.yrs <- setup.yrs + project.yrs + 2
total.yrs   <- pre.fishery.yrs + fishery.yrs
ass.num     <- (project.yrs / 4) + 1
ass.yr = seq(first.ass.yr, fishery.yrs, 4)


med.ssb         = array(0, dim = c(length(ds.list), total.yrs + 1, 3))
med.ry          = array(0, dim = c(length(ds.list), total.yrs - 1, 3))
med.depl        = array(0, dim = c(length(ds.list), total.yrs + 1, 3))
med.catch       = array(0, dim = c(length(ds.list), total.yrs, 3))
med.ofl         = array(0, dim = c(length(ds.list), total.yrs, 3))
med.acl         = array(0, dim = c(length(ds.list), total.yrs, 3))
med.ssb.est     = array(0, dim = c(length(ds.list), fishery.yrs, ass.num, 3))
med.ry.est      = array(0, dim = c(length(ds.list), fishery.yrs, ass.num, 3))
med.depl.est    = array(0, dim = c(length(ds.list), fishery.yrs, ass.num, 3))
med.catch.est   = array(0, dim = c(length(ds.list), total.yrs + 4, 3))
med.ofl.est     = array(0, dim = c(length(ds.list), total.yrs + 4, 3))
med.m.est       = array(0, dim = c(length(ds.list), ass.num, 3))
med.s.selex.est = array(0, dim = c(length(ds.list), 2, ass.num, 3))
med.f.selex.est = array(0, dim = c(length(ds.list), 2, ass.num, 3))
m.est.all       = array(0, dim = c(length(ds.list), ass.num, sim.range[2]))
s.selex.est.all = array(0, dim = c(length(ds.list), 6, ass.num, sim.range[2]))
f.selex.est.all = array(0, dim = c(length(ds.list), 6, ass.num, sim.range[2]))
re.depl         = array(NA,dim = c(length(ds.list), ass.num, fishery.yrs, sim.range[2]))
re.ssb          = array(NA,dim = c(length(ds.list), ass.num, fishery.yrs, sim.range[2]))
re.ssb0         = array(NA,dim = c(length(ds.list), ass.num, sim.range[2]))
re.m            = array(NA,dim = c(length(ds.list), ass.num, sim.range[2]))
re.catch        = array(NA,dim = c(length(ds.list), project.yrs + 1 , sim.range[2]))
acl.min         = array(NA,dim = c(length(ds.list), total.yrs, sim.range[2]))
rmse.sb0        = array(NA,dim = c(length(ds.list), ass.num))
rmse.depl       = array(NA,dim = c(length(ds.list), ass.num))
rmse.catch      = array(NA,dim = c(length(ds.list), ass.num))

med.ssb.split          = array(0, dim = c(length(ds.list), total.yrs + 1, 3))
med.ry.split           = array(0, dim = c(length(ds.list), total.yrs - 1, 3))
med.depl.split         = array(0, dim = c(length(ds.list), total.yrs + 1, 3))
med.catch.split        = array(0, dim = c(length(ds.list), total.yrs, 3))
med.ofl.split          = array(0, dim = c(length(ds.list), total.yrs, 3))
med.acl.split          = array(0, dim = c(length(ds.list), total.yrs, 3))
med.ssb.est.split      = array(0, dim = c(length(ds.list), fishery.yrs, ass.num, 3))
med.ry.est.split       = array(0, dim = c(length(ds.list), fishery.yrs, ass.num, 3))
med.depl.est.split     = array(0, dim = c(length(ds.list), fishery.yrs, ass.num, 3))
med.catch.est.split    = array(0, dim = c(length(ds.list), total.yrs + 4, 3))
med.ofl.est.split      = array(0, dim = c(length(ds.list), total.yrs + 4, 3))
med.m.est.split        = array(0, dim = c(length(ds.list), ass.num, 3))
med.s.selex.est.split  = array(0, dim = c(length(ds.list), 2, ass.num, 3))
med.f.selex.est.split  = array(0, dim = c(length(ds.list), 2, ass.num, 3))
m.est.split        = array(0, dim = c(length(ds.list), ass.num, sim.range[2]))
s.selex.est.split  = array(0, dim = c(length(ds.list), 6, ass.num, sim.range[2]))
f.selex.est.split  = array(0, dim = c(length(ds.list), 6, ass.num, sim.range[2])) 

#==============================================================================================
failed.to.detect.over.all <- array(0, dim = c(length(ds.list), ass.num))
failed.to.detect.rec.all  <- array(0, dim = c(length(ds.list), ass.num))
incorrect.rebuild.all     <- array(0, dim = c(length(ds.list), ass.num))

yrs.declared.rec.early.all<- array(0, dim = c(length(ds.list), sim.range[2]))
yrs.declared.rec.late.all <- array(0, dim = c(length(ds.list), sim.range[2]))
yrs.declared.all          <- array(0, dim = c(length(ds.list), sim.range[2]))

aav                       <- array(0, dim = c(length(ds.list), sim.range[2]))

#operating model values storage arrays ============================================================
ssb   = array(0, dim = c(length(ds.list), total.yrs+1, sim.range[2]))
ry    = array(0, dim = c(length(ds.list), total.yrs+1, sim.range[2]))
depl  = array(0, dim = c(length(ds.list), total.yrs+1, sim.range[2]))
catch = array(0, dim = c(length(ds.list), total.yrs,   sim.range[2]))
ofl   = array(0, dim = c(length(ds.list), total.yrs,   sim.range[2]))
acl   = array(0, dim = c(length(ds.list), total.yrs,   sim.range[2]))
f.lens= array(0, dim = c(length(ds.list), total.yrs - pre.fishery.yrs, sim.range[2]))
s.lens= array(0, dim = c(length(ds.list), total.yrs - pre.fishery.yrs,   sim.range[2]))
f.ages= array(0, dim = c(length(ds.list), total.yrs - pre.fishery.yrs,   sim.range[2]))
s.ages= array(0, dim = c(length(ds.list), total.yrs - pre.fishery.yrs,   sim.range[2]))

#estimation values storage arrays  ================================================================
ssb.est     =  array(0, dim = c(length(ds.list), fishery.yrs, ass.num, sim.range[2]))
ry.est      =  array(0, dim = c(length(ds.list), fishery.yrs, ass.num, sim.range[2]))
depl.est    =  array(0, dim = c(length(ds.list), fishery.yrs, ass.num, sim.range[2]))
m.est       =  array(0, dim = c(length(ds.list), ass.num, sim.range[2]))
s.selex.est =  array(0, dim = c(length(ds.list), 6,           ass.num, sim.range[2]))
f.selex.est =  array(0, dim = c(length(ds.list), 6,           ass.num, sim.range[2]))
catch.est   =  array(0, dim = c(length(ds.list), (total.yrs+4),   sim.range[2]))
ofl.est     =  array(0, dim = c(length(ds.list), (total.yrs+4),   sim.range[2]))
ssb0.est    =  array(0, dim = c(length(ds.list), ass.num, sim.range[2]))


#Load up the operating model values============================================================
for (spec in 1:length(ds.list))
{
  j = order[spec]
  data.scenario = ds.list[j]
  dir = paste(drive,":/PhD/Chapter3/",LH,"_",data.scenario,"_sims_",sim.range[1],"_",sim.range[2],
        "/save/", sep = "")
  
  for (i in sim.range[1]:sim.range[2]) {
      dat = paste(dir, "om_proj_",i,sep ="")
      load(dat)
      ssb[j,,i]   = Proj$SSB
      ry[j,,i]    = Proj$Ry*2
      depl[j,,i]  = Proj$depl
      catch[j,,i] = Proj$fore.catch[1:total.yrs]
      ofl[j,,i]   = Proj$true.ofl
      acl[j,,i]   = ofl[j,,i]*exp(qnorm(0.45, 0, 0.36))
      f.lens[j,,i]= Proj$f.len.samp
      s.lens[j,,i]= Proj$s.len.sam
      f.ages[j,,i]= Proj$f.age.samp
      s.ages[j,,i]= Proj$s.age.samp
  }

  #Save as an output file 
  om.all <- list()
  om.out <- paste(drive,":/PhD/Chapter3/",LH,"_om_all", sep = "")
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
  save (om.all, file = om.out)

  #Sort out the runs that were never overfished and calculate some metrics=====================================================
  if(data.scenario == "ds3" ) {
    index = apply(f.lens[j, first.ass.yr:fishery.yrs,],2,sum) != 100 * sum(length(first.ass.yr:fishery.yrs))
    index.ov = apply(f.lens[j,first.ass.yr:fishery.yrs,],2,sum) == 100 * sum(length(first.ass.yr:fishery.yrs))
    print(paste("Number not estimated overfished", sum(index.ov)))
    temp = 1:100
    save.index = index
    save.index.ov = index.ov
    print(temp[index==FALSE])

    re.ssb.split          = array(NA,dim = c(length(ds.list), ass.num, fishery.yrs, sum(save.index)))
    re.ssb0.split         = array(NA,dim = c(length(ds.list), ass.num, sum(save.index)))
    re.m.split            = array(NA,dim = c(length(ds.list), ass.num, sum(save.index)))
    re.depl.split         = array(NA,dim = c(length(ds.list), ass.num, fishery.yrs, sum(save.index)))
    re.catch.split        = array(NA,dim = c(length(ds.list), project.yrs + 1 , sum(save.index)))
    acl.min.split         = array(NA,dim = c(length(ds.list), total.yrs, sum(save.index)))
    rmse.sb0.split        = array(NA,dim = c(length(ds.list), ass.num))
    rmse.depl.split       = array(NA,dim = c(length(ds.list), ass.num))
    rmse.catch.split      = array(NA,dim = c(length(ds.list), ass.num))
    yrs.declared.all.split= array(0, dim = c(length(ds.list), sum(save.index)))
  }

  
  #Split the never determined overfished from the correct deterimination runs===================================================
  om.split <- list()
  om.out.split <- paste(drive,":/PhD/Chapter3/",LH,"_om_split", sep = "")
  om.split$ssb    <- ssb[,,save.index]
  om.split$ry     <- ry[,,save.index]
  om.split$depl   <- depl[,,save.index]
  om.split$catch  <- catch[,,save.index]
  om.split$ofl    <- ofl[,,save.index]
  om.split$acl    <- acl[,,save.index]
  om.split$f.lens   <- f.lens[,,save.index]
  om.split$s.lens   <- s.lens[,,save.index]
  om.split$f.ages   <- f.ages[,,save.index]
  om.split$s.ages   <- s.ages[,,save.index]
  om.split$ssb.no   <- ssb[,,save.index.ov]
  om.split$ry.no    <- ry[,, save.index.ov]
  om.split$depl.no  <- depl[,,save.index.ov]
  om.split$catch.no <- catch[,,save.index.ov]
  om.split$ofl.no   <- ofl[,,save.index.ov]
  om.split$acl.no   <- acl[,,save.index.ov]
  om.split$f.lens.no   <- f.lens[,,save.index.ov]
  om.split$s.lens.no   <- s.lens[,,save.index.ov]
  om.split$f.ages.no   <- f.ages[,,save.index.ov]
  om.split$s.ages.no   <- s.ages[,,save.index.ov]
  save (om.split, file = om.out.split)
  
  for (i in sim.range[1]:sim.range[2]){
    est = paste(dir,"ss_ests_",i,sep="")
    load(est)
  
    ssb.est[j,,,i]     = Est$SB[,1:ass.num]
    ry.est[j,,,i]      = Est$Recruit[,1:ass.num]
    depl.est[j,,,i]    = Est$Bratio[,1:ass.num]
    catch.est[j,,i]    = Est$ForeCat
    ofl.est[j,,i]      = Est$OFL 
    m.est[j,,i]        = Est$M.store[1,1:ass.num]
    s.selex.est[j,,,i] = Est$S.selex[,1:ass.num]
    f.selex.est[j,,,i] = Est$F.selex[,1:ass.num]
    ssb0.est[j,,i]     = Est$SB[1,]/Est$Bratio[1,]
  } 

  #Save as an output file 
  est.all <- list()
  est.out <- paste(drive,":/PhD/Chapter3/",LH,"_est_all", sep = "")
  est.all$ssb.est  <- ssb.est
  est.all$ry.est   <- ry.est
  est.all$depl.est <- depl.est
  est.all$catch.est<- catch.est
  est.all$ofl.est  <- ofl.est
  est.all$m.est    <- m.est
  est.all$s.selex.est <- s.selex.est
  est.all$f.selex.est <- f.selex.est
  est.all$ssb0.est    <- ssb0.est
  save(est.all, file = est.out)

  
  #Save the filtered output for simulations that were not deemed overfished 
  est.split <- list()
  est.out.split <- paste(drive,":/PhD/Chapter3/",LH,"_est_split", sep = "")
  est.split$ssb.est     <- ssb.est[,,, save.index]
  est.split$ssb0.est    <- ssb0.est[,, save.index]
  est.split$ry.est      <- ry.est[,,, save.index]
  est.split$depl.est    <- depl.est[,,, save.index]
  est.split$catch.est   <- catch.est[,, save.index]
  est.split$ofl.est     <- ofl.est[,, save.index]
  est.split$m.est       <- m.est[,, save.index]
  est.split$s.selex.est <- s.selex.est[,,,save.index.ov]
  est.split$f.selex.est <- f.selex.est[,,,save.index.ov]
  est.split$ssb.est.no  <- ssb.est[,,,save.index.ov]
  est.split$ssb0.est.no <- ssb0.est[,,save.index.ov]
  est.split$ry.est.no   <- ry.est[,,,save.index.ov]
  est.split$depl.est.no <- depl.est[,,,save.index.ov]
  est.split$catch.est.no<- catch.est[,,save.index.ov]
  est.split$ofl.est.no  <- ofl.est[,,save.index.ov]
  est.split$m.est.no    <- m.est[,,save.index.ov]
  est.split$s.selex.est.no <- s.selex.est[,,,save.index.ov]
  est.split$f.selex.est.no <- f.selex.est[,,,save.index.ov]
  save(est.split, file = est.out.split)
  

  #Calculate when the stock is rebuilt but not estimated as such=============================================================================  
  failed.to.detect.over <- matrix(0, ass.num, sim.range[2])
  failed.to.detect.rec  <- matrix(0, ass.num, sim.range[2])
  incorrect.rebuild     <- matrix(0, ass.num, sim.range[2])

  for(a in sim.range[1]:sim.range[2]){
    for(b in 1:ass.num){
      temp = first.ass.yr + b*4 - 4
      ind = pre.fishery.yrs + first.ass.yr + b*4 - 4
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
  
  failed.to.detect.rec.all[j,]     = apply(failed.to.detect.rec, 1, sum)
  failed.to.detect.over.all[j,]    = apply(failed.to.detect.over, 1, sum)
  incorrect.rebuild.all[j,]        = apply(incorrect.rebuild,1, sum)

  #Calculate how many years before the correct determination was made=====================================
  yrs.declared.rec.early <-  matrix(0, ass.num, sim.range[2])
  yrs.declared.rec.late  <-  matrix(0, ass.num, sim.range[2])

  for(a in 1:sim.range[2]){
    overfished = FALSE
    true.overfished = TRUE
    for(b in 1:ass.num){
      temp = first.ass.yr + b*4 - 4
      ind = pre.fishery.yrs + first.ass.yr + b*4 - 4

      if (depl.est[j,temp,b,a] < over.thres && overfished == FALSE) { 
          overfished = TRUE 
      }

      if (depl[j,ind,a] < ctl.rule.tgt && true.overfished == TRUE) {
        if (b != 1 ){
          if (depl.est[j,temp,b,a] > ctl.rule.tgt || yrs.declared.rec.early[b-1,a] == 1){
            yrs.declared.rec.early[b,a] <- -1
          }
        }
      }

      if (depl[j,ind,a] > ctl.rule.tgt && true.overfished == TRUE) {
        true.overfished = FALSE
        if (depl.est[j,temp,b,a] < ctl.rule.tgt && overfished == TRUE){
          yrs.declared.rec.late[b,a] <- 1
        }
        if (depl.est[j,temp,b,a] > ctl.rule.tgt && overfished == FALSE){
          yrs.declared.rec.early[b,a] <- -1
        }
      }

      if (depl[j,ind,a] > ctl.rule.tgt && true.overfished == FALSE){
        if(depl.est[j,temp,b,a] < ctl.rule.tgt && overfished == TRUE){
          yrs.declared.rec.late[b,a] <- 1
        }
      }

      if(depl.est[j,temp,b,a] > ctl.rule.tgt && overfished == TRUE){
        overfished = FALSE
      }
    }
  }

  yrs.declared.rec.late.all[j,] = 4*(apply(yrs.declared.rec.late, 2, sum))
  yrs.declared.rec.early.all[j,] = 4*(apply(yrs.declared.rec.early, 2, sum))
  yrs.declared.all[j,] = yrs.declared.rec.late.all[j,] + yrs.declared.rec.early.all[j,]

  #Determine if any of the stocks fall below the overfished thresh after being "rebuilt"==================
  overfished.again = array(0, c(length(ds.list), total.yrs+1, sim.range[2]))
  for (a in 1:sim.range[2]){
    for (b in 1:ass.num){
      temp = first.ass.yr + b*4 - 4
      ind = pre.fishery.yrs + first.ass.yr + b*4 - 4
      yr.rebuilt = (depl.est[j,temp,b,a] > ctl.rule.tgt) 
      if (yr.rebuilt == TRUE) { break() }
    }
    overfished.again[j, ind:dim(depl)[2], a] = (depl[j, ind:dim(depl)[2], a] < over.thres)
  }
 

  #plot(71:221, depl[1,71:221,92], ylim = c(0,1))
  #abline(h = 0.25) ; abline(h = 0.40)
  #for (b in 1:ass.num){ lines(71:221, depl.est[1, 1:151, b, 92], col = 'red')}

  error = array(0, c(length(ds.list), total.yrs+1, sim.range[2]))
  for (a in 1:sim.range[2]){
    for (b in 1:ass.num){
      temp = first.ass.yr + b*4 - 4
      ind = pre.fishery.yrs + first.ass.yr + b*4 - 4
      error[j,b,a] = (ssb.est[j,temp,b,a] -ssb[j,ind,a]) / ssb[j,ind,a]
      #if (abs(error[j,b,a]) > 1) { print(cbind(print(ds.list[j]),a, temp, error[j,b,a])) ; break() }
    }
  }

  #AAV in Catch ==========================================================================================
  abs.catch = mapply(function(x) abs(catch.est[j,x,]-catch.est[j,x+1,]), x=(pre.fishery.yrs + first.ass.yr ):(total.yrs-1))
  sum.catch.by.sim = apply(catch.est[j,(pre.fishery.yrs + setup.yrs):(total.yrs-1),],2,sum)
  aav[j,] = 100*(apply(abs.catch,1,sum))/sum.catch.by.sim

  #Calculate medians and relative errors==================================================================
  med.ssb[j,,]    = t(apply(ssb[j,,],   1, quantile, c(0.025,0.50,0.975)))
  med.ry[j,,]     = t(apply(ry[j,1:(total.yrs - 1),],    1, quantile, c(0.025,0.50,0.975)))
  med.depl[j,,]   = t(apply(depl[j,,],  1, quantile, c(0.025,0.50,0.975)))
  med.catch[j,,]  = t(apply(catch[j,,], 1, quantile, c(0.025,0.50,0.975)))
  med.ofl[j,,]    = t(apply(ofl[j,,], 1, quantile, c(0.025,0.50,0.975)))
  med.acl[j,,]    = t(apply(acl[j,,], 1, quantile, c(0.025,0.50,0.975)))
  
  for (i in 1:length(ass.yr))
  {
     y = 1:ass.yr[i]
     med.ssb.est[j,y,i,]     = t(apply( ssb.est[j, y, i, 1:sim.range[2]], 1, quantile, c(0.025,0.50,0.975)))
     med.depl.est[j,y,i,]    = t(apply(depl.est[j, y, i, 1:sim.range[2]], 1, quantile, c(0.025,0.50,0.975)))
     y = 1:(ass.yr[i]-1) 
     med.ry.est[j,y,i,]      = t(apply(  ry.est[j, y, i, 1:sim.range[2]], 1, quantile, c(0.025,0.50,0.975)))      
  }
  
  med.m.est[j,,]         = t(apply(m.est[j,,1:sim.range[2]],         1, quantile, c(0.025,0.50,0.975)))
  med.s.selex.est[j,1,,] = t(apply(s.selex.est[j,1,,1:sim.range[2]], 1, quantile, c(0.025,0.50,0.975)))
  med.s.selex.est[j,2,,] = t(apply(s.selex.est[j,3,,1:sim.range[2]], 1, quantile, c(0.025,0.50,0.975)))
  med.f.selex.est[j,1,,] = t(apply(f.selex.est[j,1,,1:sim.range[2]], 1, quantile, c(0.025,0.50,0.975)))
  med.f.selex.est[j,2,,] = t(apply(f.selex.est[j,3,,1:sim.range[2]], 1, quantile, c(0.025,0.50,0.975)))
  med.catch.est[j,,]     = t(apply(catch.est[j,,],                   1, quantile, c(0.025,0.50,0.975), na.rm = T))
  med.ofl.est[j,,]       = t(apply(ofl.est[j,,],                     1, quantile, c(0.025,0.50,0.975), na.rm = T))

  m.est.all[j,,]         = m.est[j,,]
  s.selex.est.all[j,,,]  = s.selex.est[j,,,]
  f.selex.est.all[j,,,]  = f.selex.est[j,,,]

  for (a in 1:ass.num){
    re.depl[j,a,,] <- (depl.est[j,,a,] - depl[j,(pre.fishery.yrs + 1):total.yrs,]) / depl[j,(pre.fishery.yrs + 1):total.yrs,]
    re.ssb[j,a,,]  <- (ssb.est[j,,a,]  - ssb[j,(pre.fishery.yrs + 1):total.yrs,]) / ssb[j,(pre.fishery.yrs + 1):total.yrs,]
    re.ssb0[j,a,]  <- (ssb0.est[j,a,]  - ssb[j,1,])/ ssb[j,1,]
    re.m[j,a,]     <- (m.est[j,a,] - m) / m
  }

  ind = (pre.fishery.yrs + first.ass.yr):(total.yrs - 1)
  for (a in 1:sim.range[2]) {
    for( b in 1:length(ind)){
      if ( acl[j,ind[b],a] == 0) { acl.min[j,ind[b],a] = 0.00001}
      if ( acl[j,ind[b],a] != 0) { acl.min[j,ind[b],a] = acl[j,ind[b],a]}
    }
  }

  re.catch[j,,] <- (catch.est[j,ind,] - acl[j,ind,])/max(acl[j,ind,], 0.000001)
  #med.re.depl <- array(NA, dim = c(ds, ass.num, fishery.yrs, 3))
  #med.re.ssb  <- array(NA, dim = c(ds, ass.num, fishery.yrs, 3))
  #setup.yrs = 51
  #for(b in 1:ds){
  #  for(a in 1:ass.num){
  #    med.re.depl[b,a,1:(setup.yrs+4*a-4),] <- t(apply(flat.out[[1]]$re.depl[b,a, 1:(setup.yrs + 4*a - 4),], 1, 
  #      quantile, c(0.025, 0.50, 0.975)))
  #    med.re.ssb [b,a,1:(setup.yrs+4*a-4),] <- t(apply(flat.out[[1]]$re.ssb [b,a, 1:(setup.yrs + 4*a - 4),], 1, 
  #      quantile, c(0.025, 0.50, 0.975)))
  #  }
  #}

  for(a in 1:ass.num){
    index = ass.yr[a]
    rmse.sb0[j,a]   =  100 * sqrt((1 / sim.range[2]) * 
                      sum(((ssb0.est[j,a,] - ssb[j, 1,])^2) / (ssb[j, 1 ,]^2)))
    rmse.depl[j,a]  =  100 * sqrt((1/sim.range[2])*sum(((depl.est[j,index,a,] - depl[j,pre.fishery.yrs + index ,])^2)/
                        (depl[j,pre.fishery.yrs + index ,]^2)))
  }
  
  meds.all <- list()
  meds.out <- paste(drive,":/PhD/Chapter3/",LH,"_meds_all", sep = "")

  meds.all$med.ssb  <- med.ssb
  meds.all$med.ry   <- med.ry
  meds.all$med.depl <- med.depl
  meds.all$med.catch<- med.catch
  meds.all$med.ofl  <- med.ofl
  meds.all$med.acl  <- med.acl
  meds.all$med.ssb.est  <- med.ssb.est
  meds.all$med.ry.est   <- med.ry.est
  meds.all$med.depl.est <- med.depl.est
  meds.all$med.catch.est<- med.catch.est
  meds.all$med.ofl.est  <- med.ofl.est
  meds.all$med.m.est    <- med.m.est
  meds.all$med.s.selex.est <- med.s.selex.est
  meds.all$med.f.selex.est <- med.f.selex.est
  meds.all$m.est.all       <- m.est.all
  meds.all$s.selex.est.all <- s.selex.est.all
  meds.all$f.selex.est.all <- f.selex.est.all
  meds.all$failed.to.detect.rec.all   <- failed.to.detect.rec.all
  meds.all$failed.to.detect.over      <- failed.to.detect.over
  meds.all$incorrect.rebuild          <- incorrect.rebuild  
  meds.all$yrs.declared.rec.late.all  <- yrs.declared.rec.late.all
  meds.all$yrs.declared.rec.early.all <- yrs.declared.rec.early.all
  meds.all$yrs.declared.all           <- yrs.declared.all
  meds.all$overfished.again           <- overfished.again
  meds.all$re.depl <- re.depl
  meds.all$re.ssb  <- re.ssb
  meds.all$re.ssb0 <- re.ssb0
  meds.all$re.m    <- re.m
  meds.all$re.catch<- re.catch
  meds.all$aav     <- aav
  meds.all$rmse.depl <- rmse.depl
  meds.all$rmse.sb0  <- rmse.sb0
  save (meds.all, file = meds.out)

  
  #The medians and relative errors for the filtered reults==============================================
  index = save.index

  med.ssb.split[j,,]    = t(apply(ssb[j,,index],                  1, quantile, c(0.025,0.50,0.975)))
  med.ry.split[j,,]     = t(apply(ry[j,1:(total.yrs - 1),index],  1, quantile, c(0.025,0.50,0.975)))
  med.depl.split[j,,]   = t(apply(depl[j,,index],                 1, quantile, c(0.025,0.50,0.975)))
  med.catch.split[j,,]  = t(apply(catch[j,,index],                1, quantile, c(0.025,0.50,0.975)))
  med.ofl.split[j,,]    = t(apply(ofl[j,,index],                  1, quantile, c(0.025,0.50,0.975)))
  med.acl.split[j,,]    = t(apply(acl[j,,index],                  1, quantile, c(0.025,0.50,0.975)))
  
  for (i in 1:length(ass.yr))
  {
     y = 1:ass.yr[i]
     med.ssb.est.split[j,y,i,]     = t(apply(ssb.est[j,y,i,index],  1, quantile, c(0.025,0.50,0.975)))
     med.depl.est.split[j,y,i,]    = t(apply(depl.est[j,y,i,index], 1, quantile, c(0.025,0.50,0.975)))
     y = 1:(ass.yr[i]-1) 
     med.ry.est.split[j,y,i,]      = t(apply(ry.est[j,y,i,index],   1, quantile, c(0.025,0.50,0.975)))      
  }
  
  med.m.est.split[j,,]       = t(apply(m.est[j,,index], 1, quantile, c(0.025,0.50,0.975)))
  med.s.selex.est.split[j,1,,] = t(apply(s.selex.est[j,1,,index], 1, quantile, c(0.025,0.50,0.975)))
  med.s.selex.est.split[j,2,,] = t(apply(s.selex.est[j,3,,index], 1, quantile, c(0.025,0.50,0.975)))
  med.f.selex.est.split[j,1,,] = t(apply(f.selex.est[j,1,,index], 1, quantile, c(0.025,0.50,0.975)))
  med.f.selex.est.split[j,2,,] = t(apply(f.selex.est[j,3,,index], 1, quantile, c(0.025,0.50,0.975)))
  med.catch.est.split[j,,]     = t(apply(catch.est[j,,index],    1, quantile, c(0.025,0.50,0.975), na.rm = T))
  med.ofl.est.split[j,,]       = t(apply(ofl.est[j,,index],    1, quantile, c(0.025,0.50,0.975), na.rm = T))

  m.est.split[j,,1:sum(index)]        = m.est[j,,index]
  s.selex.est.split[j,,,1:sum(index)]  = s.selex.est[j,,,index]
  f.selex.est.split[j,,,1:sum(index)]  = f.selex.est[j,,,index]

  for (a in 1:ass.num){
    re.depl.split[j,a,,] <- (depl.est[j,,a,index] - depl[j,(pre.fishery.yrs + 1):total.yrs, index]) / depl[j,(pre.fishery.yrs + 1):total.yrs,index]
    re.ssb.split[j,a,,]  <- (ssb.est[j,,a,index]  - ssb[j,(pre.fishery.yrs + 1):total.yrs,index]) / ssb[j,(pre.fishery.yrs + 1):total.yrs,index]
    re.ssb0.split[j,a,]  <- (ssb0.est[j,a,index]  - ssb[j,1,index])/ ssb[j,1,index]
    re.m.split[j,a,]     <- (m.est[j,a,index] - m) / m
  }

  for(a in 1:ass.num){
    temp = ass.yr[a]
    rmse.sb0.split[j,a]   =  100 * sqrt((1 / sum(index)) * 
                      sum(((ssb0.est[j,a,index] - ssb[j, 1,index])^2) / (ssb[j, 1 ,index]^2)))
    rmse.depl.split[j,a]  =  100 * sqrt((1 / sum(index)) * 
                      sum(((depl.est[j, temp, a, index] - depl[j, pre.fishery.yrs + temp , index])^2)/
                      (depl[j, pre.fishery.yrs + temp , index]^2)))
  }

  yrs.declared.all.split[j,] = yrs.declared.rec.late.all[j,index] + yrs.declared.rec.early.all[j,index]

  meds.split <- list()
  meds.out.split <- paste(drive,":/PhD/Chapter3/",LH,"_meds_split", sep = "")

  meds.split$med.ssb  <- med.ssb.split
  meds.split$med.ry   <- med.ry.split
  meds.split$med.depl <- med.depl.split
  meds.split$med.catch<- med.catch.split
  meds.split$med.ofl  <- med.ofl.split
  meds.split$med.acl  <- med.acl.split
  meds.split$med.ssb.est  <- med.ssb.est.split
  meds.split$med.ry.est.   <- med.ry.est.split
  meds.split$med.depl.est <- med.depl.est.split
  meds.split$med.catch.est<- med.catch.est.split
  meds.split$med.ofl.est  <- med.ofl.est.split
  meds.split$med.m.est    <- med.m.est.split
  meds.split$med.s.selex.est <- med.s.selex.est.split
  meds.split$med.f.selex.est <- med.f.selex.est.split
  meds.split$re.depl <- re.depl.split
  meds.split$re.ssb  <- re.ssb.split
  meds.split$re.ssb0 <- re.ssb0.split
  meds.split$re.m <- re.m.split
  meds.split$rmse.sb0 <- rmse.sb0.split
  meds.split$rmse.depl<- rmse.depl.split
  #meds.split$failed.to.detect.rec.all   <- failed.to.detect.rec.all
  #meds.split$failed.to.detect.over      <- failed.to.detect.over
  #meds.split$incorrect.rebuild          <- incorrect.rebuild  
  #meds.split$yrs.declared.rec.late.all  <- yrs.declared.rec.late.all
  #meds.split$yrs.declared.rec.early.all <- yrs.declared.rec.early.all
  meds.split$yrs.declared.all           <- yrs.declared.all.split
  save (meds.split, file = meds.out.split)
  
} 
