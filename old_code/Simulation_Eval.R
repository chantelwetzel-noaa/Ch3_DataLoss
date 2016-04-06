############################################
#   Chapter 3: Loss of Data & Rebuilding   #
#Load in the files from the Operating Model#
#and the estimation model.  Calculates RE  #
#and creates plots to evaluate performance.#
#    Created May 17, 2014 by               #
#         Chantel Wetzel                   #
############################################

to.load = TRUE
drive = "F"
LH = "rockfish"
ds.list = c("greatall", "normal_estM", "greathist")
sim.range = c(1,40)

#Dimensions by life-history
source(paste(drive,":/PhD/Chapter3/code/LH_parameter_values.R",sep=""))
pre.fishery.yrs <- ages - 1 
setup.yrs   <- 50
project.yrs <- 100
fishery.yrs <- setup.yrs + project.yrs + 1
total.yrs   <- pre.fishery.yrs + fishery.yrs
ass.num     <- (project.yrs / 4) + 1
ass.yr = seq(setup.yrs + 1, fishery.yrs, 4)

if(to.load == TRUE) {

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
    med.m.est       = array(0, dim = c(length(ds.list), 2, ass.num, 3))
    med.s.selex.est = array(0, dim = c(length(ds.list), 2, ass.num, 3))
    med.f.selex.est = array(0, dim = c(length(ds.list), 2, ass.num, 3))

    m.est.all       = array(0, dim = c(length(ds.list), 1, ass.num, sim.range[2]))
    s.selex.est.all = array(0, dim = c(length(ds.list), 6, ass.num, sim.range[2]))
    f.selex.est.all = array(0, dim = c(length(ds.list), 6, ass.num, sim.range[2]))

  for (j in 1:length(ds.list))
  {
    data.scenario = ds.list[j]
    dir = paste(drive,":/PhD/Chapter3/",LH,"_",data.scenario,"_sims_",sim.range[1],"_",sim.range[2],
          "/save/", sep = "")
    
  
    #Load up the operating model values
    ssb   = matrix(0, total.yrs+1, sim.range[2])
    ry    = matrix(0, total.yrs+1, sim.range[2])
    depl  = matrix(0, total.yrs+1, sim.range[2])
    catch = matrix(0, total.yrs,   sim.range[2])
    ofl   = matrix(0, total.yrs,   sim.range[2])
    acl   = matrix(0, total.yrs,   sim.range[2])
    f.lens= matrix(0, total.yrs - pre.fishery.yrs, sim.range[2])
    s.lens= matrix(0, total.yrs - pre.fishery.yrs,   sim.range[2])
    f.ages= matrix(0, total.yrs - pre.fishery.yrs,   sim.range[2])
    s.ages= matrix(0, total.yrs - pre.fishery.yrs,   sim.range[2])
    
    for (i in sim.range[1]:sim.range[2]) {
        dat = paste(dir, "om_proj_",i,sep ="")
        load(dat)
        ssb[,i]   = Proj$SSB
        ry[,i]    = Proj$Ry*2
        depl[,i]  = Proj$depl
        catch[,i] = Proj$fore.catch[1:total.yrs]
        ofl[,i]   = Proj$true.ofl
        acl[,i]   = ofl[,i]*exp(qnorm(0.45, 0, 0.36))
        f.lens[,i]= Proj$f.len.samp
        s.lens[,i]= Proj$s.len.sam
        f.ages[,i]= Proj$f.age.samp
        s.ages[,i]= Proj$s.age.samp
    }
  
    #Save as an output file 
    om.all <- list()
    om.out <- paste(drive,":/PhD/Chapter3/",LH,"_",data.scenario,"_sims_",sim.range[1],"_",sim.range[2],
          "/save/om_all", sep = "")
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
  
    #Load in the estimation values================================================================
    ssb.est     =  array(0, dim = c(fishery.yrs, ass.num, sim.range[2]))
    ry.est      =  array(0, dim = c(fishery.yrs, ass.num, sim.range[2]))
    depl.est    =  array(0, dim = c(fishery.yrs, ass.num, sim.range[2]))
    m.est       =  array(0, dim = c(2,           ass.num, sim.range[2]))
    s.selex.est = array(0,dim = c(6,           ass.num, sim.range[2]))
    f.selex.est = array(0,dim = c(6,           ass.num, sim.range[2]))
    catch.est   = matrix(0, (total.yrs+4),   sim.range[2])
    ofl.est     = matrix(0, (total.yrs+4),   sim.range[2])
      
    for (i in sim.range[1]:sim.range[2]){
      est = paste(dir,"ss_ests_",i,sep="")
      load(est)
    
      ssb.est[,,i]     = Est$SB[,1:ass.num]
      ry.est[,,i]      = Est$Recruit[,1:ass.num]
      depl.est[,,i]    = Est$Bratio[,1:ass.num]
      catch.est[,i]    = Est$ForeCat
      ofl.est[,i]      = Est$OFL 
      m.est[,,i]       = Est$M.store[,1:ass.num]
      s.selex.est[,,i] = Est$S.selex[,1:ass.num]
      f.selex.est[,,i] = Est$F.selex[,1:ass.num]
    } 

    #Save as an output file 
    est.all <- list()
    est.out <- paste(drive,":/PhD/Chapter3/",LH,"_",data.scenario,"_sims_",sim.range[1],"_",sim.range[2],
          "/save/est_all", sep = "")
    est.all$ssb.est  <- ssb.est
    est.all$ry.est   <- ry.est
    est.all$depl.est <- depl.est
    est.all$catch.est<- catch.est
    est.all$ofl.est  <- ofl.est
    est.all$m.est    <- m.est
    est.all$s.selex.est <- s.selex.est
    est.all$f.selex.est <- f.selex.est
    save(est.all, file = est.out)

    #Calculate medians and relative errors====================================================
    med.ssb[j,,]    = t(apply(ssb,   1, quantile, c(0.025,0.50,0.975)))
    med.ry[j,,]     = t(apply(ry[1:(total.yrs - 1),],    1, quantile, c(0.025,0.50,0.975)))
    med.depl[j,,]   = t(apply(depl,  1, quantile, c(0.025,0.50,0.975)))
    med.catch[j,,]  = t(apply(catch, 1, quantile, c(0.025,0.50,0.975)))
    med.ofl[j,,]    = t(apply(ofl, 1, quantile, c(0.025,0.50,0.975)))
    med.acl[j,,]    = t(apply(acl, 1, quantile, c(0.025,0.50,0.975)))
    
    for (i in 1:length(ass.yr))
    {
       y = 1:ass.yr[i]
       med.ssb.est[j,y,i,]     = t(apply(ssb.est[y,i,1:sim.range[2]],       1, quantile, c(0.025,0.50,0.975)))
       med.depl.est[j,y,i,]    = t(apply(depl.est[y,i,1:sim.range[2]],      1, quantile, c(0.025,0.50,0.975)))
       y = 1:(ass.yr[i]-1) 
       med.ry.est[j,y,i,]      = t(apply(ry.est[y,i,1:sim.range[2]],   1, quantile, c(0.025,0.50,0.975)))      
    }
    
    med.m.est[j,1,,]       = t(apply(m.est[1,,1:sim.range[2]], 1, quantile, c(0.025,0.50,0.975)))
    med.m.est[j,2,,]       = t(apply(m.est[2,,1:sim.range[2]], 1, quantile, c(0.025,0.50,0.975)))
    med.s.selex.est[j,1,,] = t(apply(s.selex.est[1,,1:sim.range[2]], 1, quantile, c(0.025,0.50,0.975)))
    med.s.selex.est[j,2,,] = t(apply(s.selex.est[3,,1:sim.range[2]], 1, quantile, c(0.025,0.50,0.975)))
    med.f.selex.est[j,1,,] = t(apply(f.selex.est[1,,1:sim.range[2]], 1, quantile, c(0.025,0.50,0.975)))
    med.f.selex.est[j,2,,] = t(apply(f.selex.est[3,,1:sim.range[2]], 1, quantile, c(0.025,0.50,0.975)))
    med.catch.est[j,,]     = t(apply(catch.est,    1, quantile, c(0.025,0.50,0.975), na.rm = T))
    med.ofl.est[j,,]       = t(apply(ofl.est,    1, quantile, c(0.025,0.50,0.975), na.rm = T))

    m.est.all[j,,,]        = m.est[1,,]
    s.selex.est.all[j,,,]  = s.selex.est
    f.selex.est.all[j,,,]  = f.selex.est

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
    save (meds.all, file = meds.out)
  } 
}

if(to.load == FALSE) {
  load(paste(drive,":/PhD/Chapter3/rockfish_meds_all", sep = ""))
  rock <- meds.all
  load(paste(drive,":/PhD/Chapter3/flatfish_meds_all", sep = ""))
  flat <- meds.all
  out <- list()
  out[[1]] <- rock
  out[[2]] <- flat
}


#Create Plots to compare results===========================================================

a = 2
par(mfrow= c(3,1))
for(b in 1:3){
  #for (a in 1:2){
    #plot(1:fishery.yrs,  out[[a]]$med.ssb[b,(pre.fishery.yrs+1):total.yrs,2], col = 1, type = 'l', lwd =2, ylim = c(0,max(meds.all$med.ssb)))
    plot(1:100,  out[[a]]$med.ssb[b,(pre.fishery.yrs+1):140,2], col = 1, type = 'l', lwd =2, ylim = c(0,max(meds.all$med.ssb)))
    lines(1:fishery.yrs, out[[a]]$med.ssb[b,(pre.fishery.yrs+1):total.yrs,1], col = 1, lty = 2,    lwd =2)
    lines(1:fishery.yrs, out[[a]]$med.ssb[b,(pre.fishery.yrs+1):total.yrs,3], col = 1, lty = 2,    lwd =2)
    for (i in 1:ass.num)
    {
      y = 1:ass.yr[i]
      lines(y, out[[a]]$med.ssb.est[b,y,i,2], col = 'red', lty = 1, lwd =1)
    }
  #}
}

par(mfrow= c(3,1))
for(b in 1:3){
    plot(1:fishery.yrs, out[[a]]$med.depl[b,(pre.fishery.yrs+1):total.yrs,2], col = 1, type = 'l', lwd =2, ylim = c(0,1),
        ylab = "Depletion", xlab = "Year")
    lines(1:fishery.yrs, out[[a]]$med.depl[b,(pre.fishery.yrs+1):total.yrs,1], col = 1, lty = 2,    lwd =2)
    lines(1:fishery.yrs, out[[a]]$med.depl[b,(pre.fishery.yrs+1):total.yrs,3], col = 1, lty = 2,    lwd =2)
    abline(h = 0.40, col = 'blue',lty = 3)
    for (i in 1:ass.num)
    {
      y = 1:ass.yr[i]
      lines(y, out[[a]]$med.depl.est[b,y,i,2], col = 'red', lty = 1, lwd =1)
    }
}

for(b in 1:3){
    par(mfrow= c(3,1))
    plot(1:(fishery.yrs-1),  out[[a]]$med.ry[b,(pre.fishery.yrs+1):(total.yrs-1),2], col = 1, type = 'l', lwd =2, ylim = c(1000,6000),
        ylab = "Recruits", xlab = "Year")
    lines(1:(fishery.yrs-1), out[[a]]$med.ry[b,(pre.fishery.yrs+1):(total.yrs - 1),1], col = 1, lty = 2,    lwd =2)
    lines(1:(fishery.yrs-1), out[[a]]$med.ry[b,(pre.fishery.yrs+1):(total.yrs - 1),3], col = 1, lty = 2,    lwd =2)
    abline(h = 0.40, col = 'blue',lty = 3)
    for (i in 1:ass.num)
    {
      y = 1:(ass.yr[i]-1)
      lines(y, out[[a]]$med.ry.est[b,y,i,2], col = 'red', lty = 1, lwd =1)
    }
}

for(b in 1:3){
    par(mfrow = c(3,1))
    boxplot(t(out[[a]]$m.est[b,1,,]), ylim = c(m-0.20*m, m + 0.20*m))
    abline(h = m,col = 1, lty =2)
}

for(b in 1:3){    
    par(mfrow = c(3,2))
    boxplot(t(out[[a]]$med.s.selex.est[b,1,,]), ylim = c(ssp1 - 0.20*ssp1, ssp1 + 0.20*ssp1))
    abline(h = ssp1,col = 1, lty =2)
    boxplot(t(out[[a]]$med.s.selex.est[b,2,,]), ylim = c(ssp3 - 0.20*ssp3, ssp3 + 0.20*ssp3))
    abline(h = ssp3, col = 1, lty =2)
    
    par(mfrow = c(3,2))
    boxplot(t(out[[a]]$med.f.selex.est[b,1,,]), ylim = c(fsp1 - 0.20*fsp1, fsp1 + 0.20*fsp1))
    abline(h = fsp1,col = 1, lty =2)
    boxplot(t(out[[a]]$med.f.selex.est[b,2,,]), ylim = c(fsp3 - 0.20*fsp3, fsp3 + 0.20*fsp3))
    abline(h = fsp3,col = 1, lty =2)
}






#This plot needs to be fixed to use the buffered OFL values from the true population
par(mfrow = c(1,1))
plot(1:project.yrs,  med.ofl[(pre.fishery.yrs+1+50):(total.yrs-1),2], col = 1, type = 'l', lwd =2, ylim = c(0,max(med.catch)),
    ylab = "ABC", xlab = "Year")
lines(1:project.yrs, med.ofl[(pre.fishery.yrs+1+50):(total.yrs-1),1], col = 1, lty = 2,    lwd =2)
lines(1:project.yrs, med.ofl[(pre.fishery.yrs+1+50):(total.yrs-1),3], col = 1, lty = 2,    lwd =2)
ind = (pre.fishery.yrs+1+50):(total.yrs-1)
lines(1:project.yrs, med.catch.est[ind,2], col = 'red', lty = 1, lwd =1)

par(mfrow = c(1,1))
plot(1:project.yrs,  med.ofl[(pre.fishery.yrs+1+50):(total.yrs-1),2], col = 1, type = 'l', lwd =2, ylim = c(0,max(med.catch)),
    ylab = "OFL", xlab = "Year")
lines(1:project.yrs, med.ofl[(pre.fishery.yrs+1+50):(total.yrs-1),1], col = 1, lty = 2,    lwd =2)
lines(1:project.yrs, med.ofl[(pre.fishery.yrs+1+50):(total.yrs-1),3], col = 1, lty = 2,    lwd =2)
ind = (pre.fishery.yrs+1+50):(total.yrs-1)
lines(1:project.yrs, med.ofl.est[ind,2], col = 'red', lty = 1, lwd =1)

#Calculate when the stock is rebuilt but not estimated as such==============================================================================
failed.to.detect.over <-  matrix(0, ass.num, sim.range[2])
failed.to.detect.rec  <-  matrix(0, ass.num, sim.range[2])
incorrect.rebuild <- matrix(0, ass.num, sim.range[2])

for(a in sim.range[1]:sim.range[2]){
  for(b in 1:ass.num){
    temp = setup.yrs + b*4 - 3
    ind = pre.fishery.yrs + setup.yrs + b*4 - 3
    past.state = FALSE
    if (b == 1) {
      if (depl.est[temp,b,a] > over.thres){
        failed.to.detect.over[b,a] <- 1
        overfished = FALSE
      }
      if (depl.est[temp,b,a] < over.thres){
        failed.to.detect.over[b,a] <- 0
        overfished = TRUE
      }
    }
  
    if (b > 1 && depl.est[temp,b,a] > over.thres && depl[ind,a] < over.thres){
        failed.to.detect.over[b,a] <- 1
        overfished = FALSE
    }
    
    if (overfished == FALSE) {
      if(depl.est[temp,b,a] > over.thres && depl[ind,a] > over.thres){
        overfished = FALSE
      }
      if(depl.est[temp,b,a] < over.thres && depl[ind,a] < over.thres){
        overfished = TRUE
      }
    }

    if(overfished == TRUE){
      if(depl.est[temp,b,a] < ctl.rule.tgt && depl[ind,a] < ctl.rule.tgt){
        overfished = TRUE
        if(b > 1 && failed.to.detect.rec[b-1,a] == 1){
          failed.to.detect.rec[b,a] = 1
        }
      }
      if(b > 1 && depl.est[temp,b,a] < ctl.rule.tgt && depl[ind,a] < ctl.rule.tgt && incorrect.rebuild[b-1,a] == 1){
        incorrect.rebuild[b,a] = 1
        overfished = TRUE
      }
      if(depl.est[temp,b,a] < ctl.rule.tgt && depl[ind,a] > ctl.rule.tgt){
        failed.to.detect.rec[b,a] <- 1
        overfished = TRUE
      }
      if(b > 1 && depl.est[temp,b,a] < ctl.rule.tgt && depl[ind,a] > ctl.rule.tgt && failed.to.detect.rec[b-1,a] == 1){
        failed.to.detect.rec[b,a] <- 1
        overfished = TRUE
      }
      if(b > 1 && depl.est[temp,b,a] < ctl.rule.tgt && depl[ind,a] > ctl.rule.tgt && incorrect.rebuild[b-1,a] == 1){
        failed.to.detect.rec[b,a] <- 0
        incorrect.rebuild[b,a] <- 0
        overfished = FALSE
      }

      if(depl.est[temp,b,a] > ctl.rule.tgt && depl[ind,a] < ctl.rule.tgt){
        incorrect.rebuild[b,a] <- 1
        overfished = TRUE
      }
      if(depl.est[temp,b,a] > ctl.rule.tgt && depl[ind,a] > ctl.rule.tgt){
        overfished = FALSE
      }
    }

    if(overfished == FALSE && past.state == TRUE){
      if(depl.est[temp,b,a] > ctl.rule.tgt && depl[ind,a] < ctl.rule.tgt){
        incorrect.rebuild[b,a] <- 1
      }
      if(depl.est[temp,b,a] > ctl.rule.tgt && depl[ind,a] > ctl.rule.tgt){
        incorrect.rebuild[b,a] <- 0
      }
    }

    past.state = overfished
  }
}

temp1 = apply(failed.to.detect.rec, 1, sum)
temp2 = apply(failed.to.detect.over, 1, sum)
temp3 = apply(incorrect.rebuild,1, sum)
plot(1:ass.num, temp1, ylab="Failed to Detect", type = 'b', col = "red", ylim = c(0,25))
points(1:ass.num, temp1, pch = 16, col = 'red')
points(1:ass.num, temp2, pch = 16, col = "blue")
lines (1:ass.num, temp2, lty = 1,  col = "blue")
points(1:ass.num, temp3, pch = 16, col = "green")
lines (1:ass.num, temp3, lty = 1,  col = "green")

#Calculate the Relative Errors ==============================================================================================================
re.depl <- array(NA, dim = c(ass.num, fishery.yrs, sim.range[2]))
re.ssb  <- array(NA, dim = c(ass.num, fishery.yrs, sim.range[2]))
re.m    <- matrix(NA, ass.num, sim.range[2])

for (a in 1:ass.num){
  re.depl[a,,] <- (depl.est[,a,] - depl[(pre.fishery.yrs + 1):total.yrs,]) / depl[(pre.fishery.yrs + 1):total.yrs,]
  re.ssb[a,,]  <- (ssb.est[,a,] - ssb[(pre.fishery.yrs + 1):total.yrs,]) / ssb[(pre.fishery.yrs + 1):total.yrs,]
  re.m[a,]     <- (m.est[1,a,] - m) / m
}

par(mfrow=c(3,2))
for (a in 1:ass.num){
  boxplot(t(re.depl[a,,]), main = a, xlab= "RE depl", ylim = c(-0.25, 0.5))
  abline(h = 0, lty = 2, col = 'red')
  boxplot(t(re.ssb[a,,]), main = a, xlab= "RE ssb", ylim = c(-0.25, 0.5))
  abline(h = 0, lty = 2, col = 'red')
}

boxplot(t(re.m), ylab = "RE M")
abline(h = 0, lty = 2, col = 'red')

med.re.depl <- array(NA, dim = c(ass.num, fishery.yrs, 3))
med.re.ssb  <- array(NA, dim = c(ass.num, fishery.yrs, 3))

for(a in 1:ass.num){
  med.re.depl[a,1:(setup.yrs+4*a-4),] <- t(apply(re.depl[a, 1:(setup.yrs + 4*a - 4),], 1, quantile, c(0.025, 0.50, 0.975)))
  med.re.ssb [a,1:(setup.yrs+4*a-4),] <- t(apply(re.ssb [a, 1:(setup.yrs + 4*a - 4),], 1, quantile, c(0.025, 0.50, 0.975)))
}