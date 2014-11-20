sim = 1
pre.fishery.yrs <- ages - 1 
setup.yrs   <- 49
first.ass.yr <- 50
project.yrs <- 50
project.yrs = 52 
fishery.yrs <- setup.yrs + project.yrs + 2
total.yrs   <- pre.fishery.yrs + fishery.yrs + 1

par(mfrow= c(4,2))
for (sim in 1:2)
{

plot(1:ss.years,  ssb[1:ss.years,sim], col = 1, type = 'l', lwd =2, main = sim)
for (i in 1:ass.length)
{
  y = ind = 1:(50 + a*4 -4)
  lines(y, ssb.est[y,i,sim], col = 'red', lty = 1, lwd =1)
}



plot(1:ss.years, depl[1:ss.years,sim], col = 1, type = 'l', lwd =2, ylim = c(0,1),
    ylab = "Depletion", xlab = "Year")
abline(h = 0.40, col = 'blue',lty = 3)
for (i in 1:ass.length)
{
  ind = 1:(50 + a*4 -4)
  lines(y, depl.est[y,i,sim], col = 'red', lty = 1, lwd =1)
}
}


boxplot(t(m.est[1,,]), ylim = c(0.06, 0.10), main = "Female M")
abline(h = 0.08,col = 1, lty =2)
boxplot(t(m.est[2,,]), ylim = c(0.06, 0.10), main = "Male M")
abline(h = 0.08,col = 1, lty =2)

boxplot(t(s.selex.est[1,,]),ylim =c ( ssp1 -2, ssp1 +10), main = "Survey")
abline(h = ssp1 ,col = 1, lty =2)
boxplot(t(s.selex.est[3,,]), ylim = c(ssp3 -2, ssp3+2))
abline(h = ssp3 ,col = 1, lty =2)

boxplot(t(f.selex.est[1,,]), ylim= c(fsp1-5,fsp1+5), main = "Fishery")
abline(h = fsp1 ,col = 1, lty =2)
boxplot(t(f.selex.est[3,,]), ylim = c(fsp3-2,fsp3 +2))
abline(h = fsp3 ,col = 1, lty =2)


}

cbind(depl[71:221,1],depl.est[,26,1])
cbind(ssb[71:221,1],ssb.est[,26,1])

par(mfrow=c(1,1))
plot(1:151, Ry[71:221]*2,type ='l',col=1,lwd=2,ylim=c(0,5000))
lines(1:151,Recruits[1:151,26],col = 'red')


library(r4ss)
update_r4ss_files(save=FALSE,local="C:/Program Files/R/R-3.1.0/library/r4ss_update_May132014")
wd = "F:/PhD/Chapter3/Compare/Determ_short/"

determ1 = SS_output(paste(wd, "1", sep =""), covar = F)
determ2 = SS_output(paste(wd, "2", sep =""), covar = F)
determ3 = SS_output(paste(wd, "3", sep =""), covar = F)

wd = "F:/PhD/Chapter3/Compare/Stoch_short/"
stoch1 = SS_output(paste(wd, "1", sep =""), covar = F)
stoch2 = SS_output(paste(wd, "2", sep =""), covar = F)
stoch3 = SS_output(paste(wd, "3", sep =""), covar = F)
stoch4 = SS_output(paste(wd, "4", sep =""), covar = F)
stoch5 = SS_output(paste(wd, "5", sep =""), covar = F)

modelnames <- c("Determ", "Determ-M", "Determ-All", "Stoch", "Stoch-Auto", "Stoch-M", "Stoch-All", "Stoch-CV")
mysummary <- SSsummarize(list(determ1,determ2, determ3, stoch1, stoch2, stoch3, stoch4, stoch5))
SSplotComparisons(mysummary, legendlabels=modelnames,plot=T)