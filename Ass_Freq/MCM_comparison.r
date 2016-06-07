
mcmc = read.table("C:/Users/xwetzelch/Desktop/Sim6_MCMC/derived_posteriors.sso",header =T)
load("F:/PhD/Chapter3/rockfish_greatall_sims_1_25_AgeErr_Prop/save/om_proj_6")
load("F:/PhD/Chapter3/rockfish_greatall_sims_1_25_AgeErr_Prop/save/ss_ests_6")

Depl.mc = mcmc[,623:772]
SSB.mc  = mcmc[,5:155]

med.depl = apply(Depl.mc, 2, quantile, c(0.025, 0.50, 0.975))
med.ssb  = apply(SSB.mc, 2, quantile, c(0.024, 0.50, 0.975))

depl = Proj$depl
ssb  = Proj$SSB

est.depl = Est$Bratio
est.ssb  = Est$SB


par(mfrow = c(2,1))
plot(1:151, ssb[71:221], ylim = c(0, 15000), type = 'b')
lines(1:151, med.ssb[2,], col = 2, lty = 1, lwd =2)
lines(1:151, med.ssb[1,], col = 2, lty = 2, lwd =2)
lines(1:151, med.ssb[3,], col = 2, lty = 2, lwd =2)
lines(1:151, est.ssb[1:151,26], col = 3, lty = 1, lwd =2)


plot(1:151, depl[71:221], ylim = c(0, 1), type = 'b')
lines(1:151, c(1, med.depl[2,]), col = 2, lty = 1, lwd =2)
lines(1:151, c(1, med.depl[1,]), col = 2, lty = 2, lwd =2)
lines(1:151, c(1, med.depl[3,]), col = 2, lty = 2, lwd =2)
lines(1:151, est.depl[1:151,26], col = 3, lty = 1, lwd =2)

legend("topright", legend = c("OM","MCMC","MLE"), bty = 'n', col = c(1,2,3), lwd = c(2,2,2), lty = c(1,1,1))
