d = "C:/PhD/Chapter3/output/flatfish_g_sims_1_40/save/" ; par(mfrow =c (3,3))
#wd = "C:/PhD/Chapter3/output/flatfish_normal_estM_sims_1_40/save/"
#wd = "C:/PhD/Chapter3/output/flatfish_greathist_sims_1_40/save/"
setwd(wd)
ass.length = 26#14
start.year = 71#41
ss.years = 150#102
end.year = start.year + ss.years - 1
sims = 40
target = 0.40#0.25 
thres  = 0.25#0.08
ssb = matrix(0, ss.years, sims)
ssb.est = array (0, dim = c(ss.years, ass.length, sims))
#par(mfrow=c(2,2))
for (i in 1:sims){
	#if (i < 26) { temp = 25 + i }
	#if (i > 25) { temp = 50 + i }
 	est = paste("ss_ests_",i,sep="")
	dat = paste("om_proj_",i,sep ="")
	load(est); load (dat)
	ssb[,i] = Proj$SSB[start.year:end.year]
	ssb.est[,,i] = Est$SB[1:ss.years ,]
	#plot(1:102, Proj$SSB[start.year:end.year], type = 'l', col = 1, lwd =2, main = i)
	#for(j in 1:ass.length){
	#	ind = 1:(50 + j*4 -4)
	#	lines(ind, Est$SB[ind, j], lty = 2, col =2)
	#}
}

#par(mfrow=c(2,2))
depl = matrix(0, ss.years, sims)
depl.est = array (0, dim = c(ss.years, ass.length, sims))
for (i in 1:sims){
	#if (i < 26) { temp = 25 + i }
	#if (i > 25) { temp = 50 + i }
 	est = paste("ss_ests_",i,sep="")
	dat = paste("om_proj_",i,sep ="")
	load(est); load (dat)

	depl[,i] = Proj$depl[start.year:end.year]
	depl.est[,,i] = Est$Bratio[1:ss.years,]
	#plot(1:102, Proj$depl[start.year:end.year], type = 'l', col = 1, lwd =2, main = i, ylim = c(0, 1.2))
	#abline(h = 0.08); abline(v = 50) ; abline(h =0.25)
	#for(j in 1:ass.length){
	#	ind = 1:(50 + j*4 -4)
	#	lines(ind, Est$Bratio[ind, j], lty = 2, col =2)
	#}
}

recruit = matrix(0, ss.years, sims)
recruit.est = array (0, dim = c(ss.years, ass.length, sims))
for (i in 1:sims){
	#if (i < 26) { temp = 25 + i }
	#if (i > 25) { temp = 50 + i }
 	est = paste("ss_ests_",i,sep="")
	dat = paste("om_proj_",i,sep ="")
	load(est); load (dat)

	recruit[,i] = 2*Proj$Ry[start.year:end.year]
	recruit.est[,,i] = Est$Recruits[1:ss.years,]
	#plot(1:102, Proj$depl[start.year:end.year], type = 'l', col = 1, lwd =2, main = i, ylim = c(0, 1.2))
	#abline(h = 0.08); abline(v = 50) ; abline(h =0.25)
	#for(j in 1:ass.length){
	#	ind = 1:(50 + j*4 -4)
	#	lines(ind, Est$Bratio[ind, j], lty = 2, col =2)
	#}
}

med.depl = apply(depl, 1, median)
med.ssb = apply(ssb, 1, median)
med.rec = apply(recruit, 1, median)
med.depl.est = matrix(0, ss.years, ass.length)
med.ssb.est = matrix(0, ss.years, ass.length)
med.rec.est = matrix(0, ss.years, ass.length)
for (a in 1:ass.length){
	med.depl.est[,a] = apply(depl.est[,a,], 1, median)
	med.ssb.est[,a] = apply(ssb.est[,a,], 1, median)
	med.rec.est[,a] = apply(recruit.est[,a,], 1, median)
}

#par(mfrow =c (3,3))
plot(1:ss.years, med.depl[1:ss.years], ylim =c(0,1), type ='l', lwd =2)
abline (h =target) ; abline ( h =thres)
for (a in 1:ass.length){
	ind = 1:(50 + a*4 -4)
	lines(ind, med.depl.est[ind,a], lty = 2, col =2)
}
plot(1:ss.years, med.ssb[1:ss.years], type ='l', lwd =2, ylim = c(0, 8000))
for (a in 1:ass.length){
	ind = 1:(50 + a*4 -4)
	lines(ind, med.ssb.est[ind,a], lty = 2, col =2)
}
plot(1:ss.years, med.rec[1:ss.years], type ='l', lwd =2, ylim = c(0, 8000))
for (a in 1:ass.length){
	ind = 1:(50 + a*4 -4)
	lines(ind, med.rec.est[ind,a], lty = 2, col =2)
}



M.mat = matrix(NA, ass.length, sims)
k.mat = matrix(NA, ass.length, sims)
lmin.mat = matrix(NA, ass.length, sims)
lmax.mat = matrix(NA, ass.length, sims)
for (i in 1:sims){
	#if (i < 26) { temp = 25 + i }
	#if (i > 25) { temp = 50 + i }
	est = paste("ss_ests_",i,sep="")
	load(est)
 	M.mat[,i] = Est$M.store[1,]
 	k.mat[,i] = Est$k.store
 	lmin.mat[,i] = Est$Lmin.store
 	lmax.mat[,i] = Est$Lmax.store
}

par(mfrow=c(2,2))
boxplot(t(M.mat), ylim = c(0.13, 0.25)) ; abline (h = 0.15, col =2)
boxplot(t(k.mat), ylim = c(0.10, 0.20)) ; abline (h = 0.143779 , col =2)
boxplot(t(lmin.mat), ylim = c(15, 35)) ; abline (h = 24.6219, col =2)
boxplot(t(lmax.mat), ylim = c(45, 65)) ; abline (h = 55.4099, col =2)




par(mfrow=c(2,2))
boxplot(t(M.mat), ylim = c(0.04, 0.12)) ; abline (h = 0.08, col =2)
boxplot(t(k.mat), ylim = c(0, 0.10)) ; abline (h = 0.05 , col =2)
boxplot(t(lmin.mat), ylim = c(10, 20)) ; abline (h = 18, col =2)
boxplot(t(lmax.mat), ylim = c(60, 70)) ; abline (h = 64, col =2)



re.m = (M.mat - 0.15)/ 0.15
boxplot(t(re.m), ylim = c(-0.25, 0.50)); abline (h = 0)