wd = "C:/PhD/Chapter3/rockfish_ds4_sims_1_20_cv25/save/" ; par(mfrow =c (4,3), oma =c(1,1,1,1), mar = c(2,4,2,3))

#wd = "C:/PhD/Chapter3/flatfish_ds1_sims_1_100/save/" ; par(mfrow =c (4,3))
#wd = "C:/PhD/Chapter3/flatfish_ds2_sims_1_100/save/"
#wd = "C:/PhD/Chapter3/flatfish_ds3_sims_1_100/save/"
#wd = "C:/PhD/Chapter3/flatfish_ds4_sims_1_100/save/"
setwd(wd)
ass.length = 20#20 #14
start.year = 1
ss.years = 197#143#197
first.ass = 120#90#120
end.year = ss.years + 1
sims = 18
target = 0.40#0.25 
thres  = 0.25#0.08
ssb = matrix(0, end.year, sims)
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
depl = matrix(0, end.year, sims)
depl.est = array (0, dim = c(ss.years, ass.length, sims))
for (i in 1:sims){
	#if (i < 26) { temp = 25 + i }
	#if (i > 25) { temp = 50 + i }
 	est = paste("ss_ests_",i,sep="")
	dat = paste("om_proj_",i,sep ="")
	load(est); load (dat)

	depl[,i] = Proj$Depl[start.year:end.year]
	depl.est[,,i] = Est$Bratio[1:ss.years,] #Est$Bratio[1:ss.years,]
	#plot(1:102, Proj$depl[start.year:end.year], type = 'l', col = 1, lwd =2, main = i, ylim = c(0, 1.2))
	#abline(h = 0.08); abline(v = 50) ; abline(h =0.25)
	#for(j in 1:ass.length){
	#	ind = 1:(50 + j*4 -4)
	#	lines(ind, Est$Bratio[ind, j], lty = 2, col =2)
	#}
}

recruit = matrix(0, end.year, sims)
recruit.est = array (0, dim = c(ss.years, ass.length, sims))
for (i in 1:sims){
	#if (i < 26) { temp = 25 + i }
	#if (i > 25) { temp = 50 + i }
 	est = paste("ss_ests_",i,sep="")
	dat = paste("om_proj_",i,sep ="")
	load(est); load (dat)

	recruit[,i] = 2*Proj$Ry[start.year:end.year]
	recruit.est[,,i] = Est$Recruits[1:ss.years,] #Est$Recruits[1:ss.years,]
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
	med.rec.est[,a] = apply(2*recruit.est[,a,], 1, median)
}

re.depl = matrix(0, sims, ass.length)
re.ssb  = matrix(0, sims, ass.length)
re.sb0 = matrix(0, sims, ass.length)
for (a in 1:ass.length){
	ind = first.ass + 4*a - 4
	re.depl[,a] = (depl.est[ind,a,] - depl[ind,])/depl[ind,]
	re.ssb [,a] = (ssb.est[ind,a,] - ssb[ind,]) / ssb[ind,]
	re.sb0[,a] = (ssb.est[1,a,] - ssb[1,]) / ssb[1,]
}
par(mfrow=c(3,1))
boxplot(re.depl, ylim = c(-0.25, 0.25)); abline(h = 0)
boxplot(re.ssb, ylim = c(-0.25, 0.25)); abline(h = 0)
boxplot(re.sb0, ylim = c(-0.25, 0.25)); abline(h = 0)




par(mfrow =c (3,1))
plot(1:ss.years, med.depl[1:ss.years], ylim =c(0,1), type ='l', lwd =2, ylab = "Depletion")
abline (h =target) ; abline ( h =thres)
for (a in 1:ass.length){
	ind = 1:(first.ass + a*4 -4)
	if (a == ass.length) { ind = 1:(first.ass+a*4-5) }
	lines(ind, med.depl.est[ind,a], lty = 2, col =2)
}
plot(1:ss.years, med.ssb[1:ss.years], type ='l', lwd =2, ylim = c(0, 12000), ylab = "SSB")
for (a in 1:ass.length){
	ind = 1:(first.ass + a*4 -4)
	if (a == ass.length) { ind = 1:(first.ass+a*4-5) }
	lines(ind, med.ssb.est[ind,a], lty = 2, col =2)
}
plot(1:ss.years, med.rec[1:ss.years], type ='l', lwd =2, ylim = c(0, 8000), ylab = "Recruits")
for (a in 1:ass.length){
	ind = 1:(first.ass + a*4 -4)
	if (a == ass.length) { ind = 1:(50+a*4-5) }
	lines(ind, med.rec.est[ind,a], lty = 2, col =2)
}



M.mat = matrix(NA, ass.length, sims)
k.mat = matrix(NA, ass.length, sims)
lmin.mat = matrix(NA, ass.length, sims)
lmax.mat = matrix(NA, ass.length, sims)
f.selex = array(NA, dim = c(2, ass.length, sims))
s.selex = array(NA, dim = c(2, ass.length, sims))
f.adj   = matrix(NA, ass.length, sims)
for (i in 1:sims){
	#if (i < 26) { temp = 25 + i }
	#if (i > 25) { temp = 50 + i }
	est = paste("ss_ests_",i,sep="")
	load(est)
 	M.mat[,i] = Est$M.store[1,]
 	k.mat[,i] = Est$k.store
 	lmin.mat[,i] = Est$Lmin.store
 	lmax.mat[,i] = Est$Lmax.store
 	f.selex[,,i] = Est$F.selex[c(1,3),]
 	s.selex[,,i] = Est$S.selex[c(1,3),]
 	#f.adj[,i] = Est$F.selex.1.adj
}

#par(mfrow=c(2,2))
boxplot(t(M.mat), ylim = c(0.13, 0.25), ylab = "M") ; abline (h = 0.15, col =2)
#boxplot(t(k.mat), ylim = c(0.10, 0.20)) ; abline (h = 0.143779 , col =2)
boxplot(t(lmin.mat), ylim = c(15, 35), ylab = "Lmin") ; abline (h = 24.6219, col =2)
boxplot(t(lmax.mat), ylim = c(45, 65), ylab = "Lmax") ; abline (h = 55.4099, col =2)
boxplot(t(f.selex[1,,]), ylim = c(40, 47), ylab = "F Peak Select") ; abline (h = 43, col =2)
boxplot(t(f.selex[2,,]), ylim = c(4, 5), ylab = "F Slope Select") ; abline (h = 4.25, col =2)
boxplot(t(f.adj), ylim = c(43, 53), ylab = "F Peak Adj Select"); abline (h = 48, col = 2)
boxplot(t(s.selex[1,,]), ylim = c(30, 37), ylab = "S Peak Select") ; abline (h = 33, col =2)
boxplot(t(s.selex[2,,]), ylim = c(4, 5), ylab = "S Slope Select") ; abline (h = 4.25, col =2)


#par(mfrow=c(2,2))
boxplot(t(M.mat), ylim = c(0.04, 0.12), ylab = "M") ; abline (h = 0.08, col =2)
#boxplot(t(k.mat), ylim = c(0, 0.10), ylab = "Lmin") ; abline (h = 0.05 , col =2)
boxplot(t(lmin.mat), ylim = c(10, 20), ylab = "Lmin") ; abline (h = 18, col =2)
boxplot(t(lmax.mat), ylim = c(60, 70), ylab = "Lmax") ; abline (h = 64, col =2)
boxplot(t(f.selex[1,,]), ylim = c(42, 48), ylab = "F Peak Select") ; abline (h = 45, col =2)
boxplot(t(f.selex[2,,]), ylim = c(4, 5), ylab = "F Slope Select") ; abline (h = 4.25, col =2)
boxplot(t(f.adj), ylim = c(45, 55), ylab = "F Peak Adj Select"); abline (h = 50, col = 2)
boxplot(t(s.selex[1,,]), ylim = c(36, 43), ylab = "S Peak Select") ; abline (h = 39, col =2)
boxplot(t(s.selex[2,,]), ylim = c(4, 5), ylab = "S Slope Select") ; abline (h = 4.25, col =2)




re.m = (M.mat - 0.08)/ 0.08
boxplot(t(re.m), ylim = c(-0.1, 0.10)); abline (h = 0)