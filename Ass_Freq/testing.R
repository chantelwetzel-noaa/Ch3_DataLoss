LH= "rockfish"
hist.len = "50_yr"
#wd = paste("C:/PhD/Chapter3/",LH,"_ds7_50yr_sims_1_25/save/",sep="") 
wd = paste("C:/PhD/Chapter3/",LH,"_ds1_50yr_sims_multinom_boot_AE_FALSE_1_100/save/",sep="") 
wd = paste("D:/PhD/Chapter3/April16_PreCPUE/",LH,"_ds0_50yr_sims_multinom_boot_AE_TRUE_1_100/save/",sep="") 
par(mfrow =c (4,3), oma =c(1,1,1,1), mar = c(2,4,2,3))
hist.len = "50_yr"

#wd = "C:/PhD/Chapter3/flatfish_ds1_sims_1_100/save/" ; par(mfrow =c (4,3))
#wd = "C:/PhD/Chapter3/flatfish_ds2_sims_1_100/save/"
#wd = "C:/PhD/Chapter3/flatfish_ds3_sims_1_100/save/"
#wd = "C:/PhD/Chapter3/flatfish_ds4_sims_1_100/save/"
setwd(wd)
sims = 100
start.year = 1
if (LH == "rockfish") {
	ss.years = 246#271#221#142
	first.ass = 170#120#90#120
	if (hist.len == "50_yr") {
		ss.years = 221#271#221#142
		first.ass = 120#170#120#90#120
	}
	ass.length = 26
	target = 0.40#0.25 
	thres  = 0.25#0.08
	ass.index = c(1,3,6,9,12,15, 18, 21, 24)
	ages = 71
}
if (LH == "flatfish") {
	ss.years = 193
	first.ass = 140
	if (hist.len == "50_yr"){
		ss.years = 143
		first.ass = 90
	}
	ass.length = 14
	target = 0.25 
	thres  = 0.08
	ass.index = c(1,3,5,7)
	ages = 41
}
end.year = ss.years + 1


ssb = matrix(0, end.year, sims)
ssb.est = array (0, dim = c(ss.years, ass.length, sims))
for (i in 1:sims){
 	est = paste("ss_ests_",i,sep="")
	dat = paste("om_proj_",i,sep ="")
	load(est); load (dat)
	ssb[,i] = Proj$SSB[start.year:end.year]
	ssb.est[,,i] = Est$SB[1:ss.years ,]
}

depl = matrix(0, end.year, sims)
depl.est = array (0, dim = c(ss.years, ass.length, sims))
for (i in 1:sims){
 	est = paste("ss_ests_",i,sep="")
	dat = paste("om_proj_",i,sep ="")
	load(est); load (dat)

	depl[,i] = Proj$Depl[start.year:end.year]
	depl.est[,,i] = Est$Bratio[1:ss.years,] #Est$Bratio[1:ss.years,]
}


ind = 41
if (LH == "rockfish"){ind = 71 }
catch = matrix(0,end.year-ind + 1, sims)
acl.true = matrix(0,end.year-ind + 1, sims)
for (i in 1:sims){
	dat = paste("om_proj_",i,sep ="")
	load (dat)
	catch[,i] = Proj$catch[ind:end.year]
	acl.true[,i] = Proj$acl.true[ind:end.year]	
}


recruit = matrix(0, end.year, sims)
recruit.est = array (0, dim = c(ss.years, ass.length, sims))
om.time.over <- time.over <- re.time.over <- numeric(sims)
for (i in 1:sims){
	#if (i < 26) { temp = 25 + i }
	#if (i > 25) { temp = 50 + i }
 	est = paste("ss_ests_",i,sep="")
	dat = paste("om_proj_",i,sep ="")
	load(est); load (dat)

	recruit[,i] = Proj$Ry[start.year:end.year]
	recruit.est[,,i] = Est$Recruits[1:ss.years,] #Est$Recruits[1:ss.years,]

	if (sum(Est$recovered.est) != 0){
 		ind = Est$recovered.est>0
 		values = unique(sort(Est$recovered.est[ind]))
 		print(values)
 		time.over[i] = values[2] -values[1] + 1
 		if(is.na(time.over[i])) { time.over[i] = ss.years - first.ass }
 	}
 	om.time.over[i] = max(Proj$recovered.om) - first.ass
 	if(om.time.over[i] < 0) { om.time.over[i] = ifelse(LH == "rockfish", 100, 50) }
}



med.depl = apply(depl, 1, median)
med.ssb = apply(ssb, 1, median)
med.rec = apply(recruit, 1, median)
med.catch = apply(catch, 1, quantile, c(0.05, 0.50, 0.95))
med.depl.est = matrix(0, ss.years, ass.length)
med.ssb.est = matrix(0, ss.years, ass.length)
med.rec.est = matrix(0, ss.years, ass.length)
med.catch.est = matrix(0, ss.years, ass.length)
for (a in 1:ass.length){
	med.depl.est[,a] = apply(depl.est[,a,], 1, median)
	med.ssb.est[,a] = apply(ssb.est[,a,], 1, median)
	med.rec.est[,a] = apply(recruit.est[,a,], 1, median)
}

re.depl = matrix(0, sims, ass.length)
re.ssb  = matrix(0, sims, ass.length)
re.sb0 = matrix(0, sims, ass.length)
re.sb1 = matrix(0, sims, ass.length)
for (a in 1:ass.length){
	ind = first.ass + 4*a - 4
	re.depl[,a] = (depl.est[ind,a,] - depl[ind,])/depl[ind,]
	re.ssb [,a] = (ssb.est[ind,a,] - ssb[ind,]) / ssb[ind,]
	re.sb0[,a] = (ssb.est[1,a,] - ssb[1,]) / ssb[1,]
	re.sb1[,a] = (ssb.est[ages,a,] - ssb[ages,])/ ssb[ages,]
}

for (a in 1:ass.length){
	ind = first.ass + 4*a - 5
	print(cbind(median(recruit.est[ind,a,]/recruit.est[ind-5,a,]),
	median(recruit[ind,]/recruit[ind-5,])))
}

re.time.over = (time.over - om.time.over)/ om.time.over
par(mfrow=c(3,2))
boxplot(re.depl, ylim = c(-1, 1), ylab = "re depl"); abline(h = 0)
boxplot(re.ssb, ylim = c(-1, 1), ylab = "re ssb"); abline(h = 0)
boxplot(re.sb0, ylim = c(-0.5, 0.5), ylab = "re sb0"); abline(h = 0)
boxplot(re.sb1, ylim = c(-0.5, 0.5), ylab = "re sb1"); abline(h = 0)
boxplot(re.time.over, ylim =  c(-1, 1), ylab = "re time over"); abline(h = 0)

apply(re.depl, 1, median)


grey = rgb(0,0,0,0.10)
par(mfrow =c (2, length(ass.index)), oma = c(4,4,2,4), mar = c(2,0,2,0))
for (a in 1:length(ass.index)){
  b = ass.index[a]
  temp = (first.ass - 20):(first.ass + b* 4 - 4)
  temp.re.ssb = (ssb.est[temp,b,] - ssb[temp,]) / ssb[temp,]
  med.re.ssb = apply(temp.re.ssb, 1, quantile, c(0.975, 0.50, 0.25))
  plot( temp, med.re.ssb[2,], type = 'l', lwd =2, ylim = c( -0.5, 0.5), axes = F)# xlim = c(1, max(50 + ass.index.flat* 4 - 4)) )
  xx = c(temp, rev(temp)); yy = c(med.re.ssb[1,], rev(med.re.ssb[3,]))
  polygon(xx, yy, col = grey, border = NA)
  lines(temp, med.re.ssb[1,], lty = 2)
  box(); abline (h = 0, lty =1); abline(v = first.ass, lty = 2)
  axis(side = 1)
  if (a == 1) { 
  	axis(side = 2)
  	mtext(side = 2, "Relative Error Spawning Biomass", outer = T, line = 2.5) 
	mtext(side = 1, "Year", outer = T, line = 2.5) 
  } 
}  


for (a in 1:length(ass.index)){
  b = ass.index[a]
  temp = (first.ass - 20):(first.ass + b * 4 - 4)
  temp.re.depl = (depl.est[temp,b,] - depl[temp,])/depl[temp,]
  med.re.depl = apply(temp.re.depl, 1, quantile, c(0.975, 0.50, 0.25))
  plot( temp, med.re.depl[2,], type = 'l', lwd =2, ylim = c( -0.5, 0.50), axes = F) 
  xx = c(temp, rev(temp)); yy = c(med.re.depl[1,], rev(med.re.depl[3,]))
  polygon(xx, yy, col = grey, border = NA)
  lines(temp, med.re.depl[1,], lty = 2)
  lines(temp, med.re.depl[3,], lty = 2)
  box(); abline (h = 0, lty =1); abline(v = first.ass, lty = 2)
  axis(side = 1)
  if (a == 1) { axis(side = 2)
  	mtext(side = 2, "Relative Error Depletion", outer = T, line = 2.5) 
	mtext(side = 1, "Year", outer = T, line = 2.5)
  }
}  


par(mfrow =c (3,1))
plot(1:ss.years, med.depl[1:ss.years], ylim =c(0,1.1), type ='l', lwd =2, ylab = "Depletion")
abline (h =target) ; abline ( h =thres)
for (a in 1:ass.length){
	ind = 1:(first.ass + a*4 -4)
	if (a == ass.length) { ind = 1:(first.ass+a*4-5) }
	lines(ind, med.depl.est[ind,a], lty = 2, col =2)
}
y.max = 8000; if (LH == "rockfish") { y.max = 15000}
plot(1:ss.years, med.ssb[1:ss.years], type ='l', lwd =2, ylim = c(0, y.max), ylab = "SSB")
for (a in 1:ass.length){
	ind = 1:(first.ass + a*4 -4)
	if (a == ass.length) { ind = 1:(first.ass+a*4-5) }
	lines(ind, med.ssb.est[ind,a], lty = 2, col = a)
}
plot(1:ss.years, med.rec[1:ss.years], type ='l', lwd =2, ylim = c(0, 3000), ylab = "Recruits")
for (a in 1:ass.length){
	ind = 1:(first.ass + a*4 -4)
	if (a == ass.length) { ind = 1:(first.ass+a*4-5) }
	lines(ind, med.rec.est[ind,a], lty = 2, col =2)
}

plot(1:ss.years, recruit[1:ss.years], type ='l', lwd =2, ylim = c(0, 10000), ylab = "Recruits")
for (a in 1:ass.length){
	plot(1:ss.years, recruit[1:ss.years], type ='l', lwd =2, ylim = c(0, 10000), ylab = "Recruits")
	ind = 1:(first.ass + a*4 -4)
	if (a == ass.length) { ind = 1:(first.ass+a*4-5) }
	lines(ind, recruit.est[ind,a,1], lty = 2, col =2)
}

plot(1:ss.years, ssb[1:ss.years], type ='l', lwd =2, ylim = c(0, 15000), ylab = "SSB")
for (a in 1:ass.length){
	ind = 1:(first.ass + a*4 -4)
	if (a == ass.length) { ind = 1:(first.ass+a*4-5) }
	lines(ind, ssb.est[ind,a,1], lty = 2, col =2)
}

plot(1:ss.years, depl[1:ss.years], ylim =c(0,1.1), type ='l', lwd =2, ylab = "Depletion")
abline (h =target) ; abline ( h =thres)
for (a in 1:ass.length){
	ind = 1:(first.ass + a*4 -4)
	if (a == ass.length) { ind = 1:(first.ass+a*4-5) }
	lines(ind, depl.est[ind,a,1], lty = 2, col =2)
}



M.mat = matrix(NA, ass.length, sims)
k.mat = matrix(NA, ass.length, sims)
lmin.mat = matrix(NA, ass.length, sims)
lmax.mat = matrix(NA, ass.length, sims)
f.selex = array(NA, dim = c(2, ass.length, sims))
s.selex = array(NA, dim = c(2, ass.length, sims))
f.adj   = matrix(NA, ass.length, sims)
h.mat   = matrix(NA, ass.length, sims)
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
 	#f.adj[,i] = Est[[16]]
 	h.mat[,i] = Est$h
}

par(mfrow=c(3,3))
boxplot(t(M.mat), ylim = c(0.04, 0.12), ylab = "M") ; abline (h = 0.08, col =2)
boxplot(t(h.mat), ylim = c(0.20, 1), ylab = "h") ; abline (h = 0.60, col =2)
boxplot(t(k.mat), ylim = c(0, 0.10), ylab = "k") ; abline (h = 0.05 , col =2)
boxplot(t(lmin.mat), ylim = c(10, 20), ylab = "Lmin") ; abline (h = 18, col =2)
boxplot(t(lmax.mat), ylim = c(60, 70), ylab = "Lmax") ; abline (h = 64, col =2)
boxplot(t(f.selex[1,,]), ylim = c(42, 48), ylab = "F Peak Select") ; abline (h = 45, col =2)
boxplot(t(f.selex[2,,]), ylim = c(4, 5), ylab = "F Slope Select") ; abline (h = 4.25, col =2)
#boxplot(t(f.adj), ylim = c(-10, 0), ylab = "F Peak Adj Select"); abline (h = 6, col = 2)
boxplot(t(s.selex[1,,]), ylim = c(36, 43), ylab = "S Peak Select") ; abline (h = 39, col =2)
boxplot(t(s.selex[2,,]), ylim = c(4, 5), ylab = "S Slope Select") ; abline (h = 4.25, col =2)




par(mfrow=c(3,3))
boxplot(t(M.mat), ylim = c(0.13, 0.25), ylab = "M") ; abline (h = 0.15, col =2)
boxplot(t(k.mat), ylim = c(0.10, 0.20), ylab = 'k') ; abline (h = 0.143779 , col =2)
boxplot(t(lmin.mat), ylim = c(15, 35), ylab = "Lmin") ; abline (h = 24.6219, col =2)
boxplot(t(lmax.mat), ylim = c(45, 65), ylab = "Lmax") ; abline (h = 55.4099, col =2)
boxplot(t(f.selex[1,,]), ylim = c(40, 47), ylab = "F Peak Select") ; abline (h = 43, col =2)
boxplot(t(f.selex[2,,]), ylim = c(4, 5), ylab = "F Slope Select") ; abline (h = 4.25, col =2)
boxplot(t(f.adj), ylim = c(43, 53), ylab = "F Peak Adj Select"); abline (h = 48, col = 2)
boxplot(t(s.selex[1,,]), ylim = c(30, 37), ylab = "S Peak Select") ; abline (h = 33, col =2)
boxplot(t(s.selex[2,,]), ylim = c(4, 5), ylab = "S Slope Select") ; abline (h = 4.25, col =2)


re.m = (M.mat - 0.15)/ 0.15
boxplot(t(re.m), ylim = c(-0.1, 0.10)); abline (h = 0)