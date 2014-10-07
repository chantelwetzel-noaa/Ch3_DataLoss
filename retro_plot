load("F:/PhD/Chapter3/flatfish_normal_data_retro/save/retro_18")
depl.est <- retro.out$depl.est
ssb.est  <- retro.out$ssb.est

ass.year = 50
retro.num = 8


par(mfrow = c(2,1))
depl.med <- apply(depl.est[1:48,1,1:4], 1, median)
plot(1:48, depl.med, ylim = c(0,1), type ='l', col = 1)
abline (h = 0.08)
for(a in 2:retro.num){ 
	depl.med <- apply(depl.est[1:(50 - a*2),a,1:4], 1, median)
	lines(1:(50 - a*2), depl.med, lty =1, col = a)}

ssb.med  <- apply(ssb.est[1:48, 1,1:4],  1, median)
plot(1:48, ssb.med, ylim = c(0,8000), type ='l', col = 1)
for(a in 2:retro.num){ 
	ssb.med  <- apply(ssb.est[1:(50 - a*2), a,1:4],  1, median)
	lines(1:(50 - a*2), ssb.med, lty =1, col = a)}


SSplotPars("C:/Users/xwetzelch/Desktop/New folder - Copy")