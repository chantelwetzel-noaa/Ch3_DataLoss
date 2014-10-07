
LH = "flatfish"
drive = "F"
scenario = "_normal_data_retro"
wd = paste(drive, ":/PhD/Chapter3/", sep = "")
ctl = paste(wd, LH, scenario, "/ctl/", sep ="")
dir = paste(wd, LH, scenario,"/", sep = "")
library(r4ss)
update_r4ss_files(save=FALSE,local="C:\\Program Files\\R\\R-3.1.0\\library\\r4ss_update_May132014")
source(paste(dir, "Rep_Summary.R",sep=""))
#Parameters
ass.year = 50
retro.num = 8
start.survey = 41
ssb.est <- array(0, dim =c(50, retro.num, 20))
depl.est<- array(0, dim =c(50, retro.num, 20))
ssb0.est<- array(0, dim =c(1, retro.num,  20))
m.est<- array(0, dim =c(1, retro.num,     20))


for(i in 1:20){
	setwd(dir)
	#file.copy( paste(ctl, "sim", i, "_50.ctl", sep = ""), paste(dir,"sim.ctl", sep = ""), overwrite = T)
	file.copy( paste(ctl, "sim", i, "_50.dat", sep = ""), paste(dir,"sim.dat", sep = ""), overwrite = T)

	retro = paste(dir, "save/retro_", i, sep ="")
	retro.out = list()

	for (j in 1:retro.num){

		remove = -2 * j
		tot.yrs = 1:(ass.year + remove)
		starter.file <- SS_readstarter(paste(dir,"starter.ss",sep=""))
		starter.file$retro_yr <- remove
		SS_writestarter(starter.file,file="starter.ss",overwrite=T)

		shell("ss3_opt.exe -nohess  > test.txt 2>&1")
		rep.new   <- readLines(paste(dir, "Report.sso", sep=""))
		rep.out   <- Rep_Summary(rep.new, y = ass.year, pre.fishery.yrs = 0)
		ssb.est[,j,i]  <- rep.out$SB
		depl.est[,j,i] <- rep.out$Depl
		ssb0.est[,j,i] <- rep.out$SB.virgin
		m.est[,j,i]    <- rep.out$M[1]
	}

	retro.out$ssb.est <- ssb.est
	retro.out$depl.est<- depl.est
	retro.out$ssb0.est<- ssb0.est
	retro.out$m.est   <- m.est
	save(retro.out, file = retro)
}

par(mfrow = c(2,1))
plot(1:48, depl.est[1:48,1,1], ylim = c(0,1), type ='l', col = 1)
for(a in 2:retro.num){ lines(1:(50 - a*2), depl.est[1:(50 - a*2),a,1], lty =1, col = a)}

plot(1:48, ssb.est[1:48,1,1], ylim = c(0,max(ssb.est)), type ='l', col = 1)
for(a in 2:retro.num){ lines(1:(50 - a*2), ssb.est[1:(50 - a*2),a,1], lty =1, col = a)}