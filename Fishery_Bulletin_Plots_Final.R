#=================================================
#
#  Selectivity plot
#
#=================================================

drive = "C" #"E:/SyncBack"
run.name = ""
LH = 'rockfish'
# Source in the parameters
github = TRUE
git.wd = "C:/Users/Chantel.Wetzel/Documents/GitHub/Ch3_DataLoss/"
source(paste(git.wd, run.name, "/functions/LH_parameter_values.R", sep=""))

selex.fxn <- function(a){
	#Double Normal Selectivity
  	startbin <- 1
  	peak <- fsp1[a]
  	upselex <- exp(fsp3)
  	downselex <- exp(fsp4)
  	final <- fsp6
	
  	point1 <- 1 / (1 + exp(-fsp5)) 
  	t1min <- exp(-((len.step[startbin] + 1) - peak)^2 / upselex)
  	peak2 <- peak + 2 + (0.99 * (len.step[length(len.step)] + 1) - peak - 2) / (1 + exp(-fsp2[a]))
  	point2 <- 1 / (1 + exp(-final))
  	t2min <- exp(-((len.step[length(len.step)] + 1) - peak2)^2 / downselex)
  	t1 <- len.step + 1 - peak
  	t2 <- len.step + 1 - peak2
  	join1 <- 1 / (1 + exp(-(20 / (1 + abs(t1))) * t1))
  	join2 <- 1 / (1 + exp(-(20 / (1 + abs(t2))) * t2))
  	asc <- point1 + (1 - point1) * (exp(-t1^2 / upselex) - t1min) / (1 - t1min)
  	if (fsp5 <= -999) { asc <-  exp(-(t1^2) / upselex)}
  	dsc <- 1 +(point2-1)*(exp(-t2^2 / downselex) - 1) / (t2min - 1)
  	if (fsp6 <- -999) { dsc <- exp(-(t2^2) / downselex)}
	
  	select.out <- asc * (1-join1) + join1 * (1 - join2 + dsc * join2) 
  	return(select.out)
}


N = 50
ds.vec = 1:4 # 4 option fixed asym, fixed, dome, tv asym, tv dome
selec <- array(NA, dim = c(length(ds.vec), length(len.step), N))
selec.adj <- 0
dome.adj  <- -8.5


for (a in 1: length(ds.vec)){
	source(paste(git.wd, run.name, "/functions/LH_parameter_values.R", sep=""))
	fsp2.start  <- fsp2
	# Load the seeds
	load(paste("C:/PhD/Chapter3/seed_list",sep="")) 
	select.seed   <- as.numeric(seed.list[[1]][,"spare1"])
	select.sd   <- ifelse(a>2, 0.05, 0)
	dome.sd     <- ifelse(a>2, 0.20, 0)

	for (b in 1:N){
		set.seed(select.seed[N])
		select.err     <- rnorm(N, 0, select.sd)
		fsp1           <- round(fsp1.start*exp(-0.5*select.sd*select.sd + select.err),0)
		if (a == 4 || a == 2)  { fsp1 <- round((fsp1.start + selec.adj)*exp(-0.5*select.sd*select.sd + select.err),0) }   	
    	fsp2           <- sample(x = c(-3, fsp2.start), size = 100, replace = T, prob = c(0,1))
    	select.err     <- rnorm(N, 0, dome.sd)
    	if (a == 4 || a == 2) { fsp2 <- round((fsp2 + dome.adj)*exp(-0.5*dome.sd*dome.sd + select.err), 1) }

		selec[a,,b] <- selex.fxn(a = b)
	}
}

print.letter <- function(label="(a)",xy=c(0.1,0.925),...) {   #... means any new parameters are copied faithfully to text() below
  tmp <- par("usr")
  text.x <- tmp[1]+xy[1]*diff(tmp[1:2])   #x position, diff=difference
  text.y <- tmp[3]+xy[2]*diff(tmp[3:4])   #y position
  text(x=text.x, y=text.y, labels=label, ...)             
}

alpha.cex = 2

setwd(paste0(drive,":/PhD/Chapter3/WriteUp/Fishery_Bulletin_Submission/FB-2017-0065-A.R1/4_revision"))

pdf(file = "fig1_selectivity.pdf", width = 6.7, height = 6.7)
par(mfrow = c(2,2), mar = c(2,2,2,2), oma = c(4,4,4,4))
axis.size = 1.1
plot(len.step, selec[1,,1], type = 'l', axes = F, xlab = "", ylab = "", lwd = 1, yaxs ='i', xaxs = 'i', ylim = c(-0.001, 1))
axis(side = 2, at =seq(0, 1, 0.25), labels = c("0.0","0.25", "0.50", "0.75", "1.00"), cex.axis = axis.size, las = 1)
axis(side = 1, at = seq(0,80,20), cex.axis = axis.size) 
mtext(side = 2, "Selectivity", line = 2.5, outer = T)
#mtext(side = 3, "Historical/Rebuilt", outer = F)
print.letter("A", xy = c(0.09, 0.95), cex = alpha.cex, font = 2, family = "serif")

plot(len.step, selec[2,,1], typ = 'l', axes = F, xlab = "", ylab = "", lwd = 1, yaxs ='i', xaxs = 'i', ylim = c(-0.001, 1))
#mtext(side = 3, "Overfished",  outer = F)
#text(par("usr")[2]*1.05, mean(par("usr")[3:4])+0.15, "Time-invarient", srt = -90, xpd = TRUE, pos = 4, cex = 1.2)
print.letter("B", xy = c(0.09, 0.95), cex = alpha.cex, font = 2, family = "serif")
axis(side = 1, at = seq(0,80,20), cex.axis = axis.size) 
axis(side = 2, at =seq(0, 1, 0.25), labels = c("0.0","0.25", "0.50", "0.75", "1.00"), cex.axis = axis.size, las = 1)

plot(len.step, selec[3,,1], type = 'l', axes = F, xlab = "", ylab = "", las = 1, yaxs ='i', xaxs = 'i', ylim = c(-0.001, 1))
for(b in 2:N){
	lines(len.step, selec[3,,b])
}
axis(side = 1, at = seq(0,80,20), cex.axis = axis.size) 
axis(side = 2, at =seq(0, 1, 0.25), labels = c("0.0","0.25", "0.50", "0.75", "1.00"), cex.axis = axis.size, las = 1)
print.letter("C", xy = c(0.09, 0.95), cex = alpha.cex, font = 2, family = "serif")

plot(len.step, selec[4,,1], type = 'l', axes = F, xlab = "", ylab = "", yaxs ='i', xaxs = 'i', ylim = c(-0.001, 1))
for(b in 2:N){
	lines(len.step, selec[4,,b])
}
axis(side = 1, at = seq(0,80,20), cex.axis = axis.size) 
axis(side = 2, at =seq(0, 1, 0.25), labels = c("0.0","0.25", "0.50", "0.75", "1.00"), cex.axis = axis.size, las = 1)
mtext(side = 1, "Length (cm)", outer = T, line = 2)
#text(par("usr")[2]*1.05, mean(par("usr")[3:4])+0.15, "Time-varying", srt = -90, xpd = TRUE, pos = 4, cex = 1.2)
print.letter("D", xy = c(0.09, 0.95), cex = alpha.cex, font = 2, family = "serif")

dev.off()

########################################################################################################################
# Data Availability Plot
########################################################################################################################

catch.start  = 1
end.yr       = 100 - 2
survey.freq  = 2
survey.start = s.age.start  = s.len.start  = 30#19
f.age.start  = f.len.start  = 25 #15
cpue.start   = 35

y.min = 1
y.max = 13 
cpue.size = 5
fl.size  = 10
flr.size = 2
fa.size  = 5
far.size = 1
lab.size = 1
# Create vectors that will be ploted for each data source
catch  <- catch.start:end.yr
f.lens <- f.len.start:end.yr
f.ages <- f.age.start:end.yr
cpue   <- cpue.start:end.yr
survey <- seq(survey.start, end.yr, survey.freq)
s.lens <- seq(s.len.start, end.yr, survey.freq)
s.ages <- seq(s.age.start, end.yr, survey.freq)

#name.label = c('Full data', 'Data reduction', 'Historical data')
#
#label<-c("catch", "survey index", "fishery lengths", "survey lengths", "fishery ages", "survey ages", #DS 1
#				  "survey index", "fishery lengths", "survey lengths", "fishery ages", "survey ages", #DS 2
#				  "survey index", "fishery lengths", "survey lengths", "fishery ages", "survey ages") #DS 3
#
#label<-c("catch", "survey index",  "survey lengths",  "survey ages", #DS 1
#				  "fishery cpue", "fishery lengths",  "fishery ages",  #DS 2
#				  "fishery cpue", "fishery lengths",  "fishery ages" ) #DS 3
#

print.letter <- function(label="(a)",xy=c(0.1,0.925),...) {   #... means any new parameters are copied faithfully to text() below
  tmp <- par("usr")
  text.x <- tmp[1]+xy[1]*diff(tmp[1:2])   #x position, diff=difference
  text.y <- tmp[3]+xy[2]*diff(tmp[3:4])   #y position
  text(x=text.x, y=text.y, labels=label, ...)             
}

linecol1 <- rgb(0.3,0.3,0.3,1)
linecol2 <- rgb(0.3,0.3,0.3,.75) # dark grey with alpha (opacity) = 0.5
linecol3 <- rgb(0.3,0.3,0.3,.50) # black with alpha (opacity) = 0.5
linecol4 <- rgb(0.3,0.3,0.3,.35)

samp.cex = 0.80
lab.cex = 0.85

y.min = 1
y.max = 10 
cpue.size = 5
fl.size  = 10
flr.size = 2
fa.size  = 5
far.size = 1
lab.size = 1

name.label = c('Full data', 'Reduced data', 'Eliminated data')
label<-c("Catch", "Fishery CPUE", "Fishery lengths",  "Fishery ages") 

##############################################################################################################
# Figure 2
##############################################################################################################

pdf(file = "fig2_datascenario.pdf",width=7,height=4.5)
#Catch
	#windows(width=9,height=7)
	par(mar=c(5.1,2.1,2.1,4.1))
	plot(catch, rep(max(y.max), end.yr), type="l", lwd=14, ylim=c(1,y.max), xlim=c(-1, end.yr + 25), xaxs="i", 
		col=linecol1,axes=FALSE, xlab="",ylab="")
	axis(side = 1, at = c(0, 20, 60, 90, end.yr + 25), labels = c( "","Historical", "Overfished", "Rebuilt", "" ))
	mtext(side = 1,"Time period", line=2.5)
	print.letter(xy = c(0.86, 0.965), label[1], cex = lab.cex)
	abline(v = end.yr + 2, lty=1)
	abline(v = 40, lty = 3)
	abline(v = 80, lty = 3)
	abline(h = max(y.max) - 0.4, lty =3)

# No reduction in sampling data scenario
	text(-2, y.max - 1,"Full data", pos = 4, cex = lab.size)

	# Fishery CPUE
		points(cpue.start:end.yr,   rep(max(y.max) - 1, length(cpue.start:end.yr)), type = "l", lwd = cpue.size, col = linecol2)
		print.letter(xy = c(0.91, 0.86), label[2], cex = lab.cex)
		print.letter(xy = c(0.27, 0.89), "CV = 0.30", cex = samp.cex)

	#Fishery Lengths
		lines(f.lens,  rep(max(y.max) - 2, length(f.lens)), type="l", lwd=fl.size, col=linecol2)
		print.letter(xy = c(0.915, 0.76), label[3], cex = lab.cex)
		print.letter(xy = c(0.29, 0.81), expression(paste(italic("n"), " = 50")), cex = samp.cex)

	#Fishery Ages
		lines(f.ages,  rep(max(y.max) - 3, length(f.ages)), type="l", lwd=fa.size, col=linecol2)
		print.letter(xy = c(0.90, 0.65), label[4], cex = lab.cex)
		print.letter(xy = c(0.29, 0.71), expression(paste(italic("n"), " = 25")), cex = samp.cex)

	abline(h = max(y.max) - 3.4, lty=3)

# Reduction in Data 
	text(-2, y.max - 4, "Reduced data", pos = 4, cex = lab.size)

	# Fishery CPUE
		points(cpue.start:39,   rep(max(y.max) - 4, length(cpue.start:39)), type = "l", lwd = cpue.size, col = linecol2)
		lines(81:end.yr,        rep(max(y.max) - 4, length(81:end.yr)), type = "l", lwd = cpue.size, col = linecol2)
		print.letter(xy = c(0.91, 0.555), label[2], cex = lab.cex)
		print.letter(xy = c(0.27, 0.59), "CV = 0.30", cex = samp.cex)
		print.letter(xy = c(0.71, 0.59), "CV = 0.30", cex = samp.cex)

	#Fishery Lengths
		lines(f.len.start:39,   rep(max(y.max) - 5, length(f.len.start:39)), type="l", lwd=fl.size, col=linecol2)		
		lines(41:80,            rep(max(y.max) - 5, 40), type="l", lwd=flr.size, col=linecol2)
		lines(81:end.yr,        rep(max(y.max) - 5, length(81:end.yr)), type="l", lwd=fl.size, col=linecol2)
		print.letter(xy = c(0.915, 0.45), label[3], cex = lab.cex)
		print.letter(xy = c(0.29, 0.50), expression(paste(italic("n"), " = 50")), cex = samp.cex)
		print.letter(xy = c(0.50, 0.50), expression(paste(italic("n"), " = 10")), cex = samp.cex)
		print.letter(xy = c(0.70, 0.50), expression(paste(italic("n"), " = 50")), cex = samp.cex)

	#Fishery Ages
		lines(f.age.start:39,   rep(max(y.max) - 6, length(f.age.start:39)), type="l", lwd=fa.size, col=linecol2)		
		lines(41:80,            rep(max(y.max) - 6, 40), type="l", lwd=far.size, col=linecol2)
		lines(81:end.yr,        rep(max(y.max) - 6, length(81:end.yr)), type="l", lwd=fa.size, col=linecol2)
		print.letter(xy = c(0.90, 0.35), label[4], cex = lab.cex)
		print.letter(xy = c(0.29, 0.395), expression(paste(italic("n"), " = 25")), cex = samp.cex)
		print.letter(xy = c(0.50, 0.395), expression(paste(italic("n"), " = 5" )), cex = samp.cex)
		print.letter(xy = c(0.70, 0.395), expression(paste(italic("n"), " = 25")), cex = samp.cex)

	abline(h = max(y.max) - 6.4, lty=3)

# Loss of Fishery Data
	text(-2, y.max - 7, "Eliminated data", pos = 4)

	# Fishery CPUE
		points(cpue.start:39,   rep(max(y.max) - 7, length(cpue.start:39)), type = "l", lwd = cpue.size, col = linecol2)
		lines(81:end.yr,        rep(max(y.max) - 7, length(81:end.yr)), type = "l", lwd = cpue.size, col = linecol2)
		print.letter(xy = c(0.91, 0.25), label[2], cex = lab.cex)
		print.letter(xy = c(0.27, 0.275), "CV = 0.30", cex = samp.cex)
		print.letter(xy = c(0.71, 0.275), "CV = 0.30", cex = samp.cex)

	#Fishery Lengths
		lines(f.len.start:39,   rep(max(y.max) - 8, length(f.len.start:39)), type="l", lwd=fl.size, col=linecol2)		
		lines(81:end.yr,        rep(max(y.max) - 8, length(81:end.yr)), type="l", lwd=fl.size, col=linecol2)
		print.letter(xy = c(0.915, 0.14), label[3], cex = lab.cex)
		print.letter(xy = c(0.29, 0.19), expression(paste(italic("n"), " = 50")), cex = samp.cex)
		print.letter(xy = c(0.70, 0.19), expression(paste(italic("n"), " = 50")), cex = samp.cex)

	#Fishery Ages
		lines(f.age.start:39,   rep(max(y.max) - 9, length(f.age.start:39)), type="l", lwd=fa.size, col=linecol2)		
		lines(81:end.yr,        rep(max(y.max) - 9, length(81:end.yr)), type="l", lwd=fa.size, col=linecol2)
		#text(end.yr + 21,           max(y.max) - 12, label[10], pos=2, col=1, font=2, cex=lab.size)
		print.letter(xy = c(0.90, 0.04), label[4], cex = lab.cex)
		print.letter(xy = c(0.29, 0.075), expression(paste(italic("n"), " = 25")), cex = samp.cex)
		print.letter(xy = c(0.70, 0.075), expression(paste(italic("n"), " = 25")), cex = samp.cex)

dev.off()
##################################################################################################################







#Load in the R objects from the Simulation Eval Code ========================================
drive = "C:"
run.name = "Final_wo_survey"
load(paste0(drive,"/PhD/Chapter3/",run.name,"/output/rockfish_meds_all"))
load(paste0(drive,"/PhD/Chapter3/",run.name,"/output/rockfish_est_all"))
load(paste0(drive,"/PhD/Chapter3/",run.name,"/output/rockfish_om_all"))

source(paste0(drive, "/Users/Chantel.Wetzel/Documents/GitHub/Ch3_DataLoss/box95.R"))

rock.out  <- med.out <- est.out <- om.out <- list()
rock.out[[1]] <- med.out[[1]] <- meds.all
rock.out[[2]] <- est.out[[1]] <- est.all
rock.out[[3]] <- om.out[[1]]  <- om.all


#==================================================================================================================
print.letter <- function(label="(a)",xy=c(0.1,0.925),...) {   #... means any new parameters are copied faithfully to text() below
  tmp <- par("usr")
  text.x <- tmp[1]+xy[1]*diff(tmp[1:2])   #x position, diff=difference
  text.y <- tmp[3]+xy[2]*diff(tmp[3:4])   #y position
  text(x=text.x, y=text.y, labels=label, ...)             
}

alpha.label = c('A', 'B', 'C', 'D', 'E', 'F')
name.label = c('Full data', 'Reduced data', 'Eliminated data')

library(RColorBrewer)

#Dimensions used for plotting======================================================================================
ds =  dim(rock.out[[2]]$ssb.est)[1] #Determines the number of data scenarios to plot
hist.yrs = 50
proj.yrs = 102
ass.yr1  = 120
pre.yrs  = 71
final.yr = proj.yrs + hist.yrs + pre.yrs - 1  #rock.yrs + 50 + pre.yrs.rock - 1
ass.freq = 6

ass.yrs = seq(ass.yr1, ass.yr1 + proj.yrs, ass.freq)
N       = dim(om.out[[1]]$ssb)[3] 


#=========================================================================================================
# Figure 3: Time-invariant estimates
#=========================================================================================================
pdf(file = "fig3_time_invariant.pdf", width = 6.7, height = 9)
par(mfrow= c(5,3), mar = c(1.5, 1.5, 1.5, 1.5), oma = c(5,5,2,5), cex.lab = 0.8)
letter.cex = 1; axis.cex = 1; lab.cex = 0.75; alpha.cex = 1.5
ass.plot.yrs = ifelse(ass.freq == 8, 13, 26)
ass.plot.yrs = ifelse(ass.freq == 6, 18, ass.plot.yrs)
min = -1.5; max = 1.55
text.yr = c(2, 7, 11,15)
ass.num = ifelse(ass.freq == 8, 13, 26)
steep = 0.65

 
# RE SSB
for (a in 1:3){

  to.plot = ass.yrs[1:ass.plot.yrs]
  re.ssb = matrix(0, length(to.plot), N)
  for (b in 1:length(to.plot)){
    re.ssb[b,] = med.out[[1]]$re.ssb[a,b,to.plot[b],]
  }

  box95(t(re.ssb), list = F, ylim = c(-1, max), col = rep('grey',length(to.plot)), axes = F, add = FALSE, 
  	boxwex = rep(0.75, length(to.plot)), yaxs = "i", xaxs = 'i')
  abline(h = 0, lty = 2) #; abline(h = -0.98) 

  if(a == 1) {  mtext(side = 2, outer = F, "RE SB", line = 3, cex = lab.cex) } 
  mtext(side = 3, outer = F, name.label[a], cex = lab.cex) 
  axis(side = 2, at = c(-10, seq( -0.5, max, 0.5)), las = 1, cex.axis = axis.cex)
  axis(side = 1, at = c(-5, seq(1,length(to.plot),4)), labels = c("", seq(to.plot[1]- pre.yrs + 1, to.plot[length(to.plot)] - pre.yrs + 1, 4*ass.freq)), cex.axis = axis.cex )
  axis(side = 1, at = c(-100, 100)); axis(side = 2, at = c(-100, 100))
  print.letter(alpha.label[a], xy = c(0.87, 0.95), cex = alpha.cex, font = 2, family = "serif")
}

# RE Depletion

for (a in 1:3){
  to.plot = ass.yrs[1:ass.plot.yrs]
  re.depl = matrix(0, length(to.plot), N)
  for (b in 1:length(to.plot)){
    re.depl[b,] = med.out[[1]]$re.depl[a,b,to.plot[b],]
  }
  if (a == 1){ re.depl[1,40] = 0.961795}
 
  box95(t(re.depl), list = F, ylim = c(-1, max), col = rep('grey',length(to.plot)), axes = F, add = FALSE, boxwex = rep(0.75, length(to.plot)), yaxs = "i", xaxs = 'i')
  abline(h = 0, lty = 2) #; abline(h= -0.98)
  if(a == 1) { mtext(side = 2, outer = F, "RE Relative SB", line = 3, cex = lab.cex)} 
  axis(side = 2, at = seq(-0.5, max, 0.5), las = 1, cex.axis = axis.cex) 
  axis(side = 1, at = seq(1,length(to.plot),4), labels = seq(to.plot[1]- pre.yrs + 1, to.plot[length(to.plot)] - pre.yrs + 1, 4*ass.freq), cex.axis = axis.cex )
  axis(side = 1, at = c(-100, 200)); axis(side = 2, at = c(-100, 100))
  print.letter(c("D", "E", "F")[a], xy = c(0.87, 0.95), cex = alpha.cex, font = 2, family = "serif")  
}

# Steepness
for(b in 1:3){

  box95(x=t(est.out[[1]]$h.est[b,,]), list = F, ylab = "Steepness", ylim = c(0.19, 1.05), col = rep('grey',ass.num), axes = F,
     boxwex = rep(0.70, length(to.plot)), add = FALSE, yaxs = "i", xaxs = 'i')
  abline(h= 0); abline(h = steep, lty = 2, col = 1)
  
  axis(side = 2, at = c(0.20, 0.40, 0.60, 0.80, 1.0), las = 1, cex.axis = axis.cex) 
  axis(side = 1, at = seq(1,length(to.plot),4), labels = seq(to.plot[1]- pre.yrs + 1, to.plot[length(to.plot)] - pre.yrs + 1, 4*ass.freq), cex.axis = axis.cex )
  axis(side = 1, at = c(-100, 100)); axis(side = 2, at = c(-100, 100))
  print.letter(c("G", "H", "I")[b], xy = c(0.87, 0.95), cex = alpha.cex, font = 2, family = "serif")  
  if(b == 1) { mtext(side = 2, "Steepness", outer = F, line = 3, cex = lab.cex) }
}

# Fishery selectivity
for(b in 1:3){

  box95(x=t(est.out[[1]]$f.selex.est[b,1,,]), ylim = c(42, 49), axes = F,col = 'grey',  add = FALSE, yaxs = "i", xaxs = 'i')
  abline(h = 45, col = 1, lty =2)
  if(b == 1) { mtext(side =2, "Size at max. select. (cm)",  outer = F, line = 3, cex = lab.cex) }
  axis (side = 2, at = seq(43, 47, 2), las = 1, cex.axis = axis.cex)
  axis(side = 1, at = seq(1,length(to.plot),4), labels = seq(to.plot[1]- pre.yrs + 1, to.plot[length(to.plot)] - pre.yrs + 1, 4*ass.freq), cex.axis = axis.cex )
  axis(side = 1, at = c(-100, 100)); axis(side = 2, at = c(-100, 100))
  print.letter(c("J", "K", "L")[b], xy = c(0.87, 0.95), cex = alpha.cex, font = 2, family = "serif")  
  
}

# Fishery Dome
min = -15; max = 7

for (a in 1:3){

  scale.per.of = (1- med.out[[1]]$n.overfished[a,] / 100)*5 + min 
  om.scale.per.of = (1- med.out[[1]]$om.n.overfished[a,] / 100)*5 + min 
  plot(1:length(to.plot), om.scale.per.of, type = 'l', lwd = 1.5, ylim = c(min, max), axes= F)
  lines(1:length(to.plot), scale.per.of, col = 1, lty = 2)
  if(a == 1) { legend("bottomright", legend = c("OM", "EM"), lty = c(1,2), bty = 'n', cex = 0.90) }
  if(a == 2) { print.letter("% Rebuilt", xy = c(0.50, 0.05), cex = 0.9) }
  if(a == 3) { axis(side = 4, at = c(min, min + 2.5, -10), label = c("0%", "50%", "100%"), las = 1, cex.axis = axis.cex) } 

  est.out[[1]]$f.selex.adj.est[a,1,1,] = max + 5
  if (a == 3) {est.out[[1]]$f.selex.adj.est[a,1,,] = max + 20 }
  box95(x=t(est.out[[1]]$f.selex.adj.est[a,1,,]), xlim = c(1,18), axes = F, add = TRUE) 
  abline(h = -2.5, col = 1, lty =2)
  abline(h = -10)
  if(a == 1) { mtext(side =2, "Width at max. select.",  outer = F, line = 3, cex = lab.cex) }
  axis(side = 2, las = 1, at = c(-5,0,5), labels = c(-5, 0, 5), cex.axis = axis.cex)
  axis(side = 1, at = seq(1.5,length(to.plot)+0.5, 4), labels = seq(to.plot[1]- pre.yrs + 1, to.plot[length(to.plot)] - pre.yrs + 1, 4*ass.freq), cex.axis = axis.cex )
  axis(side = 1, at = c(-100, 100)); axis(side = 2, at = c(-100, 100))
  print.letter(c("M", "N", "O")[a], xy = c(0.87, 0.95), cex = alpha.cex, font = 2, family = "serif")  

   
}

mtext(side =1, "Assessment Year", outer = T, line = 2, cex = lab.cex)
dev.off()



#=========================================================================================================
# Figure 4: RMSE over time for spawning biomass
#=========================================================================================================

pdf(file = "fig4_rmse.pdf", width = 6.7, height = 3.5)
par(mfrow= c(1,2), mar = c(1,2,2,1), oma = c(4,4,2,4), cex.axis = 1.1, cex.lab = 1.1)

letter.cex = 1; axis.cex = 0.80; label.cex = 0.80; alpha.cex = 1.2
ass.plot.yrs = ifelse(ass.freq == 8, 13, 26)
ass.plot.yrs = ifelse(ass.freq == 6, 18, ass.plot.yrs)
min = 0; max = max(med.out[[1]]$rmse.ssb.ass[4,]+5)+10
pch.vec = rep(16:18,2)

for (a in 1:ds){
  index = ass.yrs - pre.yrs
  if (a == 1 || a == 4){

    if (a == 1) { 
      plot(index, med.out[[1]]$rmse.ssb.ass[a,], type = 'l', lwd = 1, ylim = c(min, max), xlim = c(46, 154), axes = F, xaxs="i", yaxs = 'i')
      points(index, med.out[[1]]$rmse.ssb.ass[a,], pch = pch.vec[a])
      mtext(side = 1, "Assessment Year", outer = T, cex = label.cex, line = 1)
      mtext(side = 2, "RMSE spawning biomass", cex = label.cex, outer = T, line = 0.5)
      #mtext(side = 3, outer = F, "Time-invariant")
      axis(side = 2, at = seq(0, max, 20), , cex.axis = axis.cex, las = 1)
      axis(side = 1, at = seq(index[1], max(index), 25), labels = seq(50, 150, 25), cex.axis = axis.cex)
      axis(side = 1, at = c(-100, 1000)); axis(side = 2, at = c(-100, 1000))
      print.letter(alpha.label[a], xy = c(0.08, 0.95), cex = alpha.cex, font = 2, family = "serif")  
      legend("topright", legend = name.label, pch = pch.vec, bty= 'n', cex = 0.85) }

    if( a == 4) { 
      plot(index, med.out[[1]]$rmse.ssb.ass[a,], type = 'l', lwd = 1, ylim = c(min, max), xlim = c(46, 154), axes = F, xaxs="i", yaxs = 'i')
      points(index, med.out[[1]]$rmse.ssb.ass[a,], pch = pch.vec[a])
      axis(side = 2, at = seq(0, max, 20), cex.axis = axis.cex, las = 1)
      axis(side = 1, at = seq(index[1], max(index), 25), labels = seq(50, 150, 25), cex.axis = axis.cex) 
      axis(side = 1, at = c(-100, 1000)); axis(side = 2, at = c(-100, 1000))
      print.letter(alpha.label[a-2], xy = c(0.08, 0.95), cex = alpha.cex, font = 2, family = "serif")  
      #mtext(side = 3, outer = F, "Time-varying")      
    }

  }
  lines(index, med.out[[1]]$rmse.ssb.ass[a,], lwd = 1, col = 1)
  points(index, med.out[[1]]$rmse.ssb.ass[a,], pch = pch.vec[a], col = 1)
}

dev.off()


#=========================================================================================================
# Figure 5: Steepness ================================================================================
#=========================================================================================================
pdf(file = "fig5_ressb_h_compare.pdf", width = 6.7, height = 6)
par(mfrow= c(2,2), mar = c(1, 2, 2, 1), oma = c(5,5,2,5), cex.axis = 1.1, cex.lab = 1.1)
letter.cex = 0.9; axis.cex = 1; label.cex = 1; alpha.cex = 1.5
ass.plot.yrs = ifelse(ass.freq == 8, 13, 26)
ass.plot.yrs = ifelse(ass.freq == 6, 18, ass.plot.yrs)
to.plot = ass.yrs[1:ass.plot.yrs]
min = -0.85; max = 1.25
text.yr = c(2, 7, 11,15)
colors = rep( c('red', 'steelblue'), length(to.plot))
colors = rep( c('white', 'darkgrey'), length(to.plot))
plot.list = list()

for (aa in 1:2){ 
  a = ifelse(aa > 1, 3, 1)
  not = which(est.out[[1]]$time.over[3,] == 101) 
  re.ssb = matrix(0, length(to.plot), N)
  for (b in 1:length(to.plot)){
    re.ssb[b,] = med.out[[1]]$re.ssb[a,b,to.plot[b],]
  }

  #plot.list[[b]]
  temp1 = 1; temp2 = 1
  for( b in 1:36){
    if(b %% 2 != 0){ plot.list[[b]] = re.ssb[temp1,not]; temp1 = temp1 + 1}
    if(b %% 2 == 0){ plot.list[[b]] = re.ssb[temp2,-not]; temp2 = temp2 + 1}
  }

  box95(plot.list, list = T, ylim = c(min, max), col = colors, axes = F, boxwex = rep(0.60, length(to.plot)))
  abline(h = 0) 
  if( a ==1 ) { mtext(side = 2, outer = F, "RE spawning biomass", line = 3, cex = label.cex )}
  axis(side = 2, at = seq(-0.5, max, 0.5), las = 1, cex.axis = axis.cex)
  axis(side = 1, cex.axis = axis.cex, at = seq(1.5,length(to.plot)*2,8), 
        labels = seq(to.plot[1]- pre.yrs + 1, to.plot[length(to.plot)] - pre.yrs + 1, 4*ass.freq) )
  print.letter(alpha.label[aa], xy = c(0.9, 0.95),cex = alpha.cex, font = 2, family = "serif")  
  mtext(side = 3, outer = F, name.label[a], cex = label.cex)  
  axis(side = 1, at = c(-100, 1000)); axis(side = 2, at = c(-100, 1000))
}

ymin = 0.20; ymax = 1.02
steep = 0.65
for(aa in 1:2){
  a = ifelse(aa > 1, 3, 1)
  not = which(est.out[[1]]$time.over[3,] == 101) 
  plot.list = list()
  temp1 = 1; temp2 = 1
  for( b in 1:36){
    if(b %% 2 != 0){ plot.list[[b]] = est.out[[1]]$h.est[a,temp1, not]; temp1 = temp1 + 1}
    if(b %% 2 == 0){ plot.list[[b]] = est.out[[1]]$h.est[a,temp2,-not]; temp2 = temp2 + 1}
  }

  box95(plot.list, list = T, ylab = "Steepness", ylim = c(ymin,ymax), col = colors, axes = F,
     boxwex = rep(0.60, length(to.plot)))
  abline(h = steep, lty = 2, col = 1)
  print.letter(alpha.label[aa+2], xy = c(0.9, 0.95), cex = alpha.cex, font = 2, family = "serif")  
  axis(side = 2, las = 1, cex.axis = axis.cex) 
  axis(side = 1, cex.axis = axis.cex, at = seq(1.5,length(to.plot)*2,8), 
        labels = seq(to.plot[1]- pre.yrs + 1, to.plot[length(to.plot)] - pre.yrs + 1, 4*ass.freq) )
  if (a == 1) { mtext(side = 2, "Steepness", outer =F, line = 2.5, cex = label.cex) }
  if (a == 1) { mtext(side = 1, "Assessment Year", outer = T, line = 2, cex = label.cex) }
  axis(side = 1, at = c(-100, 1000)); axis(side = 2, at = c(-100, 1000))
}

dev.off()


#=========================================================================================================
# Figure 6: Time-varying estimates
#=========================================================================================================
pdf(file = "fig6_time_varying.pdf", width = 6.7, height = 9)
par(mfrow= c(5,3), mar = c(1.5, 1.5, 1.5, 1.5), oma = c(5,5,2,5),  cex.lab = 0.8)
letter.cex = 1; axis.cex = 1; lab.cex = 0.75; alpha.cex = 1.5
ass.plot.yrs = ifelse(ass.freq == 8, 13, 26)
ass.plot.yrs = ifelse(ass.freq == 6, 18, ass.plot.yrs)
min = -1.5; max = 1.55
text.yr = c(2, 7, 11,15)
 
# RE SSB
for (a in 4:6){

  to.plot = ass.yrs[1:ass.plot.yrs]
  re.ssb = matrix(0, length(to.plot), N)
  for (b in 1:length(to.plot)){
    re.ssb[b,] = med.out[[1]]$re.ssb[a,b,to.plot[b],]
  }

  box95(t(re.ssb), list = F, ymin = c(-0.6, max), col = rep('grey',length(to.plot)), axes = F, add = FALSE, boxwex = rep(0.75, length(to.plot)), yaxs = "i", xaxs = 'i')
  abline(h = 0, lty = 2) 

  if(a == 4) {mtext(side = 2, outer = F, "RE SB", line = 3, cex = lab.cex) } 
  axis(side = 2, at = seq(-0.5, max, 0.5), las = 1, cex.axis = axis.cex )
  axis(side = 1, at = seq(1,length(to.plot),4), labels = seq(to.plot[1]- pre.yrs + 1, to.plot[length(to.plot)] - pre.yrs + 1, 4*ass.freq), cex.axis = axis.cex ) 
  axis(side = 1, at = c(-100, 1000)); axis(side = 2, at = c(-100, 1000)) 
  mtext(side = 3, outer = F, name.label[a-3], cex = lab.cex)  
  print.letter(alpha.label[a-3], xy = c(0.87, 0.95), cex = alpha.cex, font = 2, family = "serif")    
}

# RE Depletion
for (a in 4:6){
  to.plot = ass.yrs[1:ass.plot.yrs]
  re.depl = matrix(0, length(to.plot), N)
  for (b in 1:length(to.plot)){
    re.depl[b,] = med.out[[1]]$re.depl[a,b,to.plot[b],]
  }
 
  box95(t(re.depl), list = F, ymin = c(-0.6, max), col = rep('grey',length(to.plot)), axes = F, add = FALSE, boxwex = rep(0.75, length(to.plot)), yaxs = "i", xaxs = 'i')
  abline(h = 0, lty = 2) 
  if(a == 4) {  mtext(side = 2, outer = F, "RE Relative SB", line = 3, cex = lab.cex)} 
  axis(side = 2, at = seq(-0.5, max, 0.5), las = 1, cex.axis = axis.cex )
  axis(side = 1, at = seq(1,length(to.plot),4), labels = seq(to.plot[1]- pre.yrs + 1, to.plot[length(to.plot)] - pre.yrs + 1, 4*ass.freq), cex.axis = axis.cex )
  axis(side = 1, at = c(-100, 1000)); axis(side = 2, at = c(-100, 1000))
  print.letter(c("D", "E", "F")[a-3], xy = c(0.87, 0.95), cex = alpha.cex, font = 2, family = "serif")  
}

# Steepness
for(b in 4:6){

  box95(x=t(est.out[[1]]$h.est[b,,]), list = F, ylab = "Steepness", ylim = c(0.19, 1.05), col = rep('grey',ass.num), axes = F,
     boxwex = rep(0.70, length(to.plot)), add = FALSE, yaxs = "i", xaxs = 'i')
  abline(h= 0); abline(h = steep, lty = 2, col = 1)
  print.letter(c("G", "H", "I")[b-3], xy = c(0.87, 0.95), cex = alpha.cex, font = 2, family = "serif")  
  
  axis(side = 2, at = c(0.20, 0.40, 0.60, 0.80, 1.0), las = 1, cex.axis = axis.cex )
  axis(side = 1, at = seq(1,length(to.plot),4), labels = seq(to.plot[1]- pre.yrs + 1, to.plot[length(to.plot)] - pre.yrs + 1, 4*ass.freq), cex.axis = axis.cex )
  axis(side = 1, at = c(-100, 1000)); axis(side = 2, at = c(-100, 1000))
  if(b == 4) { mtext(side = 2, "Steepness", outer = F, line = 3, cex = lab.cex) }
}

# Fishery selectivity
for(b in 4:6){

  box95(x=t(est.out[[1]]$f.selex.est[b,1,,]), ylim = c(42, 49), axes = F,col = 'grey',  add = FALSE, yaxs = "i", xaxs = 'i')
  abline(h = 45, col = 1, lty =2)
  if(b == 4) { mtext(side =2, "Size at max. select. (cm)",  outer = F, line = 3, cex = lab.cex) }
  axis (side = 2, at = seq(43, 47, 2), las = 1, cex.axis = axis.cex )
  axis(side = 1, at = seq(1,length(to.plot),4), labels = seq(to.plot[1]- pre.yrs + 1, to.plot[length(to.plot)] - pre.yrs + 1, 4*ass.freq), cex.axis = axis.cex )
  axis(side = 1, at = c(-100, 1000)); axis(side = 2, at = c(-100, 1000))
  print.letter(c("J", "K", "L")[b-3], xy = c(0.87, 0.95), cex = alpha.cex, font = 2, family = "serif")  
  
}

# Fishery Dome 
min = -15; max = 7

for (a in 4:6){

  scale.per.of = (1- med.out[[1]]$n.overfished[a,] / 100)*5 + min 
  om.scale.per.of = (1- med.out[[1]]$om.n.overfished[a,] / 100)*5 + min 
  plot(1:length(to.plot), om.scale.per.of, type = 'l', lwd = 1.5, ylim = c(min, max), axes= F)
  lines(1:length(to.plot), scale.per.of, col = 1, lty = 2)
  if(a == 4) { legend("bottomright", legend = c("OM", "EM"), lty = c(1,2), bty = 'n', cex = 0.9) }
  if(a == 5) { print.letter("% Rebuilt", xy = c(0.50, 0.05), cex = 0.9) }
  if(a == 6) { axis(side = 4, at = c(min, min + 2.5, -10), label = c("0%", "50%", "100%"), las = 1, cex.axis = axis.cex) } 
  
  est.out[[1]]$f.selex.adj.est[a,1,1,] = max + 5
  if (a == 6) {est.out[[1]]$f.selex.adj.est[a,1,,] = max + 20 }
  box95(x=t(est.out[[1]]$f.selex.adj.est[a,1,,]), xlim = c(1,18), axes = F, add = TRUE) 
  abline(h = -2.5, col = 1, lty =2)
  abline(h = -10)
  if(a == 4) { mtext(side =2, "Width at max. select.",  outer = F, line = 3, cex = lab.cex) }
  print.letter(c("M", "N", "O")[a-3], xy = c(0.87, 0.95), cex = alpha.cex, font = 2, family = "serif")  
  axis(side = 2, las = 1, at = c(-5, 0, 5))
  axis(side = 1, at = seq(1.5,length(to.plot)+0.5,4), labels = seq(to.plot[1]- pre.yrs + 1, to.plot[length(to.plot)] - pre.yrs + 1, 4*ass.freq) ) 
  axis(side = 1, at = c(-100, 1000)); axis(side = 2, at = c(-100, 1000))
}

mtext(side =1, "Assessment Year", outer = T, line = 2, cex = lab.cex)

dev.off()


###########################################################################################################################

# Supplementary Plots
drive = "C:"
run.name = "Final_w_survey"
load(paste0(drive,"/PhD/Chapter3/",run.name,"/output/rockfish_meds_all"))
load(paste0(drive,"/PhD/Chapter3/",run.name,"/output/rockfish_est_all"))
load(paste0(drive,"/PhD/Chapter3/",run.name,"/output/rockfish_om_all"))

rock.out  <- med.out <- est.out <- om.out <- list()
rock.out[[1]] <- med.out[[1]] <- meds.all
rock.out[[2]] <- est.out[[1]] <- est.all
rock.out[[3]] <- om.out[[1]]  <- om.all

#############################################################################################################################
# Figure 7
#############################################################################################################################

pdf(file = "fig7_survey_sensitivity_time_invariant.pdf", width = 6.7, height = 6.7)
par(mfrow= c(3,3), mar = c(1.5, 1.5, 1.5, 1.5), oma = c(5,5,2,5),  cex.lab = 0.8)
letter.cex = 1; axis.cex = 1; lab.cex = 0.75; alpha.cex = 1.5
ass.plot.yrs = ifelse(ass.freq == 8, 13, 26)
ass.plot.yrs = ifelse(ass.freq == 6, 18, ass.plot.yrs)
min = -1.5; max = 1.0
text.yr = c(2, 7, 11,15)
ass.num = ifelse(ass.freq == 8, 13, 26)
steep = 0.65
name.label.alt = c('Full data with survey', 'Reduced data with survey', 'Eliminated data with survey')
 
# RE SSB
for (a in 1:3){

  to.plot = ass.yrs[1:ass.plot.yrs]
  re.ssb = matrix(0, length(to.plot), N)
  for (b in 1:length(to.plot)){
    re.ssb[b,] = med.out[[1]]$re.ssb[a,b,to.plot[b],]
  }

  box95(t(re.ssb), list = F, ylim = c(-0.75, max), col = rep('grey',length(to.plot)), axes = F, add = FALSE, boxwex = rep(0.75, length(to.plot)), yaxs = "i", xaxs = 'i')
  abline(h = 0, lty = 2)

  if(a == 1) { mtext(side = 2, outer = F, "RE SB", line = 3, cex = lab.cex)} 
  axis(side = 2, at = seq(-0.5, max, 0.5), las = 1, cex.axis = axis.cex )
  axis(side = 1, at = seq(1.5,length(to.plot)+0.5,4), labels = seq(to.plot[1]- pre.yrs + 1, to.plot[length(to.plot)] - pre.yrs + 1, 4*ass.freq), cex.axis = axis.cex )
  axis(side = 1, at = c(-100, 1000)); axis(side = 2, at = c(-100, 1000))
  mtext(side = 3, outer = F, name.label.alt[a], line = 0.5, cex = 0.8) 
  print.letter(alpha.label[a], xy = c(0.87, 0.95), cex = alpha.cex, font = 2, family = "serif")  
}

# RE Depletion

for (a in 1:3){
  to.plot = ass.yrs[1:ass.plot.yrs]
  re.depl = matrix(0, length(to.plot), N)
  for (b in 1:length(to.plot)){
    re.depl[b,] = med.out[[1]]$re.depl[a,b,to.plot[b],]
  }
  if (a == 1){ re.depl[1,40] = 0.961795}
 
  box95(t(re.depl), list = F, ylim = c(-0.75, max), col = rep('grey',length(to.plot)), axes = F, add = FALSE, boxwex = rep(0.75, length(to.plot)), yaxs = "i", xaxs = 'i')
  abline(h = 0, lty = 2) 
  if(a == 1) { mtext(side = 2, outer = F, "RE Relative SB", line = 3, cex = lab.cex)} 
  axis(side = 2, at = seq(-0.5, max, 0.5), las = 1, cex.axis = axis.cex )
  axis(side = 1, at = seq(1.5,length(to.plot)+0.5,4), labels = seq(to.plot[1]- pre.yrs + 1, to.plot[length(to.plot)] - pre.yrs + 1, 4*ass.freq), cex.axis = axis.cex )
  axis(side = 1, at = c(-100, 1000)); axis(side = 2, at = c(-100, 1000))
  print.letter(c("D", "E", "F")[a], xy = c(0.87, 0.95), cex = alpha.cex, font = 2, family = "serif")  
}

# Steepness
min = -0.3; max = 1.2
for(b in 1:3){
  scale.per.of = (1- med.out[[1]]$n.overfished[a,] / 100)*0.5 + min 
  om.scale.per.of = (1- med.out[[1]]$om.n.overfished[a,] / 100)*0.5 + min 
  plot(0.5:(length(to.plot)-0.5), om.scale.per.of, type = 'l', lwd = 1.5, ylim = c(min, max), xlim = c(0, 18), axes= F)
  lines(1:length(to.plot), scale.per.of, col = 1, lty = 2)
  if(b == 1) { legend("bottomright", legend = c("OM", "EM"), lty = c(1,2), bty = 'n', cex = 1) }
  if(b == 2) { print.letter("% Rebuilt", xy = c(0.50, 0.05), cex = 1) }
  if(b == 3) { axis(side = 4, at = c(min, min + 0.25, min + 0.5), label = c("0%", "50%", "100%"), las = 1, cex.axis = axis.cex) } 

  box95(x=t(est.out[[1]]$h.est[b,,]), list = F, ylab = "Steepness", ylim = c(0.19, 1.05), col = rep('grey',ass.num), axes = F,
     boxwex = rep(0.70, length(to.plot)), add = TRUE)
  abline(h = 0.20); abline(h = steep, lty = 2, col = 1)
  print.letter(c("G", "H", "I")[b], xy = c(0.87, 0.95), cex = alpha.cex, font = 2, family = "serif")   
  axis(side = 2, at = c(0.20, 0.40, 0.60, 0.80, 1.0), las = 1, cex.axis = axis.cex )
  axis(side = 1, at = seq(1.5, length(to.plot)+0.5, 4), labels = seq(to.plot[1]- pre.yrs + 1, to.plot[length(to.plot)] - pre.yrs + 1, 4*ass.freq), cex.axis = axis.cex )
  axis(side = 1, at = c(-100, 1000)); axis(side = 2, at = c(-100, 1.1), label = c("", ""), lwd.tick = -1)
  if(b == 1) { mtext(side = 2, "Steepness", outer = F, line = 3, cex = lab.cex) }
}
mtext(side = 1, "Assessment Year", outer = T, line = 1.5, cex = lab.cex)

dev.off()


###############################################################################################################################
# Figure 8: RMSE over time for spawning biomass
###############################################################################################################################

pdf(file = "fig8_rmse.pdf", width = 6.7, height = 3.5)
par(mfrow= c(1,2), mar = c(1,2,2,1), oma = c(4,4,2,4), cex.axis = 1.1, cex.lab = 1.1)
letter.cex = 1; axis.cex = 0.80; label.cex = 0.80; alpha.cex = 1.2

ass.plot.yrs = ifelse(ass.freq == 8, 13, 26)
ass.plot.yrs = ifelse(ass.freq == 6, 18, ass.plot.yrs)
min = 0; max = max(med.out[[1]]$rmse.ssb.ass[4,]+5)
pch.vec = rep(16:18,2)

for (a in 1:ds){
  index = ass.yrs - pre.yrs
  if (a == 1 || a == 4){
    plot(index, med.out[[1]]$rmse.ssb.ass[a,], type = 'l', lwd = 1, xlim = c(46, 154), ylim = c(min, max), axes = F, xaxs="i", yaxs = "i")
    points(index, med.out[[1]]$rmse.ssb.ass[a,], pch = pch.vec[a])

    if (a == 1) { mtext(side = 1, "Assessment Year", outer = T, cex = label.cex, line = 1)                  
                  mtext(side = 2, "RMSE spawning biomass", cex = label.cex, outer = T, line = 0.5)
                  #mtext(side = 3, outer = F, "Time-invariant")
                  legend("topright", legend = name.label, pch = pch.vec, bty= 'n', cex = 0.85)  
                  print.letter(alpha.label[a], xy = c(0.10, 0.95), cex = alpha.cex, font = 2, family = "serif")  
                  axis(side = 2, at = seq(0, max, 20), cex.axis = axis.cex, las = 1)
  				  axis(side = 1, at = seq(index[1], max(index), 25), labels = seq(50, 150, 25), cex.axis = axis.cex)
  				  axis(side = 1, at = c(-100, 1000)); axis(side = 2, at = c(-100, 1000))}

    if( a == 4) { print.letter(alpha.label[a-2], xy = c(0.10, 0.95), cex = alpha.cex, font = 2, family = "serif") 
				  axis(side = 2, at = seq(0, max, 20), cex.axis = axis.cex, las = 1)
  				  axis(side = 1, at = seq(index[1], max(index), 25), labels = seq(50, 150, 25), cex.axis = axis.cex)
  				  axis(side = 1, at = c(-100, 1000)); axis(side = 2, at = c(-100, 1000))}
                  #mtext(side = 3, outer = F, "Time-varying")                  

  }

  lines(index, med.out[[1]]$rmse.ssb.ass[a,], lwd = 1, col = 1)
  points(index, med.out[[1]]$rmse.ssb.ass[a,], pch = pch.vec[a], col = 1)
}

dev.off()
