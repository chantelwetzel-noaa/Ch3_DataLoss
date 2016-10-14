##########################################################
##########################################################
##													   	##
##				Data Availability Plot				   	##
##						Chapter 3						##
##		       Created: March 31, 2016				   	##
##				   Chantel Wetzel					   	##
##														##
##########################################################
##########################################################


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

name.label = c('full data', 'data reduction', 'historical data')

label<-c("catch", "survey index", "fishery lengths", "survey lengths", "fishery ages", "survey ages", #DS 1
				  "survey index", "fishery lengths", "survey lengths", "fishery ages", "survey ages", #DS 2
				  "survey index", "fishery lengths", "survey lengths", "fishery ages", "survey ages") #DS 3

label<-c("catch", "survey index",  "survey lengths",  "survey ages", #DS 1
				  "fishery cpue", "fishery lengths",  "fishery ages",  #DS 2
				  "fishery cpue", "fishery lengths",  "fishery ages" ) #DS 3


#symbols(x=x, y=y, circles=sqrt(size.cex)*maxsize,
#                      bg=adjustcolor(fleetcol[fleets==ifleet], alpha.f=alphasize),
#                      add=TRUE, inches=FALSE)

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
lab.cex = 0.95

setwd("C:/PhD/Chapter3/WriteUp/Plots")
png("DataScenarios.png",width=7,height=7,res=300,units="in") 

#Catch
	#windows(width=9,height=7)
	par(mar=c(5.1,2.1,2.1,4.1))
	plot(catch, rep(max(y.max), end.yr), type="l", lwd=14, ylim=c(1,y.max), xlim=c(-1, end.yr + 25), xaxs="i", 
		col=linecol1,axes=FALSE, xlab="",ylab="")
	axis(side = 1, at = c(0, 20, 60, 90, end.yr + 25), labels = c( "","Historical", "Overfished", "Rebuilt", "" ), font =2 )
	mtext(side = 1,"Time period", line=2.5, font = 2)
	#text(end.yr + 12, max(y.max), label[1], pos = 2, font = 2, col = 1, cex = lab.size) 
	print.letter(xy = c(0.86, 0.965), label[1], cex = lab.cex)
	abline(v = end.yr + 2, lty=1)
	abline(v = 40, lty = 3)
	abline(v = 80, lty = 3)

	# Survey
		points(survey, rep(max(y.max) - 1, length(survey)), type = "p", pch = 16, lwd = 2, col = linecol3)
		#text(end.yr + 21,  max(y.max) - 1, label[2], pos=2, col=1, font=2, cex=lab.size)
		print.letter(xy = c(0.905, 0.89), label[2], cex = lab.cex)
		print.letter(xy = c(0.27, 0.92), "CV = 0.40", cex = samp.cex)

	#Survey Lengths
		points(s.lens, rep(max(y.max) - 2, length(s.lens)), type="p", pch = 16, lwd=far.size, col=linecol3)
		#text(end.yr + 23.5,  max(y.max) - 2, label[3], pos=2, col=1, font=2, cex=lab.size)
		print.letter(xy = c(0.915, 0.81), label[3], cex = lab.cex)
		print.letter(xy = c(0.29, 0.83), "N = 10", cex = samp.cex)

	#Survey Ages
		points(s.lens, rep(max(y.max) - 3, length(s.ages)), type="p", pch = 16, lwd=far.size, col=linecol3)
		#text(end.yr + 20,  max(y.max) - 3, label[4], pos=2, col=1, font=2, cex=lab.size)
		print.letter(xy = c(0.29, 0.76), "N = 10", cex = samp.cex)
		print.letter(xy = c(0.90, 0.74), label[4], cex = lab.cex)
	abline(h = max(y.max) - 3.3, lty=3)

# No reduction in sampling data scenario
	text(-1, y.max - 4,"full data", font = 2, pos = 4, cex = lab.size)

	# Fishery CPUE
		points(cpue.start:40,   rep(max(y.max) - 4, length(cpue.start:40)), type = "l", lwd = cpue.size, col = linecol2)
		points(41:80,           rep(max(y.max) - 4, 40), type = "l", lwd = cpue.size + 5, col = linecol2)
		points(81:end.yr,        rep(max(y.max) - 4, length(81:end.yr)), type = "l", lwd = cpue.size + 2, col = linecol2)
		#text(end.yr + 21,  max(y.max) - 4, label[5], pos=2, col=1, font=2, cex=lab.size)
		print.letter(xy = c(0.90, 0.665), label[5], cex = lab.cex)
		print.letter(xy = c(0.27, 0.69), "CV = 0.30", cex = samp.cex)
		print.letter(xy = c(0.51, 0.69), "CV = 0.50", cex = samp.cex)
		print.letter(xy = c(0.71, 0.69), "CV = 0.30", cex = samp.cex)

	#Fishery Lengths
		lines(f.lens,  rep(max(y.max) - 5, length(f.lens)), type="l", lwd=fl.size, col=linecol2)
		#text(end.yr + 24,  max(y.max) - 5, label[6], pos=2, col=1, font=2, cex=lab.size)
		print.letter(xy = c(0.91, 0.585), label[6], cex = lab.cex)
		print.letter(xy = c(0.29, 0.61), "N = 50", cex = samp.cex)

	#Fishery Ages
		lines(f.ages,  rep(max(y.max) - 6, length(f.ages)), type="l", lwd=fl.size, col=linecol2)
		#text(end.yr + 21,  max(y.max) - 6, label[7], pos=2, col=1, font=2, cex=lab.size)
		print.letter(xy = c(0.29, 0.53), "N = 25", cex = samp.cex)
		print.letter(xy = c(0.90, 0.51), label[7], cex = lab.cex)
	abline(h = max(y.max) - 6.2, lty=3)

# Reduction in Data 
	text(-1, y.max - 7, "reduced data", font = 2, pos = 4, cex = lab.size)

	# Fishery CPUE
		points(cpue.start:39,   rep(max(y.max) - 7, length(cpue.start:39)), type = "l", lwd = cpue.size, col = linecol2)
		lines(81:end.yr,        rep(max(y.max) - 7, length(81:end.yr)), type = "l", lwd = cpue.size, col = linecol2)
		#text(end.yr + 21,  max(y.max) - 7, label[8], pos=2, col=1, font=2, cex=lab.size)
		print.letter(xy = c(0.90, 0.42), label[5], cex = lab.cex)
		print.letter(xy = c(0.27, 0.45), "CV = 0.30", cex = samp.cex)
		print.letter(xy = c(0.71, 0.45), "CV = 0.30", cex = samp.cex)

	#Fishery Lengths
		lines(f.len.start:39,   rep(max(y.max) - 8, length(f.len.start:39)), type="l", lwd=fl.size, col=linecol2)		
		lines(41:80,            rep(max(y.max) - 8, 40), type="l", lwd=flr.size, col=linecol2)
		lines(81:end.yr,        rep(max(y.max) - 8, length(81:end.yr)), type="l", lwd=fl.size, col=linecol2)
		#text(end.yr + 24,           max(y.max) - 8, label[9], pos=2, col=1, font=2, cex=lab.size)
		print.letter(xy = c(0.91, 0.35), label[6], cex = lab.cex)
		print.letter(xy = c(0.29, 0.38), "N = 50", cex = samp.cex)
		print.letter(xy = c(0.5, 0.38), "N = 10", cex = samp.cex)
		print.letter(xy = c(0.7, 0.38), "N = 50", cex = samp.cex)

	#Fishery Ages
		lines(f.age.start:39,   rep(max(y.max) - 9, length(f.age.start:39)), type="l", lwd=fa.size, col=linecol2)		
		lines(41:80,            rep(max(y.max) - 9, 40), type="l", lwd=far.size, col=linecol2)
		lines(81:end.yr,        rep(max(y.max) - 9, length(81:end.yr)), type="l", lwd=fa.size, col=linecol2)
		#text(end.yr + 21,           max(y.max) - 9, label[10], pos=2, col=1, font=2, cex=lab.size)
		print.letter(xy = c(0.90, 0.27), label[7], cex = lab.cex)
		print.letter(xy = c(0.29, 0.30), "N = 25", cex = samp.cex)
		print.letter(xy = c(0.5, 0.30), "N = 5", cex = samp.cex)
		print.letter(xy = c(0.7, 0.30), "N = 25", cex = samp.cex)

	abline(h = max(y.max) - 9.2, lty=3)

# Loss of Fishery Data
	text(-1, y.max - 10, "historical data", font = 2, pos = 4)

	# Fishery CPUE
		points(cpue.start:39,   rep(max(y.max) - 10, length(cpue.start:39)), type = "l", lwd = cpue.size, col = linecol2)
		lines(81:end.yr,        rep(max(y.max) - 10, length(81:end.yr)), type = "l", lwd = cpue.size, col = linecol2)
		#text(end.yr + 21,  max(y.max) - 10, label[8], pos=2, col=1, font=2, cex=lab.size)
		print.letter(xy = c(0.90, 0.19), label[5], cex = lab.cex)
		print.letter(xy = c(0.27, 0.22), "CV = 0.30", cex = samp.cex)
		print.letter(xy = c(0.71, 0.22), "CV = 0.30", cex = samp.cex)

	#Fishery Lengths
		lines(f.len.start:39,   rep(max(y.max) - 11, length(f.len.start:39)), type="l", lwd=fl.size, col=linecol2)		
		lines(81:end.yr,        rep(max(y.max) - 11, length(81:end.yr)), type="l", lwd=fl.size, col=linecol2)
		#text(end.yr + 24,           max(y.max) - 11, label[9], pos=2, col=1, font=2, cex=lab.size)
		print.letter(xy = c(0.91, 0.12), label[6], cex = lab.cex)
		print.letter(xy = c(0.29, 0.15), "N = 50", cex = samp.cex)
		print.letter(xy = c(0.70, 0.15), "N = 50", cex = samp.cex)

	#Fishery Ages
		lines(f.age.start:39,   rep(max(y.max) - 12, length(f.age.start:39)), type="l", lwd=fa.size, col=linecol2)		
		lines(81:end.yr,        rep(max(y.max) - 12, length(81:end.yr)), type="l", lwd=fa.size, col=linecol2)
		#text(end.yr + 21,           max(y.max) - 12, label[10], pos=2, col=1, font=2, cex=lab.size)
		print.letter(xy = c(0.90, 0.04), label[7], cex = lab.cex)
		print.letter(xy = c(0.29, 0.07), "N = 25", cex = samp.cex)
		print.letter(xy = c(0.70, 0.07), "N = 25", cex = samp.cex)

dev.off()

setwd("C:/PhD/Chapter3/WriteUp/Plots")
setwd("C:/Users/chantell.wetzel/Documents/GitHub/Ch3_DataLoss/WriteUp/Journal_Submission/JournalPlots")
png("supp1_datascenarios.png",width=7,height=7,res=300,units="in") 

#Catch
	#windows(width=9,height=7)
	par(mar=c(5.1,2.1,2.1,4.1))
	plot(catch, rep(max(y.max), end.yr), type="l", lwd=14, ylim=c(1,y.max), xlim=c(-1, end.yr + 25), xaxs="i", 
		col=linecol1,axes=FALSE, xlab="",ylab="")
	axis(side = 1, at = c(0, 20, 60, 90, end.yr + 25), labels = c( "","Historical", "Overfished", "Rebuilt", "" ), font =2 )
	mtext(side = 1,"Time period", line=2.5, font = 2)
	#text(end.yr + 12, max(y.max), label[1], pos = 2, font = 2, col = 1, cex = lab.size) 
	print.letter(xy = c(0.86, 0.965), label[1], cex = lab.cex)
	abline(v = end.yr + 2, lty=1)
	abline(v = 40, lty = 3)
	abline(v = 80, lty = 3)

	# Survey
		points(survey, rep(max(y.max) - 1, length(survey)), type = "p", pch = 16, lwd = 2, col = linecol3)
		#text(end.yr + 21,  max(y.max) - 1, label[2], pos=2, col=1, font=2, cex=lab.size)
		print.letter(xy = c(0.905, 0.89), label[2], cex = lab.cex)
		print.letter(xy = c(0.27, 0.92), "CV = 0.40", cex = samp.cex)

	#Survey Lengths
		points(s.lens, rep(max(y.max) - 2, length(s.lens)), type="p", pch = 16, lwd=far.size, col=linecol3)
		#text(end.yr + 23.5,  max(y.max) - 2, label[3], pos=2, col=1, font=2, cex=lab.size)
		print.letter(xy = c(0.915, 0.81), label[3], cex = lab.cex)
		print.letter(xy = c(0.29, 0.83), "N = 10", cex = samp.cex)

	#Survey Ages
		points(s.lens, rep(max(y.max) - 3, length(s.ages)), type="p", pch = 16, lwd=far.size, col=linecol3)
		#text(end.yr + 20,  max(y.max) - 3, label[4], pos=2, col=1, font=2, cex=lab.size)
		print.letter(xy = c(0.29, 0.76), "N = 10", cex = samp.cex)
		print.letter(xy = c(0.90, 0.74), label[4], cex = lab.cex)
	abline(h = max(y.max) - 3.3, lty=3)

# No reduction in sampling data scenario
	text(-1, y.max - 4,"full data", font = 2, pos = 4, cex = lab.size)

	# Fishery CPUE
		points(cpue.start:end.yr,   rep(max(y.max) - 4, length(cpue.start:end.yr)), type = "l", lwd = cpue.size, col = linecol2)
		#text(end.yr + 21,  max(y.max) - 4, label[5], pos=2, col=1, font=2, cex=lab.size)
		print.letter(xy = c(0.90, 0.665), label[5], cex = lab.cex)
		print.letter(xy = c(0.27, 0.69), "CV = 0.30", cex = samp.cex)

	#Fishery Lengths
		lines(f.lens,  rep(max(y.max) - 5, length(f.lens)), type="l", lwd=fl.size, col=linecol2)
		#text(end.yr + 24,  max(y.max) - 5, label[6], pos=2, col=1, font=2, cex=lab.size)
		print.letter(xy = c(0.91, 0.585), label[6], cex = lab.cex)
		print.letter(xy = c(0.29, 0.61), "N = 50", cex = samp.cex)

	#Fishery Ages
		lines(f.ages,  rep(max(y.max) - 6, length(f.ages)), type="l", lwd=fa.size, col=linecol2)
		#text(end.yr + 21,  max(y.max) - 6, label[7], pos=2, col=1, font=2, cex=lab.size)
		print.letter(xy = c(0.29, 0.53), "N = 25", cex = samp.cex)
		print.letter(xy = c(0.90, 0.51), label[7], cex = lab.cex)
	abline(h = max(y.max) - 6.2, lty=3)

# Reduction in Data 
	text(-1, y.max - 7, "reduced data", font = 2, pos = 4, cex = lab.size)

	# Fishery CPUE
		points(cpue.start:39,   rep(max(y.max) - 7, length(cpue.start:39)), type = "l", lwd = cpue.size, col = linecol2)
		lines(81:end.yr,        rep(max(y.max) - 7, length(81:end.yr)), type = "l", lwd = cpue.size, col = linecol2)
		#text(end.yr + 21,  max(y.max) - 7, label[8], pos=2, col=1, font=2, cex=lab.size)
		print.letter(xy = c(0.90, 0.42), label[5], cex = lab.cex)
		print.letter(xy = c(0.27, 0.45), "CV = 0.30", cex = samp.cex)
		print.letter(xy = c(0.71, 0.45), "CV = 0.30", cex = samp.cex)

	#Fishery Lengths
		lines(f.len.start:39,   rep(max(y.max) - 8, length(f.len.start:39)), type="l", lwd=fl.size, col=linecol2)		
		lines(41:80,            rep(max(y.max) - 8, 40), type="l", lwd=flr.size, col=linecol2)
		lines(81:end.yr,        rep(max(y.max) - 8, length(81:end.yr)), type="l", lwd=fl.size, col=linecol2)
		#text(end.yr + 24,           max(y.max) - 8, label[9], pos=2, col=1, font=2, cex=lab.size)
		print.letter(xy = c(0.91, 0.35), label[6], cex = lab.cex)
		print.letter(xy = c(0.29, 0.38), "N = 50", cex = samp.cex)
		print.letter(xy = c(0.5, 0.38), "N = 10", cex = samp.cex)
		print.letter(xy = c(0.7, 0.38), "N = 50", cex = samp.cex)

	#Fishery Ages
		lines(f.age.start:39,   rep(max(y.max) - 9, length(f.age.start:39)), type="l", lwd=fa.size, col=linecol2)		
		lines(41:80,            rep(max(y.max) - 9, 40), type="l", lwd=far.size, col=linecol2)
		lines(81:end.yr,        rep(max(y.max) - 9, length(81:end.yr)), type="l", lwd=fa.size, col=linecol2)
		#text(end.yr + 21,           max(y.max) - 9, label[10], pos=2, col=1, font=2, cex=lab.size)
		print.letter(xy = c(0.90, 0.27), label[7], cex = lab.cex)
		print.letter(xy = c(0.29, 0.30), "N = 25", cex = samp.cex)
		print.letter(xy = c(0.5, 0.30), "N = 5", cex = samp.cex)
		print.letter(xy = c(0.7, 0.30), "N = 25", cex = samp.cex)

	abline(h = max(y.max) - 9.2, lty=3)

# Loss of Fishery Data
	text(-1, y.max - 10, "eliminated data", font = 2, pos = 4)

	# Fishery CPUE
		points(cpue.start:39,   rep(max(y.max) - 10, length(cpue.start:39)), type = "l", lwd = cpue.size, col = linecol2)
		lines(81:end.yr,        rep(max(y.max) - 10, length(81:end.yr)), type = "l", lwd = cpue.size, col = linecol2)
		#text(end.yr + 21,  max(y.max) - 10, label[8], pos=2, col=1, font=2, cex=lab.size)
		print.letter(xy = c(0.90, 0.19), label[5], cex = lab.cex)
		print.letter(xy = c(0.27, 0.22), "CV = 0.30", cex = samp.cex)
		print.letter(xy = c(0.71, 0.22), "CV = 0.30", cex = samp.cex)

	#Fishery Lengths
		lines(f.len.start:39,   rep(max(y.max) - 11, length(f.len.start:39)), type="l", lwd=fl.size, col=linecol2)		
		lines(81:end.yr,        rep(max(y.max) - 11, length(81:end.yr)), type="l", lwd=fl.size, col=linecol2)
		#text(end.yr + 24,           max(y.max) - 11, label[9], pos=2, col=1, font=2, cex=lab.size)
		print.letter(xy = c(0.91, 0.12), label[6], cex = lab.cex)
		print.letter(xy = c(0.29, 0.15), "N = 50", cex = samp.cex)
		print.letter(xy = c(0.70, 0.15), "N = 50", cex = samp.cex)

	#Fishery Ages
		lines(f.age.start:39,   rep(max(y.max) - 12, length(f.age.start:39)), type="l", lwd=fa.size, col=linecol2)		
		lines(81:end.yr,        rep(max(y.max) - 12, length(81:end.yr)), type="l", lwd=fa.size, col=linecol2)
		#text(end.yr + 21,           max(y.max) - 12, label[10], pos=2, col=1, font=2, cex=lab.size)
		print.letter(xy = c(0.90, 0.04), label[7], cex = lab.cex)
		print.letter(xy = c(0.29, 0.07), "N = 25", cex = samp.cex)
		print.letter(xy = c(0.70, 0.07), "N = 25", cex = samp.cex)

dev.off()


y.min = 1
y.max = 10 
cpue.size = 5
fl.size  = 10
flr.size = 2
fa.size  = 5
far.size = 1
lab.size = 1

name.label = c('full data', 'data reduction', 'historical data')


label<-c("catch", "fishery cpue", "fishery lengths",  "fishery ages") 

setwd("C:/PhD/Chapter3/WriteUp/Plots")
setwd("C:/Users/chantell.wetzel/Documents/GitHub/Ch3_DataLoss/WriteUp/Journal_Submission/JournalPlots")

png("fig2_datascenario.png",width=7,height=4.5,res=300,units="in") 
#Catch
	#windows(width=9,height=7)
	par(mar=c(5.1,2.1,2.1,4.1))
	plot(catch, rep(max(y.max), end.yr), type="l", lwd=14, ylim=c(1,y.max), xlim=c(-1, end.yr + 25), xaxs="i", 
		col=linecol1,axes=FALSE, xlab="",ylab="")
	axis(side = 1, at = c(0, 20, 60, 90, end.yr + 25), labels = c( "","Historical", "Overfished", "Rebuilt", "" ), font =2 )
	mtext(side = 1,"Time period", line=2.5, font = 2)
	print.letter(xy = c(0.86, 0.965), label[1], cex = lab.cex)
	abline(v = end.yr + 2, lty=1)
	abline(v = 40, lty = 3)
	abline(v = 80, lty = 3)
	abline(h = max(y.max) - 0.4, lty =3)

# No reduction in sampling data scenario
	text(-2, y.max - 1,"full data", font = 2, pos = 4, cex = lab.size)

	# Fishery CPUE
		points(cpue.start:end.yr,   rep(max(y.max) - 1, length(cpue.start:end.yr)), type = "l", lwd = cpue.size, col = linecol2)
		print.letter(xy = c(0.90, 0.86), label[2], cex = lab.cex)
		print.letter(xy = c(0.27, 0.89), "CV = 0.30", cex = samp.cex)

	#Fishery Lengths
		lines(f.lens,  rep(max(y.max) - 2, length(f.lens)), type="l", lwd=fl.size, col=linecol2)
		print.letter(xy = c(0.91, 0.76), label[3], cex = lab.cex)
		print.letter(xy = c(0.29, 0.81), "N = 50", cex = samp.cex)

	#Fishery Ages
		lines(f.ages,  rep(max(y.max) - 3, length(f.ages)), type="l", lwd=fa.size, col=linecol2)
		print.letter(xy = c(0.90, 0.65), label[4], cex = lab.cex)
		print.letter(xy = c(0.29, 0.71), "N = 25", cex = samp.cex)

	abline(h = max(y.max) - 3.4, lty=3)

# Reduction in Data 
	text(-2, y.max - 4, "reduced data", font = 2, pos = 4, cex = lab.size)

	# Fishery CPUE
		points(cpue.start:39,   rep(max(y.max) - 4, length(cpue.start:39)), type = "l", lwd = cpue.size, col = linecol2)
		lines(81:end.yr,        rep(max(y.max) - 4, length(81:end.yr)), type = "l", lwd = cpue.size, col = linecol2)
		print.letter(xy = c(0.90, 0.555), label[2], cex = lab.cex)
		print.letter(xy = c(0.27, 0.59), "CV = 0.30", cex = samp.cex)
		print.letter(xy = c(0.71, 0.59), "CV = 0.30", cex = samp.cex)

	#Fishery Lengths
		lines(f.len.start:39,   rep(max(y.max) - 5, length(f.len.start:39)), type="l", lwd=fl.size, col=linecol2)		
		lines(41:80,            rep(max(y.max) - 5, 40), type="l", lwd=flr.size, col=linecol2)
		lines(81:end.yr,        rep(max(y.max) - 5, length(81:end.yr)), type="l", lwd=fl.size, col=linecol2)
		print.letter(xy = c(0.91, 0.45), label[3], cex = lab.cex)
		print.letter(xy = c(0.29, 0.50), "N = 50", cex = samp.cex)
		print.letter(xy = c(0.50, 0.50), "N = 10", cex = samp.cex)
		print.letter(xy = c(0.70, 0.50), "N = 50", cex = samp.cex)

	#Fishery Ages
		lines(f.age.start:39,   rep(max(y.max) - 6, length(f.age.start:39)), type="l", lwd=fa.size, col=linecol2)		
		lines(41:80,            rep(max(y.max) - 6, 40), type="l", lwd=far.size, col=linecol2)
		lines(81:end.yr,        rep(max(y.max) - 6, length(81:end.yr)), type="l", lwd=fa.size, col=linecol2)
		print.letter(xy = c(0.90, 0.35), label[4], cex = lab.cex)
		print.letter(xy = c(0.29, 0.395), "N = 25", cex = samp.cex)
		print.letter(xy = c(0.50, 0.395), "N = 5", cex = samp.cex)
		print.letter(xy = c(0.70, 0.395), "N = 25", cex = samp.cex)

	abline(h = max(y.max) - 6.4, lty=3)

# Loss of Fishery Data
	text(-2, y.max - 7, "eliminated data", font = 2, pos = 4)

	# Fishery CPUE
		points(cpue.start:39,   rep(max(y.max) - 7, length(cpue.start:39)), type = "l", lwd = cpue.size, col = linecol2)
		lines(81:end.yr,        rep(max(y.max) - 7, length(81:end.yr)), type = "l", lwd = cpue.size, col = linecol2)
		print.letter(xy = c(0.90, 0.25), label[2], cex = lab.cex)
		print.letter(xy = c(0.27, 0.275), "CV = 0.30", cex = samp.cex)
		print.letter(xy = c(0.71, 0.275), "CV = 0.30", cex = samp.cex)

	#Fishery Lengths
		lines(f.len.start:39,   rep(max(y.max) - 8, length(f.len.start:39)), type="l", lwd=fl.size, col=linecol2)		
		lines(81:end.yr,        rep(max(y.max) - 8, length(81:end.yr)), type="l", lwd=fl.size, col=linecol2)
		print.letter(xy = c(0.91, 0.14), label[3], cex = lab.cex)
		print.letter(xy = c(0.29, 0.19), "N = 50", cex = samp.cex)
		print.letter(xy = c(0.70, 0.19), "N = 50", cex = samp.cex)

	#Fishery Ages
		lines(f.age.start:39,   rep(max(y.max) - 9, length(f.age.start:39)), type="l", lwd=fa.size, col=linecol2)		
		lines(81:end.yr,        rep(max(y.max) - 9, length(81:end.yr)), type="l", lwd=fa.size, col=linecol2)
		#text(end.yr + 21,           max(y.max) - 12, label[10], pos=2, col=1, font=2, cex=lab.size)
		print.letter(xy = c(0.90, 0.04), label[4], cex = lab.cex)
		print.letter(xy = c(0.29, 0.075), "N = 25", cex = samp.cex)
		print.letter(xy = c(0.70, 0.075), "N = 25", cex = samp.cex)

dev.off()