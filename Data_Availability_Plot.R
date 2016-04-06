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
survey.start = s.age.start  = s.len.start  = 19
f.age.start  = f.len.start  = 15

y.max = 1 + 5 + 5 + 3 +2 + 3 #Catch + Survey + Comps + Comps + Survey only comps + blank lines
fl.size  = 10
flr.size = 2
fa.size  = 5
far.size = 1
lab.size = 1
# Create vectors that will be ploted for each data source
catch  <- catch.start:end.yr
f.lens <- f.len.start:end.yr
f.ages <- f.age.start:end.yr
survey <- seq(survey.start, end.yr, survey.freq)
s.lens <- seq(s.len.start, end.yr, survey.freq)
s.ages <- seq(s.age.start, end.yr, survey.freq)

label<-c("Catch", "Survey Index", "Fishery Lengths", "Survey Lengths", "Fishery Ages", "Survey Ages", #DS 1
				  "Survey Index", "Fishery Lengths", "Survey Lengths", "Fishery Ages", "Survey Ages", #DS 2
				  "Survey Index", "Fishery Lengths", "Survey Lengths", "Fishery Ages", "Survey Ages") #DS 3


#symbols(x=x, y=y, circles=sqrt(size.cex)*maxsize,
#                      bg=adjustcolor(fleetcol[fleets==ifleet], alpha.f=alphasize),
#                      add=TRUE, inches=FALSE)


linecol1 <- rgb(0.3,0.3,0.3,1)
linecol2 <- rgb(0.3,0.3,0.3,.75) # dark grey with alpha (opacity) = 0.5
linecol3 <- rgb(0.3,0.3,0.3,.50) # black with alpha (opacity) = 0.5
linecol4 <- rgb(0.3,0.3,0.3,.35)

#png("DataScenarios.png",width=7,height=7,res=300,units="in") 

#Catch
	windows(width=9,height=7)
	par(mar=c(5.1,2.1,2.1,4.1))
	plot(catch, rep(max(y.max), end.yr), type="l", lwd=14, ylim=c(1,y.max), xlim=c(-1, end.yr + 25), xaxs="i", 
		col=linecol1,axes=FALSE, xlab="",ylab="")
	axis(side = 1, at = c(1, 20, 40, 60, 80, end.yr + 25), labels = c( 1, 25, 50, "Overfished", "Rebuilt", "" ), font =2 )
	mtext(side = 1,"Year", line=2.5, font = 2)
	text(end.yr + 16, max(y.max), label[1], pos = 2, font = 2, col = 1, cex = lab.size) 
	abline(v = end.yr + 2, lty=1); 
	#abline(h = max(y.max) - 0.20, lty=3)
	abline(h = max(y.max) - 0.5, lty=3)

# No reduction in sampling data scenario
	text(25, y.max - 1,"No Reduction in Data", font = 2, pos = 4, cex = lab.size)
	# Survey
		points(survey, rep(max(y.max) - 2, length(survey)), type = "p", pch = 16, lwd = 2, col = linecol2)
		text(end.yr + 28,  max(y.max) - 2, label[2], pos=2, col=1, font=2, cex=lab.size)
	#Fishery Lengths
		lines(f.lens,  rep(max(y.max) - 3, length(f.lens)), type="l", lwd=fl.size, col=linecol2)
		text(end.yr + 33,  max(y.max) - 3, label[3], pos=2, col=1, font=2, cex=lab.size)
	#Survey Lengths
		points(s.lens, rep(max(y.max) - 4, length(s.lens)), type="p", pch = 16, lwd=far.size, col=linecol3)
		text(end.yr + 32,  max(y.max) - 4, label[4], pos=2, col=1, font=2, cex=lab.size)
	#Fishery Ages
		lines(f.ages,  rep(max(y.max) - 5, length(f.ages)), type="l", lwd=fl.size, col=linecol2)
		text(end.yr + 28,  max(y.max) - 5, label[5], pos=2, col=1, font=2, cex=lab.size)
	#Survey Ages
		points(s.lens, rep(max(y.max) - 6, length(s.ages)), type="p", pch = 16, lwd=far.size, col=linecol3)
		text(end.yr + 27,  max(y.max) - 6, label[6], pos=2, col=1, font=2, cex=lab.size)
	abline(h = max(y.max) - 6.5, lty=3)

# Reduction in Data 
	text(25, y.max - 7, "Reduction in Fishery Data", font = 2, pos = 4, cex = lab.size)
	# Survey
		points(survey, rep(max(y.max) - 8, length(survey)), type = "p", pch = 16, lwd = 2, col = linecol2)
		text(end.yr + 28,  max(y.max) - 8, label[7], pos=2, col=1, font=2, cex=lab.size)
	#Fishery Lengths
		lines(f.len.start:40,   rep(max(y.max) - 9, length(f.len.start:40)), type="l", lwd=fl.size, col=linecol2)		
		lines(41:80,            rep(max(y.max) - 9, 40), type="l", lwd=flr.size, col=linecol2)
		lines(81:end.yr,        rep(max(y.max) - 9, length(81:end.yr)), type="l", lwd=fl.size, col=linecol2)
		text(end.yr + 33,           max(y.max) - 9, label[8], pos=2, col=1, font=2, cex=lab.size)
	#Survey Lengths
		points(s.lens, rep(max(y.max) - 10, length(s.lens)), type="p", pch = 16, lwd=far.size, col=linecol3)
		text(end.yr + 32,  max(y.max) - 10, label[9], pos=2, col=1, font=2, cex=lab.size)
	#Fishery Ages
		lines(f.age.start:40,   rep(max(y.max) - 11, length(f.age.start:40)), type="l", lwd=fa.size, col=linecol2)		
		lines(41:80,            rep(max(y.max) - 11, 40), type="l", lwd=far.size, col=linecol2)
		lines(81:end.yr,        rep(max(y.max) - 11, length(81:end.yr)), type="l", lwd=fa.size, col=linecol2)
		text(end.yr + 28,           max(y.max) - 11, label[10], pos=2, col=1, font=2, cex=lab.size)
	#Survey Ages
		points(s.lens, rep(max(y.max) - 12, length(s.ages)), type="p", pch = 16, lwd=far.size, col=linecol3)
		text(end.yr + 27,  max(y.max) - 12, label[11], pos=2, col=1, font=2, cex=lab.size)
	abline(h = max(y.max) - 12.5, lty=3)

# Loss of Fishery Data
	text(28, y.max - 13, "Loss of Fishery Data", font = 2, pos = 4)
	# Survey
		points(survey, rep(max(y.max) - 14, length(survey)), type = "p", pch = 16, lwd = 2, col = linecol2)
		text(end.yr + 28,  max(y.max) - 14, label[12], pos=2, col=1, font=2, cex=lab.size)
	#Fishery Lengths
		lines(f.len.start:40,   rep(max(y.max) - 15, length(f.len.start:40)), type="l", lwd=fl.size, col=linecol2)		
		lines(81:end.yr,        rep(max(y.max) - 15, length(81:end.yr)), type="l", lwd=fl.size, col=linecol2)
		text(end.yr + 33,           max(y.max) - 15, label[13], pos=2, col=1, font=2, cex=lab.size)
	#Survey Lengths
		points(s.lens, rep(max(y.max) - 16, length(s.lens)), type="p", pch = 16, lwd=far.size, col=linecol3)
		text(end.yr + 32,  max(y.max) - 16, label[14], pos=2, col=1, font=2, cex=lab.size)
	#Fishery Ages
		lines(f.age.start:40,   rep(max(y.max) - 17, length(f.age.start:40)), type="l", lwd=fa.size, col=linecol2)		
		lines(81:end.yr,        rep(max(y.max) - 17, length(81:end.yr)), type="l", lwd=fa.size, col=linecol2)
		text(end.yr + 28,           max(y.max) - 17, label[15], pos=2, col=1, font=2, cex=lab.size)
	#Survey Ages
		points(s.lens, rep(max(y.max) - 18, length(s.ages)), type="p", pch = 16, lwd=far.size, col=linecol3)
		text(end.yr + 27,  max(y.max) - 18, label[16], pos=2, col=1, font=2, cex=lab.size)
