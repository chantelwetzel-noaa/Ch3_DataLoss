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

#install.packages("extrafont")
#library(extrafont)
#font_import()

#setwd("E:/SyncBack/PhD/Chapter3/Final_wo_survey/JournalPlots")
#setwd(paste0(drive,"/PhD/Chapter3/WriteUp/Fishery_Bulletin_Submission/revised_submission/JournalPlots"))
setwd(paste0(drive,":/PhD/Chapter3/WriteUp/Fishery_Bulletin_Submission/FB-2017-0065-A.R1/third_revision"))

#png(filename = "fig1_selectivity.png", width = 6.7, height = 6.7, units = 'in', res = 256)
pdf(file = "fig1_selectivity.pdf", width = 6.7, height = 6.7)
par(mfrow = c(2,2), mar = c(1,1,1,2), oma = c(4,4,4,4))
axis.size = 1.1
plot(len.step, selec[1,,1], type = 'l', axes = F, xlab = "", ylab = "", lwd = 1)
box(); axis(side = 2, cex.axis = axis.size, las = 1)
mtext(side = 2, "Selectivity", line = 2.5, outer = T)
mtext(side = 3, "Historical/Rebuilt", outer = F)
print.letter("A", xy = c(0.09, 0.95), cex = 2, font = 2, family = "serif")

plot(len.step, selec[2,,1], typ = 'l', axes = F, xlab = "", ylab = "", lwd = 1)
box()
mtext(side = 3, "Overfished",  outer = F)
text(par("usr")[2]*1.05, mean(par("usr")[3:4])+0.15, "Time-invarient", srt = -90, xpd = TRUE, pos = 4, cex = 1.2)
#mtext( "time-invarient", outer = F)
#text( 0:8, par("usr")[4] + 2,  xpd = TRUE, srt = 45)
print.letter("B", xy = c(0.09, 0.95))

plot(len.step, selec[3,,1], type = 'l', axes = F, xlab = "", ylab = "", las = 1)
for(b in 2:N){
	lines(len.step, selec[3,,b])
}
box(); axis(side = 1, at = seq(20,80,20), cex.axis = axis.size) 
axis(side = 2, cex.axis = axis.size, las = 1)
print.letter("C", xy = c(0.09, 0.95))

plot(len.step, selec[4,,1], type = 'l', axes = F, xlab = "", ylab = "")
for(b in 2:N){
	lines(len.step, selec[4,,b])
}
box();axis(side = 1, at = seq(20,80,20), cex.axis = axis.size) 
mtext(side = 1, "Length (cm)", outer = T, line = 2.5)
text(par("usr")[2]*1.05, mean(par("usr")[3:4])+0.15, "Time-varying", srt = -90, xpd = TRUE, pos = 4, cex = 1.2)
#mtext(side = 4, "time-varying", outer = F)
print.letter("D", xy = c(0.09, 0.95))

dev.off()