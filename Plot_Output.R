############################################
#   Chapter 3: Loss of Data & Rebuilding   #
#                                          #
#    Create Plots from R object created    #
#          by Simulation_Eval code         #
#         Created July 31, 2014 by         #
#             Chantel Wetzel               #
############################################


#Load in the R objects from the Simulation Eval Code ========================================
drive = "C:"
run.name = "Final_wo_survey"#"CPUE_smallN_AE"#"Ass_Freq"#"CPUE_smallN_AE" #"April16_PreCPUE"#"Fall2015"
setwd(paste0(drive,"/PhD/Chapter3/", run.name, "/Plots"))
load(paste0(drive,"/PhD/Chapter3/",run.name,"/output/rockfish_meds_all"))
load(paste0(drive,"/PhD/Chapter3/",run.name,"/output/rockfish_est_all"))
load(paste0(drive,"/PhD/Chapter3/",run.name,"/output/rockfish_om_all"))

source(paste0(drive, "/Users/Chantell.Wetzel/Documents/GitHub/Ch3_DataLoss/box95.R"))

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

alpha.label = c('(a)', '(b)', '(c)', '(d)', '(e)', '(f)')
name.label = c('full data', 'reduced data', 'eliminated data')

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

# Relative error about spawning biomas==================================================================

png(filename = "RE_SSB_full.png", width = 6.7, height = 5, units = 'in', res = 256)

par(mfrow= c(2,3), mar = c(0.2,0.2,0.2,0.2), oma = c(5,5,2,5), cex.axis = 1.1, cex.lab = 1.1)
letter.cex = 1; axis.cex = 1.2; label.cex = 0.8
ass.plot.yrs = ifelse(ass.freq == 8, 13, 26)
ass.plot.yrs = ifelse(ass.freq == 6, 18, ass.plot.yrs)
min = -1; max = 1.25
text.yr = c(2, 7, 11,15)
 
for (a in 1:ds){
  to.plot = ass.yrs[1:ass.plot.yrs]
  re.ssb = matrix(0, length(to.plot), N)
  for (b in 1:length(to.plot)){
    re.ssb[b,] = med.out[[1]]$re.ssb[a,b,to.plot[b],]
  }
  per.of = med.out[[1]]$n.overfished[a,text.yr] 
  boxplot(t(re.ssb), ylim = c(min, max), col = rep('grey',length(to.plot)), axes= F)
  box(); abline(h = 0)
  text( labels = paste0(per.of, "%"), x = text.yr, y = rep(-1, length(text.yr)), cex = 1 )
  if( a ==1 || a==4) { axis(side = 2, las = 1)
      mtext(side = 2, outer = T, "RE spawning biomass", line = 3)} 
  if( a > 3){
    axis(side = 1, at = seq(1,length(to.plot),4), 
        labels = seq(to.plot[1]- pre.yrs + 1, to.plot[length(to.plot)] - pre.yrs + 1, 4*ass.freq) )
    mtext(side = 1, outer = T, "Assessment year", line = 3)
  }
  if (a < 4) { mtext(side = 3, outer = F, name.label[a]) }
  if (a == 3 ){ mtext(side = 4, outer = F, "time-invariant", line = 1) }
  #text(par("usr")[2]*1.03, mean(par("usr")[3:4])+0.1, "time-invarient", srt = -90, xpd = TRUE, pos = 4, cex = 1.5)}
  if (a == 6 ){ mtext(side = 4, outer = F, "time-varying", line = 1) }
  #text(par("usr")[2]*1.03, mean(par("usr")[3:4])+0.1, "time-varying", srt = -90, xpd = TRUE, pos = 4, cex = 1.5)}
  print.letter(alpha.label[a], xy = c(0.95, 0.95), cex = 1.1)  
}

dev.off()


png(filename = "RE_SSB_cumm.png", width = 6.7, height = 6, units = 'in', res = 256)

par(mfrow= c(2,3), mar = c(0.2,0.2,0.2,0.2), oma = c(5,5,2,5), cex.axis = 1.1, cex.lab = 1.1)
letter.cex = 1; axis.cex = 1.2; label.cex = 0.8
ass.plot.yrs = ifelse(ass.freq == 8, 13, 26)
ass.plot.yrs = ifelse(ass.freq == 6, 18, ass.plot.yrs)
min = -1.5; max = 1.25
text.yr = c(2, 7, 11,15)
 
for (a in 1:ds){
  to.plot = ass.yrs[1:ass.plot.yrs]
  re.ssb = matrix(0, length(to.plot), N)
  for (b in 1:length(to.plot)){
    re.ssb[b,] = med.out[[1]]$re.ssb[a,b,to.plot[b],]
  }
  per.of = med.out[[1]]$n.overfished[a,text.yr]
  scale.per.of = (1-med.out[[1]]$n.overfished[a,] / 100)*0.5 + min 
  plot(1:length(to.plot), scale.per.of, type = 'l', lwd = 1.5, ylim = c(min, max), axes= F, yaxs = "i")
  points(1:length(to.plot), scale.per.of, pch = 21, cex = 0.85)
  boxplot(t(re.ssb),col = rep('grey',length(to.plot)), axes = F, add = TRUE, boxwex = rep(0.75, length(to.plot)))
  box(); abline(h = 0) ; abline(h = -0.98)
  if (a == 2 || a==5) { print.letter("% Rebuilt", xy = c(0.50, 0.05), cex = 1.1) }
  #text( labels = paste0(per.of, "%"), x = text.yr, y = rep(-1, length(text.yr)), cex = 1 )
  if( a ==1 || a==4) { axis(side = 2, at = seq(-0.5, max, 0.5), las = 1)
                      mtext(side = 2, outer = T, "RE spawning biomass", line = 3)} 
  if( a ==3 || a==6) { axis(side = 4, at = c(min, min + 0.25, -1), label = c("0%", "50%", "100%"), las = 1) } 
  if( a > 3){
    axis(side = 1, at = seq(1,length(to.plot),4), 
        labels = seq(to.plot[1]- pre.yrs + 1, to.plot[length(to.plot)] - pre.yrs + 1, 4*ass.freq) )
    mtext(side = 1, outer = T, "Assessment year", line = 3)
  }
  if (a < 4) { mtext(side = 3, outer = F, name.label[a]) }
  if (a == 3 ){ mtext(side = 4, outer = F, "time-invariant", line = 1) }
  #text(par("usr")[2]*1.03, mean(par("usr")[3:4])+0.1, "time-invarient", srt = -90, xpd = TRUE, pos = 4, cex = 1.5)}
  if (a == 6 ){ mtext(side = 4, outer = F, "time-varying", line = 1) }
  #text(par("usr")[2]*1.03, mean(par("usr")[3:4])+0.1, "time-varying", srt = -90, xpd = TRUE, pos = 4, cex = 1.5)}
  print.letter(alpha.label[a], xy = c(0.95, 0.95), cex = 1.1)  
}

dev.off()

png(filename = "RE_SSB_cumm_v2.png", width = 6.7, height = 6, units = 'in', res = 256)

par(mfrow= c(2,3), mar = c(0.2,0.2,0.2,0.2), oma = c(5,5,2,5), cex.axis = 1.1, cex.lab = 1.1)
letter.cex = 1; axis.cex = 1.2; label.cex = 0.8
ass.plot.yrs = ifelse(ass.freq == 8, 13, 26)
ass.plot.yrs = ifelse(ass.freq == 6, 18, ass.plot.yrs)
min = -1.5; max = 1.25
text.yr = c(2, 7, 11,15)
 
for (a in 1:ds){
  if (a == 1){ not = which(est.out[[1]]$time.over[3,] == 101) }
  if (a == 4){ not = which(est.out[[1]]$time.over[6,] == 101) }
  to.plot = ass.yrs[1:ass.plot.yrs]
  re.ssb = matrix(0, length(to.plot), N)
  for (b in 1:length(to.plot)){
    re.ssb[b,] = med.out[[1]]$re.ssb[a,b,to.plot[b],]
  }

  plot.list = list()
  plot.list[[3]] = re.ssb[1,]
  plot.list[[2]] = re.ssb[1,-not]
  plot.list[[1]] = re.ssb[1, not]
  for (b in 4:19){ plot.list[[b]] = re.ssb[b-2,]}
  plot.list[[20]] = re.ssb[length(to.plot),]
  plot.list[[21]] = re.ssb[length(to.plot),-not]
  plot.list[[22]] = re.ssb[length(to.plot), not]

  per.of = med.out[[1]]$n.overfished[a,text.yr]
  scale.per.of = (1-med.out[[1]]$n.overfished[a,] / 100)*0.5 + min 
  scale.per.of = c(rep(scale.per.of[1],3), scale.per.of[2:(length(scale.per.of) -1)], rep(scale.per.of[length(scale.per.of)],3))
  plot(1:(length(to.plot) + 4), scale.per.of, type = 'l', lwd = 1.5, ylim = c(min, max), axes= F, yaxs = "i")
  points(1:(length(to.plot) + 4), scale.per.of, pch = 21, cex = 0.85)
  xx = c(seq(-1,3.5,0.25), rev(seq(-1,3.5,0.25))); yy = c(rep(-0.98,0.5*length(xx)), rep(max, 0.5*length(xx)))
  polygon(xx, yy, col = 'lightgrey', border = F)
  xx = c(19.5:23.5, rev(19.5:23.5)); yy = c(rep(-0.98,5), rep(max, 5))
  polygon(xx, yy, col = 'lightgrey', border = F)
  box95(x = plot.list, list = TRUE, col = c('red','forestgreen',rep('steelblue3',length(to.plot)), 'forestgreen', 'red'), 
          axes = F, add = TRUE, boxwex = rep(0.6, length(to.plot) + 4))

  #box95(x = t(re.ssb), col = rep('grey',length(to.plot)), axes = F, add = TRUE, boxwex = rep(0.75, length(to.plot)))
  box(); abline(h = 0) ; abline(h = -0.98)
  if (a == 2 || a==5) { print.letter("% Rebuilt", xy = c(0.50, 0.05), cex = 1.1) }
  #text( labels = paste0(per.of, "%"), x = text.yr, y = rep(-1, length(text.yr)), cex = 1 )
  if( a ==1 || a==4) { axis(side = 2, at = seq(-0.5, max, 0.5), las = 1)
                      mtext(side = 2, outer = T, "RE spawning biomass", line = 3)} 
  if( a ==3 || a==6) { axis(side = 4, at = c(min, min + 0.25, -1), label = c("0%", "50%", "100%"), las = 1) } 
  if( a > 3){
    axis(side = 1, at = seq(2,length(to.plot),4), 
        labels = seq(to.plot[1]- pre.yrs + 1, to.plot[length(to.plot)] - pre.yrs + 1, 4*ass.freq) )
    mtext(side = 1, outer = T, "Assessment year", line = 3)
  }
  if (a < 4) { mtext(side = 3, outer = F, name.label[a]) }
  if (a == 3 ){ mtext(side = 4, outer = F, "time-invariant", line = 1) }
  #text(par("usr")[2]*1.03, mean(par("usr")[3:4])+0.1, "time-invarient", srt = -90, xpd = TRUE, pos = 4, cex = 1.5)}
  if (a == 6 ){ mtext(side = 4, outer = F, "time-varying", line = 1) }
  #text(par("usr")[2]*1.03, mean(par("usr")[3:4])+0.1, "time-varying", srt = -90, xpd = TRUE, pos = 4, cex = 1.5)}
  print.letter(alpha.label[a], xy = c(0.95, 0.95), cex = 1.1)  
}

dev.off()

# Partition the results to only stocks that rebuild ================================================
png(filename = "RE_SSB_Rebuilt.png", width = 6.7, height = 6, units = 'in', res = 256)

par(mfrow= c(2,3), mar = c(0.2,0.2,0.2,0.2), oma = c(5,5,2,5), cex.axis = 1.1, cex.lab = 1.1)
letter.cex = 1; axis.cex = 1.2; label.cex = 0.8
ass.plot.yrs = ifelse(ass.freq == 8, 13, 26)
ass.plot.yrs = ifelse(ass.freq == 6, 18, ass.plot.yrs)
min = -1.5; max = 1.25
text.yr = c(2, 7, 11,15)
 
for (a in 1:ds){
  to.plot = ass.yrs[1:ass.plot.yrs]
  ind = est.out[[1]]$time.over[a,] != 101
  re.ssb = list()
  for (b in 1:length(to.plot)){
    re.ssb[[b]] = med.out[[1]]$re.ssb[a,b,to.plot[b],ind]
  }
  per.of = med.out[[1]]$n.overfished[a,text.yr]
  scale.per.of = (1-med.out[[1]]$n.overfished[a,] / 100)*0.5 + min 
  plot(1:length(to.plot), scale.per.of, type = 'l', lwd = 1.5, ylim = c(min, max), axes= F, yaxs = "i")
  points(1:length(to.plot), scale.per.of, pch = 21, cex = 0.85)
  boxplot(re.ssb,col = rep('grey',length(to.plot)), axes = F, add = TRUE, boxwex = rep(0.75, length(to.plot)))
  box(); abline(h = 0) ; abline(h = -0.98)
  if (a == 2 || a==5) { print.letter("% Rebuilt", xy = c(0.50, 0.05), cex = 1.1) }
  #text( labels = paste0(per.of, "%"), x = text.yr, y = rep(-1, length(text.yr)), cex = 1 )
  if( a ==1 || a==4) { axis(side = 2, at = seq(-0.5, max, 0.5), las = 1)
                      mtext(side = 2, outer = T, "RE spawning biomass", line = 3)} 
  if( a ==3 || a==6) { axis(side = 4, at = c(min, min + 0.25, -1), label = c("0%", "50%", "100%"), las = 1) } 
  if( a > 3){
    axis(side = 1, at = seq(1,length(to.plot),4), 
        labels = seq(to.plot[1]- pre.yrs + 1, to.plot[length(to.plot)] - pre.yrs + 1, 4*ass.freq) )
    mtext(side = 1, outer = T, "Assessment year", line = 3)
  }
  if (a < 4) { mtext(side = 3, outer = F, name.label[a]) }
  if (a == 3 ){ mtext(side = 4, outer = F, "time-invariant", line = 1) }
  #text(par("usr")[2]*1.03, mean(par("usr")[3:4])+0.1, "time-invarient", srt = -90, xpd = TRUE, pos = 4, cex = 1.5)}
  if (a == 6 ){ mtext(side = 4, outer = F, "time-varying", line = 1) }
  #text(par("usr")[2]*1.03, mean(par("usr")[3:4])+0.1, "time-varying", srt = -90, xpd = TRUE, pos = 4, cex = 1.5)}
  print.letter(alpha.label[a], xy = c(0.95, 0.95), cex = 1.1)  
}

dev.off()

png(filename = "RE_SSB_not_rebuilt_elim_fixed.png", width = 6.7, height = 6, units = 'in', res = 256)

par(mfrow= c(1,3), mar = c(0.2,0.2,0.2,0.2), oma = c(5,5,2,5), cex.axis = 1.1, cex.lab = 1.1)
letter.cex = 1; axis.cex = 1.2; label.cex = 0.8
ass.plot.yrs = ifelse(ass.freq == 8, 13, 26)
ass.plot.yrs = ifelse(ass.freq == 6, 18, ass.plot.yrs)
min = -1.5; max = 1.25
text.yr = c(2, 7, 11,15)
 
for (a in 1:3){
  to.plot = ass.yrs[1:ass.plot.yrs]
  ind = est.out[[1]]$time.over[3,] == 101
  re.ssb = list()
  for (b in 1:length(to.plot)){
    re.ssb[[b]] = med.out[[1]]$re.ssb[a,b,to.plot[b],ind]
  }
  per.of = med.out[[1]]$n.overfished[a,text.yr]
  scale.per.of = (1-med.out[[1]]$n.overfished[a,] / 100)*0.5 + min 
  plot(1:length(to.plot), scale.per.of, type = 'l', lwd = 1.5, ylim = c(min, max), axes= F, yaxs = "i")
  points(1:length(to.plot), scale.per.of, pch = 21, cex = 0.85)
  boxplot(re.ssb,col = rep('grey',length(to.plot)), axes = F, add = TRUE, boxwex = rep(0.75, length(to.plot)))
  box(); abline(h = 0) ; abline(h = -0.98)
  if (a == 2 || a==5) { print.letter("% Rebuilt", xy = c(0.50, 0.05), cex = 1.1) }
  #text( labels = paste0(per.of, "%"), x = text.yr, y = rep(-1, length(text.yr)), cex = 1 )
  if( a ==1 || a==4) { axis(side = 2, at = seq(-0.5, max, 0.5), las = 1)
                      mtext(side = 2, outer = T, "RE spawning biomass", line = 3)} 
  if( a ==3 || a==6) { axis(side = 4, at = c(min, min + 0.25, -1), label = c("0%", "50%", "100%"), las = 1) } 
  if( a > 3){
    axis(side = 1, at = seq(1,length(to.plot),4), 
        labels = seq(to.plot[1]- pre.yrs + 1, to.plot[length(to.plot)] - pre.yrs + 1, 4*ass.freq) )
    mtext(side = 1, outer = T, "Assessment year", line = 3)
  }
  if (a < 4) { mtext(side = 3, outer = F, name.label[a]) }
  if (a == 3 ){ mtext(side = 4, outer = F, "time-invariant", line = 1) }
  #text(par("usr")[2]*1.03, mean(par("usr")[3:4])+0.1, "time-invarient", srt = -90, xpd = TRUE, pos = 4, cex = 1.5)}
  if (a == 6 ){ mtext(side = 4, outer = F, "time-varying", line = 1) }
  #text(par("usr")[2]*1.03, mean(par("usr")[3:4])+0.1, "time-varying", srt = -90, xpd = TRUE, pos = 4, cex = 1.5)}
  print.letter(alpha.label[a], xy = c(0.95, 0.95), cex = 1.1)  
}

dev.off()

#===================================================================================================
png(filename = "RE_SSB_short.png", width = 6.7, height = 5, units = 'in', res = 256)

par(mfrow= c(2,3), mar = c(0.2,0.2,0.2,0.2), oma = c(5,5,2,5), cex.axis = 1.1, cex.lab = 1.1)
letter.cex = 1; axis.cex = 1.2; label.cex = 0.8
ass.plot.yrs = ifelse(ass.freq == 8, 5, 10)
ass.plot.yrs = ifelse(ass.freq == 6, 7, ass.plot.yrs)
text.yr = c(2, 4, 5, 7)
min = -0.85; max = 1.25

for (a in 1:ds){
  to.plot = ass.yrs[1:ass.plot.yrs]
  re.ssb = matrix(0, length(to.plot), N)
  for (b in 1:length(to.plot)){
    re.ssb[b,] = med.out[[1]]$re.ssb[a,b,to.plot[b],]
  }

  boxplot(t(re.ssb), ylim = c(min, max), col = rep('grey',length(to.plot)), axes= F)
  box(); abline(h = 0)
  per.of = med.out[[1]]$n.overfished[a,text.yr] 
  text( labels = paste0(per.of, "%"), x = text.yr, y = rep(-1, length(text.yr)), cex = 1 )
  if( a ==1 || a==4) { axis(side = 2)
      mtext(side = 2, outer = T, "RE spawning biomass", line = 3)} 
  if( a > 3){
    axis(side = 1, at = seq(1,length(to.plot),4), 
        labels = seq(to.plot[1]- pre.yrs + 1, to.plot[length(to.plot)] - pre.yrs + 1, 4*ass.freq) )
    mtext(side = 1, outer = T, "Assessment year", line = 3)
  }
  if (a < 4) { mtext(side = 3, outer = F, name.label[a]) }
  if (a == 3 ){ mtext(side = 4, outer = F, "time-invariant", line = 1) }
  #text(par("usr")[2]*1.03, mean(par("usr")[3:4])+0.1, "time-invarient", srt = -90, xpd = TRUE, pos = 4, cex = 1.5)}
  if (a == 6 ){ mtext(side = 4, outer = F, "time-varying", line = 1) }
  #text(par("usr")[2]*1.03, mean(par("usr")[3:4])+0.1, "time-varying", srt = -90, xpd = TRUE, pos = 4, cex = 1.5)}
  print.letter(alpha.label[a], xy = c(0.95, 0.95), cex = 1.1)  
}

dev.off()

#=======================================================================================================
# Relative error about relative biomass ================================================================
#=======================================================================================================

png(filename = "RE_depl_full.png", width = 6.7, height = 5, units = 'in', res = 256)

par(mfrow= c(2,3), mar = c(0.2,0.2,0.2,0.2), oma = c(5,5,2,5), cex.axis = 1.1, cex.lab = 1.1)
letter.cex = 1; axis.cex = 1.2; label.cex = 0.8
ass.plot.yrs = ifelse(ass.freq == 8, 13, 26)
ass.plot.yrs = ifelse(ass.freq == 6, 18, ass.plot.yrs)
text.yr = c(2, 7, 11,15)
min = -1; max = 1.25

spot = c(0.40, 0.40, 0.40, 0.50, 0.50, 0.50)
for (a in 1:ds){
  to.plot = ass.yrs[1:ass.plot.yrs]
  re.depl = matrix(0, length(to.plot), N)
  for (b in 1:length(to.plot)){
    re.depl[b,] = med.out[[1]]$re.depl[a,b,to.plot[b],]
  }
  if (a == 1){ re.depl[1,40] = 0.961795}
 

  boxplot(t(re.depl), ylim = c(min, max), col = rep('grey',length(to.plot)), axes= F, las = 1)
  box(); abline(h = 0) 

  per.of = med.out[[1]]$n.overfished[a,text.yr] 
  text( labels = paste0(per.of, "%"), x = text.yr, y = rep(-1, length(text.yr)), cex = 1 ) 
  if( a ==1 || a==4) { axis(side = 2, las = 2)
      mtext(side = 2, outer = T, "RE relative biomass", line = 3)} 
  if( a > 3){
    axis(side = 1, at = seq(1,length(to.plot),4), 
        labels = seq(to.plot[1]- pre.yrs + 1, to.plot[length(to.plot)] - pre.yrs + 1, 4*ass.freq) )
    mtext(side = 1, outer = T, "Assessment year", line = 3)
  }
  if (a < 4) { mtext(side = 3, outer = F, name.label[a]) }
  if (a == 3 ){ mtext(side = 4, outer = F, "time-invariant", line = 1) }
  #text(par("usr")[2]*1.03, mean(par("usr")[3:4])+0.1, "time-invarient", srt = -90, xpd = TRUE, pos = 4, cex = 1.5)}
  if (a == 6 ){ mtext(side = 4, outer = F, "time-varying", line = 1) }
  #text(par("usr")[2]*1.03, mean(par("usr")[3:4])+0.1, "time-varying", srt = -90, xpd = TRUE, pos = 4, cex = 1.5)}
  print.letter(alpha.label[a], xy = c(0.95, 0.95), cex = 1.1)  
}

dev.off()

png(filename = "RE_depl_full_cumm.png", width = 6.7, height = 6, units = 'in', res = 256)

par(mfrow= c(2,3), mar = c(0.2,0.2,0.2,0.2), oma = c(5,5,2,5), cex.axis = 1.1, cex.lab = 1.1)
letter.cex = 1; axis.cex = 1.2; label.cex = 0.8
ass.plot.yrs = ifelse(ass.freq == 8, 13, 26)
ass.plot.yrs = ifelse(ass.freq == 6, 18, ass.plot.yrs)
text.yr = c(2, 7, 11,15)
min = -1.5; max = 1.25

spot = c(0.40, 0.40, 0.40, 0.50, 0.50, 0.50)
for (a in 1:ds){
  to.plot = ass.yrs[1:ass.plot.yrs]
  re.depl = matrix(0, length(to.plot), N)
  for (b in 1:length(to.plot)){
    re.depl[b,] = med.out[[1]]$re.depl[a,b,to.plot[b],]
  }
  if (a == 1){ re.depl[1,40] = 0.961795}
 
  scale.per.of = (1- med.out[[1]]$n.overfished[a,] / 100)*0.5 + min 
  plot(1:length(to.plot), scale.per.of, type = 'l', lwd = 1.5, ylim = c(min, max), axes= F, yaxs = 'i')
  points(1:length(to.plot), scale.per.of, pch = 21, cex = 0.85)
  boxplot(t(re.depl),col = rep('grey',length(to.plot)), axes = F, add = TRUE, boxwex = rep(0.75, length(to.plot)))
  box(); abline(h = 0) ; abline(h= -0.98)
  if (a == 2 || a==5) { print.letter("% Rebuilt", xy = c(0.50, 0.05), cex = 1.1) }
  if( a ==1 || a==4) { axis(side = 2, at = seq(-0.5, max, 0.5), las = 1) 
      mtext(side = 2, outer = T, "RE relative spawning biomass", line = 3)} 
  if( a ==3 || a==6) { axis(side = 4, at = c(min, min + 0.25, -1), label = c("0%", "50%", "100%"), las = 1) } 
  if( a > 3){
    axis(side = 1, at = seq(1,length(to.plot),4), 
        labels = seq(to.plot[1]- pre.yrs + 1, to.plot[length(to.plot)] - pre.yrs + 1, 4*ass.freq) )
    mtext(side = 1, outer = T, "Assessment year", line = 3)
  }
  if (a < 4) { mtext(side = 3, outer = F, name.label[a]) }
  if (a == 3 ){ mtext(side = 4, outer = F, "time-invariant", line = 1) }
  if (a == 6 ){ mtext(side = 4, outer = F, "time-varying", line = 1) }
  print.letter(alpha.label[a], xy = c(0.95, 0.95), cex = 1.1)  
}

dev.off()

#=====================================================================================================

png(filename = "RE_depl_cumm_v2.png", width = 6.7, height = 6, units = 'in', res = 256)

par(mfrow= c(2,3), mar = c(0.2,0.2,0.2,0.2), oma = c(5,5,2,5), cex.axis = 1.1, cex.lab = 1.1)
letter.cex = 1; axis.cex = 1.2; label.cex = 0.8
ass.plot.yrs = ifelse(ass.freq == 8, 13, 26)
ass.plot.yrs = ifelse(ass.freq == 6, 18, ass.plot.yrs)
min = -1.5; max = 1.25
text.yr = c(2, 7, 11,15)
 
for (a in 1:ds){
  if (a == 1){ not = which(est.out[[1]]$time.over[3,] == 101) }
  if (a == 4){ not = which(est.out[[1]]$time.over[6,] == 101) }
  to.plot = ass.yrs[1:ass.plot.yrs]
  re.ssb = matrix(0, length(to.plot), N)
  for (b in 1:length(to.plot)){
    re.ssb[b,] = med.out[[1]]$re.depl[a,b,to.plot[b],]
  }

  plot.list = list()
  plot.list[[3]] = re.ssb[1,]
  plot.list[[2]] = re.ssb[1,-not]
  plot.list[[1]] = re.ssb[1, not]
  for (b in 4:19){ plot.list[[b]] = re.ssb[b-2,]}
  plot.list[[20]] = re.ssb[length(to.plot),]
  plot.list[[21]] = re.ssb[length(to.plot),-not]
  plot.list[[22]] = re.ssb[length(to.plot), not]

  per.of = med.out[[1]]$n.overfished[a,text.yr]
  scale.per.of = (1-med.out[[1]]$n.overfished[a,] / 100)*0.5 + min 
  scale.per.of = c(rep(scale.per.of[1],3), scale.per.of[2:(length(scale.per.of) -1)], rep(scale.per.of[length(scale.per.of)],3))
  plot(1:(length(to.plot) + 4), scale.per.of, type = 'l', lwd = 1.5, ylim = c(min, max), axes= F, yaxs = "i")
  points(1:(length(to.plot) + 4), scale.per.of, pch = 21, cex = 0.85)
  xx = c(seq(-1,3.5,0.25), rev(seq(-1,3.5,0.25))); yy = c(rep(-0.98,0.5*length(xx)), rep(max, 0.5*length(xx)))
  polygon(xx, yy, col = 'lightgrey', border = F)
  xx = c(19.5:23.5, rev(19.5:23.5)); yy = c(rep(-0.98,5), rep(max, 5))
  polygon(xx, yy, col = 'lightgrey', border = F)
  box95(x = plot.list, list = TRUE,col = c('red','forestgreen',rep('steelblue3',length(to.plot)), 'forestgreen', 'red'), 
          axes = F, add = TRUE, boxwex = rep(0.60, length(to.plot) + 4))

  #box95(x = t(re.ssb), col = rep('grey',length(to.plot)), axes = F, add = TRUE, boxwex = rep(0.75, length(to.plot)))
  box(); abline(h = 0) ; abline(h = -0.98)
  if (a == 2 || a==5) { print.letter("% Rebuilt", xy = c(0.50, 0.05), cex = 1.1) }
  #text( labels = paste0(per.of, "%"), x = text.yr, y = rep(-1, length(text.yr)), cex = 1 )
  if( a ==1 || a==4) { axis(side = 2, at = seq(-0.5, max, 0.5), las = 1)
                      mtext(side = 2, outer = T, "RE relative spawning biomass", line = 3)} 
  if( a ==3 || a==6) { axis(side = 4, at = c(min, min + 0.25, -1), label = c("0%", "50%", "100%"), las = 1) } 
  if( a > 3){
    axis(side = 1, at = seq(2,length(to.plot),4), 
        labels = seq(to.plot[1]- pre.yrs + 1, to.plot[length(to.plot)] - pre.yrs + 1, 4*ass.freq) )
    mtext(side = 1, outer = T, "Assessment year", line = 3)
  }
  if (a < 4) { mtext(side = 3, outer = F, name.label[a]) }
  if (a == 3 ){ mtext(side = 4, outer = F, "time-invariant", line = 1) }
  #text(par("usr")[2]*1.03, mean(par("usr")[3:4])+0.1, "time-invarient", srt = -90, xpd = TRUE, pos = 4, cex = 1.5)}
  if (a == 6 ){ mtext(side = 4, outer = F, "time-varying", line = 1) }
  #text(par("usr")[2]*1.03, mean(par("usr")[3:4])+0.1, "time-varying", srt = -90, xpd = TRUE, pos = 4, cex = 1.5)}
  print.letter(alpha.label[a], xy = c(0.95, 0.95), cex = 1.1)  
}

dev.off()
#==================================================================================================================

png(filename = "RE_depl_rebuilt.png", width = 6.7, height = 6, units = 'in', res = 256)

par(mfrow= c(2,3), mar = c(0.2,0.2,0.2,0.2), oma = c(5,5,2,5), cex.axis = 1.1, cex.lab = 1.1)
letter.cex = 1; axis.cex = 1.2; label.cex = 0.8
ass.plot.yrs = ifelse(ass.freq == 8, 13, 26)
ass.plot.yrs = ifelse(ass.freq == 6, 18, ass.plot.yrs)
text.yr = c(2, 7, 11,15)
min = -1.5; max = 1.25

spot = c(0.40, 0.40, 0.40, 0.50, 0.50, 0.50)
for (a in 1:ds){
  to.plot = ass.yrs[1:ass.plot.yrs]
  ind = est.out[[1]]$time.over[a,] != 101  
  re.depl = list()
  for (b in 1:length(to.plot)){
    re.depl[[b]] = med.out[[1]]$re.depl[a,b,to.plot[b],ind]
  }
 
  scale.per.of = (1- med.out[[1]]$n.overfished[a,] / 100)*0.5 + min 
  plot(1:length(to.plot), scale.per.of, type = 'l', lwd = 1.5, ylim = c(min, max), axes= F, yaxs = 'i')
  points(1:length(to.plot), scale.per.of, pch = 21, cex = 0.85)
  boxplot(re.depl,col = rep('grey',length(to.plot)), axes = F, add = TRUE, boxwex = rep(0.75, length(to.plot)))
  box(); abline(h = 0) ; abline(h= -0.98)
  if (a == 2 || a==5) { print.letter("% Rebuilt", xy = c(0.50, 0.05), cex = 1.1) }
  if( a ==1 || a==4) { axis(side = 2, at = seq(-0.5, max, 0.5), las = 1) 
      mtext(side = 2, outer = T, "RE relative spawning biomass", line = 3)} 
  if( a ==3 || a==6) { axis(side = 4, at = c(min, min + 0.25, -1), label = c("0%", "50%", "100%"), las = 1) } 
  if( a > 3){
    axis(side = 1, at = seq(1,length(to.plot),4), 
        labels = seq(to.plot[1]- pre.yrs + 1, to.plot[length(to.plot)] - pre.yrs + 1, 4*ass.freq) )
    mtext(side = 1, outer = T, "Assessment year", line = 3)
  }
  if (a < 4) { mtext(side = 3, outer = F, name.label[a]) }
  if (a == 3 ){ mtext(side = 4, outer = F, "time-invariant", line = 1) }
  if (a == 6 ){ mtext(side = 4, outer = F, "time-varying", line = 1) }
  print.letter(alpha.label[a], xy = c(0.95, 0.95), cex = 1.1)  
}

dev.off()

png(filename = "RE_depl_not_rebuilt.png", width = 6.7, height = 6, units = 'in', res = 256)

par(mfrow= c(2,3), mar = c(0.2,0.2,0.2,0.2), oma = c(5,5,2,5), cex.axis = 1.1, cex.lab = 1.1)
letter.cex = 1; axis.cex = 1.2; label.cex = 0.8
ass.plot.yrs = ifelse(ass.freq == 8, 13, 26)
ass.plot.yrs = ifelse(ass.freq == 6, 18, ass.plot.yrs)
text.yr = c(2, 7, 11,15)
min = -1.5; max = 1.25

spot = c(0.40, 0.40, 0.40, 0.50, 0.50, 0.50)
for (a in 1:ds){
  to.plot = ass.yrs[1:ass.plot.yrs]
  ind = est.out[[1]]$time.over[a,] == 101  
  re.depl = list()
  for (b in 1:length(to.plot)){
    re.depl[[b]] = med.out[[1]]$re.depl[a,b,to.plot[b],ind]
  }
 
  scale.per.of = (1- med.out[[1]]$n.overfished[a,] / 100)*0.5 + min 
  plot(1:length(to.plot), scale.per.of, type = 'l', lwd = 1.5, ylim = c(min, max), axes= F, yaxs = 'i')
  points(1:length(to.plot), scale.per.of, pch = 21, cex = 0.85)
  boxplot(re.depl,col = rep('grey',length(to.plot)), axes = F, add = TRUE, boxwex = rep(0.75, length(to.plot)))
  box(); abline(h = 0) ; abline(h= -0.98)
  if (a == 2 || a==5) { print.letter("% Rebuilt", xy = c(0.50, 0.05), cex = 1.1) }
  if( a ==1 || a==4) { axis(side = 2, at = seq(-0.5, max, 0.5), las = 1) 
      mtext(side = 2, outer = T, "RE relative spawning biomass", line = 3)} 
  if( a ==3 || a==6) { axis(side = 4, at = c(min, min + 0.25, -1), label = c("0%", "50%", "100%"), las = 1) } 
  if( a > 3){
    axis(side = 1, at = seq(1,length(to.plot),4), 
        labels = seq(to.plot[1]- pre.yrs + 1, to.plot[length(to.plot)] - pre.yrs + 1, 4*ass.freq) )
    mtext(side = 1, outer = T, "Assessment year", line = 3)
  }
  if (a < 4) { mtext(side = 3, outer = F, name.label[a]) }
  if (a == 3 ){ mtext(side = 4, outer = F, "time-invariant", line = 1) }
  if (a == 6 ){ mtext(side = 4, outer = F, "time-varying", line = 1) }
  print.letter(alpha.label[a], xy = c(0.95, 0.95), cex = 1.1)  
}

dev.off()

png(filename = "RE_depl_short.png", width = 6.7, height = 5, units = 'in', res = 256)

par(mfrow= c(2,3), mar = c(0.2,0.2,0.2,0.2), oma = c(5,5,2,5), cex.axis = 1.1, cex.lab = 1.1)
letter.cex = 1; axis.cex = 1.2; label.cex = 0.8
ass.plot.yrs = ifelse(ass.freq == 8, 5, 10)
ass.plot.yrs = ifelse(ass.freq == 6, 7, ass.plot.yrs)
text.yr = c(2, 4, 5, 7)
min = -0.75; max = 1.25

for (a in 1:ds){
  to.plot = ass.yrs[1:ass.plot.yrs]
  re.depl = matrix(0, length(to.plot), N)
  for (b in 1:length(to.plot)){
    re.depl[b,] = med.out[[1]]$re.depl[a,b,to.plot[b],]
  }

  boxplot(t(re.depl), ylim = c(min, max), col = rep('grey',length(to.plot)), axes= F)
  box(); abline(h = 0)
  per.of = med.out[[1]]$n.overfished[a,text.yr] 
  text( labels = paste0(per.of, "%"), x = text.yr, y = rep(-1, length(text.yr)), cex = 1 )
  if( a ==1 || a==4) { axis(side = 2, las = 1)
      mtext(side = 2, outer = T, "RE relative biomass", line = 3)} 
  if( a > 3){
    axis(side = 1, at = seq(1,length(to.plot),4), 
        labels = seq(to.plot[1]- pre.yrs + 1, to.plot[length(to.plot)] - pre.yrs + 1, 4*ass.freq) )
    mtext(side = 1, outer = T, "Assessment year", line = 3)
  }
  if (a < 4) { mtext(side = 3, outer = F, name.label[a]) }
  if (a == 3 ){ mtext(side = 4, outer = F, "time-invariant", line = 1) }
  #text(par("usr")[2]*1.03, mean(par("usr")[3:4])+0.1, "time-invarient", srt = -90, xpd = TRUE, pos = 4, cex = 1.5)}
  if (a == 6 ){ mtext(side = 4, outer = F, "time-varying", line = 1) }
  #text(par("usr")[2]*1.03, mean(par("usr")[3:4])+0.1, "time-varying", srt = -90, xpd = TRUE, pos = 4, cex = 1.5)}
  print.letter(alpha.label[a], xy = c(0.95, 0.95), cex = 1.1)  
}

dev.off()

#=======================================================================================================
# Relative error about SB0              ================================================================
#=======================================================================================================

png(filename = "RE_SB0.png", width = 6.7, height = 5, units = 'in', res = 256)

par(mfrow= c(2,3), mar = c(0.2,0.2,0.2,0.2), oma = c(4,4,2,4), cex.axis = 1.1, cex.lab = 1.1)
letter.cex = 1; axis.cex = 1.2; label.cex = 0.8
ass.plot.yrs = ifelse(ass.freq == 8, 13, 26)
ass.plot.yrs = ifelse(ass.freq == 6, 18, ass.plot.yrs)
min = -0.25; max = 0.25

for (a in 1:ds){
  to.plot = ass.yrs[1:ass.plot.yrs]
  #re.depl = matrix(0, length(to.plot), 100)
  #for (b in 1:length(to.plot)){
  #  re.depl[b,] = med.out[[1]]$re.depl[a,b,to.plot[b],]
  #}

  boxplot(t(med.out[[1]]$re.ssb0[a,,]), ylim = c(min, max), col = rep('grey',length(to.plot)), axes= F)
  box(); abline(h = 0)
  if( a ==1 || a==4) { axis(side = 2)
      mtext(side = 2, outer = T, "RE SB0", line = 2.5)} 
  if( a > 3){
    axis(side = 1, at = 1:length(to.plot), labels = to.plot - pre.yrs + 1)
    mtext(side = 1, outer = T, "Assessment year", line = 2.5)
  }
  if (a < 4) { mtext(side = 3, outer = F, name.label[a]) }
  if (a == 3 ){ mtext(side = 4, outer = F, "Time-invariant", line = 1) }
  if (a == 6 ){ mtext(side = 4, outer = F, "Time-varying", line = 1) }
  print.letter(alpha.label[a], xy = c(0.07, 0.05), cex = 1.1)  
}

dev.off()

#=======================================================================================================
# Relative error recovery time ================================================================
#=======================================================================================================

png(filename = "RE_timeover.png", width = 6.7, height = 5, units = 'in', res = 256)

par(mfrow= c(1,2), mar = c(0.2,0.2,0.2,0.2), oma = c(4,4,2,4), cex.axis = 1.1, cex.lab = 1.1)
letter.cex = 1; axis.cex = 1.2; label.cex = 0.8
ass.plot.yrs = ifelse(ass.freq == 8, 13, 26)
ass.plot.yrs = ifelse(ass.freq == 6, 18, ass.plot.yrs)
min = -1.5; max = 2
to.plot = ass.yrs[1:ass.plot.yrs]

boxplot(c(med.out[[1]]$filt.re.time.over[1], med.out[[1]]$filt.re.time.over[2], med.out[[1]]$filt.re.time.over[3]),
       ylim = c(min, max), col = rep('grey',length(to.plot)), axes= F)
print.letter(length(med.out[[1]]$filt.re.time.over[[1]]) , xy = c(0.20, 0.10))
print.letter(length(med.out[[1]]$filt.re.time.over[[2]]) , xy = c(0.50, 0.10))
print.letter(length(med.out[[1]]$filt.re.time.over[[3]]) , xy = c(0.80, 0.10))
box(); abline(h = 0)
axis(side = 1, at = 1:3, labels = c('full data', "reduced data", "historical data"))
axis(side = 2, las = 1)
mtext(side = 2, outer = T, "RE time over", line = 2.5)
mtext(side = 3, outer = F, "time-invariant")
#axis(side = 1, at = 1:length(to.plot), labels = to.plot - pre.yrs + 1)
mtext(side = 1, outer = T, "Data scenario", line = 2.5)

boxplot(c(med.out[[1]]$filt.re.time.over[4], med.out[[1]]$filt.re.time.over[5], med.out[[1]]$filt.re.time.over[6]),
       ylim = c(min, max), col = rep('grey',length(to.plot)), axes= F)
box(); abline(h = 0)
mtext(side = 3, outer = F, "time-varying")
axis(side = 1, at = 1:3, labels = c('full data', "reduced data", "historical data"))
print.letter(length(med.out[[1]]$filt.re.time.over[[4]]) , xy = c(0.20, 0.10))
print.letter(length(med.out[[1]]$filt.re.time.over[[5]]) , xy = c(0.50, 0.10))
print.letter(length(med.out[[1]]$filt.re.time.over[[6]]) , xy = c(0.80, 0.10))

dev.off()

#=======================================================================================================
# Estimated Recovery Distributions ================================================================
#=======================================================================================================

png(filename = "RecoveryDist.png", width = 6.7, height = 5, units = 'in', res = 256)

par(mfrow= c(2,3), mar = c(0.2,0.2,0.2,0.2), oma = c(4,4,2,4), cex.axis = 1.1, cex.lab = 1.1)
letter.cex = 1; axis.cex = 1.2; label.cex = 0.8
ass.plot.yrs = ifelse(ass.freq == 8, 13, 26)
ass.plot.yrs = ifelse(ass.freq == 6, 18, ass.plot.yrs)
min = -1.5; max = 2
to.plot = ass.yrs[1:ass.plot.yrs]

plot.matrix = matrix(0,ds,N)
for(a in 1:ds){
  for (b in 1:N){
    plot.matrix[a,b] = est.out[[1]]$time.over[a,b] 
  }
}

for (a in 1:ds){
  hist(plot.matrix[a,], axes = F, main = "", xlim = c(0, 110), ylim = c(0, 30))
  #abline(v = median(plot.matrix[a,]), lty = 2)
  box()
  print.letter(sum(plot.matrix[a,] == 101), xy = c(0.92,0.90), cex = 1.1)
  if (a == 1) {mtext(side = 2, "Frequeny", outer = T, line = 1)}
  if (a > 3) axis(side = 1)
  print.letter(alpha.label[a], xy = c(0.07, 0.95), cex = 1.1) 
  if (a < 4 )  { mtext(side = 3, outer = F, name.label[a]) }
  if (a == 3 ) { mtext(side = 4, outer = F, "time-invariant", line = 1) }
  if (a == 6 ) { mtext(side = 4, outer = F, "time-varying", line = 1) }
}

dev.off()

#=======================================================================================================
# OM Recovery Distributions ================================================================
#=======================================================================================================

png(filename = "OMRecoveryDist.png", width = 6.7, height = 5, units = 'in', res = 256)

par(mfrow= c(2,3), mar = c(0.2,0.2,0.2,0.2), oma = c(4,4,2,4), cex.axis = 1.1, cex.lab = 1.1)
letter.cex = 1; axis.cex = 1.2; label.cex = 0.8
ass.plot.yrs = ifelse(ass.freq == 8, 13, 26)
ass.plot.yrs = ifelse(ass.freq == 6, 18, ass.plot.yrs)
min = -1.5; max = 2
to.plot = ass.yrs[1:ass.plot.yrs]

for (a in 1:ds){
  hist(om.out[[1]]$om.time.over[a,], axes = F, main = "", xlim = c(0, 110), ylim = c(0, 30))
  #abline(v = median(plot.matrix[a,]), lty = 2)
  box()
  print.letter(sum(om.out[[1]]$om.time.over[a,] == 101), xy = c(0.92,0.90), cex = 1.1)
  if (a == 1) {mtext(side = 2, "Frequeny", outer = T, line = 1)}
  if (a > 3) axis(side = 1)
  print.letter(alpha.label[a], xy = c(0.07, 0.95), cex = 1.1) 
  if (a < 4 )  { mtext(side = 3, outer = F, name.label[a]) }
  if (a == 3 ) { mtext(side = 4, outer = F, "time-invariant", line = 1) }
  if (a == 6 ) { mtext(side = 4, outer = F, "time-varying", line = 1) }
}

dev.off()

#=======================================================================================================
# Total Catch While Overfished ================================================================
#=======================================================================================================

png(filename = "Total_OverfishedCatch.png", width = 6.7, height = 5, units = 'in', res = 256)

par(mfrow= c(1,2), mar = c(0.2,0.2,0.2,0.2), oma = c(5,5,2,4), cex.axis = 1.1, cex.lab = 1.1)
letter.cex = 1; axis.cex = 1.2; label.cex = 0.8
ass.plot.yrs = ifelse(ass.freq == 8, 13, 26)
ass.plot.yrs = ifelse(ass.freq == 6, 18, ass.plot.yrs)
min = 0; max = 20000
to.plot = ass.yrs[1:ass.plot.yrs]

boxplot(t(est.out[[1]]$over.catch[1:3,]), ylim = c(min, max), col = rep('grey',length(to.plot)), axes= F, na.action = TRUE)
print.letter(length(med.out[[1]]$filt.re.time.over[[1]]) , xy = c(0.20, 0.03))
print.letter(length(med.out[[1]]$filt.re.time.over[[2]]) , xy = c(0.50, 0.03))
print.letter(length(med.out[[1]]$filt.re.time.over[[3]]) , xy = c(0.80, 0.03))
box()
axis(side = 2, las = 1)
mtext(side = 2, outer = T, "Total overfished catch", line = 3.5)
mtext(side = 1, outer = T, "Data scenario", line = 3)
axis(side = 1, at = 1:3, labels = c('full data', "reduced data", "historical data"))
mtext(side = 3, outer = F, "time-invariant")


boxplot(t(est.out[[1]]$over.catch[4:6,]), ylim = c(min, max), col = rep('grey',length(to.plot)), axes= F)
box()
print.letter(length(med.out[[1]]$filt.re.time.over[[4]]) , xy = c(0.20, 0.03))
print.letter(length(med.out[[1]]$filt.re.time.over[[5]]) , xy = c(0.50, 0.03))
print.letter(length(med.out[[1]]$filt.re.time.over[[6]]) , xy = c(0.80, 0.03))
axis(side = 1, at = 1:3, labels = c('full data', "reduced data", "historical data"))
mtext(side = 3, outer = F, "time-varying")

dev.off()

#=======================================================================================================
# Average Catch While Overfished ================================================================
#=======================================================================================================

png(filename = "Ave_OverfishedCatch.png", width = 6.7, height = 5, units = 'in', res = 256)

par(mfrow= c(1,2), mar = c(0.2,0.2,0.2,0.2), oma = c(4,4,2,4), cex.axis = 1.1, cex.lab = 1.1)
letter.cex = 1; axis.cex = 1.2; label.cex = 0.8
ass.plot.yrs = ifelse(ass.freq == 8, 13, 26)
ass.plot.yrs = ifelse(ass.freq == 6, 18, ass.plot.yrs)
min = 0; max = 125
to.plot = ass.yrs[1:ass.plot.yrs]

boxplot(t(est.out[[1]]$ave.over.catch[1:3,]), ylim = c(min, max), col = rep('grey',length(to.plot)), axes= F, na.action = TRUE)
box()
axis(side = 2, las = 1)
mtext(side = 2, outer = T, "Average overfished catch", line = 2.5)
mtext(side = 1, outer = T, "Data scenario", line = 2.5)
axis(side = 1, at = 1:3, labels = c('full data', "reduced data", "historical data"))
mtext(side = 3, outer = F, "time-invariant")

boxplot(t(est.out[[1]]$ave.over.catch[4:6,]), ylim = c(min, max), col = rep('grey',length(to.plot)), axes= F)
box()
axis(side = 1, at = 1:3, labels = c('full data', "reduced data", "historical data"))
mtext(side = 3, outer = F, "time-varying")


dev.off()

#=======================================================================================================
# AAV  ================================================================
#=======================================================================================================

png(filename = "AAV.png", width = 6.7, height = 5, units = 'in', res = 256)

par(mfrow= c(1,2), mar = c(0.2,0.2,0.2,0.2), oma = c(4,4,2,4), cex.axis = 1.1, cex.lab = 1.1)
letter.cex = 1; axis.cex = 1.2; label.cex = 0.8
ass.plot.yrs = ifelse(ass.freq == 8, 13, 26)
ass.plot.yrs = ifelse(ass.freq == 6, 18, ass.plot.yrs)
min = 0; max = 8
to.plot = ass.yrs[1:ass.plot.yrs]

boxplot(t(med.out[[1]]$aav[1:3,]), ylim = c(min, max), col = rep('grey',length(to.plot)), axes= F, na.action = TRUE)
box()
axis(side = 2, las = 1)
mtext(side = 2, outer = T, "AAV", line = 2.5)
mtext(side = 1, outer = T, "Data scenario", line = 2.5)
axis(side = 1, at = 1:3, labels = c('full data', "reduced data", "historical data"))
mtext(side = 3, outer = F, "time-invariant")

boxplot(t(med.out[[1]]$aav[4:6,]), ylim = c(min, max), col = rep('grey',length(to.plot)), axes= F)
box()
axis(side = 1, at = 1:3, labels = c('full data', "reduced data", "historical data"))
mtext(side = 3, outer = F, "time-varying")


dev.off()

#=======================================================================================================
# Total Catch Over All Years ================================================================
#=======================================================================================================

png(filename = "TotalCatch.png", width = 6.7, height = 5, units = 'in', res = 256)

par(mfrow= c(1,2), mar = c(0.2,0.2,0.2,0.2), oma = c(4,4,2,4), cex.axis = 1.1, cex.lab = 1.1)
letter.cex = 1; axis.cex = 1.2; label.cex = 0.8
ass.plot.yrs = ifelse(ass.freq == 8, 13, 26)
ass.plot.yrs = ifelse(ass.freq == 6, 18, ass.plot.yrs)
min = 0; max = 40000
to.plot = ass.yrs[1:ass.plot.yrs]

plot <- matrix(0,ds,100)
for(a in 1:ds){ plot[a,] = apply(est.out[[1]]$acl.est[a,120:220,], 2, sum) }

boxplot(t(plot[1:3,]), ylim = c(min, max), col = rep('grey',length(to.plot)), axes= F, na.action = TRUE)
box()
axis(side = 2)
mtext(side = 2, outer = T, "Total Catch", line = 2.5)
#axis(side = 1, at = 1:length(to.plot), labels = to.plot - pre.yrs + 1)
mtext(side = 1, outer = T, "Scenario", line = 2.5)

boxplot(t(plot[4:6,]), ylim = c(min, max), col = rep('grey',length(to.plot)), axes= F)
box()

dev.off()



#=========================================================================================================
# Relative Error about catch
#=========================================================================================================
png(filename = "RE_catch.png", width = 6.7, height = 3, units = 'in', res = 256)

par(mfrow= c(2,3), mar = c(0.2,0.2,0.2,0.2), oma = c(4,4,2,4), cex.axis = 1.1, cex.lab = 1.1)
letter.cex = 1; axis.cex = 1.2; label.cex = 0.8
ass.plot.yrs = ifelse(ass.freq == 8, 13, 26)
ass.plot.yrs = ifelse(ass.freq == 6, 18, ass.plot.yrs)
to.plot = ass.yrs[1:ass.plot.yrs]
limit.yrs = 1:75
min = -2; max = 2

for(a in 1:ds){
  boxplot(t(med.out[[1]]$re.catch[a,limit.yrs,]), ylim = c(min, max), col = rep('grey',length(to.plot)), axes= F)
  box(); abline(h = 0)
  if( a ==1 || a==4) { axis(side = 2, las = 1)
      mtext(side = 2, outer = T, "RE catch", line = 2.5)} 
  if( a > 3){
    axis(side = 1, at = seq(1, max(limit.yrs), 10), labels = seq(50, max(limit.yrs) + 50, 10))
    mtext(side = 1, outer = T, "Assessment year", line = 2.5)
  }
  if (a < 4)  { mtext(side = 3, outer = F, name.label[a]) }
  if (a == 3 ){ mtext(side = 4, outer = F, "time-invariant", line = 1) }
  if (a == 6 ){ mtext(side = 4, outer = F, "time-varying", line = 1) }
  print.letter(alpha.label[a], xy = c(0.07, 0.05), cex = 1.1)  
}


dev.off()

#=========================================================================================================
# RMSE over time for spawning biomass
#=========================================================================================================
png(filename = "RMSE_SSB_full.png", width = 6.7, height = 3.5, units = 'in', res = 256)

par(mfrow= c(1,2), mar = c(0.2,0.2,0.2,0.2), oma = c(4,4,2,4), cex.axis = 1.1, cex.lab = 1.1)
letter.cex = 1; axis.cex = 1; label.cex = 1
ass.plot.yrs = ifelse(ass.freq == 8, 13, 26)
ass.plot.yrs = ifelse(ass.freq == 6, 18, ass.plot.yrs)
min = 0; max = max(med.out[[1]]$rmse.ssb.ass[4,]+5)
pch.vec = rep(16:18,2)

for (a in 1:ds){
  index = ass.yrs - pre.yrs
  if (a == 1 || a == 4){
    plot(index, med.out[[1]]$rmse.ssb.ass[a,], type = 'l', lwd = 1, ylim = c(min, max), axes = F, xaxs="i")
    points(index, med.out[[1]]$rmse.ssb.ass[a,], pch = pch.vec[a])
    box()
    if (a == 1) { mtext(side = 1, "Assessment Year", outer = T, cex = label.cex, line = 2)
                  axis(side = 2, at = seq(0, max, 20), cex = axis.cex, las = 1)
                  mtext(side = 2, "RMSE spawning biomass", cex = label.cex, outer = T, line = 2)
                  mtext(side = 3, outer = F, "time-invariant")
                  axis(side = 1, at = seq(index[1], (max(index)-5), 25), labels = seq(50, 125, 25))
                  print.letter(alpha.label[a], xy = c(0.05, 0.95), cex = 1)  }
    if( a == 4) { print.letter(alpha.label[a-2], xy = c(0.05, 0.95), cex = 1) 
                  mtext(side = 3, outer = F, "time-varying")
                  axis(side = 1, at = seq(index[1], max(index), 25), labels = seq(50, 150, 25))
                  legend("topright", legend = name.label, pch = pch.vec, bty= 'n')  }

  }
  lines(index, med.out[[1]]$rmse.ssb.ass[a,], lwd = 1, col = 1)
  points(index, med.out[[1]]$rmse.ssb.ass[a,], pch = pch.vec[a], col = 1)
}

dev.off()

#=========================================================================================================
# RMSE over time for Depletion
#=========================================================================================================
png(filename = "RMSE_depl_full.png", width = 6.7, height = 3.5, units = 'in', res = 256)

par(mfrow= c(1,2), mar = c(0.2,0.2,0.2,0.2), oma = c(4,4,2,4), cex.axis = 1.1, cex.lab = 1.1)
letter.cex = 0.9; axis.cex = 0.9; label.cex = 0.9
ass.plot.yrs = ifelse(ass.freq == 8, 13, 26)
ass.plot.yrs = ifelse(ass.freq == 6, 18, ass.plot.yrs)
min = 0; max = max(med.out[[1]]$rmse.depl[1:5,]) + 10
pch.vec = rep(16:18,2)

for (a in 1:ds){
  index = ass.yrs - pre.yrs
  if (a == 1 || a == 4){
    plot(index, med.out[[1]]$rmse.depl[a,], type = 'l', lwd = 1, ylim = c(min, max), axes = F)
    points(index, med.out[[1]]$rmse.depl[a,], pch = pch.vec[a])
    box()
    #axis(side = 1, at = seq(50,150, 10))
    if (a == 1) { mtext(side = 1, "Assessment Year", outer = T, cex = label.cex, line = 2)
                  axis(side = 2, at = seq(0, max, 20), las = 1, cex.axis = axis.cex)
                  axis(side = 1, at = seq(index[1], (max(index)-5), 25), labels = seq(50, 125, 25), cex.axis = axis.cex)
                  mtext(side = 2, "RMSE relative spawning biomass", cex = label.cex, outer = T, line = 2)
                  mtext(side = 3, outer = F, "time-invariant", cex = label.cex)
                  print.letter(alpha.label[a], xy = c(0.05, 0.95), cex = letter.cex)  }
    if( a == 4) { print.letter(alpha.label[a-2], xy = c(0.05, 0.95), cex = letter.cex)  
                  axis(side = 1, at = seq(index[1], max(index), 25), labels = seq(50, 150, 25), cex.axis = axis.cex)
                  mtext(side = 3, outer = F, "time-varying", cex = label.cex)}
    if( a == 1) { legend("topright", legend = name.label, pch = pch.vec, bty= 'n', cex = label.cex)}
  }
  lines(index, med.out[[1]]$rmse.depl[a,], lwd = 1, col = 1)
  points(index, med.out[[1]]$rmse.depl[a,], pch = pch.vec[a], col = 1)
}

dev.off()

#=========================================================================================================
# MARE over time for Depletion
#=========================================================================================================
png(filename = "MARE_depl_full.png", width = 6.7, height = 3.5, units = 'in', res = 256)

par(mfrow= c(1,2), mar = c(0.2,0.2,0.2,0.2), oma = c(4,4,2,4), cex.axis = 1.1, cex.lab = 1.1)
letter.cex = 1; axis.cex = 1; label.cex = 1
ass.plot.yrs = ifelse(ass.freq == 8, 13, 26)
ass.plot.yrs = ifelse(ass.freq == 6, 18, ass.plot.yrs)
min = 0; max = 0.15
pch.vec = rep(16:18,2)

med.mare = matrix(0, 6, ass.plot.yrs)
for (a in 1:ds){
  for(b in 1:ass.plot.yrs){
    ind = 49 + b* ass.freq - ass.freq
    med.mare[a,b] = median(med.out[[1]]$mare.depl[a,b,ind,])
  }
}

for (a in 1:ds){
  index = ass.yrs - pre.yrs
  if (a == 1 || a == 4){
    plot(index,   med.mare[a,], type = 'l', lwd = 1, ylim = c(min, max), axes = F, xaxs="i")
    points(index, med.mare[a,], pch = pch.vec[a])
    box()
    #axis(side = 1, at = seq(50,150, 10))
    if (a == 1) { mtext(side = 1, "Assessment Year", outer = T, cex = label.cex, line = 2)
                  axis(side = 2, at = seq(0, max, 20), las = 1, cex = axis.cex)
                  axis(side = 1, at = seq(index[1], (max(index)-5), 25), labels = seq(50, 125, 25))
                  mtext(side = 2, "MARE depletion", cex = label.cex, outer = T, line = 2)
                  print.letter(alpha.label[a], xy = c(0.05, 0.95), cex = 1)  }
    if( a == 4) { print.letter(alpha.label[a-2], xy = c(0.05, 0.95), cex = 1)  
                  axis(side = 1, at = seq(index[1], max(index), 25), labels = seq(50, 150, 25))
                  legend("topright", legend = name.label, pch = pch.vec, bty= 'n')}
  }
  lines(index,   med.mare[a,], lwd = 1, col = 1)
  points(index,  med.mare[a,], pch = pch.vec[a], col = 1)
}

dev.off()

#=========================================================================================================
# Steepness ================================================================================
#=========================================================================================================
png(filename = "h.png", width = 6.7, height = 5, units = 'in', res = 256)
par(mfrow= c(2,3), mar = c(0.1,0.1,0.1,0.1), oma = c(4,4,4,4), cex.axis = 1.1, cex.lab = 1.1)
ymin = 0.20 ; ymax = 1.1
ind = 1:14
ass.num = ifelse(ass.freq == 8, 13, 26)

for(b in 1:ds){
  box95(x=t(est.out[[1]]$h.est[b,,]), list = F, ylab = "Steepness", ylim = c(ymin,ymax), col = rep('grey',ass.num), axes = F,
     boxwex = rep(0.70, length(to.plot)))
  abline(h = steep, lty = 2, col = 1); box()
  print.letter(alpha.label[b], xy = c(0.95, 0.95))
  
  if (b == 1 || b == 4) { axis(side = 2) }
  if (b > 3) { axis(side =1, at = seq(1,18,2), labels = seq(hist.yrs, hist.yrs + 100, 12)) }
  if (b < 4 ) { mtext(side = 3, outer = F, name.label[b]) }
  if (b == 3) { mtext(side = 4, outer = F, "time-invariant", line = 1) }
  if (b == 6) { mtext(side = 4, outer = F, "time-varying", line = 1) }
  if (b == ds) { mtext(side = 1, "Assessment Year", outer = T, line = 2.5) 
       mtext(side = 2, "Steepness", outer =T, line = 2.5) }
}
dev.off()

#=========================================================================================================
# RE steepness ================================================================================
#=========================================================================================================
png(filename = "re_h.png", width = 6.7, height = 5, units = 'in', res = 256)
par(mfrow= c(2,3), mar = c(0.1,0.1,0.1,0.1), oma = c(4,5,4,4), cex.axis = 1.1, cex.lab = 1)
ymin = -0.60 ; ymax = 0.70
ind = 1:14
ass.num = ifelse(ass.freq == 8, 13, 26)
to.plot = ass.yrs[1:ass.plot.yrs]
for(b in 1:ds){
  boxplot(t(med.out[[1]]$re.h[b,,]), ylab = "RE steepness", ylim = c(ymin,ymax), col = rep('grey',ass.num), axes = F)
  abline(h = 0, lty = 2, col = 1); box()
  print.letter(alpha.label[b], xy = c(0.05, 0.95))

  per.of = med.out[[1]]$n.overfished[a,text.yr] 
  text( labels = paste0(per.of, "%"), x = text.yr, y = rep(-1, length(text.yr)), cex = 1 )
  if (b == 1 || b == 4) { axis(side = 2, las = 1) }
  if (b > 3) { axis(side = 1, at = seq(1,length(to.plot),4), 
        labels = seq(to.plot[1]- pre.yrs + 1, to.plot[length(to.plot)] - pre.yrs + 1, 4*ass.freq) ) }
  if (b < 4 ) { mtext(side = 3, outer = F, name.label[b]) }
  if (b == 3) { mtext(side = 4, outer = F, "time-invariant", line = 1) }
  if (b == 6) { mtext(side = 4, outer = F, "time-varying", line = 1) }
  if (b == ds) { mtext(side = 1, "Assessment Year", outer = T, line = 2.5,cex = 1) 
       mtext(side = 2, "RE steepness", outer =T, line = 3, cex = 1) }
}
dev.off()

#=========================================================================================================
# Relative error of k ================================================================================
#=========================================================================================================
png(filename = "RE_k.png", width = 6.7, height = 5, units = 'in', res = 256)
par(mfrow= c(2,3), mar = c(0.1,0.1,0.1,0.1), oma = c(4,4,4,4), cex.axis = 1.1, cex.lab = 1.1)
ymin = -0.35 ; ymax = 0.35
ind = 1:14
ass.num = ifelse(ass.freq == 8, 13, 26)
text.yr = c(2, 7, 11,15)

for(b in 1:ds){
  boxplot(t(med.out[[1]]$re.k[b,,]), ylab = "", ylim = c(ymin,ymax), col = rep('grey',ass.num), axes = F)
  abline(h = 0, lty = 2, col = 1); box()
  print.letter(alpha.label[b], xy = c(0.05, 0.05))
  
  if (b == 1 || b == 4) { axis(side = 2) }
  if (b > 3) { axis(side =1, at = seq(1,26,3), labels = seq(hist.yrs, hist.yrs + 100, 12)) }
  if (b < 4 ) { mtext(side = 3, outer = F, name.label[b]) }
  if (b == 3) { mtext(side = 4, outer = F, "Time-invariant", line = 1) }
  if (b == 6) { mtext(side = 4, outer = F, "Time-varying", line = 1) }
  if (b == ds) { mtext(side = 1, "Assessment Year", outer = T, line = 2.5) 
       mtext(side = 2, "RE coefficient of growth", outer =T, line = 2.5) }
}

dev.off()

#=========================================================================================================
# Relative error of M ================================================================================
#=========================================================================================================
png(filename = "RE_M.png", width = 6.7, height = 5, units = 'in', res = 256)
par(mfrow= c(2,3), mar = c(0.1,0.1,0.1,0.1), oma = c(4,4,4,4), cex.axis = 1.1, cex.lab = 1.1)
ymin = -0.5 ; ymax = 0.5
ind = 1:14
ass.num = ifelse(ass.freq == 8, 13, 26)

for(b in 1:ds){
  boxplot(t(med.out[[1]]$re.m[b,,]), ylab = "", ylim = c(ymin,ymax), col = rep('grey',ass.num), axes = F)
  abline(h = 0, lty = 2, col = 1); box()
  print.letter(alpha.label[b], xy = c(0.05, 0.05))
  
  if (b == 1 || b == 4) { axis(side = 2) }
  if (b > 3) { axis(side =1, at = seq(1,26,3), labels = seq(hist.yrs, hist.yrs + 100, 12)) }
  if (b < 4 ) { mtext(side = 3, outer = F, name.label[b]) }
  if (b == 3) { mtext(side = 4, outer = F, "Time-invariant", line = 1) }
  if (b == 6) { mtext(side = 4, outer = F, "Time-varying", line = 1) }
  if (b == ds) { mtext(side = 1, "Assessment Year", outer = T, line = 2.5) 
       mtext(side = 2, "RE M", outer =T, line = 2.5) }
}
dev.off()

#=========================================================================================================
# Relative error of lmin ================================================================================
#=========================================================================================================
png(filename = "RE_lmin.png", width = 6.7, height = 5, units = 'in', res = 256)
par(mfrow= c(2,3), mar = c(0.1,0.1,0.1,0.1), oma = c(4,4,4,4), cex.axis = 1.1, cex.lab = 1.1)
ymin = -0.25 ; ymax = 0.25
ind = 1:14
ass.num = ifelse(ass.freq == 8, 13, 26)

for(b in 1:ds){
  boxplot(t(med.out[[1]]$re.lmin[b,,]), ylab = "", ylim = c(ymin,ymax), col = rep('grey',ass.num), axes = F)
  abline(h = 0, lty = 2, col = 1); box()
  print.letter(alpha.label[b], xy = c(0.05, 0.95))
  
  if (b == 1 || b == 4) { axis(side = 2) }
  if (b > 3) { axis(side =1, at = seq(1,26,3), labels = seq(hist.yrs, hist.yrs + 100, 12)) }
  if (b < 4 ) { mtext(side = 3, outer = F, name.label[b]) }
  if (b == 3) { mtext(side = 4, outer = F, "Time-invariant", line = 1) }
  if (b == 6) { mtext(side = 4, outer = F, "Time-varying", line = 1) }
  if (b == ds) { mtext(side = 1, "Assessment Year", outer = T, line = 2.5) 
       mtext(side = 2, "RE Lmin", outer =T, line = 2.5) }
}
dev.off()

#=========================================================================================================
# Relative error of lmax ================================================================================
#=========================================================================================================
png(filename = "RE_lmax.png", width = 6.7, height = 5, units = 'in', res = 256)
par(mfrow= c(2,3), mar = c(0.1,0.1,0.1,0.1), oma = c(4,4,4,4), cex.axis = 1.1, cex.lab = 1.1)
ymin = -0.25 ; ymax = 0.25
ind = 1:14
ass.num = ifelse(ass.freq == 8, 13, 26)

for(b in 1:ds){
  boxplot(t(med.out[[1]]$re.lmax[b,,]), ylab = "", ylim = c(ymin,ymax), col = rep('grey',ass.num), axes = F)
  abline(h = 0, lty = 2, col = 1); box()
  print.letter(alpha.label[b], xy = c(0.05, 0.95))
  
  if (b == 1 || b == 4) { axis(side = 2) }
  if (b > 3) { axis(side =1, at = seq(1,26,3), labels = seq(hist.yrs, hist.yrs + 100, 12)) }
  if (b < 4 ) { mtext(side = 3, outer = F, name.label[b]) }
  if (b == 3) { mtext(side = 4, outer = F, "Time-invariant", line = 1) }
  if (b == 6) { mtext(side = 4, outer = F, "Time-varying", line = 1) }
  if (b == ds) { mtext(side = 1, "Assessment Year", outer = T, line = 2.5) 
       mtext(side = 2, "RE Lmax", outer =T, line = 2.5) }
}
dev.off()


#=========================================================================================================
#Selectivity Plots ==========================================================================
#=========================================================================================================
png(filename = "FisherySelect.png", width = 6.7, height = 4.5, units = 'in', res = 256)

alpha.label = c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)", '(g)', '(h)', "(i)", '(j)', "(k)", "(l)")
scenario.lab = c("full data", "reduced data", "full data", "reduced data")
alpha.cex = 1 ; lab.cex = 0.80
ds.list = c(1,2,4,5)
to.plot = ass.yrs[1:ass.plot.yrs]
par(mfrow = c(2,length(ds.list)), mar = c(0.2, 0.2, 0.2, 0.2), oma = c(5,5,5,5))

ymin = -0.07; ymax = 0.11
for(b in 1:length(ds.list)){
  a = ds.list[b]
  boxplot(t(med.out[[1]]$re.f.selex[a,1,,]), ylim = c(ymin, ymax), axes = F)
  abline(h = 0,col = 1, lty =2); box() ; 
  if( b == 1) {axis (side = 2, las = 1) }
  print.letter(alpha.label[b], xy = c(0.93, 0.95), cex = alpha.cex)
  print.letter(scenario.lab[b], xy = c(0.50, 0.95), cex = alpha.cex)
  if( b == 1) { mtext(side = 3, "time-invariant", line = 1, adj = 1.5, cex = lab.cex)}
  if( b == 3) { mtext(side = 3, "time-varying",   line = 1, adj = 1.5, cex = lab.cex)}
  if( b == 4) { mtext(side = 4, "Size at maximum", line = 1, cex =lab.cex)
                mtext(side = 4,  "selectivity", line = 2, cex = lab.cex)  }
}

ymin = -2.5; ymax = 3.1
for (b in 1:length(ds.list)){
  a = ds.list[b]
  boxplot(t(rbind(0, med.out[[1]]$re.f.selex[a,3,2:17,])), ylim = c(ymin, ymax), xlim = c(1,18),axes = F)
  abline(h = 0, col = 1, lty =2); box() 
  if( b == 1) {axis(side = 2, las = 1) }
  print.letter(alpha.label[b + 4], xy = c(0.93, 0.95), cex = alpha.cex)#; axis(side = 2)
  if (b == 4) { mtext(side = 4, "Length at peak", line = 1, cex = lab.cex) 
                mtext(side = 4,  "selectivity", line = 2, cex = lab.cex)}
  axis(side = 1, at = seq(1,length(to.plot),4), 
        labels = seq(to.plot[1]- pre.yrs + 1, to.plot[length(to.plot)] - pre.yrs + 1, 4*ass.freq) ) 
}

mtext(side =2, "Relative error",  outer = T, line = 3, cex = lab.cex)
mtext(side =1, "Assessment Year", outer = T, line = 3, cex = lab.cex)

dev.off()

#=========================================================================================================
# Relative Error Selectivity Plots Alternative Versions  ===============================================
#=========================================================================================================
png(filename = "RE_FisherySelect_peak.png", width = 6.7, height = 4.5, units = 'in', res = 256)

alpha.label = c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)", '(g)', '(h)', "(i)", '(j)', "(k)", "(l)")
scenario.lab = c("full data", "reduced data", "eliminated data")
alpha.cex = 1 ; lab.cex = 1
ass.plot.yrs = ifelse(ass.freq == 8, 13, 26)
ass.plot.yrs = ifelse(ass.freq == 6, 18, ass.plot.yrs)
to.plot = ass.yrs[1:ass.plot.yrs]

par(mfrow = c(2, ds/2 ), mar = c(0.2, 0.2, 0.2, 0.2), oma = c(5,5,5,5))
ymin = -0.07; ymax = 0.11
for(b in 1:ds){
  box95(x=t(med.out[[1]]$re.f.selex[b,1,,]), ylim = c(ymin, ymax), axes = F)
  abline(h = 0,col = 1, lty =2); box() ; 
  if( b == 1 || b == 4) {axis (side = 2, las = 1) }
  print.letter(alpha.label[b], xy = c(0.93, 0.95), cex = alpha.cex)
  #print.letter(scenario.lab[b], xy = c(0.50, 0.95), cex = alpha.cex)
  mtext(side = 3, scenario.lab[b], cex = alpha.cex)
  if( b == 3) { mtext(side = 4, "time-invariant", line = 1, cex = lab.cex)}
  if( b == ds) { mtext(side = 4, "time-varying",  line = 1, cex = lab.cex)}
  #if( b == 1) { mtext(side = 3, "Size at maximum selectivity", line = 1, outer = T, cex =lab.cex) }
  if( b >= 4) {   axis(side = 1, at = seq(1,length(to.plot),4), 
        labels = seq(to.plot[1]- pre.yrs + 1, to.plot[length(to.plot)] - pre.yrs + 1, 4*ass.freq) ) }
}

mtext(side =2, "RE size at maximum selectivity",  outer = T, line = 3, cex = lab.cex)
mtext(side =1, "Assessment Year", outer = T, line = 3, cex = lab.cex)
dev.off()

png(filename = "RE_FisherySelect_dome.png", width = 6.7, height = 4.5, units = 'in', res = 256)

alpha.label = c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)", '(g)', '(h)', "(i)", '(j)', "(k)", "(l)")
scenario.lab = c("full data", "reduced data")
alpha.cex = 1 ; lab.cex = 0.80
ds.list = c(1,2,4,5)
par(mfrow = c(2, length(ds.list)/2), mar = c(0.2, 0.2, 0.2, 0.2), oma = c(5,5,5,5))

ymin = -2.5; ymax = 3.3
for (b in 1:length(ds.list)){
  a = ds.list[b]
  boxplot(t(rbind(0, med.out[[1]]$re.f.selex[a,3,2:17,])), ylim = c(ymin, ymax), xlim = c(1,18),axes = F)
  abline(h = 0, col = 1, lty =2); box() 
  if( b == 1 || b == 3) {axis(side = 2, las = 1) }
  print.letter(alpha.label[b], xy = c(0.93, 0.95), cex = alpha.cex)
  print.letter(scenario.lab[b], xy = c(0.50, 0.95), cex = alpha.cex)
  if( b == 2) { mtext(side = 4, "time-invariant", line = 1, cex = lab.cex)}
  if( b == 4) { mtext(side = 4, "time-varying",  line = 1, cex = lab.cex)}
  if (b >= 3) { axis(side = 1, at = seq(1,length(to.plot),4), 
        labels = seq(to.plot[1]- pre.yrs + 1, to.plot[length(to.plot)] - pre.yrs + 1, 4*ass.freq) ) }
}
mtext(side =2, "RE width at maximum selectivity",  outer = T, line = 3, cex = lab.cex)
mtext(side =1, "Assessment Year", outer = T, line = 3, cex = lab.cex)

dev.off()

#=========================================================================================================
#Selectivity Plots Alternative Versions  =================================================================
#=========================================================================================================
png(filename = "FisherySelect_peak.png", width = 6.7, height = 4.5, units = 'in', res = 256)

alpha.label = c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)", '(g)', '(h)', "(i)", '(j)', "(k)", "(l)")
scenario.lab = c("full data", "reduced data", "eliminated data")
alpha.cex = 1 ; lab.cex = 1
ass.plot.yrs = ifelse(ass.freq == 8, 13, 26)
ass.plot.yrs = ifelse(ass.freq == 6, 18, ass.plot.yrs)
to.plot = ass.yrs[1:ass.plot.yrs]

par(mfrow = c(2, ds/2 ), mar = c(0.2, 0.2, 0.2, 0.2), oma = c(5,5,5,5))
ymin = 42; ymax = 49
for(b in 1:ds){
  box95(x=t(est.out[[1]]$f.selex.est[b,1,,]), ylim = c(ymin, ymax), axes = F,col = 'grey')
  abline(h = 45, col = 1, lty =2); box() ; 
  if( b == 1 || b == 4) {axis (side = 2, at = seq(ymin, ymax, 2), las = 1) }
  print.letter(alpha.label[b], xy = c(0.93, 0.95), cex = alpha.cex)
  #print.letter(scenario.lab[b], xy = c(0.50, 0.95), cex = alpha.cex)
  mtext(side = 3, scenario.lab[b], cex = alpha.cex)
  if( b == 3) { mtext(side = 4, "time-invariant", line = 1, cex = lab.cex)}
  if( b == ds) { mtext(side = 4, "time-varying",  line = 1, cex = lab.cex)}
  #if( b == 1) { mtext(side = 3, "Size at maximum selectivity", line = 1, outer = T, cex =lab.cex) }
  if( b >= 4) {   axis(side = 1, at = seq(1,length(to.plot),4), 
        labels = seq(to.plot[1]- pre.yrs + 1, to.plot[length(to.plot)] - pre.yrs + 1, 4*ass.freq) ) }
}

mtext(side =2, "Size at maximum selectivity (cm)",  outer = T, line = 3, cex = lab.cex)
mtext(side =1, "Assessment Year", outer = T, line = 3, cex = lab.cex)
dev.off()

png(filename = "FisherySelect_dome.png", width = 6.7, height = 4.5, units = 'in', res = 256)

alpha.label = c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)", '(g)', '(h)', "(i)", '(j)', "(k)", "(l)")
scenario.lab = c("full data", "reduced data")
alpha.cex = 1 ; lab.cex = 1
ds.list = c(1,2,4,5)
par(mfrow = c(2, length(ds.list)/2), mar = c(0.2, 0.2, 0.2, 0.2), oma = c(5,5,5,5))
ymin = -10; ymax = 7

for (b in 1:length(ds.list)){
  a = ds.list[b]
  est.out[[1]]$f.selex.adj.est[a,1,1,] = ymax + 5
  box95(x=t(est.out[[1]]$f.selex.adj.est[a,1,,]), ylim = c(ymin, ymax), xlim = c(1,18),axes = F)
  abline(h = -2.5, col = 1, lty =2); box() 
  if( b == 1 || b == 3) {axis(side = 2, las = 1) }
  print.letter(alpha.label[b], xy = c(0.93, 0.95), cex = alpha.cex)
  #print.letter(scenario.lab[b], xy = c(0.50, 0.95), cex = alpha.cex)
  mtext(side = 3, scenario.lab[b], cex = alpha.cex)
  if( b == 2) { mtext(side = 4, "time-invariant", line = 1, cex = lab.cex)}
  if( b == 4) { mtext(side = 4, "time-varying",  line = 1, cex = lab.cex)}
  if (b >= 3) { axis(side = 1, at = seq(1,length(to.plot),4), 
        labels = seq(to.plot[1]- pre.yrs + 1, to.plot[length(to.plot)] - pre.yrs + 1, 4*ass.freq) ) }
}
mtext(side =2, "Width at maximum selectivity",  outer = T, line = 3, cex = lab.cex)
mtext(side =1, "Assessment Year", outer = T, line = 3, cex = lab.cex)

dev.off()

#=============================================================================================================

png(filename = "SurveySelect.png", width = 6.7, height = 5, units = 'in', res = 256)
par(mfrow = c(ds,2), mar = c(1,1,1,1), oma = c(4,4,4,4))
for(a in 1:ds){
  boxplot(t(med.out[[1]]$re.s.selex[a,1,,]), ylim = c(-0.25, 0.25), axes = F)
  box(); abline(h = 0, col = 1, lty =2)
  if(a == 1) {mtext(side = 3, "Survey Peak") }
  if(a == ds) { axis(side = 1)}
  axis(side = 2)

  boxplot(t(med.out[[1]]$re.s.selex[a,2,,]), ylim = c(-0.25, 0.25), axes = F)
  box(); abline(h = 0, col = 1, lty =2)
  if(a == 1) { mtext(side = 3, "Survey Ascending")}
  if(a == ds) { axis(side = 1)}

  if(a == 1){ mtext(side =2, "Relative error", outer = T, line = 2)}
}
dev.off()

#===========================================================================================================
# Output table =============================================================================================
#===========================================================================================================
output = paste(getwd(), "/table_quants", sep = "")
quant.vec = c(0.05, 0.50, 0.95)
failed = om.failed = NULL
ave.over.catch.out = over.out = filt.over.out = aav.out = aav.over.out = filt.time.over = filt.om.time.over = 
      time.over = om.time.over = matrix(NA, 6, 2)
rownames(filt.over.out) = rownames(over.out) = rownames(ave.over.catch.out) = rownames(aav.out)  = rownames(aav.over.out)= rownames(filt.time.over) = rownames(filt.om.time.over) =
    rownames(time.over) = rownames(om.time.over) = c("full", "reduced", "eliminated", "full", "reduced", "eliminated")
colnames(filt.over.out) = colnames(over.out) = colnames(ave.over.catch.out) = colnames(aav.out) = colnames(aav.over.out) = colnames(time.over) = colnames(om.time.over)=
   colnames(filt.time.over) = colnames(filt.om.time.over)= c("median", "90si")
alt.quants = c(0.05, 0.50, 0.95)

ave.temp = apply(est.out[[1]]$ave.over.catch, 1, quantile, quant.vec)
aav.temp = apply(med.out[[1]]$aav, 1, quantile, quant.vec)
aav.over.temp = apply(med.out[[1]]$aav.over, 1, quantile, quant.vec)
over.temp = apply(med.out[[1]]$re.time.over, 1, quantile, quant.vec)


for (a in 1:ds){

  ave.over.catch.out[a, 1] = round(ave.temp[2,a],2)
  ave.over.catch.out[a, 2] = paste0( "(", round(ave.temp[1,a],2), " - ", round(ave.temp[3,a],2), ")")

  over.out[a, 1] = round(over.temp[2,a],2)
  over.out[a, 2] = paste0( "(", round(over.temp[1,a],2), " - ", round(over.temp[3,a],2), ")")

  filt.over.temp = quantile(med.out[[1]]$filt.re.time.over[[a]],quant.vec)
  filt.over.out[a, 1] = round(filt.over.temp[2],2)
  filt.over.out[a, 2] = paste0( "(", round(filt.over.temp[1],2), " - ", round(filt.over.temp[3],2), ")")

  aav.out[a, 1]  = round(aav.temp[2,a],2)
  aav.out[a, 2]  = paste0( "(", round(aav.temp[1,a],2), " - ", round(aav.temp[3,a],2), ")")

  aav.over.out[a, 1]  = round(aav.over.temp[2,a],2)
  aav.over.out[a, 2]  = paste0( "(", round(aav.over.temp[1,a],2), " - ", round(aav.over.temp[3,a],2), ")")

  filter = est.out[[1]]$time.over[a,] != 101
  temp = quantile(est.out[[1]]$time.over[a,filter], quant.vec)
  filt.time.over[a, 1] = temp[2]
  filt.time.over[a, 2] = paste0( "(", round(temp[1],0), " - ", round(temp[3],0), ")")
  failed = c(failed, N - sum(filter))

  filter = om.out[[1]]$om.time.over[a,] != 101
  temp = quantile(om.out[[1]]$om.time.over[a,filter], quant.vec)
  filt.om.time.over[a, 1] = temp[2]
  filt.om.time.over[a, 2] = paste0( "(", round(temp[1],0), " - ", round(temp[3],0), ")")
  om.failed = c(om.failed, N-sum(filter))

  filter = est.out[[1]]$time.over[a,] == 101
  change = est.out[[1]]$time.over[a,]
  change[filter] = 101
  temp = quantile(change, quant.vec)
  time.over[a, 1] = temp[2]
  time.over[a, 2] = paste0( "(", round(temp[1],0), " - ", round(temp[3],0), ")")

  temp = quantile(om.out[[1]]$om.time.over[a,], quant.vec)
  om.time.over[a, 1] = temp[2]
  om.time.over[a, 2] = paste0( "(", round(temp[1],0), " - ", round(temp[3],0), ")")

}

out <- list()
out$ave.over.catch.out = ave.over.catch.out
out$filt.over.out = filt.over.out
out$over.out = over.out
out$aav.out = aav.out
out$aav.over.out = aav.over.out
out$filt.time.over = filt.time.over
out$failed = failed
out$filt.om.time.over = filt.om.time.over
out$om.failed = om.failed
out$time.over = time.over
out$om.time.over = om.time.over
capture.output(out, file = output, append = F)


#RMSE Table ========================================================================================================================
ind = c(2, 3, 4, 5, 8, 11, 13)

c = rock.out[[1]]$rmse.sb0[, ind]
d = rock.out[[1]]$rmse.depl[,ind]

write.table(round(c, 0) , file = "", quote = F, row.names = F, col.names = F)
write.table(round(d, 0) , file = "", quote = F, row.names = F, col.names = F)

#Median Relative Errors ============================================================================================================ 
ind = c(3, 6, 10, 14, 18, 22, 26)  
c = d =NULL
for (i in 1:length(ind)){
  c = cbind(c, apply(rock.out[[1]]$re.ssb0[,ind[i],],1,median))
  y = 50 + ind[i]*4 - 4
  d = cbind(d, apply(rock.out[[1]]$re.depl[,ind[i],y,], 1, median))
}


write.table(round(c, 2) , file = "", quote = F, row.names = F, col.names = F)
write.table(round(d, 2) , file = "", quote = F, row.names = F, col.names = F)


#=========================================================================================================
# Not working miscallaneous plots
#=========================================================================================================
par(mfrow = c(2,3))
color <- rgb(0, 0, 0, 0.30) 
ass.index = rev(c(1, 4, 10, 16, 26))
text.yr = sort((pre.yrs + ass.yr)[ass.index]) - 5
start.plot = 120
a = 256
plot.color = c(rgb(0, 153/a, 0), rgb(255/a, 128/a, 0), "Blue", "violet")
trans = 0.20
trans.color = c(rgb(0, 153/a, 0, trans), rgb(255/a, 128/a, 0, trans+ 0.05), rgb(0,0,1, trans), rgb(0,0.5, 0.5, trans))

lty.type = c(4, 3, 2, 5, 1)
ymax = ceiling( max(med.out[[1]]$med.ssb[,start.plot:final.yr,]))

for(b in 1:ds){
  
  #Plot rockfish
  plot(start.plot:final.yr,  med.out[[1]]$med.ssb[b,start.plot:final.yr,2], col = 1, type = 'l', lwd =2, 
    ylim = c(0,ymax), ylab = "SB", xlab = "Year",  axes = F, xaxs="i")
  if(b ==1) { mtext(side = 3, "Rockfish", outer = T, line = 2.5)
              mtext(side = 2, "SSB", outer = T, line = 2.5) }
  if (b ==1 || b == 3) { axis(side = 2)}
  box()
  if(b == ds) { 
    #axis(side = 1, at =seq(120, 220,20), labels = seq(50, 150, 20))
    axis(side = 1, at =seq(170, 270,20), labels = seq(100, 200, 20))
    mtext("Year", side = 1, outer = T, line = 2.5)
  }
  #Calculate the % determined to be overfished
  #per.of = round(rock.out[[1]]$n.overfished[b,sort(ass.index.rock)]/N.rock,2)
  lines(start.plot:final.yr,  med.out[[1]]$med.ssb[b,start.plot:final.yr,1], col = 'darkgrey', lty = 4, lwd =1)
  lines(start.plot:final.yr,  med.out[[1]]$med.ssb[b,start.plot:final.yr,3], col = 'darkgrey', lty = 4, lwd =1)
  #print.letter(alpha.label2[b], xy = c(0.15, 0.95), cex = 1.1)
  #text(labels = per.of, x = text.yr.rock, y = rep(10, length(text.yr.rock)), cex = 1.2 ) 

  for (i in 1:length(ass.index)){
    temp = ass.index[i]*4 - 5 + hist.yrs + pre.yrs
    y = (start.plot):(temp)
    ind = (start.plot):temp
    xx = c(y, rev(y))
    yy = c(med.out[[1]]$med.ssb.est[b,ind,ass.index[i],1], rev(med.out[[1]]$med.ssb.est[b,ind,ass.index[i],3]))
    polygon(xx, yy, col = trans.color[i], lty = 0)
  }
  lines(start.plot:final.yr,  med.out[[1]]$med.ssb[b,start.plot:final.yr,2], col = 'grey70', lty = 1, lwd =3)
  for (i in 1:length(ass.index))
  {
    temp = ass.index[i]*4 - 5 + hist.yrs + pre.yrs
    y = (start.plot):(temp)
    ind = (start.plot):temp
    lines(y, med.out[[1]]$med.ssb.est[b,ind,ass.index[i],2], col = plot.color[i], lty = lty.type[i], lwd =2)
  }
}

#Depletion================================================================================================================================
png(filename = "Depl_Traj.png", width = 6.7, height = 5, units = 'in', res = 256)

par(mfrow= c(2,3), mar = c(0.1,0.1,0.1,0.1), oma = c(4,4,2,4), cex.axis = 1.1, cex.lab = 1.1)
color <- rgb(0, 0, 0, 0.30) 
text.yr = sort((pre.yrs + ass.yr)[ass.index]) - 5
start.plot = 120
a = 256
plot.color = c(rgb(0, 153/a, 0), rgb(255/a, 128/a, 0), "Blue", "violet")
trans = 0.20
trans.color = c(rgb(0, 153/a, 0, trans), rgb(255/a, 128/a, 0, trans+ 0.05), rgb(0,0,1, trans), rgb(0,0.5, 0.5, trans))

lty.type = c(4, 3, 2, 5, 1)
ymax = ceiling( max(med.out[[1]]$med.ssb[,start.plot:final.yr,]))
ymax = 0.75
ass.index = rev(c(1, 2, 3, 4, 5)) #
ass.index = rev(c(1, 8, 13, 20, 26))


for(b in 1:ds){
   #Plot rockfish
  plot(start.plot:final.yr,  med.out[[1]]$med.depl[b,start.plot:final.yr,2], col = 1, type = 'l', lwd =2, 
    ylim = c(0,ymax), ylab = "Depletion", xlab = "Year",  axes = F, xaxs="i")
  if(b == 1 || b == 3){ axis(side = 2)}
  if(b == 1 ) {mtext("Depeltion", side = 2, outer = T, line = 2)}
  box() ; i
  if(b == ds) { 
      axis(side = 1, at =seq(120, 220,10), labels = seq(50, 150, 10))
      mtext("Year", side = 1, outer = T, line = 2.5)}
  lines(start.plot:final.yr,  med.out[[1]]$med.depl[b,start.plot:final.yr,1], col ='darkgrey', lty = 4, lwd = 1)
  lines(start.plot:final.yr,  med.out[[1]]$med.depl[b,start.plot:final.yr,3], col ='darkgrey', lty = 4, lwd = 1)
  abline(h = 0.40, lty = 3, col = 'red')

  print.letter(alpha.label[b], xy = c(0.15, 0.95), cex = 1.1)

  for (i in 1:length(ass.index))
  {
    temp = ass.index[i]*ass.freq - ass.freq + 1 + hist.yrs + pre.yrs
    y = (start.plot):(temp)
    ind = (start.plot):temp
    xx = c(y, rev(y))
    yy = c(med.out[[1]]$med.depl.est[b,ind,ass.index[i],1], rev(med.out[[1]]$med.depl.est[b,ind,ass.index[i],3]))
    polygon(xx, yy, col = trans.color[i], lty = 0)
  }
  lines(start.plot:final.yr, med.out[[1]]$med.depl[b,start.plot:final.yr,2], col = 'grey70', lty = 1, lwd =3)
  for (i in 1:length(ass.index))
  {
    temp = ass.index[i]*ass.freq - ass.freq + 1 + hist.yrs + pre.yrs
    y = (start.plot):(temp)
    ind = (start.plot):temp
    lines(y, med.out[[1]]$med.depl.est[b,ind,ass.index[i],2], col = plot.color[i], lty = lty.type[i], lwd =2)
  }
}

dev.off()