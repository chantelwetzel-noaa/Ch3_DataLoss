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
run.name = "Final_wo_survey"
#setwd(paste0(drive,"/PhD/Chapter3/", run.name, "/JournalPlots"))
setwd(paste0(drive,"/PhD/Chapter3/WriteUp/Fishery_Bulletin_Submission/revised_submission/JournalPlots"))
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

alpha.label = c('(a)', '(b)', '(c)', '(d)', '(e)', '(f)')

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

#Figure 3 ========================================================================================================
png(filename = "fig3_ressb.png", width = 6.7, height = 6, units = 'in', res = 256)

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

  om.per.of = med.out[[1]]$om.n.overfished[a,text.yr]
  om.scale.per.of = (1-med.out[[1]]$om.n.overfished[a,] / 100)*0.5 + min 

  plot(1:length(to.plot), om.scale.per.of, type = 'l', lwd = 1.5, ylim = c(min, max), axes= F, yaxs = "i")
  lines(1:length(to.plot), scale.per.of, col = 1, lty = 2)
  if(a ==1 || a == 4) { legend("bottomright", legend = c("OM", "EM"), lty = c(1,2), bty = 'n', cex = 1.1) }

  #per.of = med.out[[1]]$n.overfished[a,text.yr]
  #scale.per.of = (1-med.out[[1]]$n.overfished[a,] / 100)*0.5 + min 
  #plot(1:length(to.plot), scale.per.of, type = 'l', lwd = 1.5, ylim = c(min, max), axes= F, yaxs = "i")
  #points(1:length(to.plot), scale.per.of, pch = 21, cex = 0.85)
  
  box95(t(re.ssb), list = F, col = rep('grey',length(to.plot)), axes = F, add = TRUE, boxwex = rep(0.75, length(to.plot)))
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
  if (a == 3 ){ mtext(side = 4, outer = F, "Time-invariant", line = 1) }
  #text(par("usr")[2]*1.03, mean(par("usr")[3:4])+0.1, "time-invarient", srt = -90, xpd = TRUE, pos = 4, cex = 1.5)}
  if (a == 6 ){ mtext(side = 4, outer = F, "Time-varying", line = 1) }
  #text(par("usr")[2]*1.03, mean(par("usr")[3:4])+0.1, "time-varying", srt = -90, xpd = TRUE, pos = 4, cex = 1.5)}
  print.letter(alpha.label[a], xy = c(0.95, 0.95), cex = 1.1)  
}

dev.off()


# Figure 6: Partition the results to only stocks that rebuild ================================================
png(filename = "fig6_ressb_h_compare.png", width = 6.7, height = 6, units = 'in', res = 256)

par(mfrow= c(2,2), mar = c(0.2,0.2,0.2,0.2), oma = c(5,5,2,5), cex.axis = 1.1, cex.lab = 1.1)
letter.cex = 0.9; axis.cex = 1; label.cex = 1
ass.plot.yrs = ifelse(ass.freq == 8, 13, 26)
ass.plot.yrs = ifelse(ass.freq == 6, 18, ass.plot.yrs)
to.plot = ass.yrs[1:ass.plot.yrs]
min = -0.85; max = 1.25
text.yr = c(2, 7, 11,15)
colors = rep( c('red', 'steelblue'), length(to.plot))
colors = rep( c('white', 'darkgrey'), length(to.plot))

for (aa in 1:2){ 
  a = ifelse(aa > 1, 3, 1)
  not = which(est.out[[1]]$time.over[3,] == 101) 
  re.ssb = matrix(0, length(to.plot), N)
  for (b in 1:length(to.plot)){
    re.ssb[b,] = med.out[[1]]$re.ssb[a,b,to.plot[b],]
  }

  temp1 = 1; temp2 = 1
  for( b in 1:36){
    if(b %% 2 != 0){ plot.list[[b]] = re.ssb[temp1,not]; temp1 = temp1 + 1}
    if(b %% 2 == 0){ plot.list[[b]] = re.ssb[temp2,-not]; temp2 = temp2 + 1}
  }

  box95(plot.list, list = T, ylim = c(min, max), col = colors, axes = F, boxwex = rep(0.60, length(to.plot)))
  box(); abline(h = 0) 
  if( a ==1 ) { axis(side = 2, at = seq(-0.5, max, 0.5), las = 1, cex.axis = axis.cex)
                mtext(side = 2, outer = F, "RE spawning biomass", line = 3, cex = label.cex )}
  print.letter(alpha.label[aa], xy = c(0.95, 0.95), cex = letter.cex) 
  mtext(side = 3, outer = F, name.label[a], cex = label.cex)  
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
  abline(h = steep, lty = 2, col = 1); box()
  print.letter(alpha.label[aa+2], xy = c(0.95, 0.95), cex = letter.cex)
  if (a == 1 ) { axis(side = 2, las = 1, cex.axis = axis.cex) }
  axis(side = 1, cex.axis = axis.cex, at = seq(1.5,length(to.plot)*2,8), 
        labels = seq(to.plot[1]- pre.yrs + 1, to.plot[length(to.plot)] - pre.yrs + 1, 4*ass.freq) )
  if (a == 1) { mtext(side = 2, "Steepness", outer =F, line = 2.5, cex = label.cex) }
  if (a == 1) { mtext(side = 1, "Assessment Year", outer = T, line = 2.5, cex = label.cex) }
}

dev.off()

# Figure 6b Time-varying: Partition the results to only stocks that rebuild ================================================
png(filename = "fig6_ressb_tv_h_compare.png", width = 6.7, height = 6, units = 'in', res = 256)

par(mfrow= c(2,2), mar = c(0.2,0.2,0.2,0.2), oma = c(5,5,2,5), cex.axis = 1.1, cex.lab = 1.1)
letter.cex = 0.9; axis.cex = 1; label.cex = 1
ass.plot.yrs = ifelse(ass.freq == 8, 13, 26)
ass.plot.yrs = ifelse(ass.freq == 6, 18, ass.plot.yrs)
to.plot = ass.yrs[1:ass.plot.yrs]
min = -0.85; max = 1.25
text.yr = c(2, 7, 11,15)
colors = rep( c('red', 'steelblue'), length(to.plot))
colors = rep( c('white', 'darkgrey'), length(to.plot))

for (aa in 1:2){ 
  a = ifelse(aa > 1, 6, 4)
  #pulling from the non-time-varying scenario to see how they exactly compare
  not = which(est.out[[1]]$time.over[6,] == 101) 
  re.ssb = matrix(0, length(to.plot), N)
  for (b in 1:length(to.plot)){
    re.ssb[b,] = med.out[[1]]$re.ssb[a,b,to.plot[b],]
  }

  plot.list = list()
  temp1 = 1; temp2 = 1
  for( b in 1:36){
    if(b %% 2 != 0){ plot.list[[b]] = re.ssb[temp1,not]; temp1 = temp1 + 1}
    if(b %% 2 == 0){ plot.list[[b]] = re.ssb[temp2,-not]; temp2 = temp2 + 1}
  }

  box95(plot.list, list = T, ylim = c(min, max), col = colors, axes = F, boxwex = rep(0.60, length(to.plot)))
  box(); abline(h = 0) 
  if( a ==4 ) { axis(side = 2, at = seq(-0.5, max, 0.5), las = 1, cex.axis = axis.cex)
                mtext(side = 2, outer = F, "RE spawning biomass", line = 3, cex = label.cex )}
  print.letter(alpha.label[aa], xy = c(0.95, 0.95), cex = letter.cex) 
  mtext(side = 3, outer = F, name.label[a], cex = label.cex)  
}

ymin = 0.20; ymax = 1.02
steep = 0.65
for(aa in 1:2){
  a = ifelse(aa > 1, 6, 4)
  #pulling from the non-time-varying scenario to see how they exactly compare
  not = which(est.out[[1]]$time.over[6,] == 101) 
  plot.list = list()
  temp1 = 1; temp2 = 1
  for( b in 1:36){
    if(b %% 2 != 0){ plot.list[[b]] = est.out[[1]]$h.est[a,temp1, not]; temp1 = temp1 + 1}
    if(b %% 2 == 0){ plot.list[[b]] = est.out[[1]]$h.est[a,temp2,-not]; temp2 = temp2 + 1}
  }

  box95(plot.list, list = T, ylab = "Steepness", ylim = c(ymin,ymax), col = colors, axes = F,
     boxwex = rep(0.60, length(to.plot)))
  abline(h = steep, lty = 2, col = 1); box()
  print.letter(alpha.label[aa+2], xy = c(0.95, 0.95), cex = letter.cex)
  if (a == 4 ) { axis(side = 2, las = 1, cex.axis = axis.cex) }
  axis(side = 1, cex.axis = axis.cex, at = seq(1.5,length(to.plot)*2,8), 
        labels = seq(to.plot[1]- pre.yrs + 1, to.plot[length(to.plot)] - pre.yrs + 1, 4*ass.freq) )
  if (a == 4) { mtext(side = 2, "Steepness", outer =F, line = 2.5, cex = label.cex) }
  if (a == 4) { mtext(side = 1, "Assessment Year", outer = T, line = 2.5, cex = label.cex) }
}

dev.off()


#================================================================================================

# Figure 4: RE Depl =======================================================================================================
png(filename = "fig4_redepl.png", width = 6.7, height = 6, units = 'in', res = 256)

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
  om.scale.per.of = (1- med.out[[1]]$om.n.overfished[a,] / 100)*0.5 + min 
  plot(1:length(to.plot), om.scale.per.of, type = 'l', lwd = 1.5, ylim = c(min, max), axes= F, yaxs = "i")
  lines(1:length(to.plot), scale.per.of, col = 1, lty = 2)
  if(a == 1 || a == 4) { legend("bottomright", legend = c("OM", "EM"), lty = c(1,2), bty = 'n', cex = 1.1) }
  if(a == 2 || a == 5) { print.letter("% Rebuilt", xy = c(0.50, 0.05), cex = 1.1) }
 
  #scale.per.of = (1- med.out[[1]]$n.overfished[a,] / 100)*0.5 + min 
  #plot(1:length(to.plot), scale.per.of, type = 'l', lwd = 1.5, ylim = c(min, max), axes= F, yaxs = 'i')
  #points(1:length(to.plot), scale.per.of, pch = 21, cex = 0.85)
  box95(t(re.depl), list = F, col = rep('grey',length(to.plot)), axes = F, add = TRUE, boxwex = rep(0.75, length(to.plot)))
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
  if (a == 3 ){ mtext(side = 4, outer = F, "Time-invariant", line = 1) }
  if (a == 6 ){ mtext(side = 4, outer = F, "Time-varying", line = 1) }
  print.letter(alpha.label[a], xy = c(0.95, 0.95), cex = 1.1)  
}

dev.off()

#=====================================================================================================
#==================================================================================================================

#=========================================================================================================
# RMSE over time for spawning biomass
#=========================================================================================================
png(filename = "fig5_rmse.png", width = 6.7, height = 3.5, units = 'in', res = 256)

par(mfrow= c(1,2), mar = c(0.2,0.2,0.2,0.2), oma = c(4,4,2,4), cex.axis = 1.1, cex.lab = 1.1)
#par(mfrow= c(1,2), mar = c(0.2,3,0.2,3), oma = c(4,4,2,4), cex.axis = 1.1, cex.lab = 1.1)
letter.cex = 1; axis.cex = 1; label.cex = 1
ass.plot.yrs = ifelse(ass.freq == 8, 13, 26)
ass.plot.yrs = ifelse(ass.freq == 6, 18, ass.plot.yrs)
min = 0; max = max(med.out[[1]]$rmse.ssb.ass[4,]+5) #; max2 = 360
pch.vec = rep(16:18,2)

for (a in 1:ds){
  index = ass.yrs - pre.yrs
  if (a == 1 || a == 4){

    if (a == 1) { 
      plot(index, med.out[[1]]$rmse.ssb.ass[a,], type = 'l', lwd = 1, ylim = c(min, max), axes = F, xaxs="i")
      points(index, med.out[[1]]$rmse.ssb.ass[a,], pch = pch.vec[a])
      box()
      mtext(side = 1, "Assessment Year", outer = T, cex = label.cex, line = 2)
      axis(side = 2, at = seq(0, max, 20), cex = axis.cex, las = 1)
      mtext(side = 2, "RMSE spawning biomass", cex = label.cex, outer = T, line = 2)
      mtext(side = 3, outer = F, "Time-invariant")
      axis(side = 1, at = seq(index[1], (max(index)-5), 25), labels = seq(50, 125, 25))
      print.letter(alpha.label[a], xy = c(0.05, 0.95), cex = 1)  
      legend("topright", legend = name.label, pch = pch.vec, bty= 'n') }

    if( a == 4) { 
      plot(index, med.out[[1]]$rmse.ssb.ass[a,], type = 'l', lwd = 1, ylim = c(min, max), axes = F, xaxs="i")
      points(index, med.out[[1]]$rmse.ssb.ass[a,], pch = pch.vec[a])
      #axis(side = 2, at = seq(0, max2, 50), cex = axis.cex, las = 1)
      box()
      print.letter(alpha.label[a-2], xy = c(0.05, 0.95), cex = 1) 
      mtext(side = 3, outer = F, "Time-varying")
      axis(side = 1, at = seq(index[1], max(index), 25), labels = seq(50, 150, 25)) }

  }
  lines(index, med.out[[1]]$rmse.ssb.ass[a,], lwd = 1, col = 1)
  points(index, med.out[[1]]$rmse.ssb.ass[a,], pch = pch.vec[a], col = 1)
}

dev.off()

#=========================================================================================================
# Steepness ================================================================================
#=========================================================================================================
png(filename = "fig7_h.png", width = 6.7, height = 6, units = 'in', res = 256)
par(mfrow= c(2,3), mar = c(0.1,0.1,0.1,0.1), oma = c(4,4,4,4), cex.axis = 1.1, cex.lab = 1.1)
ymin = 0.20 ; ymax = 1.2
ind = 1:14
steep = 0.65
ass.num = ifelse(ass.freq == 8, 13, 26)
min = -0.25; max = 1.2

for(b in 1:ds){

  scale.per.of = (1- med.out[[1]]$n.overfished[b,] / 100)*0.25 + min 
  om.scale.per.of = (1- med.out[[1]]$om.n.overfished[b,] / 100)*0.25 + min 
  plot(1:length(to.plot), om.scale.per.of, type = 'l', lwd = 1.5, ylim = c(min, max), axes= F, yaxs = "i")
  lines(1:length(to.plot), scale.per.of, col = 1, lty = 2)
  if(b == 1 || b == 4) { legend("bottomright", legend = c("OM", "EM"), lty = c(1,2), bty = 'n', cex = 1.1) }
  if(b == 2 || b == 5) { print.letter("% Rebuilt", xy = c(0.50, 0.05), cex = 1.1) }


  box95(x=t(est.out[[1]]$h.est[b,,]), list = F, ylab = "Steepness", ylim = c(ymin,ymax), col = rep('grey',ass.num), axes = F,
     boxwex = rep(0.70, length(to.plot)), add = TRUE)
  box(); abline(h= 0); abline(h = steep, lty = 2, col = 1)
  print.letter(alpha.label[b], xy = c(0.95, 0.95))
  
  if (b == 1 || b == 4) { axis(side = 2, at = c(0, 0.30, 0.60, 1)) }
  if (b > 3) { axis(side = 1 , at = seq(1,18,2), labels = seq(hist.yrs, hist.yrs + 100, 12)) }
  if (b < 4 ) { mtext(side = 3, outer = F, name.label[b]) }
  if (b == 3) { mtext(side = 4, outer = F, "time-invariant", line = 1) }
  if (b == 6) { mtext(side = 4, outer = F, "time-varying", line = 1) }
  if (b == ds) { mtext(side = 1, "Assessment Year", outer = T, line = 2.5) 
       mtext(side = 2, "Steepness", outer =T, line = 2.5) }
}
dev.off()


#=========================================================================================================
#Selectivity Plots ==========================================================================
#=========================================================================================================

png(filename = "fig8_FisherySelect_peak.png", width = 6.7, height = 6, units = 'in', res = 256)

#alpha.label = c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)", '(g)', '(h)', "(i)", '(j)', "(k)", "(l)")
#scenario.lab = c("Full data", "Reduced data", "Eliminated data")
alpha.cex = 1 ; lab.cex = 1
ass.plot.yrs = ifelse(ass.freq == 8, 13, 26)
ass.plot.yrs = ifelse(ass.freq == 6, 18, ass.plot.yrs)
to.plot = ass.yrs[1:ass.plot.yrs]
min = 41.5; max = 49

par(mfrow = c(2, ds/2 ), mar = c(0.2, 0.2, 0.2, 0.2), oma = c(5,5,5,5))
ymin = 43; ymax = 49
for(b in 1:ds){
  scale.per.of = (1- med.out[[1]]$n.overfished[b,] / 100)*1.5 + min 
  om.scale.per.of = (1- med.out[[1]]$om.n.overfished[b,] / 100)*1.5 + min 
  plot(1:length(to.plot), om.scale.per.of, type = 'l', lwd = 1.5, ylim = c(min, max), axes= F, yaxs = "i")
  lines(1:length(to.plot), scale.per.of, col = 1, lty = 2)
  if(b == 1 || b == 4) { legend("bottomright", legend = c("OM", "EM"), lty = c(1,2), bty = 'n', cex = 1.1) }
  if(b == 2 || b == 5) { print.letter("% Rebuilt", xy = c(0.50, 0.05), cex = 1.1) }


  box95(x=t(est.out[[1]]$f.selex.est[b,1,,]), ylim = c(ymin, ymax), axes = F,col = 'grey',  add = TRUE)
  abline(h = 45, col = 1, lty =2); box() ; abline (h = 43)
  if( b == 1 || b == 4) {axis (side = 2, at = seq(ymin, ymax, 2), las = 1) }
  print.letter(alpha.label[b], xy = c(0.93, 0.95), cex = alpha.cex)
  mtext(side = 3, name.label[b], cex = alpha.cex)
  if( b == 3) { mtext(side = 4, "Time-invariant", line = 1, cex = lab.cex)}
  if( b == ds) { mtext(side = 4, "Time-varying",  line = 1, cex = lab.cex)}
  if( b >= 4) {   axis(side = 1, at = seq(1,length(to.plot),4), 
        labels = seq(to.plot[1]- pre.yrs + 1, to.plot[length(to.plot)] - pre.yrs + 1, 4*ass.freq) ) }
}

mtext(side =2, "Size at maximum selectivity (cm)",  outer = T, line = 3, cex = lab.cex)
mtext(side =1, "Assessment Year", outer = T, line = 3, cex = lab.cex)
dev.off()

#======================================================================================================
png(filename = "fig9_FisherySelect_dome.png", width = 6.7, height = 6, units = 'in', res = 256)
alpha.cex = 1 ; lab.cex = 1
ds.list = c(1,2,4,5)
par(mfrow = c(2, length(ds.list)/2), mar = c(0.2, 0.2, 0.2, 0.2), oma = c(5,5,5,5))
min = -15; max = 7

for (b in 1:length(ds.list)){
  a = ds.list[b]

  scale.per.of = (1- med.out[[1]]$n.overfished[a,] / 100)*5 + min 
  om.scale.per.of = (1- med.out[[1]]$om.n.overfished[a,] / 100)*5 + min 
  plot(1:length(to.plot), om.scale.per.of, type = 'l', lwd = 1.5, ylim = c(min, max), axes= F, yaxs = "i")
  lines(1:length(to.plot), scale.per.of, col = 1, lty = 2)
  if(b == 1 ) { legend("bottomright", legend = c("OM", "EM"), lty = c(1,2), bty = 'n', cex = 1) }
  if(b == 2 ) { print.letter("% Rebuilt", xy = c(0.50, 0.05), cex = 1) }
  
  est.out[[1]]$f.selex.adj.est[a,1,1,] = ymax + 5
  box95(x=t(est.out[[1]]$f.selex.adj.est[a,1,,]), xlim = c(1,18), axes = F, add = TRUE) #ylim = c(ymin, ymax), 
  abline(h = -2.5, col = 1, lty =2); box() 
  abline(h = -10)
  if( b == 1 || b == 3) {axis(side = 2, las = 1) }
  print.letter(alpha.label[b], xy = c(0.93, 0.95), cex = alpha.cex)
  if(b < 3) { mtext(side = 3, name.label[b], cex = alpha.cex) }
  if( b == 2) { mtext(side = 4, "Time-invariant", line = 1, cex = lab.cex)}
  if( b == 4) { mtext(side = 4, "Time-varying",  line = 1, cex = lab.cex)}
  if (b >= 3) { axis(side = 1, at = seq(1,length(to.plot),4), 
        labels = seq(to.plot[1]- pre.yrs + 1, to.plot[length(to.plot)] - pre.yrs + 1, 4*ass.freq) ) }
}
mtext(side =2, "Width at maximum selectivity",  outer = T, line = 3, cex = lab.cex)
mtext(side =1, "Assessment Year", outer = T, line = 3, cex = lab.cex)

dev.off()

#=============================================================================================================

# Supplementary Plots
drive = "C:"
run.name = "Final_w_survey"
setwd(paste0(drive,"/PhD/Chapter3/WriteUp/Fishery_Bulletin_Submission/revised_submission/JournalPlots"))
load(paste0(drive,"/PhD/Chapter3/",run.name,"/output/rockfish_meds_all"))
load(paste0(drive,"/PhD/Chapter3/",run.name,"/output/rockfish_est_all"))
load(paste0(drive,"/PhD/Chapter3/",run.name,"/output/rockfish_om_all"))

rock.out  <- med.out <- est.out <- om.out <- list()
rock.out[[1]] <- med.out[[1]] <- meds.all
rock.out[[2]] <- est.out[[1]] <- est.all
rock.out[[3]] <- om.out[[1]]  <- om.all

#Alternative master figure for time-invariant=====================================================================

png(filename = "fig_7_survey_sensitivity_time_invariant.png", width = 6.7, height = 7.7, units = 'in', res = 256)

par(mfrow= c(3,3), mar = c(0.2,0.2,0.2,0.2), oma = c(5,5,2,5), cex.axis = 1.1, cex.lab = 0.8)
letter.cex = 1; axis.cex = 1.2; lab.cex = 0.75
alpha.cex = 1 ; lab.cex = 0.9
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

  box95(t(re.ssb), list = F, ylim = c(-0.6, max), col = rep('grey',length(to.plot)), axes = F, add = FALSE, boxwex = rep(0.75, length(to.plot)))
  box(); abline(h = 0) ; abline(h = -0.98) 

  if(a == 1) { axis(side = 2, at = seq(-0.5, max, 0.5), las = 1) ; mtext(side = 2, outer = F, "RE SB", line = 3, cex = lab.cex)} 
  #if(a == 3) { axis(side = 4, at = c(min, min + 0.25, -1), label = c("0%", "50%", "100%"), las = 1) } 
  mtext(side = 3, outer = F, name.label.alt[a], cex = 0.85) 
 
  print.letter(alpha.label[a], xy = c(0.95, 0.95), cex = 1.1)  
}

# RE Depletion

for (a in 1:3){
  to.plot = ass.yrs[1:ass.plot.yrs]
  re.depl = matrix(0, length(to.plot), N)
  for (b in 1:length(to.plot)){
    re.depl[b,] = med.out[[1]]$re.depl[a,b,to.plot[b],]
  }
  if (a == 1){ re.depl[1,40] = 0.961795}
 
  box95(t(re.depl), list = F, ylim = c(-0.6, max), col = rep('grey',length(to.plot)), axes = F, add = FALSE, boxwex = rep(0.75, length(to.plot)))
  box(); abline(h = 0) ; abline(h= -0.98)
  if(a == 1) { axis(side = 2, at = seq(-0.5, max, 0.5), las = 1) ; mtext(side = 2, outer = F, "RE Relative SB", line = 3, cex = lab.cex)} 
  #if(a == 3) { axis(side = 4, at = c(min, min + 0.25, -1), label = c("0%", "50%", "100%"), las = 1) } 

  print.letter(c("D", "E", "F")[a], xy = c(0.95, 0.95), cex = 1.1)  
}

# Steepness
min = -0.5; max = 1.2
for(b in 1:3){
  scale.per.of = (1- med.out[[1]]$n.overfished[a,] / 100)*0.5 + min 
  om.scale.per.of = (1- med.out[[1]]$om.n.overfished[a,] / 100)*0.5 + min 
  plot(1:length(to.plot), om.scale.per.of, type = 'l', lwd = 1.5, ylim = c(min, max), axes= F, yaxs = "i")
  lines(1:length(to.plot), scale.per.of, col = 1, lty = 2)
  if(b == 1) { legend("bottomright", legend = c("OM", "EM"), lty = c(1,2), bty = 'n', cex = 1) }
  if(b == 2) { print.letter("% Rebuilt", xy = c(0.50, 0.05), cex = 1) }
  if(b == 3) { axis(side = 4, at = c(min, min + 2.5, -10), label = c("0%", "50%", "100%"), las = 1) } 

  box95(x=t(est.out[[1]]$h.est[b,,]), list = F, ylab = "Steepness", ylim = c(0.19, 1.05), col = rep('grey',ass.num), axes = F,
     boxwex = rep(0.70, length(to.plot)), add = TRUE)
  box(); abline(h = 0); abline(h = steep, lty = 2, col = 1)
  print.letter(c("G", "H", "I")[b], xy = c(0.95, 0.95))
  
  if(b == 1) { axis(side = 2, at = c(0.20, 0.40, 0.60, 0.80, 1.0), las = 1) }
  if(b == 1) { mtext(side = 2, "Steepness", outer = F, line = 3, cex = lab.cex) }
}


mtext(side =1, "Assessment Year", outer = T, line = 3, cex = lab.cex)


dev.off()


#Figure 3 ========================================================================================================
png(filename = "supp2_ressb.png", width = 6.7, height = 6, units = 'in', res = 256)

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

  om.per.of = med.out[[1]]$om.n.overfished[a,text.yr]
  om.scale.per.of = (1-med.out[[1]]$om.n.overfished[a,] / 100)*0.5 + min 

  plot(1:length(to.plot), om.scale.per.of, type = 'l', lwd = 1.5, ylim = c(min, max), axes= F, yaxs = "i")
  lines(1:length(to.plot), scale.per.of, col = 1, lty = 2)
  if(a ==1 || a == 4) { legend("bottomright", legend = c("OM", "EM"), lty = c(1,2), bty = 'n', cex = 1.1) }

  #per.of = med.out[[1]]$n.overfished[a,text.yr]
  #scale.per.of = (1-med.out[[1]]$n.overfished[a,] / 100)*0.5 + min 
  #plot(1:length(to.plot), scale.per.of, type = 'l', lwd = 1.5, ylim = c(min, max), axes= F, yaxs = "i")
  #points(1:length(to.plot), scale.per.of, pch = 21, cex = 0.85)
  box95(t(re.ssb), list = F, col = rep('grey',length(to.plot)), axes = F, add = TRUE, boxwex = rep(0.75, length(to.plot)))
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
  if (a == 3 ){ mtext(side = 4, outer = F, "Time-invariant", line = 1) }
  #text(par("usr")[2]*1.03, mean(par("usr")[3:4])+0.1, "time-invarient", srt = -90, xpd = TRUE, pos = 4, cex = 1.5)}
  if (a == 6 ){ mtext(side = 4, outer = F, "Time-varying", line = 1) }
  #text(par("usr")[2]*1.03, mean(par("usr")[3:4])+0.1, "time-varying", srt = -90, xpd = TRUE, pos = 4, cex = 1.5)}
  print.letter(alpha.label[a], xy = c(0.95, 0.95), cex = 1.1)  
}

dev.off()




#================================================================================================
png(filename = "supp3_redepl.png", width = 6.7, height = 6, units = 'in', res = 256)

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
  om.scale.per.of = (1- med.out[[1]]$om.n.overfished[a,] / 100)*0.5 + min 
  plot(1:length(to.plot), om.scale.per.of, type = 'l', lwd = 1.5, ylim = c(min, max), axes= F, yaxs = "i")
  lines(1:length(to.plot), scale.per.of, col = 1, lty = 2)
  if(a ==1 || a == 4) { legend("bottomright", legend = c("OM", "EM"), lty = c(1,2), bty = 'n', cex = 1.1) }
 
  #scale.per.of = (1- med.out[[1]]$n.overfished[a,] / 100)*0.5 + min 
  #plot(1:length(to.plot), scale.per.of, type = 'l', lwd = 1.5, ylim = c(min, max), axes= F, yaxs = 'i')
  #points(1:length(to.plot), scale.per.of, pch = 21, cex = 0.85)
  box95(t(re.depl), list = F, col = rep('grey',length(to.plot)), axes = F, add = TRUE, boxwex = rep(0.75, length(to.plot)))
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
  if (a == 3 ){ mtext(side = 4, outer = F, "Time-invariant", line = 1) }
  if (a == 6 ){ mtext(side = 4, outer = F, "Time-varying", line = 1) }
  print.letter(alpha.label[a], xy = c(0.95, 0.95), cex = 1.1)  
}

dev.off()

#=========================================================================================================
# RMSE over time for spawning biomass
#=========================================================================================================
png(filename = "supp4_rmse.png", width = 6.7, height = 3.5, units = 'in', res = 256)

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
                  mtext(side = 3, outer = F, "Time-invariant")
                  axis(side = 1, at = seq(index[1], (max(index)-5), 25), labels = seq(50, 125, 25))
                  print.letter(alpha.label[a], xy = c(0.05, 0.95), cex = 1)  }
    if( a == 4) { print.letter(alpha.label[a-2], xy = c(0.05, 0.95), cex = 1) 
                  mtext(side = 3, outer = F, "Time-varying")
                  axis(side = 1, at = seq(index[1], max(index), 25), labels = seq(50, 150, 25))
                  legend("topright", legend = name.label, pch = pch.vec, bty= 'n')  }

  }
  lines(index, med.out[[1]]$rmse.ssb.ass[a,], lwd = 1, col = 1)
  points(index, med.out[[1]]$rmse.ssb.ass[a,], pch = pch.vec[a], col = 1)
}

dev.off()


#=========================================================================================================
# Alternative Super Figure
# Time-invariant estimates
#=========================================================================================================

png(filename = "fig_time_invariant.png", width = 6.7, height = 9, units = 'in', res = 256)

par(mfrow= c(5,3), mar = c(0.2,0.2,0.2,0.2), oma = c(5,5,2,5), cex.axis = 1.1, cex.lab = 0.8)
letter.cex = 1; axis.cex = 1.2; lab.cex = 0.75
ass.plot.yrs = ifelse(ass.freq == 8, 13, 26)
ass.plot.yrs = ifelse(ass.freq == 6, 18, ass.plot.yrs)
min = -1.5; max = 1.55
text.yr = c(2, 7, 11,15)
 
# RE SSB
for (a in 1:3){

  to.plot = ass.yrs[1:ass.plot.yrs]
  re.ssb = matrix(0, length(to.plot), N)
  for (b in 1:length(to.plot)){
    re.ssb[b,] = med.out[[1]]$re.ssb[a,b,to.plot[b],]
  }

  box95(t(re.ssb), list = F, ylim = c(-0.6, max), col = rep('grey',length(to.plot)), axes = F, add = FALSE, boxwex = rep(0.75, length(to.plot)))
  box(); abline(h = 0) ; abline(h = -0.98) 

  if(a == 1) { axis(side = 2, at = seq(-0.5, max, 0.5), las = 1) ; mtext(side = 2, outer = F, "RE SB", line = 3, cex = lab.cex)} 
  #if(a == 3) { axis(side = 4, at = c(min, min + 0.25, -1), label = c("0%", "50%", "100%"), las = 1) } 
  mtext(side = 3, outer = F, name.label[a], cex = lab.cex) 
 
  print.letter(alpha.label[a], xy = c(0.95, 0.95), cex = 1.1)  
}

# RE Depletion

for (a in 1:3){
  to.plot = ass.yrs[1:ass.plot.yrs]
  re.depl = matrix(0, length(to.plot), N)
  for (b in 1:length(to.plot)){
    re.depl[b,] = med.out[[1]]$re.depl[a,b,to.plot[b],]
  }
  if (a == 1){ re.depl[1,40] = 0.961795}
 
  box95(t(re.depl), list = F, ylim = c(-0.6, max), col = rep('grey',length(to.plot)), axes = F, add = FALSE, boxwex = rep(0.75, length(to.plot)))
  box(); abline(h = 0) ; abline(h= -0.98)
  if(a == 1) { axis(side = 2, at = seq(-0.5, max, 0.5), las = 1) ; mtext(side = 2, outer = F, "RE Relative SB", line = 3, cex = lab.cex)} 
  #if(a == 3) { axis(side = 4, at = c(min, min + 0.25, -1), label = c("0%", "50%", "100%"), las = 1) } 

  print.letter(c("D", "E", "F")[a], xy = c(0.95, 0.95), cex = 1.1)  
}

# Steepness

for(b in 1:3){

  box95(x=t(est.out[[1]]$h.est[b,,]), list = F, ylab = "Steepness", ylim = c(0.19, 1.05), col = rep('grey',ass.num), axes = F,
     boxwex = rep(0.70, length(to.plot)), add = FALSE)
  box(); abline(h= 0); abline(h = steep, lty = 2, col = 1)
  print.letter(c("G", "H", "I")[b], xy = c(0.95, 0.95))
  
  if(b == 1) { axis(side = 2, at = c(0.20, 0.40, 0.60, 0.80, 1.0), las = 1) }
  if(b == 1) { mtext(side = 2, "Steepness", outer = F, line = 3, cex = lab.cex) }
}

# Fishery selectivity
for(b in 1:3){

  box95(x=t(est.out[[1]]$f.selex.est[b,1,,]), ylim = c(42, 49), axes = F,col = 'grey',  add = FALSE)
  abline(h = 45, col = 1, lty =2); box() 
  if(b == 1) {axis (side = 2, at = seq(43, 47, 2), las = 1)
   mtext(side =2, "Size at max. select. (cm)",  outer = F, line = 3, cex = lab.cex) }
  print.letter(c("J", "K", "L")[b], xy = c(0.93, 0.95), cex = alpha.cex)
  
}

# Fishery Dome
min = -15; max = 7

for (a in 1:3){

  scale.per.of = (1- med.out[[1]]$n.overfished[a,] / 100)*5 + min 
  om.scale.per.of = (1- med.out[[1]]$om.n.overfished[a,] / 100)*5 + min 
  plot(1:length(to.plot), om.scale.per.of, type = 'l', lwd = 1.5, ylim = c(min, max), axes= F, yaxs = "i")
  lines(1:length(to.plot), scale.per.of, col = 1, lty = 2)
  if(a == 1) { legend("bottomright", legend = c("OM", "EM"), lty = c(1,2), bty = 'n', cex = 0.9) }
  if(a == 2) { print.letter("% Rebuilt", xy = c(0.50, 0.05), cex = 0.9) }
  if(a == 3) { axis(side = 4, at = c(min, min + 2.5, -10), label = c("0%", "50%", "100%"), las = 1) } 

  est.out[[1]]$f.selex.adj.est[a,1,1,] = max + 5
  if (a == 3) {est.out[[1]]$f.selex.adj.est[a,1,,] = max + 20 }
  box95(x=t(est.out[[1]]$f.selex.adj.est[a,1,,]), xlim = c(1,18), axes = F, add = TRUE) 
  abline(h = -2.5, col = 1, lty =2); box() 
  abline(h = -10)
  if(a == 1) {axis(side = 2, las = 1, at = c(-5, 0, 5))
  	mtext(side =2, "Width at max. select.",  outer = F, line = 3, cex = lab.cex) }
  print.letter(c("M", "N", "O")[a], xy = c(0.93, 0.95), cex = alpha.cex)

  axis(side = 1, at = seq(1,length(to.plot),4), labels = seq(to.plot[1]- pre.yrs + 1, to.plot[length(to.plot)] - pre.yrs + 1, 4*ass.freq) ) 
}

mtext(side =1, "Assessment Year", outer = T, line = 3, cex = lab.cex)


dev.off()


#=========================================================================================================
# Alternative Super Figure
# Time-varying estimates
#=========================================================================================================

png(filename = "fig_time_varying.png", width = 6.7, height = 9, units = 'in', res = 256)

par(mfrow= c(5,3), mar = c(0.2,0.2,0.2,0.2), oma = c(5,5,2,5), cex.axis = 1.1, cex.lab = 0.8)
letter.cex = 1; axis.cex = 1.2; lab.cex = 0.75
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

  box95(t(re.ssb), list = F, ymin = c(-0.6, max), col = rep('grey',length(to.plot)), axes = F, add = FALSE, boxwex = rep(0.75, length(to.plot)))
  box(); abline(h = 0) ; abline(h = -0.98) 

  if(a == 4) { axis(side = 2, at = seq(-0.5, max, 0.5), las = 1) ; mtext(side = 2, outer = F, "RE SB", line = 3, cex = lab.cex) } 
  mtext(side = 3, outer = F, name.label[a-3], cex = lab.cex)  
  print.letter(alpha.label[a-3], xy = c(0.95, 0.95), cex = 1.1)  
}

# RE Depletion
for (a in 4:6){
  to.plot = ass.yrs[1:ass.plot.yrs]
  re.depl = matrix(0, length(to.plot), N)
  for (b in 1:length(to.plot)){
    re.depl[b,] = med.out[[1]]$re.depl[a,b,to.plot[b],]
  }
 
  box95(t(re.depl), list = F, ymin = c(-0.6, max), col = rep('grey',length(to.plot)), axes = F, add = FALSE, boxwex = rep(0.75, length(to.plot)))
  box(); abline(h = 0) ; abline(h= -0.98)
  if(a == 4) { axis(side = 2, at = seq(-0.5, max, 0.5), las = 1) ; mtext(side = 2, outer = F, "RE Relative SB", line = 3, cex = lab.cex)} 

  print.letter(c("D", "E", "F")[a-3], xy = c(0.95, 0.95), cex = 1.1)  
}

# Steepness
for(b in 4:6){

  box95(x=t(est.out[[1]]$h.est[b,,]), list = F, ylab = "Steepness", ylim = c(0.19, 1.05), col = rep('grey',ass.num), axes = F,
     boxwex = rep(0.70, length(to.plot)), add = FALSE)
  box(); abline(h= 0); abline(h = steep, lty = 2, col = 1)
  print.letter(c("G", "H", "I")[b-3], xy = c(0.95, 0.95))
  
  if(b == 4) { axis(side = 2, at = c(0.20, 0.40, 0.60, 0.80, 1.0), las = 1) }
  if(b == 4) { mtext(side = 2, "Steepness", outer = F, line = 3, cex = lab.cex) }
}

# Fishery selectivity
for(b in 4:6){

  box95(x=t(est.out[[1]]$f.selex.est[b,1,,]), ylim = c(42, 49), axes = F,col = 'grey',  add = FALSE)
  abline(h = 45, col = 1, lty =2); box() 
  if(b == 4) {axis (side = 2, at = seq(43, 47, 2), las = 1)
   mtext(side =2, "Size at max. select. (cm)",  outer = F, line = 3, cex = lab.cex) }
  print.letter(c("J", "K", "L")[b-3], xy = c(0.93, 0.95), cex = alpha.cex)
  
}

# Fishery Dome 
min = -15; max = 7

for (a in 4:6){

  scale.per.of = (1- med.out[[1]]$n.overfished[a,] / 100)*5 + min 
  om.scale.per.of = (1- med.out[[1]]$om.n.overfished[a,] / 100)*5 + min 
  plot(1:length(to.plot), om.scale.per.of, type = 'l', lwd = 1.5, ylim = c(min, max), axes= F, yaxs = "i")
  lines(1:length(to.plot), scale.per.of, col = 1, lty = 2)
  if(a == 4) { legend("bottomright", legend = c("OM", "EM"), lty = c(1,2), bty = 'n', cex = 0.9) }
  if(a == 5) { print.letter("% Rebuilt", xy = c(0.50, 0.05), cex = 0.9) }
  if(a == 6) { axis(side = 4, at = c(min, min + 2.5, -10), label = c("0%", "50%", "100%"), las = 1) } 
  
  est.out[[1]]$f.selex.adj.est[a,1,1,] = max + 5
  if (a == 6) {est.out[[1]]$f.selex.adj.est[a,1,,] = max + 20 }
  box95(x=t(est.out[[1]]$f.selex.adj.est[a,1,,]), xlim = c(1,18), axes = F, add = TRUE) 
  abline(h = -2.5, col = 1, lty =2); box() 
  abline(h = -10)
  if(a == 4) {axis(side = 2, las = 1, at = c(-5, 0, 5))
  	mtext(side =2, "Width at max. select.",  outer = F, line = 3, cex = lab.cex) }

  print.letter(c("M", "N", "O")[a-3], xy = c(0.93, 0.95), cex = alpha.cex)
  axis(side = 1, at = seq(1,length(to.plot),4), labels = seq(to.plot[1]- pre.yrs + 1, to.plot[length(to.plot)] - pre.yrs + 1, 4*ass.freq) ) 
}

mtext(side =1, "Assessment Year", outer = T, line = 3, cex = lab.cex)


dev.off()

