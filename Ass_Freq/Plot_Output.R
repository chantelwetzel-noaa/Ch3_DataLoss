############################################
#   Chapter 3: Loss of Data & Rebuilding   #
#                                          #
#    Create Plots from R object created    #
#          by Simulation_Eval code         #
#         Created July 31, 2014 by         #
#             Chantel Wetzel               #
############################################


#Load in the R objects from the Simulation Eval Code ========================================
drive = "D:"
run.name = "April16_PreCPUE"#"Fall2015"

load(paste0(drive,"/PhD/Chapter3/",run.name,"/output/rockfish_meds_all"))
load(paste0(drive,"/PhD/Chapter3/",run.name,"/output/rockfish_est_all"))
load(paste0(drive,"/PhD/Chapter3/",run.name,"/output/rockfish_om_all"))


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
name.label = c('Full Data', 'Data Reduction', 'Historical Data')

library(RColorBrewer)

#Dimensions used for plotting======================================================================================
ds =  dim(rock.out[[2]]$ssb.est)[1] #Determines the number of data scenarios to plot
hist.yrs = 50
proj.yrs = 100
ass.yr1  = 120
pre.yrs  = 71
final.yr = proj.yrs + hist.yrs + pre.yrs - 1  #rock.yrs + 50 + pre.yrs.rock - 1

ass.yrs = seq(ass.yr1, ass.yr1 + proj.yrs,4)
N       = dim(om.out[[1]]$ssb)[3] 

# Relative error about spawning biomas==================================================================
setwd("C:/PhD/Chapter3/WriteUp/Plots")
setwd("D:/PhD/Chapter3/April16_PreCPUE/Plots")
png(filename = "RE_SSB_full.png", width = 6.7, height = 5, units = 'in', res = 256)

par(mfrow= c(2,3), mar = c(0.2,0.2,0.2,0.2), oma = c(4,4,2,4), cex.axis = 1.1, cex.lab = 1.1)
letter.cex = 1; axis.cex = 1.2; label.cex = 0.8
ass.plot.yrs = 26
min = -0.75; max = 1.25

for (a in 1:ds){
  to.plot = ass.yrs[1:ass.plot.yrs]
  re.ssb = matrix(0, length(to.plot), 100)
  for (b in 1:length(to.plot)){
    re.ssb[b,] = med.out[[1]]$re.ssb[a,b,to.plot[b],]
  }

  boxplot(t(re.ssb), ylim = c(min, max), col = rep('grey',length(to.plot)), axes= F)
  box(); abline(h = 0)
  if( a ==1 || a==4) { axis(side = 2)
      mtext(side = 2, outer = T, "RE spawning biomass", line = 2.5)} 
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
# Relative error about relative biomass ================================================================
#=======================================================================================================

png(filename = "RE_depl_full.png", width = 6.7, height = 5, units = 'in', res = 256)

par(mfrow= c(2,3), mar = c(0.2,0.2,0.2,0.2), oma = c(4,4,2,4), cex.axis = 1.1, cex.lab = 1.1)
letter.cex = 1; axis.cex = 1.2; label.cex = 0.8
ass.plot.yrs = 26
min = -0.75; max = 1.25

for (a in 1:ds){
  to.plot = ass.yrs[1:ass.plot.yrs]
  re.depl = matrix(0, length(to.plot), 100)
  for (b in 1:length(to.plot)){
    re.depl[b,] = med.out[[1]]$re.depl[a,b,to.plot[b],]
  }

  boxplot(t(re.depl), ylim = c(min, max), col = rep('grey',length(to.plot)), axes= F)
  box(); abline(h = 0)
  if( a ==1 || a==4) { axis(side = 2)
      mtext(side = 2, outer = T, "RE relative biomass", line = 2.5)} 
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

#=========================================================================================================
# RMSE over time for spawning biomass
#=========================================================================================================
png(filename = "RMSE_SSB_full.png", width = 6.7, height = 3, units = 'in', res = 256)

par(mfrow= c(1,2), mar = c(0.2,0.2,0.2,0.2), oma = c(4,4,2,4), cex.axis = 1.1, cex.lab = 1.1)
letter.cex = 1; axis.cex = 1; label.cex = 1
ass.plot.yrs = 26
min = 0; max = 100
pch.vec = rep(16:18,2)

for (a in 1:ds){
  index = ass.yrs - pre.yrs
  if (a == 1 || a ==4){
    plot(index, med.out[[1]]$rmse.depl[a,], type = 'l', lwd = 1, ylim = c(min, max), axes = F, xaxs="i")
    points(index, med.out[[1]]$rmse.depl[a,], pch = pch.vec[a])
    box()
    axis(side = 1)#, at = 1:length(to.plot), labels = to.plot - pre.yrs + 1)
    if (a == 1) { mtext(side = 1, "Assessment Year", outer = T, cex = label.cex, line = 2)
                  axis(side = 2, cex = axis.cex)
                  mtext(side = 2, "RMSE spawning biomass", cex = label.cex, outer = T, line = 2)
                  print.letter(alpha.label[a], xy = c(0.10, 0.95), cex = 1.1)  }
    if( a == 4) { print.letter(alpha.label[a-2], xy = c(0.10, 0.95), cex = 1.1)  }
  }
  lines(index, med.out[[1]]$rmse.depl[a,], lwd = 1)
  points(index, med.out[[1]]$rmse.depl[a,], pch = pch.vec[a])
}

dev.off()






color <- rgb(0, 0, 0, 0.30) 
ass.index.flat = rev(c(2, 1, 4, 6))#rev(c(1,  5, 10, 14))#rev(c(1, 7, 14))
ass.index.rock = rev(c(1, 4, 10, 16, 26))#rev(c(1, 10, 20, 26))#rev(c(1, 13, 26))
text.yr.flat = sort((pre.yrs.flat + ass.yr)[ass.index.flat]) - 2
text.yr.rock = sort((pre.yrs.rock + ass.yr)[ass.index.rock]) - 5
a = 256
plot.color = c(rgb(0, 153/a, 0), rgb(255/a, 128/a, 0), "Blue", "violet")
#plot.color = c('blue', "darkgreen", 'red')
trans = 0.20
trans.color = c(rgb(0, 153/a, 0, trans), rgb(255/a, 128/a, 0, trans+ 0.05), rgb(0,0,1, trans), rgb(0,0.5, 0.5, trans))

lty.type = c(4, 3, 2, 5, 1)
ymax = ceiling( max(rock.out[[1]]$med.ssb[,start.plot.rock:final.yr.rock,]))

for(b in 1:ds){
  
  #Plot rockfish
  plot(start.plot.rock:final.yr.rock,  rock.out[[1]]$med.ssb[b,start.plot.rock:final.yr.rock,2], col = 1, type = 'l', lwd =2, 
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
  lines(start.plot.rock:final.yr.rock,  rock.out[[1]]$med.ssb[b,start.plot.rock:final.yr.rock,1], col = 'darkgrey', lty = 4, lwd =1)
  lines(start.plot.rock:final.yr.rock,  rock.out[[1]]$med.ssb[b,start.plot.rock:final.yr.rock,3], col = 'darkgrey', lty = 4, lwd =1)
  print.letter(alpha.label2[b], xy = c(0.15, 0.95), cex = 1.1)
  #text(labels = per.of, x = text.yr.rock, y = rep(10, length(text.yr.rock)), cex = 1.2 ) 

  for (i in 1:length(ass.index.rock)){
    temp = ass.index.rock[i]*4 - 5 + hist.yrs + pre.yrs.rock
    y = (start.plot.rock):(temp)
    ind = (start.plot.rock):temp
    xx = c(y, rev(y))
    yy = c(rock.out[[1]]$med.ssb.est[b,ind,ass.index.rock[i],1], rev(rock.out[[1]]$med.ssb.est[b,ind,ass.index.rock[i],3]))
    polygon(xx, yy, col = trans.color[i], lty = 0)
  }
  lines(start.plot.rock:final.yr.rock,  rock.out[[1]]$med.ssb[b,start.plot.rock:final.yr.rock,2], col = 'grey70', lty = 1, lwd =3)
  for (i in 1:length(ass.index.rock))
  {
    temp = ass.index.rock[i]*4 - 5 + hist.yrs + pre.yrs.rock
    y = (start.plot.rock):(temp)
    ind = (start.plot.rock):temp
    lines(y, rock.out[[1]]$med.ssb.est[b,ind,ass.index.rock[i],2], col = plot.color[i], lty = lty.type[i], lwd =2)
  }
}


#Working====================================================================================================================================
#Spawning Biomass
par(mfrow= c(3,2), mar = c(0.1,1.2,0.1,1.2), oma = c(4,4,4,4), cex.axis = 1.1, cex.lab = 1.1)

final.yr.rock = 220#195#246 #196

start.plot.rock = 105#155 #105#112
color <- rgb(0, 0, 0, 0.30) 

ass.index.rock = rev(c(1, 8, 13, 20, 26))#rev(c(1, 10, 20, 26))#rev(c(1, 13, 26))

text.yr.rock = sort((pre.yrs.rock + ass.yr)[ass.index.rock]) - 5
a = 256
plot.color = c(rgb(0, 153/a, 0), rgb(255/a, 128/a, 0), "Blue", "Brown")
#plot.color = c('blue', "darkgreen", 'red')
trans = 0.20
trans.color = c(rgb(0, 153/a, 0, trans), rgb(255/a, 128/a, 0, trans+ 0.05),rgb(0,0,1, trans), rgb(153/a,0, 0, trans))
line.color = "white"
lty.type = c(4, 3, 2, 5, 1)
ymax.rock = ceiling( max(rock.out[[1]]$med.ssb[,start.plot.rock:final.yr.rock,])) + 1000
for(b in 1:ds){
  #Plot rockfish
  plot(start.plot.rock:final.yr.rock,  rock.out[[1]]$med.ssb[b,start.plot.rock:final.yr.rock,2], col = 1, type = 'l', lwd =2, 
    ylim = c(0,ymax.rock), ylab = "SB", xlab = "Year",  axes = F, xaxs="i")
  if(b ==1) { mtext(side = 3, "Rockfish", outer = F, line = 1)
              mtext(side = 2, "SSB", outer = T, line = 1.5) }
  box() ; axis(side =2)
  if(b > 2) { 
    #axis(side = 1, at =seq(120, 220,10), labels = seq(50, 150, 10))
    axis(side = 1, at =seq(120, 220, 10), labels = seq(50, 150, 10))
    mtext("Year", side = 1, outer = T, line = 2.5)
  }
  lines(start.plot.rock:final.yr.rock,  rock.out[[1]]$med.ssb[b,start.plot.rock:final.yr.rock,1], col = 'darkgrey', lty = 4, lwd =1)
  lines(start.plot.rock:final.yr.rock,  rock.out[[1]]$med.ssb[b,start.plot.rock:final.yr.rock,3], col = 'darkgrey', lty = 4, lwd =1)
  #print.letter(alpha.label2[b], xy = c(0.17, 0.95))
  #per.of = round(rock.out[[1]]$n.overfished[b,sort(ass.index.rock)]/N.rock,2)
  #text( labels = per.of, x = text.yr.rock, y = rep(10, length(text.yr.rock)), cex = 1.2 )
  for (i in 1:length(ass.index.rock)){
    temp = ass.index.rock[i]*4 - 5 + hist.yrs + pre.yrs.rock
    y = (start.plot.rock):(temp)
    ind = (start.plot.rock):temp
    xx = c(y, rev(y))
    yy = c(rock.out[[1]]$med.ssb.est[b,ind,ass.index.rock[i],1], rev(rock.out[[1]]$med.ssb.est[b,ind,ass.index.rock[i],3]))
    polygon(xx, yy, col = trans.color[i], lty = 0)
  }
    lines(start.plot.rock:final.yr.rock,  rock.out[[1]]$med.ssb[b,start.plot.rock:final.yr.rock,2], col = line.color, lty = 1, lwd =3)
  for (i in 1:length(ass.index.rock))
  {
    temp = ass.index.rock[i]*4 - 5 + hist.yrs + pre.yrs.rock
    y = (start.plot.rock):temp
    ind = (start.plot.rock):temp
    lines(y, rock.out[[1]]$med.ssb.est[b,ind,ass.index.rock[i],2], col = plot.color[i], lty = lty.type[i], lwd =2)
  }
}



#Depletion================================================================================================================================
par(mfrow= c(3,2), mar = c(0.1,0.1,0.1,0.1), oma = c(4,4,2,4), cex.axis = 1.1, cex.lab = 1.1)
ymax = 0.75
ass.index.rock = rev(c(1, 8, 13, 20, 26))
for(b in 1:ds){
   #Plot rockfish
  plot(start.plot.rock:final.yr.rock,  rock.out[[1]]$med.depl[b,start.plot.rock:final.yr.rock,2], col = 1, type = 'l', lwd =2, 
    ylim = c(0,ymax), ylab = "Depletion", xlab = "Year",  axes = F, xaxs="i")
  if(b == 1 || b == 3){ axis(side = 2)}
  if(b == 1 ) {mtext("Depeltion", side = 2, outer = T, line = 2)}
  box() ; if(b == ds) { 
      axis(side = 1, at =seq(120, 220,10), labels = seq(50, 150, 10))
      #axis(side = 1, at =seq(170, 270,10), labels = seq(100, 200, 10))
      mtext("Year", side = 1, outer = T, line = 2.5)}
  if(b ==1) { mtext(side = 3, "Rockfish", outer = F, line = 1) }
  lines(start.plot.rock:final.yr.rock,  rock.out[[1]]$med.depl[b,start.plot.rock:final.yr.rock,1], col ='darkgrey', lty = 4, lwd = 1)
  lines(start.plot.rock:final.yr.rock,  rock.out[[1]]$med.depl[b,start.plot.rock:final.yr.rock,3], col ='darkgrey', lty = 4, lwd = 1)
  abline(h = 0.40, lty = 3, col = 'red')
  #print.letter(alpha.label2[b], xy = c(0.12, 0.95))
  #per.of = round(rock.out[[1]]$n.overfished[b,sort(ass.index.rock)]/N.rock,2)
  #text( labels = per.of, x = text.yr.rock, y = rep(0, length(text.yr.rock)), cex = 1.2 )
  print.letter(alpha.label2[b], xy = c(0.15, 0.95), cex = 1.1)

  for (i in 1:length(ass.index.rock))
  {
    temp = ass.index.rock[i]*4 - 5 + hist.yrs + pre.yrs.rock
    y = (start.plot.rock):(temp)
    ind = (start.plot.rock):temp
    xx = c(y, rev(y))
    yy = c(rock.out[[1]]$med.depl.est[b,ind,ass.index.rock[i],1], rev(rock.out[[1]]$med.depl.est[b,ind,ass.index.rock[i],3]))
    polygon(xx, yy, col = trans.color[i], lty = 0)
  }
  lines(start.plot.rock:final.yr.rock,  rock.out[[1]]$med.depl[b,start.plot.rock:final.yr.rock,2], col = line.color, lty = 1, lwd =3)
  for (i in 1:length(ass.index.rock))
  {
    temp = ass.index.rock[i]*4 - 5 + hist.yrs + pre.yrs.rock
    y = (start.plot.rock):(temp)
    ind = (start.plot.rock):temp
    lines(y, rock.out[[1]]$med.depl.est[b,ind,ass.index.rock[i],2], col = plot.color[i], lty = lty.type[i], lwd =2)
  }
}

#Plot the estimated recovery lines======================================================================
  par(mfrow = c(1,2))
  transform = 100 - rock.out[[1]]$n.overfished
  om.trans = 100 - rock.out[[1]]$om.n.overfished

  plot(1:26, transform[1,], type = 'l', col = 2, lwd = 2)
  lines(1:26, om.trans[1,], lty = 2, col = 2, lwd = 2)
  lines(1:26, transform[2,],lty = 1, col = 3, lwd = 2)
  lines(1:26, om.trans[2,], lty = 2, col = 3, lwd = 2)
  plot(1:26, transform[3,], type = 'l', col = 4, lwd = 2)
  lines(1:26, om.trans[3,], lty = 2, col = 4, lwd = 2)
  lines(1:26, transform[4,],lty = 1, col = 5, lwd = 2)
  lines(1:26, om.trans[4,], lty = 2, col = 5, lwd = 2)
  legend("bottomright", legend = c("data-no tv (EM)", "reduced - no tv (EM)", "data-tv (EM)", "reduced-tv (EM)"), col = c(2,3,4,5), lty = c(1,1,1,1))

  boxplot(t(re.time.over)) ; abline(h = 0, lty = 1)


#Plot the relative error ===============================================================================
par(mfrow=c(3,2), mar = c(2,2,3,3))

#Relative Error about depletion
max = min = 0.5
for (a in 1:ds){
  ass.num = 26
  re.depl = matrix(0, ass.num, 100)
  for (b in 1:ass.num){
    ind = b*4 - 5+ hist.yrs + pre.yrs.rock
    re.depl[b,] = rock.out[[1]]$re.depl[a,b,ind,]
  }
  boxplot(t(re.depl), ylim = c(-min, max), col = rep('grey',ass.num), axes= F)
  box(); abline(h = 0)
  axis(side = 2); axis(side = 1, at = seq(1,26,3), labels = seq(hist.yrs, hist.yrs + 100, 12))
  #print.letter(alpha.label2[a], xy = c(0.14, 0.05))
  if(a == 1) { 
    mtext(side = 3, outer = F, "Rockfish", line = 1)
    mtext(side = 2, outer = T, "Depletion", line = 2)
  }
  print.letter(alpha.label2[a], xy = c(0.15, 0.05), cex = 1.1) 
}

#Relative Error about final spawning biomass
for (a in 1:ds){
  ass.num = 26
  re.ssb = matrix(0, ass.num, 100)
  for (b in 1:ass.num){
    ind = b*4 - 5+ hist.yrs + pre.yrs.rock
    re.ssb[b,] = rock.out[[1]]$re.ssb[a,b,ind,]
  }
  boxplot(t(re.ssb), ylim = c(-min, max), col = rep('grey',ass.num), axes= F)
  box(); abline(h = 0)
  axis(side = 2); axis(side = 1, at = seq(1,26,3), labels = seq(hist.yrs, hist.yrs + 100, 12))
  #print.letter(alpha.label2[a], xy = c(0.16, 0.05))
  if(a == 1) { 
    mtext(side = 3, outer = T, "Rockfish", line = 1)
    mtext(side = 2, outer = T, "Spawning Biomass", line = 2)
  }
  print.letter(alpha.label2[a], xy = c(0.15, 0.05), cex = 1.1)  
}

#Relative Error about virgin spawning biomass
for (a in 1:ds){
  ass.num = 26
  re.ssb0 = matrix(0, ass.num, 100)
  for (b in 1:ass.num){
    re.ssb0[b,] = rock.out[[1]]$re.ssb[a,b,1,]
  }
  boxplot(t(re.ssb0), ylim = c(-0.5, 0.50), col = rep('grey',ass.num), axes= F)
  box(); abline(h = 0)
  axis(side = 2); axis(side = 1, at = seq(1,26,3), labels = seq(hist.yrs, hist.yrs + 100, 12))
  #print.letter(alpha.label2[a], xy = c(0.16, 0.05))
  if(a == 1) { 
    mtext(side = 3, outer = T, "Rockfish", line = 1)
    mtext(side = 2, outer = T, "SSB0", line = 2)
  } 
  print.letter(alpha.label2[a], xy = c(0.15, 0.05), cex = 1.1)  
}



#Relative Error about Natural Mortality ================================================
par(mfrow= c(3,2), mar = c(0.1,0.1,0.1,0.1), oma = c(4,4,2,4), cex.axis = 1.1, cex.lab = 1.1)
ymin = -0.30 ; ymax = 0.45
ind = 1:14
for(b in 1:ds){
  ass.num = 26
  if (b == ds) { axis(side =1, at = seq(1,max(ind),2), labels = seq(hist.yrs, hist.yrs + 4 * max(ind) - 4, 8))
       mtext(side = 2, "RE Natural Mortality", outer =T, line = 2.5) }
  #print.letter(alpha.label1[b], xy = c(0.04, 0.95))
  boxplot(t(rock.out[[1]]$re.m[b,,]), ylab = "RE M", ylim = c(ymin,ymax), col = rep('grey',ass.num), axes = F)
  abline(h = 0, lty = 2, col = 1); box()
  if (b == 1) { mtext(side =3, "Rockfish", outer = F, line = 0.5); axis(side = 2)}
  if (b == 3) { axis(side = 2) }
  #print.letter(alpha.label2[b], xy = c(0.16, 0.05))
  if (b == ds) { axis(side =1, at = seq(1,26,3), labels = seq(hist.yrs, hist.yrs + 100, 12)); 
                  mtext(side = 1, "Assessment Year", outer = T, line = 2.5) }
}

# Steepness ================================================================================
par(mfrow= c(3,2), mar = c(0.1,0.1,0.1,0.1), oma = c(4,4,2,4), cex.axis = 1.1, cex.lab = 1.1)
ymin = 0.20 ; ymax = 1
ind = 1:14
for(b in 1:ds){
  ass.num = 26
  if (b == ds) { axis(side =1, at = seq(1,max(ind),2), labels = seq(hist.yrs, hist.yrs + 4 * max(ind) - 4, 8))
       mtext(side = 2, "Steepness", outer =T, line = 2.5) }
  #print.letter(alpha.label1[b], xy = c(0.04, 0.95))
  boxplot(t(rock.out[[2]]$h.est[b,,]), ylab = "Steepness", ylim = c(ymin,ymax), col = rep('grey',ass.num), axes = F)
  abline(h = 0.60, lty = 2, col = 1); box()
  if (b == 1) { mtext(side =3, "Rockfish", outer = F, line = 0.5); axis(side = 2)}
  if (b == 3) { axis(side = 2) }
  #print.letter(alpha.label2[b], xy = c(0.16, 0.05))
  if (b == ds) { axis(side =1, at = seq(1,26,3), labels = seq(hist.yrs, hist.yrs + 100, 12)); 
                  mtext(side = 1, "Assessment Year", outer = T, line = 2.5) }
}

# RMSE SB0 ===========================================================================================
par(mfrow = c(2,2))

rmse.sb0 = rock.out[[1]]$rmse.sb0
plot(1:26, rmse.sb0[1,], type = 'l', ylim = c(0, 30), col = 1, axes= F)
lines(1:26, rmse.sb0[3,], lty = 2, col = 2)
lines(1:26, rmse.sb0[5,], lty = 2, col = 3)
mtext(side = 2, outer = F, "RMSE SB0", line = 2)
box(); abline(h = 0)
axis(side = 2); axis(side = 1, at = seq(1,26,3), labels = seq(hist.yrs, hist.yrs + 100, 12))

plot(1:26, rmse.sb0[2,], type = 'l', ylim = c(0, 30), col = 1, axes= F)
lines(1:26, rmse.sb0[4,], lty = 2, col = 2)
lines(1:26, rmse.sb0[6,], lty = 2, col = 3)
box(); abline(h = 0)
axis(side = 2); axis(side = 1, at = seq(1,26,3), labels = seq(hist.yrs, hist.yrs + 100, 12))
mtext(side = 3, outer = T, "Rockfish", line = 1)

rmse.depl = rock.out[[1]]$rmse.depl
plot(1:26, rmse.depl[1,], type = 'l', ylim = c(0, 70), col = 1, axes= F)
lines(1:26, rmse.depl[3,], lty = 2, col = 2)
lines(1:26, rmse.depl[5,], lty = 2, col = 3)
mtext(side = 2, outer = F, "RMSE Depletion", line = 2)
box(); abline(h = 0)
axis(side = 2); axis(side = 1, at = seq(1,26,3), labels = seq(hist.yrs, hist.yrs + 100, 12))

plot(1:26, rmse.depl[2,], type = 'l', ylim = c(0, 70), col = 1, axes= F)
lines(1:26, rmse.depl[4,], lty = 2, col = 2)
lines(1:26, rmse.depl[6,], lty = 2, col = 3)
box(); abline(h = 0)
axis(side = 2); axis(side = 1, at = seq(1,26,3), labels = seq(hist.yrs, hist.yrs + 100, 12))
mtext(side = 3, outer = T, "Rockfish", line = 1)


#Plot the distributions of stock determinations========================================================

temp = rock.out[[2]]$time.over - rock.out[[3]]$om.time.over
ind = temp[1,] > -101
a = hist(temp[1,ind], ylim = c(0,35), breaks = c(seq(-75, 75, 5)))
boxplot(t(rock.out[[1]]$re.time.over), ylim = c(-1,1.25)); abline(h=0)

par(mfrow= c(ds,2), mar = c(1, 1, 1, 1), oma = c(4,4,2,4), cex.axis = 1.1, cex.lab = 1.1)
for (b in 1:ds){
  a = hist(rock.out[[1]]$yrs.declared.all[b,]/48, ylim = c(0,35), breaks = c(seq(-1.55,2.15,0.15)), main = "", axes = T)
  lines(rep(0,2), c(0, a$counts[11] ), col =1, lty = 2)
  print.letter(alpha.label2[b], xy = c(0.05, 0.90))
  if (b == 1) { mtext(side =3, "Rockfish", outer = F, line = 0.5)}
  if (b == ds) {  mtext(side = 1, "Relative Years Declared Recovered", line = 2.5, outer = T)}
}

par(mfrow= c(3,2), mar = c(1, 1, 1, 1), oma = c(4,4,2,4), cex.axis = 1.1, cex.lab = 1.1)
for (b in 1:ds){
  #a = hist(flat.out[[1]]$yrs.declared.all[b,], ylim = c(0,35), breaks = c(seq(-54,95,4)), main = "", axes = T)
  #lines(rep(0,2), c(0, a$counts[14] ), col =1, lty = 2.5)
  #if (b == 1) { mtext(side =3, "Flatfish", outer = F, line = 0.5)}
  ##axis(side = 2) ; axis(side = 1, at = seq(-2, 2, 1))
  #if (b == ds) { mtext(side = 2, "Count", line = 2, outer = T) }
  #print.letter(alpha.label1[b], xy = c(0.05, 0.90))

  a = hist(rock.out[[1]]$yrs.declared.all[b,], ylim = c(0,35), breaks = c(seq(-61,95,4)), main = "", axes = T)
  lines(rep(0,2), c(0, a$counts[14] ), col =1, lty = 2)
  print.letter(alpha.label2[b], xy = c(0.16, 0.90))
  #axis(side = 1, at = seq(-2, 3, 1))
  #axis(side = 1, at = seq(-2, 3, 1));
  if (b == 1) { mtext(side =3, "Rockfish", outer = F, line = 0.5)}
  if (b == ds) {  mtext(side = 1, "Relative Years Declared Recovered", line = 2.5, outer = T)}
}

#plot(1:ass.num, temp1, ylab="Failed to Detect", type = 'b', col = "red", ylim = c(0, max(temp1)+2))
#points(1:ass.num, temp1, pch = 16, col = 'red')
#points(1:ass.num, temp2, pch = 16, col = "blue")
#lines (1:ass.num, temp2, lty = 1,  col = "blue")
#points(1:ass.num, temp3, pch = 16, col = "green")
#lines (1:ass.num, temp3, lty = 1,  col = "green")

#RMSE Table =========================================================================================================================
ind = c(2, 3, 4, 5, 8, 11, 13)
#a = flat.out[[1]]$rmse.sb0[, ind]
#b = flat.out[[1]]$rmse.depl[,ind]
ind = c(3, 6, 10, 14, 18, 22, 26) 
c = rock.out[[1]]$rmse.sb0[, ind]
d = rock.out[[1]]$rmse.depl[,ind]

#write.table(round(a, 0) , file = "", quote = F, row.names = F, col.names = F)
#write.table(round(b, 0) , file = "", quote = F, row.names = F, col.names = F)
write.table(round(c, 0) , file = "", quote = F, row.names = F, col.names = F)
write.table(round(d, 0) , file = "", quote = F, row.names = F, col.names = F)

#Median Relative Errors ============================================================================================================ 
#ind = c(2, 3, 4, 5, 8, 11, 13)
#a  = b = NULL
#for (i in 1:length(ind)){
#  a = cbind(a, apply(flat.out[[1]]$re.ssb0[,ind[i],],1,median))
#  y = 50 + ind[i]*4 - 4
#  b = cbind(b, apply(flat.out[[1]]$re.depl[,ind[i],y,], 1, median))
#}

ind = c(3, 6, 10, 14, 18, 22, 26)  
c = d =NULL
for (i in 1:length(ind)){
  c = cbind(c, apply(rock.out[[1]]$re.ssb0[,ind[i],],1,median))
  y = 50 + ind[i]*4 - 4
  d = cbind(d, apply(rock.out[[1]]$re.depl[,ind[i],y,], 1, median))
}

#write.table(round(a, 2) , file = "", quote = F, row.names = F, col.names = F)
#write.table(round(b, 2) , file = "", quote = F, row.names = F, col.names = F)
write.table(round(c, 2) , file = "", quote = F, row.names = F, col.names = F)
write.table(round(d, 2) , file = "", quote = F, row.names = F, col.names = F)


#####################################################################################################################################
##                ADDITIONAL PLOTS NOT USED IN THE PAPER FOR MODEL EXPLORATION                                                  #####
#####################################################################################################################################

#Relative error of spawning biomass and depletion

#Rockfish
ass.index = c(26, 20, 15, 10, 5)
par(mfrow =c (ds,length(ass.index)), oma = c(4,4,2,4), mar = c(1,1,1,1))
for (b in 1:ds){
  for (a in 1:length(ass.index)){
    ind = sort(ass.index)
    temp = 90:(120 + ind[a]* 4 - 4)
    med.re.ssb = apply(rock.out[[1]]$re.ssb[b, ind[a], temp,], 1, quantile, c(0.975, 0.50, 0.25))
    plot( temp, med.re.ssb[2,], type = 'l', lwd =2,ylim = c( -0.50, 1), axes = F)# xlim = c(1, max(50 + ass.index.rock* 4 - 4)),
    xx = c(temp, rev(temp)); yy = c(med.re.ssb[1,], rev(med.re.ssb[3,]))
    polygon(xx, yy, col = grey, border = NA)
    lines(temp, med.re.ssb[1,], lty = 2)
    lines(temp, med.re.ssb[3,], lty = 2)
    #boxplot(t(flat.out[[1]]$re.ssb[b, ind[a], temp, ]), ylim = c(-1, 1), axes = F, xlim = c(1, max(50 + ass.index.flat* 4 - 4)))
    box(); abline (h = 0, lty =1); abline(v = 120, lty = 2)
    if (a == 1) { axis(side = 2); mtext(side = 2, "Relative Error Spawning Biomass", outer = T, line = 2.5) }
    if (b == ds){ axis(side = 1); mtext(side = 1, "Year", outer = T, line = 2.5) }
  }  
}

#Rockfish
ass.index = c(26, 20, 15, 10, 5)
par(mfrow =c (ds,length(ass.index)), oma = c(4,4,2,4), mar = c(1,1,1,1))
for (b in 1:ds){
  for (a in 1:length(ass.index)){
    ind = sort(ass.index)
    temp = 1:(50 + ind[a]* 4 - 4)
    med.re.depl = apply(rock.out[[1]]$re.depl[b, ind[a], temp,], 1, quantile, c(0.975, 0.50, 0.25))
    plot( temp, med.re.depl[2,], type = 'l', lwd =2,  ylim = c( -0.25, 0.60), axes = F) #,xlim = c(1, max(50 + ass.index* 4 - 4)),
    xx = c(temp, rev(temp)); yy = c(med.re.depl[1,], rev(med.re.depl[3,]))
    polygon(xx, yy, col = grey, border = NA)
    lines(temp, med.re.depl[1,], lty = 2)
    lines(temp, med.re.depl[3,], lty = 2)
    #boxplot(t(flat.out[[1]]$re.ssb[b, ind[a], temp, ]), ylim = c(-1, 1), axes = F, xlim = c(1, max(50 + ass.index.flat* 4 - 4)))
    box(); abline (h = 0, lty =1); abline(v = 50, lty = 2)
    if (a == 1) { axis(side = 2); mtext(side = 2, "Relative Error Depletion", outer = T, line = 2.5) }
    if (b == ds){ axis(side = 1); mtext(side = 1, "Year", outer = T, line = 2.5) }
  }  
}

#--------------------------------------------------------------------------------------------------------------

#Estimates of Natural Mortality
par(mfrow = c(3,2), oma = c(4,4,2,4), mar = c(2,2,2,4))
for(b in 1:ds){
    #boxplot(t(flat.out[[2]]$m.est[b,,]), ylim = c(0.15 - 0.30*0.15, 0.15 + 0.30*0.15), axes = F, col = rep('grey',ass.num))
    #abline(h = 0.15,col = 1, lty =2) ; axis(side = 2); box()
    if (b == ds) { axis(side = 1); mtext(side = 1, outer = T, "Assessment Year" , line = 2)
                   mtext(side = 2, outer = T, "Natural Mortality", line = 2)}
    boxplot(t(rock.out[[2]]$m.est[b,,]), ylim = c(0.08 - 0.30*0.08, 0.12), axes = F, col = rep('grey',ass.num))
    abline(h = 0.08,col = 1, lty =2); axis(side = 2); box()
    if (b == ds) { axis(side = 1)}
}


#Selectivity Plots ==========================================================================================
par(mfrow = c(ds,5), oma = c(4,4,2,4), mar = c(2,2,2,2))

for(b in 1:ds){
  xfact = 0.10
  boxplot(t(rock.out[[2]]$s.selex.est[b,1,,]), ylim = c(39 - xfact*39, 39 + xfact*39))
  abline(h = 39,col = 1, lty =2)
  mtext(side = 3, "Survey Peak")
  boxplot(t(rock.out[[2]]$s.selex.est[b,3,,]), ylim = c(2, 5))
  abline(h = 4.25, col = 1, lty =2)
  mtext(side = 3, "Survey Asc")
  
  xfact = 0.05
  boxplot(t(rock.out[[2]]$f.selex.est[b,1,,]), ylim = c(45 - xfact*45, 45 + xfact*45))
  abline(h = 45,col = 1, lty =2)
  mtext(side = 3, "Fishery Peak")
  boxplot(t(rock.out[[2]]$f.selex.est[b,3,,]), ylim = c(2, 5))
  abline(h = 4.25,col = 1, lty =2)
  mtext(side = 3, "Fishery Asc")
  boxplot(t(rock.out[[2]]$f.selex.adj.est[b,1,,]), ylim = c(-10, 10))
  abline(h = -2.5,col = 1, lty =2)
  mtext(side = 3, "Fishery Top")
}

#Relative Error for Selectivity ================================================================================================
par(mfrow = c(ds,5), oma = c(4,4,2,4), mar = c(0,0,0,0))

#for(b in 1:ds){  
#  boxplot(t(flat.out[[1]]$re.s.selex[b,1,,]), ylim =   c(-0.15, 0.15), axes = F)
#  abline(h = 0,col = 1, lty =2); box(); axis(side = 2 )
#  if(b == ds) {  axis(side =1, at = 1:14, labels = seq(50, 102,4))
#     mtext(side = 2, outer = T, "Selecitivity", line = 2.5 ) }
#  boxplot(t(flat.out[[1]]$re.s.selex[b,2,,]), ylim =   c(-0.15, 0.15), axes = F)
#  abline(h = 0, col = 1, lty =2); box()
#  if(b == ds) { axis(side =1, at = 1:14, labels = seq(50, 102,4)) }
#  
#  xfact = 0.05
#  boxplot(t(flat.out[[1]]$re.f.selex[b,1,,]), ylim =   c(-0.15, 0.15), axes = F)
#  abline(h = 0,col = 1, lty =2); box()
#  if(b == ds) { axis(side =1, at = 1:14, labels = seq(50, 102,4)) }
#  boxplot(t(flat.out[[1]]$re.f.selex[b,2,,]), ylim =   c(-0.15, 0.15), axes = F)
#  abline(h = 0,col = 1, lty =2); box()
#  if(b == ds) { axis(side =1, at = 1:14, labels = seq(50, 102,4)) }
#  boxplot(t(flat.out[[1]]$re.f.selex.adj[b,,]), ylim = c(-0.15, 0.15), axes = F)
#  abline(h = 0,col = 1, lty =2); box()
#  if(b == ds) { axis(side =1, at = 1:14, labels = seq(50, 102,4))
#     mtext(side = 1, outer = T, "Assessment Year", line = 2.5) }
#}


for(b in 1:ds){
  boxplot(t(rock.out[[1]]$re.s.selex[b,1,,]), ylim =  c(-0.1, 0.1), axes = F)
  abline(h = 0,col = 1, lty =2); box(); axis(side = 2 )
  if(b == ds) { axis(side =1); mtext(side = 2, outer = T, "Selecitivity", line = 2.5 ) }
  boxplot(t(rock.out[[1]]$re.s.selex[b,2,,]), ylim =  c(-0.1, 0.1), axes = F)
  abline(h = 0, col = 1, lty =2); box()
  if(b == ds) { axis(side =1, at = 1:26, labels = seq(50,150,4)) }
  
  boxplot(t(rock.out[[1]]$re.f.selex[b,1,,]), ylim =  c(-0.1, 0.1), axes = F)
  abline(h = 0,col = 1, lty =2); box()
  if(b == ds) { axis(side =1, at = 1:26, labels = seq(50,150,4))  }
  boxplot(t(rock.out[[1]]$re.f.selex[b,2,,]), ylim =  c(-0.1, 0.1), axes = F)
  abline(h = 0,col = 1, lty =2); box()
  if(b == ds) { axis(side =1, at = 1:26, labels = seq(50,150,4))  }
  #boxplot(t(rock.out[[1]]$re.f.selex.adj[b,,]), ylim =  c(-0.1, 0.1), axes = F)
  #abline(h = 0,col = 1, lty =2); box()
  if(b == ds) { axis(side =1, at = 1:26, labels = seq(50,150,4)) 
    mtext(side = 1, outer = T, "Assessment Year", line = 2.5) }
}

#Catches ========================================================================================================================
par(mfrow= c(1,1), mar = c(0,0,0,0), oma = c(4,4,2,4), cex.axis = 1.1, cex.lab = 1.1)
ymax = max(rock.out[[1]]$med.acl)
tot.catch = NULL
for(b in 1:ds){

  ind = (pre.yrs.rock + 51):(final.yr.rock - 1)
  tot.catch = cbind(tot.catch, apply(rock.out[[2]]$acl.est[b, ind,], 2, sum) )
  #boxplot(rock.out[[2]]$catch.est[,ind,], ylim = c(0, 1000))
  #plot(ind,  rock.out[[1]]$med.acl[ds,ind,2], col = 1, 
  #  type = 'l', lwd =2, ylim = c(0,ymax), axes = F)
  #box() 
  #if (b == ds) { axis(side = 1)}
  #lines(ind, rock.out[[1]]$med.acl[ds,ind,1], col = 1, lty = 2, lwd =2)
  #lines(ind, rock.out[[1]]$med.acl[ds,ind,3], col = 1, lty = 2, lwd =2)
  #lines(ind, rock.out[[1]]$med.catch.est[b,ind,2], col = 'red', lty = 1, lwd =1)
}

boxplot(tot.catch, ylim = c(0, 35000))

#Relative Error about estimated final depletion========================================
par(mfrow = c(ds,2), oma = c(4,4,2,4), mar = c(0.25,0.25,1,1))
for (b in 1:ds){
  ind = seq(50,102,4)
  final.depl.re = matrix(0, length(ind), dim(flat.out[[1]]$re.depl)[4])
  for(a in 1:length(ind)){
    index = ind[a]
    final.depl.re[a,] = flat.out[[1]]$re.depl[b,a,index,]
  }

  boxplot(t(final.depl.re), ylim = c(-0.5, 0.5), axes = F)
  abline(h = 0, lty = 2, col = 'red')
  box(); axis(side = 2); mtext(side = 2, "RE Assessment Year Relative Biomass", outer = T, line = 2)
  if (b == ds) { axis(side = 1, at =seq(1,14,1), labels = ind) ; mtext(side = 1, "Assessment Year", outer = T, line = 2) }

  ind = seq(50,150,4)
  final.depl.re = matrix(0, length(ind), dim(rock.out[[1]]$re.depl)[4])

  for(a in 1:length(ind)){
    index = ind[a]
    final.depl.re[a,] = rock.out[[1]]$re.depl[b,a,index,]
  }

  boxplot(t(final.depl.re), ylim = c(-0.5, 0.5), axes = F)
  abline(h = 0, lty = 2, col = 'red')
  box()
  if (b == ds) { axis(side = 1, at= seq(1,26,1), labels = ind) }
}

#Relative Error about estimated virgin SB ========================================
par(mfrow = c(ds,2), oma = c(4,4,2,4), mar = c(2,2,2,4))
for (b in 1:ds){
  ind = seq(50,102,4)
  boxplot(t(flat.out[[1]]$re.ssb0[b,,]), xlab= "RE SB0", ylim = c(-0.5, 0.5), axes = F)
  box(); axis(side = 2)
  abline(h = 0, lty = 2, col = 'red')
  mtext(side = 2, "RE Assessment Year Relative Biomass", outer = T, line = 2)
  if (b == ds) { axis(side = 1, at =seq(1,14,1), labels = ind) ; mtext(side = 1, "Assessment Year", outer = T, line = 2) }

  ind = seq(50,150,4)
  boxplot(t(rock.out[[1]]$re.ssb0[b,,]), xlab= "RE SB0", ylim = c(-0.5, 0.5), axes = F)
  box(); axis(side = 2)
  abline(h = 0, lty = 2, col = 'red')
  if (b == ds) { axis(side = 1, at= seq(1,26,1), labels = ind) }
}



#sim = 92
#par(mfrow=c(2,2))
#for(sim in 1:sim) 
#{
#  plot(1:151, depl[71:221,sim], type = 'l', ylim =c(0,1.2), lwd =2, main = sim)
#  abline (h =0.40)
#  for(i in 1:26)
#  {
#    ind2 = setup.yrs + i*4 - 3
#    ind = pre.fishery.yrs + setup.yrs + i*4 - 3
#    points(ind2, depl.est[ind2,i,sim], col = 'red', pch = 16)
#    print(cbind(depl.est[ind2,i,sim],depl[ind,sim]))
#  }
#}
