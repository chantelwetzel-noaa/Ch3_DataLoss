############################################
#   Chapter 3: Loss of Data & Rebuilding   #
#                                          #
#    Create Plots from R object created    #
#          by Simulation_Eval code         #
#         Created July 31, 2014 by         #
#             Chantel Wetzel               #
############################################


#Load in the R objects from the Simulation Eval Code ========================================
drive = "G"

load(paste(drive,":/PhD/Chapter3/rockfish_meds_split", sep = ""))
med.rock <- meds.split
load(paste(drive,":/PhD/Chapter3/rockfish_est_split", sep = ""))
est.rock <- est.split
load(paste(drive,":/PhD/Chapter3/rockfish_om_split", sep = ""))
om.rock <- om.split
load(paste(drive,":/PhD/Chapter3/flatfish_meds_split", sep = ""))
med.flat <- meds.split
load(paste(drive,":/PhD/Chapter3/flatfish_est_split", sep = ""))
est.flat <- est.split
load(paste(drive,":/PhD/Chapter3/flatfish_om_split", sep = ""))
om.flat <- om.split

#load(paste(drive,":/PhD/Chapter3/rockfish_meds_all", sep = ""))
#med.rock <- meds.all
#load(paste(drive,":/PhD/Chapter3/rockfish_est_all", sep = ""))
#est.rock <- est.all
#load(paste(drive,":/PhD/Chapter3/rockfish_om_all", sep = ""))
#om.rock <- om.all
#load(paste(drive,":/PhD/Chapter3/flatfish_meds_all", sep = ""))
#med.flat <- meds.all
#load(paste(drive,":/PhD/Chapter3/flatfish_est_all", sep = ""))
#est.flat <- est.all
#load(paste(drive,":/PhD/Chapter3/flatfish_om_all", sep = ""))
#om.flat <- om.all

rock.out <- flat.out <- list()
rock.out[[1]] <- med.rock
rock.out[[2]] <- est.rock
rock.out[[3]] <- om.rock
flat.out[[1]] <- med.flat
flat.out[[2]] <- est.flat
flat.out[[3]] <- om.flat

#==================================================================================================================
print.letter <- function(label="(a)",xy=c(0.1,0.925),...) {   #... means any new parameters are copied faithfully to text() below
  tmp <- par("usr")
  text.x <- tmp[1]+xy[1]*diff(tmp[1:2])   #x position, diff=difference
  text.y <- tmp[3]+xy[2]*diff(tmp[3:4])   #y position
  text(x=text.x, y=text.y, labels=label, ...)             
}

alpha.label1 = c('(a)', '(c)', '(e)')
alpha.label2 = c('(b)', '(d)', '(f)')
name.label = c('Great All', 'Great Historical', 'Normal')

library(RColorBrewer)

#Dimensions used for plotting======================================================================================
ds =  dim(flat.out[[2]]$ssb.est)[1] #Determines the number of data scenarios to plot
flat.yrs = 20#50
rock.yrs = 100
pre.yrs.flat = 41
pre.yrs.rock = 71
final.yr.flat = flat.yrs + 50 + pre.yrs.flat
final.yr.rock = rock.yrs + 50 + pre.yrs.rock - 1
ass.num = dim(flat.out[[2]]$ssb.est)[3]
ass.yr = seq(50,150,4)

#Create Plots to compare results==================================================================
windows(10, 10, record = TRUE)

#Spawning Biomass
par(mfrow= c(ds,2), mar = c(0.1,0.1,0.1,0.1), oma = c(4,4,2,4), cex.axis = 1.1, cex.lab = 1.1)
start.plot.flat = 85#75#82
start.plot.rock = 105#112
color <- rgb(0, 0, 0, 0.30) 
ass.index.flat = rev(c(2, 1, 4, 6))#rev(c(1,  5, 10, 14))#rev(c(1, 7, 14))
ass.index.rock = rev(c(1, 14, 26))#rev(c(1, 10, 20, 26))#rev(c(1, 13, 26))
a = 256
plot.color = c(rgb(0, 153/a, 0), rgb(255/a, 128/a, 0), "Blue", "violet")
#plot.color = c('blue', "darkgreen", 'red')
trans = 0.20
trans.color = c(rgb(0, 153/a, 0, trans), rgb(255/a, 128/a, 0, trans+ 0.05), rgb(0,0,1, trans), rgb(0,0.5, 0.5, trans))
#trans.color = c(rgb(0, 0, 1, trans), rgb(0, 1, 0, 0.25), rgb(1,0,0, 0.3))

#c(rgb(153/a, 0, 0), rgb(255/a, 128/a, 0), rgb(0, 153/a, 0), "Blue")
#colorRampPalette(brewer.pal(10, "Blues"))(10)
lty.type = c(4, 3, 2, 5, 1)
ymax = ceiling( max(flat.out[[1]]$med.ssb[,start.plot.flat:final.yr.flat,], rock.out[[1]]$med.ssb[,start.plot.rock:final.yr.rock,]))

for(b in 1:ds){
  #Plot flatfish
  plot(start.plot.flat:final.yr.flat,  flat.out[[1]]$med.ssb[b,start.plot.flat:final.yr.flat,2], col = rgb(1,1,1,1), type = 'l', lwd =2,
     ylim = c(0,ymax), ylab = "SB", xlab = "Year", axes = F, xaxs="i")
  #xx = c(start.plot.flat:final.yr.flat,rev(start.plot.flat:final.yr.flat))
  #yy = c(flat.out[[1]]$med.ssb[b,start.plot.flat:final.yr.flat,1],rev(flat.out[[1]]$med.ssb[b,start.plot.flat:final.yr.flat,3])) 
  #polygon(xx, yy, col=color, lty=1, border = 'grey')
  #lines(start.plot.flat:final.yr.flat,  flat.out[[1]]$med.ssb[b,start.plot.flat:final.yr.flat,2], col = rgb(1,1,1,1), lty = 1, lwd =4)
  lines(start.plot.flat:final.yr.flat,  flat.out[[1]]$med.ssb[b,start.plot.flat:final.yr.flat,1], col = 'darkgrey', lty = 4, lwd =1)
  lines(start.plot.flat:final.yr.flat,  flat.out[[1]]$med.ssb[b,start.plot.flat:final.yr.flat,3], col = 'darkgrey', lty = 4, lwd =1)
  box() ; axis(side = 2)
  print.letter(alpha.label1[b], xy = c(0.05, 0.95))
  if(b == 1)  {  mtext("Flatfish", side = 2, outer = T, line = 2.5) }
  if(b == ds) { 
    mtext("Spawning Biomass", side = 2, outer = T, line = 2.5); axis(side = 1 , at =seq(80, 140,10), labels = seq(40, 100, 10))
  }

  for (i in 1:length(ass.index.flat)){
    temp = ass.index.flat[i]*4 - 4 + 50
    y = (start.plot.flat):(temp + pre.yrs.flat - 1)
    ind =(start.plot.flat - pre.yrs.flat + 1):temp
    xx = c(y, rev(y))
    yy = c(flat.out[[1]]$med.ssb.est[b,ind,ass.index.flat[i],1], rev(flat.out[[1]]$med.ssb.est[b,ind,ass.index.flat[i],3]))
    polygon(xx, yy, col = trans.color[i], border = "NA")
  }
  lines(start.plot.flat:final.yr.flat,  flat.out[[1]]$med.ssb[b,start.plot.flat:final.yr.flat,2], col = 'grey70', lty = 1, lwd = 3)
  for (i in 1:length(ass.index.flat))
  {
    temp = ass.index.flat[i]*4 - 4 + 50
    y = (start.plot.flat):(temp + pre.yrs.flat - 1)
    ind =(start.plot.flat - pre.yrs.flat + 1):temp
    lines(y, flat.out[[1]]$med.ssb.est[b,ind,ass.index.flat[i],2], col = plot.color[i], lty = lty.type[i], lwd = 2)
    #lines(y, flat.out[[1]]$med.ssb.est[b,ind,ass.index.flat[i],1], col = plot.color[i], lty = lty.type[i], lwd = 1)
    #lines(y, flat.out[[1]]$med.ssb.est[b,ind,ass.index.flat[i],3], col = plot.color[i], lty = lty.type[i], lwd = 1)
  }
  
  #Plot rockfish
  plot(start.plot.rock:final.yr.rock,  rock.out[[1]]$med.ssb[b,start.plot.rock:final.yr.rock,2], col = 1, type = 'l', lwd =2, 
    ylim = c(0,ymax), ylab = "SB", xlab = "Year",  axes = F, xaxs="i")
  if(b ==1) { mtext(side = 3, "Rockfish", outer = T, line = 2.5) }
  box()
  if(b == ds) { 
    axis(side = 1, at =seq(120, 220,20), labels = seq(50, 150, 20)) ; mtext("Year", side = 1, outer = T, line = 2.5)
  }
  #xx = c(start.plot.rock:final.yr.rock,rev(start.plot.rock:final.yr.rock))
  #yy = c(rock.out[[1]]$med.ssb[b,start.plot.rock:final.yr.rock,1],rev(rock.out[[1]]$med.ssb[b,start.plot.rock:final.yr.rock,3])) 
  #polygon(xx, yy, col=color, lty=0)
  #lines(start.plot.rock:final.yr.rock,  rock.out[[1]]$med.ssb[b,start.plot.rock:final.yr.rock,2], col = rgb(1,1,1,1), lty = 1, lwd =4)
  lines(start.plot.rock:final.yr.rock,  rock.out[[1]]$med.ssb[b,start.plot.rock:final.yr.rock,1], col = 'darkgrey', lty = 4, lwd =1)
  lines(start.plot.rock:final.yr.rock,  rock.out[[1]]$med.ssb[b,start.plot.rock:final.yr.rock,3], col = 'darkgrey', lty = 4, lwd =1)
  print.letter(alpha.label2[b], xy = c(0.05, 0.95))

  for (i in 1:length(ass.index.rock)){
    temp = ass.index.rock[i]*4 - 4 + 50
    y = (start.plot.rock):(temp + pre.yrs.rock - 1)
    ind = (start.plot.rock - pre.yrs.rock + 1):temp
    xx = c(y, rev(y))
    yy = c(rock.out[[1]]$med.ssb.est[b,ind,ass.index.rock[i],1], rev(rock.out[[1]]$med.ssb.est[b,ind,ass.index.rock[i],3]))
    polygon(xx, yy, col = trans.color[i], lty = 0)
  }
    lines(start.plot.rock:final.yr.rock,  rock.out[[1]]$med.ssb[b,start.plot.rock:final.yr.rock,2], col = 'grey70', lty = 1, lwd =3)
  for (i in 1:length(ass.index.rock))
  {
    temp = ass.index.rock[i]*4 - 4 + 50
    y = (start.plot.rock):(temp + pre.yrs.rock - 1)
    ind = (start.plot.rock - pre.yrs.rock + 1):temp
    lines(y, rock.out[[1]]$med.ssb.est[b,ind,ass.index.rock[i],2], col = plot.color[i], lty = lty.type[i], lwd =2)
    #lines(y, rock.out[[1]]$med.ssb.est[b,ind,ass.index.rock[i],1], col = plot.color[i], lty = lty.type[i], lwd =1)
    #lines(y, rock.out[[1]]$med.ssb.est[b,ind,ass.index.rock[i],3], col = plot.color[i], lty = lty.type[i], lwd =1)
  }
}


#Working====================================================================================================================================
#Spawning Biomass
par(mfrow= c(ds,2), mar = c(0.1,1.2,0.1,1.2), oma = c(4,4,4,4), cex.axis = 1.1, cex.lab = 1.1)
final.yr.flat = 110
final.yr.rock = 196
start.plot.flat = 85#75#82
start.plot.rock = 105#112
color <- rgb(0, 0, 0, 0.30) 
ass.index.flat = rev(c(1, 2, 3, 6)) #rev(c(1,  5, 10, 14))#rev(c(1, 7, 14))
ass.index.rock = rev(c(1, 8, 13, 20))#rev(c(1, 10, 20, 26))#rev(c(1, 13, 26))
a = 256
plot.color = c(rgb(0, 153/a, 0), rgb(255/a, 128/a, 0), "Blue", "Brown")
#plot.color = c('blue', "darkgreen", 'red')
trans = 0.20
trans.color = c(rgb(0, 153/a, 0, trans), rgb(255/a, 128/a, 0, trans+ 0.05),rgb(0,0,1, trans), rgb(153/a,0, 0, trans))
line.color = "white"
lty.type = c(4, 3, 2, 5, 1)
ymax.flat = ceiling( max(flat.out[[1]]$med.ssb[,start.plot.flat:final.yr.flat,]))
ymax.rock = ceiling( max(rock.out[[1]]$med.ssb[,start.plot.rock:final.yr.rock,])) + 1000
for(b in 1:ds){
  #Plot flatfish
  plot(start.plot.flat:final.yr.flat,  flat.out[[1]]$med.ssb[b,start.plot.flat:final.yr.flat,2], col = rgb(1,1,1,1), type = 'l', lwd =2,
     ylim = c(0,ymax.flat), ylab = "SB", xlab = "Year", axes = F, xaxs="i")
  lines(start.plot.flat:final.yr.flat,  flat.out[[1]]$med.ssb[b,start.plot.flat:final.yr.flat,1], col = 'darkgrey', lty = 4, lwd =1)
  lines(start.plot.flat:final.yr.flat,  flat.out[[1]]$med.ssb[b,start.plot.flat:final.yr.flat,3], col = 'darkgrey', lty = 4, lwd =1)
  box() ; axis(side = 2)
  print.letter(alpha.label1[b], xy = c(0.05, 0.95))
  if(b == 1)  {  mtext("Flatfish", side = 3, outer = F, line = 1) }
  if(b == ds) { 
    mtext("Spawning Biomass", side = 2, outer = T, line = 2.5); axis(side = 1 , at =seq(80, 140,5), labels = seq(40, 100, 5))
  }

  for (i in 1:length(ass.index.flat)){
    temp = ass.index.flat[i]*4 - 4 + 50
    y = (start.plot.flat):(temp + pre.yrs.flat - 1)
    ind =(start.plot.flat - pre.yrs.flat + 1):temp
    xx = c(y, rev(y))
    yy = c(flat.out[[1]]$med.ssb.est[b,ind,ass.index.flat[i],1], rev(flat.out[[1]]$med.ssb.est[b,ind,ass.index.flat[i],3]))
    polygon(xx, yy, col = trans.color[i], border = "NA")
  }
  lines(start.plot.flat:final.yr.flat,  flat.out[[1]]$med.ssb[b,start.plot.flat:final.yr.flat,2], col = line.color, lty = 1, lwd = 3)
  for (i in 1:length(ass.index.flat))
  {
    temp = ass.index.flat[i]*4 - 4 + 50
    y = (start.plot.flat):(temp + pre.yrs.flat - 1)
    ind =(start.plot.flat - pre.yrs.flat + 1):temp
    lines(y, flat.out[[1]]$med.ssb.est[b,ind,ass.index.flat[i],2], col = plot.color[i], lty = lty.type[i], lwd = 2)
  }
  
  #Plot rockfish
  plot(start.plot.rock:final.yr.rock,  rock.out[[1]]$med.ssb[b,start.plot.rock:final.yr.rock,2], col = 1, type = 'l', lwd =2, 
    ylim = c(0,ymax.rock), ylab = "SB", xlab = "Year",  axes = F, xaxs="i")
  if(b ==1) { mtext(side = 3, "Rockfish", outer = F, line = 1) }
  box() ; axis(side =2)
  if(b == ds) { 
    axis(side = 1, at =seq(120, 220,10), labels = seq(50, 150, 10)) ; mtext("Year", side = 1, outer = T, line = 2.5)
  }
  lines(start.plot.rock:final.yr.rock,  rock.out[[1]]$med.ssb[b,start.plot.rock:final.yr.rock,1], col = 'darkgrey', lty = 4, lwd =1)
  lines(start.plot.rock:final.yr.rock,  rock.out[[1]]$med.ssb[b,start.plot.rock:final.yr.rock,3], col = 'darkgrey', lty = 4, lwd =1)
  print.letter(alpha.label2[b], xy = c(0.05, 0.95))

  for (i in 1:length(ass.index.rock)){
    temp = ass.index.rock[i]*4 - 4 + 50
    y = (start.plot.rock):(temp + pre.yrs.rock - 1)
    ind = (start.plot.rock - pre.yrs.rock + 1):temp
    xx = c(y, rev(y))
    yy = c(rock.out[[1]]$med.ssb.est[b,ind,ass.index.rock[i],1], rev(rock.out[[1]]$med.ssb.est[b,ind,ass.index.rock[i],3]))
    polygon(xx, yy, col = trans.color[i], lty = 0)
  }
    lines(start.plot.rock:final.yr.rock,  rock.out[[1]]$med.ssb[b,start.plot.rock:final.yr.rock,2], col = line.color, lty = 1, lwd =3)
  for (i in 1:length(ass.index.rock))
  {
    temp = ass.index.rock[i]*4 - 4 + 50
    y = (start.plot.rock):(temp + pre.yrs.rock - 1)
    ind = (start.plot.rock - pre.yrs.rock + 1):temp
    lines(y, rock.out[[1]]$med.ssb.est[b,ind,ass.index.rock[i],2], col = plot.color[i], lty = lty.type[i], lwd =2)
  }
}



#Depletion================================================================================================================================
par(mfrow= c(ds,2), mar = c(0.1,0.1,0.1,0.1), oma = c(4,4,2,4), cex.axis = 1.1, cex.lab = 1.1)
ymax = 0.65
for(b in 1:ds){
  #Plot flatfish
  plot(start.plot.flat:final.yr.flat,  flat.out[[1]]$med.depl[b,start.plot.flat:final.yr.flat,2], col = rgb(1,1,1,1), type = 'l', lwd =2,
     ylim = c(0,ymax), ylab = "SB", xlab = "Year", axes = F, xaxs="i")
  #xx = c(start.plot.flat:final.yr.flat,rev(start.plot.flat:final.yr.flat))
  #yy = c(flat.out[[1]]$med.depl[b,start.plot.flat:final.yr.flat,1],rev(flat.out[[1]]$med.depl[b,start.plot.flat:final.yr.flat,3])) 
  #polygon(xx, yy, col=color, lty=0)
  #lines(start.plot.flat:final.yr.flat,  flat.out[[1]]$med.depl[b,start.plot.flat:final.yr.flat,2], col = rgb(1,1,1,1), lty = 1, lwd =4)
  lines(start.plot.flat:final.yr.flat,  flat.out[[1]]$med.depl[b,start.plot.flat:final.yr.flat,1], col = 'darkgrey', lty = 4, lwd =1)
  lines(start.plot.flat:final.yr.flat,  flat.out[[1]]$med.depl[b,start.plot.flat:final.yr.flat,3], col = 'darkgrey', lty = 4, lwd =1)
  box() ; axis(side = 2)
  abline(h = 0.25, lty = 3, col = 'red')
  print.letter(alpha.label1[b], xy = c(0.05, 0.95))
  if(b ==1) { mtext(side = 3, "Flatfish", outer = F, line = 1) }
  if(b == ds) { mtext("Depletion", side = 2, outer = T, line = 2.5); axis(side = 1 , at =seq(80, 140, 5), labels = seq(40, 100, 5)) }

  for (i in 1:length(ass.index.flat))
  {
    temp = ass.index.flat[i]*4 - 4 + 50
    y = (start.plot.flat):(temp + pre.yrs.flat - 1)
    ind =(start.plot.flat - pre.yrs.flat + 1):temp
    xx = c(y, rev(y))
    yy = c(flat.out[[1]]$med.depl.est[b,ind,ass.index.flat[i],1], rev(flat.out[[1]]$med.depl.est[b,ind,ass.index.flat[i],3]))
    polygon(xx, yy, col = trans.color[i], lty = 0)
  }
  lines(start.plot.flat:final.yr.flat,  flat.out[[1]]$med.depl[b,start.plot.flat:final.yr.flat,2], col = line.color, lty = 1, lwd =3)
  for (i in 1:length(ass.index.flat))
  {
    temp = ass.index.flat[i]*4 - 4 + 50
    y = (start.plot.flat):(temp + pre.yrs.flat - 1)
    ind =(start.plot.flat - pre.yrs.flat + 1):temp
    lines(y, flat.out[[1]]$med.depl.est[b,ind,ass.index.flat[i],2], col = plot.color[i], lty = lty.type[i], lwd =2)
    #lines(y, flat.out[[1]]$med.depl.est[b,ind,ass.index.flat[i],1], col = plot.color[i], lty = lty.type[i], lwd =1)
    #lines(y, flat.out[[1]]$med.depl.est[b,ind,ass.index.flat[i],3], col = plot.color[i], lty = lty.type[i], lwd =1)
  }

  #Plot rockfish
  plot(start.plot.rock:final.yr.rock,  rock.out[[1]]$med.depl[b,start.plot.rock:final.yr.rock,2], col = 1, type = 'l', lwd =2, 
    ylim = c(0,ymax), ylab = "Depletion", xlab = "Year",  axes = F, xaxs="i")
  box() ; if(b == ds) { axis(side = 1, at =seq(120, 220,10), labels = seq(50, 150, 10)) ; mtext("Year", side = 1, outer = T, line = 2.5)}
  if(b ==1) { mtext(side = 3, "Rockfish", outer = F, line = 1) }
  #xx = c(start.plot.rock:final.yr.rock,rev(start.plot.rock:final.yr.rock))
  #yy = c(rock.out[[1]]$med.depl[b,start.plot.rock:final.yr.rock,1],rev(rock.out[[1]]$med.depl[b,start.plot.rock:final.yr.rock,3])) 
  #polygon(xx, yy, col=color, lty=0)
  #lines(start.plot.rock:final.yr.rock,  rock.out[[1]]$med.depl[b,start.plot.rock:final.yr.rock,2], col = rgb(1,1,1,1), lty = 1, lwd =4)
  lines(start.plot.rock:final.yr.rock,  rock.out[[1]]$med.depl[b,start.plot.rock:final.yr.rock,1], col ='darkgrey', lty = 4, lwd = 1)
  lines(start.plot.rock:final.yr.rock,  rock.out[[1]]$med.depl[b,start.plot.rock:final.yr.rock,3], col ='darkgrey', lty = 4, lwd = 1)
  abline(h = 0.40, lty = 3, col = 'red')
  print.letter(alpha.label2[b], xy = c(0.05, 0.95))

  for (i in 1:length(ass.index.rock))
  {
    temp = ass.index.rock[i]*4 - 4 + 50
    y = (start.plot.rock):(temp + pre.yrs.rock - 1)
    ind = (start.plot.rock - pre.yrs.rock + 1):temp
    xx = c(y, rev(y))
    yy = c(rock.out[[1]]$med.depl.est[b,ind,ass.index.rock[i],1], rev(rock.out[[1]]$med.depl.est[b,ind,ass.index.rock[i],3]))
    polygon(xx, yy, col = trans.color[i], lty = 0)
  }
  lines(start.plot.rock:final.yr.rock,  rock.out[[1]]$med.depl[b,start.plot.rock:final.yr.rock,2], col = line.color, lty = 1, lwd =3)
  for (i in 1:length(ass.index.rock))
  {
    temp = ass.index.rock[i]*4 - 4 + 50
    y = (start.plot.rock):(temp + pre.yrs.rock - 1)
    ind = (start.plot.rock - pre.yrs.rock + 1):temp
    lines(y, rock.out[[1]]$med.depl.est[b,ind,ass.index.rock[i],2], col = plot.color[i], lty = lty.type[i], lwd =2)
    #lines(y, rock.out[[1]]$med.depl.est[b,ind,ass.index.rock[i],1], col = plot.color[i], lty = lty.type[i], lwd =1)
    #lines(y, rock.out[[1]]$med.depl.est[b,ind,ass.index.rock[i],3], col = plot.color[i], lty = lty.type[i], lwd =1)
  }
}




#Relative Error about Natural Mortality ================================================
par(mfrow= c(ds,2), mar = c(0.1,0.1,0.1,0.1), oma = c(4,4,2,4), cex.axis = 1.1, cex.lab = 1.1)
ymin = -0.30 ; ymax = 0.45
ind = 1:14
for(b in 1:ds){
  boxplot(t(flat.out[[1]]$re.m[b,ind,]), ylab = "", ylim = c(ymin,ymax), col = rep('grey',ass.num), axes = F)
  abline(h = 0, lty = 2, col = 1); box(); axis(side =2) 
  if (b == 1) { mtext(side =3, "Flatfish", outer = F, line = 0.5)}
  if (b == ds) { axis(side =1, at = seq(1,max(ind),2), labels = seq(50, 50 + 4 * max(ind) - 4, 8))
        mtext(side = 2, "RE Natural Mortality", outer =T, line = 2.5) }
  print.letter(alpha.label1[b], xy = c(0.04, 0.95))
  boxplot(t(rock.out[[1]]$re.m[b,,]), ylab = "RE M", ylim = c(ymin,ymax), col = rep('grey',ass.num), axes = F)
  abline(h = 0, lty = 2, col = 1); box()
  if (b == 1) { mtext(side =3, "Rockfish", outer = F, line = 0.5)}
  print.letter(alpha.label2[b], xy = c(0.04, 0.95))
  if (b == ds) { axis(side =1, at = seq(1,26,3), labels = seq(50, 150, 12)); mtext(side = 1, "Assessment Year", outer = T, line = 2.5) }
}

#Plot the distributions of stock determinations========================================================
#par(mfrow = c(ds,2), oma = c(4,4,2,4), mar = c(2,2,2,4))
#for (b in 1:ds){
#  a = hist(flat.out[[1]]$yrs.declared.all[b,], ylim = c(0,40), breaks = c(seq(-30,30,4)), main = "")
#  axis(side = 2) 
#  lines(rep(0,2), c(0, a$counts[13] ), col =1, lty = 2.5)
#  if (b == ds) { axis(side = 1, at = seq(-2, 2, 0.50)); mtext(side = 2, "Counts", line = 2, outer = T) }
#  print.letter(alpha.label1[b], xy = c(0.05, 0.85))
#
#  a = hist(rock.out[[1]]$yrs.declared.all[b,], ylim = c(0,40), breaks = c(seq(-50,90,4)), main = "")
#  lines(rep(0,2), c(0, a$counts[13] ), col =1, lty = 2)
#  print.letter(alpha.label2[b], xy = c(0.05, 0.85))
#  if (b == ds) { axis(side = 1, at = seq(-2, 3, 1)); mtext(side = 1, "Relative Years Declared Recovered", line = 2.5, outer = T)}
#}

par(mfrow= c(ds,2), mar = c(1, 1, 1, 1), oma = c(4,4,2,4), cex.axis = 1.1, cex.lab = 1.1)
for (b in 1:ds){
  a = hist(flat.out[[1]]$yrs.declared.all[b,]/26.5, ylim = c(0,35), breaks = c(seq(-1.45,2.15,0.10)), main = "", axes = T)
  lines(rep(0,2), c(0, a$counts[15] ), col =1, lty = 2.5)
  if (b == 1) { mtext(side =3, "Flatfish", outer = F, line = 0.5)}
  #axis(side = 2) ; axis(side = 1, at = seq(-2, 2, 1))
  if (b == ds) { mtext(side = 2, "Count", line = 2, outer = T) }
  print.letter(alpha.label1[b], xy = c(0.05, 0.90))

  a = hist(rock.out[[1]]$yrs.declared.all[b,]/48, ylim = c(0,35), breaks = c(seq(-1.45,2.15,0.10)), main = "", axes = T)
  lines(rep(0,2), c(0, a$counts[15] ), col =1, lty = 2)
  print.letter(alpha.label2[b], xy = c(0.05, 0.90))
  #axis(side = 1, at = seq(-2, 3, 1))
  #axis(side = 1, at = seq(-2, 3, 1));
  if (b == 1) { mtext(side =3, "Rockfish", outer = F, line = 0.5)}
  if (b == ds) {  mtext(side = 1, "Relative Years Declared Recovered", line = 2.5, outer = T)}
}

par(mfrow= c(ds,2), mar = c(1, 1, 1, 1), oma = c(4,4,2,4), cex.axis = 1.1, cex.lab = 1.1)
for (b in 1:ds){
  a = hist(flat.out[[1]]$yrs.declared.all[b,], ylim = c(0,35), breaks = c(seq(-45,95,4)), main = "", axes = T)
  lines(rep(0,2), c(0, a$counts[15] ), col =1, lty = 2.5)
  if (b == 1) { mtext(side =3, "Flatfish", outer = F, line = 0.5)}
  #axis(side = 2) ; axis(side = 1, at = seq(-2, 2, 1))
  if (b == ds) { mtext(side = 2, "Count", line = 2, outer = T) }
  print.letter(alpha.label1[b], xy = c(0.05, 0.90))

  a = hist(rock.out[[1]]$yrs.declared.all[b,], ylim = c(0,35), breaks = c(seq(-45,95,4)), main = "", axes = T)
  lines(rep(0,2), c(0, a$counts[15] ), col =1, lty = 2)
  print.letter(alpha.label2[b], xy = c(0.05, 0.90))
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
a = flat.out[[1]]$rmse.sb0[, ind]
b = flat.out[[1]]$rmse.depl[,ind]
ind = c(3, 6, 10, 14, 18, 22, 26) 
c = rock.out[[1]]$rmse.sb0[, ind]
d = rock.out[[1]]$rmse.depl[,ind]

write.table(round(a, 0) , file = "", quote = F, row.names = F, col.names = F)
write.table(round(b, 0) , file = "", quote = F, row.names = F, col.names = F)
write.table(round(c, 0) , file = "", quote = F, row.names = F, col.names = F)
write.table(round(d, 0) , file = "", quote = F, row.names = F, col.names = F)

#Median Relative Errors ============================================================================================================ 
ind = c(2, 3, 4, 5, 8, 11, 13)
a  = b = NULL
for (i in 1:length(ind)){
  a = cbind(a, apply(flat.out[[1]]$re.ssb0[,ind[i],],1,median))
  y = 50 + ind[i]*4 - 4
  b = cbind(b, apply(flat.out[[1]]$re.depl[,ind[i],y,], 1, median))
}

ind = c(3, 6, 10, 14, 18, 22, 26)  
c = d =NULL
for (i in 1:length(ind)){
  c = cbind(c, apply(rock.out[[1]]$re.ssb0[,ind[i],],1,median))
  y = 50 + ind[i]*4 - 4
  d = cbind(d, apply(rock.out[[1]]$re.depl[,ind[i],y,], 1, median))

}

write.table(round(a, 2) , file = "", quote = F, row.names = F, col.names = F)
write.table(round(b, 2) , file = "", quote = F, row.names = F, col.names = F)
write.table(round(c, 2) , file = "", quote = F, row.names = F, col.names = F)
write.table(round(d, 2) , file = "", quote = F, row.names = F, col.names = F)


#####################################################################################################################################
##                ADDITIONAL PLOTS NOT USED IN THE PAPER FOR MODEL EXPLORATION                                                  #####
#####################################################################################################################################

#Estimates of Natural Mortality
par(mfrow = c(ds,2), oma = c(4,4,2,4), mar = c(2,2,2,4))
for(b in 1:ds){
    boxplot(t(flat.out[[2]]$m.est[b,,]), ylim = c(0.15 - 0.30*0.15, 0.15 + 0.30*0.15), axes = F, col = rep('grey',ass.num))
    abline(h = 0.15,col = 1, lty =2) ; axis(side = 2); box()
    if (b == ds) { axis(side = 1); mtext(side = 1, outer = T, "Assessment Year" , line = 2)
                   mtext(side = 2, outer = T, "Natural Mortality", line = 2)}
    boxplot(t(rock.out[[2]]$m.est[b,,]), ylim = c(0.08 - 0.30*0.08, 0.08 + 0.30*0.08), axes = F, col = rep('grey',ass.num))
    abline(h = 0.08,col = 1, lty =2); axis(side = 2); box()
    if (b == ds) { axis(side = 1)}
}


#Selectivity Plots ==========================================================================================
par(mfrow = c(ds,4), oma = c(4,4,2,4), mar = c(2,2,2,4))
xfact = 0.10
for(b in 1:ds){    
  boxplot(t(flat.out[[2]]$s.selex.est[b,1,,]), ylim = c(33 - xfact*33, 33 + xfact*33))
  abline(h = 33,col = 1, lty =2)
  boxplot(t(flat.out[[2]]$s.selex.est[b,3,,]), ylim = c(4.25 - xfact*4.25, 4.25 + xfact*4.25))
  abline(h = 4.25, col = 1, lty =2)
  
  boxplot(t(flat.out[[2]]$f.selex.est[b,1,,]), ylim = c(43 - xfact*43, 43 + xfact*43))
  abline(h = 43,col = 1, lty =2)
  boxplot(t(flat.out[[2]]$f.selex.est[b,3,,]), ylim = c(4.25- xfact*4.25, 4.25 + xfact*4.25))
  abline(h = 4.25,col = 1, lty =2)
}

for(b in 1:ds){
  boxplot(t(rock.out[[2]]$s.selex.est[b,1,,]), ylim = c(39 - xfact*39, 39 + xfact*39))
  abline(h = 39,col = 1, lty =2)
  boxplot(t(rock.out[[2]]$s.selex.est[b,3,,]), ylim = c(4.25 - xfact*4.25, 4.25 + xfact*4.25))
  abline(h = 4.25, col = 1, lty =2)
  
  boxplot(t(rock.out[[2]]$f.selex.est[b,1,,]), ylim = c(45 - xfact*45, 45 + xfact*45))
  abline(h = 45,col = 1, lty =2)
  boxplot(t(rock.out[[2]]$f.selex.est[b,3,,]), ylim = c(4.25- xfact*4.25, 4.25 + xfact*4.25))
  abline(h = 4.25,col = 1, lty =2)
}



#Catches ========================================================================================================================
par(mfrow= c(ds,2), mar = c(0,0,0,0), oma = c(4,4,2,4), cex.axis = 1.1, cex.lab = 1.1)
ymax = max(flat.out[[1]]$med.acl, rock.out[[1]]$med.acl)
for(b in 1:ds){
  ind = (pre.yrs.flat + 51):(final.yr.flat-1)
  plot(ind,  flat.out[[1]]$med.acl[ds,ind,2], col = 1, 
    type = 'l', lwd = 2, ylim = c(0,ymax), axes = F)
  box() ; axis(side = 2)
  if (b == ds) { axis(side = 1)}
  lines(ind, flat.out[[1]]$med.acl[ds,ind,1], col = 1, lty = 2, lwd =2)
  lines(ind, flat.out[[1]]$med.acl[ds,ind,3], col = 1, lty = 2, lwd =2)
  lines(ind, flat.out[[1]]$med.catch.est[b,ind,2], col = 'red', lty = 1, lwd =1)

  ind = (pre.yrs.rock + 51):(final.yr.rock - 1)
  plot(ind,  rock.out[[1]]$med.acl[ds,ind,2], col = 1, 
    type = 'l', lwd =2, ylim = c(0,ymax), axes = F)
  box() 
  if (b == ds) { axis(side = 1)}
  lines(ind, rock.out[[1]]$med.acl[ds,ind,1], col = 1, lty = 2, lwd =2)
  lines(ind, rock.out[[1]]$med.acl[ds,ind,3], col = 1, lty = 2, lwd =2)
  lines(ind, rock.out[[1]]$med.catch.est[b,ind,2], col = 'red', lty = 1, lwd =1)
}

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
  boxplot(t(flat.out[[1]]$re.ssb0[b,,]), xlab= "RE SB0", ylim = c(-0.5, 0.5))
  abline(h = 0, lty = 2, col = 'red')

  boxplot(t(rock.out[[1]]$re.ssb0[b,,]), xlab= "RE SB0", ylim = c(-0.5, 0.5))
  abline(h = 0, lty = 2, col = 'red')
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
