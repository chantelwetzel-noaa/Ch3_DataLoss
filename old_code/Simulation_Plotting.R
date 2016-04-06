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
run.name = ""

load(paste0(drive,"/PhD/Chapter3/output/rockfish_meds_split"))
med.rock <- meds.split
load(paste0(drive,"/PhD/Chapter3/output/rockfish_est_split"))
est.rock <- est.split
load(paste0(drive,"/PhD/Chapter3/output/rockfish_om_split"))
om.rock <- om.split
#load(paste0(drive,"/PhD/Chapter3/output/flatfish_meds_split"))
#med.flat <- meds.split
#load(paste0(drive,"/PhD/Chapter3/output/flatfish_est_split"))
#est.flat <- est.split
#load(paste0(drive,"/PhD/Chapter3/output/flatfish_om_split"))
#om.flat <- om.split

#load(paste(drive,":/PhD/Chapter3/", run.name, "/output/rockfish_meds_all", sep = ""))
#med.rock <- meds.all
#load(paste(drive,":/PhD/Chapter3/", run.name, "/output/rockfish_est_all", sep = ""))
#est.rock <- est.all
#load(paste(drive,":/PhD/Chapter3/", run.name, "/output/rockfish_om_all", sep = ""))
#om.rock <- om.all
#load(paste(drive,":/PhD/Chapter3/", run.name, "/output/flatfish_meds_all", sep = ""))
#med.flat <- meds.all
#load(paste(drive,":/PhD/Chapter3/", run.name, "/output/flatfish_est_all", sep = ""))
#est.flat <- est.all
#load(paste(drive,":/PhD/Chapter3/", run.name, "/output/flatfish_om_all", sep = ""))
#om.flat <- om.all

rock.out <- flat.out <- list()
rock.out[[1]] <- med.rock
rock.out[[2]] <- est.rock
rock.out[[3]] <- om.rock
#flat.out[[1]] <- med.flat
#flat.out[[2]] <- est.flat
#flat.out[[3]] <- om.flat
#
#==================================================================================================================
print.letter <- function(label="(a)",xy=c(0.1,0.925),...) {   #... means any new parameters are copied faithfully to text() below
  tmp <- par("usr")
  text.x <- tmp[1]+xy[1]*diff(tmp[1:2])   #x position, diff=difference
  text.y <- tmp[3]+xy[2]*diff(tmp[3:4])   #y position
  text(x=text.x, y=text.y, labels=label, ...)             
}

alpha.label1 = c('(a)', '(c)', '(e)', '(g)')
alpha.label2 = c('(b)', '(d)', '(f)', '(h)')
alpha.label2 = c('a) data all, no tv', 'b) reduce data, no tv', 'c) data all, tv', 'd) reduce data, tv')
name.label = c('Great All', 'Great Historical', 'Normal')

library(RColorBrewer)

#Dimensions used for plotting======================================================================================
ds =  dim(rock.out[[2]]$ssb.est)[1] #Determines the number of data scenarios to plot
hist.yrs = 50
flat.yrs = 50
rock.yrs = 100
ass.yr1.flat = 90 ; ass.yr1.rock = 120
pre.yrs.flat = 41
pre.yrs.rock = 71
final.yr.flat = flat.yrs + hist.yrs + pre.yrs.flat  #flat.yrs + 50 + pre.yrs.flat
final.yr.rock = rock.yrs + hist.yrs + pre.yrs.rock - 1  #rock.yrs + 50 + pre.yrs.rock - 1
#ass.num = dim(flat.out[[2]]$ssb.est)[3]
ass.yr = seq(hist.yrs,hist.yrs + 100,4) #seq(50,150,4)

ass.yrs.flat = seq(ass.yr1.flat, ass.yr1.flat + flat.yrs,4)
ass.yrs.rock = seq(ass.yr1.rock, ass.yr1.rock + rock.yrs,4)

#N.flat = dim(om.flat$ssb)[3]
N.rock = dim(om.rock$ssb)[3] 

#Create Plots to compare results==================================================================
windows(10, 10, record = TRUE)

#Spawning Biomass
par(mfrow= c(2,2), mar = c(0.1,0.1,0.1,0.1), oma = c(4,4,2,4), cex.axis = 1.1, cex.lab = 1.1)
start.plot.flat = 85#135 #85#75#82
start.plot.rock = 105#155 #105#112
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
#trans.color = c(rgb(0, 0, 1, trans), rgb(0, 1, 0, 0.25), rgb(1,0,0, 0.3))

#c(rgb(153/a, 0, 0), rgb(255/a, 128/a, 0), rgb(0, 153/a, 0), "Blue")
#colorRampPalette(brewer.pal(10, "Blues"))(10)
lty.type = c(4, 3, 2, 5, 1)
#ymax = ceiling( max(flat.out[[1]]$med.ssb[,start.plot.flat:final.yr.flat,], rock.out[[1]]$med.ssb[,start.plot.rock:final.yr.rock,]))
ymax = ceiling( max(rock.out[[1]]$med.ssb[,start.plot.rock:final.yr.rock,]))

for(b in 1:ds){
  #Plot flatfish
  #plot(start.plot.flat:final.yr.flat,  flat.out[[1]]$med.ssb[b,start.plot.flat:final.yr.flat,2], col = rgb(1,1,1,1), type = 'l', lwd =2,
  #   ylim = c(0,ymax), ylab = "SB", xlab = "Year", axes = F, xaxs="i")
  ##Calculate the % determined to be overfished
  #per.of = round(flat.out[[1]]$n.overfished[b,sort(ass.index.flat)]/N.flat,2)
  ##xx = c(start.plot.flat:final.yr.flat,rev(start.plot.flat:final.yr.flat))
  ##yy = c(flat.out[[1]]$med.ssb[b,start.plot.flat:final.yr.flat,1],rev(flat.out[[1]]$med.ssb[b,start.plot.flat:final.yr.flat,3])) 
  ##polygon(xx, yy, col=color, lty=1, border = 'grey')
  ##lines(start.plot.flat:final.yr.flat,  flat.out[[1]]$med.ssb[b,start.plot.flat:final.yr.flat,2], col = rgb(1,1,1,1), lty = 1, lwd =4)
  #lines(start.plot.flat:final.yr.flat,  flat.out[[1]]$med.ssb[b,start.plot.flat:final.yr.flat,1], col = 'darkgrey', lty = 4, lwd =1)
  #lines(start.plot.flat:final.yr.flat,  flat.out[[1]]$med.ssb[b,start.plot.flat:final.yr.flat,3], col = 'darkgrey', lty = 4, lwd =1)
  #box() ; axis(side = 2)
  #print.letter(alpha.label1[b], xy = c(0.05, 0.95))
  #text( labels = per.of, x = text.yr.flat, y = rep(10, length(text.yr.flat)), cex = 1.2 ) 
#
  #if(b == 1)  {  mtext("Flatfish", side = 2, outer = T, line = 2.5) }
  #if(b == ds) { 
  #  mtext("Spawning Biomass", side = 2, outer = T, line = 2.5)
  #  axis(side = 1 , at =seq(130, 190,10), labels = seq(90, 150, 10))
  #  #axis(side = 1 , at =seq(80, 140,10), labels = seq(40, 100, 10))
  #}

  #for (i in 1:length(ass.index.flat)){
  #  temp = ass.index.flat[i]*4 - 5 + hist.yrs + pre.yrs.flat
  #  y = (start.plot.flat):temp
  #  ind =(start.plot.flat):temp
  #  xx = c(y, rev(y))
  #  yy = c(flat.out[[1]]$med.ssb.est[b,ind,ass.index.flat[i],1], rev(flat.out[[1]]$med.ssb.est[b,ind,ass.index.flat[i],3]))
  #  polygon(xx, yy, col = trans.color[i], border = "NA")
  #}
  #lines(start.plot.flat:final.yr.flat,  flat.out[[1]]$med.ssb[b,start.plot.flat:final.yr.flat,2], col = 'grey70', lty = 1, lwd = 3)
  #for (i in 1:length(ass.index.flat))
  #{
  #  temp = ass.index.flat[i]*4 - 5 + hist.yrs + pre.yrs.flat
  #  y = (start.plot.flat):temp
  #  ind =(start.plot.flat):temp
  #  lines(y, flat.out[[1]]$med.ssb.est[b,ind,ass.index.flat[i],2], col = plot.color[i], lty = lty.type[i], lwd = 2)
  #  #lines(y, flat.out[[1]]$med.ssb.est[b,ind,ass.index.flat[i],1], col = plot.color[i], lty = lty.type[i], lwd = 1)
  #  #lines(y, flat.out[[1]]$med.ssb.est[b,ind,ass.index.flat[i],3], col = plot.color[i], lty = lty.type[i], lwd = 1)
  #}
  
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
  per.of = round(rock.out[[1]]$n.overfished[b,sort(ass.index.rock)]/N.rock,2)
  #xx = c(start.plot.rock:final.yr.rock,rev(start.plot.rock:final.yr.rock))
  #yy = c(rock.out[[1]]$med.ssb[b,start.plot.rock:final.yr.rock,1],rev(rock.out[[1]]$med.ssb[b,start.plot.rock:final.yr.rock,3])) 
  #polygon(xx, yy, col=color, lty=0)
  #lines(start.plot.rock:final.yr.rock,  rock.out[[1]]$med.ssb[b,start.plot.rock:final.yr.rock,2], col = rgb(1,1,1,1), lty = 1, lwd =4)
  lines(start.plot.rock:final.yr.rock,  rock.out[[1]]$med.ssb[b,start.plot.rock:final.yr.rock,1], col = 'darkgrey', lty = 4, lwd =1)
  lines(start.plot.rock:final.yr.rock,  rock.out[[1]]$med.ssb[b,start.plot.rock:final.yr.rock,3], col = 'darkgrey', lty = 4, lwd =1)
  print.letter(alpha.label2[b], xy = c(0.15, 0.95), cex = 1.1)
  text(labels = per.of, x = text.yr.rock, y = rep(10, length(text.yr.rock)), cex = 1.2 ) 

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
    #lines(y, rock.out[[1]]$med.ssb.est[b,ind,ass.index.rock[i],1], col = plot.color[i], lty = lty.type[i], lwd =1)
    #lines(y, rock.out[[1]]$med.ssb.est[b,ind,ass.index.rock[i],3], col = plot.color[i], lty = lty.type[i], lwd =1)
  }
}


#Working====================================================================================================================================
#Spawning Biomass
par(mfrow= c(2,2), mar = c(0.1,1.2,0.1,1.2), oma = c(4,4,4,4), cex.axis = 1.1, cex.lab = 1.1)
final.yr.flat = 125#160 #110
final.yr.rock = 195#246 #196
start.plot.flat = 85#115 #85#75#82
start.plot.rock = 105#155 #105#112
color <- rgb(0, 0, 0, 0.30) 
ass.index.flat = rev(c(1, 3, 5, 9)) #rev(c(1,  5, 10, 14))#rev(c(1, 7, 14))
ass.index.rock = rev(c(1, 8, 13, 20, 26))#rev(c(1, 10, 20, 26))#rev(c(1, 13, 26))
text.yr.flat = sort((pre.yrs.flat + ass.yr)[ass.index.flat]) - 2
text.yr.rock = sort((pre.yrs.rock + ass.yr)[ass.index.rock]) - 5
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
  #plot(start.plot.flat:final.yr.flat,  flat.out[[1]]$med.ssb[b,start.plot.flat:final.yr.flat,2], col = rgb(1,1,1,1), type = 'l', lwd =2,
  #   ylim = c(0,ymax.flat), ylab = "SB", xlab = "Year", axes = F, xaxs="i")
  #lines(start.plot.flat:final.yr.flat,  flat.out[[1]]$med.ssb[b,start.plot.flat:final.yr.flat,1], col = 'darkgrey', lty = 4, lwd =1)
  #lines(start.plot.flat:final.yr.flat,  flat.out[[1]]$med.ssb[b,start.plot.flat:final.yr.flat,3], col = 'darkgrey', lty = 4, lwd =1)
  #box() ; axis(side = 2)
  #print.letter(alpha.label1[b], xy = c(0.05, 0.95))
  #per.of = round(flat.out[[1]]$n.overfished[b,sort(ass.index.flat)]/N.flat,2)
  #text( labels = per.of, x = text.yr.flat, y = rep(10, length(text.yr.flat)), cex = 1.2 )
  #if(b == 1)  {  mtext("Flatfish", side = 3, outer = F, line = 1) }
  #if(b == ds) { 
  #  mtext("Spawning Biomass", side = 2, outer = T, line = 2.5) 
  #  axis(side = 1 , at =seq(90, 140, 5), labels = seq(50, 100, 5))
  #  #axis(side = 1 , at =seq(80, 140, 5), labels = seq(40, 100, 5))
  #}

  #for (i in 1:length(ass.index.flat)){
  #  temp = ass.index.flat[i]*4 - 5 + hist.yrs +pre.yrs.flat
  #  y = (start.plot.flat):(temp )
  #  ind =(start.plot.flat):temp
  #  xx = c(y, rev(y))
  #  yy = c(flat.out[[1]]$med.ssb.est[b,ind,ass.index.flat[i],1], rev(flat.out[[1]]$med.ssb.est[b,ind,ass.index.flat[i],3]))
  #  polygon(xx, yy, col = trans.color[i], border = "NA")
  #}
  #lines(start.plot.flat:final.yr.flat,  flat.out[[1]]$med.ssb[b,start.plot.flat:final.yr.flat,2], col = line.color, lty = 1, lwd = 3)
  #for (i in 1:length(ass.index.flat))
  #{
  #  temp = ass.index.flat[i]*4 - 5 + hist.yrs +pre.yrs.flat
  #  y = (start.plot.flat):(temp)
  #  ind =(start.plot.flat):temp
  #  lines(y, flat.out[[1]]$med.ssb.est[b,ind,ass.index.flat[i],2], col = plot.color[i], lty = lty.type[i], lwd = 2)
  #}
  
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
  print.letter(alpha.label2[b], xy = c(0.17, 0.95))
  per.of = round(rock.out[[1]]$n.overfished[b,sort(ass.index.rock)]/N.rock,2)
  text( labels = per.of, x = text.yr.rock, y = rep(10, length(text.yr.rock)), cex = 1.2 )
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
par(mfrow= c(2,2), mar = c(0.1,0.1,0.1,0.1), oma = c(4,4,2,4), cex.axis = 1.1, cex.lab = 1.1)
ymax = 0.75
for(b in 1:ds){
  #Plot flatfish
  #plot(start.plot.flat:final.yr.flat,  flat.out[[1]]$med.depl[b,start.plot.flat:final.yr.flat,2], col = rgb(1,1,1,1), type = 'l', lwd =2,
  #   ylim = c(0,ymax), ylab = "SB", xlab = "Year", axes = F, xaxs="i")
  #lines(start.plot.flat:final.yr.flat,  flat.out[[1]]$med.depl[b,start.plot.flat:final.yr.flat,1], col = 'darkgrey', lty = 4, lwd =1)
  #lines(start.plot.flat:final.yr.flat,  flat.out[[1]]$med.depl[b,start.plot.flat:final.yr.flat,3], col = 'darkgrey', lty = 4, lwd =1)
  #box() ; axis(side = 2)
  #abline(h = 0.25, lty = 3, col = 'red')
  #print.letter(alpha.label1[b], xy = c(0.05, 0.95))
  #per.of = round(flat.out[[1]]$n.overfished[b,sort(ass.index.flat)]/N.flat,2)
  #text( labels = per.of, x = text.yr.flat, y = rep(0, length(text.yr.flat)), cex = 1.2 )
  #if(b ==1) { mtext(side = 3, "Flatfish", outer = F, line = 1) }
  #if(b == ds) { mtext("Depletion", side = 2, outer = T, line = 2.5)
  #  axis(side = 1 , at =seq(80, 140, 5), labels = seq(40, 100, 5)) }
  #  #axis(side = 1 , at =seq(130, 190, 5), labels = seq(90, 150, 5)) }

  #for (i in 1:length(ass.index.flat))
  #{
  #  temp = ass.index.flat[i]*4 - 5 + hist.yrs + pre.yrs.flat
  #  y = (start.plot.flat):(temp)
  #  ind =(start.plot.flat):temp
  #  xx = c(y, rev(y))
  #  yy = c(flat.out[[1]]$med.depl.est[b,ind,ass.index.flat[i],1], rev(flat.out[[1]]$med.depl.est[b,ind,ass.index.flat[i],3]))
  #  polygon(xx, yy, col = trans.color[i], lty = 0)
  #}
  #lines(start.plot.flat:final.yr.flat,  flat.out[[1]]$med.depl[b,start.plot.flat:final.yr.flat,2], col = line.color, lty = 1, lwd =3)
  #for (i in 1:length(ass.index.flat))
  #{
  #  temp = ass.index.flat[i]*4 - 5 + hist.yrs + pre.yrs.flat
  #  y = (start.plot.flat):(temp)
  #  ind =(start.plot.flat):temp
  #  lines(y, flat.out[[1]]$med.depl.est[b,ind,ass.index.flat[i],2], col = plot.color[i], lty = lty.type[i], lwd =2)
  #  #lines(y, flat.out[[1]]$med.depl.est[b,ind,ass.index.flat[i],1], col = plot.color[i], lty = lty.type[i], lwd =1)
  #  #lines(y, flat.out[[1]]$med.depl.est[b,ind,ass.index.flat[i],3], col = plot.color[i], lty = lty.type[i], lwd =1)
  #}

  #Plot rockfish
  plot(start.plot.rock:final.yr.rock,  rock.out[[1]]$med.depl[b,start.plot.rock:final.yr.rock,2], col = 1, type = 'l', lwd =2, 
    ylim = c(0,ymax), ylab = "Depletion", xlab = "Year",  axes = F, xaxs="i")
  box() ; if(b == ds) { 
      axis(side = 1, at =seq(120, 220,10), labels = seq(50, 150, 10))
      #axis(side = 1, at =seq(170, 270,10), labels = seq(100, 200, 10))
      mtext("Year", side = 1, outer = T, line = 2.5)}
  if(b ==1) { mtext(side = 3, "Rockfish", outer = F, line = 1) }
  #xx = c(start.plot.rock:final.yr.rock,rev(start.plot.rock:final.yr.rock))
  #yy = c(rock.out[[1]]$med.depl[b,start.plot.rock:final.yr.rock,1],rev(rock.out[[1]]$med.depl[b,start.plot.rock:final.yr.rock,3])) 
  #polygon(xx, yy, col=color, lty=0)
  #lines(start.plot.rock:final.yr.rock,  rock.out[[1]]$med.depl[b,start.plot.rock:final.yr.rock,2], col = rgb(1,1,1,1), lty = 1, lwd =4)
  lines(start.plot.rock:final.yr.rock,  rock.out[[1]]$med.depl[b,start.plot.rock:final.yr.rock,1], col ='darkgrey', lty = 4, lwd = 1)
  lines(start.plot.rock:final.yr.rock,  rock.out[[1]]$med.depl[b,start.plot.rock:final.yr.rock,3], col ='darkgrey', lty = 4, lwd = 1)
  abline(h = 0.40, lty = 3, col = 'red')
  print.letter(alpha.label2[b], xy = c(0.05, 0.95))
  per.of = round(rock.out[[1]]$n.overfished[b,sort(ass.index.rock)]/N.rock,2)
  text( labels = per.of, x = text.yr.rock, y = rep(0, length(text.yr.rock)), cex = 1.2 )

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
    #lines(y, rock.out[[1]]$med.depl.est[b,ind,ass.index.rock[i],1], col = plot.color[i], lty = lty.type[i], lwd =1)
    #lines(y, rock.out[[1]]$med.depl.est[b,ind,ass.index.rock[i],3], col = plot.color[i], lty = lty.type[i], lwd =1)
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
  #plot(1:26, transform[3,], type = 'l', col = 4, lwd = 2)
  #lines(1:26, om.trans[3,], lty = 2, col = 4, lwd = 2)
  #lines(1:26, transform[4,],lty = 1, col = 5, lwd = 2)
  #lines(1:26, om.trans[4,], lty = 2, col = 5, lwd = 2)

  boxplot(t(re.time.over)) ; abline(h = 0, lty = 1)


#Plot the relative error ===============================================================================
par(mfrow=c(2,2), mar = c(2,2,3,3))

#Relative Error about depletion
max = min = 1
for (a in 1:ds){
  #ass.num = 14
  #re.depl = matrix(0, ass.num, 100)
  #for (b in 1:ass.num){
  #  ind = b*4 - 5+ hist.yrs + pre.yrs.flat 
  #  re.depl[b,] = flat.out[[1]]$re.depl[a,b,ind,]
  #}
  #boxplot(t(re.depl), ylim = c(-min, max), col = rep('grey',ass.num), axes = F)
  #box(); abline(h = 0)
  #axis(side = 2); axis(side = 1, at = seq(1, 14, 2), labels = seq(hist.yrs, hist.yrs + 50, 8))
  #print.letter(alpha.label1[a], xy = c(0.04, 0.95))
  #if(a == 1) { 
  #  mtext(side = 3, outer = F, "Flatfish", line = 1)
  #  mtext(side = 2, outer = T, "Relative Error: Relative Stock Status", line = 2)
  #  mtext(side = 1, outer = T, "Assessment Year", line = 2)
  #}

  ass.num = 26
  re.depl = matrix(0, ass.num, 100)
  for (b in 1:ass.num){
    ind = b*4 - 5+ hist.yrs + pre.yrs.rock
    re.depl[b,] = rock.out[[1]]$re.depl[a,b,ind,]
  }
  boxplot(t(re.depl), ylim = c(-min, max), col = rep('grey',ass.num), axes= F)
  box(); abline(h = 0)
  axis(side = 2); axis(side = 1, at = seq(1,26,3), labels = seq(hist.yrs, hist.yrs + 100, 12))
  print.letter(alpha.label2[a], xy = c(0.04, 0.95))
  if(a == 1) { 
    mtext(side = 3, outer = F, "Rockfish", line = 1)
  } 
}

#Relative Error about final spawning biomass
for (a in 1:ds){
  #ass.num = 14
  #re.ssb = matrix(0, ass.num, 100)
  #for (b in 1:ass.num){
  #  ind = b*4 - 5+ hist.yrs + pre.yrs.flat
  #  re.ssb[b,] = flat.out[[1]]$re.ssb[a,b,ind,]
  #}
  #boxplot(t(re.ssb), ylim = c(-min, max), col = rep('grey',ass.num), axes = F)
  #box(); abline(h = 0)
  #axis(side = 2); axis(side = 1, at = seq(1, 14, 2), labels = seq(hist.yrs, hist.yrs + 50, 8))
  #print.letter(alpha.label1[a], xy = c(0.04, 0.95))
  #if(a == 1) { 
  #  mtext(side = 3, outer = F, "Flatfish", line = 1)
  #  mtext(side = 2, outer = T, "Relative Error: Spawning Biomass", line = 2)
  #  mtext(side = 1, outer = T, "Assessment Year", line = 2)
  #}
#
  ass.num = 26
  re.ssb = matrix(0, ass.num, 100)
  for (b in 1:ass.num){
    ind = b*4 - 5+ hist.yrs + pre.yrs.rock
    re.ssb[b,] = rock.out[[1]]$re.ssb[a,b,ind,]
  }
  boxplot(t(re.ssb), ylim = c(-min, max), col = rep('grey',ass.num), axes= F)
  box(); abline(h = 0)
  axis(side = 2); axis(side = 1, at = seq(1,26,3), labels = seq(hist.yrs, hist.yrs + 100, 12))
  print.letter(alpha.label2[a], xy = c(0.94, 0.95))
  if(a == 1) { 
    mtext(side = 3, outer = F, "Rockfish")
  } 
}

#Relative Error about virgin spawning biomass
for (a in 1:ds){
  #ass.num = 14
  #re.ssb0 = matrix(0, ass.num, 100)
  #for (b in 1:ass.num){
  #  re.ssb0[b,] = flat.out[[1]]$re.ssb[a,b,1,]
  #}
  #boxplot(t(re.ssb0), ylim = c(-min, max), col = rep('grey',ass.num), axes = F)
  #box(); abline(h = 0)
  #axis(side = 2); axis(side = 1, at = seq(1, 14, 2), labels = seq(hist.yrs, hist.yrs + 50, 8))
  #print.letter(alpha.label1[a], xy = c(0.04, 0.95))
  #if(a == 1) { 
  #  mtext(side = 3, outer = F, "Flatfish", line = 1)
  #  mtext(side = 2, outer = T, "Relative Error: Virgin Spawning Biomass", line = 2)
  #  mtext(side = 1, outer = T, "Assessment Year", line = 2)
  #}

  ass.num = 26
  re.ssb0 = matrix(0, ass.num, 100)
  for (b in 1:ass.num){
    re.ssb0[b,] = rock.out[[1]]$re.ssb[a,b,1,]
  }
  boxplot(t(re.ssb0), ylim = c(-0.5, 0.50), col = rep('grey',ass.num), axes= F)
  box(); abline(h = 0)
  axis(side = 2); axis(side = 1, at = seq(1,26,3), labels = seq(hist.yrs, hist.yrs + 100, 12))
  print.letter(alpha.label2[a], xy = c(0.04, 0.95))
  if(a == 1) { 
    mtext(side = 3, outer = F, "Rockfish", line = 1)
  } 
}



#Relative Error about Natural Mortality ================================================
par(mfrow= c(2,2), mar = c(0.1,0.1,0.1,0.1), oma = c(4,4,2,4), cex.axis = 1.1, cex.lab = 1.1)
ymin = -0.30 ; ymax = 0.45
ind = 1:14
for(b in 1:ds){
  #boxplot(t(flat.out[[1]]$re.m[b,ind,]), ylab = "", ylim = c(ymin,ymax), col = rep('grey',ass.num), axes = F)
  #abline(h = 0, lty = 2, col = 1); box(); axis(side =2) 
  #if (b == 1) { mtext(side =3, "Flatfish", outer = F, line = 0.5)}
  if (b == ds) { axis(side =1, at = seq(1,max(ind),2), labels = seq(hist.yrs, hist.yrs + 4 * max(ind) - 4, 8))
       mtext(side = 2, "RE Natural Mortality", outer =T, line = 2.5) }
  #print.letter(alpha.label1[b], xy = c(0.04, 0.95))
  boxplot(t(rock.out[[1]]$re.m[b,,]), ylab = "RE M", ylim = c(ymin,ymax), col = rep('grey',ass.num), axes = F)
  abline(h = 0, lty = 2, col = 1); box()
  if (b == 1) { mtext(side =3, "Rockfish", outer = F, line = 0.5)}
  print.letter(alpha.label2[b], xy = c(0.04, 0.95))
  if (b == ds) { axis(side =1, at = seq(1,26,3), labels = seq(hist.yrs, hist.yrs + 100, 12)); mtext(side = 1, "Assessment Year", outer = T, line = 2.5) }
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

temp = rock.out[[2]]$time.over - rock.out[[3]]$om.time.over
ind = temp[1,] > -101
a = hist(temp[1,ind], ylim = c(0,35), breaks = c(seq(-65, 60, 5)))
boxplot(t(rock.out[[1]]$re.time.over), ylim = c(-1,1.25)); abline(h=0)

par(mfrow= c(ds,2), mar = c(1, 1, 1, 1), oma = c(4,4,2,4), cex.axis = 1.1, cex.lab = 1.1)
for (b in 1:ds){
  #a = hist(flat.out[[1]]$yrs.declared.all[b,]/26.5, ylim = c(0,35), breaks = c(seq(-1.55,2.15,0.15)), main = "", axes = T)
  #lines(rep(0,2), c(0, a$counts[11] ), col =1, lty = 2.5)
  #if (b == 1) { mtext(side =3, "Flatfish", outer = F, line = 0.5)}
  ##axis(side = 2) ; axis(side = 1, at = seq(-2, 2, 1))
  #if (b == ds) { mtext(side = 2, "Count", line = 2, outer = T) }
  #print.letter(alpha.label1[b], xy = c(0.05, 0.90))

  a = hist(rock.out[[1]]$yrs.declared.all[b,]/48, ylim = c(0,35), breaks = c(seq(-1.55,2.15,0.15)), main = "", axes = T)
  lines(rep(0,2), c(0, a$counts[11] ), col =1, lty = 2)
  print.letter(alpha.label2[b], xy = c(0.05, 0.90))
  #axis(side = 1, at = seq(-2, 3, 1))
  #axis(side = 1, at = seq(-2, 3, 1));
  if (b == 1) { mtext(side =3, "Rockfish", outer = F, line = 0.5)}
  if (b == ds) {  mtext(side = 1, "Relative Years Declared Recovered", line = 2.5, outer = T)}
}

par(mfrow= c(ds,2), mar = c(1, 1, 1, 1), oma = c(4,4,2,4), cex.axis = 1.1, cex.lab = 1.1)
for (b in 1:ds){
  #a = hist(flat.out[[1]]$yrs.declared.all[b,], ylim = c(0,35), breaks = c(seq(-54,95,4)), main = "", axes = T)
  #lines(rep(0,2), c(0, a$counts[14] ), col =1, lty = 2.5)
  #if (b == 1) { mtext(side =3, "Flatfish", outer = F, line = 0.5)}
  ##axis(side = 2) ; axis(side = 1, at = seq(-2, 2, 1))
  #if (b == ds) { mtext(side = 2, "Count", line = 2, outer = T) }
  #print.letter(alpha.label1[b], xy = c(0.05, 0.90))

  a = hist(rock.out[[1]]$yrs.declared.all[b,], ylim = c(0,35), breaks = c(seq(-61,95,4)), main = "", axes = T)
  lines(rep(0,2), c(0, a$counts[14] ), col =1, lty = 2)
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

#Relative error of spawning biomass and depletion


#Flatfish
ass.index = c(ass.index.flat, 10, 14)
grey = rgb(0,0,0,0.10)
par(mfrow =c (ds,length(ass.index)), oma = c(4,4,2,4), mar = c(2,2,2,2))
for (b in 1:ds){
  for (a in 1:length(ass.index)){
    ind = sort(ass.index)
    temp = 70:(90 + ind[a]* 4 - 4)
    med.re.ssb = apply(flat.out[[1]]$re.ssb[b, ind[a], temp,], 1, quantile, c(0.975, 0.50, 0.25))
    plot( temp, med.re.ssb[2,], type = 'l', lwd =2, ylim = c( -0.50, 1.25), axes = F)# xlim = c(1, max(50 + ass.index.flat* 4 - 4)) )
    xx = c(temp, rev(temp)); yy = c(med.re.ssb[1,], rev(med.re.ssb[3,]))
    polygon(xx, yy, col = grey, border = NA)
    lines(temp, med.re.ssb[1,], lty = 2)
    lines(temp, med.re.ssb[3,], lty = 2)
    #boxplot(t(flat.out[[1]]$re.ssb[b, ind[a], temp, ]), ylim = c(-1, 1), axes = F, xlim = c(1, max(50 + ass.index.flat* 4 - 4)))
    box(); abline (h = 0, lty =1); abline(v = 90, lty = 2)
    if (a == 1) { axis(side = 2); mtext(side = 2, "Relative Error Spawning Biomass", outer = T, line = 2.5) }
    if (b == ds){ axis(side = 1); mtext(side = 1, "Year", outer = T, line = 2.5) }
  }  
}

#Rockfish
ass.index = c(ass.index.rock, 18)
par(mfrow =c (ds,length(ass.index)), oma = c(4,4,2,4), mar = c(0.2,0.2,0.2,0.2))
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


#Flatfish
ass.index = c(ass.index.flat, 10)
grey = rgb(0,0,0,0.10)
par(mfrow =c (ds,length(ass.index)), oma = c(4,4,2,4), mar = c(0.2,0.2,0.2,0.2))
for (b in 1:ds){
  for (a in 1:length(ass.index)){
    ind = sort(ass.index)
    temp = 1:(50 + ind[a]* 4 - 4)
    med.re.depl = apply(flat.out[[1]]$re.depl[b, ind[a], temp,], 1, quantile, c(0.975, 0.50, 0.25))
    plot( temp, med.re.depl[2,], type = 'l', lwd =2, ylim = c( -0.25, 1), axes = F) #, xlim = c(1, max(50 + ass.index* 4 - 4))
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

#Rockfish
ass.index = c(ass.index.rock,18, 26)
par(mfrow =c (ds,length(ass.index)), oma = c(4,4,2,4), mar = c(0.2,0.2,0.2,0.2))
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
par(mfrow = c(ds,2), oma = c(4,4,2,4), mar = c(2,2,2,4))
for(b in 1:ds){
    #boxplot(t(flat.out[[2]]$m.est[b,,]), ylim = c(0.15 - 0.30*0.15, 0.15 + 0.30*0.15), axes = F, col = rep('grey',ass.num))
    #abline(h = 0.15,col = 1, lty =2) ; axis(side = 2); box()
    if (b == ds) { axis(side = 1); mtext(side = 1, outer = T, "Assessment Year" , line = 2)
                   mtext(side = 2, outer = T, "Natural Mortality", line = 2)}
    boxplot(t(rock.out[[2]]$m.est[b,,]), ylim = c(0.08 - 0.30*0.08, 0.08 + 0.30*0.08), axes = F, col = rep('grey',ass.num))
    abline(h = 0.08,col = 1, lty =2); axis(side = 2); box()
    if (b == ds) { axis(side = 1)}
}


#Selectivity Plots ==========================================================================================
par(mfrow = c(ds,4), oma = c(4,4,2,4), mar = c(2,2,2,4))

for(b in 1:ds){  
  xfact = 0.10  
  boxplot(t(flat.out[[2]]$s.selex.est[b,1,,]), ylim = c(33 - xfact*33, 33 + xfact*33))
  abline(h = 33,col = 1, lty =2)
  boxplot(t(flat.out[[2]]$s.selex.est[b,3,,]), ylim = c(4.25 - xfact*4.25, 4.25 + xfact*4.25))
  abline(h = 4.25, col = 1, lty =2)
  
  xfact = 0.05
  boxplot(t(flat.out[[2]]$f.selex.est[b,1,,]), ylim = c(43 - xfact*43, 43 + xfact*43))
  abline(h = 43,col = 1, lty =2)
  boxplot(t(flat.out[[2]]$f.selex.est[b,3,,]), ylim = c(4.25- xfact*4.25, 4.25 + xfact*4.25))
  abline(h = 4.25,col = 1, lty =2)
}


for(b in 1:ds){
  xfact = 0.10
  boxplot(t(rock.out[[2]]$s.selex.est[b,1,,]), ylim = c(39 - xfact*39, 39 + xfact*39))
  abline(h = 39,col = 1, lty =2)
  boxplot(t(rock.out[[2]]$s.selex.est[b,3,,]), ylim = c(4.25 - xfact*4.25, 4.25 + xfact*4.25))
  abline(h = 4.25, col = 1, lty =2)
  
  xfact = 0.05
  boxplot(t(rock.out[[2]]$f.selex.est[b,1,,]), ylim = c(45 - xfact*45, 45 + xfact*45))
  abline(h = 45,col = 1, lty =2)
  boxplot(t(rock.out[[2]]$f.selex.est[b,3,,]), ylim = c(4.25- xfact*4.25, 4.25 + xfact*4.25))
  abline(h = 4.25,col = 1, lty =2)
}

#Relative Error for Selectivity ================================================================================================
par(mfrow = c(ds,5), oma = c(4,4,2,4), mar = c(0,0,0,0))

for(b in 1:ds){  
  boxplot(t(flat.out[[1]]$re.s.selex[b,1,,]), ylim =   c(-0.15, 0.15), axes = F)
  abline(h = 0,col = 1, lty =2); box(); axis(side = 2 )
  if(b == ds) {  axis(side =1, at = 1:14, labels = seq(50, 102,4))
     mtext(side = 2, outer = T, "Selecitivity", line = 2.5 ) }
  boxplot(t(flat.out[[1]]$re.s.selex[b,2,,]), ylim =   c(-0.15, 0.15), axes = F)
  abline(h = 0, col = 1, lty =2); box()
  if(b == ds) { axis(side =1, at = 1:14, labels = seq(50, 102,4)) }
  
  xfact = 0.05
  boxplot(t(flat.out[[1]]$re.f.selex[b,1,,]), ylim =   c(-0.15, 0.15), axes = F)
  abline(h = 0,col = 1, lty =2); box()
  if(b == ds) { axis(side =1, at = 1:14, labels = seq(50, 102,4)) }
  boxplot(t(flat.out[[1]]$re.f.selex[b,2,,]), ylim =   c(-0.15, 0.15), axes = F)
  abline(h = 0,col = 1, lty =2); box()
  if(b == ds) { axis(side =1, at = 1:14, labels = seq(50, 102,4)) }
  boxplot(t(flat.out[[1]]$re.f.selex.adj[b,,]), ylim = c(-0.15, 0.15), axes = F)
  abline(h = 0,col = 1, lty =2); box()
  if(b == ds) { axis(side =1, at = 1:14, labels = seq(50, 102,4))
     mtext(side = 1, outer = T, "Assessment Year", line = 2.5) }
}


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
  boxplot(t(rock.out[[1]]$re.f.selex.adj[b,,]), ylim =  c(-0.1, 0.1), axes = F)
  abline(h = 0,col = 1, lty =2); box()
  if(b == ds) { axis(side =1, at = 1:26, labels = seq(50,150,4)) 
    mtext(side = 1, outer = T, "Assessment Year", line = 2.5) }
}

#Catches ========================================================================================================================
par(mfrow= c(ds,2), mar = c(0,0,0,0), oma = c(4,4,2,4), cex.axis = 1.1, cex.lab = 1.1)
ymax = max(flat.out[[1]]$med.acl, rock.out[[1]]$med.acl)
for(b in 1:ds){
  #ind = (pre.yrs.flat + 51):(final.yr.flat-1)
  #plot(ind,  flat.out[[1]]$med.acl[ds,ind,2], col = 1, 
  #  type = 'l', lwd = 2, ylim = c(0,ymax), axes = F)
  #box() ; axis(side = 2)
  #if (b == ds) { axis(side = 1)}
  #lines(ind, flat.out[[1]]$med.acl[ds,ind,1], col = 1, lty = 2, lwd =2)
  #lines(ind, flat.out[[1]]$med.acl[ds,ind,3], col = 1, lty = 2, lwd =2)
  #lines(ind, flat.out[[1]]$med.catch.est[b,ind,2], col = 'red', lty = 1, lwd =1)

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
