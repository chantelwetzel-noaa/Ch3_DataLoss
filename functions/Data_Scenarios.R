###############################################################################################
#                                       Data Scenarios                                        #
#                                                                                             #
#   ds0 = this is the test sceneario with high sample size with data starting in year 1 of    #
#   of the model.  This also estimates blocks with annual selectivity deviations
#
#   ds1 = Base data scenario where sample sizes do not change during rebuilding and the model
#   is allowed to estimate blocks and annual selectivity deviations
#
#   ds2 = Data levels do not change during rebuilding, blocks are estimated, but no annual
#   selectivity deviations are estimated
#
#   ds3 = Data levels are reduced during the rebuilding period, blocks are estimated, but 
#   no annual selectivity deviations are estimated
#
#   ds4 = There are no additional data beyond year 50.  Blocks and annual deviations for 
#   selectivity are not estimated
#
#   ds5 = This is no composition scenario where the only data are the survey indices
#
###############################################################################################

sigma <- 0.36
p.value <- 0.45
auto <- FALSE
selec.adj <- 5
start.survey <- 106 #36
AgeError <- FALSE 
#Adjust the max age value that would be used if ageing error is included
max.age = ages - 1
if(AgeError == TRUE) { max.age  <- ages + 4  }  

survey.CV <- 0.50 
tv.err    <- 0    #Time Varying StDev

pre.fishery.yrs <- ages - 1 
setup.yrs   <- 50
project.yrs <- 76
if (LH == "flatfish") { project.yrs = 52 }
fishery.yrs <- setup.yrs + project.yrs + 1
total.yrs   <- pre.fishery.yrs + fishery.yrs

years       <- 1:total.yrs #fishery.yrs 
#years      <- c(seq(-pre.fishery.yrs,-1,1),seq(1,fishery.yrs,1)) 
ass.num     <- (project.yrs / 4) + 1

estimate.m = TRUE
if (data.scenario == "ds3") { estimate.m = FALSE }

#if (data.scenario == "ds0") { start.survey = 1 }
if (data.scenario == "ds0") { start.survey = 71 }

#Determine when the data begins
start.fishery.len.samp <- start.survey #pre.fishery.yrs + start.survey
start.fishery.age.samp <- start.survey #pre.fishery.yrs + start.survey
start.survey.len.samp  <- start.survey #pre.fishery.yrs + start.survey
start.survey.age.samp  <- start.survey #pre.fishery.yrs + start.survey

#Data Available Based on Scenario
N.f.len = 100 ; N.s.len = 10 ; N.f.age = 100 ; N.s.age = 10 
if (data.scenario == "ds0") { 
    N.f.len = 500 ; N.s.len = 500 ; N.f.age = 500 ; N.s.age = 500 }

data.yrs   <- start.fishery.len.samp : total.yrs
f.len.samp <- c(rep(0,start.survey - 1),rep(N.f.len,length(data.yrs)))

data.yrs   <- start.survey.len.samp : total.yrs
s.len.samp <- c(rep(0,start.survey - 1),rep(N.s.len,length(data.yrs)))

data.yrs   <- start.fishery.age.samp : total.yrs
f.age.samp <- c(rep(0,start.survey - 1),rep(N.f.age,length(data.yrs)))

data.yrs   <- start.survey.age.samp : total.yrs
s.age.samp <- c(rep(0,start.survey - 1),rep(N.s.len,length(data.yrs)))

#Estimate Annual Deviations for fishery selectivity
selec.dev = 0
dev.yr1   = 0

#if (data.scenario == "ds0" || data.scenario == "ds1"){
#    selec.dev = 2
#    dev.yr1 = start.survey
#    #the dev.yr2 variable is dynamically resent in the projection period
#}



