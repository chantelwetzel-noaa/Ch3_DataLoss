###############################################################################################
#                                       Data Scenarios                                        #
#                                                                                             #
#   ds0 = this is the test sceneario with high sample size with data starting in year 1 of    #
#   of the model.  This also estimates blocks with annual selectivity deviations
#
#   ds1 = Base data scenario where sample sizes do not change during rebuilding and the model
#   is allowed to estimate blocks and annual selectivity deviations
#
#   ds2 = Data levels do not change during rebuilding, deviation in selectivity and
#   natural mortality.
#
#   ds3 = Fishery data levels are reduced during rebuilding period, deviation in selectivity
#	and natural mortality
#
#   ds4 = Data levels are reduced during the rebuilding period, no time varying parameters
#   
#   ds5 = There are no additional data beyond year 50 for the fishey.  
#	Blocks and annual deviations for selectivity are not estimated
#   Time varying parameters allowed
#   
#   ds6 = There are no additional data beyond year 50 for the fishey.  
#	Blocks and annual deviations for selectivity are not estimated
#   No time varying parameters 
#
###############################################################################################

sigma     <- 0.36
p.value   <- 0.45
auto      <- FALSE
selec.adj <- 0
dome.adj  <- -8.5

ass.freq  <- 6

#Adjust the max age value that would be used if ageing error is included
max.age = ages - 1

sigmaR 	     <- 0.50  
ss.survey.cv <- survey.cv <- 0.40 
cpue.cv      <- 0.30
hi.cpue.cv   <- 0.30 #0.50
select.sd 	 <- 0.05  #Selectivity Time Varying StDev
dome.sd      <- 0.20
m.sd <- k.sd <- 0.20
age.error    <- 0
if (AgeError == TRUE) { age.error    <- 0.10 }


if (data.scenario == "ds0" || data.scenario == "ds1" || data.scenario == "ds4" || data.scenario == 'ds6' || data.scenario == 'ds8'){
	select.sd <- 0
	m.sd      <- 0 
	dome.sd   <- 0 
	selec.adj <- 0
}

pre.fishery.yrs <- ages - 1 
project.yrs <- 102 
fishery.yrs <- setup.yrs + project.yrs + 1
total.yrs   <- pre.fishery.yrs + fishery.yrs

n.yrs = ifelse(do.survey == T, 11, 2)
start.survey  <- ages + setup.yrs - n.yrs #21# 11 #21 
start.fishery <- ages + setup.yrs - 15 #25
start.cpue    <- ages + setup.yrs - 5 #2

years       <- 1:total.yrs 
ass.num     <- ceiling(project.yrs/ ass.freq) +1

estimate.m = TRUE

if (data.scenario == "ds0") { start.survey = ages + 1; start.fishery = ages +1  }

#Determine when the data begins
start.fishery.len.samp <- start.fishery
start.fishery.age.samp <- start.fishery
start.survey.len.samp  <- start.survey 
start.survey.age.samp  <- start.survey 

#Data Available Based on Scenario
#N.f.len = 75 ; N.s.len = 10 ; N.f.age = 25 ; N.s.age = 10 
#N.f.len = 30 ; N.s.len = 5 ; N.f.age = 20 ; N.s.age = 5 #ass freq and fixed M
N.f.len = 50 ; N.s.len = 10 ; N.f.age = 25 ; N.s.age = 10 #Ass Freq new Ns
#N.f.len = 100 ; N.s.len = 10 ; N.f.age = 50 ; N.s.age = 10 

if (data.scenario == "ds0") { 
    N.f.len = 500 ; N.s.len = 500 ; N.f.age = 500 ; N.s.age = 500 }

data.yrs   <- start.fishery.len.samp : total.yrs
f.len.samp <- c(rep(0,start.fishery - 1),rep(N.f.len,length(data.yrs)))

data.yrs   <- start.survey.len.samp : total.yrs
s.len.samp <- c(rep(0,start.survey - 1),rep(N.s.len,length(data.yrs)))

data.yrs   <- start.fishery.age.samp : total.yrs
f.age.samp <- c(rep(0,start.fishery - 1),rep(N.f.age,length(data.yrs)))

data.yrs   <- start.survey.age.samp : total.yrs
s.age.samp <- c(rep(0,start.survey - 1),rep(N.s.age,length(data.yrs)))