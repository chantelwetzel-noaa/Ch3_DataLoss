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

#Adjust the max age value that would be used if ageing error is included
max.age = ages - 1

sigmaR 	     <- 0.60  
ss.survey.cv <- 0.50 
survey.cv 	 <- 0.50
ss.survey.cv <- survey.cv <- 0.40 
select.sd 	 <- 0.05  #Selectivity Time Varying StDev
dome.sd      <- 0.20
m.sd <- k.sd <- 0.20

if (data.scenario == "ds0" || data.scenario == "ds1" || data.scenario == "ds4" || data.scenario == 'ds6'){
	select.sd <- 0
	m.sd      <- 0 
	dome.sd   <- 0 
}

pre.fishery.yrs <- ages - 1 
project.yrs <- 100 
if (LH == "flatfish") { project.yrs = 52 }
fishery.yrs <- setup.yrs + project.yrs + 1
total.yrs   <- pre.fishery.yrs + fishery.yrs

start.survey  <- ages + setup.yrs - 21 
start.fishery <- ages + setup.yrs - 25

years       <- 1:total.yrs 
ass.num     <- (project.yrs)/ 4 + 1

estimate.m = TRUE

if (data.scenario == "ds0") { start.survey = ages + 1; start.fishery = ages +1  }

#Determine when the data begins
start.fishery.len.samp <- start.fishery
start.fishery.age.samp <- start.fishery 
start.survey.len.samp  <- start.survey 
start.survey.age.samp  <- start.survey 

#Data Available Based on Scenario
#N.f.len = 75 ; N.s.len = 10 ; N.f.age = 25 ; N.s.age = 10 

N.f.len = 100 ; N.s.len = 10 ; N.f.age = 50 ; N.s.age = 10 
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


