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
#   ds5 = There are no additional data beyond year 50.  Blocks and annual deviations for 
#   selectivity are not estimated
#
###############################################################################################

sigma <- 0.36
p.value <- 0.45
auto <- FALSE
selec.adj <- 5


AgeError <- FALSE 
#Adjust the max age value that would be used if ageing error is included
max.age = ages - 1
if(AgeError == TRUE) { max.age  <- ages + 4  }  

sigmaR 	     <- 0.60  
ss.survey.cv <- 0.50 
survey.cv 	 <- 0.50 
select.sd 	 <- 0.05  #Selectivity Time Varying StDev
m.sd 		 <- 0.05

if (data.scenario == "ds0" || data.scenario == "ds1" || data.scenario == "ds4"){
	select.sd <- 0
	m.sd      <- 0  }

if (data.scenario == "ds6" || data.scenario == "ds7"){
	auto <- TRUE
}

if (data.scenario == "ds_survey"){
	ss.survey.cv = survey.cv = 0.20
}

pre.fishery.yrs <- ages - 1 
#setup.yrs   <- 100 #100
project.yrs <- 100 #76
if (LH == "flatfish") { project.yrs = 52 }
fishery.yrs <- setup.yrs + project.yrs + 1
total.yrs   <- pre.fishery.yrs + fishery.yrs

start.survey <- ages + setup.yrs - 15 #106 #36

years       <- 1:total.yrs 
ass.num     <- (project.yrs / 4) + 1

estimate.m = TRUE

if (data.scenario == "ds0") { start.survey = ages }

#Determine when the data begins
start.fishery.len.samp <- start.survey #pre.fishery.yrs + start.survey
start.fishery.age.samp <- start.survey #pre.fishery.yrs + start.survey
start.survey.len.samp  <- start.survey #pre.fishery.yrs + start.survey
start.survey.age.samp  <- start.survey #pre.fishery.yrs + start.survey

#Data Available Based on Scenario
N.f.len = 75 ; N.s.len = 10 ; N.f.age = 25 ; N.s.age = 10 
if (data.scenario == "ds0") { 
    N.f.len = 500 ; N.s.len = 500 ; N.f.age = 500 ; N.s.age = 500 }

data.yrs   <- start.fishery.len.samp : total.yrs
f.len.samp <- c(rep(0,start.survey - 1),rep(N.f.len,length(data.yrs)))

#data.yrs   <- seq(start.survey.len.samp, total.yrs, 3) 
data.yrs   <- start.survey.len.samp : total.yrs
s.len.samp <- c(rep(0,start.survey - 1),rep(N.s.len,length(data.yrs)))
#s.len.samp <- numeric(total.yrs)
#s.len.samp[data.yrs] <- N.s.len

data.yrs   <- start.fishery.age.samp : total.yrs
f.age.samp <- c(rep(0,start.survey - 1),rep(N.f.age,length(data.yrs)))

#data.yrs   <- seq(start.survey.age.samp, total.yrs, 3)
data.yrs   <- start.survey.age.samp : total.yrs
s.age.samp <- c(rep(0,start.survey - 1),rep(N.s.age,length(data.yrs)))
#s.age.samp <- numeric(total.yrs)
#s.age.samp[data.yrs] <- N.s.age
