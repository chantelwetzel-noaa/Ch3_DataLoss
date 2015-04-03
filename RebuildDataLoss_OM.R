###############################################
##      Data Loss During Rebuilding         ###
##            CHANTEL WETZEL                ###
##           Created 3/18/15                ###
##											###
##      SS3 used as the operating and		###
##			the estimation model			### 
###############################################


#source("//even_more_home//h_cwetzel//PhD//Chapter3//code//RebuildDataLoss_OM.R") 
#source("F://PhD//Chapter3//Code//RebuildDataLoss_OM.R") 
#source("C:/Users/Chantell.Wetzel/Documents/GitHub/Ch3_DataLoss/RebuildDataLoss_OM.R")

drive <-"C:" #"//home//cwetzel//h_cwetzel"
LH <- "rockfish"
start.n <- 1
end.n <- 1
data.scenario <- "ds_test" 
tantalus <- FALSE
github <- TRUE

#Load packages
require(r4ss)
require(compiler)

#Set the directory and create folder
directory <<- paste(drive,"/PhD/Chapter3/",LH, "_", data.scenario,"_sims_",start.n,"_",end.n,"/",sep="")
om  <- paste( directory, "om", sep = "")
run <- paste( directory, "run", sep = "")
if( !file.info(directory)$isdir == TRUE) {
	dir.create(directory)
	dir.create(om)
	dir.create(paste(directory,"/save",sep=""))
	dir.create(run)
}

#Move the executable to the correct folder to run simulations
if ( !tantalus ) {
	file.copy(paste(drive,"/PhD/Chapter3/ss3.exe",sep=""), om ) 
	file.copy(paste(drive,"/PhD/Chapter3/ss3.exe",sep=""), run ) }
if ( tantalus ) {
	file.copy(paste(drive,"/PhD/Chapter3/SS3",sep=""), om )
	file.copy(paste(drive,"/PhD/Chapter3/SS3",sep=""), run ) }

#Source in external functions
if ( github ) { 
 git.wd = "/Users/Chantell.Wetzel/Documents/GitHub/Ch3_DataLoss/"
 source(paste(drive, git.wd, "functions/Functions.R", sep = "")) }
if ( !github ){ source(paste(drive,"/PhD/Chapter3/code/functions/Functions.R",sep="")) }

print(LH) ; print(paste("True Depletion", final.depl,sep=" "))
print(paste("Survey Length", start.survey, sep=" "))
print(paste("Auto-Correlation", auto, sep =" "))

#Start the simulation loop
for (nsim in start.n:end.n)
 {
 	#Resource the functions
 	if (github) { 
 	  git.wd = "/Users/Chantell.Wetzel/Documents/GitHub/Ch3_DataLoss/"
 	  source(paste(drive, git.wd, "functions/Functions.R", sep = "")) }
 	if (!github){ 
 	  source(paste(drive,"/PhD/Chapter3/code/functions/Functions.R",sep="")) }

 	#nsim = 1 ; 
 	#sigmaR = 0 ; 
 	#####survey.cv = 0; 
 	tv.err = 0; ss.survey.cv = 0.50; 
 	selec.adj = 0; CV1 = CV2 <- 0.05  
 	equil = TRUE

 	# Save the run information ===========================================================================
 	capture.output(list(Survey_Start = start.survey, 
                     Overfished_Selectivity_Shift = selec.adj,
                     Annual_TimeVarying_Selectivity = tv.err,
                     Estimate_Annual_Deviations = selec.dev, 
                     Depletion_Year_50 = final.depl, 
                     Recruitment_AutoCorrelation = auto,
                     Survey_CV = survey.CV,
                     SigmaR = sigmaR,
                     Pstar = p.value, 
                     Sigma = sigma, 
                     Age_Error = AgeError),
                     file = paste(directory,'/save/Run_Details.txt',sep="")) 

 	#Save Output
 	projections <- paste(directory,"save/om_proj_",nsim,sep="")
 	estimates   <- paste(directory,"save/ss_ests_",nsim,sep="")
 	
 	#Read in the seeds 
 	load(paste(drive,"/PhD/Chapter3/seed_list",sep=""))
 	recruit.seed  <- as.numeric(seed.list[[1]][,"recruit.seed"])
 	catch.seed    <- as.numeric(seed.list[[1]][,"catch.seed"])
 	survey.seed   <- as.numeric(seed.list[[1]][,"survey.seed"])
 	comp.seed     <- as.numeric(seed.list[[1]][,"comp.seed"])  
 	age.err.seed  <- as.numeric(seed.list[[1]][,"spare1"]) 
 	select.seed   <- as.numeric(seed.list[[1]][,"spare2"])
	 
	#Catch History -----------------------------------------------------------------------------------------------------------
	set.seed(catch.seed[nsim])
	catch.dev <- c(rnorm(10,0,0.50),rnorm(30,0,0.07),rnorm(10,0,0.15))
	CatchTot <- rep(0,setup.yrs) ;  CatchTot[1] <- 25
	  
	for (y in 2:(setup.yrs-11)) { 
	   CatchTot[y]<- CatchTot[y-1]*1.4 
	       if (CatchTot[y-1]*1.4 > 1000 ) { 
	           CatchTot[y] <- 1000 }  
	}
	for (y in (setup.yrs-10):setup.yrs) { 
	   CatchTot[y]<- CatchTot[y-1]*0.97 
	}
	  
	CatchTot     <- round(CatchTot,0)    
	CatchTot.err <- round(CatchTot+CatchTot*catch.dev,0)
	hist.catch   <- c(rep(0,pre.fishery.yrs),CatchTot.err)
	 
	#Draw recruitment deviations----------------------------------------------------------------------------------------------------- 
	set.seed(recruit.seed[nsim])
	rho      <- 0
	if ( auto ) { rho <- 1 / sqrt(2) }
	recdevs  <- rnorm(total.yrs, 0, sigmaR)
	autocorr <- rep(0, total.yrs)
	autocorr[1] <- recdevs[1]  
	for (e in 2:total.yrs) { 
	   autocorr[e] <- rho*autocorr[e-1]+sqrt(1-rho*rho)*recdevs[e]  }
	
	#This will start the population in equilibrium
	if (equil) { autocorr[1:(ages-1)] <- 0 }
	
	#Draw Survey Error---------------------------------------------------------------------------------------------------------------  
	#set.seed(survey.seed[nsim])
	#survey.err <<- rnorm(fishery.yrs, 0, survey.cv)

	#Variation in Selectivity----------------------------------------------------------------------------------------------------------
	set.seed(select.seed[nsim])
	select.err   <- rnorm(total.yrs, 0, tv.err)
	inflec.selec <- numeric(total.yrs)

	# Calculate the buffer for the forecast file
	buffer <- exp(qnorm(p.value, 0, sigma))

	# Create the operating model historical population ---------------------------------------------------------------------
	setwd(om)
	y = setup.yrs + pre.fishery.yrs
	OM = TRUE
	n.devs = length(autocorr[1:y])
	write.devs = cbind(c(-1*pre.fishery.yrs:1, 0, 1:(y - pre.fishery.yrs-1)), autocorr[1:y])

	# Set up the bias adjustment parameters ----------------------------------------------------------------------------------
	main.rec.start <-  1
	main.rec.end   <-  setup.yrs            
	start.bias     <- -pre.fishery.yrs - 2
	full.bias      <- -pre.fishery.yrs - 1
	last.bias      <- setup.yrs        
	last.no.bias   <- setup.yrs
	start.devs     <- start.bias + 1
	max.bias.adj   <- 1 # full bias adjustment = ry*exp(-0.5*sigmaR^2 + recdev)


	# This is the constant added to the proportional composition data
	add.const  = 0.000000001 
	boot.files = 3 # Create a true, perfect, bootstrapped data set
    end.phase  = 1  # Only estimate a R0 value that makes the depletion survey true

    # Create the depletion survey for the data file
    fleets = "Fishery%Survey%Depl"
    n.fleet = 3
    block.num =  block.fxn = 0
    R0 = 10000
    need.blocks = FALSE
    survey = rep(5000, length(start.survey:(y-pre.fishery.yrs)))

    # Write the operating model files
    dat.file = "om.dat"; ctl.file = "om.ctl"
    do.forecast = 0
    writeStarter(starter = "starter.ss")
    writeForecast(forecast = "forecast.ss", y = y)
    writeCtl(ctl = "om.ctl", y = y)
    writeDat(dat = "om.dat", y = y, survey , fore.catch = hist.catch)
    if (tantalus)  { system("./SS3 -nohess > test.txt 2>&1")  }
    if (!tantalus) { shell("ss3.exe -nohess > test.txt 2>&1")  }
    
    #Read in the report file and save needed quantities
    rep.new   <- readLines(paste(om, "/Report.sso", sep=""))
    rep.out   <- Rep_Summary(rep.new, y, pre.fishery.yrs)
    fore.out  <- readLines(paste(om, "/Forecast-report.sso", sep=""))
    fmult     <- as.numeric(strsplit(fore.out[grep(paste("Fmult",sep=""),fore.out)]," ")[[4]][2])

    SSB[1:(y-pre.fishery.yrs)]   <- rep.out$SB
    Ry[1:(y-pre.fishery.yrs-1)]  <- rep.out$Recruits
    depl[1:(y-pre.fishery.yrs)]  <- rep.out$Depl
    R0		   <- exp(rep.out$R0)
    SB0		   <- rep.out$SB.virgin
    ofl.true[(y-pre.fishery.yrs+1):(y-pre.fishery.yrs+4)] <- rep.out$OFL
    acl.true[(y-pre.fishery.yrs+1):(y-pre.fishery.yrs+4)] <- rep.out$ACL


    #Start the projection loop
 	for (y in (pre.fishery.yrs + setup.yrs):total.yrs) {
 		#Move the needed files to the estimation area
    	file.copy("starter.ss", paste(run,"/starter.ss",sep =""))
    	file.copy("forecast.ss", paste(run,"/forecast.ss",sep =""))
    	file.copy("data.ss_new", paste(run,"/data.ss_new",sep =""))
    	OM = FALSE
    	setwd(run)
    	start.devs     <- 0
    	main.rec.start <-  1
		main.rec.end   <-  setup.yrs - 5            
		start.bias     <-  1
		full.bias      <-  30
		last.bias      <- y - ages - 4      
		last.no.bias   <- y - ages - 3
		max.bias.adj   <- 0.90 # full bias adjustment = ry*exp(-0.5*sigmaR^2 + recdev)
		n.devs = 0
    	writeCtl(ctl = "est.ctl", y = y)
    	#Split the data file and modify
		SS_splitdat(inpath = run, outpath = run,
    	        inname="data.ss_new", outpattern=paste("boot",nsim,"_",y,sep=""),number=F,verbose=T,fillblank=T,MLE=F)
    	dat <- NULL
    	dat <- SS_readdat(paste(run,"/boot",nsim,".ss",sep=""))
    	dat$Nsurveys <- 1 
    	dat$fleetnames <- c("Fishery", "Survey")
    	dat$areas <- c(1,1)
    	dat$surveytiming <- c(0.5, 0.5) 
    	dat$N_cpue <- length(survey)
    	dat$CPUEinfo <- dat$CPUEinfo[1:2,]
    	dat$CPUE <- dat$CPUE[1:length(survey),]
    	SS_writedat(datlist=dat,outfile=paste(run,"/boot",nsim,"_",y,".ss",sep=""),overwrite=TRUE,verbose=TRUE)
	
    	#Modify the starter file
    	starter<-SS_readstarter(file=paste(run,"/starter.ss",sep=""))
    	starter$datfile<-paste("boot",nsim,"_",y,".ss", sep ="")
    	starter$ctlfile<-"est.ctl"
    	starter$last_estimation_phase <- 10
    	SS_writestarter(starter,dir=paste(run,"/",sep=""),file="starter.ss", overwrite=TRUE,verbose=TRUE)
	
    	#Run SS
    	if (tantalus)  { system("./SS3  > test.txt 2>&1")  }
    	if (!tantalus) { shell("ss3.exe > test.txt 2>&1")  }
	
    	#Rename the ctl and data files
    	file.rename(paste(om,"/om.ctl",sep =""), 
    	                   paste(om,"/om",nsim,"_",y-pre.fishery.yrs,".ctl",sep =""))      
    	file.rename(paste(om,"/om.ctl",sep =""), 
    	                   paste(om,"/om",nsim,"_",y-pre.fishery.yrs,".ctl",sep =""))  
	
    	#rename files
    	#split new dat files
    	#change the control file to remove the rec devs and remove the depletion survey
    	#edit the data file to remove the depletion survey and add the added constant
    	#move to the main folder for estimation
	

 	} #end projection loop

    