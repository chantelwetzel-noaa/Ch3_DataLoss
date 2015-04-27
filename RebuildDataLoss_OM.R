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

drive <-"E:" #"//home//cwetzel//h_cwetzel"
LH <- "rockfish"
start.n <- 1
end.n <- 1
data.scenario <- "ds0" 
tantalus <- FALSE
github <- TRUE


#Load packages
require(r4ss)
require(compiler)

#Set the directory and create folder
directory <<- paste(drive,"/PhD/Chapter3/",LH, "_", data.scenario,"_sims_",start.n,"_",end.n,"/",sep="")
om  <- paste( directory, "om", sep = "")
run <- paste( directory, "run", sep = "")
if( file.exists(directory) == FALSE) {
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
 source(paste("C:", git.wd, "functions/Functions.R", sep = "")) }
if ( !github ){ source(paste(drive,"/PhD/Chapter3/code/functions/Functions.R",sep="")) }

print(LH) ; print(paste("True Depletion", final.depl,sep=" "))
print(paste("Survey Length", start.survey, sep=" "))
print(paste("Auto-Correlation", auto, sep =" "))

#Start the simulation loop
for (nsim in start.n:end.n)
 {
 	Proj <- Est       <- list()
 	#Resource the functions
 	if (github) { 
 	  git.wd = "/Users/Chantell.Wetzel/Documents/GitHub/Ch3_DataLoss/"
 	  source(paste("C:", git.wd, "functions/Functions.R", sep = "")) }
 	if (!github){ 
 	  source(paste(drive,"/PhD/Chapter3/code/functions/Functions.R",sep="")) }

 	#nsim = 1 ; 
 	sigmaR = 0.60 ; 
 	survey.cv = 0.25; 
 	ss.survey.cv = 0.25; 
 	#selec.adj = 0; CV1 = CV2 <- 0.05  
 	equil = TRUE
    pre.dev.phase = ifelse(equil == TRUE, -3, 4)
    determ = ifelse(sigmaR == 0, TRUE, FALSE)
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
	catch   <- c(rep(0,pre.fishery.yrs),CatchTot.err)
	 
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
	#if (equil) { autocorr[1:(ages-1)] <- 0 }
    if (equil) { autocorr[1:(ages)] <- 0 }
	
	#Draw Survey Error---------------------------------------------------------------------------------------------------------------  
	set.seed(survey.seed[nsim])
	survey.err <- rnorm(fishery.yrs, 0, survey.cv)

	#Variation in Selectivity----------------------------------------------------------------------------------------------------------
	set.seed(select.seed[nsim])
	select.err   <- rnorm(total.yrs, 0, tv.err)
	inflec.selec <- numeric(total.yrs)

	# Calculate the buffer for the forecast file
	buffer <- exp(qnorm(p.value, 0, sigma))

	# Create the operating model historical population ---------------------------------------------------------------------
	setwd(om)
	y = setup.yrs + pre.fishery.yrs + 1
	OM = TRUE
    fix.q = ifelse(OM ==TRUE, 2, 0)
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
	max.bias.adj   <- 0 #when no devs this results in no correction #1 # full bias adjustment = ry*exp(-0.5*sigmaR^2 + recdev)


	# This is the constant added to the proportional composition data
	add.const  = 0
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
    get.forecast = FALSE
    writeStarter(starter = "starter.ss")
    writeForecast(forecast = "forecast.ss", y = y)
    writeCtl(ctl = "om.ctl", y = y)
    writeDat(dat = "om.dat", y = y, survey , fore.catch = catch)
    if (tantalus)  { system("./SS3 -nohess > test.txt 2>&1")  }
    if (!tantalus) { shell("ss3.exe -nohess > test.txt 2>&1")  }
    #Save the original model files
    file.copy(paste(om,"/om.ctl",sep =""), 
    		            paste(om,"/om",nsim,"_",y-pre.fishery.yrs,".ctl",sep =""), overwrite = TRUE)      
    file.copy(paste(om,"/om.dat",sep =""), 
    		            paste(om,"/om",nsim,"_",y-pre.fishery.yrs,".dat",sep =""), overwrite = TRUE) 
    file.copy(paste(om,"/data.ss_new",sep =""), 
                        paste(om,"/data",nsim,"_",y-pre.fishery.yrs,".ss_new",sep =""), overwrite = TRUE) 
    file.copy(paste(om,"/Report.sso",sep =""), 
                        paste(om,"/Report_depl",nsim,"_",y-pre.fishery.yrs,".sso",sep =""), overwrite = TRUE) 

    #Rerun the model with forecast turned on with no estimation
    #This will produce the true ofl and acls for the next four years
    rep.new   <- readLines(paste(om, "/Report.sso", sep=""))
    R0 <- exp(as.numeric(strsplit(rep.new[grep(paste("SR_LN",sep=""),rep.new)]," ")[[1]][3]))
    #Remove the depletion survey
    dat <- NULL
    dat <- SS_readdat(paste(om,"/om.dat",sep=""))
    dat$Nsurveys <- 1 
    dat$fleetnames <- c("Fishery", "Survey")
    dat$areas <- c(1,1)
    dat$surveytiming <- c(0.5, 0.5) 
    dat$catch[,1] <- catch[(pre.fishery.yrs +1):y]
    dat$N_cpue <- length(survey)
    dat$CPUEinfo <- dat$CPUEinfo[1:2,]
    dat$CPUE <- dat$CPUE[1:length(survey),]
    dat$add_to_comp <- 0
    SS_writedat(datlist=dat,outfile=paste(om,"/om.dat",sep=""),overwrite=TRUE,verbose=TRUE)
    #Change the final phase for estimation
    starter <- SS_readstarter(file=paste(om,"/starter.ss",sep=""))
    starter$last_estimation_phase <- 0
    SS_writestarter(starter,dir=paste(om,"/",sep=""),file="starter.ss", overwrite=TRUE,verbose=TRUE)
    #Remove the depletion survey from the control file and set the new estimated R0 value
    get.forecast = TRUE
    OM = FALSE
    writeCtl(ctl = "om.ctl", y = y)
    do.forecast = 1
    writeForecast(forecast = "forecast.ss", y = y)
    if (tantalus)  { system("./SS3 -nohess > test.txt 2>&1")  }
    if (!tantalus) { shell("ss3.exe -nohess > test.txt 2>&1")  }
    file.rename(paste(om,"/om.ctl",sep =""), 
    		            paste(om,"/om_fore_",nsim,"_",y-pre.fishery.yrs,".ctl",sep =""))      
    file.rename(paste(om,"/om.dat",sep =""), 
    		            paste(om,"/om_fore_",nsim,"_",y-pre.fishery.yrs,".dat",sep =""))
    file.copy(paste(om,"/Report.sso",sep =""), 
                        paste(om,"/Report_",nsim,"_",y-pre.fishery.yrs,".sso",sep =""))
    
    #Read in the report file and save needed quantities
    rep.new   <- readLines(paste(om, "/Report.sso", sep=""))
    rep.out   <- Rep_Summary(rep.new, y, pre.fishery.yrs, do.forecast)
    fore.out  <- readLines(paste(om, "/Forecast-report.sso", sep=""))
    fmult     <- as.numeric(strsplit(fore.out[grep(paste("Fmult",sep=""),fore.out)]," ")[[4]][2])

    SSB[1:(y-pre.fishery.yrs)]   <- rep.out$SB
    Ry[1:(y-pre.fishery.yrs-1)]  <- rep.out$Recruits
    depl[1:(y-pre.fishery.yrs)]  <- rep.out$Depl
    R0		   <- exp(rep.out$R0)
    SB0		   <- rep.out$SB.virgin
    ofl.true[(y-pre.fishery.yrs+1):(y-pre.fishery.yrs+4)] <- rep.out$OFL
    acl.true[(y-pre.fishery.yrs+1):(y-pre.fishery.yrs+4)] <- rep.out$ACL

    decl.overfished = FALSE
    counter = 0

    ###################################################################################################################
    #Start the projection loop
    ###################################################################################################################
 	for (y in (pre.fishery.yrs + setup.yrs):total.yrs) {
 		
 		do.ass = y
    	if(LH == "flatfish") { do.ass = y - 2} 

    	##################################################################
 		#Operating Model
 		##################################################################
 		if (do.ass %% 4 == 0 & counter > 0){
 			# do not have a depl survey
 			# fix the R0 at the estimated initial R0
 			# check how the samples sizes are set during this period to ensure reduction
 			setwd(om)
 			OM = TRUE
            fix.q = ifelse(OM ==TRUE, 2, 0)
			n.devs = length(autocorr[1:y])
			write.devs = cbind(c(-1*pre.fishery.yrs:1, 0, 1:(y - pre.fishery.yrs-1)), autocorr[1:y])
			survey = rep(5000, length(start.survey:(y-pre.fishery.yrs)))
		
			# Set up the bias adjustment parameters ----------------------------------------------------------------------------------
			main.rec.start <-  1
			main.rec.end   <-  setup.yrs            
			start.bias     <- -pre.fishery.yrs - 2
			full.bias      <- -pre.fishery.yrs - 1
			last.bias      <- setup.yrs        
			last.no.bias   <- setup.yrs
			start.devs     <- start.bias + 1
			max.bias.adj   <- 0 #when no devs this results in no correction #1 # full bias adjustment = ry*exp(-0.5*sigmaR^2 + recdev)
				
			# This is the constant added to the proportional composition data
			add.const  = 0

    		writeDat(dat = "om.dat", y = y, survey , fore.catch = catch)
    		data = NULL
    		dat <- SS_readdat(paste(om,"/om.dat",sep=""))
    		dat$Nsurveys <- 1 
    		dat$fleetnames <- c("Fishery", "Survey")
    		dat$areas <- c(1,1)
    		dat$surveytiming <- c(0.5, 0.5) 
    		dat$N_cpue <- length(survey)
    		dat$CPUEinfo <- dat$CPUEinfo[1:2,]
    		dat$CPUE <- dat$CPUE[1:length(survey),]
    		SS_writedat(datlist=dat, outfile=paste(om,"/om.dat",sep=""),overwrite=TRUE,verbose=TRUE)

 			starter <- SS_readstarter(file=paste(om,"/starter.ss",sep=""))
    		starter$last_estimation_phase <- 0
    		SS_writestarter(starter,dir=paste(om,"/",sep=""),file="starter.ss", overwrite=TRUE,verbose=TRUE)
    		#Remove the depletion survey from the control file and set the new estimated R0 value
    		OM = FALSE ; get.forecast = TRUE
 			writeCtl(ctl = "om.ctl", y = y)
    		do.forecast = 1
    		writeForecast(forecast = "forecast.ss", y = y)
    		if (tantalus)  { system("./SS3 -nohess > test.txt 2>&1")  }
    		if (!tantalus) { shell("ss3.exe -nohess > test.txt 2>&1")  }

    		rep.new   <- readLines(paste(om, "/Report.sso", sep=""))
    		rep.out   <- Rep_Summary(rep.new, y, pre.fishery.yrs, do.forecast)
    		fore.out  <- readLines(paste(om, "/Forecast-report.sso", sep=""))
    		fmult     <- as.numeric(strsplit(fore.out[grep(paste("Fmult",sep=""),fore.out)]," ")[[4]][2])
		
    		SSB[1:(y-pre.fishery.yrs)]   <- rep.out$SB
    		Ry[1:(y-pre.fishery.yrs-1)]  <- rep.out$Recruits
    		depl[1:(y-pre.fishery.yrs)]  <- rep.out$Depl
    		ofl.true[(y-pre.fishery.yrs+1):(y-pre.fishery.yrs+4)] <- rep.out$OFL
    		acl.true[(y-pre.fishery.yrs+1):(y-pre.fishery.yrs+4)] <- rep.out$ACL

            file.rename(paste(om,"/Report.sso",sep =""), 
                        paste(om,"/Report",nsim,"_",y-pre.fishery.yrs,".sso",sep ="")) 
            file.copy(paste(om,"/data.ss_new",sep =""), 
                        paste(om,"/data",nsim,"_",y-pre.fishery.yrs,".ss_new",sep =""), overwrite = TRUE) 
            file.copy(paste(om,"/om.dat",sep =""), 
                        paste(om,"/om",nsim,"_",y-pre.fishery.yrs,".dat",sep =""), overwrite = TRUE)  
            file.copy(paste(om,"/om.ctl",sep =""), 
                        paste(om,"/om",nsim,"_",y-pre.fishery.yrs,".ctl",sep =""), overwrite = TRUE) 
 		}

 		#################################################################
    	#Estimation Model
 		################################################################
    	if ( do.ass %% 4 == 0 ){
       		counter = counter + 1
 			#Move the needed files to the estimation area
    		file.copy("starter.ss", paste(run,"/starter.ss",sep =""), overwrite=T)
    		file.copy("data.ss_new", paste(run,"/data.ss_new",sep =""), overwrite=T)

    		OM = FALSE
            fix.q = ifelse(OM ==TRUE, 2, 0)
    		setwd(run)
    		if (counter != 1) {
    			start.bias   <- start.bias.est 
        		full.bias    <- full.bias.est  
				last.bias    <- y - pre.fishery.yrs - 7
        		last.no.bias <- last.bias + 2
        		main.rec.end <- last.bias - 1 
        		max.bias.adj <- max.bias.adj.est
                print(c(main.rec.end, start.bias, full.bias, last.bias, last.no.bias, max.bias.adj))
        	}

    		if ( counter == 1){
    			start.devs     <- start.survey - ages
    			main.rec.start <- 1          
				start.bias     <- 1
				full.bias      <- 30
				last.bias      <- y - pre.fishery.yrs - 7
        		last.no.bias   <- last.bias + 2
        		main.rec.end   <- last.bias - 1 
				max.bias.adj   <- 0.90 # full bias adjustment = ry*exp(-0.5*sigmaR^2 + recdev)
    		  
            }

			n.devs = 0
			get.forecast = FALSE
			do.forecast = 1 #This switches on and off the forecast
    		writeForecast(forecast = "forecast.ss", y = y)
    		writeCtl(ctl = "est.ctl", y = y)
    		#Split the data file and modify
			SS_splitdat(inpath = om, outpath = run,
    		        inname="data.ss_new", outpattern=paste("boot",nsim,"_",y-pre.fishery.yrs,sep=""),number=F,verbose=T,fillblank=T,MLE=F)
			if (counter == 1){
				dat <- NULL
    			dat <- SS_readdat(paste(run,"/boot",nsim,"_", y-pre.fishery.yrs,".ss",sep=""))
    			dat$Nsurveys <- 1 
    			dat$fleetnames <- c("Fishery", "Survey")
    			dat$areas <- c(1,1)
    			dat$surveytiming <- c(0.5, 0.5) 
    			dat$catch[,1] <- catch[(pre.fishery.yrs +1):y]
    			dat$N_cpue <- length(survey)
    			dat$CPUEinfo <- dat$CPUEinfo[1:2,]
    			dat$CPUE <- dat$CPUE[1:length(survey),]
    			dat$add_to_comp <- 0.00001
			}
    		if (counter != 1){
    			dat.new <- dat.old <- dat <- NULL
    			dat.new <- SS_readdat(paste(run,"/boot",nsim,"_", y-pre.fishery.yrs,".ss",sep=""))
    			dat.old <- SS_readdat(paste(run,"/boot",nsim,"_", y-pre.fishery.yrs-4,".ss",sep=""))
    			dat.old$endyr <- dat.new$endyr
    			dat.old$N_catch <- dat.new$N_catch
    			dat.old$catch <- cbind(catch[(pre.fishery.yrs+1):y], dat.new$catch[,2:3])
    			dat.old$N_cpue <- dat.new$N_cpue
    			ind = (dim(dat.new$CPUE)[1]-3):(dim(dat.new$CPUE)[1])
    			dat.old$CPUE <- rbind(dat.old$CPUE, dat.new$CPUE[ind,])
    			dat$add_to_comp <- 0.00001
    			dat.old$N_lencomp <- dat.new$N_lencomp
    			ind = dat.new$lencomp$FltSvy == 1 ; ind.old.1 = dat.old$lencomp$FltSvy == 1
    			ind1 = (sum(ind)-3):sum(ind)
    			ind = dat.new$lencomp$FltSvy == 2 ; ind.old.2 = dat.old$lencomp$FltSvy == 2
    			ind2 = (length(ind)-3):length(ind)
    			dat.old$lencomp <- rbind(dat.old$lencomp[ind.old.1,], dat.new$lencomp[ind1,], dat.old$lencomp[ind.old.2,], dat.new$lencomp[ind2,])
    			dat.old$N_agecomp <- dat.new$N_agecomp
				ind = dat.new$agecomp$FltSvy == 1 ; ind.old.1 = dat.old$agecomp$FltSvy == 1
    			ind1 = (sum(ind)-3):sum(ind)
    			ind = dat.new$agecomp$FltSvy == 2 ; ind.old.2 = dat.old$agecomp$FltSvy == 2
    			ind2 = (length(ind)-3):length(ind)
    			dat.old$agecomp <- rbind(dat.old$agecomp[ind.old.1,], dat.new$agecomp[ind1,], dat.old$agecomp[ind.old.2,], dat.new$agecomp[ind2,])
    			dat = dat.old
    		}
    		SS_writedat(datlist=dat,outfile=paste(run,"/boot",nsim,"_",y-pre.fishery.yrs,".ss",sep=""),overwrite=TRUE,verbose=TRUE)
		
    		#Modify the starter file
    		starter<-SS_readstarter(file=paste(run,"/starter.ss",sep=""))
    		starter$datfile<-paste("boot",nsim,"_",y-pre.fishery.yrs,".ss", sep ="")
    		starter$ctlfile<-"est.ctl"
    		starter$last_estimation_phase <- 10
    		SS_writestarter(starter,dir=paste(run,"/",sep=""),file="starter.ss", overwrite=TRUE,verbose=TRUE)
		
    		#Run SS
    		if (y <= (pre.fishery.yrs + setup.yrs + 9)){
    			if (tantalus)  { system("./SS3  > test.txt 2>&1")  }
    			if (!tantalus) { shell("ss3.exe > test.txt 2>&1")  }
    		}

    		if (y > (pre.fishery.yrs + setup.yrs + 9)){
    			if (tantalus)  { system("./SS3 -nohess > test.txt 2>&1")  }
    			if (!tantalus) { shell("ss3.exe -nohess > test.txt 2>&1")  }
    		}
	
    		#Make sure the model converged
    		rep.new   <- readLines(paste(run, "/Report.sso", sep=""))
        	virgin.SB <- as.numeric(strsplit(rep.new[grep(paste("SPB_Virgin",sep=""),rep.new)]," ")[[1]][3])
        	while(virgin.SB < (SB0/4) || virgin.SB > (SB0*4)) {
        	  rerun = rerun + 1  
        	  starter.file = SS_readstarter(paste(directory, "starter.ss", sep = ""))
        	  starter.file$jitter_fraction = 0.10
        	  SS_writestarter(starter.file, paste(directory, sep = ""), overwrite = T )
        	  if (y <= (pre.fishery.yrs + setup.yrs + 9)){
             	if (tantalus == T) { system("./SS3 > test.txt 2>&1")  }
        	  	if (tantalus == F) { shell("ss3.exe > test.txt 2>&1")  }
        	  }
        	  if (y > (pre.fishery.yrs + setup.yrs + 9)){
        	  	if (tantalus == T) { system("./SS3 -nohess > test.txt 2>&1")  }
        	  	if (tantalus == F) { shell("ss3.exe -nohess > test.txt 2>&1")  }
        	  }
        	  rep.new   <- readLines(paste(directory, "Report.sso", sep=""))
        	  virgin.SB <- as.numeric(strsplit(rep.new[grep(paste("SPB_Virgin",sep=""),rep.new)]," ")[[1]][3])
        	  if (virgin.SB > (SB0/4) && virgin.SB < (SB0*4)) {
        	    break()
        	  }
        	  if(rerun > 10) { break () }
        	}
	
			if (determ == FALSE & y <= (pre.fishery.yrs + setup.yrs + 9)){
    			#Apply the bias correction
    			rep.bias     <- SS_output(run, covar = TRUE, printstats = FALSE)
        		new.bias     <- SS_fitbiasramp(rep.bias, 
        		                    startvalues = c(start.bias, full.bias , last.bias, last.no.bias ,max.bias.adj))
        		start.bias   <- start.bias.est <- new.bias$df[1,1]
        		full.bias    <- full.bias.est  <- new.bias$df[2,1]
        		#last.bias    <- last.bias.est  <- new.bias$df[3,1]
        		last.bias    <- y - pre.fishery.yrs - 7
        		last.no.bias <- last.bias + 2
        		#last.no.bias <- last.no.bias.est<-new.bias$df[4,1]
        		max.bias.adj <- max.bias.adj.est <-new.bias$df[5,1]
        		main.rec.end <- main.rec.end.est <-last.bias - 1
		
        		#Rewrite the control file with the new bias adjustment values
        		writeCtl(ctl = "est.ctl", y = y)
        		#Rerun the model with the new bias values
        		if (tantalus)  { system("./SS3 -nohess > test.txt 2>&1")  }
        		if (!tantalus) { shell("ss3.exe -nohess > test.txt 2>&1")  }
        	}
		
			#Read the report file and save values
			rep.new   <- readLines(paste(run, "/Report.sso", sep=""))
			rep.out   <- Rep_Summary(rep.new, y, pre.fishery.yrs, do.forecast)
			fore.out  <- readLines(paste(run, "/Forecast-report.sso", sep=""))
        	fmult     <- as.numeric(strsplit(fore.out[grep(paste("Fmult",sep=""),fore.out)]," ")[[4]][2])
        	ind       <- y - pre.fishery.yrs - 1 
        	TotBio[1:ind,counter]    <- rep.out$TotBio
        	Recruits[1:ind,counter]  <- rep.out$Recruits
        	
        	OFL[(y+1):(y+4)]      <- rep.out$OFL
        	ForeCat[(y+1):(y+4)]  <- mapply(function(x) ForeCat = ifelse(rep.out$ForeCatch[x] < 1, 1, rep.out$ForeCatch[x]), x = 1:4)
        	FSPR[,counter]        <- rep.out$FSPR
        	Fmult[,counter]       <- fmult
        	M.store[,counter]     <- rep.out$M
        	Lmin.store[,counter]  <- rep.out$Lmin
        	Lmax.store[,counter]  <- rep.out$Lmax
        	k.store[,counter]     <- rep.out$k
        	R0.out[,counter]      <- rep.out$R0
        	F.selex[,counter]     <- rep.out$FSelex
        	S.selex[,counter]     <- rep.out$SSelex
        	ind                   <- y - pre.fishery.yrs
        	SB[1:ind,counter]     <- rep.out$SB
        	Bratio[1:ind,counter] <- rep.out$Depl

        	Est[[1]] <- TotBio
        	Est[[3]] <- OFL
        	Est[[4]] <- ForeCat
        	Est[[5]] <- Fmult
        	Est[[6]] <- FSPR
        	Est[[7]] <- M.store
        	Est[[8]] <- R0.out
        	Est[[9]] <- SB
        	Est[[10]]<- Bratio
        	Est[[11]]<- F.selex
        	Est[[12]]<- S.selex
        	Est[[13]]<- Recruits
        	Est[[16]]<- Lmin.store
        	Est[[17]]<- Lmax.store
        	Est[[18]]<- k.store

       		names(Est) <- c("TotBio","OFL","ForeCat","Fmult","FSPR","M.store","R0.out","SB","Bratio","F.selex","S.selex","Recruits",
                          	 "Lmin.store", "Lmax.store", "k.store")
	        save(Est, file=estimates)

	        #Determine is the stock if assessed overfished for the first time
	        if (decl.overfished == FALSE & Bratio[(y - pre.fishery.yrs),counter] < over.thres) {
	        	decl.overfished = TRUE } 

	        if(decl.overfished == TRUE & Bratio[(y - pre.fishery.yrs),counter] >= bio.target){
	        	decl.overfished = FALSE }

	        #Change the data levels based upon the status and data scenario
	        if (decl.overfished  == TRUE) {
	        	if (data.scenario == "ds3" ) {
	        		ind             <- y - pre.fishery.yrs
            		f.len.samp[ind] <- floor(0.25 * f.len.samp[ind])
            		s.len.samp[ind] <- s.len.samp[ind]
            		f.age.samp[ind] <- floor(0.25 * f.age.samp[ind])
            		s.age.samp[ind] <- s.age.samp[ind]
	        	}

            	if (data.scenario == "ds0" || data.scenario == "ds1" || data.scenario == "ds2") {
      				ind             <- y - pre.fishery.yrs
      				f.len.samp[ind] <- f.len.samp[ind]
      				s.len.samp[ind] <- s.len.samp[ind]
      				f.age.samp[ind] <- f.age.samp[ind]
      				s.age.samp[ind] <- s.age.samp[ind]
    			}
        	}
        
        	#Set the ACLs for the next four years 
        	catch[(y+1):(y+4)] <- ForeCat[(y+1):(y+4)] 
	
    		#Rename the ctl and data files
    		file.rename(paste(run,"/Report.sso",sep =""), 
    		            paste(run,"/Report",nsim,"_",y-pre.fishery.yrs,".sso",sep =""))  
    		file.rename(paste(run,"/est.ctl",sep =""), 
    		            paste(run,"/est",nsim,"_",y-pre.fishery.yrs,".ctl",sep =""))      
		} #end assessment loop

    	Proj[[1]] <- SSB
    	Proj[[2]] <- depl
    	Proj[[3]] <- log(R0)
    	Proj[[4]] <- Ry
    	Proj[[5]] <- catch
    	Proj[[6]] <- ofl.true
    	Proj[[7]] <- acl.true
    	Proj[[8]] <- f.len.samp
    	Proj[[9]] <- s.len.samp
    	Proj[[10]]<- f.age.samp
    	Proj[[11]]<- s.age.samp
    	Proj[[12]]<- inflec.selec

    	names(Proj) <- c ("SSB", "Depl","R0","Ry", "catch","ofl.true", "acl.true", "f.len.samp","s.len.sam","f.age.samp",
    					"s.age.samp", "new.peak")
    	save(Proj, file = projections)
 	} #end projection loop
} #end sim loop
    