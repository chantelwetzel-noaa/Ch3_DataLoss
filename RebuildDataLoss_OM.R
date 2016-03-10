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

drive    <-"C:" #"//home//cwetzel//h_cwetzel"
LH       <- "rockfish"
start.n  <- 1
end.n    <- 50
data.scenario <- "ds6" 
tantalus      <- FALSE
github        <- TRUE
file.type     <- "boot" #"boot" "perfect"
determ        <- FALSE
setup.yrs     <- 50
do.MLE        <- FALSE
error.struct  <- "multinom" #"multinom" #"dirich"
if (file.type == "perfect") { do.MLE = T }
AgeError      <- TRUE


#Load packages
require(r4ss)
require(compiler)
require(gtools)

#Set the directory and create folder
directory <<- paste(drive,"/PhD/Chapter3/",LH, "_", data.scenario,"_",setup.yrs,"yr_sims_", error.struct, "_","AE_", AgeError, "_",start.n,"_",end.n,"/",sep="")
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

prior.matrix <- matrix(NA, end.n, 7)

#Start the simulation loop
for (nsim in start.n:end.n)
 {
 	Proj <- Est <- list()
 	#Resource the functions
    if (github) { 
      git.wd = "/Users/Chantell.Wetzel/Documents/GitHub/Ch3_DataLoss/"
      source(paste("C:", git.wd, "functions/Functions.R", sep = "")) 
      source(paste0(drive, git.wd, "functions/Arrays.R"))
    }
    if (!github){ 
      source(paste(drive,"/PhD/Chapter3/code/functions/Functions.R",sep="")) 
      source(paste0(drive, git.wd, "functions/Arrays.R"))
    }  

 	equil = FALSE
    pre.dev.phase = ifelse(equil == TRUE, -3, 4)
    sigmaR = ifelse(determ == TRUE, 0, sigmaR)
    determ = ifelse(sigmaR == 0, TRUE, FALSE)
    stop.rec.est <- ifelse(LH == "rockfish", 7, 3 )
 	# Save the run information ===========================================================================
 	capture.output(list(Survey_Start = start.survey, 
                     Overfished_Selectivity_Shift = selec.adj,
                     Annual_TimeVarying_Selectivity = select.sd,
                     #Estimate_Annual_Deviations = selec.dev, 
                     Depletion_Year_50 = final.depl, 
                     Recruitment_AutoCorrelation = auto,
                     Survey_CV = survey.cv,
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
    m.seed        <- as.numeric(seed.list[[1]][,"m.seed"])   
 	select.seed   <- as.numeric(seed.list[[1]][,"spare1"])
    prior.seed    <- as.numeric(seed.list[[1]][,"spare2"]) 
	 
    #Set comp seed -----------------------------------------------------------------------------------------------------------
    set.seed(comp.seed[nsim])

	#Catch History -----------------------------------------------------------------------------------------------------------
	set.seed(catch.seed[nsim])
	catch.dev <- c(rnorm(setup.yrs * 0.2,0,0.50),rnorm(setup.yrs*0.70,0,0.07),rnorm(setup.yrs*0.10,0,0.15))
	CatchTot <- rep(0,setup.yrs) ;  CatchTot[1] <- 25
	  
	#for (a in 2:(setup.yrs-11)) { 
	#   CatchTot[a]<- CatchTot[a-1]*1.1 
	#       if (CatchTot[a-1]*1.1 > 1000 ) { 
	#           CatchTot[a] <- 1000 }  
	#}
	#for (a in (setup.yrs-10):setup.yrs) { 
	#   CatchTot[a]<- CatchTot[a-1]*0.97 
	#}

    for (a in 2:(setup.yrs-25)) { 
       CatchTot[a]<- CatchTot[a-1]*1.2 
           if (CatchTot[a-1]*1.2 > 1000 ) { 
               CatchTot[a] <- 1000 }  
    }
    for (a in (setup.yrs-26):(setup.yrs-6)) { 
       CatchTot[a]<- CatchTot[a-1]*1 
    }
    #for (a in (setup.yrs-5):(setup.yrs)) { 
       CatchTot[(setup.yrs-5):(setup.yrs)]<- CatchTot[(setup.yrs-6)]*1 #0.25
    #}
	  
	CatchTot     <- round(CatchTot,0)    
	CatchTot.err <- round(CatchTot+CatchTot*catch.dev,0)
	catch   <- c(rep(0,pre.fishery.yrs),CatchTot.err)
	 
	#Draw recruitment deviations----------------------------------------------------------------------------------------------------- 
	set.seed(recruit.seed[nsim])
	rho      <- 0
	if ( auto ) { rho <- 1 / sqrt(2) }
	recdevs  <- rnorm((total.yrs+1), 0, sigmaR)
	autocorr <- matrix(0, total.yrs + 4, 1)
	autocorr[1] <- recdevs[1]  
	for (e in 2:(total.yrs + 1)) { 
	   autocorr[e] <- rho*autocorr[e-1]+sqrt(1-rho*rho)*recdevs[e]  }
	#rownames(autocorr) = c(-ages:-1,0,1:(setup.yrs + project.yrs + 5))
	#This will start the population in equilibrium
    if (equil) { autocorr[1:(ages+1)] <- 0 }
    	
	#Draw Survey Error---------------------------------------------------------------------------------------------------------------  
	set.seed(survey.seed[nsim])
	survey.err <- rnorm(total.yrs, 0, survey.cv)

	#Variation in Selectivity----------------------------------------------------------------------------------------------------------
	set.seed(select.seed[nsim])
	select.err     <- rnorm(total.yrs, 0, select.sd)
	fsp1.vec       <- round(fsp1.start*exp(-0.5*select.sd*select.sd + select.err),0)
    fsp1.shift.vec <- round((fsp1.start + selec.adj)*exp(-0.5*select.sd*select.sd + select.err),0)
    fsp2.vec       <- sample(x = c(-3, fsp2), size = total.yrs, replace = T, prob = c(0,1))
    select.err     <- rnorm(total.yrs, 0, 0.25)
    fsp2.shift.vec <- round((fsp2.vec + dome.adj)*exp(-0.5*select.sd*select.sd + select.err), 0)

    #Variation in Natural Mortality--------------------------------------------------------------------------------------------------
    set.seed(m.seed[nsim])
    m.devvec <- rnorm(total.yrs, 0, m.sd)#round(rlnorm(total.yrs, meanlog=(log(m)-0.5*m.sd^2), sdlog=m.sd),3) 
    #Apply autocorrelation in the annual variation
    rho       <- 1/sqrt(2) #0.50
    m.auto    <- numeric(total.yrs)
    m.auto[1] <- m.devvec[1] 
    for (e in 2:total.yrs){
        m.auto[e] <- rho * m.auto[e-1] + sqrt(1 - rho * rho) * m.devvec[e]  }
    m.vec = round(m * exp( m.auto - 0.50 * m.sd^2),3)

    #Variation in growth -------------------------------------------------------------------------------------------------------------
    k.vec = numeric(total.yrs)
    k.vec = rep(kf, total.yrs) 
    #k.vec = round(kf * exp(m.auto - 0.50 * m.sd^2),3)

    # Prior value for parameters-----------------------------------------------------------------------------------------------------
    set.seed(prior.seed[nsim])
    #m.prior <- round(rlnorm(1000, meanlog = (log(m) - 0.5*m.sd^2), sdlog = m.sd), 3)
    #m.prior <- round(rlnorm(1, meanlog=(log(m)-0.5*0.50^2), sdlog=0.50),3) 
    #lmin.prior <- round(rnorm(1, L1,  0.50),3)
    #lmax.prior <- round(rnorm(1, L2f, 0.50),3)
    #cv.1.prior <- round(rnorm(1, CV1, 0.50),3)
    #cv.2.prior <- round(rnorm(1, CV2, 0.50),3)
    m.prior    <- round(runif(1,  m -  m *0.25,  m + m*0.25),3) 
    lmin.prior <- L1 #round(runif(1, L1 - L1 *0.10, L1 + L1 *0.10),3)
    lmax.prior <- round(runif(1, L2f- L2f*0.10, L2f + L2f*0.10),3)
    k.prior    <- round(runif(1, kf - kf *0.10, kf  + kf *0.10),3)
    cv1.prior  <- round(runif(1, CV1- CV1*0.10, CV1 + CV1*0.10),3)
    cv2.prior  <- round(runif(1, CV2- CV2*0.10, CV2 + CV2*0.10),3)
    h.prior    <- round(runif(1, steep-steep*0.10, steep+steep*0.10),2)

    prior.matrix[nsim,]     = c(m.prior, lmin.prior, lmax.prior, k.prior, cv1.prior, cv2.prior, h.prior)
    colnames(prior.matrix)  = c("m.prior", "lmin.prior", "lmax.prior", "k.prior", "cv1.prior", "cv2.prior", "h.prior")
    save(prior.matrix, file = paste0(directory,"save/priors"))

	# Calculate the buffer for the forecast file
	buffer <- 0.95 #exp(qnorm(p.value, 0, sigma))

	# Create the operating model historical population ---------------------------------------------------------------------
	setwd(om)
	y = setup.yrs + pre.fishery.yrs 
	OM = OM.run.1 = TRUE
    fix.q = ifelse(OM ==TRUE, 2, 0)
    #fsp1.om[1:y] <- fsp1.start
    fsp1 <- fsp1.start
	#n.devs = length(autocorr[1:y])
	#write.devs = cbind(c((-1*pre.fishery.yrs+1):1,0, 1:(y - pre.fishery.yrs)), autocorr[1:y])
    #write.devs = cbind(c((-1*ages):-1, 0, 1:(y - pre.fishery.yrs + 4)), autocorr[1:(y + 6)])
    write.devs = cbind(c(1:(ages-1), ages:y), autocorr[1:y])
    n.devs = dim(write.devs)[1]

	# Set up the bias adjustment parameters ----------------------------------------------------------------------------------
	main.rec.start <-  1
	main.rec.end   <-  y  #setup.yrs            
	start.bias     <- -2  #-pre.fishery.yrs - 2
	full.bias      <- -1  #-pre.fishery.yrs - 1
	last.bias      <-  y  #setup.yrs        
	last.no.bias   <-  y  #setup.yrs
	start.devs     <-  0  #start.bias + 1
	max.bias.adj   <-  1  #0 #when no devs this results in no correction #1 # full bias adjustment = ry*exp(-0.5*sigmaR^2 + recdev)
    start.bias.est <-  85
    full.bias.est  <- start.bias.est + 15

    #harvest control rule
    hcr.high = 0.011
    hcr.low  = 0.01

	# This is the constant added to the proportional composition data
	add.const  = 0
	boot.files = 3 # Create a true, perfect, bootstrapped data set
    end.phase  = 1  # Only estimate a R0 value that makes the depletion survey true
    est.R0 = 1
    m.phase = 5
    h.phase = 6

    # Create the depletion survey for the data file
    fleets = "Fishery%Survey%Depl"
    n.fleet = 3
    block.num =  block.fxn = 0
    R0 = 10000
    need.blocks = FALSE
    survey = rep(5000, length(start.survey:y))

    # Write the operating model files
    dat.file = "om.dat"; ctl.file = "om.ctl"
    do.forecast = 0
    get.forecast = do.true.fspr = do.est.fspr = FALSE
    writeStarter(starter = "starter.ss")
    writeForecast(forecast = "forecast.ss", y = y)
    writeCtl.om(ctl = "om.ctl", y = y)
    writeDat(dat = "om.dat", y = y, survey , fore.catch = catch)
    if (tantalus)  { system("./SS3 -nohess > test.txt 2>&1")  }
    if (!tantalus) { shell("ss3.exe -nohess > test.txt 2>&1")  }
    #Save the original model files
    file.copy(paste(om,"/om.ctl",sep =""), paste(om,"/om",nsim,"_",y,".ctl",sep =""), overwrite = TRUE)    		                 
    file.copy(paste(om,"/om.dat",sep =""), paste(om,"/om",nsim,"_",y,".dat",sep =""), overwrite = TRUE)
    file.copy(paste(om,"/Report.sso",sep =""), paste(om,"/Report_depl",nsim,"_",y,".sso",sep =""), overwrite = TRUE) 


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
    dat$N_cpue <- dat$N_cpue - 1 #length(survey)
    dat$CPUEinfo <- dat$CPUEinfo[1:2,]
    dat$CPUE <- dat$CPUE[1:dat$N_cpue,]
    dat$add_to_comp <- 0
    SS_writedat(datlist=dat,outfile=paste(om,"/om.dat",sep=""),overwrite=TRUE,verbose=TRUE)
    #Change the final phase for estimation
    starter <- SS_readstarter(file=paste(om,"/starter.ss",sep=""))
    starter$last_estimation_phase <- 0
    SS_writestarter(starter,dir=paste(om,"/",sep=""),file="starter.ss", overwrite=TRUE,verbose=TRUE)
    #Remove the depletion survey from the control file and set the new estimated R0 value
    get.forecast = TRUE
    OM.run.1 = FALSE
    writeCtl.om(ctl = "om.ctl", y = y)
    do.forecast = 3
    writeForecast(forecast = "forecast.ss", y = y)
    if (tantalus)  { system("./SS3 -nohess > test.txt 2>&1")  }
    if (!tantalus) { shell("ss3.exe -nohess > test.txt 2>&1")  }
    file.rename(paste(om,"/om.ctl",sep =""), paste(om,"/om_fore_",nsim,"_",y,".ctl",sep =""))         
    file.rename(paste(om,"/om.dat",sep =""), paste(om,"/om_fore_",nsim,"_",y,".dat",sep =""))
    file.copy(paste(om,"/Report.sso",sep =""), paste(om,"/Report_",nsim,"_",y,".sso",sep =""))
    file.rename(paste(om,"/forecast.ss",sep =""), paste(om,"/forecast_",nsim,"_",y,".ss",sep =""))  
    file.copy(paste(om,"/data.ss_new",sep =""), paste(om,"/data",nsim,"_",y,".ss_new",sep =""), overwrite = TRUE) 
    file.copy(paste(om,"/Forecast-report.sso",sep =""), paste(om,"/Forecast-report",nsim,"_",y,".sso",sep =""), overwrite = TRUE) 
    
    #Read in the report file and save needed quantities
    rep.new   <- readLines(paste(om, "/Report.sso", sep=""))
    rep.out   <- Rep_Summary(rep.new, y, pre.fishery.yrs, do.forecast)
    index     <- Do_Survey(file = rep.new, ind = seq(start.survey, y, 2), survey.err)

    SSB [1:y]      <- rep.out$SB
    Ry  [1:(y-1)]  <- rep.out$Recruits
    depl[1:y]      <- rep.out$Depl
    R0		       <- exp(rep.out$R0)
    SB0		       <- rep.out$SB.virgin
    ofl.true[(y+1):(y+4)] <- rep.out$OFL
    acl.true[(y+1):(y+4)] <- rep.out$ACL

    decl.overfished = OM = FALSE
    counter = overfished.counter = 0
    block.num = block.fxn = bind.block = 0

    ###################################################################################################################
    #Start the projection loop
    ###################################################################################################################
 	for (y in (pre.fishery.yrs + setup.yrs):total.yrs) {


        #Change the data levels based upon the status and data scenario
        if (decl.overfished  == TRUE) {
            hcr.high = 0.011
            hcr.low  = 0.01

            if (data.scenario == "ds3" || data.scenario == "ds4" || data.scenario == "ds7") {
                f.len.samp[y] <- floor(0.20 * f.len.samp[y])
                s.len.samp[y] <- s.len.samp[y]
                f.age.samp[y] <- floor(0.20 * f.age.samp[y])
                s.age.samp[y] <- s.age.samp[y]
            }
            if (data.scenario == "ds0" || data.scenario == "ds1" || data.scenario == "ds2" ) {
                f.len.samp[y] <- f.len.samp[y]
                s.len.samp[y] <- s.len.samp[y]
                f.age.samp[y] <- f.age.samp[y]
                s.age.samp[y] <- s.age.samp[y]
            }
            if (data.scenario == "ds5" || data.scenario == "ds6"){
                f.len.samp[y] <- 0
                #s.len.samp[y] <- 0
                f.age.samp[y] <- 0
                #s.age.samp[y] <- 0
            }
        }

        if (data.scenario == "ds5" || data.scenario == 'ds6') {
            need.blocks = FALSE
            block.num = block.fxn = 0 }
 		
 		do.ass = y
        if(LH == "rockfish") { 
            do.ass = y - 2
            if (setup.yrs == 50) { do.ass = y }
        }

    	if(LH == "flatfish") { 
            do.ass = y 
            if (setup.yrs == 50) { do.ass = y - 2 }
        } 

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

            write.devs = cbind(1:y, autocorr[1:y])
            n.devs = dim(write.devs)[1]
			#survey = rep(5000, length(start.survey:(y-pre.fishery.yrs)))
            survey = rep(5000, length(start.survey:y))

            #Selectivity shift while overfished
            if (decl.overfished){
                fsp1.vec[(y-3):y] = fsp1.shift.vec[(y-3):y] 
                fsp2.vec[(y-3):y] = fsp2.shift.vec[(y-3):y]}


			# Set up the bias adjustment parameters ----------------------------------------------------------------------------------
			main.rec.start <-  1
			main.rec.end   <-  y           
			start.bias     <- -2 
			full.bias      <- -1 
			last.bias      <-  y       
			last.no.bias   <-  y+1
			start.devs     <-  0
			max.bias.adj   <-  1 #0 #when no devs this results in no correction #1 # full bias adjustment = ry*exp(-0.5*sigmaR^2 + recdev)
				
			# This is the constant added to the proportional composition data
			add.const  = 0
            est.R0 = 1

    		writeDat(dat = "om.dat", y = y, survey , fore.catch = catch)
    		data = NULL
    		dat <- SS_readdat(paste(om,"/om.dat",sep=""))
    		dat$Nsurveys <- 1 
    		dat$fleetnames <- c("Fishery", "Survey")
    		dat$areas <- c(1,1)
    		dat$surveytiming <- c(0.5, 0.5) 
    		dat$N_cpue <- dat$N_cpue - 1 #length(survey)
    		dat$CPUEinfo <- dat$CPUEinfo[1:2,]
    		dat$CPUE <- dat$CPUE[1:dat$N_cpue,]
    		SS_writedat(datlist=dat, outfile=paste(om,"/om.dat",sep=""),overwrite=TRUE,verbose=TRUE)

 			starter <- SS_readstarter(file=paste(om,"/starter.ss",sep=""))
    		starter$last_estimation_phase <- 0
    		SS_writestarter(starter,dir=paste(om,"/",sep=""),file="starter.ss", overwrite=TRUE,verbose=TRUE)
    		#Remove the depletion survey from the control file and set the new estimated R0 value
    		get.forecast = TRUE
 			writeCtl.om(ctl = "om.ctl", y = y)
            #do.true.fspr  = TRUE
    		#fspr.input =0.75*fspr.om # ifelse(LH == "rockfish", 0.02, 0.10)#0.75*fspr.om
            buffer = 0.75
            do.forecast = 3
    		writeForecast(forecast = "forecast.ss", y = y)
    		if (tantalus)  { system("./SS3 -nohess > test.txt 2>&1")  }
    		if (!tantalus) { shell("ss3.exe -nohess > test.txt 2>&1")  }

    		rep.new   <- readLines(paste(om, "/Report.sso", sep=""))
    		rep.out   <- Rep_Summary(rep.new, y, pre.fishery.yrs, do.forecast)

            check.depl<- rep.out$Depl[y]
            if (check.depl > bio.target){
                #fspr.input = 0.95*fspr.om # ifelse(LH == "rockfish", 0.05, 0.20)#fspr.om
                buffer = 0.95
                hcr.high = ctl.rule.tgt
                hcr.low  = ctl.rule.thres
                writeForecast(forecast = "forecast.ss", y = y)
                if (tantalus)  { system("./SS3 -nohess > test.txt 2>&1")  }
                if (!tantalus) { shell("ss3.exe -nohess > test.txt 2>&1")  }
                rep.new   <- readLines(paste(om, "/Report.sso", sep=""))
                rep.out   <- Rep_Summary(rep.new, y, pre.fishery.yrs, do.forecast)
            }
            
            ind <- seq(y-2, y, 2)
            if (data.scenario == "ds0") { ind = seq(y-3, y, 2)}
            index     <- c(index, Do_Survey(file = rep.new, ind , survey.err))
            SSB [1:y]   <- rep.out$SB
            Ry  [1:(y-1)]  <- rep.out$Recruits
            depl[1:y]  <- rep.out$Depl
            ofl.true[(y+1):(y+4)] <- rep.out$OFL
            acl.true[(y+1):(y+4)] <- rep.out$ACL

            file.rename(paste(om,"/Report.sso",sep =""), paste(om,"/Report",nsim,"_",y,".sso",sep ="")) 
            file.copy(paste(om,"/data.ss_new",sep =""), paste(om,"/data",nsim,"_",y,".ss_new",sep =""), overwrite = TRUE) 
            file.copy(paste(om,"/om.dat",sep =""), paste(om,"/om",nsim,"_",y,".dat",sep =""), overwrite = TRUE)
            file.copy(paste(om,"/om.ctl",sep =""), paste(om,"/om",nsim,"_",y,".ctl",sep =""), overwrite = TRUE) 
            file.copy(paste(om,"/forecast.ss",sep =""), paste(om,"/forecast",nsim,"_",y,".ss",sep =""), overwrite = TRUE)
            file.copy(paste(om,"/Forecast-report.sso",sep =""), paste(om,"/Forecast-report",nsim,"_",y,".sso",sep =""), overwrite = TRUE) 
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
                start.devs     <- 1
                main.rec.start <- ifelse(start.survey - ages < ages, ages, start.survey - ages)
    			start.bias     <- start.bias.est 
        		full.bias      <- full.bias.est  
				last.bias      <- y - stop.rec.est 
        		last.no.bias   <- ifelse( LH == "rockfish", y-6 , y-4 ) 
        		main.rec.end   <- y 
        		max.bias.adj   <- max.bias.adj.est
        	}

    		if ( counter == 1){
    			start.devs     <- 1 
    			main.rec.start <- ifelse(start.survey - ages < ages, ages, start.survey - ages)
				start.bias     <- ifelse(start.survey - ages - floor(ages/2) < ages, 1, 
                                            start.survey - ages - floor(ages/2))
				full.bias      <- start.survey - floor(ages/2) 
				last.bias      <- y - stop.rec.est 
        		last.no.bias   <- y 
        		main.rec.end   <- y 
				max.bias.adj   <- 0.80 	  
            }

            est.R0 = 1
			n.devs = 0
            #do.est.fspr = FALSE

    		writeCtl(ctl = "est.ctl", y = y)

            #SS_writedat(datlist=dat,outfile=paste(run,"/", file.type,nsim,"_",y,".ss",sep=""),overwrite=TRUE,verbose=TRUE)
    		#Split the data file and modify
			SS_splitdat(inpath = om, outpath = run,
                    inname="data.ss_new", outpattern=paste(file.type,nsim,"_",y,sep=""),
                    number=F, verbose=T, fillblank=T, MLE= do.MLE)

			if (counter == 1){
				dat <- NULL
                dat <- SS_readdat(paste(run,"/", file.type ,nsim,"_", y,".ss",sep=""))
    			dat$catch[,1] <- catch[1:y]#[(pre.fishery.yrs +1):y]
    			dat$add_to_comp <- 0.00001
                #dat$ageerror[2,] <- rep(0.10, ages)
                #This value is just a filler, the model will be rerun below with the estimated and adjusted value
                #fspr.input = 0.95*fspr.om
                buffer = 0.95
                hcr.high = ctl.rule.tgt
                hcr.low  = ctl.rule.thres
                dat$agecomp = Get_Samps(data.type = "age", year.vec = (pre.fishery.yrs + 1): y )
                dat$lencomp = Get_Samps(data.type = "len", year.vec = (pre.fishery.yrs + 1): y )
                # Read the expected values and generate my own survey data
                dat$CPUE[,4] <- index
			}
    		if (counter != 1){
    			dat.new <- dat.old <- dat <- NULL
    			dat.new <- SS_readdat(paste(run,"/", file.type, nsim,"_", y,".ss",sep=""))
    			dat.old <- SS_readdat(paste(run,"/", file.type ,nsim,"_", y-4,".ss",sep=""))
    			dat.old$endyr <- dat.new$endyr
    			dat.old$N_catch <- dat.new$N_catch
    			dat.old$catch <- cbind(catch[1:y], dat.new$catch[,2:3])#cbind(catch[(pre.fishery.yrs+1):y], dat.new$catch[,2:3])
    		
                dat.old$N_cpue <- dat.new$N_cpue
    			#ind = (dim(dat.new$CPUE)[1]-1):(dim(dat.new$CPUE)[1])
    			#dat.old$CPUE <- rbind(dat.old$CPUE, dat.new$CPUE[ind,])
                dat.new$CPUE[,4] <- index
                dat.old$CPUE <- dat.new$CPUE
    			dat.old$add_to_comp <- 0.00001
                #dat.old$ageerror[2,] <- rep(0.10, ages)
                dat.new$agecomp = Get_Samps(data.type = "age", year.vec = (y - 3): y )
                dat.new$lencomp = Get_Samps(data.type = "len", year.vec = (y - 3): y )

    			dat.old$N_lencomp <- dat.new$N_lencomp
    			ind = dat.new$lencomp$FltSvy == 1 ; ind.old.1 = dat.old$lencomp$FltSvy == 1
                if ( sum(ind) != sum(ind.old.1) ) { ind1 = (sum(ind)-3):sum(ind) }
                if ( sum(ind) == sum(ind.old.1) ) { ind1 = 0}
    			#ind1 = ifelse(sum(ind) != sum(ind.old.1), (sum(ind)-3):sum(ind), 0)
    			ind = dat.new$lencomp$FltSvy == 2 ; ind.old.2 = dat.old$lencomp$FltSvy == 2
                ind2 = (length(ind)-1):length(ind)
                if (sum(ind1) != 0){
                    dat.old$lencomp <- rbind(dat.old$lencomp[ind.old.1,], dat.new$lencomp[ind1,], 
                                         dat.old$lencomp[ind.old.2,], dat.new$lencomp[ind2,])
                }
                if (sum(ind1) == 0){
                    dat.old$lencomp <- rbind(dat.old$lencomp[ind.old.1,], 
                                         dat.old$lencomp[ind.old.2,], dat.new$lencomp[ind2,])
                }
    			dat.old$N_agecomp <- dat.new$N_agecomp
				ind = dat.new$agecomp$FltSvy == 1 ; ind.old.1 = dat.old$agecomp$FltSvy == 1
    			if ( sum(ind) != sum(ind.old.1) ) { ind1 = (sum(ind)-3):sum(ind) }
                if ( sum(ind) == sum(ind.old.1) ) { ind1 = 0}
    			ind = dat.new$agecomp$FltSvy == 2 ; ind.old.2 = dat.old$agecomp$FltSvy == 2
    			ind2 = (length(ind)-1):length(ind)
                if (sum(ind1) != 0){
                    dat.old$agecomp <- rbind(dat.old$agecomp[ind.old.1,], dat.new$agecomp[ind1,], 
                                            dat.old$agecomp[ind.old.2,], dat.new$agecomp[ind2,]) }
                if (sum(ind1) == 0){
                    dat.old$agecomp <- rbind(dat.old$agecomp[ind.old.1,], 
                                            dat.old$agecomp[ind.old.2,], dat.new$agecomp[ind2,])                    
                }              

    			dat = dat.old

                if(decl.overfished == FALSE) { 
                    #fspr.input = 0.95*fspr.est }#ifelse(LH == "rockfish", 0.05, 0.20) }#fspr.est }
                    buffer = 0.95
                    hcr.high = ctl.rule.tgt
                    hcr.low = ctl.rule.thres }#0.95 }
    		}

            SS_writedat(datlist=dat,outfile=paste(run,"/", file.type,nsim,"_",y,".ss",sep=""),overwrite=TRUE,verbose=TRUE)
            if(decl.overfished == TRUE) { 
                buffer = 0.75  }
            get.forecast = FALSE #do.true.fspr  = FALSE
            do.forecast = 3 #This switches on and off the forecast, forecast catches coincide with btarget
            writeForecast(forecast = "forecast.ss", y = y)

    		#Modify the starter file
    		starter<-SS_readstarter(file=paste(run,"/starter.ss",sep=""))
            starter$datfile<-paste(file.type,nsim,"_",y,".ss", sep ="")
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
            rerun = 0
    		rep.new   <- readLines(paste(run, "/Report.sso", sep=""))
            gradient  <- as.numeric(strsplit(rep.new[grep("Convergence_Level",rep.new)], " ")[[1]][2]) 
        	virgin.SB <- as.numeric(strsplit(rep.new[grep(paste("SPB_Virgin",sep=""),rep.new)]," ")[[1]][3])
        	while(virgin.SB < (SB0/4) || virgin.SB > (SB0*4)) {
        	  rerun = rerun + 1  
        	  starter.file = SS_readstarter(paste(run, "/starter.ss", sep = ""))
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
        	  rep.new   <- readLines(paste(run, "/Report.sso", sep=""))
        	  virgin.SB <- as.numeric(strsplit(rep.new[grep(paste("SPB_Virgin",sep=""),rep.new)]," ")[[1]][3])
        	  if (virgin.SB > (SB0/4) && virgin.SB < (SB0*4)) {
        	    break()
        	  }
        	  if(rerun > 1) { break () }
        	}

            # Now check the gradient
            grad.rerun = 0
            if (gradient > 0.50){
                grad.rerun = grad.rerun + 1
                starter.file = SS_readstarter(paste(run, "/starter.ss", sep = ""))
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
            }
	
			if (determ == FALSE & y <= (pre.fishery.yrs + setup.yrs + 9)){
                if(rerun == 2){
                    start.bias <- start.bias.est
                    full.bias <- full.bias.est
                    last.bias <- y - stop.rec.est
                    last.no.bias <- y
                    max.bias.adj <- max.bias.adj.est
                    main.rec.end <- main.rec.end.est
                }

                if(rerun != 2){
                    #Apply the bias correction
                    rep.bias     <- SS_output(run, covar = TRUE, printstats = FALSE)
                    new.bias     <- SS_fitbiasramp(rep.bias, 
                                        startvalues = c(start.bias, full.bias , last.bias, last.no.bias ,max.bias.adj))
                    start.bias   <- start.bias.est <- new.bias$df[1,1]
                    full.bias    <- full.bias.est  <- new.bias$df[2,1]
                    last.bias    <- last.bias.est <- new.bias$df[3,1] #y - stop.rec.est 
                    last.no.bias <- last.no.bias.est <- new.bias$df[4,1] #y 
                    max.bias.adj <- max.bias.adj.est <-new.bias$df[5,1]
                    main.rec.end <- main.rec.end.est <- y 
                }

                #Rewrite the control file with the new bias adjustment values
                writeCtl(ctl = "est.ctl", y = y)
                rep.new   <- readLines(paste(run, "/Report.sso", sep=""))
                rep.out   <- Rep_Summary(rep.new, y, pre.fishery.yrs, do.forecast)
                check.depl <- rep.out$Depl[y] 
                if (decl.overfished == FALSE && check.depl < over.thres){
                    buffer = 0.75
                    hcr.high = 0.011
                    hcr.low  = 0.01
                    writeForecast(forecast = "forecast.ss", y = y)
                }  
                #Rerun the model with the new bias values
                if (tantalus)  { system("./SS3 -nohess > test.txt 2>&1")  }
                if (!tantalus) { shell("ss3.exe -nohess > test.txt 2>&1")  }                    
        	}

            rep.new   <- readLines(paste(run, "/Report.sso", sep=""))
            rep.out   <- Rep_Summary(rep.new, y, pre.fishery.yrs, do.forecast)
            check.depl<- rep.out$Depl[y]
            if (decl.overfished == FALSE && counter != 1 && check.depl < over.thres){
                buffer = 0.75
                hcr.high = 0.011
                hcr.low  = 0.01
                writeForecast(forecast = "forecast.ss", y = y)
                if (tantalus)  { system("./SS3 -nohess > test.txt 2>&1")  }
                if (!tantalus) { shell("ss3.exe -nohess > test.txt 2>&1")  } 
            }

            #Determine if the stock is rebuilt and adjust the harvest if so
            if (decl.overfished == TRUE && check.depl > bio.target){
                #Change harvest rate to fspr
                buffer = 0.95
                hcr.high = ctl.rule.tgt
                hcr.low  = ctl.rule.thres
                writeForecast(forecast = "forecast.ss", y = y)
                if (tantalus)  { system("./SS3 -nohess > test.txt 2>&1")  }
                if (!tantalus) { shell("ss3.exe -nohess > test.txt 2>&1")  } 
            }

		
			#Read the report file and save values
			rep.new   <- readLines(paste(run, "/Report.sso", sep=""))
			rep.out   <- Rep_Summary(rep.new, y, pre.fishery.yrs, do.forecast)
            fspr.est  <- as.numeric(strsplit(rep.new[grep(paste("SPR_Btgt",sep=""),rep.new)]," ")[[1]][3])
        	ind       <- y - 1 
        	TotBio[1:ind,counter]    <- rep.out$TotBio
        	Recruits[1:ind,counter]  <- rep.out$Recruits
        	
        	OFL[(y+1):(y+4)]      <- rep.out$OFL
        	ForeCat[(y+1):(y+4)]  <- mapply(function(x) ForeCat = ifelse(rep.out$ForeCatch[x] < 1, 10, rep.out$ForeCatch[x]), x = 1:4)
        	FSPR[,counter]        <- rep.out$FSPR
        	Fmult[,counter]       <- fspr.est
        	M.store[,counter]     <- rep.out$M
        	Lmin.store[,counter]  <- rep.out$Lmin
        	Lmax.store[,counter]  <- rep.out$Lmax
        	k.store[,counter]     <- rep.out$k
            h.store[,counter]     <- rep.out$h
            cv.y.store[,counter]  <- rep.out$cv.young
            cv.old.store[,counter]<- rep.out$cv.old
        	R0.out[,counter]      <- rep.out$R0
        	F.selex[,counter]     <- rep.out$FSelex
        	S.selex[,counter]     <- rep.out$SSelex
        	ind                   <- y 
        	SB[1:ind,counter]     <- rep.out$SB
        	Bratio[1:ind,counter] <- rep.out$Depl
            grad.out[counter]     <- rep.out$gradient
            grad.check[counter]   <- grad.rerun
            #fsp1.est[,counter]    <- ifelse(need.blocks == F, 0, rep.out$F.selex.1.adj)
            fsp2.est[,counter]    <- ifelse(need.blocks == F, 0, rep.out$F.selex.2.adj)

        	Est[[1]] <- TotBio
        	Est[[2]] <- OFL
        	Est[[3]] <- ForeCat
        	Est[[4]] <- Fmult
        	Est[[5]] <- FSPR
        	Est[[6]] <- M.store
        	Est[[7]] <- R0.out
        	Est[[8]] <- SB
        	Est[[9]] <- Bratio
        	Est[[10]]<- F.selex
        	Est[[11]]<- S.selex
        	Est[[12]]<- Recruits
        	Est[[13]]<- Lmin.store
        	Est[[14]]<- Lmax.store
        	Est[[15]]<- k.store
            Est[[16]]<- fsp2.est #fsp1.est
            Est[[17]]<- recovered.est
            Est[[18]]<- h.store

       		names(Est) <- c("TotBio","OFL","ForeCat","Fmult","FSPR","M.store","R0.out","SB","Bratio","F.selex","S.selex","Recruits",
                          	 "Lmin.store", "Lmax.store", "k.store",  "fsp2.est","recovered.est", "h")
	        save(Est, file=estimates)

            #Set the ACLs for the next four years 
            catch[(y+1):(y+4)] <- ForeCat[(y+1):(y+4)] 
    
            #Rename the ctl and data files
            file.rename(paste(run,"/Report.sso",sep =""), paste(run,"/Report",nsim,"_",y,".sso",sep ="")) 
            file.rename(paste(run,"/est.ctl",sep =""), paste(run,"/est",nsim,"_",y,".ctl",sep =""))
            file.rename(paste(run,"/forecast.ss",sep =""), paste(run,"/forecast",nsim,"_",y,".ss",sep =""))

            #Determine is the stock if assessed overfished for the first time
            if (decl.overfished == FALSE & Bratio[y,counter] < over.thres) {
                decl.overfished = TRUE 
                overfished.counter = 1 + overfished.counter 
                need.blocks = TRUE
                block.num = block.pattern = 1; block.fxn = 2
                if (data.scenario == "ds5" || data.scenario == "ds6") {
                    need.blocks = FALSE
                    block.number = block.fxn = 0 }
                decl.yr = y + 1
                recovered.est[y] <- decl.yr
            } 

            if(decl.overfished == TRUE){
                end.yr = y + 4
                block.yrs = c(decl.yr, end.yr)
                if(Bratio[y,counter] >= bio.target){
                    decl.overfished = FALSE 
                    end.yr = y
                    recovered.est[y] <- end.yr
                    block.yrs = c(decl.yr, end.yr)
                }
            }           

		} #end assessment loop

        if (sum(recovered.om) == 0){
            recovered.om[y] <- ifelse(depl[y] > bio.target, y, 0)
        } 

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
    	Proj[[12]]<- fsp1.vec
        Proj[[13]]<- fsp2.vec
        Proj[[14]]<- recovered.om

    	names(Proj) <- c ("SSB", "Depl","R0","Ry", "catch","ofl.true", "acl.true", "f.len.samp","s.len.sam","f.age.samp",
    					"s.age.samp", "peak", "dome", "recovered.om")
    	save(Proj, file = projections)
 	} #end projection loop
} #end sim loop
    