##          SS3 OPERATING MODEL             ###
##            CHANTEL WETZEL                ###
##           UPDATED 2/20/14                ###

#source("F://PhD//Chapter3//Code//OM_2172014.R") 


DoSim <- function(drive, LH, SR,  depl.value, sigma, p.value, start.n, end.n, data.scenario) {

 NSIM         <<- end.n
 final.depl   <- depl.value
 
 seed.list <- list()
 
 require(compiler)
 require(r4ss)
 #update_r4ss_files(save=FALSE,local="C:/Program Files/R/R-3.1.0/library/r4ss_update_May132014")
 update_r4ss_files()
 
 #Set the directory 
 directory <<- paste(drive,":/PhD/Chapter3/",LH, "_", data.scenario,"_sims_",start.n,"_",end.n,"/",sep="")
 dir.create(directory)
 dir.create(paste(directory,"/save",sep=""))
 dir.create(paste(directory,"/ctl", sep=""))

 #Move the executable to the correct folder to run simulations
 if (tantalus == F) {
 file.copy(paste(drive,":/PhD/Chapter3/ss3_opt.exe",sep=""),paste(directory,"/ss3_opt.exe",sep="")) }
 
 if (tantalus == T) {
 file.copy(paste(drive,":/PhD/Chapter3/SS3",sep=""),paste(directory,"/SS3",sep="")) }
 
 #Source in the life-history parameter values
 source(paste(drive,":/PhD/Chapter3/code/LH_parameter_values.R",sep=""))

 #Source in the data scenarios
 source(paste(drive,":/PhD/Chapter3/code/Data_Scenarios.R",sep=""))
  
 #Source in external functions
 source(paste(drive,":/PhD/Chapter3/code/Functions.R",sep=""))
 
 #Source in Array Setup
 source(paste(drive,":/PhD/Chapter3/code/Arrays.R",sep=""))
 
 print(LH) ; print(paste("True Depletion", final.depl,sep=" "))
 print(paste("Survey Length", start.survey, sep=" "))

#Parameter Section =================================================================================== 

#Bound for solving for R0
 if (LH == "flatfish") { 
    low.bound <- 1500 ; upper.bound <- 15000 ; first.R0 <- 10000}
 
 if (LH == "rockfish") { 
    low.bound <- 500  ; upper.bound <- 15000 ; first.R0 <- 13000 }
 
  
#---------------------------------------------------------------------------------------------------
for (nsim in start.n:end.n)
 {
 
 #SS.survey.cv = survey.CV = 0.25
 #nsim = 1 ; sigmaR = 0 #; survey.CV = 0

#Source in the data scenarios
 source(paste(drive,":/PhD/Chapter3/code/Data_Scenarios.R",sep=""))

 #Save Output
 projections <- paste(directory,"save/om_proj_",nsim,sep="")
 estimates   <- paste(directory,"save/ss_ests_",nsim,sep="")
 
 #Read in the seeds 
 load(paste(drive,":/PhD/Chapter3/seed_list",sep=""))
 recruit.seed  <- as.numeric(seed.list[[1]][,"recruit.seed"])
 catch.seed    <- as.numeric(seed.list[[1]][,"catch.seed"])
 survey.seed   <- as.numeric(seed.list[[1]][,"survey.seed"])
 comp.seed     <- as.numeric(seed.list[[1]][,"comp.seed"])  
 jitter.seed   <- as.numeric(seed.list[[1]][,"spare1"]) 
  
 #Catch History -----------------------------------------------------------------------------------------------------------
 set.seed(catch.seed[nsim])
 catch.dev <- c(rnorm(10,0,0.75^2),rnorm(30,0,0.25^2),rnorm(10,0,0.40^2))
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
 rho      <- 0 #1 / sqrt(2)
 recdevs  <- rnorm(total.yrs, 0, sigmaR^2)
 autocorr <- rep(0, total.yrs)
 autocorr[1] <- recdevs[1]  
 for (e in 2:total.yrs) { 
    autocorr[e] <- rho*autocorr[e-1]+sqrt(1-rho*rho)*recdevs[e] 
 }
 
 #This will start the population in equilibrium
 #autocorr[1:ages] <- 0
 
 #Draw Survey Error---------------------------------------------------------------------------------------------------------------  
 set.seed(survey.seed[nsim])
 survey.err <<- rnorm(fishery.yrs, 0, survey.CV^2)

 #Calculate the buffer
 buffer <- exp(qnorm(p.value,0,sigma))

 #Setup the Jitter fraction for each simulation-----------------------------------------------------------------------------------
 set.seed(jitter.seed[nsim])
 jitter <- rnorm(ass.num, 0, 0.05)
 
 #Recruits Spawning biomass  Vulnerable biomas------------------------------------------------------------------------------------
 Update_Dynamics <- function(R0, catch = hist.catch, biology)
 {
 UpdateDyn <- list() 
 
 #Virgin Population Structure ----------------------------------------------------------------------------------------------------
 Ry[1]<- R0 / 2
 numbers[1,1:(ages-1),] <- (R0 / 2) * exp(-m * (0:(ages-2)))
 numbers[1,ages,]       <- numbers[1,ages-1,] * exp( -m ) / (1 - exp(-m)) 
    
 #Virgin Biomass By Age  
 SSB0 <- SSB[1] <- sum(numbers[1,,1] * biology$fecund)
 
 #Find F values based on catch-----------------------------------------------------------------------------------------------------
 Findf <- function(f){
    z.m <- (1 - exp(-(m + biology$selec.age.m * f))) / (m + biology$selec.age.m * f)
    z.f <- (1 - exp(-(m + biology$selec.age.f * f))) / (m + biology$selec.age.f * f)
    #Catch at Age
    catch.at.age.f <- f * (numbers[y,,1] * biology$selec.age.f) * z.f
    catch.at.age.m <- f * (numbers[y,,2] * biology$selec.age.m) * z.m
    #Catch At Length
    mid.temp.f <- numbers[y,,1] * z.f
    mid.temp.m <- numbers[y,,2] * z.m
    catch.at.len.f <- ((biology$mid.phi.f * biology$selec[,1]) %*% (mid.temp.f))
    catch.at.len.m <- ((biology$mid.phi.m * biology$selec[,2]) %*% (mid.temp.m))
        
    #Catch in Weight by Sex, mid.wght (41X2) calculated in the GetWght() function  
    catch.wght <- f * (sum(biology$mid.wght.at.len[,1] * catch.at.len.f) +
                           sum(biology$mid.wght.at.len[,2] * catch.at.len.m))    
 
    output <- NULL
    output$catch.at.len.f <- catch.at.len.f
    output$catch.at.len.m <- catch.at.len.m
    output$catch.wght <- catch.wght
    output$catch.at.age.f <- catch.at.age.f
    output$catch.at.age.m <- catch.at.age.m
    return(output)
 } #End FindF function
 
 cmp_F <- cmpfun(Findf)
 
 #Objective Function----------------------------------------------------------------------------------------------------------
 Obj.Fun.F <- function(f) {
    obj.fun.f <- (Findf(f)$catch.wght - hist.catch[y])^2
    return(obj.fun.f) 
 }
      
 for(y in 1:(pre.fishery.yrs+setup.yrs)) {  
    f <- ifelse(y > pre.fishery.yrs, optimize(Obj.Fun.F, lower=0, upper=2)$minimum, 0)
    if(f > 2) print(paste("F > 2 in year",y))
      f.values[y] <- f
      
    find.f <- Findf(f)
    catch.at.age[y,,1]   <- find.f$catch.at.age.f
    catch.at.age[y,,2]   <- find.f$catch.at.age.m
    catch.at.len[y,,1]   <- find.f$catch.at.len.f
    catch.at.len[y,,2]   <- find.f$catch.at.len.m
    catch.wght.values[y] <- find.f$catch.wght
      
    # survival at age by gender
    S.f <- exp(-(m + biology$selec.age.f * f))
    S.m <- exp(-(m + biology$selec.age.m * f))
      
    #Update the numbers and remove the catch by applying the solved for f value
    numbers[y+1, 2:ages, 1] <- numbers[y, 1:(ages-1), 1] * S.f[1:(ages-1)]
    numbers[y+1, 2:ages, 2] <- numbers[y, 1:(ages-1), 2] * S.m[1:(ages-1)]
    numbers[y+1, ages, 1]   <- numbers[y+1, ages, 1] + numbers[y, ages, 1] * exp(-m - biology$selec.age.f[ages] * f)
    numbers[y+1, ages, 2]   <- numbers[y+1, ages, 2] + numbers[y, ages, 2] * exp(-m - biology$selec.age.m[ages] * f)
      
    SSB[y+1] <- sum(numbers[y+1, 2:ages, 1] * biology$fecund[2:ages])

    #Expected (and then realized) recruitment
    Ry[y+1] <- (4 * steep * ( R0 / 2 ) * SSB[y+1]) / (SSB0 * (1 - steep) + SSB[y+1] * (5 * steep - 1))  
    Ry[y+1] <- Ry[y+1] * exp(-0.5 * (sigmaR^2)) * exp(autocorr[y+1])
    numbers[y+1,1,] <- Ry[y+1]
         
 } #closes yearly loop
 
 UpdateDyn[[1]] <- f.values
 UpdateDyn[[2]] <- catch.wght.values
 UpdateDyn[[3]] <- catch.at.age
 UpdateDyn[[4]] <- catch.at.len
 UpdateDyn[[5]] <- numbers
 UpdateDyn[[6]] <- SSB
 UpdateDyn[[7]] <- Ry
 names(UpdateDyn) <- c("f.values","catch.wght.values","catch.at.age","catch.at.len","numbers","SSB","Ry")
 return(UpdateDyn)
}

#=================================================================================================================================

 #Objective Function to solve for R0
 Test.R0 <- function(R0) {
     SSB<- Update_Dynamics(R0, catch=hist.catch, biology = cmp_bio())$SSB
     obj.fun <- (SSB[pre.fishery.yrs + setup.yrs] / SSB[pre.fishery.yrs] - final.depl)^2
     return(obj.fun) 
 }

 #Find R0 that results in the correct depletion level
 start.time <- Sys.time()
 r0.value   <- optimize(Test.R0, interval = c(low.bound, upper.bound))
 end.time   <- Sys.time()
 R0 <- r0.value$minimum
 end.time <- Sys.time()
 print(end.time-start.time)

 # Create Virgin population
 dyn <- Update_Dynamics(R0, catch = hist.catch, biology = Get_Biology())
 Dyn <- list() 
 Dyn[[1]] <- dyn$numbers
 Dyn[[2]] <- dyn$SSB
 Dyn[[3]] <- dyn$Ry
 Dyn[[4]] <- dyn$SSB / dyn$SSB[pre.fishery.yrs]
 Dyn[[5]] <- dyn$f.values
 Dyn[[6]] <- dyn$z.rate
 Dyn[[7]] <- dyn$catch.wght.values
 Dyn[[8]] <- dyn$catch.at.age
 Dyn[[9]] <- dyn$catch.at.len
 names(Dyn) <- c("numbers", "SSB", "Ry", "Depl", "f.values", "z.rate", "catch.wght.values", "catch.at.age", "catch.at.len")
 rm(dyn) 

#Format Data ----------------------------------------------------------------------------------------------------------
 #Get the survey data from the start of the survey until year 50
 survey.dur     <- start.survey:setup.yrs
 survey.out     <- Do_Survey(biology = Get_Biology(), f.values = Dyn$f.values , numbers = Dyn$numbers, 
                                     index = survey.dur)
 survey.proj    <- numeric(fishery.yrs)
 survey.proj[survey.dur]    <- survey.out$temp.index
 survey.catch.age.len[survey.dur,,,] <- survey.out$temp.cal
 
 #Control file M estimation
 m.f.est  <- m  #set the initial prior equal to the true M
 m.m.est  <- m  #set the initial prior equal to the true M
 do.est   <- 2  #phase for M estimation

 
#Generate Data for the base fishery years -----------------------------------------------------------------------------
 #Bring in the needed population values from the fishery
 catch.at.age         <- Dyn$catch.at.age[(pre.fishery.yrs+1):(pre.fishery.yrs+fishery.yrs), , ]
 catch.at.len         <- Dyn$catch.at.len[(pre.fishery.yrs+1):(pre.fishery.yrs+fishery.yrs), , ]
 
 #Set the seed for the composition sampling
 set.seed(comp.seed[nsim])

 for (a in 1:length(survey.dur)) {
     #Format the survey data for sampling
     ind                <- survey.dur[a]
     survey.catch.len   <- cbind(apply(survey.catch.age.len[ind,,,1],2,sum), apply(survey.catch.age.len[ind,,,2], 2, sum))
     survey.catch.age   <- cbind(apply(survey.catch.age.len[ind,,,1],1,sum), apply(survey.catch.age.len[ind,,,2], 1, sum))
   
     f.lengths[ind,]    <- Multinom_Lengths(catch.type = catch.at.len[ind,,], len.samp = f.len.samp[ind]) 
     s.lengths[ind,]    <- Multinom_Lengths(catch.type = survey.catch.len,    len.samp = s.len.samp[ind])
     
     age.out            <- Multinom_Ages(catch.type = catch.at.age[ind,,], age.samp = f.age.samp[ind] ,AgeError = FALSE )
     f.a.ca[ind,]       <- age.out
     f.sample.size[ind] <- sum(age.out)
     
     age.out            <- Multinom_Ages(catch.type = survey.catch.age , age.samp=s.age.samp[ind] , AgeError=FALSE ) 
     s.a.ca[ind,]       <- age.out
     s.sample.size[ind] <- sum(age.out)
 } 
 
 #Format the data for SS
 SS.survey.data      <<- cbind(survey.dur, 
                               rep(1, length(survey.dur)),
                               rep(2, length(survey.dur)), 
                               survey.proj[survey.dur], 
                               rep(SS.survey.cv, 
                               length(survey.dur))) 
 fishery.length.data <<- cbind(survey.dur, 
                               rep(1, length(survey.dur)), 
                               rep(1, length(survey.dur)), 
                               rep(3, length(survey.dur)),
                               rep(2, length(survey.dur)), 
                               f.len.samp[survey.dur], 
                               f.lengths[survey.dur, ])
 survey.length.data  <<- cbind(survey.dur, 
                               rep(1, length(survey.dur)), 
                               rep(2, length(survey.dur)), 
                               rep(3, length(survey.dur)),
                               rep(2, length(survey.dur)), 
                               s.len.samp[survey.dur], 
                               s.lengths[survey.dur, ])
 fishery.age.data    <<- cbind(survey.dur, 
                               rep(1, length(survey.dur)), 
                               rep(1, length(survey.dur)), 
                               rep(3, length(survey.dur)), 
                               rep(0, length(survey.dur)), 
                               rep(1, length(survey.dur)), 
                               rep(-1, length(survey.dur)), 
                               rep(-1, length(survey.dur)),
                               f.sample.size[survey.dur], 
                               f.a.ca[survey.dur, ])
 survey.age.data     <<- cbind(survey.dur, 
                               rep(1, length(survey.dur)), 
                               rep(2, length(survey.dur)), 
                               rep(3, length(survey.dur)), 
                               rep(0, length(survey.dur)), 
                               rep(1, length(survey.dur)), 
                               rep(-1, length(survey.dur)), 
                               rep(-1, length(survey.dur)),
                               s.sample.size[survey.dur], 
                               s.a.ca[survey.dur, ])
                            
 n.length.obs        <<- length(fishery.length.data[,6]) + length(survey.length.data[,6])
 n.age.obs           <<- length(fishery.age.data[,9])    + length(survey.age.data[,9])   
 


#Project with the estimated harvest----------------------------------------------------------------------------------------
 setwd(directory)
 Proj <- Est       <- list()
 biology           <- Get_Biology()
 #Pull in values from the Dynamic Setup Years Section
 f.values          <- Dyn$f.values 
 numbers           <- Dyn$numbers
 SSB               <- Dyn$SSB
 SSB0              <- Dyn$SSB[pre.fishery.yrs]
 Ry                <- Dyn$Ry
 catch.at.age      <- Dyn$catch.at.age
 catch.at.len      <- Dyn$catch.at.len
 catch.wght.values <- Dyn$catch.wght.values
 
 #Pull in values from the original survey year
 vul.total.obs[survey.dur]     <- survey.out$temp.vul.total
 index.expect[survey.dur]      <- survey.out$temp.index
 
 true.ofl          <- numeric(fishery.yrs)
 true.f            <- numeric(fishery.yrs)
 fore.catch        <- numeric(total.yrs)
 fore.catch[1:(pre.fishery.yrs + setup.yrs)] <- hist.catch
 
 counter = 0
 decl.overfished = FALSE
 rerun = 0
 

 #Find F values based on catch-----------------------------------------------------------------------------------------------------
 Findf <- function(f){
    z.m <- (1 - exp(-(m + biology$selec.age.m * f))) / (m + biology$selec.age.m * f)
    z.f <- (1 - exp(-(m + biology$selec.age.f * f))) / (m + biology$selec.age.f * f)
    #Catch at Age
    catch.at.age.f <- f * (numbers[y,,1] * biology$selec.age.f) * z.f
    catch.at.age.m <- f * (numbers[y,,2] * biology$selec.age.m) * z.m
    #Catch At Length
    mid.temp.f <- numbers[y,,1] * z.f
    mid.temp.m <- numbers[y,,2] * z.m
    catch.at.len.f <- ((biology$mid.phi.f * biology$selec[,1]) %*% (mid.temp.f))
    catch.at.len.m <- ((biology$mid.phi.m * biology$selec[,2]) %*% (mid.temp.m))
        
    #Catch in Weight by Sex, mid.wght (41X2) calculated in the GetWght() function  
    catch.wght <- f * (sum(biology$mid.wght.at.len[,1] * catch.at.len.f) +
                           sum(biology$mid.wght.at.len[,2] * catch.at.len.m))    
 
    output <- NULL
    output$catch.at.len.f <- catch.at.len.f
    output$catch.at.len.m <- catch.at.len.m
    output$catch.wght <- catch.wght
    output$catch.at.age.f <- catch.at.age.f
    output$catch.at.age.m <- catch.at.age.m
    return(output)
 } #End FindF function
 
 #Objective Function----------------------------------------------------------------------------------------------------------
 Obj.Fun.F <- function(f) {
    obj.fun.f <- (Findf(f)$catch.wght - fore.catch[y])^2
    return(obj.fun.f) 
 }
  
 for (y in (pre.fishery.yrs + setup.yrs + 1):total.yrs) {
    do.ass = y -1
    if(LH == "flatfish") { do.ass = y - 3}     
    if ( do.ass %% 4 == 0 ){
        counter = counter + 1
        
        writeStarter(starter = "starter.ss")
        writeForecast(forecast = "forecast.ss", y = y)
        writeCtl(ctl = "sim.ctl", y = y)
        writeDat(dat = "sim.dat", y = y, SS.survey.data = SS.survey.data, fore.catch = fore.catch)
        
        #Read in the Report File to run the bias ramp code 
        if (y <= (pre.fishery.yrs + setup.yrs + 9)) {
            if (tantalus == T) { system("./SS3  > test.txt 2>&1")  }
            if (tantalus == F) { shell("ss3_opt.exe > test.txt 2>&1")  }
            #Check for convergence
            rep.new   <- readLines(paste(directory, "Report.sso", sep=""))
            gradiant  <- as.numeric(strsplit(rep.new[grep(paste("Convergence_Level",sep=""),rep.new)]," ")[[1]][2])
            virgin.SB <- as.numeric(strsplit(rep.new[grep(paste("SPB_Virgin",sep=""),rep.new)]," ")[[1]][3])
            while(virgin.SB < (SSB0/4) || virgin.SB > (SSB0*4)) {
              rerun = rerun + 1  
              if (tantalus == T) { system("./SS3  > test.txt 2>&1")  }
              if (tantalus == F) { shell("ss3_opt.exe > test.txt 2>&1")  }
              rep.new   <- readLines(paste(directory, "Report.sso", sep=""))
              virgin.SB <- as.numeric(strsplit(rep.new[grep(paste("SPB_Virgin",sep=""),rep.new)]," ")[[1]][3])
              if (virgin.SB > (SSB0/4) && virgin.SB < (SSB0*4)) {
                break()
              }
            }
            
            #Read model and rump bias ramp adjustment function
            rep.bias     <- SS_output(directory, covar = TRUE, printstats = FALSE)
            new.bias     <- SS_fitbiasramp(rep.bias, 
                            startvalues = c(start.bias, full.bias , last.bias, last.no.bias ,max.bias.adj))
            start.bias   <- new.bias$df[1,1]
            full.bias    <- new.bias$df[2,1]
            max.bias.adj <- new.bias$df[5,1]
        } 
                       
        #The declining slope is sometimes estimated poorly so do not use those values
        last.bias    <- y - pre.fishery.yrs - 10
        last.no.bias <- y - pre.fishery.yrs - 2
        #last.bias <- new.bias$df[3,1]
        #last.no.bias <- new.bias$df[4,1]
            
        if(data.scenario == 'greathist'  && decl.overfished == TRUE ) {
            last.bias    <- setup.yrs + 1 - 10
            last.no.bias <- setup.yrs + 1 - 2
        }
 
        writeCtl(ctl = "sim.ctl", y = y)
        file.copy("sim.ctl", paste(directory,"/ctl/sim.ctl",sep =""))
        file.rename(paste(directory,"/ctl/sim.ctl",sep =""), 
                        paste(directory,"/ctl/sim",nsim,"_",y-pre.fishery.yrs,".ctl",sep =""))    
                        
        file.copy("sim.dat", paste(directory,"/ctl/sim.dat",sep =""))
        file.rename(paste(directory,"/ctl/sim.dat",sep =""), 
                        paste(directory,"/ctl/sim",nsim,"_",y-pre.fishery.yrs,".dat",sep =""))        
        
        if (tantalus == T) { system("./SS3 -nohess > test.txt 2>&1")  }
        if (tantalus == F) { shell("ss3_opt.exe -nohess > test.txt 2>&1") }
        
        rep.new   <- readLines(paste(directory, "Report.sso", sep=""))
        gradiant  <- as.numeric(strsplit(rep.new[grep(paste("Convergence_Level",sep=""),rep.new)]," ")[[1]][2])
        virgin.SB <- as.numeric(strsplit(rep.new[grep(paste("SPB_Virgin",sep=""),rep.new)]," ")[[1]][3])
        while(virgin.SB < (SSB0/4) || virgin.SB > (SSB0*4)) {
              rerun = rerun + 1  
              if (tantalus == T) { system("./SS3  > test.txt 2>&1")  }
              if (tantalus == F) { shell("ss3_opt.exe > test.txt 2>&1")  }
              rep.new   <- readLines(paste(directory, "Report.sso", sep=""))
              virgin.SB <- as.numeric(strsplit(rep.new[grep(paste("SPB_Virgin",sep=""),rep.new)]," ")[[1]][3])
              if (virgin.SB > (SSB0/4) && virgin.SB < (SSB0*4)) {
                break()
              }
        }

        #Read in the Report File and Save Quantities 
        rep.out   <- Rep_Summary(rep.new, y, pre.fishery.yrs)
        fore.out  <- readLines(paste(directory, "Forecast-report.sso", sep=""))
        fmult     <- as.numeric(strsplit(fore.out[grep(paste("Fmult",sep=""),fore.out)]," ")[[4]][2])
        ind       <- y - pre.fishery.yrs - 1 
        TotBio[1:ind,counter]    <- rep.out$TotBio
        VulBioEst[start.survey:ind,counter] <- rep.out$VulBioEst
        Recruits[1:ind,counter]  <- rep.out$Recruits
        
        OFL[y:(y+3)]          <- rep.out$OFL
        ForeCat[y:(y+3)]      <- rep.out$ForeCatch
        FSPR[,counter]        <- rep.out$FSPR
        Fmult[,counter]       <- fmult
        M.store[,counter]     <- rep.out$M
        R0.out[,counter]      <- rep.out$R0
        F.selex[,counter]     <- rep.out$FSelex
        S.selex[,counter]     <- rep.out$SSelex
        ind                   <- y - pre.fishery.yrs
        SB[1:ind,counter]     <- rep.out$SB
        Bratio[1:ind,counter] <- rep.out$Depl
        Gradiant.out[,counter]<- gradiant
        
        
        Est[[1]] <- TotBio
        Est[[2]] <- VulBioEst
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
        Est[[14]]<- Gradiant.out
        Est[[15]]<- rerun
        
        names(Est) <- c("TotBio","VulBioEst","OFL","ForeCat","Fmult","FSPR","M.store","R0.out","SB","Bratio","F.selex","S.selex","Recruits",
                          "Gradiant.out", "rerun")
        save(Est, file=estimates)  
        
        #Pull the ACL from the Report File and use that for the next two years 
        fore.catch[y:(y+3)]         <- rep.out$ForeCatch 
        
        if (decl.overfished == FALSE) {
            if(Bratio[ind,counter] < over.thres) {
                decl.overfished <- TRUE
            }
        }
    
        if (decl.overfished == TRUE) {
            if(Bratio[ind,counter] >= ctl.rule.tgt) {
            decl.overfished <- FALSE
            }
        }
        
        if(estimate.m == FALSE) {
            #Control file M estimation
            #Sets the prior and the starting value equal to the estimation from the first assessment
            m.f.est  <- M.store[1,counter]  
            m.m.est  <- M.store[2,counter]
            if(decl.overfished == TRUE)  { 
                do.est <- -2  #Phase for M Estimation
            }
            if(decl.overfished == FALSE) { 
                m.f.est <- m.m.est <- m
                do.est  <-  2  #Phase for M Estimation
            }  
        }
        
    } #Close the assessment loop  
   
                                    
    #Calculate what the true F should be based on the HCR
    if (SSB[y-1] / SSB[pre.fishery.yrs+1] >= ctl.rule.tgt) {
        Fadj      <- Fmsy 
    }
    if (SSB[y-1] / SSB[pre.fishery.yrs+1] < ctl.rule.tgt) {
        Fadj      <- (Fmsy * ((SSB[y-1] / SSB[pre.fishery.yrs+1]) - ctl.rule.thres) * ctl.rule.tgt) / 
                    ((ctl.rule.tgt - ctl.rule.thres) * (SSB[y-1] / SSB[pre.fishery.yrs+1]))
    }
    if (SSB[y-1] / SSB[pre.fishery.yrs+1] < ctl.rule.thres) {
        Fadj      <- 0 
    }
      
    true.f[y]   <- Fadj
    true.ofl[y] <- Findf(Fadj)$catch.wght  
    
    #Determine the F value from the estimated ACL      
    f                    <- optimize(Obj.Fun.F, lower = 0, upper = 2)$minimum  
    f.values[y]          <- f 
    find.f               <- Findf(f)
    catch.at.age[y,,1]   <- find.f$catch.at.age.f
    catch.at.age[y,,2]   <- find.f$catch.at.age.m
    catch.at.len[y,,1]   <- find.f$catch.at.len.f
    catch.at.len[y,,2]   <- find.f$catch.at.len.m
    catch.wght.values[y] <- find.f$catch.wght
         
    # survival at age by gender
    S.f                <- exp(-(m + biology$selec.age.f * f))
    S.m                <- exp(-(m + biology$selec.age.m * f))
      
    #Update the numbers and remove the catch by applying the solved for f value
    numbers[y+1, 2:ages, 1] <- numbers[y, 1:(ages-1), 1] * S.f[1:(ages-1)]
    numbers[y+1, 2:ages, 2] <- numbers[y, 1:(ages-1), 2] * S.m[1:(ages-1)]
    numbers[y+1, ages, 1]   <- numbers[y+1, ages, 1] + numbers[y, ages, 1] * exp(-m - biology$selec.age.f[ages] * f)
    numbers[y+1, ages, 2]   <- numbers[y+1, ages, 2] + numbers[y, ages, 2] * exp(-m - biology$selec.age.m[ages] * f)
      
    SSB[y+1] <- max(0, sum(numbers[y+1, 2:ages, 1] * biology$fecund[2:ages]))

    #Expected (and then realized) recruitment
    Ry[y+1] <- (4 * steep * (R0 / 2) * SSB[y+1]) / (SSB0 * (1-steep) + SSB[y+1] * (5 * steep - 1))
    Ry[y+1] <- Ry[y+1] * exp(-0.5 * (sigmaR^2)) * exp(autocorr[y+1])
         
    numbers[y+1,1,] <- Ry[y+1]
      
    
    #Determine the data collection levels based on status-------------------- 
    #There is no more data regardless of status in the projection period   
    if (data.scenario == "greathist") {
      ind             <- y - pre.fishery.yrs
      f.len.samp[ind] <- 0
      s.len.samp[ind] <- 0
      f.age.samp[ind] <- 0
      s.age.samp[ind] <- 0
    }
    #Always have data available regardless of status (overfished status does not impact sampling)
    if (data.scenario == "greatall" || data.scenario == "test") {
      ind             <- y - pre.fishery.yrs
      f.len.samp[ind] <- f.len.samp[ind]
      s.len.samp[ind] <- s.len.samp[ind]
      f.age.samp[ind] <- f.age.samp[ind]
      s.age.samp[ind] <- s.age.samp[ind]
    }
    #Reduce the data during the period that the stock is overfished
    if (data.scenario == "normal") {
        if (decl.overfished  == TRUE) {
            ind             <- y - pre.fishery.yrs
            f.len.samp[ind] <- floor(0.25 * f.len.samp[ind])
            s.len.samp[ind] <- s.len.samp[ind]
            f.age.samp[ind] <- floor(0.25 * f.age.samp[ind])
            s.age.samp[ind] <- s.age.samp[ind]
        }
    } 
    
    #Observation Index------------------------------------------------------------------------------------------------------------
    ind                          <- y - pre.fishery.yrs
    survey.out                   <- Do_Survey(biology = Get_Biology(), f.values = f.values , numbers = numbers, index = ind)
    survey.proj[ind]             <- survey.out$temp.index
    vul.total.obs[ind]           <- survey.out$temp.vul.total
    survey.catch.age.len[ind,,,] <- survey.out$temp.cal
    survey.dur                   <- start.survey:ind
    
    #Format data for SS 
    SS.survey.data <- cbind(survey.dur, 
                            rep(1, length(survey.dur)),
                            rep(2, length(survey.dur)), 
                            survey.proj[survey.dur], 
                            rep(SS.survey.cv, 
                            length(survey.dur))) 
                    
    #Get Composition Data    
    survey.catch.len   <- cbind(apply(survey.catch.age.len[ind,,,1],2,sum), apply(survey.catch.age.len[ind,,,2], 2, sum))
    survey.catch.age   <- cbind(apply(survey.catch.age.len[ind,,,1],1,sum), apply(survey.catch.age.len[ind,,,2], 1, sum))
    f.lengths[ind,]    <- Multinom_Lengths(catch.type = catch.at.len[pre.fishery.yrs + ind,,], len.samp = f.len.samp[ind]) 
    s.lengths[ind,]    <- Multinom_Lengths(catch.type = survey.catch.len,    len.samp = s.len.samp[ind])
    age.out            <- Multinom_Ages(catch.type = catch.at.age[pre.fishery.yrs + ind,,], age.samp = f.age.samp[ind])
    f.a.ca[ind,]       <- age.out
    f.sample.size[ind] <- sum(age.out)
    age.out            <- Multinom_Ages(catch.type = survey.catch.age, age.samp = s.age.samp[ind]) 
    s.a.ca[ind,]       <- age.out
    s.sample.size[ind] <- sum(age.out)
 
    #Format the data for SS
    if(f.len.samp[ind] > 0) {
    #Only want the years where sample > 0
    i <- f.len.samp[start.survey:(y-pre.fishery.yrs-1)] > 0
    fishery.length.data <<- cbind(survey.dur[i], 
                                  rep(1, length(survey.dur[i])), 
                                  rep(1, length(survey.dur[i])), 
                                  rep(3, length(survey.dur[i])),
                                  rep(2, length(survey.dur[i])), 
                                  f.len.samp[survey.dur[i]], 
                                  f.lengths[survey.dur[i], ])
    }
    if(s.len.samp[ind] > 0) {
    i <- s.len.samp[start.survey:(y-pre.fishery.yrs-1)] > 0
    survey.length.data  <<- cbind(survey.dur[i], 
                                  rep(1, length(survey.dur[i])), 
                                  rep(2, length(survey.dur[i])), 
                                  rep(3, length(survey.dur[i])),
                                  rep(2, length(survey.dur[i])), 
                                  s.len.samp[survey.dur[i]], 
                                  s.lengths[survey.dur[i], ]) 
    }
    if(f.age.samp[ind] > 0) { 
    i <- f.age.samp[start.survey:(y-pre.fishery.yrs-1)] > 0
    fishery.age.data    <<- cbind(survey.dur[i], 
                                  rep(1, length(survey.dur[i])), 
                                  rep(1, length(survey.dur[i])), 
                                  rep(3, length(survey.dur[i])), 
                                  rep(0,length(survey.dur[i])), 
                                  rep(1, length(survey.dur[i])), 
                                  rep(-1, length(survey.dur[i])), 
                                  rep(-1, length(survey.dur[i])),
                                  f.age.samp[survey.dur[i]], 
                                  f.a.ca[survey.dur[i], ])
    }
    if(s.age.samp[ind] > 0) {
    i <- s.age.samp[start.survey:(y-pre.fishery.yrs-1)] > 0
    survey.age.data     <<- cbind(survey.dur[i], 
                                  rep(1, length(survey.dur[i])), 
                                  rep(2, length(survey.dur[i])), 
                                  rep(3, length(survey.dur[i])), 
                                  rep(0, length(survey.dur[i])), 
                                  rep(1, length(survey.dur[i])), 
                                  rep(-1, length(survey.dur[i])), 
                                  rep(-1, length(survey.dur[i])),
                                  s.age.samp[survey.dur[i]], 
                                  s.a.ca[survey.dur[i], ]) 
    }
    
    n.length.obs        <<- length(fishery.length.data[,6]) + length(survey.length.data[,6])
    n.age.obs           <<- length(fishery.age.data[,9])    + length(survey.age.data[,9])

     
    Proj[[1]] <- f.values 
    Proj[[2]] <- catch.wght.values 
    Proj[[3]] <- catch.at.age
    Proj[[4]] <- catch.at.len
    Proj[[5]] <- numbers 
    Proj[[6]] <- SSB
    Proj[[7]] <- SSB / SSB[pre.fishery.yrs+1]
    Proj[[8]] <- Ry
    Proj[[9]] <- index.expect
    Proj[[10]] <- vul.total.obs
    Proj[[11]]<- fore.catch
    Proj[[12]]<- true.ofl
    Proj[[13]]<- true.f
    Proj[[14]]<- f.len.samp
    Proj[[15]]<- s.len.samp
    Proj[[16]]<- f.age.samp
    Proj[[17]]<- s.age.samp

    names(Proj) <- c("f.values", "catch.wght.values", "catch.at.age", "catch.at.len", "numbers", "SSB", "depl","Ry", "index.expect",
                "vul.total.obs", "fore.catch","true.ofl", "true.f","f.len.samp","s.len.sam","f.age.samp","s.age.samp")
    save(Proj, file=projections)  
        
   } #closes yearly projection loop

  print(paste("Simulation",nsim, "Done", sep =" "))
 } #end simulation loop
 
} #function loop
