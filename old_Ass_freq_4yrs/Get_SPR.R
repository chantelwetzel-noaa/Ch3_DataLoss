 drive <-"F"
 LH <- "flatfish"
 SR <- "BH" #"Shep" 
 nsim <- NSIM <- 1
 start.survey <- 1
 require(compiler)
 
 #Source in the life-history parameter values
 source(paste(drive,":/PhD/Chapter3/code/LH_parameter_values.R",sep=""))
 
 #Source in external functions
 source(paste(drive,":/PhD/Chapter3/code/Functions.R",sep=""))

 fishery.yrs <- 1000 #add one year for following year analysis 
 setup.yrs   <- fishery.yrs
 total.yrs   <- fishery.yrs
 years       <- 1:fishery.yrs
 
 #Arrays----------------------------------------------------------------------------------------------------
  age.bins <- seq(1,num.ages,1)
  numbers <- array(0,dim=c(total.yrs,ages,sexes)) ; rownames(numbers)<-years
  biomass <- array(0,dim=c(total.yrs,ages,sexes)) 
  
  #Matrix Storage for output to R list
  SSB.matrix <- array(NA,dim=c(total.yrs,NSIM))
  depl.matrix <- array(NA,dim=c(total.yrs,NSIM))
  Ry.matrix <- array(NA,dim=c(total.yrs,NSIM))
  rec.matrix <- array(NA,dim=c(total.yrs,NSIM))
  vul.total.matrix <- array(NA,dim=c(total.yrs,NSIM))
  index.expect.matrix <- array(NA,dim=c(total.yrs,NSIM))
  vul.total.obs.matrix <- array(NA,dim=c(total.yrs,NSIM))
  f.values.matrix <- array(NA,dim=c((total.yrs-1),NSIM))
  fore.catch.matrix <- array(NA,dim=c((total.yrs+1),NSIM))
  true.ofl.matrix <- array(NA,dim=c((total.yrs-1),NSIM))
  vul.total.obs <- matrix(0, total.yrs, 1)
  rownames(vul.total.obs) <- years
  index.expect <- matrix(0,total.yrs,1) ; rownames(index.expect) <- years
  survey.catch.age.len <- array(NA,c(total.yrs,ages, length(len.step), sexes))
  Ry <- matrix(0,total.yrs,1);  rownames(Ry)<-years
  SSB<-matrix(0,total.yrs,1)  ; rownames(SSB)<-years
  catch.at.age <- array(NA, dim=c(total.yrs, ages, sexes))  ; rownames(catch.at.age) <- years
  catch.at.len <- array(NA, dim=c(total.yrs, length(len.step), sexes))
  f.values <- rep(0,(total.yrs-1))
  catch.wght.values <- rep(0,(total.yrs-1)) 
  z.rate <- array(NA,dim = c(total.yrs, ages, sexes)) ; rownames(z.rate) <- years
 
 R0 = 2000 ; steep = 1
 

#Recruits Spawning biomass  Vulnerable biomas---------------------------------------------------
Update_Dynamics <- function(f, biology)
 {
 UpdateDyn <- list() 
 
 #Virgin Population Structure ----------------------------------------------------------------------------------------------------
 Ry[1] <- R0 / 2
 numbers[1,1:(ages-1),] <- (R0 / 2)* exp(-m * (0:(ages-2)))
 numbers[1,ages,]       <- numbers[1,ages-1,] * exp( -m ) / (1 - exp(-m))
 catch.wght <- numeric(fishery.yrs) 
    
 #Virgin Biomass By Age  
 SSB0 <- SSB[1]     <- sum(numbers[1,,1] * biology$fecund) 
      
 for(y in 1:(fishery.yrs -1)) {
   
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
    catch.wght[y] <- f * (sum(biology$mid.wght.at.len[,1] * catch.at.len.f) +
                           sum(biology$mid.wght.at.len[,2] * catch.at.len.m))    
      
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
    numbers[y+1,1,] <- Ry[y+1]
         
 } #closes yearly loop
 
 UpdateDyn[[1]] <- NULL#f.values
 UpdateDyn[[2]] <- NULL#z.rate
 UpdateDyn[[3]] <- catch.wght
 UpdateDyn[[4]] <- catch.at.age
 UpdateDyn[[5]] <- catch.at.len
 UpdateDyn[[6]] <- numbers
 UpdateDyn[[7]] <- SSB
 UpdateDyn[[8]] <- Ry
 names(UpdateDyn) <- c("f.values","z.rate","catch.wght.values","catch.at.age","catch.at.len","numbers","SSB","Ry")
 return(UpdateDyn)
}

if (LH == "flatfish") {
    f<-uniroot(function(f)(Update_Dynamics(f, biology=Get_Biology())$SSB[fishery.yrs]/
                    Update_Dynamics(f, biology=Get_Biology())$SSB[1])-0.30,lower=0,upper=4,tol=0.0001) }
if (LH == "rockfish") {
    f<-uniroot(function(f)(Update_Dynamics(f, biology=Get_Biology())$SSB[fishery.yrs]/
                    Update_Dynamics(f,biology=Get_Biology())$SSB[1])-0.50,lower=0,upper=4,tol=0.0001) }
                    
out = Update_Dynamics(f$root, biology=Get_Biology())

steep = 0.60
f.values <- seq(0,0.40,0.01)
catch <- numeric(length(f.values))
for (i in 1:length(f.values)) {
    catch[i] <- Update_Dynamics(f.values[i], biology=Get_Biology())$catch.wght[fishery.yrs-1]
}
plot(f.values,catch)
