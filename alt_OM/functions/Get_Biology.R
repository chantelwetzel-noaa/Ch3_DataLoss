Get_Biology<- function()
{

#Calculate Lengths
 Len <- list()
 len <- matrix(NA,ages,sexes)
 mid.len <- matrix(0,ages,sexes)
 
 #L infinity (cm)
  Linf_f <- L1 + ((L2f - L1) / (1 - exp( -k * (a4 - a3))))
  Linf_m <- L1 + ((L2m - L1) / (1 - exp( -k * (a4 - a3))))
  len.slope <- (L1-len.step[1])/a3
 
 #Length at the start of the year (cm)
  len[1:(a.linear+1),]<-len.step[1]+len.slope*(seq(1,(a.linear+1),1)-1)  #For smallest fish length is a linear function  
  #Growth based on the VB
  len[(a.linear+2):ages,2]<-Linf_m+(L1-Linf_m)*exp(-k*((seq(a.linear+2,ages,1)-1)-a3))
  len[(a.linear+2):ages,1]<-Linf_f+(L1-Linf_f)*exp(-k*((seq(a.linear+2,ages,1)-1)-a3))
  #Plus Group Growth
  temp1 <- sum(exp(-0.20*(ages:(ages*2)-ages))*(len[ages,2]+((ages:(ages*2)-ages)/ages)*(Linf_m-len[ages,2])))
  temp2 <- sum(exp(-0.20*(ages:(ages*2)-ages)))
  len[ages,2] <- temp1/temp2
  
  temp1 <- sum(exp(-0.20*(ages:(ages*2)-ages))*(len[ages,1]+((ages:(ages*2)-ages)/ages)*(Linf_f-len[ages,1])))
  temp2 <- sum(exp(-0.20*(ages:(ages*2)-ages)))
  len[ages,1] <- temp1/temp2
  
   
 #Mid-year lengths   (cm)
  mid.len[1:a.linear,]<-mean(len[1:2,1])+len.slope*(seq(1,a.linear,1)-1)   #For smallest fish length is a linear function 
  #Growth bases on VB
  mid.len[(a.linear+1):ages,2]<-len[(a.linear+1):ages,2]+(len[(a.linear+1):ages,2]-Linf_m)*(exp(-0.5*k)-1) 
  mid.len[(a.linear+1):ages,1]<-len[(a.linear+1):ages,1]+(len[(a.linear+1):ages,1]-Linf_f)*(exp(-0.5*k)-1)
  #Plus Group Growth
  temp1 <- sum(exp(-0.20*(ages:(ages*2)-ages))*(mid.len[ages,2]+((ages:(ages*2)-ages)/ages)*(Linf_m-mid.len[ages,2])))
  temp2 <- sum(exp(-0.20*(ages:(ages*2)-ages)))
  mid.len[ages,2] <- temp1/temp2
  
  temp1 <- sum(exp(-0.20 * (ages:(ages * 2) - ages)) * (mid.len[ages,1] + 
                ((ages:(ages * 2) - ages) / ages) * (Linf_f - mid.len[ages,1])))
  temp2 <- sum(exp(-0.20 * (ages:(ages * 2) - ages)))
  mid.len[ages,1] <- temp1 / temp2
  
 
 Len[[1]] <- len
 Len[[2]] <- mid.len
 names(Len) <- c("len","mid.len")

#=================================================================================================================
#Calculate the Transisition Matrix
 Phi <- list()
 #St Dev of Mid-year lengths  (cm)
 mid.sigma <- matrix(0,ages,sexes) 
 sigma.phi <- matrix(0,ages,sexes)
 phi.m <- matrix(0,length(len.step),ages) 
 phi.f <- matrix(0,length(len.step),ages) 
 mid.phi.m <- matrix(0,length(len.step),ages) 
 mid.phi.f <- matrix(0,length(len.step),ages)

 #CV about Length at the start of year
 sigma.phi[1:a.linear,] <- Len$len[1:a.linear,] * CV1
 sigma.phi[(a.linear+1):floor(a4),] <- Len$len[(a.linear+1):floor(a4),] * (CV1+(Len$len[(a.linear+1):floor(a4),]
                                     - Len$len[a.linear,]) / (Len$len[ceiling(a4),] - Len$len[a.linear,]) * (CV2 - CV1))
 sigma.phi[ceiling(a4):ages,] <- Len$len[ceiling(a4):ages,] * CV2

 #CV about Length at the mid-year
 mid.sigma[1:a.linear,] <- Len$mid.len[1:a.linear,] * CV1
 mid.sigma[(a.linear+1):floor(a4),] <- Len$mid.len[(a.linear+1):floor(a4),] * (CV1+(Len$mid.len[(a.linear+1):floor(a4),]
                                      - Len$mid.len[a.linear,]) / (Len$mid.len[ceiling(a4),] - Len$mid.len[a.linear,]) * (CV2-CV1))
 mid.sigma[ceiling(a4):ages,] <- Len$mid.len[ceiling(a4):ages,] * CV2


 #Start of year phi matrix
 for (b in 1:ages) {   
    phi.m[1,b]<-pnorm(len.step[1+1],Len$len[b,2],sigma.phi[b,2])
    phi.f[1,b]<-pnorm(len.step[1+1],Len$len[b,1],sigma.phi[b,1])
    total.m <- phi.m[1,b]
    total.f<-phi.f[1,b] 
      for (a in 2:(length(len.step)-1)) {
       p1 <- pnorm(len.step[a+1],Len$len[b,2],sigma.phi[b,2])
       p2 <- pnorm(len.step[a+1],Len$len[b,1],sigma.phi[b,1])
       phi.m[a,b] <- p1-total.m
       phi.f[a,b] <- p2-total.f
       total.m <- p1
       total.f <- p2
      } 
    phi.m[bin,b] <- 1-total.m
    phi.f[bin,b] <- 1-total.f
 }
        
  
 #Mid year phi matrix
  for (b in 1:ages) 
   {   
    mid.phi.m[1,b]<-pnorm(len.step[1+1],Len$mid.len[b,2],mid.sigma[b,2])
    mid.phi.f[1,b]<-pnorm(len.step[1+1],Len$mid.len[b,1],mid.sigma[b,1])
    total.m <- mid.phi.m[1,b]
    total.f<-mid.phi.f[1,b] 
     for (a in 2:(length(len.step)-1)) 
      {
       p1 <- pnorm(len.step[a+1],Len$mid.len[b,2],mid.sigma[b,2])
       p2 <- pnorm(len.step[a+1],Len$mid.len[b,1],mid.sigma[b,1])
       mid.phi.m[a,b] <- p1-total.m
       mid.phi.f[a,b] <- p2-total.f
       total.m <- p1
       total.f <- p2
      } 
    mid.phi.m[bin,b] <- 1-total.m
    mid.phi.f[bin,b] <- 1-total.f
   }
   
 Phi[[1]] <- mid.sigma
 Phi[[2]] <- sigma.phi
 Phi[[3]] <- phi.m
 Phi[[4]] <- phi.f
 Phi[[5]] <- mid.phi.m
 Phi[[6]] <- mid.phi.f
 names(Phi) <- c("mid.sigma","sigma.phi","phi.m","phi.f","mid.phi.m","mid.phi.f")
 
#GetWght==========================================================================================
 Wght <- list()
 
 mid.wght        <- matrix(0,ages,sexes)
 sample.mid.wght <- matrix(0,ages,sexes)
 wght            <- matrix(NA,ages,sexes)
 wght.at.len     <- matrix(0,length(len.step),sexes)
 mid.wght.at.len <- matrix(0,length(mid.len.step),sexes)

 #Virgin Weight @ Length (kg)
 wght.at.len[,2]     <- (wght.coef.m * (len.step) ^ wght.exp.m)
 wght.at.len[,1]     <- (wght.coef.f * (len.step) ^ wght.exp.f)
 mid.wght.at.len[,2] <- (wght.coef.m * (mid.len.step) ^ wght.exp.m)  #this is the value being outputted in ss3
 mid.wght.at.len[,1] <- (wght.coef.f * (mid.len.step) ^ wght.exp.f)

 #Virgin Weight @ Age  (kg)
 wght[,2]      <- (t(Phi$phi.m)) %*% mid.wght.at.len[,2]   
 wght[,1]      <- (t(Phi$phi.f)) %*% mid.wght.at.len[,1]
 mid.wght[,2]  <- (t(Phi$mid.phi.m)) %*% mid.wght.at.len[,2]
 mid.wght[,1]  <- (t(Phi$mid.phi.f)) %*% mid.wght.at.len[,1]    

 Wght[[1]] <- wght.at.len
 Wght[[2]] <- mid.wght.at.len 
 Wght[[3]] <- wght
 Wght[[4]] <- mid.wght
 names(Wght) <- c("wght.at.len","mid.wght.at.len","wght","mid.wght")

#Fecundity================================================================================================
 Fecund <- list()
 
 mature.len <- 1 / (1 + exp((ohm3) * (mid.len.step-ohm4)))#SS calcs using mid lengths
 #mature.age <- (t(Phi$mid.phi.f)) %*% mature.len  #SS uses mid phi values
 mature.age <- (t(Phi$phi.f)) %*% mature.len  #SS uses mid phi values
 eggs <- ohm5 + ohm6 * Wght$wght.at.len[,1]  # eggs per kg

 fecund <- rep(0,ages)
 fecund <- (t(Phi$phi.f)) %*% (mature.len *eggs *Wght$mid.wght.at.len[,1])

 Fecund[[1]] <- mature.age
 Fecund[[2]] <- mature.len
 Fecund[[3]] <- fecund
 names(Fecund) <- c("mature.age","mature.len","fecund")

#Obs_Selectivity =========================================================================================================================================
 Obs.Selex <- list()
 obs.selec<-matrix(0,length(mid.len.step),sexes) 
 obs.selec.age.m<-matrix(0,ages,1)
 obs.selec.age.f<-matrix(0,ages,1)

  #Survey Selectivity pattern Double Normal 
  startbin <- 1
  peak <- ssp1
  upselex <- exp(ssp3)
  downselex <- exp(ssp4)
  final <- ssp6

  point1 <- 1/(1+exp(-ssp5))
  t1min <- exp(-((len.step[startbin]+1)-peak)^2/upselex)
  peak2 <- peak + 2 + (0.99*(len.step[1]+1)-peak-2)^2/(1+exp(-ssp2))
  point2 <- 1/(1+exp(-final))
  t2min <- exp(-((len.step[length(len.step)]+1)-peak2)^2/downselex)
  t1 <- len.step+1-peak
  t2 <- len.step+1-peak2
  join1 <- 1/(1+exp(-(20/(1+abs(t1)))*t1))
  join2 <- 1/(1+exp(-(20/(1+abs(t2)))*t2))
  asc <- point1 +(1-point1)*(exp(-t1^2/upselex)-t1min)/(1-t1min)
  dsc <- 1 +(point2-1)*exp(-t2^2/downselex-1)/(t2min-1)

  obs.selec[,1] <- asc*(1-join1)+join1*(1-join2+dsc*join2) 
  obs.selec[,2]<-obs.selec[,1]
 
  #Mid-year Selectivity by Age
  obs.selec.age.m <-(t(Phi$mid.phi.m))%*%obs.selec[,2]
  obs.selec.age.f <-(t(Phi$mid.phi.f))%*%obs.selec[,1]

  Obs.Selex[[1]] <- obs.selec
  Obs.Selex[[2]] <- obs.selec.age.m
  Obs.Selex[[3]] <- obs.selec.age.f
  names(Obs.Selex) <- c("obs.selec","obs.selec.age.m","obs.selec.f")

#=======================================================================================================

 Bio <- list()
 Bio[[1]] <- Phi$phi.f
 Bio[[2]] <- Phi$phi.m
 Bio[[3]] <- Phi$mid.phi.m
 Bio[[4]] <- Phi$mid.phi.f
 Bio[[5]] <- Wght$mid.wght
 Bio[[6]] <- Wght$wght
 Bio[[7]] <- Wght$mid.wght.at.len
 Bio[[8]] <- Wght$wght.at.len
 Bio[[9]] <- Len$mid.len
 Bio[[10]]<- Wght$wght
 Bio[[11]]<- Len$len
 Bio[[12]]<- Fecund$fecund
 Bio[[13]]<- Phi$sigma.len
 Bio[[14]]<- Obs.Selex$obs.selec
 Bio[[15]]<- Obs.Selex$obs.selec.age.f
 Bio[[16]]<- Obs.Selex$obs.selec.age.m

 names(Bio) <- c("phi.f","phi.m","mid.phi.m","mid.phi.f","mid.wght","wght","mid.wght.at.len","wght.at.len",
                "mid.len","wght","len","fecund","sigma.len","obs.selec","obs.selec.age.f","obs.selec.age.m")
 return(Bio)
}
