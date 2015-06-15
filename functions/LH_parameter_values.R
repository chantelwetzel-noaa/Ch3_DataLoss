#General parameters

#Survey Parameters
 Q <- 1
 survey.time <- 0.5
 #survey.cv <- 0.50
 #ss.survey.cv <- 0.50

#Life-History Parameters
if (LH == "flatfish" ) {
 sexes <- 2
 ages  <- 41 
 num.ages <- ages-1
 fishing.time <- -1         ; max.F <- 4 

 len.step <- seq(from=10,to=64,by=2)      ; mid.len.step <- seq(from=11,to=65,by=2)  
 bin <- length(len.step)
        
 m <- 0.15                  ; steep <- 0.875 
 wght.coef.f <- wght.coef.m <- 0.00000342
 #wght.coef.m <- 0.00000717  ; wght.coef.f <- 0.00000342
 #wght.exp.m <- 3.134        ; wght.exp.f <- 3.346
 wght.exp.f  <- wght.exp.m <- 3.346
 L1 <- 24.6219  
 L2f <- L2m <- 55.4099              
 #L2f <- 55.4099             
 #L2m <- 40.6664             
 ohm3 <- -0.734  #slope of mat fnc
 ohm4 <- 33.10   #length at 50% mat
 ohm5 <- 1
 ohm6 <- 0 
 kf <- km <- 0.143779 
 #kf <- 0.143779 
 #km <- 0.299548                   
 #sigmaR <- 0.60         
 a3 <- 2.833                ; a4 <- ages - 11               
 a.linear <- floor(a3)      ; len.slope <- 5.1613
 CV1 <- 0.12                ; CV2 <- 0.05
 Amat <-amat <- 5           
 mat.age <- 0 #First Mature Age
 width.95per <- 4.05
 
 Fmsy <- 0.1964341
 
 fsp1.start <- 43         ; ssp1 <- 33
 fsp2 <- 3                ; ssp2 <- 3 
 fsp3 <- 4.25             ; ssp3 <- 4.25  
 fsp4 <- 6                ; ssp4 <- 6
 fsp5 <- -12              ; ssp5 <- -12 
 fsp6 <- 70               ; ssp6 <- 70
 
 #Forecast File Values=================================================================================================
 spr.target <- spr.target.int <-  0.30         
 bio.target <- 0.25
 ctl.rule.tgt<- 0.25
 ctl.rule.thres <- 0.05
 over.thres <- 0.125
 
 #Depletion value in year 50 of the fishery ===========================================================================
 final.depl <- 0.08 
} 



#Rockfish Section========================================================================================
if (LH == "rockfish" ) { 
 sexes <- 2
 ages  <- 71 
 num.ages <- ages-1
 fishing.time <- -1         ; max.F <- 4 
 
 len.step <- seq(from = 10, to = 76, by = 2)      ; mid.len.step <- seq(from = 11, to = 77, by = 2)  
 bin <- length(len.step)
       
 m <- 0.08                  ; steep <- 0.60 
 wght.coef.m <- wght.coef.f <- 1.5e-5
 wght.exp.m  <- wght.exp.f  <- 3.00
 
 #wght.coef.m <- 9.5e-6      ; wght.coef.f <-  1.7e-5 
 #wght.exp.m  <- 3.15        ; wght.exp.f <- 3.00
 L1  <- 18                  ; ohm3 <- -0.5 #slope of mat fnc
 L2f <- 64                  ; ohm4 <- 37 #length at 50% mat
 L2m  <- L2f                ; ohm5 <- 1
 #L2m <- L2f - 4             ; ohm5 <- 1
 #kf <- 0.05                 ; ohm6 <- 0
 #km <- 0.05                 ; sigmaR <- 0.60  
 km <- kf <- 0.05
 ohm6 <- 0
 #sigmaR <- 0.60
 a3 <- 2                    ; a4 <- ages - 21  
 a.linear <- floor(a3)      ; len.slope <- 5.32         
 CV1 <- 0.12                ; CV2 <- 0.05
 Amat <- amat <- 13
 mat.age <- 0 #First Mature Age
 
 Fmsy <- 0.05080846  
 
 fsp1.start <- 45         ; ssp1 <- 39
 fsp2 <- 3                ; ssp2 <- 3 
 fsp3 <- 4.25             ; ssp3 <- 4.25  
 fsp4 <- 6                ; ssp4 <- 6
 fsp5 <- -12              ; ssp5 <- -12 
 fsp6 <- 80               ; ssp6 <- 80
 
 #Forecast File Values=================================================================================================
 spr.target <- spr.target.int <- 0.50         
 bio.target <- 0.40
 ctl.rule.tgt<- 0.40        
 ctl.rule.thres <- 0.10
 over.thres <- 0.25
 
 #Depletion value in year 50 of the fishery ===========================================================================
 final.depl <- 0.15
}


