#General parameters

# Survey Parameters
 Q <- 1
 survey.time <- 0.5

# Rockfish Section========================================================================================
 sexes <- 2
 ages  <- 71 
 num.ages <- ages-1
 fishing.time <- -1         
 max.F <- 4 
 
 len.step <- seq(from = 10, to = 80, by = 2)      
 mid.len.step <- seq(from = 11, to = 79, by = 2)  
 bin <- length(len.step)
       
 m <- 0.08                  
 steep <- 0.60 
 wght.coef.m <- wght.coef.f <- 1.5e-5
 wght.exp.m  <- wght.exp.f  <- 3.00

 L1  <- 18                  ; ohm3 <- -0.5 #slope of mat fnc
 L2f <- 64                  ; ohm4 <- 37 #length at 50% mat
 L2m  <- L2f                ; ohm5 <- 1

 k <- k <- 0.05
 ohm6 <- 0

 a3 <- 2                    ; a4 <- ages - 21  
 a.linear <- floor(a3)      ; len.slope <- 5.32         
 CV1 <- 0.12                ; CV2 <- 0.05
 mat.age <- 0 #First Mature Age
 
 Fmsy <- 0.05080846  
 
 fsp1.start <- 45         ; ssp1 <- 39
 fsp2 <- 6                ; ssp2 <- 3 
 fsp3 <- 4.25             ; ssp3 <- 4.25  
 fsp4 <- 6                ; ssp4 <- 6
 fsp5 <- -12              ; ssp5 <- -12 
 fsp6 <- -999             ; ssp6 <- 80
 
 #Forecast File Values=================================================================================================
 spr.target <- spr.target.int <- 0.50         
 bio.target <- 0.40
 ctl.rule.tgt<- 0.40        
 ctl.rule.thres <- 0.10
 over.thres <- 0.25
 
 #Depletion value in year 50 of the fishery ===========================================================================
 final.depl <- 0.10


