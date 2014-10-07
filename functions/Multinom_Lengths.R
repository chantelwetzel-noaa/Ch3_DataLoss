#########################################################
##       Generate Multinomial Length Samples           ##
##        for the Fishery or the Survey                ##
##         Written by:  Chantel Wetzel                 ##
##              Date: 4-22-2014                        ##
#########################################################


Multinom_Lengths <- function(catch.type, len.samp){
     
  len.expect <-  catch.type/(sum(catch.type)) 
  small <- 0.00001 #Set to the value in the data file
  #small <- 0.0000001
  new.prob.len.m <- len.expect[,2] + small
  new.prob.len.f <- len.expect[,1] + small  
  
  prob.len.m <- new.prob.len.m/sum(new.prob.len.f + new.prob.len.m)
  prob.len.f <- new.prob.len.f/sum(new.prob.len.f + new.prob.len.m)
  prob       <- c(prob.len.f, prob.len.m)

  len.samp   <- rmultinom(n = 1, size = len.samp, prob = prob)

  lengths  <- as.vector(len.samp)
  return(lengths)  
}
