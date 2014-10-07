#########################################################
##            Random Seed Function                     ##
##         Written by:  Chantel Wetzel                 ##
##              Date: 4-18-2014                        ##
#########################################################


Get_Seed <- function(n){ 
  seed <- round(runif(n, 1, 10000000),0)
  return(seed)
}