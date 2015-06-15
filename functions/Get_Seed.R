#########################################################
##            Random Seed Function                     ##
##         Written by:  Chantel Wetzel                 ##
##              Date: 4-18-2014                        ##
#########################################################


Get_Seed <- function(n){ 
  seed <- round(runif(n, 1, 10000000),0)
  return(seed)
}

i = 10000
seeds <- matrix(0,i,10)

seeds[,1] <- Get_Seed(i)
seeds[,2]<- Get_Seed(i)
seeds[,3] <- Get_Seed(i)
seeds[,4]<- Get_Seed(i)
seeds[,5] <- Get_Seed(i)
seeds[,6]<- Get_Seed(i)
seeds[,7] <- Get_Seed(i)
seeds[,8] <- Get_Seed(i)
seeds[,9] <- Get_Seed(i)
seeds[,10] <- Get_Seed(i) 

seeds[1,2] = 679877

colnames(seeds) = c("recruit.seed",
				   "catch.seed",
				   "survey.seed",
				   "comp.seed",
				   "m.seed",
				   "spare1",
				   "spare2",
				   "spare3",
				   "spare4",
				   "spare5")

seed.list.long = list(seeds)
save(seed.list.long, file = "C:/PhD/Chapter3/seed_list_long")

