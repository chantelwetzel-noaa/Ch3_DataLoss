n = 100
directory <- "F:/PhD/Chapter3/"

# Seed file for output
seed.file <- paste(directory,"seed_list",sep="")

Get_Seed <- function(n){ 
  seed <- round(runif(n, 1, 10000000),0)
  return(seed)
}

seed.list     <- list()
recruit.seed  <- Get_Seed(n)
catch.seed    <- rep(679877, n) #Get_Seed()
survey.seed   <- Get_Seed(n) 
comp.seed     <- Get_Seed(n)
m.seed        <- Get_Seed(n)
spare1        <- Get_Seed(n)
spare2        <- Get_Seed(n)
spare3        <- Get_Seed(n)

#Save the seeds and write them out
seed.list[[1]] <- cbind(recruit.seed, catch.seed, survey.seed, comp.seed, m.seed, spare1, spare2, spare3)
save(seed.list, file = seed.file)