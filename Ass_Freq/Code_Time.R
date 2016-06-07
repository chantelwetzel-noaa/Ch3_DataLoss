setwd("F:/Phd/Chapter3/")
Rprof(file = 'bio.out')
x = Get_Biology()
Rprof(NULL)
summaryRprof('bio.out')

Rprof(file = 'dyn.out')
x = Update_Dynamics(R0=2000, catch=hist.catch, biology = Get_Biology())
Rprof(NULL)
summaryRprof('dyn.out')


Rprof(file = 'sim.out')
x = DoSim(
  drive <- "F", 
  LH <- "rockfish", 
  SR <- "BH",
  depl.value <- 0.20, 
  sigma <- 0.36, 
  p.value <- 0.45, 
  start.n <- 1, 
  end.n <- 100, 
  survey.start.yr <- 31)
Rprof(NULL)
summaryRprof("sim.out")

(drive, LH, SR,  depl.value, sigma, p.value, start.n, end.n, start.survey)
