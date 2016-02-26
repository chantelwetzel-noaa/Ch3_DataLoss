TimeVarying_Selex <- function(mid.phi.m, mid.phi.f, sel.para)
{
  #Selectivity Function=====================================================================================
  Selex <- list()
  selec <- matrix(NA,length(len.step),sexes)
  selec.age.m<-matrix(0,ages,1)
  selec.age.f<-matrix(0,ages,1) 
  
  #Double Normal Selectivity
  startbin <- 1
  peak <- sel.para #fsp1 + selec.adj
  upselex <- exp(fsp3)
  downselex <- exp(fsp4)
  final <- fsp6

  point1 <- 1 / (1 + exp(-fsp5)) 
  t1min <- exp(-((len.step[startbin] + 1) - peak)^2 / upselex)
  peak2 <- peak + 2 + (0.99 * (len.step[length(len.step)] + 1) - peak - 2) / (1 + exp(-fsp2))
  point2 <- 1 / (1 + exp(-final))
  t2min <- exp(-((len.step[length(len.step)] + 1) - peak2)^2 / downselex)
  t1 <- len.step + 1 - peak
  t2 <- len.step + 1 - peak2
  join1 <- 1 / (1 + exp(-(20 / (1 + abs(t1))) * t1))
  join2 <- 1 / (1 + exp(-(20 / (1 + abs(t2))) * t2))
  asc <- point1 + (1 - point1) * (exp(-t1^2 / upselex) - t1min) / (1 - t1min)
  if (fsp5 <= -999) { asc <-  exp(-(t1^2) / upselex)}
  dsc <- 1 +(point2-1)*(exp(-t2^2 / downselex) - 1) / (t2min - 1)
  if (fsp6 <- -999) { dsc <- exp(-(t2^2) / downselex)}

  selec[,1] <- asc * (1-join1) + join1 * (1 - join2 + dsc * join2) 
  selec[,2] <- selec[,1]

  #Mid-year Selectivity by Age
  selec.age.m <-(t(mid.phi.m)) %*% selec[,2]
  selec.age.f <-(t(mid.phi.f)) %*% selec[,1]
  
  Selex[[1]] <- selec
  Selex[[2]] <- selec.age.f
  Selex[[3]] <- selec.age.m
  Selex[[4]] <- peak
  names(Selex) <- c("selec","selec.age.f","selec.age.m", "new.peak")
  return(Selex)
}
