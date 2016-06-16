

selec <- matrix(NA, length(len.step), 100)
fsp2 = sample(x = c(-3,6), size = 100, replace = T, prob = c(0.25, 0.75))
fsp2.shift = -2.5 #round(runif(total.yrs, -3, 0),0)
select.sd = 0.25
select.err = rnorm(100, 0, select.sd)
fsp2 = round(fsp2.shift*exp(-0.5*select.sd*select.sd + select.err), 0)
fsp6 = -999
fsp3 = 3
select.sd = 0.05
select.err = rnorm(100, 0, select.sd)
fsp1       = round(45*exp(-0.5*select.sd*select.sd + select.err),0)

for (a in 1:100){
  #Double Normal Selectivity
  startbin <- 1
  peak <- fsp1[a]
  upselex <- exp(fsp3)
  downselex <- exp(fsp4)
  final <- fsp6

  point1 <- 1 / (1 + exp(-fsp5)) 
  t1min <- exp(-((len.step[startbin] + 1) - peak)^2 / upselex)
  peak2 <- peak + 2 + (0.99 * (len.step[length(len.step)] + 1) - peak - 2) / (1 + exp(-fsp2[a]))
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

  selec[,a] <- asc * (1-join1) + join1 * (1 - join2 + dsc * join2) 
}

par(mfrow = c(2,1))
plot(len.step, fix.selec[,1], type = 'l')
plot(len.step, selec[,1], type = 'l')
for(b in 2:100){
	lines(len.step, selec[,b])
}