
Get_Samps <- function(data.type, year.vec ){
	perfect.file <- paste("perfect",nsim,"_",y,sep="")
	#Get the perfect data compositions from the data.ss_new file
	SS_splitdat(inpath = om, outpath = run,
           inname="data.ss_new", outpattern = perfect.file,
            number=F, verbose=T, fillblank = T, MLE = TRUE)

	data.file <- SS_readdat(paste0(run,"/", perfect.file,".ss",sep=""))

	if (data.type == "len"){ new.comp = data.file$lencomp ; end = 6}
	if (data.type == "age"){ new.comp = data.file$agecomp ; end = 9}

	temp.comp = new.comp
	final.comp = NULL
	#temp.comp = matrix(0, length(year.vec), dim(new.comp)[2])
	# Select the years for the fishery
	for (f in 1:2){
		for (a in 1:length(year.vec)){
			year.temp = new.comp$Yr == year.vec[a] & new.comp$FltSvy == f
			if (sum (year.temp) != 0){
				if (error.struct == "multinom"){
					probs = new.comp[year.temp, -(1:end)]
					Nsamp = new.comp[year.temp, "Nsamp"]
					if (data.type == 'len'){
						out.comp = as.matrix(rmultinom(1, size = Nsamp, prob = probs))
					}
					if (data.type == "age" && AgeError == FALSE){
						out.comp = as.matrix(rmultinom(1, size = Nsamp, prob = probs))
					}
				}

				#if (error.struct == "dirich"){
				#	para = 2
				#	probs = new.comp[year.temp, -(1:end)]
				#	Nsamp = new.comp[year.temp, "Nsamp"]
				#	lambda = Nsamp/para^2 - 1
				#	#out.comp = round(rdirichlet(1, as.numeric(probs) * lambda) * Nsamp, 4)
				#	out.comp = round(rdirichlet(1, as.numeric(probs) * lambda) * Nsamp, 0)
				#	effectiveN = Nsamp/para^2
				#	#temp.comp[year.temp, "Nsamp"] = effectiveN
				#}

				#if (AgeError == TRUE && data.type == 'age'){
				#	new.ages = numeric(ages*2-2)
				#	for (c in 1:(2*ages-2)) {
				#		ind = c + end + 1
				#		number = out.comp[c]		
				#		if(number !=0 ){
				#			for(d in 1:number){
				#				err = rnorm(1, 0, 0.1)
				#				if(c < ages)     { temp.age = c*exp(err-0.5*err^2) }
				#				if(c > (ages-1)) { temp.age = (c-ages + 1)*exp(err-0.5*err^2) }	
				#				rd.age = ifelse(round(temp.age,0) == 0, round(temp.age,0),
				#						 ifelse(round(temp.age,0) > (ages-1), ages - 1, round(temp.age,0)))
				#				if(c < ages)     { find = rd.age }
				#				if(c > (ages-1)) { find = rd.age + ages - 1}	
				#				new.ages[find] = new.ages[find]+1						
				#			}
				#		}
				#	}
				#	out.comp = new.ages
				#}		
				if (AgeError == TRUE && data.type == 'age'){
					new.ages = numeric(ages*2-2)
					age.sd <- rep(0.10, ages - 1)
					age.sd <- c(age.sd, age.sd)
					Nsamp  <- new.comp[year.temp, "Nsamp"]
					for (c in 1:Nsamp) {
						samp <- rmultinom(n = 1, size = 1, prob = probs)
						find <- sort(samp == 1, index.return =T)$ix[ages*2 -2]
						age.err <- rnorm(1, 0, age.sd[find])
      					temp1   <- ifelse(find > (ages-1), find - (ages-1), find)
      					temp2   <- temp1*exp(age.err - 0.5 * age.err^2) 
      					temp3   <- ifelse(temp2 > (floor(temp2)+ 0.50), ceiling(temp2), floor(temp2)) 
      					temp3   <- min(temp3, ages-1)		
						# Females go into 1:max.age
      					if ((find + temp3 - temp1) <= ages) { 
          					index = ifelse( (find + temp3 - temp1) < max.age, 
                           			find + temp3 - temp1, max.age) }
      					# Males go into max.age + 1 : max.age * 2
      					if ((find + temp3 - temp1) > ages) { 
          					index = ifelse( (find + temp3 - temp1 ) < max.age * 2, 
                            	temp3 + ages, max.age * 2) }
          				new.ages[index] <- new.ages[index] + 1
					}
					out.comp = new.ages
				}				
		
				temp.comp[year.temp,(end + 1):dim(new.comp)[2]] = out.comp
				#final.comp = rbind(final.comp, t(out.comp))
			}

		}
	}
	test.comp = cbind(temp.comp[,1:end], round(temp.comp[,(end + 1):dim(new.comp)[2]],0))
	return(test.comp)	
}