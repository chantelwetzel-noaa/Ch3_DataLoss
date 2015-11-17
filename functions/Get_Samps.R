
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
	# Select the years for the fishery
	for (f in 1:2){
		for (a in 1:length(year.vec)){
			year.temp = new.comp$Yr == year.vec[a] & new.comp$FltSvy == f
			if (sum (year.temp) != 0){
				if (error.struct == "multinom"){
					probs = new.comp[year.temp, -(1:end)]
					Nsamp = new.comp[year.temp, "Nsamp"]
					out.comp = as.matrix(rmultinom(1, size = Nsamp, prob = probs))
				}
				if (error.struct == "dirich"){
					para = 2
					probs = new.comp[year.temp, -(1:end)]
					Nsamp = new.comp[year.temp, "Nsamp"]
					lambda = Nsamp/para^2 - 1
					out.comp = round(rdirichlet(1, as.numeric(probs) * lambda) * Nsamp,4)
					effectiveN = Nsamp/para^2
					#temp.comp[year.temp, "Nsamp"] = effectiveN
				}				
				temp.comp[year.temp,(end + 1):dim(new.comp)[2]] = out.comp
			}				
		}
	}

	return(temp.comp)	
}