Do_Survey <- function(file, ind, survey.err){

	surv.exp = mapply(function(x) 
			  surv.exp = as.numeric(strsplit(file[grep(paste(2, "Survey",x),file)]," ")[[1]][8]),
			  x = ind)

	index  <- surv.exp * exp(survey.err[ind] - 0.5 * (survey.err[ind])^2)
	if (file.type == "perfect") { index  <- surv.exp }
	return(index)
}