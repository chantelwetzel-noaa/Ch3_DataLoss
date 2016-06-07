Do_Fishery <- function(file, ind, cpue.err){

	cpue.exp = mapply(function(x) 
			  cpue.exp = as.numeric(strsplit(file[grep(paste(1, "Fishery", x, 1, x+0.5),file)]," ")[[2]][8]),
			  x = ind)

	cpue  <- cpue.exp * exp(cpue.err[ind] - 0.5 * (cpue.err[ind])^2)
	if (file.type == "perfect") { cpue  <- cpue.exp }
	return(cpue)
}