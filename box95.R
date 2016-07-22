box95 <- function(x,list = FALSE,dat=NULL,col='grey',lty=1,...)
{
  bx1 <- boxplot(x,plot=F, ...)
  if(!list){
  	 bx1$stats[c(1,5),] <- apply(x,2,quantile,probs=c(.025,.975))
  }
  #bx1$stats[c(1,5),] <- quantile(x, probs = c(.025,.975))
  if(list == TRUE){
  	for(ii in 1:length(x)){
  		bx1$stats[c(1,5),ii] <- quantile(x[[ii]], probs = c(.025,.975))
  	}
  }
  bx1$out = NULL
  bx1$group = NULL
  pars <- list(boxfill=col,boxlty=1,...)
  bxp(bx1,pars=pars,lty=1,...)
}
