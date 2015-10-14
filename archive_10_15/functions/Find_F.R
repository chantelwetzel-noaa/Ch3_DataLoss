#########################################################
##         Find F that results in Depl                 ##
##                                                     ##
##         Written by:  Chantel Wetzel                 ##
##              Date: 4-18-2014                        ##
#########################################################

Findf <- function(f, biology){
  #Catch at age
  #z.rate[y,,1] <- m + biology$selec.age.f * f
  #z.rate[y,,2] <- m + biology$selec.age.m * f
  z.m <- (1 - exp(-(m + biology$selec.age.m * f))) / (m + biology$selec.age.m * f)
  z.f <- (1 - exp(-(m + biology$selec.age.f * f))) / (m + biology$selec.age.f * f)
  #Catch at Age
  catch.at.age.f <- f * (numbers[y,,1] * biology$selec.age.f) * z.f
  catch.at.age.m <- f * (numbers[y,,2] * biology$selec.age.m) * z.m
  #Catch At Length
  mid.temp.f <- numbers[y,,1] * z.f
  mid.temp.m <- numbers[y,,2] * z.m
  catch.at.len.f <- ((biology$mid.phi.f * biology$selec[,1]) %*% (mid.temp.f))
  catch.at.len.m <- ((biology$mid.phi.m * biology$selec[,2]) %*% (mid.temp.m))
  
  #Catch in Weight by Sex, mid.wght (41X2) calculated in the GetWght() function  
  catch.wght <- f * (sum(biology$mid.wght.at.len[,1] * catch.at.len.f) +
                       sum(biology$mid.wght.at.len[,2] * catch.at.len.m))    
  
  output <- NULL
  #output$catch.at.age <- catch.at.age
  output$catch.at.len.f <- catch.at.len.f
  output$catch.at.len.m <- catch.at.len.m
  output$catch.wght <- catch.wght
  output$catch.at.age.f <- catch.at.age.f
  output$catch.at.age.m <- catch.at.age.m
  return(output)
} #End FindF function 

#Objective Function----------------------------------------------------------------------------------------------------------
Obj.Fun.F <- function(f) {
  obj.fun.f <- (Findf(f, biology = Get_Biology())$catch.wght - hist.catch[y])^2
  return(obj.fun.f) 
}
