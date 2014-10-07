#Data Scenarios========================================================================================================

pre.fishery.yrs <- ages - 1 
setup.yrs   <- 50
project.yrs <- 100
if (LH == "flatfish") { project.yrs = 52 }
fishery.yrs <- setup.yrs + project.yrs + 1
total.yrs   <- pre.fishery.yrs + fishery.yrs

years       <- c(seq(-pre.fishery.yrs,-1,1),seq(1,fishery.yrs,1)) 
ass.num     <- (project.yrs / 4) + 1


if (data.scenario == "greathist") {
    #Only data available are prior to the stock being declared overfished
    #During rebuilding NO comp data from the fishery or the survey
    estimate.m             <- TRUE
    start.fishery.len.samp <- pre.fishery.yrs + start.survey
    start.fishery.age.samp <- pre.fishery.yrs + start.survey
    start.survey.len.samp  <- pre.fishery.yrs + start.survey
    start.survey.age.samp  <- pre.fishery.yrs + start.survey
    
    data.yrs   <- start.fishery.len.samp : total.yrs
    f.len.samp <- c(rep(0,start.survey - 1),rep(100,length(data.yrs)))

    data.yrs   <- start.survey.len.samp : total.yrs
    s.len.samp <- c(rep(0,start.survey - 1),rep(5,length(data.yrs)))

    data.yrs   <- start.fishery.age.samp : total.yrs
    f.age.samp <- c(rep(0,start.survey - 1),rep(50,length(data.yrs)))

    data.yrs   <- start.survey.age.samp : total.yrs
    s.age.samp <- c(rep(0,start.survey - 1),rep(5,length(data.yrs)))
}


if (data.scenario == "nocomps") {
    #Only data available are prior to the stock being declared overfished
    #During rebuilding NO comp data from the fishery or the survey
    estimate.m             <- TRUE
    start.fishery.len.samp <- pre.fishery.yrs + start.survey
    start.fishery.age.samp <- pre.fishery.yrs + start.survey
    start.survey.len.samp  <- pre.fishery.yrs + start.survey
    start.survey.age.samp  <- pre.fishery.yrs + start.survey
    
    data.yrs   <- start.fishery.len.samp : total.yrs
    f.len.samp <- c(rep(0,start.survey - 1),rep(0,length(data.yrs)))

    data.yrs   <- start.survey.len.samp : total.yrs
    s.len.samp <- c(rep(0,start.survey - 1),rep(0,length(data.yrs)))

    data.yrs   <- start.fishery.age.samp : total.yrs
    f.age.samp <- c(rep(0,start.survey - 1),rep(0,length(data.yrs)))

    data.yrs   <- start.survey.age.samp : total.yrs
    s.age.samp <- c(rep(0,start.survey - 1),rep(0,length(data.yrs)))
}

if (data.scenario == "normal")  {
    #Regular data levels until the stock is declared overfished
    #During rebuilding restricted comp data from the fishery
    estimate.m             <- FALSE
    start.fishery.len.samp <- pre.fishery.yrs + start.survey
    start.fishery.age.samp <- pre.fishery.yrs + start.survey
    start.survey.len.samp  <- pre.fishery.yrs + start.survey
    start.survey.age.samp  <- pre.fishery.yrs + start.survey

    data.yrs   <- start.fishery.len.samp : total.yrs
    f.len.samp <- c(rep(0,start.survey - 1),rep(100,length(data.yrs)))

    data.yrs   <- start.survey.len.samp : total.yrs
    s.len.samp <- c(rep(0,start.survey - 1),rep(5,length(data.yrs)))

    data.yrs   <- start.fishery.age.samp : total.yrs
    f.age.samp <- c(rep(0,start.survey - 1),rep(50,length(data.yrs)))

    data.yrs   <- start.survey.age.samp : total.yrs
    s.age.samp <- c(rep(0,start.survey - 1),rep(5,length(data.yrs)))
}

if (data.scenario == "normal_estM")  {
    #Regular data levels until the stock is declared overfished
    #During rebuilding restricted comp data from the fishery
    estimate.m             <- TRUE
    start.fishery.len.samp <- pre.fishery.yrs + start.survey
    start.fishery.age.samp <- pre.fishery.yrs + start.survey
    start.survey.len.samp  <- pre.fishery.yrs + start.survey
    start.survey.age.samp  <- pre.fishery.yrs + start.survey

    data.yrs   <- start.fishery.len.samp : total.yrs
    f.len.samp <- c(rep(0,start.survey - 1),rep(100,length(data.yrs)))

    data.yrs   <- start.survey.len.samp : total.yrs
    s.len.samp <- c(rep(0,start.survey - 1),rep(5,length(data.yrs)))

    data.yrs   <- start.fishery.age.samp : total.yrs
    f.age.samp <- c(rep(0,start.survey - 1),rep(50,length(data.yrs)))

    data.yrs   <- start.survey.age.samp : total.yrs
    s.age.samp <- c(rep(0,start.survey - 1),rep(5,length(data.yrs)))
}

if (data.scenario == "normal_auto")  {
    #Regular data levels until the stock is declared overfished
    #During rebuilding restricted comp data from the fishery
    estimate.m             <- TRUE
    start.fishery.len.samp <- pre.fishery.yrs + start.survey
    start.fishery.age.samp <- pre.fishery.yrs + start.survey
    start.survey.len.samp  <- pre.fishery.yrs + start.survey
    start.survey.age.samp  <- pre.fishery.yrs + start.survey

    data.yrs   <- start.fishery.len.samp : total.yrs
    f.len.samp <- c(rep(0,start.survey - 1),rep(100,length(data.yrs)))

    data.yrs   <- start.survey.len.samp : total.yrs
    s.len.samp <- c(rep(0,start.survey - 1),rep(5,length(data.yrs)))

    data.yrs   <- start.fishery.age.samp : total.yrs
    f.age.samp <- c(rep(0,start.survey - 1),rep(50,length(data.yrs)))

    data.yrs   <- start.survey.age.samp : total.yrs
    s.age.samp <- c(rep(0,start.survey - 1),rep(5,length(data.yrs)))
}

if (data.scenario == "greatall")  {
    #Regular data levels until the stock is declared overfished
    #During rebuilding restricted comp data from the fishery
    estimate.m             <- TRUE
    start.fishery.len.samp <- pre.fishery.yrs + start.survey
    start.fishery.age.samp <- pre.fishery.yrs + start.survey
    start.survey.len.samp  <- pre.fishery.yrs + start.survey
    start.survey.age.samp  <- pre.fishery.yrs + start.survey

    data.yrs   <- start.fishery.len.samp : total.yrs
    f.len.samp <- c(rep(0,start.survey - 1),rep(100,length(data.yrs)))

    data.yrs   <- start.survey.len.samp : total.yrs
    s.len.samp <- c(rep(0,start.survey - 1),rep(5,length(data.yrs)))

    data.yrs   <- start.fishery.age.samp : total.yrs
    f.age.samp <- c(rep(0,start.survey - 1),rep(50,length(data.yrs)))

    data.yrs   <- start.survey.age.samp : total.yrs
    s.age.samp <- c(rep(0,start.survey - 1),rep(5,length(data.yrs)))
}

if (data.scenario == "test")  {
    #Regular data levels until the stock is declared overfished
    #During rebuilding restricted comp data from the fishery
    estimate.m             <- TRUE
    start.survey           <- 1
    start.fishery.len.samp <- pre.fishery.yrs + start.survey
    start.fishery.age.samp <- pre.fishery.yrs + start.survey
    start.survey.len.samp  <- pre.fishery.yrs + start.survey
    start.survey.age.samp  <- pre.fishery.yrs + start.survey

    data.yrs   <- start.fishery.len.samp : total.yrs
    f.len.samp <- c(rep(0,start.survey - 1),rep(5000,length(data.yrs)))

    data.yrs   <- start.survey.len.samp : total.yrs
    s.len.samp <- c(rep(0,start.survey - 1),rep(5000,length(data.yrs)))

    data.yrs   <- start.fishery.age.samp : total.yrs
    f.age.samp <- c(rep(0,start.survey - 1),rep(5000,length(data.yrs)))

    data.yrs   <- start.survey.age.samp : total.yrs
    s.age.samp <- c(rep(0,start.survey - 1),rep(5000,length(data.yrs)))
}