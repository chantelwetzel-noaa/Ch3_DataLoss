sim = 15
est=c(depl.est[51,1,sim],
depl.est[55,2,sim],
depl.est[59,3,sim],
depl.est[63,4,sim],
depl.est[67,5,sim],
depl.est[71,6,sim],
depl.est[75,7,sim],
depl.est[79,8,sim],
depl.est[83,9,sim],
depl.est[87,10,sim],
depl.est[91,11,sim],
depl.est[95,12,sim],
depl.est[99,13,sim],
depl.est[103,14,sim],
depl.est[107,15,sim],
depl.est[111,16,sim],
depl.est[115,17,sim],
depl.est[119,18,sim],
depl.est[123,19,sim],
depl.est[127,20,sim],
depl.est[131,21,sim],
depl.est[135,22,sim],
depl.est[139,23,sim],
depl.est[143,24,sim],
depl.est[147,25,sim],
depl.est[151,26,sim])

test = seq(121,221,4)
bio = depl[test,sim]


cbind(est,bio,failed.to.detect.rec[,sim],failed.to.detect.over[,sim],incorrect.rebuild[,sim])