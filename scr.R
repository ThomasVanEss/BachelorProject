fitline = function(modPars,colour,dist){
  modExp = sum(count)*dist(modPars,1:sum(count))
  lines(1:length(modExp),modExp, col = colour);
}

fitmodel = function(dis,start,gamma) {
  o2 = optim(start, LL, dist = dis, lower = 0,  method = "L-BFGS-B")
  gamma = length(modPars)-1; beta = modPars[length(modPars)]; opars = modPars[1:(gamma)]
  #o1 = optim(c(gamma,beta),LL2,opars = opars, dis = gsDist,lower = 0, method = "L-BFGS-B")
  modPars = as.vector(o2$par)
  fitline(modPars,'red', dis)
  #Exp = gsDist(o1$par,modPars,1:length(count))*sum(count) 
  #lines(1:length(count),Exp, col = 'blue')
  return(o2)
}


simStats = function(nsamples,modPars,dist,data) {
  ksDistance = rep(0,nsamples); lrt = rep(0,nsamples); chiSquared = rep(0,nsamples)
  modExp = sum(count)*dist(modPars,1:length(count));
  datachi = sum((count - modExp)^2/modExp)
  dataks = ksDist(count,modExp)
  for (i in 1:nsamples) {
    s = sample(x = 1:(length(count)*2), sum(count), replace = T, prob = dist(modPars,1:(length(count)*2))) 
    freqs = tabulate(s)
    if (0) {
      optsimp = optim(modPars, LL,dist = dist, lower = 0,dat = freqs, method = "L-BFGS-B")
      optsad =  optim(c(optsimp$par,0.1),dist = dist, LL, lower = 0,dat = freqs,  method = "L-BFGS-B")
      lrt[i] = 2 * (optsimp$value - optsad$value)
    }
    chiSquared[i] = sum((modExp[1:length(freqs)] - freqs)^2 / modExp[1:length(freqs)]);
    ksDistance[i] = ksDist(freqs,modExp)
  }
  return(list(Rsq = sort(chiSquared),ksDist = sort(ksDistance), freq = freqs,dataks = dataks, chidata = datachi, lrt = sort(lrt)))
}