fitline = function(modPars,colour,dist){
  modExp = sum(count)*dist(modPars,1:sum(count))
  lines(1:length(modExp),modExp, col = colour);
}


#fit the gs-model with beta = 0
fitmodel = function(dis,start) {
  o2 = optim(start, LL, dist = dis, lower = 0,  method = "L-BFGS-B")
  modPars = as.vector(o2$par)
  fitline(modPars,'red', dis)
  return(o2)
}

#fit the gs-model with a given gamma paramter
fitmodel2 = function(dis,start,gamma) {
  o1 = optim(start,LL2,gamma = gamma, dis = gsDist,lower = 0, method = "L-BFGS-B")
  Exp = gsDist(o1$par,gamma,1:length(count))*sum(count) 
  lines(1:length(count),Exp, col = 'blue')
  return(o1)
}


#function that samples nsamples from the distribution (dist) and
#calculates the Peason chi-squared test statistic as well as the komogrov smirnov distance
#for the samples
simStats = function(nsamples,modPars,dist,count) {
  ksDistance = rep(0,nsamples); lrt = rep(0,nsamples); chiSquared = rep(0,nsamples)
  modExp = sum(count)*dist(modPars,1:(length(count)*2));
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
  chiSquared = sort(chiSquared); ksDistance = sort(ksDistance)
  chiPerc = placeInArr(chiSquared,datachi);
  ksPerc = placeInArr(ksDistance,dataks)
  return(list(Rsq = sort(chiSquared),ksDist = sort(ksDistance), freq = freqs,chiPerc = chiPerc, ksPerc = ksPerc, lrt = sort(lrt)))
}