
source("model.R")
source("ksDist.R")
df = read.table("facebook2.txt",sep=",", header = 1)
ldf = log(df)
fit = lm(ldf[[2]]~ldf[[1]])
nsamples = 1000
#plot(df,log = 'xy')
plot(df,xlab = 'Degrees', ylab = 'Frequencies', log = 'xy')
#abline(fit)
amount = sum(df[[2]]);
#degree = df[[1]]; count = df[[2]];
degree = 1:max(df[[1]]);  count = rep(0,max(df[[1]])); 

#degree = 1:amount;  count = rep(0,amount); 
for (i in 1:length(df[[1]])) {
  count[df[[1]][i]] =  df[[2]][i]
}
ldegree = log(degree); lcount = log(count);


LL = function (pars,dist,dat) {
  if (missing(dat)) {dat = count}
  R = log(dist(pars,1:length(dat)))*dat;
  return(sum(-R))
}

LL2 = function (pars,gamma,dis) {
  R = log(dis(pars,gamma,degree))*count;
  return(sum(-R))
}

sumSquares = function(pars) {
  sumsq = 0;
  modExp = sum(count)*sfDist(modPars,1:length(count))
  for (deg in degree) {
    sumsq =  sumsq + (count[deg] -modExp[deg])^2;
  }
  return(sumsq)
}



placeInArr = function(arr,x) {
  n = 0
  for (ele in arr) {
    if (x < ele) {
      break
    } else {
      n = n + 1
    }
  }
  return(n / length(arr))
}



modPars = c(1,0,0,0,0,0); modParsEc = c(3,10); modParspl = 3;
for (i in 1:1) {
  o2 = optim(modPars, LL, dist = sfDist, lower = 0,  method = "L-BFGS-B")
  modPars = as.vector(o2$par)
  oLsq = optim(modPars,sumSquares,lower = 0)
  gamma = length(modPars) + 1; beta = 1;
  o1 = optim(c(gamma,beta),LL2,opars = modPars, dis = gsDist,lower = c(gamma,0), method = "L-BFGS-B")
  #o3 = optim(modParsEc, LL,dist = ecDist,  lower = 1.1, method = "L-BFGS-B")
  #modParsEc = as.vector(o3$par)
  #o4 = optim(modParspl, LL,dist = plDist,lower = 1.1)
  #modParspl = as.vector(o4$par)
}
modExp = sum(count)*sfDist(modPars,1:sum(count))
#modExp2 = sum(count)*ecDist(modParsEc,1:sum(count))
#modExppl = sum(count)*plDist(modParspl,1:sum(count))


x2 = 1:sum(count)
lines(x2,modExp); lines(x2,modExp2, col = 'red'); lines(x2,modExppl, col = 'green')
#k = mle(LL2, start = list(a = 1,b=1,c=1,d=1),method = "Nelder-Mead", nobs = sum(df[[2]]))
lhood = rep(0,nsamples); chiSquared = rep(0,nsamples); maxDegree = rep(0,nsamples); 


rSqModel = sum((modExp[1:length(count)] - count)^2 / modExp[1:length(count)]);
j = 0; 
lhood = sort(lhood); chiSquared = sort(chiSquared)


