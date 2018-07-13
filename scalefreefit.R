
source("model.R")
source("ksDist.R")
source("modelfit.R")
df = read.table("Facebook.txt",sep=",", header = 1)
ldf = log(df)
nsamples = 1000
plot(df,xlab = 'Degrees', ylab = 'Frequencies', log = 'xy')
amount = sum(df[[2]]);
degree = 1:max(df[[1]]);  count = rep(0,max(df[[1]])); 
for (i in 1:length(df[[1]])) {
  count[df[[1]][i]] =  df[[2]][i]
}
ldegree = log(degree); lcount = log(count);

#log likelihood
LL = function (pars,dist,dat) {
  if (missing(dat)) {dat = count}
  R = log(dist(pars,1:length(dat)))*dat/100;
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
opt = fitmodel(sfDist,modPars)
sims = simStats(nsamples,opt$par,sfDist,count)

 




