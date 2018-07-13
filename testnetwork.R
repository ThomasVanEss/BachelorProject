#This script is for applying the simpler method to all data sets

source("~/thesis/model.R")
source('~/thesis/ksDist.R')
source('~/thesis/modelfit.R')

#function that determines if a given distribution is scale-free
isScaleFree = function(degcount,distr,nsamples) {
  scalefree = 0;
  startPars = c(1,0,0,0,0,0,0);
  opt = fitmodel(sfDist,startPars,0);
  modPars = opt$par;
  #check if gamma > 1
  if (sum(modPars[3:7]) == 0){return(scalefree)}
  s = simStats(nsamples,modPars,sfDist,degcount);
  if(s$chiPerc < 0.95) {scalefree = 1}
  if(s$ksPerc < 0.95) {scalefree = 2}
  if(s$chiPerc < 0.95 && s$ksPerc < 0.95) {
    scalefree = 3;
  }
  return(scalefree)
}


file.names = dir(path,pattern = ".txt")
setwd(path)
results = rep(0,length(file.names))
for(i in 1:length(file.names)){
  df = read.table(file.names[i],sep=",", header = 1)
  degree = 1:max(df[[1]]);  count = rep(0,max(df[[1]])); 
  for (j in 1:length(df[[1]])) {
    count[df[[1]][j]] =  df[[2]][j]
  }
  sfPos = isScaleFree(count,sfDist,100);
  print(c(i,sfPos))
  results[i] = sfPos; 
}
perc = tot / length(file.names)
setwd("~/thesis")

