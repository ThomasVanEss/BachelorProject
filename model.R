sfDist  <- function(pars,k) {
  p = rep(0,10)
  for (i in 1:length(pars)) {
    p[i] = pars[i]
  }
  y = 1:(length(count)*2)
  const = 1/sum(1 / (p[1] + p[2]*y + (p[3]*y)^2 + (p[4]*y)^3 + (p[5] * y)^4 + (p[6] * y)^5 + (p[7] * y) ^ 6))
  return (const / (p[1] + p[2]*k + (p[3]*k)^2 + (p[4]*k)^3 + (p[5] * k)^4 + (p[6] * k)^5 + (p[7] * k)^6)) 
}

ecDist = function(pars,k) {
  y = 1:(length(count)*2)
  gamma = pars[1]; beta = pars[2];
  const = 1 / sum( (y^(-gamma) * exp(-y/beta)))
  return(const * k^(-gamma) * exp(-k/beta))
}


plDist <- function(gamma,k) {
  return (k^(-gamma)/pracma::zeta(gamma))
}

gsDist = function(pars,gamma,k) {
  p = rep(0,6)
  for (i in 1:(length(pars)-1)) {
    p[i] = pars[i]
  }
  beta = pars[length(pars)]; 
  y = 1:(length(count)*2)
  const = 1/sum(1 / ((beta*y)^gamma + p[1] + p[2]*y + (p[3]*y)^2 + (p[4]*y)^3 + (p[5] * y)^4 + (p[6] * y)^5 ))
  return (const / ((beta*k)^gamma + p[1] + p[2]*k + (p[3]*k)^2 + (p[4]*k)^3 + (p[5] * k)^4 + (p[6] * k)^5))
}

