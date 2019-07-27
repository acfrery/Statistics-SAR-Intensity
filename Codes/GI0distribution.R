### GI0 distribution density

### Contents:
## dGI0
## rGI0
## pGI0
## qGI0

# Version 1: from scratch
dGI0 <- function(z, p_alpha, p_gamma, p_Looks, log=FALSE) {
  
  if(log==TRUE) {
    return(
      (p_Looks*log(p_Looks) + lgamma(p_Looks-p_alpha) + (p_Looks-1)*log(z) ) - 
        (p_alpha*log(p_gamma) + lgamma(-p_alpha) + lgamma(p_Looks) + 
        (p_Looks-p_alpha)*log(p_gamma + z*p_Looks) ) 
      )   
    }
  else { return( 
    ( p_Looks^p_Looks * gamma(p_Looks-p_alpha) * z^(p_Looks-1) ) / 
    (p_gamma^p_alpha * gamma(-p_alpha) * gamma(p_Looks) * (p_gamma + z*p_Looks)^(p_Looks-p_alpha)) 
  )
  }
}

# Version 2: as a transformation of the density of the F distribution
#### ESTÁ ERRADA!!!
dGI0_F <- function(z, p_alpha, p_gamma, p_Looks, log=FALSE) {
  
  p_alpha <- -abs(p_alpha)
  p_gamma <- abs(p_gamma)
  p_Looks <- abs(p_Looks)
  
  return( 
      (-p_alpha)*df(-p_alpha * z / p_gamma, df1 = 2*p_Looks, df2=-2*p_alpha, log=log)
    )
  
}

### Sampling from the GI0 distribution

rGI0 <- function(n, p_alpha, p_gamma, p_Looks) {
  
  return(
    rgamma(n, p_Looks, p_Looks) / rgamma(n, -p_alpha, p_gamma)
  )
  
}

### The cumulative distribution function of the GI0 distribution

pGI0 <- function(z, p_alpha, p_gamma, p_Looks) {
 
  return(pf(-p_alpha * z / p_gamma, df1 = 2*p_Looks, df2=-2*p_alpha))
}

### The quantiles of the GI0 distribution // VERIFICAR!!!

qGI0 <- function(z, p_alpha, p_gamma, p_Looks) {
  
  return(qf(-p_alpha * x / p_gamma, df1 = 2*p_Looks, df2=-2*p_alpha))
}

### Returns the Inverse Gamma Kernel of fixed bandwidth 1 / (5 * sqrt(n)) of sample z

IGkernel <- function(t, z, b) {
  
  exp((2 - t/z - z/t) / (2 * b * z))  / sqrt(2 * pi * b * t^3)
  
}

IGkernelDensityEstimator <- function(x, z)
{
  n <- length(z)
  b <- 1 / (5*sqrt(n))
  return(sum(IGkernel(x, z, b)) / n)
}




