### Functions for the univariate K distribution

dKI <- function(z, p_alpha, p_lambda, p_Looks, log=FALSE) {
  
  if(log==FALSE) {
    
    lLz <- p_lambda * p_Looks* z
    
    return((2*p_lambda*p_Looks/(gamma(p_alpha)*gamma(p_Looks))) *
             (lLz)^((p_alpha+p_Looks)/2-1) *
             besselK(x = 2*sqrt(lLz), nu = p_alpha-p_Looks)
    )
  }
  
}