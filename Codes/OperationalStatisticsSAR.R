require(ggplot2)
require(scales)
require(ggthemes)
require(reshape2)
require(maxLik)
source("/Users/acfrery/Dropbox (Personal)/upper_outliers/codigos/GammaSAR.R")
source("/Users/acfrery/Documents/Programas/R/GI0Project/GI0distribution.R")
source("/Users/acfrery/Documents/Programas/R/GI0Project/KI0distribution.R")
source("/Users/acfrery/Documents/Programas/R/myread.ENVI.R")
source("/Users/acfrery/Documents/Programas/R/imagematrix.R")


### Complex Scattering

set.seed(1234567890, kind="Mersenne-Twister")

N <- 15 # número de dispersores elementales
sigma2 <- 1
RAi <- c(rnorm(n = N, mean = 0, sd = sqrt(sigma2)))
IAi <- c(rnorm(n = N, mean = 0, sd = sqrt(sigma2)))

DATA <- data.frame(RAi, IAi)

ggplot(data=DATA) +
  geom_segment(aes(x=c(0, cumsum(RAi)[1:(N-1)]), 
                   y=c(0, cumsum(IAi)[1:(N-1)]), 
                   xend=cumsum(RAi), yend=cumsum(IAi)), 
               arrow=arrow(), 
               size=.3, color="black") +
  geom_segment(aes(x=0, y=0, xend=sum(RAi), yend=sum(IAi)),
               arrow=arrow(), 
               size=.7, color="red") +
  theme_classic() +
  coord_fixed() +
  xlab("Real Components") + ylab("Imaginary Components")
ggsave(file="../Figures/Scattering.pdf")


### Exponential Distributions

ggplot(data=data.frame(x=c(0, 7)), aes(x=x)) +
  stat_function(fun=dexp, geom = "line", size=2, col="black", args = (mean=1)) +
  stat_function(fun=dexp, geom = "line", size=2, col="red", args = (mean=.5)) +
  stat_function(fun=dexp, geom = "line", size=2, col="blue", args = (mean=2)) +
  theme_classic() +
  theme(text = element_text(size=20)) +
  xlab("x") + ylab("Exponential Densities")
ggsave(file="../Figures/ExponentialDensities.pdf")  

ggplot(data=data.frame(x=seq(0, 7, length.out = 500)), aes(x=x)) +
  stat_function(fun=pexp, geom = "line", size=2, col="black", args = (mean=1)) +
  stat_function(fun=pexp, geom = "line", size=2, col="red", args = (mean=.5)) +
  stat_function(fun=pexp, geom = "line", size=2, col="blue", args = (mean=2)) +
  theme_classic() +
  theme(text = element_text(size=20)) +
  xlab("x") + ylab("Exponential Cumulative Distribution Functions")
ggsave(file="../Figures/ExponentialCDFs.pdf")  

ggplot(data=data.frame(x=seq(0, 7, length.out = 500)), aes(x=x)) +
  stat_function(fun=dexp, geom = "line", size=2, col="black", args = (mean=1)) +
  stat_function(fun=dexp, geom = "line", size=2, col="red", args = (mean=.5)) +
  stat_function(fun=dexp, geom = "line", size=2, col="blue", args = (mean=2)) +
  theme_classic() +
  theme(text = element_text(size=20)) +
  xlab("x") + ylab("Exponential Densities") +
  coord_trans(y="log10")
ggsave(file="../Figures/ExponentialDensitiesSemilog.pdf")  


### Gamma Distributions

ggplot(data=data.frame(x=seq(10^-3, 5, length.out = 500)), aes(x=x)) +
  stat_function(fun=dgamma, geom = "line", size=2, col="black", args = list(shape=1, scale=1)) +
  stat_function(fun=dgamma, geom = "line", size=2, col="red", args = list(shape=3, scale=1/3)) +
  stat_function(fun=dgamma, geom = "line", size=2, col="blue", args = list(shape=8, scale=1/8)) +
  theme_classic() +
  theme(text = element_text(size=20)) +
  xlab("x") + ylab("Gamma Densities")
  ggsave(file="../Figures/GammaDensities.pdf")  

ggplot(data=data.frame(x=seq(10^-3, 5, length.out = 500)), aes(x=x)) +
  stat_function(fun=pgamma, geom = "line", size=2, col="black", args = list(shape=1, scale=1)) +
  stat_function(fun=pgamma, geom = "line", size=2, col="red", args = list(shape=3, scale=1/3)) +
  stat_function(fun=pgamma, geom = "line", size=2, col="blue", args = list(shape=8, scale=1/8)) +
  theme_classic() +
  theme(text = element_text(size=20)) +
  xlab("x") + ylab("Gamma Cumulative Distribution Functions")
ggsave(file="../Figures/GammaCDFs.pdf")  

ggplot(data=data.frame(x=seq(10^-4, 5, length.out = 1000)), aes(x=x)) +
  stat_function(fun=dgamma, geom = "line", size=2, col="black", args = list(shape=1, scale=1)) +
  stat_function(fun=dgamma, geom = "line", size=2, col="red", args = list(shape=3, scale=1/3)) +
  stat_function(fun=dgamma, geom = "line", size=2, col="blue", args = list(shape=8, scale=1/8)) +
  theme_classic() +
  theme(text = element_text(size=20)) +
  xlab("x") + ylab("Gamma Densities") +
  coord_trans(y="log10")
ggsave(file="../Figures/GammaDensitiesSemilog.pdf")  

### Intensity K distributions

ggplot(data=data.frame(x=seq(0.01, 7, length.out = 500)), aes(x=x)) +
  stat_function(fun=dexp, geom = "line", size=2, col="green", args = (mean=1)) +
  stat_function(fun=dKI, geom = "line", size=2, col="red", args = list(p_alpha=1, p_lambda=1, p_Looks=1)) +
  stat_function(fun=dKI, geom = "line", size=2, col="blue", args = list(p_alpha=3, p_lambda=3, p_Looks=1)) +
  stat_function(fun=dKI, geom = "line", size=2, col="black", args = list(p_alpha=8, p_lambda=8, p_Looks=1)) +
  theme_classic() +
  theme(text = element_text(size=20)) +
  xlab("x") + ylab("Intensity K and Exponential Densities")
ggsave(file="../Figures/KIDensities.pdf")  

ggplot(data=data.frame(x=seq(0.01, 7, length.out = 500)), aes(x=x)) +
  stat_function(fun=dexp, geom = "line", size=2, col="green", args = (mean=1)) +
  stat_function(fun=dKI, geom = "line", size=2, col="red", args = list(p_alpha=1, p_lambda=1, p_Looks=1)) +
  stat_function(fun=dKI, geom = "line", size=2, col="blue", args = list(p_alpha=3, p_lambda=3, p_Looks=1)) +
  stat_function(fun=dKI, geom = "line", size=2, col="black", args = list(p_alpha=8, p_lambda=8, p_Looks=1)) +
  theme_classic() +
  theme(text = element_text(size=20)) +
  coord_trans(y="log10") +
  xlab("x") + ylab("Intensity K and Exponential Densities")
ggsave(file="../Figures/KIDensitiesSemilog.pdf")  

ggplot(data=data.frame(x=seq(0.01, 7, length.out = 500)), aes(x=x)) +
  stat_function(fun=dKI, geom = "line", size=2, col="red", args = list(p_alpha=2, p_lambda=2, p_Looks=1)) +
  stat_function(fun=dKI, geom = "line", size=2, col="blue", args = list(p_alpha=2, p_lambda=2, p_Looks=3)) +
  stat_function(fun=dKI, geom = "line", size=2, col="black", args = list(p_alpha=2, p_lambda=2, p_Looks=8)) +
  theme_classic() +
  theme(text = element_text(size=20)) +
  xlab("x") + ylab("Intensity K densities with varying Looks ")
ggsave(file="../Figures/KIDensitiesLooks.pdf")  

ggplot(data=data.frame(x=seq(0.01, 7, length.out = 500)), aes(x=x)) +
  stat_function(fun=dKI, geom = "line", size=2, col="red", args = list(p_alpha=2, p_lambda=2, p_Looks=1)) +
  stat_function(fun=dKI, geom = "line", size=2, col="blue", args = list(p_alpha=2, p_lambda=2, p_Looks=3)) +
  stat_function(fun=dKI, geom = "line", size=2, col="black", args = list(p_alpha=2, p_lambda=2, p_Looks=8)) +
  theme_classic() +
  theme(text = element_text(size=20)) +
  coord_trans(y="log10") +
  xlab("x") + ylab("Intensity K Densities with varying Looks")
ggsave(file="../Figures/KIDensitiesSemilogLooks.pdf")  


### Intensity GI0 distributions

ggplot(data=data.frame(x=seq(0.01, 10, length.out = 500)), aes(x=x)) +
  stat_function(fun=dexp, geom = "line", size=2, col="green", args = (mean=1)) +
  stat_function(fun=dGI0, geom = "line", size=2, col="red", args = list(p_alpha=-1.5, p_gamma=.5, p_Looks=1)) +
  stat_function(fun=dGI0, geom = "line", size=2, col="blue", args = list(p_alpha=-3, p_gamma=2, p_Looks=1)) +
  stat_function(fun=dGI0, geom = "line", size=2, col="black", args = list(p_alpha=-8, p_gamma=7, p_Looks=1)) +
  theme_classic() +
  theme(text = element_text(size=20)) +
  xlab("x") + ylab("Intensity G0 and Exponential Densities")
ggsave(file="../Figures/GI0Densities.pdf")  

ggplot(data=data.frame(x=seq(0.01, 10, length.out = 500)), aes(x=x)) +
  stat_function(fun=dexp, geom = "line", size=2, col="green", args = (mean=1)) +
  stat_function(fun=dGI0, geom = "line", size=2, col="red", args = list(p_alpha=-1.5, p_gamma=.5, p_Looks=1)) +
  stat_function(fun=dGI0, geom = "line", size=2, col="blue", args = list(p_alpha=-3, p_gamma=2, p_Looks=1)) +
  stat_function(fun=dGI0, geom = "line", size=2, col="black", args = list(p_alpha=-8, p_gamma=7, p_Looks=1)) +
  theme_classic() +
  theme(text = element_text(size=20)) +
  coord_trans(y="log10") +
  xlab("x") + ylab("Intensity G0 and Exponential Densities")
ggsave(file="../Figures/GI0DensitiesSemilog.pdf")  

ggplot(data=data.frame(x=seq(0.01, 10, length.out = 500)), aes(x=x)) +
  stat_function(fun=dGI0, geom = "line", size=2, col="red", args = list(p_alpha=-5, p_gamma=4, p_Looks=1)) +
  stat_function(fun=dGI0, geom = "line", size=2, col="blue", args = list(p_alpha=-5, p_gamma=4, p_Looks=3)) +
  stat_function(fun=dGI0, geom = "line", size=2, col="black", args = list(p_alpha=-5, p_gamma=4, p_Looks=8)) +
  theme_classic() +
  theme(text = element_text(size=20)) +
  xlab("x") + ylab("Intensity G0 densities with varying Looks ")
ggsave(file="../Figures/GI0DensitiesLooks.pdf")  

ggplot(data=data.frame(x=seq(0.01, 10, length.out = 500)), aes(x=x)) +
  stat_function(fun=dGI0, geom = "line", size=2, col="red", args = list(p_alpha=-5, p_gamma=4, p_Looks=1)) +
  stat_function(fun=dGI0, geom = "line", size=2, col="blue", args = list(p_alpha=-5, p_gamma=4, p_Looks=3)) +
  stat_function(fun=dGI0, geom = "line", size=2, col="black", args = list(p_alpha=-5, p_gamma=4, p_Looks=8)) +
  theme_classic() +
  theme(text = element_text(size=20)) +
  coord_trans(y="log10") +
  xlab("x") + ylab("Intensity G0 Densities with varying Looks")
ggsave(file="../Figures/GI0DensitiesSemilogLooks.pdf")  

### Beta distributions
ggplot(data=data.frame(x=seq(0.001, 1, length.out = 500)), aes(x=x)) +
  stat_function(fun=dbeta, geom = "line", size=2, col="red", args = list(shape1=2, shape2=2)) +
  stat_function(fun=dbeta, geom = "line", size=2, col="blue", args = list(shape1=8, shape2=8)) +
  stat_function(fun=dbeta, geom = "line", size=2, col="black", args = list(shape1=.7, shape2=.7)) +
  theme_classic() +
  theme(text = element_text(size=20)) +
  xlab("u") + ylab("Symmetric Beta densities")
ggsave(file="../Figures/SymmetricBetaDensities.pdf")  


### Amostras de imagens

imagepath <- "/Users/acfrery/imagens/DadosPolarimetricos/ESAR/"
HH_Complex <- myread.ENVI(paste(imagepath, "ESAR97HH.DAT", sep = ""), 
                          paste(imagepath, "ESAR97HH.hdr", sep = ""))
HV_Complex <- myread.ENVI(paste(imagepath, "ESAR97VH.DAT", sep = ""), 
                          paste(imagepath, "ESAR97VH.hdr", sep = ""))
VV_Complex <- myread.ENVI(paste(imagepath, "ESAR97VV.DAT", sep = ""), 
                          paste(imagepath, "ESAR97VV.hdr", sep = ""))
dim(HV_Complex)
typeof(HV_Complex)
HH_Intensity <- (Mod(HH_Complex))^2
HV_Intensity <- (Mod(HV_Complex))^2
VV_Intensity <- (Mod(VV_Complex))^2

Intensity_RGB <- array(data=c(HH_Intensity, HV_Intensity, VV_Intensity), dim = c(dim(HH_Intensity), 3))

### BEGIN Producing the PNG with annotations
dimensions <- dim(HV_Complex)
zero4 <- rep(0,4)
png(file="../Figures/ESAR_RGB_Annot.png", width=dimensions[2], height=dimensions[1])
par(mar=zero4, oma=zero4, omi=zero4)
plot(imagematrix(equalize_indep(Intensity_RGB)))

# Textureless area
lines(x=c(1350,1350,1596,1596,1350), y=c(1599-160,1599-222,1599-222,1599-160,1599-160), 
      col="yellow", lwd=7)

# Textured area
#lines(x=c(1398,1398,1506,1506,1398), y=c(1599-1308,1599-1521,1599-1521,1599-1308,1599-1308),
#      col="red", lwd=7)

dev.off()
### END Producing the PNG with annotations

plot(imagematrix(equalize_indep(Intensity_RGB)))

dark <- Intensity_RGB[160:222,1350:1596,]
save(file="../Data/dark.Rdata", dark)
bright <- Intensity_RGB[1398:1506,1308:1521,]
save(file="../Data/bright.Rdata", bright)

plot(imagematrix(normalize_indep(bright)))
plot(imagematrix(equalize_indep(bright)))

#### Remover - sólo para dibujar áreas
#dim(HH_Intensity)
#plot(imagematrix(equalize(HH_Intensity)))
#lines(x=c(1350,1350,1596,1596,1350), y = c(1599-160,1599-222,1599-222,1599-160,1599-160), col="yellow", lwd=3)
#### Remover

### Selected Complex Values
dark_HH_Re <- as.vector(Re(HH_Complex[160:222,1350:1596]))
dark_HH_Im <- as.vector(Im(HH_Complex[160:222,1350:1596]))
dark_HV_Re <- as.vector(Re(HV_Complex[160:222,1350:1596]))
dark_HV_Im <- as.vector(Im(HV_Complex[160:222,1350:1596]))
dark_VV_Re <- as.vector(Re(VV_Complex[160:222,1350:1596]))
dark_VV_Im <- as.vector(Im(VV_Complex[160:222,1350:1596]))

ggplot(data=data.frame(dark_HH_Re), aes(x=dark_HH_Re)) +
  geom_histogram(aes(y = ..density..), bins=100, col="black", fill="white") +
  stat_function(fun=dnorm, args = list(mean=0, sd=sd(dark_HH_Re)), lwd=3, col="red") +
  scale_x_continuous(limits = c(-300, 300)) +
  xlab("Real HH band") +
  ylab("Histogram and Density") +
  theme_few() +
  theme(text = element_text(size=20))
ggsave(file="../Figures/dark_HH_Re.pdf")

ggplot(data=data.frame(dark_HH_Im), aes(x=dark_HH_Im)) +
  geom_histogram(aes(y = ..density..), bins=100, col="black", fill="white") +
  stat_function(fun=dnorm, args = list(mean=0, sd=sd(dark_HH_Re)), lwd=3, col="red") +
  scale_x_continuous(limits = c(-300, 300)) +
  xlab("Imaginary HH band") +
  ylab("Histogram and Density") +
  theme_few() +
  theme(text = element_text(size=20))
ggsave(file="../Figures/dark_HH_Im.pdf")

ggplot(data=data.frame(dark_HV_Re), aes(x=dark_HV_Re)) +
  geom_histogram(aes(y = ..density..), bins=100, col="black", fill="white") +
  stat_function(fun=dnorm, args = list(mean=0, sd=sd(dark_HV_Re)), lwd=3, col="red") +
  scale_x_continuous(limits = c(-70, 70)) +
  xlab("Real HV band") +
  ylab("Histogram and Density") +
  theme_few() +
  theme(text = element_text(size=20))
ggsave(file="../Figures/dark_HV_Re.pdf")

ggplot(data=data.frame(dark_HV_Im), aes(x=dark_HV_Im)) +
  geom_histogram(aes(y = ..density..), bins=100, col="black", fill="white") +
  stat_function(fun=dnorm, args = list(mean=0, sd=sd(dark_HV_Im)), lwd=3, col="red") +
  scale_x_continuous(limits = c(-70, 70)) +
  xlab("Imaginary HV band") +
  ylab("Histogram and Density") +
  theme_few() +
  theme(text = element_text(size=20))
ggsave(file="../Figures/dark_HV_Im.pdf")

ggplot(data=data.frame(dark_VV_Re), aes(x=dark_VV_Re)) +
  geom_histogram(aes(y = ..density..), bins=100, col="black", fill="white") +
  stat_function(fun=dnorm, args = list(mean=0, sd=sd(dark_VV_Re)), lwd=3, col="red") +
  scale_x_continuous(limits = c(-200, 200)) +
  xlab("Real VV band") +
  ylab("Histogram and Density") +
  theme_few() +
  theme(text = element_text(size=20))
ggsave(file="../Figures/dark_VV_Re.pdf")

ggplot(data=data.frame(dark_VV_Im), aes(x=dark_VV_Im)) +
  geom_histogram(aes(y = ..density..), bins=100, col="black", fill="white") +
  stat_function(fun=dnorm, args = list(mean=0, sd=sd(dark_VV_Im)), lwd=3, col="red") +
  scale_x_continuous(limits = c(-200, 200)) +
  xlab("Imaginary VV band") +
  ylab("Histogram and Density") +
  theme_few() +
  theme(text = element_text(size=20))
ggsave(file="../Figures/dark_VV_Im.pdf")

### Selected Intensities
dark_HH <- as.vector(dark[,,1])
dark_HV <- as.vector(dark[,,2])
dark_VV <- as.vector(dark[,,3])

bright_HH <- as.vector(brigh[,,1])
bright_HV <- as.vector(brigh[,,2])
bright_VV <- as.vector(brigh[,,3])

ggplot(data=data.frame(dark_HH), aes(x=dark_HH)) +
  geom_histogram(aes(y = ..density..), bins=100, col="black", fill="white") +
  stat_function(fun=dexp, args = list(rate=1/mean(dark_HH)), lwd=3, col="red") +
  scale_x_continuous(limits = c(0, 4e4)) +
  xlab("Intensities HH band") +
  ylab("Histogram and Density") +
  theme_few() +
  theme(text = element_text(size=20))
ggsave(file="../Figures/darkHHfit.pdf")  

ggplot(data=data.frame(dark_HV), aes(x=dark_HV)) +
  geom_histogram(aes(y = ..density..), bins=100, col="black", fill="white") +
  stat_function(fun=dexp, args = list(rate=1/mean(dark_HV)), lwd=3, col="red") +
  scale_x_continuous(limits = c(0, 2500)) +
  xlab("Intensities HV band") +
  ylab("Histogram and Density") +
  theme_few() +
  theme(text = element_text(size=20))
ggsave(file="../Figures/darkHVfit.pdf")  

ggplot(data=data.frame(dark_VV), aes(x=dark_VV)) +
  geom_histogram(aes(y = ..density..), bins=100, col="black", fill="white") +
  stat_function(fun=dexp, args = list(rate=1/mean(dark_VV)), lwd=3, col="red") +
  scale_x_continuous(limits = c(0, 2e4)) +
  xlab("Intensities HV band") +
  ylab("Histogram and Density") +
  theme_few() +
  theme(text = element_text(size=20))
ggsave(file="../Figures/darkVVfit.pdf")  

### Descriptive statistics

dark_data.frame <-data.frame(HH=as.vector(dark[,,1]), HV=as.vector(dark[,,2]), VV=as.vector(dark[,,3]))
summary(dark_data.frame)

dark_DF <- melt(dark_data.frame)
ggplot(dark_DF, aes(x=variable, y=value)) + 
  geom_boxplot(notch = TRUE) + 
  coord_trans(y="log") + 
  xlab("Bands") +
  ylab("Observations in Logarithmic Scale") + 
  theme_few() +
  theme(text = element_text(size=20))
ggsave(file="../Figures/BoxPlotdark.pdf")  

### Introduction to R

# Histogram equalization
set.seed(1234567890, kind="Mersenne-Twister")

Z <- sample(as.vector(Re(HH_Complex)), size = 100, replace = FALSE)
W <- ecdf(Z)(Z)

HistogramEqualization <- data.frame(Z,W)
 
ggplot(data=HistogramEqualization, aes(x=Z)) +
  geom_histogram(aes(y = ..density..), col="black", fill="white") +
  xlab("Observations") +
  ylab("Histogram") + 
  theme_few() +
  theme(text = element_text(size=20))
ggsave(file="../Figures/Histogram.pdf")  

ggplot(data=HistogramEqualization, aes(x=Z)) +
  stat_ecdf(geom="step", pad=FALSE) +
  xlab("Observations") +
  ylab("Empirical Function") + 
  theme_few() +
  theme(text = element_text(size=20))
ggsave(file="../Figures/ECDF.pdf")  

ggplot(data=HistogramEqualization, aes(x=W)) +
  geom_histogram(aes(y = ..density..), col="black", fill="white") +
  xlab("Equalized Observations") +
  ylab("Histogram") + 
  theme_few() +
  theme(text = element_text(size=20))
ggsave(file="../Figures/EqualizedHistogram.pdf") 

### Examples of histogram equalization and specification

imagematrixPNG(normalize_indep(bright), name = "../Figures/BrightLinearized.png")
imagematrixPNG(equalize_indep(bright), name = "../Figures/BrightEqualized.png")

bright_rbeta <- qbeta(ecdf(bright[,,1])(bright[,,1]), shape1 = 8, shape2=8)
bright_gbeta <- qbeta(ecdf(bright[,,2])(bright[,,2]), shape1 = 8, shape2=8)
bright_bbeta <- qbeta(ecdf(bright[,,3])(bright[,,3]), shape1 = 8, shape2=8)

imagematrixPNG(
  imagematrix(
    array(c(bright_rbeta, bright_gbeta, bright_bbeta), dim = dim(bright)
    )
    ), name="../Figures/BetaBright.png"
)

### Analysis of BRIGHT[,,2] data

bHV <- as.vector(bright[,,2]) -> z
a <- -3
gamma_for_1 <- 2
L <- 1

LogLikelihoodLknown <- function(params) {
  
  p_alpha <- -abs(params[1])
  p_gamma <- abs(params[2])
  p_L <- abs(params[3])
  
  n <- length(z)
  
  return(
    n*(lgamma(p_L-p_alpha) - p_alpha*log(p_gamma) - lgamma(-p_alpha)) + 
      (p_alpha-p_L)*sum(log(p_gamma + z*p_L)) 
  )
}

estim1 <- maxNR(LogLikelihoodLknown, start=c(a,gamma_for_1,L), activePar=c(TRUE,TRUE,FALSE))$estimate[1:2]



ggplot(data=data.frame(bHV), aes(x=bHV)) +
  geom_histogram(aes(y = ..density..), bins=100, col="black", fill="white") +
  stat_function(fun=dexp, args = list(rate=1/mean(bHV)), lwd=2, col="blue", alpha=.4) +
  stat_function(fun=dGI0, args=list(p_alpha=estim1[1], p_gamma=estim1[2], p_Looks=1), lwd=3, col="red", alpha=.6) +
  scale_x_continuous(limits = c(0, 300000)) + 
  xlab("Exponential and GI0 densities") + 
  ylab("Intensity observations HV channel") +
  theme_few() + 
  theme(text = element_text(size=20))

### Code for the book
## Monte Carlo experiment to assess ML, Med and Bootstrap Med estimators

require(gtools)
require(purrr)
require(gtools)
set.seed(1234567890)

est.median.bootstrap <- function(z, R) {
  
  t <- median(z) / log(2)
  
  sample_size <- length(z)
  pwr <- sample_size^sample_size
  if(pwr < R) {
    m.Bootstrap <- permutations(sample_size, sample_size, z, 
                                set=TRUE, repeats.allowed=TRUE)
    return(2*t - mean(unlist(lapply(m.Bootstrap, median))))
  } else {
    v.Bootstrap <- rep(0, R)
    for(b in 1:R) {
      x <- sample(z, replace = TRUE)
      v.Bootstrap[b] <- median(x) / log(2)
    }
  }
  
  return(2*t - mean(v.Bootstrap))
}

N <- c(3, 5, seq(10,100,by=10), 1000, 10000)
BiasMSE <- matrix(nrow=14, ncol=7)

  start_time <- Sys.time()
  
  i <- 0
  for(n in N){
    i <- i+1
    
    r <- ceiling(2*10^6/n)
    v.mu1 <- array(rep(0, r))
    v.mu2 <- array(rep(0, r))
    v.mu3 <- array(rep(0, r))
    
    for(j in 1:r){
#      z <- rexp(n) # sample of size n from Exp(1)
      z <- rexp(n) * 100^rbernoulli(n, .001) # sample of size n from the mixed model
      
      v.mu1[j] <- mean(z)
      v.mu2[j] <- median(z) / log(2)
      v.mu3[j] <- est.median.bootstrap(z, 300)
    }
    
    bias1 <- mean(v.mu1) - 1
    eqm1 <- mean((v.mu1 - 1)^2)
    
    bias2 <- mean(v.mu2) - 1
    eqm2 <- mean((v.mu2 - 1)^2)
    
    bias3 <- mean(v.mu3) - 1
    eqm3 <- mean((v.mu3 - 1)^2)
    
    BiasMSE[i, ]  <- c(N[i], bias1, eqm1, bias2, eqm2, bias3, eqm3)
    
  }
  
  end_time <- Sys.time()
  end_time - start_time

BiasMSE <- data.frame(BiasMSE)
names(BiasMSE) <- c("N", "Bias.ML", "MSE.ML", "Bias.Med", "MSE.Med", "Bias.BootMed", "MSE.BootMed")

### Analysis of the Monte Carlo results
require(reshape)
require(reshape2)
require(ggplot2)
require(ggfortify)
require(ggthemes)
require(grid)


Bias <- data.frame(BiasMSE$N, BiasMSE$Bias.ML, BiasMSE$Bias.Med, BiasMSE$Bias.BootMed)
names(Bias) <- c("N", "ML", "Med", "BootMed")
Bias.flat <- melt(Bias, measure.vars = c("ML", "Med", "BootMed"))
names(Bias.flat) <- c("N", "Estimator", "value")



##### INSET PLOTS

### Bias

alldata <- ggplot(Bias.flat, aes(x=N, y=value, col=Estimator)) + 
  geom_line(size=1) +
  geom_point(alpha=.7, size=3) + 
  scale_x_continuous(trans='log10', 
                     breaks=N[c(1,3,12:14)], 
                     name ="Sample Size (logarithmic scale)") +
  scale_y_continuous(name="Bias") +
  theme_few() +
  theme(text = element_text(size=16),
        axis.text.x = element_text(angle = 90)) +
  theme(legend.position = "right")
print(alldata)

fewdata <- ggplot(Bias.flat, aes(x=N, y=value, col=Estimator)) + 
  geom_line(size=1) +
  geom_point(alpha=.7, size=3) + 
  scale_x_continuous(
    trans="log10",
    limits = c(3,1000),
    breaks=N, 
    name ="Sample Size (logarithic scale)"
  ) +
  scale_y_continuous(name="Bias") +
  theme_few() +
  theme(text = element_text(size=16),
        axis.text.x = element_text(angle = 90),
        legend.position = "none")

print(fewdata)
print(alldata, vp = viewport(width=.5, height=.5, just = c("left", "bottom")))

### MSE

MSE <- data.frame(BiasMSE$N, BiasMSE$MSE.ML, BiasMSE$MSE.Med, BiasMSE$MSE.BootMed)
names(MSE) <- c("N", "ML", "Med", "BootMed")
MSE.flat <- melt(MSE, measure.vars = c("ML", "Med", "BootMed"))
names(MSE.flat) <- c("N", "Estimator", "value")

alldata <- ggplot(MSE.flat, aes(x=N, y=value, col=Estimator)) + 
  geom_line(size=1) +
  geom_point(alpha=.7, size=3) + 
  scale_x_continuous(trans='log10', 
                     breaks=N[c(1,3,12:14)], 
                     name ="Sample Size (logarithmic scale)") +
  scale_y_continuous(name="MSE") +
  theme_few() +
  theme(text = element_text(size=16),
        axis.text.x = element_text(angle = 90)) +
  theme(legend.position = "right")
print(alldata)

fewdata <- ggplot(MSE.flat, aes(x=N, y=value, col=Estimator)) + 
  geom_line(size=1) +
  geom_point(alpha=.7, size=3) + 
  scale_x_continuous(trans='log10', 
                     breaks=N, 
                     limits = c(3,1000),
                     name ="Sample Size (logarithmic scale)") +
  scale_y_continuous(name="MSE") +
  theme_few() +
  theme(text = element_text(size=16),
        axis.text.x = element_text(angle = 90)) +
  theme(legend.position = "none")

print(fewdata)
print(alldata, vp = viewport(width=.5, height=.5, just = c("left", "bottom")))

### Analysis of urban area

plot(imagematrix(equalize(UrbanHV)))
imagematrixPNG(name = "../Figures/Urban.png", imagematrix(equalize(UrbanHV)))

## Now we select a region
plot(imagematrix(equalize(UrbanHV[90:200,50:100])))
imagematrixPNG(name = "../Figures/SampleUrban.png", imagematrix(equalize(UrbanHV[90:200,50:100])))

vUrbanHV <- data.frame(UHV=as.vector(UrbanHV[90:200,50:100]))
summary(vUrbanHV)

binwidth_complete <- 2*IQR(vUrbanHV$UHV)*length(vUrbanHV$UHV)^(-1/3)

ggplot(data=vUrbanHV, aes(x=UHV)) + 
  geom_histogram(aes(y=..density..), 
                 binwidth = binwidth_complete) + 
  xlab("Intensities") +
  ylab("Proportions") +
  ggtitle("Complete Histogram") +
  theme_few()
ggsave(filename = "../Figures/HistogramFullUrban.pdf")

ggplot(data=vUrbanHV, aes(x=UHV)) + 
  geom_histogram(aes(y=..density..), 
                 binwidth = binwidth_complete,
                 col="white") + 
  xlab("Intensities") +
  xlim(0, 200000) +
  ylab("Proportions") +
  ggtitle("Restricted Histogram") +
  theme_few()
ggsave(filename = "../Figures/HistogramRestrictedUrban.pdf")

## Estimation
require(maxLik)

GI0.Estimator.m1m2 <- function(z, L) {
  m1 <- mean(z)
  m2 <- mean(z^2)
  m212 <- m2/m1^2
    
  a <- -2 - (L+1) / (L * m212)
  g <- m1 * (2 + (L+1) / (L * m212))
  
  return(list("alpha"=a, "gamma"=g))
}

estim.Urban <- GI0.Estimator.m1m2(UrbanHV, 1)

LogLikelihoodLknown <- function(params) {
  
  p_alpha <- -abs(params[1])
  p_gamma <- abs(params[2])
  p_L <- abs(params[3])
  
  n <- length(z)
  
  return(
    n*(lgamma(p_L-p_alpha) - p_alpha*log(p_gamma) - lgamma(-p_alpha)) + 
      (p_alpha-p_L)*sum(log(p_gamma + z*p_L)) 
  )
}

z <- vUrbanHV$UHV
estim.UrbanML <- maxNR(LogLikelihoodLknown, 
                       start=c(estim.Urban$alpha, estim.Urban$gamma,1), 
                       activePar=c(TRUE,TRUE,FALSE))$estimate[1:2]

ggplot(data=vUrbanHV, aes(x=UHV)) + 
  geom_histogram(aes(y=..density..), col="white",
                 binwidth = binwidth_complete) + 
  xlim(0,200000) +
  stat_function(fun=dexp, args=list(rate=1/mean(vUrbanHV$UHV)), 
                col="red", lwd=2, alpha=.7) +
  stat_function(fun=dGI0, 
                args = list(p_alpha=estim.Urban$alpha, p_gamma=estim.Urban$gamma, p_Looks=1),
                col="blue", lwd=2, alpha=.7) +
  stat_function(fun=dGI0, 
                args = list(p_alpha=estim.UrbanML[1], p_gamma=estim.UrbanML[2], p_Looks=1),
                col="green", lwd=2, alpha=.7) +
  xlab("Intensities from the Urban Area") +
  ylab("Histogram, and fitted Exponential and G0 Laws") +
  ggtitle("Restricted Histogram and fitted densities") +
  theme_few()
ggsave(filename = "../Figures/HistogramRestrictedUrbanWFitted.pdf")

