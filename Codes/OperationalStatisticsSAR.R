require(ggplot2)
require(ggthemes)
source("/Users/acfrery/Dropbox (Personal)/upper_outliers/codigos/GammaSAR.R")
source("/Users/acfrery/Documents/Programas/R/GI0Project/GI0distribution.R")
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

ggplot(data=data.frame(x=c(0, 7)), aes(x=x)) +
  stat_function(fun=pexp, geom = "line", size=2, col="black", args = (mean=1)) +
  stat_function(fun=pexp, geom = "line", size=2, col="red", args = (mean=.5)) +
  stat_function(fun=pexp, geom = "line", size=2, col="blue", args = (mean=2)) +
  theme_classic() +
  theme(text = element_text(size=20)) +
  xlab("x") + ylab("Exponential Cumulative Distribution Functions")
ggsave(file="../Figures/ExponentialCDFs.pdf")  

ggplot(data=data.frame(x=c(0, 7)), aes(x=x)) +
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
  stat_function(fun=dgamma, geom = "line", size=2, col="q", args = list(shape=1, scale=1)) +
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

ggplot(data=data.frame(x=seq(10^-3, 5, length.out = 500)), aes(x=x)) +
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




### Amostras de imagens

setwd("/Users/acfrery/imagens/DadosPolarimetricos/ESAR")
HH_Complex <- myread.ENVI("ESAR97HH.DAT", 
                          "ESAR97HH.hdr")
HV_Complex <- myread.ENVI("ESAR97VH.DAT", 
                          "ESAR97VH.hdr")
VV_Complex <- myread.ENVI("ESAR97VV.DAT", 
                          "ESAR97VV.hdr")
dim(HV_Complex)
typeof(HV_Complex)
HH_Intensity <- (Mod(HH_Complex))^2
HV_Intensity <- (Mod(HV_Complex))^2
VV_Intensity <- (Mod(VV_Complex))^2
rm(HH_Complex, HV_Complex, VV_Complex)

Intensity_RGB <- array(data=c(HH_Intensity, HV_Intensity, VV_Intensity), dim = c(dim(HH_Intensity), 3))

plot(imagematrix(equalize_indep(Intensity_RGB)))

imagematrixPNG(imagematrix(equalize_indep(Intensity_RGB)), name="ESAR_RGB.png")

dark <- Intensity_RGB[160:222,1350:1596,]

dark_HH <- as.vector(dark[,,1])
dark_HV <- as.vector(dark[,,2])
dark_VV <- as.vector(dark[,,3])


ggplot(data=data.frame(dark_HH), aes(x=dark_HH)) +
  geom_histogram(aes(y = ..density..), bins=100, col="black", fill="white") +
  stat_function(fun=dexp, args = list(rate=1/mean(dark_HH)), lwd=3, col="red") +
  scale_x_continuous(limits = c(0, 4e4)) +
  xlab("Intensities HH band") +
  ylab("Histogram and Density") +
  theme_few() +
  theme(text = element_text(size=20))
  
ggplot(data=data.frame(dark_HV), aes(x=dark_HV)) +
  geom_histogram(aes(y = ..density..), bins=100, col="black", fill="white") +
  stat_function(fun=dexp, args = list(rate=1/mean(dark_HV)), lwd=3, col="red") +
  scale_x_continuous(limits = c(0, 2500)) +
  xlab("Intensities HV band") +
  ylab("Histogram and Density") +
  theme_few() +
  theme(text = element_text(size=20))

ggplot(data=data.frame(dark_VV), aes(x=dark_VV)) +
  geom_histogram(aes(y = ..density..), bins=100, col="black", fill="white") +
  stat_function(fun=dexp, args = list(rate=1/mean(dark_VV)), lwd=3, col="red") +
  scale_x_continuous(limits = c(0, 2e4)) +
  xlab("Intensities HV band") +
  ylab("Histogram and Density") +
  theme_few() +
  theme(text = element_text(size=20))


