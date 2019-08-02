source("imagematrix.R")
require(png)
require(ggplot2)
require(ggthemes)
require(reshape)
require(reshape2)

strips <- readPNG("../Figures/strips.png")
strips <- normalize(strips)
dim(strips)

plot(imagematrix(strips))
plot(strips[,214], type="l")
plot(strips[124,], type="l")

strips.Exp <- ((strips + 1) * 5) * rexp(256*256)
plot(imagematrix(equalize(strips.Exp)))
imagematrixPNG(imagematrix(equalize(strips.Exp)), 
               "../Figures/stripsExp1.png")

plot(strips.Exp[,214], type="l")
plot(strips.Exp[124,], type="l")

### Transects

## Vertical transect
ggplot(as.data.frame(strips.Exp), aes(x=1:256)) +
  geom_hline(yintercept = 5, linetype="longdash") +
  geom_hline(yintercept = 10, linetype="longdash") +
  geom_line(data=as.data.frame(strips), y=((strips + 1) * 5)[,214],
            size=3, col="blueviolet", alpha=.5) +
  geom_line(y=strips.Exp[,214], col="purple") +
  expand_limits(y=range(strips.Exp[,214])) +
  xlab("Line") + ylab("Observation") + ggtitle("Vertical transect") +
  scale_x_continuous(breaks=c(1, 128, 256)) +
  scale_y_continuous(breaks=c(5,10,60)) +
  theme_few()

## Horizontal transect
ggplot(as.data.frame(strips.Exp), aes(x=1:256)) +
  geom_hline(yintercept = 5, linetype="longdash") +
  geom_hline(yintercept = 10, linetype="longdash") +
  geom_line(data=as.data.frame(strips), y=((strips + 1) * 5)[128,],
            size=3, col="blueviolet", alpha=.5) +
  geom_line(y=strips.Exp[128,], col="purple") +
  expand_limits(y=range(strips.Exp[128,])) +
  xlab("Line") + ylab("Observation") + ggtitle("Horizontal transect") +
  scale_x_continuous(breaks=c(1, 128, 256)) +
  scale_y_continuous(breaks=c(5,10,60)) +
  theme_few()

### Filters

SkeletonMean <- function(y, s) {
  
  # Input: the image and the side of the squared support
  
  # Output: filtered image z
  
  # Input image dimensions
  m <- dim(y)[1]
  n <- dim(y)[2]
  
  # Make space for the output image
  z <- y
  
  # Main loop
  margin <- (s+1)/2
  marginm1 <- margin-1
  for(k in margin:(m-margin)) {
    for(ele in margin:(n-margin)) {
      
      values <- y[(k-marginm1):(k+marginm1),(ele-marginm1):(ele+marginm1)]
      
      z[k,ele] = mean(values)
    }
  }
  
  return(z)
}

SkeletonMedian <- function(y, s) {
  
  # Input: the image and the side of the squared support
  
  # Output: filtered image z
  
  # Input image dimensions
  m <- dim(y)[1]
  n <- dim(y)[2]
  
  # Make space for the output image
  z <- y
  
  # Main loop
  margin <- (s+1)/2
  marginm1 <- margin-1
  for(k in margin:(m-margin)) {
    for(ele in margin:(n-margin)) {
      
      values <- y[(k-marginm1):(k+marginm1),(ele-marginm1):(ele+marginm1)]
      
      z[k,ele] = median(values)
    }
  }
  
  return(z)
}

## Mean
zMean3 <- SkeletonMean(strips.Exp, 3)
zMean15 <- SkeletonMean(strips.Exp, 15)

plot(imagematrix(equalize(zMean3)))
imagematrixPNG(imagematrix(equalize(zMean3)), "../Figures/Exp1Mean3.png")

plot(imagematrix(equalize(zMean15)))
imagematrixPNG(imagematrix(equalize(zMean15)), "../Figures/Exp1Mean15.png")

## Median
zMedian3 <- SkeletonMedian(strips.Exp, 3)
zMedian15 <- SkeletonMedian(strips.Exp, 15)

plot(imagematrix(equalize(zMedian3)))
imagematrixPNG(imagematrix(equalize(zMedian3)), "../Figures/Exp1Median3.png")

plot(imagematrix(equalize(zMedian15)))
imagematrixPNG(imagematrix(equalize(zMedian15)), "../Figures/Exp1Median15.png")

### Transects after filters

transects.3 <- data.frame(
  Line = 7:249,
  Strips = as.vector(((strips + 1) * 5)[128,7:249]),
  Mean = as.vector(zMean3[128,7:249]),
  Median = as.vector(zMedian3[128,7:249]*sqrt(2))
)

transects.3.flat <- melt(transects.3, 
                          measure.vars = c("Strips", "Mean", "Median"))
names(transects.3.flat) <- c("Line", "Data", "Observations")

ggplot(transects.3.flat, 
       aes(x=Line, y=Observations, col=Data)) + 
  geom_line() +
  geom_hline(yintercept = 5, linetype="longdash", col="cornsilk3") +
  geom_hline(yintercept = 10, linetype="longdash", col="cornsilk3") +
  xlab("Line") + ylab("Observation") + 
  ggtitle("Horizontal transect, 3x3 windows") +
  scale_x_continuous(breaks=c(4, 128, 252)) +
  scale_y_continuous(breaks=c(5,10,60)) +
  theme_few()


transects.15 <- data.frame(
  Line = 7:249,
  Strips = as.vector(((strips + 1) * 5)[128,7:249]),
  Mean = as.vector(zMean15[128,7:249]),
  Median = as.vector(zMedian15[128,7:249]*sqrt(2))
)

transects.15.flat <- melt(transects.15, 
    measure.vars = c("Strips", "Mean", "Median"))
names(transects.15.flat) <- c("Line", "Data", "Observations")

ggplot(transects.15.flat, 
       aes(x=Line, y=Observations, col=Data)) + 
  geom_line() +
  geom_hline(yintercept = 5, linetype="longdash", col="cornsilk3") +
  geom_hline(yintercept = 10, linetype="longdash", col="cornsilk3") +
  xlab("Line") + ylab("Observation") + 
  ggtitle("Horizontal transect, 15x15 windows") +
  scale_x_continuous(breaks=c(4, 128, 252)) +
  scale_y_continuous(breaks=c(5,10,60)) +
  theme_few()
