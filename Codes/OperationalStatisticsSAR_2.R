source("imagematrix.R")
require(png)
require(ggplot2)
require(ggthemes)

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

