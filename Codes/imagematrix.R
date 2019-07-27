##
## imagematrix class definition
##
## $Header: /database/repository/rimage/R/Attic/imagematrix.R,v 1.1.2.5 2004/03/17 06:35:18 tomo Exp $
##
## Copyright (c) 2003 Nikon Systems Inc.
## For complete license terms see file LICENSE

imagematrix <- function(mat, type=NULL, ncol=dim(mat)[1], nrow=dim(mat)[2],
                        noclipping=FALSE) {
  if (is.null(dim(mat)) && is.null(type)) stop("Type should be specified.")
  if (length(dim(mat)) == 2 && is.null(type)) type <- "grey"
  if (length(dim(mat)) == 3 && is.null(type)) type <- "rgb"
  if (type != "rgb" && type != "grey") stop("Type is incorrect.")
  if (is.null(ncol) || is.null(nrow)) stop("Dimension is uncertain.")
  imgdim <- c(ncol, nrow, if (type == "rgb") 3 else NULL)
  if (length(imgdim) == 3 && type == "grey") {
    # force to convert grey image
    mat <- rgb2grey(mat)
  }
  if (noclipping == FALSE && ((min(mat) < 0) || (1 < max(mat)))) {
    warning("Pixel values were automatically clipped because of range over.") 
    mat <- clipping(mat)
  }
  mat <- array(mat, dim=imgdim)
  attr(mat, "type") <- type
  class(mat) <- c("imagematrix", class(mat))
  mat
}

print.imagematrix <- function(x, ...) {
  x.dim <- dim(x)
  cat("size: ", x.dim[1], "x", x.dim[2], "\n")
  cat("type: ", attr(x, "type"), "\n")
}

plot.imagematrix <- function(x, ...) {
  colvec <- switch(attr(x, "type"),
                grey=grey(x),
                rgb=rgb(x[,,1], x[,,2], x[,,3]))
  if (is.null(colvec)) stop("image matrix is broken.")
  colors <- unique(colvec)
  colmat <- array(match(colvec, colors), dim=dim(x)[1:2])
  image(x = 0:(dim(colmat)[2]), y=0:(dim(colmat)[1]),
        z = t(colmat[nrow(colmat):1, ]), col=colors,
        xlab="", ylab="", axes=FALSE, asp=1, ...)
}

imageType <- function(x) {
  attr(x, "type")
}

rgb2grey <- function(img, coefs=c(0.30, 0.59, 0.11)) {
  if (is.null(dim(img))) stop("image matrix isn't correct.")
  if (length(dim(img))<3) stop("image matrix isn't rgb image.")
  imagematrix(coefs[1] * img[,,1] + coefs[2] * img[,,2] + coefs[3] * img[,,3],
              type="grey")
}
clipping <- function(img, low=0, high=1) {
  img[img < low] <- low
  img[img > high] <- high
  img
}
normalize <- function(img) {
  (img - min(img))/(max(img) - min(img))
}

# the end of file

### Added by Alejandro C. Frery
### 24 April 2014

imagematrixPNG <- function(x, name){
  dimensions <- dim(x)
  zero4 <- rep(0,4)
  png(file=name, width=dimensions[2], height=dimensions[1])
  par(mar=zero4, oma=zero4, omi=zero4)
  plot(x)
  dev.off()
}

imagematrixEPS <- function(x, name){
  dimensions <- dim(x)
  zero4 <- rep(0,4)
  postscript(file=name, width=dimensions[1], height=dimensions[2], paper = "special")
  par(mar=zero4, oma=zero4, omi=zero4)
  plot(x)
  dev.off()
}

# Finalmente! Função que equaliza uma imagem de uma banda
equalize <- function(imagem) {
  imagemeq <- ecdf(imagem)(imagem)
  dim(imagemeq) <- dim(imagem)
  return(imagemeq)
  
}

### Added by Alejandro C. Frery
### 31 January 2018
# Função que equaliza três bandas independentemente
equalize_indep <- function(imagem) {
	req <- ecdf(imagem[,,1])(imagem[,,1])
	geq <- ecdf(imagem[,,2])(imagem[,,2])
	beq <- ecdf(imagem[,,3])(imagem[,,3])

  return(
  	imagematrix(
  		array(c(req, geq, beq), dim=dim(imagem))
    )
  )
}

### Added by Alejandro C. Frery
### 5 March 2018
# Função que lineariza três bandas independentemente
normalize_indep <- function(imagem) {
  rlin <- normalize(imagem[,,1])
  glin <- normalize(imagem[,,2])
  blin <- normalize(imagem[,,3])
  
  return(
    imagematrix(
      array(c(rlin, glin, blin), dim=dim(imagem))
    )
  )
}

### Histogram Matching
### Purpose: forces an output (destination) image to have the same histogram
### as the input (reference) image
### Works on images with one band
### Returns an imagematrix object
### Maceio, 23 August 2014
### Alejandro C. Frery 

HistogramMatching <- function(reference, destination) {
  
  return(imagematrix(
    HistToEcdf(
      hist(reference, breaks='FD', plot=FALSE), inverse=TRUE)(equalize(destination)),
    ncol=dim(reference)[1], nrow=dim(reference)[2], type='grey')
  )
  
}

# Copyright 2013 Google Inc. All Rights Reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# Author: mstokely@google.com (Murray Stokely)


HistToEcdf <- function(h, method="constant", f=0, inverse=FALSE) {
  # Compute an empirical cumulative distribution function from a histogram.
  #
  # Args:
  #   h:  An S3 histogram object.
  #   method: specifies the interpolation method to be used in call to
  #      approxfun().  Choices are ‘"linear"’ or ‘"constant"’.
  #   f: for ‘method="constant"’ a number between 0 and 1 inclusive,
  #      indicating a compromise between left- and right-continuous
  #      step functions.  See ?approxfun
  #   inverse: If TRUE, return the inverse function.
  #
  # Returns:
  #   An empirical cumulative distribution function (see ?ecdf)
  
  n <- sum(h$counts)
  # We don't want to use h$mids here, because we want at least
  # to get the correct answers at the breakpoints.
  x.vals <- h$breaks
  y.vals <- c(0, cumsum(h$counts) / n)
  if (inverse) {
    vals.tmp <- x.vals
    x.vals <- y.vals
    y.vals <- vals.tmp
  }
  rval <- approxfun(x.vals, y.vals,
                    method = method, yleft = 0, yright = 1, f = f,
                    ties = "ordered")
  class(rval) <- c("ecdf", "stepfun", class(rval))
  assign("nobs", n, envir = environment(rval))
  attr(rval, "call") <- sys.call()
  rval
}