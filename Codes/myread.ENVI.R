readComplexFloat <- function(f, size, endian){
	X2 <- readBin(f, double(), n = 2*size, size = 4, endian = endian) # caso 4
	X = complex(length.out = size)

	for(i in 1:size) {
		twoi = 2*i
		X[i] = complex(real = X2[twoi-1], imaginary=X2[twoi])
	}

	return(X)
}

myread.ENVI <- function (filename, headerfile = paste(filename, ".hdr", sep = ""))
{
    nCol <- nRow <- nBand <- data.type <- header.offset <- byte.order <- (-1)
    interleave = "bsq"
    if (!file.exists(headerfile))
        stop("read.ENVI: Could not open input header file: ",
            headerfile)
    Lines = read.table(headerfile, sep = "=", strip.white = TRUE,
        row.names = NULL, as.is = TRUE, fill = TRUE)
    Fields = c("samples", "lines", "bands", "data type", "header offset",
        "interleave", "byte order")
    for (i in 1:nrow(Lines)) {
        Lab = tolower(Lines[i, 1])
        Lab = gsub("[ ]+", " ", Lab)
        j = match(Lab, Fields)
        Val = Lines[i, 2]
        if (length(j) == 1)
            switch(j, nCol <- as.integer(Val), nRow <- as.integer(Val),
                nBand <- as.integer(Val), data.type <- as.integer(Val),
                header.offset <- as.integer(Val), interleave <- gsub(" ",
                  "", Val), byte.order <- as.integer(Val))
    }
    if (nCol <= 0 | nRow <= 0 | nBand <= 0)
        stop("read.ENVI: Error in input header file ", headerfile,
            " data sizes missing or incorrect", nRow, nCol, nBand)
    if (!(data.type %in% c(1, 2, 3, 4, 5, 6, 9, 12)))
        stop("read.ENVI: Error in input header file ", headerfile,
            " data type is missing, incorrect or unsupported ")
    ieee = if (.Platform$endian == "big")
        1
    else 0
    endian = if (ieee == byte.order | byte.order < 0)
        .Platform$endian
    else "swap"
    size = nRow * nCol * nBand
    if (!file.exists(filename))
        stop("read.ENVI: Could not open input file: ", filename)
    f = file(filename, "rb")
    if (header.offset > 0)
        readBin(f, raw(), n = header.offset)
    switch(data.type,
		X <- readBin(f, integer(), n = size, size = 1, signed = FALSE), # caso 1
		X <- readBin(f, integer(), n = size, size = 2, endian = endian), # caso 2
		X <- readBin(f, integer(), n = size, endian = endian), # caso 3
		X <- readBin(f, double(), n = size, size = 4, endian = endian), # caso 4
		X <- readBin(f, double(), n = size, endian = endian), # caso 5
		X <- readComplexFloat(f, size = size, endian = endian), # caso 6
		,  # caso 7
		,  # caso 8
		X <- readBin(f, complex(), n = size, endian = endian), # caso 9
		, # caso 10
		, # caso 11
		X <- readBin(f, integer(), n = size, size = 2, endian = endian, signed = FALSE) # caso 12
	)
    close(f)
    Fields = c("bil", "bip", "bsq")
    j = match(interleave, Fields)
    if (length(j) == 0)
        stop("read.ENVI: Error in input header file ", headerfile,
            " incorrect interleave type")
    switch(j, {
        dim(X) <- c(nCol, nBand, nRow)
        X <- aperm(X, c(3, 1, 2))
    }, {
        dim(X) <- c(nBand, nCol, nRow)
        X <- aperm(X, c(3, 2, 1))
    }, {
        dim(X) <- c(nCol, nRow, nBand)
        X <- aperm(X, c(2, 1, 3))
    })
    if (nBand == 1)
        dim(X) = c(nRow, nCol)
    return(X)
}

para01 <- function(x) {
  valores <- range(x)
  y = (x - valores[1]) / (valores[2] - valores[1])
  y
}


# x <- myread.ENVI(file='ESAR97HH.DAT', headerfile='ESAR97HH.hdr')

#subx <- x[500:900,500:900]
#modsubx <- Mod(subx)
#escala = range(modsubx)
#modsubx01 = (modsubx - escala[1]) / (escala[2]-escala[1])

#image(modsubx01, col = gray(seq(0:50)/50))

#guardarpng <- function (imagen, nombre){
#Programa para guardar las imagenes de salida en formato PNG
#guardarpng es una función que recibe dos parámetros:
#la imagen a ser guardada y el nombre con el cual va a ser guardada.
#png es una función que abre escritura en archivo formato png.
#se le ingresan las dimensiones: ancho es columnas [2], alto es filas
#[1]
#png(filename= nombre, width=dim(imagen)[2], height=dim(imagen)[1])
#Se crea un vector de ceros para eliminar los márgenes
#vectorzeros=replicate(4,0)
#par es una función que elimina los márgenes.
#par(mar=vectorzeros, oma=vectorzeros, omi=vectorzeros)
#se grafica la imagen
#plot(imagen)
#es necesario detener la función para que no se sobreescriba el
#archivo
#cada vez que quiero guardar una imagen en formato png
#dev.off()
#return(0)
#}