# Libraries
require(imager)
require(raster)
require(magick)


# loading and plotting and image
lena <- load.image("lena.jpg")
plot(lena)
dim(lena)

###################################################
# Transformation 1: Linear (brightness/contrast)
# Equation: I'(x,y) = α * I(x,y) + β
###################################################
alpha <- 1.2   # contraste
beta  <- 0.2   # brilho
lena.linear <- lena
lena.linear[,,,1] <- pmin(1, pmax(0, alpha * lena[,,,1] + beta))

###################################################
# Transformation 2: Logarithmic
# Equation: I'(x,y) = c * log(1 + I(x,y))
###################################################
c <- 1 / log(1 + 1)  # normalização para faixa 0–1
lena.log <- lena
lena.log[,,,1] <- c * log(1 + lena[,,,1])

###################################################
# visual comparison (plots lado a lado)
###################################################
par(mfrow = c(1,3))
plot(lena, main = "Original Lena")
plot(lena.linear, main = "Linear (α=1.2, β=0.2)")
plot(lena.log, main = "Logarithmic Transformation")

###################################################
# histograms's comparison
###################################################
par(mfrow = c(1,3))
hist(lena[,,,1], main="Hist: Original", col="gray", xlab="Pixel value")
hist(lena.linear[,,,1], main="Hist: Linear", col="gray", xlab="Pixel value")
hist(lena.log[,,,1], main="Hist: Log", col="gray", xlab="Pixel value")


#### Explanation ####

# 1) Linear Transformation:
# I'(x,y) = α * I(x,y) + β   (α=1.2, β=0.2)
# - Increases brightness and contrast
# - Histogram shifts to higher values
# - Result: brighter image with enhanced mid-tones

# 2) Logarithmic Transformation:
# I'(x,y) = c * log(1 + I(x,y))   (c = 1/log(2))
# - Expands low pixel values (enhances shadows)
# - Compresses high values (prevents saturation)
# - Histogram redistributed towards mid-tones
# - Result: more visible details in dark regions

#### Explicação #####

# 1) Transformação Linear:
# I'(x,y) = α * I(x,y) + β   (α=1.2, β=0.2)
# - Aumenta brilho e contraste
# - Histograma desloca-se para valores maiores
# - Resultado: imagem mais clara e com realce nos tons médios

# 2) Transformação Logarítmica:
# I'(x,y) = c * log(1 + I(x,y))   (c = 1/log(2))
# - Expande valores baixos (realça sombras)
# - Comprimi valores altos (clareia sem saturar)
# - Histograma redistribuído para tons intermediários
# - Resultado: mais detalhes nas regiões escuras
