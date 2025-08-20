#################################################     HOMEWORK - HTP IN PLANT BREEDING (HW 4)     ############################################
#  DISCENTE: JOSÉ ARTUR DE OLIVEIRA CASIMIRO
#  DOCENTES: ROBERTO FRITSCHE-NETO E JULIO CESAR DoVALE

#contato: artur.casimiro@alu.ufc.br
#atualização: 20/08/2025

##################################################################################################################################

# Pacotes/Bibliotecas
library(raster)
library(data.table)
library(foreach)
library(doParallel)
library(EBImage)
library(ggplot2)

# Configurando o número de núcleos para processamento paralelo - ACABOU SENDO PROCESSADO SEQUENCIALMENTE (PC NÃO TANKOU)
registerDoParallel(cores = detectCores() - 1)
getDoParWorkers()

# Definindo o diretório de trabalho e carregando os dados
setwd("../datasets/hyperspectral")
dir()

img.rgb <-  stack("B1-RGB.tif") 
plotRGB(img.rgb)

img.hyper <-  brick("B1.bil", continuousdata = TRUE)
print(img.hyper)
class(img.hyper)

# ATENÇÃO: Rode o código abaixo e clique nos 4 cantos da sua área de interesse na imagem
corners <- NULL
for(i in 1:4){
  aux <- locator(type = "p", n = 1, col = "red", pch = 19)
  corners <- rbind(corners, c(aux$x, aux$y))
}
corners
corners <- rbind(corners, corners[1,])
colnames(corners) <- c("x", "y")
lines(corners, col= "red", type = "l", lty = 2, lwd = 3)

# Criando um polígono espacial a partir das coordenadas clicadas
plg <- Polygons(list(Polygon(corners)), "x")
shpfl <- SpatialPolygonsDataFrame(SpatialPolygons(list(plg)), 
                                  data.frame(z=1, 
                                             row.names=c("x") ) )

# Alinhando as projeções entre o polígono e a imagem
raster::projection(shpfl) <- raster::projection(img.rgb)

# Definindo a grade de recorte (1 linha e 1 coluna para pegar a área inteira)
grid <- raster(shpfl, 
               nrows=1, 
               ncols=1, 
               crs = proj4string(img.rgb))

# Convertendo a grade para um polígono
shpfl <- rasterToPolygons(grid)
length(shpfl)

# Checando se o polígono de recorte foi criado corretamente sobre a imagem
dev.off()
plotRGB(img.rgb)
plot(shpfl, add = T, col = "white")

# Função para recortar as imagens usando o polígono de forma paralela
plot_clip <- function(ortho, shape) {
  results <- foreach(i = 1:length(shape), .packages = "raster") %dopar% {
    temp <- mask(crop(x = ortho, y = extent(shape[i,])), shape[i,])
    return(temp)
  }
  return(results)} 

# Recortando a imagem RGB
rasterbyplots <- plot_clip(img.rgb, shpfl)
class(rasterbyplots)
length(rasterbyplots)
plotRGB(rasterbyplots[[1]])

# Recortando a imagem hiperespectral
hyper.crop <-  plot_clip(img.hyper, shpfl)
print(hyper.crop)
class(hyper.crop)
length(hyper.crop)


############### ---- ANÁLISE EXPLORATÓRIA: Escolhendo o melhor índice para a máscara ---- ##################

# Selecionando a primeira imagem recortada como exemplo
EX1 <- rasterbyplots[[1]]

# Calculando os índices de vegetação para a imagem de exemplo
ExGI <- overlay(x = EX1,
                fun = function(B, G, R, NIR, RE) {
                  bn = B / 255; gn = G / 255; rn = R / 255
                  b = bn / (bn + gn + rn); g = gn / (bn + gn + rn); r = rn / (bn + gn + rn)
                  return((2 * g) - r - b)
                }
)

NGRDI <- overlay(x = EX1,
                 fun = function(Red, Green, Blue){
                   return(((Green - Red)/(Green + Red)))}) 

VARI <- overlay(x = EX1,
                fun = function (Red, Green, Blue){
                  return((Green - Red) / (Green + Red - Blue))
                })

# Visualizando os índices calculados (imagens em tons de cinza)
plot(ExGI)
plot(NGRDI)
plot(VARI)

# Interaja com o gráfico para ver os valores dos pixels, se desejar
click(NGRDI)
click(VARI)

# Obtendo o limiar (threshold) de Otsu para cada índice
thr <- EBImage::otsu(x = raster::as.matrix(ExGI),
                     range = c(min(raster::as.matrix(ExGI), na.rm = TRUE),
                               max(raster::as.matrix(ExGI), na.rm = TRUE)
                     ),
                     levels = 256)

thr2 <- EBImage::otsu(x = raster::as.matrix(NGRDI),
                      range = c(min(raster::as.matrix(NGRDI), na.rm = TRUE),
                                max(raster::as.matrix(NGRDI), na.rm = TRUE)
                      ),
                      levels = 256)

thr3 <- EBImage::otsu(x = raster::as.matrix(VARI),
                      range = c(min(raster::as.matrix(VARI), na.rm = TRUE),
                                max(raster::as.matrix(VARI), na.rm = TRUE)
                      ),
                      levels = 256)

# Criando as máscaras binárias com base nos limiares
soilMask  <- ExGI > thr
soilMask2 <- NGRDI > thr2
soilMask3 <- NGRDI < -.06 | NGRDI > .06 # Máscara com filtro manual
soilMask4 <- VARI > thr3
plot(soilMask3)
plot(soilMask4)


# Início do Ranking Quantitativo dos Índices
cat("--- Ranking de Índices pela Mediana ---\n")

# Usando a máscara do NGRDI com Otsu (soilMask2) como referência
ref_mask <- soilMask2
pixels_plant <- which(raster::as.matrix(ref_mask) == 1)
pixels_background  <- which(raster::as.matrix(ref_mask) == 0)

# Calculando a diferença das medianas para cada índice
median_plant_exgi <- median(raster::as.matrix(ExGI)[pixels_plant], na.rm = TRUE)
median_background_exgi  <- median(raster::as.matrix(ExGI)[pixels_background], na.rm = TRUE)
delta_exgi <- median_plant_exgi - median_background_exgi

median_plant_ngrdi <- median(raster::as.matrix(NGRDI)[pixels_plant], na.rm = TRUE)
median_background_ngrdi  <- median(raster::as.matrix(NGRDI)[pixels_background], na.rm = TRUE)
delta_ngrdi <- median_plant_ngrdi - median_background_ngrdi

median_plant_vari <- median(raster::as.matrix(VARI)[pixels_plant], na.rm = TRUE)
median_background_vari  <- median(raster::as.matrix(VARI)[pixels_background], na.rm = TRUE)
delta_vari <- median_plant_vari - median_background_vari

# Criando e exibindo a tabela de ranking
ranking <- data.frame(
  Index = c("ExGI", "NGRDI", "VARI"),
  Median_Plant = c(median_plant_exgi, median_plant_ngrdi, median_plant_vari),
  Median_background = c(median_background_exgi, median_background_ngrdi, median_background_vari),
  Delta_Median = c(delta_exgi, delta_ngrdi, delta_vari)
)
ordenated_ranking <- ranking[order(ranking$Delta_Median, decreasing = TRUE), ]
print(ordenated_ranking)


# Aplicando as máscaras para remover o fundo e visualizar os resultados
plotXMasked <- mask(x = EX1, mask = soilMask, maskvalue = FALSE)
plotRGB(plotXMasked)
plotXMasked2 <- mask(x = EX1, mask = soilMask2, maskvalue = FALSE)
plotRGB(plotXMasked2)
plotXMasked3 <- mask(x = EX1, mask = soilMask3, maskvalue = FALSE)
plotRGB(plotXMasked3)
plotXMasked4 <- mask(x = EX1, mask = soilMask4, maskvalue = FALSE)
plotRGB(plotXMasked4)

# Aplicando a máscara escolhida na imagem hiperespectral
hyper_Masked <- raster::mask(x = hyper.crop[[1]], mask = soilMask3)

# Extraindo os nomes dos comprimentos de onda (wavebands)
wavebands <- round(as.numeric(
  gsub(".nm", "", 
       gsub("X", "", names(hyper_Masked)))))

length(wavebands)
wavebands
aux2 <- as.array(hyper_Masked)
length(aux2)
class(aux2)

# Calculando a assinatura espectral (mediana da reflectância por banda)
med <- unlist(apply(as.array(hyper_Masked), MARGIN = 3, median, na.rm = T))
med

# Plotando a assinatura espectral da primeira amostra
plot(wavebands, med, type = "l", col = "red")


################ ---- PROCESSAMENTO EM LOTE: Aplicando o fluxo de trabalho para todas as imagens ---- ################

# Listando os nomes dos arquivos a serem processados
hyper.names <- dir()[dir() %like% ".bil$"]
hyper.names
rgb.names <- dir()[dir() %like% ".tif$"]
rgb.names

# Extraindo os IDs das amostras a partir dos nomes dos arquivos
img.id <- gsub(".bil", "", hyper.names)
img.id
length(img.id)

# Início do loop para processar todas as imagens
system.time(
  results_wide <- foreach(i = 1:length(img.id), 
                          .packages = c("raster", "EBImage"),
                          .combine = "rbind") %do% {
                            
                            i.h <- raster::brick(hyper.names[i], continuousdata = TRUE)
                            i.rgb <- stack(rgb.names[i])
                            h.c <- crop(i.h, shpfl)
                            v.c <- crop(i.rgb, shpfl)
                            
                            ExGI <- overlay(x = v.c, fun = function(B, G, R) {
                              bn=B/255; gn=G/255; rn=R/255; b=bn/(bn+gn+rn); g=gn/(bn+gn+rn); r=rn/(bn+gn+rn)
                              return((2*g)-r-b)
                            })
                            
                            NGRDI <- overlay(x = v.c, fun = function(Red, Green, Blue){
                              denom <- Green + Red; denom[denom==0] <- 1e-6
                              return(((Green - Red)/denom))
                            })
                            
                            VARI <- overlay(x = v.c, fun = function(Red, Green, Blue) {
                              denom <- Green + Red - Blue; denom[denom == 0] <- 1e-6
                              return((Green - Red) / denom)
                            })
                            
                            thr_otsu <- EBImage::otsu(raster::as.matrix(VARI), range = c(minValue(VARI), maxValue(VARI)))
                            Mask <- VARI > thr_otsu
                            
                            h.m <- raster::mask(x = h.c, mask = Mask, maskvalue = FALSE)
                            md_hyper <- unlist(apply(as.array(h.m), MARGIN = 3, median, na.rm = T))
                            
                            md_exgi <- median(ExGI[Mask == 1], na.rm = TRUE)
                            md_ngrdi <- median(NGRDI[Mask == 1], na.rm = TRUE)
                            md_vari <- median(VARI[Mask == 1], na.rm = TRUE)
                            
                            output_hyper <- as.data.frame(t(md_hyper))
                            colnames(output_hyper) <- paste0("w", wavebands)
                            
                            output <- data.frame(
                              id = img.id[i],
                              mediana_ExGI = md_exgi,
                              mediana_NGRDI = md_ngrdi,
                              mediana_VARI = md_vari
                            )
                            
                            output_final <- cbind(output, output_hyper)
                            
                            return(output_final)
                          }
)

# Visualizando o início e o fim do data frame final
head(results_wide)
tail(results_wide)

# Salvando o data frame final em um arquivo .csv
write.csv(results_wide, "hyperdata_com_indices.csv", row.names = FALSE)

# Plotando as assinaturas espectrais de TODAS as amostras juntas
ggplot(results, aes(x = wavebands, y = reflectance, group = id, col = id)) +
  geom_line(aes(linetype = id)) 

###### FIM ###########
