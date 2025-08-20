#################################################    HOMEWORK - HTP IN PLANT BREEDING (HW 2 E 3)    ############################################
#  DISCENTE: JOSÉ ARTUR DE OLIVEIRA CASIMIRO
#  DOCENTES: ROBERTO FRITSCHE-NETO E JULIO CESAR DoVALE

#contato: artur.casimiro@alu.ufc.br
#atualização: 19/08/2025

##################################################################################################################################
# ----------------------------------------------------------------------------------
#                          ÍNDICES DE VEGETAÇÃO (HOMEWORK 2)
# ----------------------------------------------------------------------------------
# Este bloco documenta os parâmetros e as referências atualizadas para os índices
# de vegetação selecionados para a análise de dados de sensoriamento remoto.
# ----------------------------------------------------------------------------------

# === 1. TGI (Triangular Greenness Index) - VISÍVEL ===
#
# Descrição: Sensível ao conteúdo de clorofila foliar. Útil para quantificar
#            o vigor da vegetação e para fenotipagem de alto rendimento com VANTs.
# Equação: TGI = -0.5 * [190 * (Banda_Vermelho - Banda_Verde) - 120 * (Banda_Vermelho - Banda_Azul)]
# Espaço Paramétrico: -0.5 a +0.5. Valores positivos e mais altos indicam maior vigor.
# Referência Atual: Lu, J., et al. (2020). Remote Sensing, 12(19), 3192.
# Contexto da Ref.: Revisão sobre o uso de índices RGB com VANTs para fenotipagem de culturas.

# === 2. NDWI (Normalized Difference Water Index) - MULTIESPECTRAL ===
#
# Descrição: Estima o conteúdo de água no dossel vegetal. Ferramenta chave para
#            a análise de estresse hídrico e seleção de genótipos tolerantes à seca.
# Equação: NDWI = (Banda_NIR - Banda_SWIR) / (Banda_NIR + Banda_SWIR)
# Espaço Paramétrico: -1.0 a +1.0. Valores > 0.3 geralmente indicam boa disponibilidade hídrica.
# Referência Atual: El-Hendawy, S., et al. (2019). Journal of Agronomy and Crop Science, 205(1), 1-15.
# Contexto da Ref.: Estudo prático demonstrando o uso do NDWI para avaliar a tolerância à seca em cereais.

# === 3. PRI (Photochemical Reflectance Index) - HIPERESPECTRAL ===
#
# Descrição: Indicador da eficiência do uso da luz (LUE). Permite a detecção precoce de
#            estresse fisiológico, antes mesmo do aparecimento de sintomas visuais.
# Equação: PRI = (Banda_531nm - Banda_570nm) / (Banda_531nm + Banda_570nm)
# Espaço Paramétrico: -0.2 a +0.2 (faixa prática para vegetação). Valores menores indicam maior estresse.
# Referência Atual: Zarco-Tejada, P. J., et al. (2018). In Hyperspectral Imaging in Agriculture...
# Contexto da Ref.: Aplicação de ponta com VANTs hiperspectrais para detecção de estresse em culturas.

# ----------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------

# Bibliotecas
library(raster)      
library(EBImage)     
library(foreach)
library(doParallel)
library(sp)
library(tidyr)
library(pliman)
library(data.table)
library(rlas)
library(lidR)

detectCores()
registerDoParallel(cores = 15) 

getDoParWorkers()
###########################

# importando os mosaicos
dir()
mosaic_parrot <- raster::stack("../datasets/mosaics/60DAS_22_03_sequoia.tif")
print(mosaic_parrot)

#plot(mosaic_parrot) # plota todas as bandas
#plot(mosaic_parrot, 1) # plota apenas a banda 1
#click(mosaic_parrot) # plota a imagem composta - composição de pixels

mosaic_VT <- stack("../datasets/mosaics/60DAS_22_03_drone.tif")
print(mosaic_VT)
plot(mosaic_VT) # plota todas as bandas
plot(mosaic_VT, 1) # plota apenas a banda 1
plotRGB(mosaic_VT) # plota a imagem composta
#click(mosaic_VT) # plota a imagem composta - composição de pixels

# importando os shapefiles (parcelas)
plots_rep1 <- shapefile("../datasets/plots/PI_R1_DAS.shp")
print(plots_rep1)
plot(plots_rep1, add = T, col = "White")


# Obtendo informações específicas
extent(plots_rep1) 
length(plots_rep1) 
plot(plots_rep1[360,]) 
head(plots_rep1@data, 6)

plots_rep1@data$cat


# recortando o raster pelas parcelas - uma função que era em paralelo, mas tirei pois meu pc crashou algumas vezes...
plot_clip <- function(ortho, shape) {
  results <- foreach(i = 1:length(shape), .packages = "raster") %do% {
    temp <- mask(crop(x = ortho, y = extent(shape[i,])), shape[i,])
    return(temp)
  }
  return(results)
}

# recortando todas as parcelas
rasterbyplots <- plot_clip(mosaic_VT, plots_rep1)

# vamos checar
class(rasterbyplots)
length(rasterbyplots)
plotRGB(rasterbyplots[[360]])
plot(plots_rep1[360,], add = T, col = "grey")
dev.off()

# Removendo o solo/fundo usando o método Otsu para determinar o limiar (threshold) com base no histograma de valores dos pixels
EX1 <- rasterbyplots[[360]]

# Primeiro, vamos calcular o índice EXG (Excess Green Index)
ExGI <- overlay(x = EX1,
                fun = function(B, G, R, NIR, RE) {
                  
                  # normalizando
                  bn = B / 255
                  gn = G / 255
                  rn = R / 255
                  
                  b = bn / (bn + gn + rn)
                  g = gn / (bn + gn + rn)
                  r = rn / (bn + gn + rn)
                  
                  return((2 * g) - r - b)
                  
                }
)

# vamos checar
plot(ExGI)

# obtém o limiar (threshold) para esta parcela
thr <- EBImage::otsu(x = raster::as.matrix(ExGI),
                     range = c(min(raster::as.matrix(ExGI), na.rm = TRUE),
                               max(raster::as.matrix(ExGI), na.rm = TRUE)
                     ),
                     levels = 256)

# criando uma máscara usando o valor de Otsu
soilMask  <- ExGI > thr

# removendo o solo
plotXMasked <- mask(x = EX1, mask = soilMask, maskvalue = FALSE)
plotRGB(plotXMasked)

# aplicando para todas as parcelas
plots_without_soil <- list()

for (i in 1:length(rasterbyplots)) {
  cat("plot", i, "\n")
  
  ExGI <- overlay(x = rasterbyplots[[i]],
                  fun = function(B, G, R, NIR, RE) {
                    
                    # normalizando
                    bn = B / 255
                    gn = G / 255
                    rn = R / 255
                    
                    b = bn / (bn + gn + rn)
                    g = gn / (bn + gn + rn)
                    r = rn / (bn + gn + rn)
                    
                    return((2 * g) - r - b)
                    
                  }
  )
  
  thr <- EBImage::otsu(x = raster::as.matrix(ExGI),
                       range = c(min(raster::as.matrix(ExGI), na.rm = TRUE),
                                 max(raster::as.matrix(ExGI), na.rm = TRUE)
                       ),
                       levels = 256)
  
  soilMask  <- ExGI > thr
  plots_without_soil[[i]] <- mask(x = rasterbyplots[[i]], mask = soilMask, maskvalue = FALSE)
  
}

length(plots_without_soil)
print(plots_without_soil[[359]])
plotRGB(plots_without_soil[[359]])

########################################
# Índices e cobertura do dossel
########################################
# Estimando os índices
NGRDI <- overlay(x = plots_without_soil[[360]],
                 fun = function(Red, Green, Blue){
                   return(((Green - Red)/(Green + Red)))})
class(NGRDI)
plot(as.raster(NGRDI))
median(raster::as.matrix(NGRDI), na.rm = T) # mediana para o índice. Muito melhor que a média
plots_rep1@data[360, "C_R"] # ID no delineamento experimental

# Estimar a área do dossel
NGRDI.df <- as.data.frame(NGRDI)
head(NGRDI.df)
sum(!is.na(NGRDI.df)) / dim(NGRDI.df)[1] *100

# aplicando para todas as parcelas
output <- data.frame()

for (i in 1:length(plots_without_soil)) {
  cat("plot", i, "\n")
  
  idx.NGRDI = overlay(x = plots_without_soil[[i]],
                      fun = function(Red, Green, Blue){
                        return(((Green - Red)/(Green + Red)))})
  
  idx.ExGI = overlay(x = plots_without_soil[[i]],
                     fun = function(Red, Green, Blue){
                       return(((2*Green/256) - Green/256 - Red/256))})
  
  # ----- INDICE TGI -----
  idx.TGI = overlay(x = plots_without_soil[[i]],
                    fun = function(Red, Green, Blue){
                      return(Green - 0.39*Red - 0.61*Blue)}) # foi solicitado no homework 3
  
  idx.NGRDI.df <- as.data.frame(idx.NGRDI)
  idx.ExGI.df <- as.data.frame(idx.ExGI)
  idx.TGI.df <- as.data.frame(idx.TGI) 
  
  output <- rbind(output, data.frame
                  (
    C_R = plots_rep1@data[i, "C_R"],
    NGRDI = median(raster::as.matrix(idx.NGRDI), na.rm = T),
    ExGI = median(raster::as.matrix(idx.ExGI), na.rm = T),
    
    
    TGI = median(raster::as.matrix(idx.TGI), na.rm = T), # foi solicitado no homework 3
  
    
    area_perc_NGRDI = sum(!is.na(idx.NGRDI.df)) / dim(idx.NGRDI.df)[1] *100,
    area_perc_ExGI = sum(!is.na(idx.ExGI.df)) / dim(idx.ExGI.df)[1] *100,
    
    
    area_perc_TGI = sum(!is.na(idx.TGI.df)) / dim(idx.TGI.df)[1] *100 # foi solicitado no homework 3
    
    
  ))
}

head(output)
tail(output)
cor(output$NGRDI, output$TGI)  #cORRELAÇÃO
########################################
# Contando o número de objetos - estande de plantas
########################################

# importando o mosaico
mosaic_V3 <- stack("../datasets/mosaics/18DAS_09_02_drone.tif")
plotRGB(mosaic_V3) # plota a imagem composta

# importando os shapefiles (parcelas)
plots_rep1 <- shapefile("../datasets/plots/PI_R1_DAS.shp")
plot(plots_rep1, add = T, col = "White")

# recortando o raster pelas parcelas - uma função em paralelo
plot_clip <- function(ortho, shape) {
  results <- foreach(i = 1:length(shape), .packages = "raster") %do% {
    temp <- mask(crop(x = ortho, y = extent(shape[i,])), shape[i,])
    return(temp)
  }
  return(results)
}

# recortando todas as parcelas - isso leva 2 minutos ou dois séculos (T-T)
V3plots <- plot_clip(mosaic_V3, plots_rep1)

# Removendo o solo/fundo usando o método Otsu para determinar o limiar com base no histograma de valores dos pixels
EX2 <- V3plots[[360]]
plotRGB(EX2)

#calcular o índice EXG (Excess Green Index)

ExGI <- overlay(x = EX2,
                fun = function(B, G, R, NIR, RE) {
                  # normalizando
                  bn = B / 255
                  gn = G / 255
                  rn = R / 255
                  b = bn / (bn + gn + rn)
                  g = gn / (bn + gn + rn)
                  r = rn / (bn + gn + rn)
                  return((2 * g) - r - b)
                }
)

# vamos checar
plot(ExGI)

# obtém o limiar (threshold) para esta parcela
thr <- EBImage::otsu(x = raster::as.matrix(ExGI),
                     range = c(min(raster::as.matrix(ExGI), na.rm = TRUE),
                               max(raster::as.matrix(ExGI), na.rm = TRUE)
                     ),
                     levels = 256)

thr

# criando uma máscara usando o valor de Otsu
soilMask  <- ExGI > thr

# removendo o solo
plotXMasked <- mask(x = EX2, mask = soilMask, maskvalue = FALSE)
plotRGB(plotXMasked)

# salvando os rasters
writeRaster(EX2, filename=file.path("plotX.grd"), overwrite = TRUE, format="raster", options="INTERLEAVE=BAND")
writeRaster(soilMask, filename=file.path("plotXMasked.grd"), overwrite = TRUE, format="raster")

# salvando a parcela mascarada como imagem em um diretório temporário
temp <- tempdir()
png(file.path(temp, "plotX.png"), height=nrow(plotXMasked), width=ncol(plotXMasked))
plotRGB(plotXMasked, maxpixels=ncell(plotXMasked))
dev.off()

# carregando novamente
img <- pliman::image_import(file.path(temp, "plotX.png"))
plot(img)
out <- pliman::analyze_objects(img, marker = "id")$results

# definindo um limiar (threshold) para o tamanho do objeto
sort(out$area, decreasing = T)[1:20]
thr.count <- sort(out$area, decreasing = T)[20]
out <- out[out$area > thr.count,]
nrow(out) # número de objetos na parcela

################################### aplicando para todas as parcelas ##############################
system.time(
  results.st <- foreach(i = 1:length(V3plots),
                        .packages = c("raster", "pliman"),
                        .combine = "rbind",
                        # .export = c("mask","overlay"),
                        .multicombine = TRUE,
                        .errorhandling = "remove",
                        .verbose = TRUE
  ) %do% {
    
    EX2 <- V3plots[[i]]
    ExGI <- overlay(x = EX2,
                    fun = function(B, G, R, NIR, RE) {
                      # normalizando
                      bn = B / 255
                      gn = G / 255
                      rn = R / 255
                      b = bn / (bn + gn + rn)
                      g = gn / (bn + gn + rn)
                      r = rn / (bn + gn + rn)
                      return((2 * g) - r - b)
                    }
    )
    thr <- EBImage::otsu(x = raster::as.matrix(ExGI),
                         range = c(min(raster::as.matrix(ExGI), na.rm = TRUE),
                                   max(raster::as.matrix(ExGI), na.rm = TRUE)
                         ),
                         levels = 256)
    soilMask  <- ExGI > thr
    plotXMasked <- mask(x = EX2, mask = soilMask, maskvalue = FALSE)
    temp <- tempdir()
    png(file.path(temp, "plotX.png"), height=nrow(plotXMasked), width=ncol(plotXMasked))
    plotRGB(plotXMasked, maxpixels=ncell(plotXMasked))
    dev.off()
    img <- pliman::image_import(file.path(temp, "plotX.png"))
    out <- pliman::analyze_objects(img, marker = "id", verbose = F, plot = F)$results
    out <- out[out$area > thr.count,]
    
    output <- data.frame(
      C_R = plots_rep1@data[i, "C_R"],
      Count = nrow(out),
      plot_area_px = dim(img)[1] * dim(img)[2],
      canopy_area = sum(out$area),
      canopy_area_perc = sum(out$area) / (dim(img)[1] * dim(img)[2]) *100
      
    )
  }
)

dim(results.st)
head(results.st)
tail(results.st)

###############################
# Altura de planta via nuvem de pontos
###############################

# importando as nuvens de pontos
c60DAS <- fread("../datasets/cloudpoints/60_DAS_CP.txt")
colnames(c60DAS) <- c("X","Y", "Z")
head(c60DAS)

c0DAS <- fread("../datasets/cloudpoints/0_DAS_CP.txt")
colnames(c0DAS) <- c("X","Y", "Z")
head(c0DAS)

# criando arquivos LAS
write.las(file = (file.path(getwd(), "60.laz")), header = header_create(c60DAS), data = c60DAS)
write.las(file = (file.path(getwd(), "0.laz")), header = header_create(c0DAS), data = c0DAS)

# carregando os arquivos LAS
las60 <- readLAS(files = "60.laz", select = "xyz")
las0 <- readLAS(files = "0.laz", select = "xyz")

# importando os shapefiles (parcelas)
plots_rep1 <- shapefile("../datasets/plots/PI_R1_DAS.shp")
print(plots_rep1)
plot(plots_rep1,
     #add = T,
     col = "Red")

## Função auxiliar para recortar a nuvem de pontos pelo shapefile (feições)
clipper.cloud <- function(cloud, shape){
  pc <- list()
  for(i in 1:nrow(shape)){
    p <- shape[i,]
    c <- clip_rectangle(cloud, xleft = p@bbox['x','min'], ytop = p@bbox['y','max'], xright = p@bbox['x','max'], ybottom = p@bbox['y','min'])
    if(!is.null(c)){
      pc[[i]] <- c
      names(pc)[i] <- paste0(shape$row[i],"_",shape$col[i])}}
  pc <- pc[!unlist(lapply(pc, is.null))]
  return(pc)
}

system.time( # leva 1 minuto
  plot.cloud60 <- clipper.cloud(las60, plots_rep1) # recorta a nuvem para a altura do dossel da parcela
)

plot.cloud0 <- clipper.cloud(las0, plots_rep1) # recorta a nuvem para o nível do solo da parcela


# aplicando percentil para a altura do dossel
p90.60 <- lapply(plot.cloud60, function(x) { quantile(x@data$Z, .90) }) # percentil 90
p90.60$`40_1`

# visualizando as formas
plot(plot.cloud60$`40_1`@data$Y, plot.cloud60$`40_1`@data$X) # visão nadir (de cima)
plot(plot.cloud60$`40_1`@data$Y, plot.cloud60$`40_1`@data$Z) # visão frontal
plot(plot.cloud60$`40_1`@data$X, plot.cloud60$`40_1`@data$Z) # visão lateral
abline(h = p90.60$`40_1`, col = "Blue")

# aplicando percentil para a elevação do solo
p50.0 <- lapply(plot.cloud0, function(x) { quantile(x@data$Z, .5) }) # percentil 50
p50.0$`40_1`
plot(plot.cloud0$`40_1`@data$Y, plot.cloud0$`40_1`@data$Z) # vista lateral
abline(h = p50.0$`40_1`, col = "Red")

# estimando a altura de planta para todas as parcelas
PH <- data.frame()

for (i in 1:length(p90.60)) {
  cat("plot", i, "\n")
  PH <- rbind(PH, data.frame(
    C_R = names(p90.60)[[i]],
    Height = round(as.numeric(p90.60[[i]]) - as.numeric(p50.0[[i]]), 2)
  ))
}

head(PH)
tail(PH)


##### Fim #######
