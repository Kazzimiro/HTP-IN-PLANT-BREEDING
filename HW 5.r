#################################################     HOMEWORK - HTP IN PLANT BREEDING (HW 5)     ############################################
#  DISCENTE: JOSÉ ARTUR DE OLIVEIRA CASIMIRO
#  DOCENTES: ROBERTO FRITSCHE-NETO E JULIO CESAR DoVALE

#contato: artur.casimiro@alu.ufc.br
#atualização: 20/08/2025

##################################################################################################################################


# --- 1. Bibliotecas ---

library(reticulate)
library(tensorflow)
library(keras3)
library(Cairo)
require(imager)
library(abind)
library(ggplot2)

# --- 2. Carregamento e Preparação dos Dados ---

# Defina aqui o seu diretório de trabalho.
# NOTA: Altere o caminho abaixo para o local onde a pasta "haploidseed" está no seu computador.
setwd("C:/Users/casim/OneDrive/Área de Trabalho/DISCIPLINAS CONDENSADAS/htp in plant science/R Labs/datasets/haploidseed")

# OTIMIZAÇÃO 1: Leitura Segura de Arquivos
# Filtramos a lista de arquivos para ler apenas imagens .jpg ou .jpeg.
# Por quê? deu problema no meu e tive de recorrer a isso, estava lendo o arquivo "desktok.ini"

image_files <- dir(pattern = "\\.jpg$|\\.jpeg$")

print(paste("Total de imagens:", length(image_files)))

# OTIMIZAÇÃO 2: Redimensionamento das Imagens
# Definimos um tamanho padrão menor para as imagens.
# Por quê? Imagens menores (256x256) treinam muito mais rápido que as
# originais (500x500) e ajudam o modelo a focar nas características
# principais, ignorando ruído de alta resolução.

new_size <- 256


X <- list()
for (i in 1:length(image_files)) {
  img <- load.image(image_files[i])
  img_resized <- resize(img, size_x = new_size, size_y = new_size)
  X[[i]]  <- as.array(img_resized[,,1,])
}

# Combina a lista de imagens em um único array 4D (imagens, altura, largura, canais)
X <- abind(X, along = 0)
dim(X)

# Loop para criar o vetor Y (rótulos/classes)
Y <- c()
for (i in 1:length(image_files)) {
  # Extrai a classe do nome do arquivo (ex: "D" de "D_103.jpg")
  Y  <- c(Y, strsplit(image_files[i], "_", fixed = T)[[1]][1])
}

# Converte os rótulos de texto para números (0, 1, 2)
Y <- as.matrix(Y)
Y <- as.matrix(as.numeric(as.factor(Y))) - 1
table(Y) # Mostra a contagem de imagens por classe

# --- 3. Divisão dos Dados em Treino e Teste ---
set.seed(29121983) # Garante que a divisão seja sempre a mesma
index <- rep(sample(c(TRUE, FALSE), 150, replace = TRUE, prob = c(0.75, 0.25)), 3)

Y_train <- Y[index]
Y_test <- Y[!index]
X_train <- X[index,,,]
X_test <- X[!index,,,]
test = list(x = X_test, y = Y_test)
rm(X) # Limpa a memória

# --- 4. Construção do Modelo de Rede Neural Convolucional (CNN) ---

# OTIMIZAÇÃO 3: Aumento de Dados (Data Augmentation)
# Esta camada cria novas imagens de treino em tempo real, aplicando
# transformações aleatórias (giros, inversões).
# Por quê? Simula um dataset muito maior, forçando o modelo a generalizar
# e aprender as características reais das sementes, o que melhora a acurácia. POrém, verificou se um loss elevado, provavelmente em decorrência disso. 

data_augmentation <- keras_model_sequential(name = "data_augmentation") %>%
  layer_random_flip("horizontal_and_vertical") %>%
  layer_random_rotation(0.2)

# CORREÇÃO CRÍTICA: O modelo agora é construído em um bloco único e contínuo.
# Isso garante que todas as camadas sejam salvas no objeto 'model'.
#MOTIVO: travei nesse erro e tive de recorrer a IA pois não sabia como resolver.

model <- keras_model_sequential() %>%
  
  # Camada de entrada com o tamanho das imagens redimensionadas e aumento de dados
  data_augmentation(input_shape = c(new_size, new_size, 3)) %>%
  
  # --- Base de Convolução (Extrator de Características) ---
  # Bloco 1
  layer_conv_2d(filters = 16, kernel_size = c(3,3), activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  
  # Bloco 2
  layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  
  # Bloco 3
  layer_conv_2d(filters = 128, kernel_size = c(3,3), activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  
  # Bloco 4
  layer_conv_2d(filters = 128, kernel_size = c(3,3), activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  
  # --- Topo de Classificação (Decisor) ---
  layer_flatten() %>% # Achata os mapas de características 2D para um vetor 1D
  layer_dense(units = 512, activation = "relu") %>%
  
  # OTIMIZAÇÃO 4: Regularização Forte (Dropout)
  # "Desliga" 50% dos neurônios aleatoriamente durante o treino.
  # Por quê? Evita que o modelo memorize os dados de treino (overfitting).
  layer_dropout(0.5) %>%
  layer_dense(units = 512, activation = "relu") %>%
  layer_dropout(0.5) %>%
  
  # OTIMIZAÇÃO 5: Função de Ativação Correta
  # Camada final com 3 neurônios (um para cada classe).
  # Por quê? 'softmax' é a função matematicamente correta para classificação
  # multiclasse, pois converte as saídas em probabilidades que somam 1.
  layer_dense(units = 3, activation = "softmax")

# Mostra um resumo da arquitetura completa do modelo
summary(model)

# --- 5. Compilação e Treinamento do Modelo ---
model %>% compile(
  optimizer = "adam",
  loss = "sparse_categorical_crossentropy", # Função de perda para classificação multiclasse com rótulos inteiros
  metrics = "accuracy"
)

# OTIMIZAÇÃO 6: Aumento do Tempo de Treino
# Aumentamos as épocas para dar ao modelo tempo suficiente para aprender com as
# milhares de imagens virtuais geradas pelo aumento de dados.
history <- model %>%
  fit(
    x = X_train, y = Y_train,
    epochs = 10, 
    validation_data = unname(test),
    verbose = 2
  )

# --- 6. Avaliação dos Resultados ---

# Gráfico da evolução do treino
plot(history)

# Gráfico da perda (loss) do modelo
plot(history$metrics$loss, main="Perda do Modelo", xlab = "Época", ylab="Perda", col="blue", type="l")
lines(history$metrics$val_loss, col="green")
legend("topright", c("Treino","Teste"), col=c("blue", "green"), lty=1)

# Gráfico da acurácia (accuracy) do modelo
plot(history$metrics$acc, main="Acurácia do Modelo", xlab = "Época", ylab="Acurácia", col="blue", type="l")
lines(history$metrics$val_acc, col="green")
legend("bottomright", c("Treino","Teste"), col=c("blue", "green"), lty=1)

# Avaliação final do modelo nos dados de teste
scores <- evaluate(model, X_test, Y_test, verbose = 0)
print(paste0("Acurácia final no teste: ", round(scores$accuracy * 100, 2), "%"))

# Predição das classes para os dados de teste
predictions_matrix <- model %>% predict(X_test) %>% as.matrix()
# O índice de 'which.max' começa em 1, subtraímos 1 para alinhar com as classes (0, 1, 2)
pred.classes <- apply(predictions_matrix, 1, which.max) - 1

# Matriz de Confusão
print("Matriz de Confusão (Linhas = Real, Colunas = Previsto):")
table(Y_test, pred.classes)

# --- 7. Salvando o Modelo ---
# Salva o modelo treinado para uso futuro
save_model(model = model, filepath = "../../5 - Mining and modeling on R/modelo_otimizado.keras")
print("Modelo salvo com sucesso!")

########################### Fim do Script ###########################
