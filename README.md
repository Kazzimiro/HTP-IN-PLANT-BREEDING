Análise de Dados de Fenotipagem de Alto Rendimento (HTP) com R
Visão Geral
Nota Importante: Este repositório foi desenvolvido como parte da avaliação da disciplina "HTP in Plant Breeding", ministrada pelos professores Roberto Fritsche-Neto e Julio Cesar DoVale. O código-base, os dados e a metodologia foram fornecidos pelo Prof. Roberto Fritsche-Neto, sendo este trabalho uma implementação e adaptação para a resolução dos exercícios propostos (Homework 2 e 3).

Este repositório apresenta um pipeline em R para análise de dados de Fenotipagem de Alto Rendimento (HTP) em experimentos de melhoramento de plantas. O objetivo é automatizar a coleta de dados fenotípicos a partir de ortomosaicos e nuvens de pontos gerados por VANTs (drones).

Índice
Funcionalidades Principais

Tecnologias Utilizadas

Pré-requisitos e Instalação

Estrutura de Dados Esperada

Como Utilizar

Autoria e Contato

Direitos Autorais e Uso

Funcionalidades Principais
O pipeline implementa as seguintes análises:

Extração de Índices de Vegetação e Cobertura de Dossel:

Recorta o ortomosaico geral para cada parcela individual.

Aplica uma máscara para remover o solo, utilizando o limiar de Otsu sobre o índice de vegetação ExG (Excess Green).

Calcula índices de vegetação (ex: NGRDI, TGI) e a percentagem de cobertura do dossel para cada parcela.

Contagem de Plantas (Estande):

Utiliza imagens de estágios fenológicos iniciais.

Realiza a segmentação da vegetação e aplica algoritmos de análise de objetos para identificar e contar o número de plantas em cada parcela.

Estimativa de Altura de Plantas:

Processa nuvens de pontos (.laz) de dois momentos: solo exposto (Dia 0) e com a cultura estabelecida (ex: Dia 60).

Calcula a altura da planta subtraindo a elevação do solo (mediana, P50) da elevação do topo do dossel (percentil 90), um método robusto contra outliers.

O processamento é otimizado para performance através de computação paralela.

Tecnologias Utilizadas
Este projeto foi desenvolvido inteiramente em R e faz uso dos seguintes pacotes principais:

raster e sp: para manipulação de dados geoespaciais (ortomosaicos e shapefiles).

lidR e rlas: para processamento e análise de nuvens de pontos LiDAR.

pliman e EBImage: para análise de imagem e segmentação.

foreach e doParallel: para implementação de processamento paralelo e ganho de performance.

data.table: para leitura e manipulação eficiente de grandes arquivos de dados.

Pré-requisitos e Instalação
Para executar este script, você precisará ter o R e, preferencialmente, o RStudio Desktop instalados.

Com o R aberto, execute o comando abaixo para instalar todas as bibliotecas necessárias:

R

install.packages(c(
  "raster", 
  "EBImage", 
  "foreach", 
  "doParallel", 
  "sp", 
  "tidyr", 
  "pliman", 
  "data.table", 
  "rlas", 
  "lidR"
))
Estrutura de Dados Esperada
Para que o script funcione corretamente, os arquivos de dados devem seguir a estrutura de pastas abaixo. O script principal deve estar localizado em uma pasta (ex: scripts/), e os dados em uma pasta datasets/ no nível superior.

seu-projeto/
├── datasets/
│   ├── mosaics/
│   │   ├── 60DAS_22_03_sequoia.tif
│   │   └── ...
│   ├── plots/
│   │   └── PI_R1_DAS.shp
│   └── cloudpoints/
│       ├── 60_DAS_CP.txt
│       └── ...
├── scripts/
│   └── htp_analysis.R      <-- SEU SCRIPT R FICA AQUI
└── README.md
Como Utilizar
Clone o repositório:

Bash

git clone https://github.com/seu-usuario/seu-repositorio.git
cd seu-repositorio
Organize seus dados conforme a estrutura detalhada na seção anterior.

Abra o script htp_analysis.R no RStudio.

Ajuste os caminhos (paths) dos arquivos no início do script, caso sua estrutura de pastas seja diferente.

Configure o número de cores para o processamento paralelo na linha registerDoParallel(cores = 15).

Execute o script para realizar as análises.

Autoria e Contato
Código-Base e Metodologia (Docente)

Prof. Roberto Fritsche-Neto

Implementação da Atividade (Discente)

José Artur de Oliveira Casimiro

Contato: artur.casimiro@alu.ufc.br

Direitos Autorais e Uso
O conteúdo deste repositório, incluindo os scripts e a metodologia, é de domínio do Prof. Roberto Fritsche-Neto e foi utilizado para fins estritamente acadêmicos no contexto da disciplina "HTP in Plant Breeding". A republicação ou utilização deste material deve ser feita com a devida atribuição à fonte original.
