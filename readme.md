# Análise de Dados de Fenotipagem de Alto Rendimento (HTP) com R
Repositório dedicado ao processamento de dados de VANTs (drones) aplicados à Fenotipagem de Alto Rendimento (HTP) em melhoramento de plantas. As rotinas automatizam a extração de traits a partir de imagens multiespectrais e nuvens de pontos.

Implementação dos Homeworks 2 e 3 (HTP in Plant Breeding)
**Nota:** Este repositório foi desenvolvido como parte da avaliação da disciplina "HTP in Plant Breeding", ministrada pelos professores Roberto Fritsche-Neto e Julio Cesar DoVale. O código-base, os dados e a metodologia foram fornecidos pelo **Prof. Roberto Fritsche-Neto**, sendo este trabalho uma implementação e adaptação para a resolução dos exercícios propostos.


1. Introdução ao Pipeline
Este projeto apresenta um pipeline completo para extrair múltiplos traços (ou características) agronômicas a partir de dados de sensoriamento remoto. O foco é a automação da análise de centenas de parcelas experimentais, processando ortomosaicos e nuvens de pontos para obter dados fenotípicos de forma rápida e precisa. As análises foram otimizadas com o uso de processamento paralelo.

2. Metodologia de Análise por Trait
O pipeline de análise executa as seguintes rotinas de forma sequencial:

Extração de Índices de Vegetação e Cobertura de Dossel:** A rotina recorta o ortomosaico geral para cada parcela. Em seguida, aplica uma máscara para remover o solo utilizando o limiar de Otsu sobre o índice ExG (Excess Green). Por fim, calcula índices de vegetação (NGRDI, TGI) e a percentagem de cobertura do dossel para cada parcela limpa.

Contagem de Plantas (Estande):** Utilizando imagens de estágios fenológicos iniciais, o script realiza a segmentação da vegetação e aplica algoritmos de análise de objetos (via pacote `pliman`) para identificar e contar o número de plantas individuais em cada parcela.

Estimativa de Altura de Plantas:** O script processa nuvens de pontos (`.laz`) de dois voos: um com o solo exposto (Dia 0) e outro com a cultura estabelecida. A altura é calculada de forma robusta, subtraindo a elevação do solo (mediana, P50) da elevação do topo do dossel (percentil 90), minimizando o efeito de outliers.

3. Estrutura do Repositório
├── README.md             # Este arquivo de apresentação
├── htp_analysis.R        # Script R completo com todas as análises
└── datasets/
├── plots/
└── cloudpoints/

4. Como Utilizar
Para replicar esta análise, siga os passos:

1. Clone este repositório.
2. Organize seus dados na pasta `datasets/` seguindo a estrutura indicada.
3. Abra o arquivo `htp_analysis.R` no RStudio.
4. Ajuste o número de cores para processamento paralelo na função `registerDoParallel()`.
5. Instale os pacotes necessários e execute o script.

Pacotes Utilizados
- raster
- EBImage
- foreach
- doParallel
- sp
- pliman
- data.table
- rlas
- lidR
