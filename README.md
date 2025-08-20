# BASIC-R
Repositório dedicado ao aprendizado dos fundamentos da linguagem R aplicados à pesquisa em Plant Science. Abordamos desde a importação e limpeza de dados até a criação de visualizações gráficas para análise exploratória.

Análise de Dados de Experimento Agronômico em R
Nota: Este repositório foi desenvolvido como parte da resolução de uma atividade acadêmica, com o objetivo de aplicar e demonstrar conceitos de análise de dados, estatística experimental e modelagem em R.

1. Introdução
Este projeto apresenta uma análise estatística completa de dados provenientes de um experimento de melhoramento genético de plantas. O foco da análise foi avaliar características agronômicas como Massa Seca da Parte Aérea (SDM), Área de Superfície Radicular (SRA) e, principalmente, a Eficiência Agronômica do Nitrogênio (NAE).

O objetivo foi explorar a variabilidade genética entre diferentes genótipos, os efeitos de distintos níveis de adubação nitrogenada e a relação entre as características, utilizando a linguagem R e diversos pacotes estatísticos.

2. Metodologia
O pipeline de análise seguiu os seguintes passos:

Análise Descritiva e Visualização: Sumarização inicial dos dados e criação de gráficos (histogramas, boxplots, gráficos de dispersão) com ggplot2 para um entendimento preliminar do comportamento das variáveis.

Controle de Qualidade: Verificação da normalidade dos dados (testes visuais e Shapiro-Wilk). Foi identificada uma forte assimetria na variável NAE, que foi corrigida com sucesso através da transformação de dados com o pacote bestNormalize.

Análise de Variância (ANOVA) e Testes de Médias: Avaliação da significância dos fatores (genótipo, nitrogênio, interação) e agrupamento das médias pelo teste de Scott-Knott para identificar os grupos de desempenho.

Modelos Mistos: Utilização do pacote breedR para uma abordagem mais robusta, permitindo a estimação de componentes de variância e parâmetros genéticos como a herdabilidade.

3. Principais Resultados
A análise dos dados permitiu extrair conclusões importantes:

Alto Potencial de Seleção: A herdabilidade no sentido amplo (h²) para a Eficiência no Uso de Nitrogênio (NAE) foi estimada em 0.68, um valor considerado alto que indica forte controle genético sobre a característica e, consequentemente, um elevado potencial para o sucesso da seleção de genótipos superiores.

Interação Genótipo x Ambiente (GxA) Significativa: Foi constatado que os genótipos respondem de maneira diferente aos níveis de adubação. O teste de Scott-Knott foi eficaz em identificar os materiais de elite tanto na média geral quanto em condições específicas de baixo N, uma informação estratégica para programas de melhoramento.

Precisão Experimental: O Coeficiente de Variação (CV) do experimento foi de aproximadamente 27%, indicando uma precisão experimental de nível médio, o que é aceitável para experimentos conduzidos a campo.

4. Estrutura do Repositório

    4.1  README.md         # Este arquivo de apresentação

    4.2 script_analise.R  # Script R completo com todos os passos da análise

    4.3 data/

    4.4 data.txt      # Conjunto de dados brutos utilizado no script


5. Como Utilizar
Para replicar esta análise, siga os passos:

Clone este repositório.

Abra o arquivo script_analise.R no RStudio.

Certifique-se de que o arquivo data.txt esteja no diretório de trabalho correto ou ajuste o caminho no script.

Instale os pacotes necessários e execute o script.

Pacotes Utilizados
ggplot2

ScottKnott

bestNormalize

breedR

drc

devtools
