
#################################################    HOMEWORK - R IN PLANT SCIENCE    ############################################
#  DISCENTE: JOSÉ ARTUR DE OLIVEIRA CASIMIRO
#  DOCENTES: ROBERTO FRITSCHE-NETO E JULIO CESAR DoVALE

#contato: artur.casimiro@alu.ufc.br
#atualização: 19/08/2025

##################################################################################################################################


#INSTALAR E CARREGAR PACOTES
# --------------------
#CRAN
install.packages("MultivariateAnalysis")
install.packages("devtools")
install.packages("BiocManager")
#GITHUB
library(devtools)
devtools::install_github('famuvie/breedR')
BiocManager::install("impute")
# carregando-os
library(breedR)
getwd()
data <- read.table("C:/Users/casim/OneDrive/Área de Trabalho/DISCIPLINAS CONDENSADAS/Basic-R-in-Plant-Science-main/data.txt", header = TRUE, na.strings = NA)

#VISUALIZAÇÃO DOS DADOS
head(data)  
tail(data) 
dim(data) 
str(data)   
# AJUSTANDO PARA FACTOR E NUMERIC
data$plot <-  as.factor(data$plot)
str(data)
# PARA TODAS AS COLUNAS
data[,1:9] <- lapply(data[,1:9], as.factor)
str(data)

#---------------------------- ANÁLISE DESCRITIVA -----------------------#
par(mfrow = c(2,2))
summary(data)     # sumário geral
mean(data$NAE)# média da variavel pedida
mean(data$SDM)# média de uma variável comentada em aula
sd(data$NAE)      # desvio padrão
sd(data$SDM)
boxplot(data$NAE)# boxplot
boxplot (data$SDM)# boxplot visto em sala
cor(data$NAE, data$SRA) # correlação entre duas traits
cor(data[,10:12], use = "pairwise", method = "spearman")

abs(round(cor(data[,10:12], use = "pairwise", method = "spearman"), 1))
round(cor(data[ ,10:12], use = "pairwise"),2) # correlação entre várias traits
heatmap(round(cor(data[ ,10:12], use = "pairwise"),2))

# fazendo o mesmo para várias traits ao mesmo tempo
apply(data[ ,10:12], 2, mean) # 2 aplica na coluna, 1 na linha
apply(data[,10:12], c(1,2), mean)

# verificar o design experimental e a distribuição espacial

library(desplot)
d1 <- desplot(data,
              NAE ~ row*col,
              out1 = rep,
              out2 = N,
              out2.gpar=list(col = "green", lwd = 1, lty = 1))
print(d1)

#CONTROLE DE QUALIDADE
# -----------------------
# identificando outliers
boxplot(data$NAE, col = "red")
#install.packages("lme4")
library(lme4)
# detecção e eliminação de outliers
fit <- lm(NAE ~ type + row + col + N + gid, data = data)
#install.packages("car")
library(car)
(outlier <- names(outlierTest(fit)$p))
data[outlier,"NAE"]
data[outlier,"NAE"] <- NA

# testando a normalidade
# Primeiro, vamos verificar usando padrões
data$NAE
length(data$NAE)
rnorm(220)
plot(rnorm(220))
plot(density(rnorm(220)))
shapiro.test(rnorm(220)) # distribuição normal
plot(density(runif(220)))
shapiro.test(runif(220)) # distribuição uniforme
# então,
shapiro.test(data$NAE)
plot(density(na.omit(data$NAE)))

install.packages("bestNormalize")
require(bestNormalize)
NAEadj <- bestNormalize(data$NAE,
                        standardize = FALSE,
                        allow_orderNorm = TRUE,
                        out_of_sample = FALSE)
NAEadj$chosen_transform
shapiro.test(NAEadj$x.t)
data$NAEadj <- NAEadj$x.t
head(data)

# Gráfico de normalidade Quartil-Quartil (Q-Q) para os resíduos
fit <- lm(NAE ~ type + row + col + N + gid, data = data)
fit2 <- lm(NAEadj ~ type + row + col + N + gid, data = data)

par(mfrow = c(2,2)) # organiza a janela de plotagem em 2 linhas e 2 colunas
qqnorm(resid(fit))
qqline(resid(fit), col = "red")
qqnorm(resid(fit2))
qqline(resid(fit2), col = "blue")
hist(data$NAE, col = "red", main = "NAE", xlab = "NAE")
hist(data$NAEadj, col = "blue", main = "NAE Ajustado", xlab = "NAE Ajustado")
dev.off()

# salvando os arquivos de dados mais recentes
str(data)
head(data)
write.table(data, "data_clean.txt")
write.csv(data, "data_clean.csv")

#---------------------------- TIPOS IMPORTANTES DE GRÁFICOS ----------------------------#

install.packages("ggplot2")
library(ggplot2)

colnames(data)

# Histograma
p1 <- ggplot(data, aes(x = NAE)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "red", color = "black") +
  geom_density(color = "darkblue", size = 1) +
  labs(title = "Histograma e Densidade de NAE", x = "NAE", y = "Densidade") +
  theme_minimal()

p1

# Boxplot
p2 <- ggplot(data, aes(x = type, y = NAE, fill = N)) +
  geom_boxplot() +
  theme_minimal()
p2

# Gráfico de Dispersão
p3 <- ggplot(data, aes(x = SRA, y = NAE)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal()
p3
 # correlação negativa entre NAE e SRA

# Gráfico de Barras
p4 <- ggplot(data, aes(x = type, fill = type)) +
  geom_bar() +
  theme_minimal()
p4
 #DIFERENÇA SIGNIFICATIVA ENTRE OS PARENTAIS E OS GENOTIPOS

# Gráfico de Linhas
p5 <- ggplot(data, aes(y = NAE, x = N, group = gid, color = gid)) +
  geom_line() +
  theme_minimal()
p5
# VERIFICA-SE A DIFERENÇA DOS GENOTIPOS EMERGIDOS EM DIFERENTES NIVEIS DE NAE, OBSERVANDO A VARIABILIDADE ENTRE ELES.

# combinando e salvando gráficos
plot_list <- list(p1, p2, p3, p5)

p_final <- ggstatsplot::combine_plots(
  plotlist = plot_list,
  plotgrid.args = list(nrow = 2))

p_final

ggsave(filename = './Fig1_NAE.pdf',
       plot = p_final,
       device = 'pdf',
       width = 300,
       height = 300,
       units = 'mm',
       dpi = 300)


# ----------------------------- ANOVA (Análise de Variância) e testes de comparação ---------------------------------------#
# ANOVA
# Útil para entender a significância e a importância dos fatores na sua variável resposta

# primeiro modelo
anova_1 <- aov(NAE ~ gid, data = data)
summary(anova_1)
plot(anova_1)

# um modelo mais realista
anova_2 <- aov(NAE ~ rep + gid + N + gid*N, data = data)
anova_2 <- aov(NAE ~ rep + gid*N, data = data)
summary(anova_2)
plot(anova_2)

# estimar CV% e herdabilidade
summary(anova_2)

# CV% - nos dá uma ideia da precisão de todo o experimento
# CV = sqrt(Ve) / media * 100
Ve <- summary(anova_2)[[1]][5,3]
CV <- sqrt(Ve) / mean(na.omit(data$NAE)) *100
CV

# herdabilidade (modelo fixo)
# h2g = Vg / (Vg + Ve/rep) = Vg / Vp
# nos dá uma ideia da acurácia de todo o experimento
# a proporção explicada pelos genótipos,
# a correlação entre fenótipos e genótipos

data$rep
unique(data$rep)
length(unique(data$rep))

n.reps <- length(unique(data$rep))
n.reps
Ve <- summary(anova_2)[[1]][5,3]
Vg <- (summary(anova_2)[[1]][2,3] - Ve)/n.reps
h2g <- Vg / (Vg + Ve / n.reps)
h2g

# discussão cv e herdabilidade. observar herdabilidade nesse material de estudo.


# Teste TUKEY HSD para fator no modelo

# Níveis de N
TukeyHSD(anova_2, "N", ordered = TRUE, conf.level=.95)
plot(TukeyHSD(anova_2, "N", ordered = TRUE,conf.level=.95))
# genótipos
TukeyHSD(anova_2, "gid", ordered = TRUE, conf.level=.95)
plot(TukeyHSD(anova_2, "gid", ordered = TRUE,conf.level=.95))

# ---------------------------- Teste SCOTT-KNOTT --------------------#

install.packages("ScottKnott")
library(ScottKnott)
# Genótipos
sk1 <- SK(x = anova_2, which = "gid")
summary(sk1)
# Níveis de N
sk2 <- SK(x = anova_2, which = "N")
summary(sk2)
# gid:N
sk3 <-  with(data, # Corrigido de FE para data
             SK(NAE ~ rep + gid + gid * N,
                data = data,
                which='N:gid',
                fl1=2 # 1 ou 2 são níveis de N
             ))
summary(sk3)

# ---------------------------- REGRESSÕES ----------------------------- #

# útil para entender a significância e a importância de variáveis numéricas para explicar sua variável resposta
colnames(data)

# 6.1 Linear = y = a + bX + e
lm_model <- lm(NAE ~ SRA, data = data)
summary(lm_model)
model_summary <- summary(lm_model)

# Acessar R-quadrado
model_summary$r.squared

# Acessar tabela de coeficientes
model_summary$coefficients

# criando um gráfico simples
plot(data$SRA, data$NAE)
abline(lm_model, col = "blue")

# Múltipla = y = a + b1X1 + b2X2 + ... + e
# -------------------------
m_model <- lm(NAE ~ SRA + SDM, data = data)
summary(m_model)
model_summary <- summary(m_model)
# Acessar R-quadrado
model_summary$r.squared
# Acessar tabela de coeficientes
model_summary$coefficients

# criando um gráfico simples
library(car)
# produz gráficos de variáveis adicionadas
avPlots(m_model)

# Modelo quadrático = y = a + bX + b^2X + e
data$SRA2 <- data$SRA^2
head(data)
qd_model <- lm(NAE ~ SRA2, data = data)
summary(qd_model)

# Plotando
# criar sequência de possíveis SRA
range(data$SRA)
SRA_Values <- seq(min(data$SRA, na.rm=T), max(data$SRA, na.rm=T), 0.005)

# criar lista de valores preditos usando o modelo quadrático
NAE_Predict <- predict(qd_model, list(SRA = SRA_Values, SRA2=SRA_Values^2))

# criar gráfico de dispersão dos dados originais

plot(data$SRA, data$NAE, pch=16)
# adicionar linhas de predição baseadas no modelo de regressão quadrática

lines(SRA_Values, NAE_Predict, col='blue')

# Logística - a variável resposta é VERDADEIRO ou FALSO (binomial)
# vamos criar uma em nosso conjunto de dados para os níveis de N

data$N
data$N == "ideal"
as.numeric(data$N == "ideal")
data$N_bi <- as.numeric(data$N == "ideal")
head(data)

mylogit <- glm(N_bi ~ NAE + SRA + SDM, family = "binomial", data = data)
summary(mylogit)
anova(mylogit)
mylogit$coefficients
library(jtools)
summ(mylogit)

# Prever probabilidades
probs <- predict(mylogit, type = "response")

# Definir seu limiar personalizado
threshold <- 0.5

# Classificar com base no limiar
predicted_class <- ifelse(probs > threshold, 1, 0)
# Matriz de confusão

table(Observado = data[names(predicted_class),"N_bi"], Previsto = predicted_class)

# Sigmoidal = y ~ a/(1 + exp(-b * (x-c)) ) + d
# -----------------------
# tende a representar a "biologia" das coisas
library(drc)
fm <- drm(NAE ~ SDM, data = data, fct = G.3())
plot(fm)
summary(fm)



#---------------------------- MODELOS MISTOS ----------------------------#
# modelando melhor, considerando efeitos fixos e aleatórios


library(breedR)
# para este pacote, precisamos "criar" uma coluna para a interação ou efeitos aninhados
head(data)
data$GxN <- paste0(data$gid, data$N)
head(data)

# usando apenas o design experimental clássico
fit1 <- remlf90(fixed = NAE ~ N + rep,
                random = ~gid + GxN,
                data = data)

# componentes de variância
fit1$var
fit1$var[1:2,1] / sum(fit1$var[,1])*100
n.reps <- length(unique(data$rep))
n.levels <- length(unique(data$N))

#CV%
sqrt(fit1$var[3,1]) / mean(na.omit(data$NAE)) *100

# Herdabilidade no sentido amplo
h2g <- fit1$var[1,1] / (fit1$var[1,1] + fit1$var[2,1]/n.levels + fit1$var[3,1]/(n.reps*n.levels))
h2g

# Melhores Predições Lineares Não Viesadas - BLUPs
# para genótipos
fit1$ranef$gid

BLUPs <- fit1$ranef$gid[[1]]
BLUPs

# para a interação G x N
GxN <- fit1$ranef$GxN[[1]]
GxN

# confiabilidade (r2) = 1 - PEV / (Vg + Vg*Fii)
# próximo da herdabilidade
(r <- mean(1 - BLUPs$s.e.^2 / fit1$var[1,1]))

# intervalo de confiança para os BLUPs
DMS <- BLUPs$s.e.*1.96
blups2 <- data.frame(gid = rownames(BLUPs), BLUPs, "DMS" = DMS)
head(blups2)

library(ggplot2)
limits <- aes(ymax = blups2$value + blups2$DMS,
              ymin = blups2$value - blups2$DMS)
p <- ggplot(data = blups2, aes(x = reorder(factor(gid), -value), y = value))
p + geom_jitter(stat = "identity", colour = "red") +
  geom_errorbar(limits, position = position_dodge(0.5),
                width = 0.10) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "gid", y = "BLUP")

# Comparando modelos - LRT e AIC
# para testar um fator do modelo ou comparar modelos, precisamos rodar outros modelos eliminando / incluindo fatores

# Removendo o fator GxN e testar se ele é significativo
fit2 <- remlf90(fixed = NAE ~ N + rep,
                random = ~gid,
                data = data)

# quão bom é o modelo (AIC) - quanto menor, melhor o modelo
fit1$fit$AIC
fit2$fit$AIC

# Teste LRT, >3.84 5%; >6.63 1%
LRT <- abs(fit1$fit$`-2logL` - fit2$fit$`-2logL`)
LRT
pchisq(abs(LRT), 1, lower.tail = F) # teste qui-quadrado


#---------------------------- USANDO LOOPS ----------------------------#
# EXEMPLO BÁSICO 
set.seed(123)
x <- rnorm(10)
x

for(i in 1:length(x)){
  print(be_positive(x[i]))
}

# Reorganizando o arquivo
colnames(data)
traits <- colnames(data)[10:12]
traits

library(reshape2)
# reorganizar os dados
data.melted <- reshape2::melt(data[,c(1:9, 10:12)], measure.vars = traits)
head(data.melted)
tail(data.melted)

# criar uma grade
grid <- expand.grid(traits)
grid

# mais de dois podem ser adicionados para criar uma grade, por exemplo:
grid2 <- expand.grid(trait = traits, N = unique(data$N))
grid2

# criando 
output <- list()

# fazendo o primeiro loop do zero - ANOVA para todas as traits dentro dos níveis de N
head(data.melted)

output <- list()
output_fig <- list()

for(i in 1:nrow(grid2)){
  smpl <- data.melted[data.melted$N == grid2$N[i] &
                        data.melted$variable == grid2$trait[i],]
  smpl <- droplevels.data.frame(smpl)
  fit <- aov(value ~ rep + gid, data = smpl)
  aux <- summary(fit)
  output[[i]] <- aux
  
  output_fig[[i]] <- ggplot(smpl, aes(x = value)) +
    geom_histogram(bins = 10, fill = "skyblue") +
    labs(title = paste(grid2$trait[i], "em Nível", grid2$N[i])) +
    theme_minimal()
}

output
names(output) <- apply(grid2, 1, function(x){paste0(x[1],"_", x[2])})
output
output_fig[[1]] # para ver o primeiro gráfico



################################### 2a questão ###################################

# 2) Escrever uma função para estimar a mediana de um vetor numérico.
# -------------------------------------------------------------------
# Lógica: somamos os elementos ordenados, identificamos a posição central 
# (ou média das duas centrais, se o número de elementos for par).

m_median <- function(x){
  x <- sort(x)                    
  n <- length(x)                  
  if(n %% 2 == 1){                # Vai egar o elemento central, caso seja impar.
    med <- x[(n+1)/2]
  } else {                        # caso seja par, pega a média dos dois
    med <- (x[n/2] + x[n/2 + 1])/2
  }
  return(med)
}

# Teste
x <- c(4,2,7,1,5)
m_median(x)
median(x)  #função base do R.


################################### 3a questão ###################################

# 3) Incluir esta função em um loop para estimar a mediana dos três traços (SDM, SRA e NAE.) usados.
# -------------------------------------------------------------------

traits <- c("SDM","SRA","NAE")
output_median <- list()

for(i in traits){
  vec <- na.omit(data[[i]])               # vai remover valores ausentes
  output_median[[i]] <- m_median(vec)    # aplicar a função criada
}

output_median


