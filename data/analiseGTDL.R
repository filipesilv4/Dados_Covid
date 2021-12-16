library(readr)
library(dplyr)
library(psych)
library(ggplot2)
library(survival)
library(survminer)
library(bbmle)

dataset <- read_csv("datasetcovid.csv")

#Transformando em fator as variáveis

dataset$CS_SEXO <- factor(dataset$CS_SEXO,
                               label = c("M", "F"),
                               levels = c(0,1))

dataset$CS_ESCOL_N <- factor(dataset$CS_ESCOL_N,
                                  labels = c("Sem escolaridade", "Fundamental 1", "Fundamental 2", "Medio", "Superior"),
                                  levels = 0:4,
                                  order = T)

dataset$CS_RACA <- factor(dataset$CS_RACA,
                               labels = c("Branca", "Preta", "Amarela", "Parda", "Indigena"),
                               levels = 1:5)

dataset$OBESIDADE <- factor(dataset$OBESIDADE,
                                 labels = c("Nao", "Sim"),
                                 levels = c(0,1))

dataset$ASMA <- factor(dataset$ASMA,
                            labels = c("Nao", "Sim"),
                            levels = c(0,1))

dataset$DIABETES <- factor(dataset$DIABETES,
                                labels = c("Nao", "Sim"),
                                levels = c(0,1))

dataset$CARDIOPATI <- factor(dataset$CARDIOPATI,
                                  labels = c("Nao", "Sim"),
                                  levels = c(0,1))

dataset$UTI <- factor(dataset$UTI,
                           labels = c("Nao", "Sim"),
                           levels = c(0,1))

dataset$NU_IDADE_N <- factor(cut(dataset$NU_IDADE_N,
                            c(0,19,60,102)),
                            labels = c("Jovem", "Adulto", "Idoso"),
                            order = T)
                          

glimpse(dataset)
attach(dataset)

#Analise Descritiva

# tabela cruzada sexo e escolaridade
tabela_1 <- table(CS_SEXO, CS_ESCOL_N)
prop.table(tabela_1)

# MUDAR
#hist(NU_IDADE_N, freq = F,  xlab = "Idade", ylim = c(0, 0.04), main = "Distribuição das idades", col = "lightblue")
#curve(dnorm(x, mean = mean(NU_IDADE_N), sd = sd(NU_IDADE_N)), add = T)
#Tabela por faixa etária 0-19, 20-59, 60 >
#tabela_2 <- table(cut(NU_IDADE_N, breaks = c(0, 19, 59, 102)))
#summary(NU_IDADE_N)
# Distribuição da idade por grupos: sexo e escolaridade.
#describeBy(NU_IDADE_N, group = CS_SEXO:CS_ESCOL_N)

# Distribuição e boxplot entre obesidade e tempo entre os primeiros sintomas e a data de evolução
tapply(TEMP_SIN, OBESIDADE, summary)
tapply(TEMP_SIN, OBESIDADE, sd)
plot(TEMP_SIN ~ OBESIDADE, ylab = "Tempo" , xlab = "Obesidade")

# Distribuição e boxplot entre asma e tempo entre os primeiros sintomas e a data de evolução
tapply(TEMP_SIN, ASMA, summary)
tapply(TEMP_SIN, ASMA, sd)
plot(TEMP_SIN ~ ASMA, ylab = "Tempo", xlab = "Asma")


# Distribuição e boxplot entre diabetes e tempo entre os primeiros sintomas e a data de evolução
tapply(TEMP_SIN, DIABETES, summary)
tapply(TEMP_SIN, DIABETES, sd)
plot(TEMP_SIN ~ DIABETES, ylab = "Tempo", xlab = "Diabates")

# Distribuição e boxplot entre cardiopatia e tempo entre os primeiros sintomas e a data de evolução
tapply(TEMP_SIN, CARDIOPATI, summary)
tapply(TEMP_SIN, CARDIOPATI, sd)
plot(TEMP_SIN ~ CARDIOPATI, ylab = "Tempo", xlab = "Cardiopatia")

#UTI X Obitos
obito <- factor(EVOLUCAO,
                labels = c("Sim", "Nao"),
                levels = c(1,0))

tabela_3 <- table(UTI, obito)
row.names(tabela_3) <- c("NAO UTI", "UTI")
tabela_3

mosaicplot(tabela_3, col = c("aquamarine", "lightblue"), cex =1.1,
           main = "Pacientes Internados" )

Q2 <- chisq.test(tabela_3)
Q2

# Sexo X Obito
tabela_4 <- table(CS_SEXO, obito)
row.names(tabela_4) <- c("Masculino", "Feminino")
tabela_4

mosaicplot(tabela_4, col = c("lightblue", "red"), cex =1.1,
           main = "Sexo dos Pacientes Internados" )

Q2 <- chisq.test(tabela_4)
Q2

# Obesidade X Obito
tabela_5 <- table(OBESIDADE, obito)
row.names(tabela_5) <- c("Possui_Obesidade", "Nao_Possui_Obesidade")
tabela_5

mosaicplot(tabela_5, col = c("#836FFF", "#483D8B"), cex =1.1,
           main = "Pacientes com Obesidade" )


Q2 <- chisq.test(tabela_5)
Q2

# Diabetes x Obito
tabela_6 <- table(DIABETES, obito)
row.names(tabela_6) <- c("Possui_Diabetes", "Nao_Possui_Diabetes")
tabela_6

barplot(tabela_6,
        main = "Pacientes com Diabetes",
        ylim = c(0, 2500),
        col = c("#87CEFA", "#4682B4")
)

Q2 <- chisq.test(tabela_6)
Q2

# Cardiopatia X Obito
tabela_7 <- table(CARDIOPATI, obito)
row.names(tabela_7) <- c("Possui_Cardiopatia", "Nao_Possui_Cardiopatia")
tabela_7
barplot(tabela_7,
        main = "Pacientes com Cardiopatia",
        ylim = c(0, 2300),
        col = c("#7FFFD4", "#5F9EA0"))

Q2 <- chisq.test(tabela_7)
Q2

# Asma X Obito
tabela_8 <- table(ASMA, obito)
row.names(tabela_8) <- c("Possui_Asma", "Nao_Possui_Asma")
tabela_8


barplot(tabela_8,
        beside = T,
        main = "Pacientes com Asma",
        ylim = c(0, 2200),
        col = c("#F4A460", "#FFDEAD"))

Q2 <- chisq.test(tabela_8)
Q2

#Idade X Obito
tabela_9 <- table(NU_IDADE_N, obito)
tabela_9

barplot(tabela_9,
        beside = TRUE,
        legend = c("Jovem", "Adulto", "Idoso"),
        args.legend = list(x = "top", bty = "n", ncol = 3),
        col = c("#1E90FF", "#00008B", "#191970"),
        ylim = c(0, 1550),
        main = "Pacientes por Faixa Etária")

Q2 <- chisq.test(tabela_9)
Q2


# Escolaridade X Obito
tabela_10 <- table(CS_ESCOL_N, obito)
tabela_10

barplot(tabela_10,
        beside = TRUE,
        legend = c("Sem escolaridade", "Fundamental 1", "Fundamental 2 ", "Médio", "Superior"),
        args.legend = list(x = "top", bty = "n", ncol = 5),
        col = c("#FA8072","#E9967A","#FFA07A","#FF7F50","#FF6347"),
        ylim = c(0, 900),
        main = "Pacientes por Escolaridade")

Q2 <- chisq.test(tabela_10)
Q2


# Raca X Obito
tabela_11 <- table(CS_RACA, obito)
tabela_11

barplot(tabela_11,
        beside = TRUE,
        legend = c("Branca", "Preta", "Amarela", "Parda", "Indigena"),
        args.legend = list(x = "top", bty = "n", ncol = 5),
        col = c("#FFFF00", "#FFD700", "#FFA500", "#FF8C00", "#FF4500"),
        ylim = c(0, 1700),
        main = "Pacientes por Raça")

#(usar teste exato de fisher)
Q2 <- chisq.test(tabela_11)
Q2

#Analise de Sobrevivência

# Gráficos de Sobrevivência de Kaplan-Meier, gráfico de taxa (função) falha acumulada
# e teste logRANK.

## UTI
km_1 <- dataset %>% 
  with(survfit(Surv(TEMP_SIN,EVOLUCAO) ~ UTI))

ggsurvplot(km_1, risk.table = F,
           palette = c("#FF9E29", "#86AA00"),
           data = dataset)

ggsurvplot(km_1, conf.int = F, 
           palette = c("#FF9E29", "#86AA00"),
           risk.table = F, risk.table.col = "strata",
           fun = "event", data = dataset)

survdiff(Surv(TEMP_SIN,EVOLUCAO) ~ UTI, rho=0)

## ASMA
km_2 <- dataset %>% 
  with(survfit(Surv(TEMP_SIN,EVOLUCAO) ~ ASMA))

ggsurvplot(km_2, risk.table = F,
           palette = c("#11F872", "#1DF0EC"),
           data = dataset)

ggsurvplot(km_2, conf.int = F, 
           palette = c("#11F872", "#1DF0EC"),
           risk.table = F, risk.table.col = "strata",
           fun = "event", data = dataset)

survdiff(Surv(TEMP_SIN,EVOLUCAO) ~  ASMA, rho=0)

##CARDIOPATIA
km_3 <- dataset %>% 
  with(survfit(Surv(TEMP_SIN,EVOLUCAO) ~ CARDIOPATI))

ggsurvplot(km_3, risk.table = F,
           palette = c("#0FD99C", "#0FA7D9"),
           data = dataset)

ggsurvplot(km_3, conf.int = F, 
           palette = c("#0FD99C", "#0FA7D9"),
           risk.table = F, risk.table.col = "strata",
           fun = "event", data = dataset)

survdiff(Surv(TEMP_SIN,EVOLUCAO) ~ CARDIOPATI, rho=0)

#DIABETES
km_4 <- dataset %>% 
  with(survfit(Surv(TEMP_SIN,EVOLUCAO) ~  DIABETES))

ggsurvplot(km_4, risk.table = F,
           palette = c("#FAC0EE", "#6125D9"),
           data = dataset)

ggsurvplot(km_4, conf.int = F, 
           palette = c("#FAC0EE", "#6125D9"),
           risk.table = F, risk.table.col = "strata",
           fun = "event", data = dataset)

survdiff(Surv(TEMP_SIN,EVOLUCAO) ~ DIABETES, rho=0)

## FAIXA ETÁRIA
km_5 <- dataset %>% 
  with(survfit(Surv(TEMP_SIN,EVOLUCAO) ~  NU_IDADE_N))

ggsurvplot(km_5, risk.table = F,
           data = as.list(NU_IDADE_N))

ggsurvplot(km_5, conf.int = F,
           risk.table = F, risk.table.col = "strata",
           fun = "event", data = as.list(NU_IDADE_N))


survdiff(Surv(TEMP_SIN,EVOLUCAO) ~ NU_IDADE_N, rho=0)

## ESCOLARIDADE
km_6 <- dataset %>% 
  with(survfit(Surv(TEMP_SIN,EVOLUCAO) ~  CS_ESCOL_N))

ggsurvplot(km_6, risk.table = F,
           palette = c("#F0E207", "#43BAF0", "#7289F0", "#F034C2", "#F08D18"),
           data = dataset)

ggsurvplot(km_6, conf.int = F, 
           palette = c("#F0E207", "#43BAF0", "#7289F0", "#F034C2", "#F08D18"),
           risk.table = F, risk.table.col = "strata",
           fun = "event", data = dataset)


survdiff(Surv(TEMP_SIN,EVOLUCAO) ~ CS_ESCOL_N, rho=0)

#Obesidade
km_7 <- dataset %>% 
  with(survfit(Surv(TEMP_SIN,EVOLUCAO) ~  OBESIDADE))

ggsurvplot(km_7, risk.table = T,
           palette = c("#F0E257", "#43BAF8"),
           data = dataset)

ggsurvplot(km_7, conf.int = F, 
           palette = c("#F0E257","#43BAF8"),
           risk.table = F, risk.table.col = "strata",
           fun = "event", data = dataset)


survdiff(Surv(TEMP_SIN,EVOLUCAO) ~ CS_ESCOL_N, rho=0)

## Residuos de Schoenfeld
cox_covid <- coxph(Surv(TEMP_SIN,EVOLUCAO) ~ UTI + ASMA + CARDIOPATI + DIABETES
                   + NU_IDADE_N +  CS_ESCOL_N + OBESIDADE)

summary(cox_covid)

par(mfrow = c(3,3))
plot(cox.zph(cox_covid))

################################################################################
#GTDL

library(readr)
library(dplyr)
library(bbmle)

dataset <- read_csv("datasetcovid.csv")

dataset <- dataset %>% mutate(INTER = 1)

dataset2 <- dataset %>% select(
  TEMP_SIN,
  EVOLUCAO,
  INTER,
  ASMA,
  CARDIOPATI,
  DIABETES,
  OBESIDADE,
  UTI
  #NU_IDADE_N,
  #CS_ESCOL_N
)


lvero3 <- function(theta_, data_){
  
  theta_ <- unlist(theta)
  data_ <- as.matrix(data_)
  alpha <- theta_[2]
  lambda <- theta_[1]
  par <- theta[seq(from = 1, to = 3, by = 2)]
  
  n <- ncol(data_)
  temp <- data_[,1]
  temp <- as.matrix(temp)
  cens <- data_[,2]
  x <- data_[,3:n]
  x <- as.matrix(x)
  dim <- ncol(x)
  beta_ <- matrix(par[1:2], ncol = dim, nrow=1)#??
  
  phi <- x %*% t(beta_)#??
  at <- alpha*temp
  
  fun_p <- exp(at + phi) / (1 + exp(at + phi))
  fun_q <- 1 / (1 + exp(at + phi))
  fun_g <- 1 + exp(phi)
  
  f <- sum(cens*log(fun_p) + (lambda/alpha)*(log(fun_q) + log(fun_g)))
  return(f)

}

maxGTDL3 <- function(theta, data_){
  
  model <- mle2(lvero3, start = list(theta) , data = list(data_))
  
  return(summary(model))
  
}

theta <- c(1,1,-1)
maxGTDL3(theta, dataset2)
lvero3(theta, dataset2)

