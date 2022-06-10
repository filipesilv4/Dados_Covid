library(GTDL)
library(dplyr)
library(psych)
library(ggplot2)
library(survival)
library(survminer)
library(bbmle)
library(cowplot)
library(gmodels)
data(SP_covid)
str(SP_covid)
dataset <- SP_covid
View(dataset)


#Transformando em fator as variÃ¡veis


dataset$sex <- factor(dataset$sex, labels = c("Masculino", "Feminino"), 
                      levels = c(0,1))

dataset$schooling <- factor(dataset$schooling, 
                            labels = c("Sem escolaridade", "Fundamental 1",
                                       "Fundamental 2", "Medio", "Superior"),
                             levels = 0:4,
                             ordered = T)

dataset$ethnicities <- factor(dataset$ethnicities,
                          labels = c("Branca", "Preta", "Amarela", "Parda",
                                     "Indigena"),
                          levels = c("White", "Black", "Asian", "Pardo",
                                     "Indigenous"))

dataset$obesity <- factor(dataset$obesity,
                            labels = c("Nao", "Sim"),
                            levels = c(0,1))

dataset$asthma <- factor(dataset$asthma,
                       labels = c("Nao", "Sim"),
                       levels = c(0,1))

dataset$diabetes <- factor(dataset$diabetes,
                           labels = c("Nao", "Sim"),
                           levels = c(0,1))

dataset$cardiopathy <- factor(dataset$cardiopathy,
                             labels = c("Nao", "Sim"),
                             levels = c(0,1))

dataset$icu <- factor(dataset$icu,
                      labels = c("Nao", "Sim"),
                      levels = c(0,1))

dataset$age_group <- factor(dataset$age_group,
                             labels = c("Jovem", "Adulto", "Idoso"),
                             levels = c("Young", "Adult", "Elderly"))




#Analise Descritiva

##filtros

#Sem Patologia

dataset2 <- dataset %>% filter(
  dataset$asthma == "Nao" & dataset$cardiopathy == "Nao" &
    dataset$obesity == "Nao" & dataset$diabetes == "Nao"
)

table(dataset2$diabetes)


#Paciente apenas com asma
dataset2 <- dataset %>% filter(
  dataset$asthma == "Sim" & dataset$cardiopathy == "Nao" &
    dataset$obesity == "Nao" & dataset$diabetes == "Nao"
)
table(dataset2$asthma)
prop.table(table(dataset2$asthma, dataset2$censure))
prop.table(table(dataset2$asthma, dataset2$icu))

#Paciente com asma e cardiopatia
dataset2 <- dataset %>% filter(
  dataset$asthma == "Sim" & dataset$cardiopathy == "Sim" &
    dataset$obesity == "Nao" & dataset$diabetes == "Nao"
)
table(dataset2$asthma, dataset2$cardiopathy)

#Paciente com asma e diabetes
dataset2 <- dataset %>% filter(
  dataset$asthma == "Sim" & dataset$cardiopathy == "Nao" & 
    dataset$obesity == "Nao" & dataset$diabetes == "Sim"
)
table(dataset2$asthma, dataset2$diabetes)

#Paciente com asma e obesidade
dataset2 <- dataset %>% filter(
  dataset$asthma == "Sim" & dataset$cardiopathy == "Nao" & 
    dataset$obesity == "Sim" & dataset$diabetes == "Nao"
)
table(dataset2$asthma, dataset2$obesity)

#Paciente apenas com cardiopatia
dataset2 <- dataset %>% filter(
  dataset$asthma == "Nao" & dataset$cardiopathy == "Sim" &
    dataset$obesity == "Nao" & dataset$diabetes == "Nao"
)
table(dataset2$cardiopathy)
prop.table(table(dataset2$cardiopathy, dataset2$censure))
prop.table(table(dataset2$cardiopathy, dataset2$icu))


#Paciente com cardiopatia e diabetes
dataset2 <- dataset %>% filter(
  dataset$asthma == "Nao" & dataset$cardiopathy == "Sim" &
    dataset$obesity == "Nao" & dataset$diabetes == "Sim"
)

table(dataset2$cardiopathy, dataset2$diabetes)

#Paciente com cardiopatia e obesidade
dataset2 <- dataset %>% filter(
  dataset$asthma == "Nao" & dataset$cardiopathy == "Sim" &
    dataset$obesity == "Sim" & dataset$diabetes == "Nao"
)

table(dataset2$cardiopathy, dataset2$obesity)

#Paciente apenas com diabetes
dataset2 <- dataset %>% filter(
  dataset$asthma == "Nao" & dataset$cardiopathy == "Nao" &
    dataset$obesity == "Nao" & dataset$diabetes == "Sim"
)
table(dataset2$diabetes)
prop.table(table(dataset2$diabetes, dataset2$censure))
prop.table(table(dataset2$diabetes, dataset2$icu))

#Paciente com diabetes e obesidade
dataset2 <- dataset %>% filter(
  dataset$asthma == "Nao" & dataset$cardiopathy == "Nao" &
    dataset$obesity == "Sim" & dataset$diabetes == "Sim"
)
table(dataset2$diabetes, dataset2$obesity)

#Paciente apenas com diabetes
dataset2 <- dataset %>% filter(
  dataset$asthma == "Nao" & dataset$cardiopathy == "Nao" &
    dataset$obesity == "Sim" & dataset$diabetes == "Nao"
)
table(dataset2$obesity)
prop.table(table(dataset2$obesity, dataset2$censure))
prop.table(table(dataset2$obesity, dataset2$icu))


#Tabelas cruzadas
CrossTable(schooling, censure, digits = 3)
CrossTable(sex, censure, digits = 3)
CrossTable(ethnicities, censure, digits = 3)
CrossTable(age_group, censure, digits = 3)
CrossTable(icu, censure, digits = 3)
CrossTable(asthma, censure, digits = 3)
CrossTable(cardiopathy, censure, digits = 3)
CrossTable(diabetes, censure, digits = 3)
CrossTable(obesity, censure, digits = 3)
CrossTable(age_group, asthma, digits = 3)
CrossTable(age_group, cardiopathy, digits = 3)
CrossTable(age_group, diabetes, digits = 3)
CrossTable(age_group, obesity, digits = 3)


#Gráfico
ggplot(data = dataset, aes(x = censure)) +
  geom_bar(stat='count', position = position_dodge()) +
  facet_grid(icu ~ sex + age_group) 


ggplot(data = dataset, aes(x = censure)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)),stat='count',
           position = position_dodge()) +
  facet_grid(icu ~ sex + age_group) +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.2) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
        ) + labs(title="", x = "", y = "")


tabela_1 <- table(icu, censure)
row.names(tabela_1) <- c("NAO UTI", "UTI")
tabela_1

p1 <- mosaicplot(tabela_1, col = c("aquamarine", "lightblue"), cex = 1.1,
           main = "Pacientes Internados" )

Q2 <- chisq.test(tabela_1)
Q2

# Sexo X Obito
tabela_2 <- table(sex, censure)
row.names(tabela_2) <- c("Masculino", "Feminino")
tabela_2

p2 <- mosaicplot(tabela_2, col = c("lightblue", "red"), cex = 1.1,
           main = "Sexo dos Pacientes Internados" )

Q2 <- chisq.test(tabela_2)
Q2

# Obesidade X Obito
tabela_3 <- table(obesity, censure)
row.names(tabela_3) <- c("Possui_Obesidade", "Nao_Possui_Obesidade")
tabela_3

p3 <- mosaicplot(tabela_3, col = c("#836FFF", "#483D8B"), cex = 1.1,
           main = "Pacientes com Obesidade" )

Q2 <- chisq.test(tabela_3)
Q2


# Diabetes x Obito
tabela_4 <- table(diabetes, censure)
row.names(tabela_4) <- c("Possui_Diabetes", "Nao_Possui_Diabetes")
tabela_4

barplot(tabela_4,
        main = "Pacientes com Diabetes",
        ylim = c(0, 2500),
        col = c("#87CEFA", "#4682B4")
)

Q2 <- chisq.test(tabela_4)
Q2

# Cardiopatia X Obito
tabela_5 <- table(cardiopathy, censure)
row.names(tabela_5) <- c("Possui_Cardiopatia", "Nao_Possui_Cardiopatia")
tabela_5
barplot(tabela_5,
        main = "Pacientes com Cardiopatia",
        ylim = c(0, 2300),
        col = c("#7FFFD4", "#5F9EA0"))

Q2 <- chisq.test(tabela_5)
Q2

# Asma X Obito
tabela_6 <- table(asthma, censure)
row.names(tabela_6) <- c("Possui_Asma", "Nao_Possui_Asma")
tabela_6

barplot(tabela_6,
        beside = T,
        main = "Pacientes com Asma",
        ylim = c(0, 2200),
        col = c("#F4A460", "#FFDEAD"))

Q2 <- chisq.test(tabela_6)
Q2

#Idade X Obito
tabela_7 <- table(age_group, censure)
tabela_7

barplot(tabela_7,
        beside = TRUE,
        legend = c("Jovem", "Adulto", "Idoso"),
        args.legend = list(x = "top", bty = "n", ncol = 3),
        col = c("#1E90FF", "#00008B", "#191970"),
        ylim = c(0, 1550),
        main = "Pacientes por Faixa EtÃ¡ria")

Q2 <- chisq.test(tabela_7)
Q2


# Escolaridade X Obito
tabela_8 <- table(schooling, censure)
tabela_8

barplot(tabela_8,
        beside = TRUE,
        legend = c("Sem escolaridade", "Fundamental 1", "Fundamental 2 ", "MÃ©dio", "Superior"),
        args.legend = list(x = "top", bty = "n", ncol = 5),
        col = c("#FA8072","#E9967A","#FFA07A","#FF7F50","#FF6347"),
        ylim = c(0, 900),
        main = "Pacientes por Escolaridade")

Q2 <- chisq.test(tabela_8)
Q2


# Raca X Obito


tabela_9 <- table(ethnicities, censure)
tabela_9

barplot(tabela_9,
        beside = TRUE,
        legend = c("Branca", "Preta", "Amarela", "Parda", "Indigena"),
        args.legend = list(x = "top", bty = "n", ncol = 5),
        col = c("#FFFF00", "#FFD700", "#FFA500", "#FF8C00", "#FF4500"),
        ylim = c(0, 1700),
        main = "Pacientes por RaÃ§a")

Q2 <- fisher.test(tabela_9, alternative = "greater", conf.int = TRUE, conf.level
                  = 0.95)
Q2


#Analise de SobrevivÃªncia

# GrÃ¡ficos de SobrevivÃªncia de Kaplan-Meier, grÃ¡fico de taxa (funÃ§Ã£o) falha acumulada
# e teste logRANK.

attach(dataset)

km <- dataset %>% 
  with(survfit(Surv(time_hospitalization,censure) ~ 1))

ggsurvplot(km, risk.table = F,
           legend = "bottom",
           pval = TRUE,
           ggtheme = theme_bw(),
           data = dataset)
## sexo
km_0 <- dataset %>% 
  with(survfit(Surv(time_symptoms,censure) ~ sex))

ggsurvplot(km_0, risk.table = F,
           size = 1,
           legend = "bottom",
           legend.title = "Sexo",
           pval = FALSE,
           ggtheme = theme_bw(),
           legend.labs = c("Masculino", "Feminino"),
           data = dataset,
           xlab = 'Tempo de Sobrevivência em Dias',
           ylab = 'Probabilidade de Sobrevivência')

ggsurvplot(km_0, conf.int = F,
           size = 1,
           surv.median.line = "hv",
           risk.table = F, risk.table.col = "strata",
           legend = "bottom",
           legend.title = "Sexo",
           ggtheme = theme_bw(),
           legend.labs = c("Masculino", "Feminino"),
           fun = "event", data = dataset,
           xlab = 'Tempo de Sobrevivência em Dias',
           ylab = 'Probabilidade Acumulada de Sobrevivência')


## UTI
km_1 <- dataset %>% 
  with(survfit(Surv(time_symptoms,censure) ~ icu))

ggsurvplot(km_1, risk.table = F,
           size = 1,
           palette = c("#FF9E29", "#86AA00"),
           legend = "bottom",
           legend.title = "UTI",
           legend.labs = c("Não", "Sim"),
           pval = F,
           ggtheme = theme_bw(),
           data = dataset,
           xlab = 'Tempo de Sobrevivência em Dias',
           ylab = 'Probabilidade de Sobrevivência')

ggsurvplot(km_1, conf.int = F,
           size = 1,
           surv.median.line = "hv",
           palette = c("#FF9E29", "#86AA00"),
           risk.table = F, risk.table.col = "strata",
           legend = "bottom",
           legend.title = "UTI",
           ggtheme = theme_bw(),
           legend.labs = c("Não", "Sim"),
           fun = "event", data = dataset,
           xlab = 'Tempo de Sobrevivência em Dias',
           ylab = 'Probabilidade Acumulada de Sobrevivência')

## ASMA
km_2 <- dataset %>% 
  with(survfit(Surv(time_symptoms,censure) ~ asthma))

ggsurvplot(km_2, risk.table = F,
           size = 1,
           palette = c("#11F872", "#1DF0EC"),
           legend = "bottom",
           legend.title = "Asma",
           legend.labs = c("Não", "Sim"),
           pval = F,
           ggtheme = theme_bw(),
           data = dataset,
           xlab = 'Tempo de Sobrevivência em Dias',
           ylab = 'Probabilidade de Sobrevivência')

ggsurvplot(km_2, conf.int = F,
           size = 1,
           surv.median.line = "hv",
           palette = c("#11F872", "#1DF0EC"),
           risk.table = F, risk.table.col = "strata",
           legend = "bottom",
           legend.title = "Asma",
           legend.labs = c("Não", "Sim"),
           ggtheme = theme_bw(),
           fun = "event", data = dataset,
           xlab = 'Tempo de Sobrevivência em Dias',
           ylab = 'Probabilidade Acumulada de Sobrevivência')



##CARDIOPATIA
km_3 <- dataset %>% 
  with(survfit(Surv(time_symptoms,censure) ~ cardiopathy))

ggsurvplot(km_3, risk.table = F,
           size = 1,
           palette = c("#0FD99C", "#0FA7D9"),
           legend = "bottom",
           legend.title = "Cardiopatia",
           legend.labs = c("Não", "Sim"),
           pval = F,
           ggtheme = theme_bw(),
           data = dataset,
           xlab = 'Tempo de Sobrevivência em Dias',
           ylab = 'Probabilidade de Sobrevivência')

ggsurvplot(km_3, conf.int = F, 
           size = 1,
           surv.median.line = "hv",
           palette = c("#0FD99C", "#0FA7D9"),
           risk.table = F, risk.table.col = "strata",
           legend = "bottom",
           legend.title = "Cardiopatia",
           legend.labs = c("Não", "Sim"),
           ggtheme = theme_bw(),
           fun = "event", data = dataset,
           xlab = 'Tempo de Sobrevivência em Dias',
           ylab = 'Probabilidade Acumulada de Sobrevivência')


#DIABETES
km_4 <- dataset %>% 
  with(survfit(Surv(time_symptoms, censure) ~  diabetes))

ggsurvplot(km_4, risk.table = F,
           size = 1,
           palette = c("#FAC0EE", "#6125D9"),
           legend = "bottom",
           legend.title = "Diabetes",
           legend.labs = c("Não", "Sim"),
           pval = F,
           ggtheme = theme_bw(),
           data = dataset,
           xlab = 'Tempo de Sobrevivência em Dias',
           ylab = 'Probabilidade de Sobrevivência')

ggsurvplot(km_4, conf.int = F,
           size = 1,
           surv.median.line = "hv",
           palette = c("#FAC0EE", "#6125D9"),
           risk.table = F, risk.table.col = "strata",
           legend = "bottom",
           legend.title = "Diabetes",
           legend.labs = c("Não", "Sim"),
           ggtheme = theme_bw(),
           fun = "event", data = dataset,
           xlab = 'Tempo de Sobrevivência em Dias',
           ylab = 'Probabilidade Acumulada de Sobrevivência')


## ESCOLARIDADE
km_5 <- dataset %>% 
  with(survfit(Surv(time_symptoms,censure) ~  schooling))

ggsurvplot(km_5, risk.table = F,
           size = 1,
           palette = c("#F0E207", "#43BAF0", "#7289F0", "#F034C2", "#F08D18"),
           legend = "bottom",
           legend.title = "Escolaridade",
           legend.labs = c("Sem Escolaridade", "Fundamental 1", "Fundamental 2", "Médio", "Superior"),
           pval = F,
           ggtheme = theme_bw(),
           data = dataset,
           xlab = 'Tempo de Sobrevivência em Dias',
           ylab = 'Probabilidade de Sobrevivência')

ggsurvplot(km_5, conf.int = F,
           size = 1,
           surv.median.line = "hv",
           palette = c("#F0E207", "#43BAF0", "#7289F0", "#F034C2", "#F08D18"),
           risk.table = F, risk.table.col = "strata",
           legend = "bottom",
           legend.title = "Escolaridade",
           legend.labs = c("Sem Escolaridade", "Fundamental 1", "Fundamental 2", "Médio", "Superior"),
           ggtheme = theme_bw(),
           fun = "event", data = dataset,
           xlab = 'Tempo de Sobrevivência em Dias',
           ylab = 'Probabilidade Acumulada de Sobrevivência')


#Obesidade
km_6 <- dataset %>% 
  with(survfit(Surv(time_symptoms, censure) ~  obesity))

ggsurvplot(km_6, risk.table = F,
           size = 1,
           palette = c("#F0E257", "#43BAF8"),
           legend = "bottom",
           legend.title = "Obesidade",
           legend.labs = c("Não", "Sim"),
           pval = F,
           ggtheme = theme_bw(),
           data = dataset,
           xlab = 'Tempo de Sobrevivência em Dias',
           ylab = 'Probabilidade de Sobrevivência')

ggsurvplot(km_6, conf.int = F,
           size = 1,
           surv.median.line = "hv",
           palette = c("#F0E257","#43BAF8"),
           risk.table = F, risk.table.col = "strata",
           legend = "bottom",
           legend.title = "Obesidade",
           legend.labs = c("Não", "Sim"),
           ggtheme = theme_bw(),
           fun = "event", data = dataset,
           xlab = 'Tempo de Sobrevivência em Dias',
           ylab = 'Probabilidade Acumulada de Sobrevivência')


#Faixa Etária

km_7 <- dataset %>% 
  with(survfit(Surv(time_symptoms, censure) ~  age_group))

ggsurvplot(km_7, risk.table = F,
           size = 1,
           palette = c("#F0E257", "#43BAF8", "#FFBAF8"),
           legend = "bottom",
           legend.title = "Faixa Etaria",
           legend.labs = c("Jovem", "Adulto", 'Idoso'),
           pval = F,
           ggtheme = theme_bw(),
           data = dataset,
           xlab = 'Tempo de Sobrevivência em Dias',
           ylab = 'Probabilidade de Sobrevivência')

ggsurvplot(km_7, conf.int = F,
           size = 1,
           surv.median.line = "hv",
           palette = c("#F0E257","#43BAF8", "#FFBAF8"),
           risk.table = F, risk.table.col = "strata",
           legend = "bottom",
           legend.title = "Faixa Etaria",
           legend.labs = c("Jovem", "Adulto", 'Idoso'),
           ggtheme = theme_bw(),
           fun = "event", data = dataset,
           xlab = 'Tempo de Sobrevivência em Dias',
           ylab = 'Probabilidade Acumulada de Sobrevivência')




#sexo
survdiff(Surv(time_symptoms,censure) ~ sex, rho=0)
coxph(Surv(time_symptoms, censure) ~ sex, data = dataset)

#uti
survdiff(Surv(time_symptoms,censure) ~ icu, rho=0)
coxph(Surv(time_symptoms, censure) ~ icu, data = dataset)

#asma
survdiff(Surv(time_symptoms,censure) ~ asthma, rho=0)
coxph(Surv(time_symptoms, censure) ~ asthma, data = dataset)

#cardiopatia
survdiff(Surv(time_symptoms,censure) ~ cardiopathy, rho=0)
coxph(Surv(time_symptoms, censure) ~ cardiopathy, data = dataset)

#diabetes
survdiff(Surv(time_symptoms, censure) ~  diabetes, rho=0)
coxph(Surv(time_symptoms, censure) ~ cardiopathy, data = dataset)

#obesidade
survdiff(Surv(time_symptoms, censure) ~  obesity, rho=0)
coxph(Surv(time_symptoms, censure) ~ obesity, data = dataset)

#faixa etaria
survdiff(Surv(time_symptoms, censure) ~  age_group, rho=0)
coxph(Surv(time_symptoms, censure) ~ age_group, data = dataset)

#escolaridade
survdiff(Surv(time_symptoms,censure) ~ schooling, rho=0)
coxph(Surv(time_symptoms, censure) ~ schooling, data = dataset)




## Residuos de Schoenfeld
cox_covid <- coxph(Surv(time_symptoms,censure) ~ icu + 
                     sex + asthma + cardiopathy+ diabetes+ 
                     + obesity + age_group + schooling)
summary(cox_covid)
cox_covid %>% gtsummary::tbl_regression(exp = TRUE)
par(mfrow = c(3,3))
plot(cox.zph(cox_covid))




##-------------------------------------------------------------------------##
ebola <- read.csv(url("https://whitlockschluter3e.zoology.ubc.ca/Data/chapter21/chap21e1EbolaMalaria.csv"))
head(ebola)
prop <- table(ebola$outcome)
prop.table(prop)

kmm <- ebola %>% with(survfit(Surv(time, outcome) ~  1))
kmm

ggsurvplot(kmm, risk.table = F,
           data = ebola)
mle2.GTDL(t=ebola$time,start=c(1,-0.2,1), formula=~1,censur = ebola$outcome)
#-----GTDL--------------------------------------------------------------------#