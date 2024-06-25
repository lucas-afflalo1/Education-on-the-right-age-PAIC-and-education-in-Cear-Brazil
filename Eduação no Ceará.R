#Preparo dos Dados#
library(readxl)
library(Synth)
Dados_SAEB <- read_excel("Dados SAEB.xlsm")
Codigo <- Dados_SAEB$Código
Estado <- Dados_SAEB$Estado
Ano <- Dados_SAEB$Ano
Portugues <- Dados_SAEB$Português
Matematica <- Dados_SAEB$Matemática
Populacao <- Dados_SAEB$População
PIB <- Dados_SAEB$PIB
PIB_per_capta <- Dados_SAEB$PIBpercapta
Territorio <- Dados_SAEB$Território
Dados_SAEB <- data.frame(col1=Codigo, col2=Estado, col3=Ano, col4=Portugues, col5=Matematica, col6=Populacao, col7=PIB, col8=PIB_per_capta, col9=Territorio)
colnames(Dados_SAEB) <- c("Código","Estado", "Ano", "Português", "Matemática", "População", "PIB", "PIBpercapta", "Território")
predictors <- c("População", "PIB", "PIBpercapta", "Território")
#Controle Sintético Matemática - CE#
        #Construção#
Matematica <- dataprep(Dados_SAEB, 
 predictors <- c("População", "PIB", "PIBpercapta", "Território"),
 unit.variable = "Código",
 treatment.identifier = 23,
 controls.identifier = c(12, 13, 16, 15, 11, 14, 17, 27, 29, 21, 25, 26, 22, 24, 28, 32, 31, 33, 35, 41, 43, 42, 53, 52, 50, 51),
 time.predictors.prior = c(1995,1997, 1999, 2001, 2003, 2005),
 time.optimize.ssr = c(2007,2009, 2011, 2013, 2015, 2017), 
 time.plot = c(1995,1997,1999,2001,2003,2005,2007,2009,2011,2013,2015,2017,2019),
 dependent = "Matemática",
 time.variable = "Ano")
synth_control_Matematica <- synth(Matematica, predictors)
        #Gráfico#
path.plot(synth.res = synth_control_Matematica,
          dataprep.res = Matematica,
          Ylab = c("Média do Ceará"),
          Xlab = c("Ano"),
          Ylim = c(140, 260),
          abline(v=2007),
          Legend = c("Ceará", "Ceará Sintético"))
        #Gaps#
gaps.plot(synth.res = synth_control_Matematica,
          dataprep.res = Matematica,
          Ylab = c("gap na média no SAEB/Prova Brasil"),
          Xlab = c("Ano"),
          Ylim = c(-70, 70),
          abline(v=2007))
#Controle Sintético Português - CE#
        #Construção#
Portugues <- dataprep(Dados_SAEB, 
                      predictors <- c("População", "PIB", "PIBpercapta", "Território"),
                      unit.variable = "Código",
                      treatment.identifier = 23,
                      controls.identifier = c(21, 27, 24, 29, 26, 22, 25, 28, 32, 31, 33, 35, 41, 43, 42, 53, 52, 50, 51),
                      time.predictors.prior = c(1995,1997, 1999, 2001, 2003, 2005),
                      time.optimize.ssr = c(2007,2009, 2011, 2013, 2015, 2017), 
                      time.plot = c(1995,1997,1999,2001,2003,2005,2007,2009,2011,2013,2015,2017,2019),
                      dependent = "Português",
                      time.variable = "Ano")
synth_control_portugues <- synth(Portugues, predictors)
#Gráfico#
path.plot(synth.res = synth_control_portugues,
          dataprep.res = Portugues,
          Ylab = c("Média do Ceará"),
          Xlab = c("Ano"),
          Ylim = c(140, 240),
          abline(v=2007),
          Legend = c("Ceará", "Ceará Sintético"))
#Gap#
gaps.plot(synth.res = synth_control_portugues,
          dataprep.res = Portugues,
          Ylab = c("gap na média no SAEB/Prova Brasil"),
          Xlab = c("Ano"),
          Ylim = c(-70, 70),
          abline(v=2007))
#Placebo#
        #Rio Grande do Norte#
                #Português#
                        #Construção#
Portugues_RN <- dataprep(Dados_SAEB, 
                      predictors <- c("População", "PIB", "PIBpercapta", "Território"),
                      unit.variable = "Código",
                      treatment.identifier = 24,
                      controls.identifier = c(21, 27, 23, 29, 26, 22, 25, 28, 32, 31, 33, 35, 41, 43, 42, 53, 52, 50, 51),
                      time.predictors.prior = c(1995,1997, 1999, 2001, 2003, 2005),
                      time.optimize.ssr = c(2007,2009, 2011, 2013, 2015, 2017), 
                      time.plot = c(1995,1997,1999,2001,2003,2005,2007,2009,2011,2013,2015,2017,2019),
                      dependent = "Português",
                      time.variable = "Ano")
synth_control_portugues_RN <- synth(Portugues_RN, predictors)
#Gráfico#
path.plot(synth.res = synth_control_portugues_RN,
          dataprep.res = Portugues_RN,
          Ylab = c("Média do Rio Grande do Norte"),
          Xlab = c("Ano"),
          Ylim = c(130, 230),
          abline(v=2007),
          Legend = c("Rio Grande do Norte", "Rio Grande do Norte Sintético"))
#Gap#
gaps.plot(synth.res = synth_control_portugues_RN,
          dataprep.res = Portugues,
          Ylab = c("gap na média no SAEB/Prova Brasil"),
          Xlab = c("Ano"),
          Ylim = c(-70, 70),
          abline(v=2007))
#Matemática#
#Construção#
Matematica_RN <- dataprep(Dados_SAEB, 
                         predictors <- c("População", "PIB", "PIBpercapta", "Território"),
                         unit.variable = "Código",
                         treatment.identifier = 24,
                         controls.identifier = c(21, 27, 23, 29, 26, 22, 25, 28, 32, 31, 33, 35, 41, 43, 42, 53, 52, 50, 51),
                         time.predictors.prior = c(1995,1997, 1999, 2001, 2003, 2005),
                         time.optimize.ssr = c(2007,2009, 2011, 2013, 2015, 2017), 
                         time.plot = c(1995,1997,1999,2001,2003,2005,2007,2009,2011,2013,2015,2017,2019),
                         dependent = "Matemática",
                         time.variable = "Ano")
synth_control_matematica_RN <- synth(Matematica_RN, predictors)
#Gráfico#
path.plot(synth.res = synth_control_matematica_RN,
          dataprep.res = Matematica_RN,
          Ylab = c("Média do Rio Grande do Norte"),
          Xlab = c("Ano"),
          Ylim = c(140, 240),
          abline(v=2007),
          Legend = c("Rio Grande do Norte", "Rio Grande do Norte Sintético"))
#Gap#
gaps.plot(synth.res = synth_control_matematica_RN,
          dataprep.res = Matematica_RN,
          Ylab = c("gap na média no SAEB/Prova Brasil"),
          Xlab = c("Ano"),
          Ylim = c(-70, 70),
          abline(v=2007))
#Paraíba#
#Português#
#Construção#
Portugues_PB <- dataprep(Dados_SAEB, 
                         predictors <- c("População", "PIB", "PIBpercapta", "Território"),
                         unit.variable = "Código",
                         treatment.identifier = 25,
                         controls.identifier = c(21, 27, 23, 29, 26, 22, 24, 28, 32, 31, 33, 35, 41, 43, 42, 53, 52, 50, 51),
                         time.predictors.prior = c(1995,1997, 1999, 2001, 2003, 2005),
                         time.optimize.ssr = c(2007,2009, 2011, 2013, 2015, 2017), 
                         time.plot = c(1995,1997,1999,2001,2003,2005,2007,2009,2011,2013,2015,2017,2019),
                         dependent = "Português",
                         time.variable = "Ano")
synth_control_portugues_PB <- synth(Portugues_PB, predictors)
#Gráfico#
path.plot(synth.res = synth_control_portugues_PB,
          dataprep.res = Portugues_PB,
          Ylab = c("Média da Paraíba"),
          Xlab = c("Ano"),
          Ylim = c(140, 240),
          abline(v=2007),
          Legend = c("Paraíba", "Paraíba Sintética"))
#Gap#
gaps.plot(synth.res = synth_control_portugues_PB,
          dataprep.res = Portugues_PB,
          Ylab = c("gap na média no SAEB/Prova Brasil"),
          Xlab = c("Ano"),
          Ylim = c(-70, 70),
          abline(v=2007))
#Matemática#
#Construção#
Matematica_PB <- dataprep(Dados_SAEB, 
 predictors <- c("População", "PIB", "PIBpercapta", "Território"),
 unit.variable = "Código",
 treatment.identifier = 25,
 controls.identifier = c(21, 27, 23, 29, 26, 22, 24, 28, 32, 31, 33, 35, 41, 43, 42, 53, 52, 50, 51),
 time.predictors.prior = c(1995,1997, 1999, 2001, 2003, 2005),
 time.optimize.ssr = c(2007,2009, 2011, 2013, 2015, 2017), 
 time.plot = c(1995,1997,1999,2001,2003,2005,2007,2009,2011,2013,2015,2017,2019),
 dependent = "Matemática",
 time.variable = "Ano")
synth_control_matematica_PB <- synth(Matematica_PB, predictors)
#Gráfico#
path.plot(synth.res = synth_control_matematica_PB,
          dataprep.res = Matematica_PB,
          Ylab = c("Média da Paraíba"),
          Xlab = c("Ano"),
          Ylim = c(140, 240),
          abline(v=2007),
          Legend = c("Paraíba", "Paraíba Sintética"))
#Gap#
gaps.plot(synth.res = synth_control_matematica_PB,
          dataprep.res = Matematica_PB,
          Ylab = c("gap na média no SAEB/Prova Brasil"),
          Xlab = c("Ano"),
          Ylim = c(-70, 70),
          abline(v=2007))
