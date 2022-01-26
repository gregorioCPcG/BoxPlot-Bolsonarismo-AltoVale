library(tidyverse)
library(knitr)
library(kableExtra)
library(readxl)
dados <- read_excel("dados.xlsx")
dados$Bolsonaro2018 <- dados$Bolsonaro2018_turno2*100
dados$Neves14 <- dados$Neves14*100
summary(dados)




dados$Cidade #

dados$nivel_Bolsonarismo <- ntile(dados$Bolsonaro2018_turno2, 2)
dados$nivel_Bolsonarismo <- as.factor(dados$nivel_Bolsonarismo)
levels(dados$nivel_Bolsonarismo)
levels(dados$nivel_Bolsonarismo) <- c('Alto', 'Muito Alto')

b5 <- dados %>% 
  dplyr::select(nivel_Bolsonarismo, Bolsonaro2018, Cidade) %>% 
  arrange(Bolsonaro2018)
b5 %>%
  kbl(caption = "Nível de Bolsonarismo") %>%
  kable_classic(full_width = F, html_font = "Garamond") #

median(dados$Bolsonaro2018)# marca a transição

g <- ggplot(dados, aes(Bolsonaro2018, nivel_Bolsonarismo)) + scale_fill_brewer( 
  palette = "Blues") 
g +  geom_boxplot() +  xlab("Votação em Bolsonaro no Alto Vale, 28 cidades") + coord_flip() +
ylab("Nível Apoio") + theme_minimal() + ggtitle("Box Plot Bolsonarismo Alto Vale")


# outra forma de ver isso
by(dados$Bolsonaro2018, dados$nivel_Bolsonarismo, mean)

# analises



by(dados$log_pop_18, dados$nivel_Bolsonarismo, mean)
a<- t.test(log_pop_18 ~ nivel_Bolsonarismo, data = dados)
a$p.value
g <- ggplot(dados, aes(nivel_Bolsonarismo, evangelico))
g + geom_boxplot() +  xlab("Apoio à Bolsonaro") +
  ylab("% Evangélico por cidade")

by(dados$rural_2010, dados$nivel_Bolsonarismo, mean)
a<- t.test(rural_2010 ~ nivel_Bolsonarismo, data = dados)
a$p.value
g <- ggplot(dados, aes(nivel_Bolsonarismo, rural_2010))
g + geom_boxplot() +  xlab("Apoio à Bolsonaro") +
  ylab("% rural por cidade")

by(dados$idosos, dados$nivel_Bolsonarismo, mean)
a<- t.test(idosos ~ nivel_Bolsonarismo, data = dados)
a$p.value
g <- ggplot(dados, aes(nivel_Bolsonarismo, idosos))
g + geom_boxplot() +  xlab("Apoio à Bolsonaro") +
  ylab("% idosos por cidade")

by(dados$idh_mun, dados$nivel_Bolsonarismo, mean)
a<- t.test(idh_mun ~ nivel_Bolsonarismo, data = dados)
a$p.value
g <- ggplot(dados, aes(nivel_Bolsonarismo, idh_mun))
g + geom_boxplot() +  xlab("Apoio à Bolsonaro") +
  ylab("% IDH por cidade")


garinnnnnnn <- dados[-c(21),] #removendo Rio do Sul outlier
b<- t.test(evangelico ~ nivel_Bolsonarismo, data = garinnnnnnn)
b$p.value # perde significância, reportar
rm(garinnnnnnn)

# segue...

by(dados$evangelico, dados$nivel_Bolsonarismo, mean)
a<- t.test(evangelico ~ nivel_Bolsonarismo, data = dados)
a$p.value
g <- ggplot(dados, aes(nivel_Bolsonarismo, evangelico))
g + geom_boxplot() +  xlab("Apoio à Bolsonaro") +
  ylab("% Evangélicos por cidade")

# retirar vitor meireles e josé boiteux outlier e rodar o teste p value
iz2222 <- dados[-c(11,27),] 
b<- t.test(evangelico ~ nivel_Bolsonarismo, data = iz2222)
b$p.value # segue nao sig, reportar
rm(iz2222)
# continuando....


by(dados$superior_comp, dados$nivel_Bolsonarismo, mean)
a<- t.test(superior_comp ~ nivel_Bolsonarismo, data = dados)
a$p.value
g <- ggplot(dados, aes(nivel_Bolsonarismo, superior_comp))
g + geom_boxplot() +  xlab("Apoio à Bolsonaro") +
  ylab("% Superior Completo por cidade")

by(dados$log_pop_18, dados$nivel_Bolsonarismo, mean)
a<- t.test(log_pop_18 ~ nivel_Bolsonarismo, data = dados)
a$p.value
g <- ggplot(dados, aes(nivel_Bolsonarismo, log_pop_18))
g + geom_boxplot() +  xlab("Apoio à Bolsonaro") +
  ylab("Tamanho do eleitorado")

by(dados$Neves14, dados$nivel_Bolsonarismo, mean)
a<- t.test(Neves14 ~ nivel_Bolsonarismo, data = dados)
a$p.value
g <- ggplot(dados, aes(nivel_Bolsonarismo, Neves14))
g + geom_boxplot() +  xlab("Apoio à Bolsonaro") +
  ylab("Votação antipetista em 2014 por cidade")

by(dados$Lula_2006, dados$nivel_Bolsonarismo, mean)
a<- t.test(Lula_2006 ~ nivel_Bolsonarismo, data = dados)
a$p.value
g <- ggplot(dados, aes(nivel_Bolsonarismo, Lula_2006)) + scale_fill_brewer( 
  palette = "Blues") 
g +  geom_boxplot() +  xlab("Apoio à Bolsonaro") +
  ylab("Votação Lula em 2006 por cidade") + theme_bw()
install.packages("RColorBrewer")
library(RcolorBrewer) 


