library(readr)
library(readxl)
library(dplyr)
library(vcd)
library(tidyverse)
library(rcompanion)
library(lubridate)
library(stringr)
library(ggplot2)

#http://painel.saude.rj.gov.br/monitoramento/covid19.html
COVID <- read_delim("data/painel_covid19_erj_ses/COVID15072020.csv", ";", escape_double = FALSE,
                    col_types = cols(dt_coleta_dt_notif = col_datetime(format = "%Y/%m/%d %H:%M:%S"), 
                    dt_evento = col_datetime(format = "%Y/%m/%d %H:%M:%S"), 
                    dt_sintoma = col_datetime(format = "%Y/%m/%d %H:%M:%S"),
                    dt_obito = col_datetime(format = "%Y/%m/%d %H:%M:%S"),
                    sexo = col_factor(levels = c("M","F"))), trim_ws = TRUE)

municipios <- read_excel("./data/municipios.xls", sheet = "Municípios")
COVID <- COVID %>% mutate(municipio_nome = toupper(stringi::stri_trans_general(municipio_res,"Latin-ASCII")))
municipios <- municipios %>% mutate( municipio_nome = toupper(stringi::stri_trans_general(`NOME DO MUNICÍPIO`, "Latin-ASCII")))%>%
  rename(populacao_estimada = `POPULAÇÃO ESTIMADA`) %>% filter(municipios$UF == "RJ")
COVID <- COVID %>% right_join(municipios, by= "municipio_nome") %>% mutate(com_obito = !is.na(dt_obito))

SIVEP <- read_delim("./data/painel_covid19_erj_ses/SIVEP28062020.csv", 
                            ";", escape_double = FALSE, col_types = cols(CS_SEXO = col_factor(levels = c("M", 
                                                                                                         "F")), DT_NASC = col_date(format = "%d/%m/%Y"), 
                                                                         NU_IDADE_N = col_integer()), trim_ws = TRUE)
mortes_por_doencas_respiratorias_em_2019 <- read_delim("./data/painel_covid19_erj_ses/mortes_por_doencas_respiratorias_em_2019.csv", 
  ";", escape_double = FALSE, col_types = cols(`ano (uid)` = col_integer(), `categoria (uid)` = col_integer()), trim_ws = TRUE)

#https://www.ibge.gov.br/explica/codigos-dos-municipios.php#:~:text=C%C3%B3digos%20dos%20munic%C3%ADpios%20IBGE,c%C3%B3digo%20da%20Unidade%20da%20Federa%C3%A7%C3%A3o.
municipios <- read_excel("./data/painel_covid19_erj_ses/RELATORIO_DTB_BRASIL_MUNICIPIO.xls")
municipios <- municipios %>% mutate(codigo_completo_municipio = as.integer(str_extract(`Código Município Completo`, "\\d{6}")))

mortes_por_doencas_respiratorias_em_2019 <- mortes_por_doencas_respiratorias_em_2019 %>%
  mutate(localidade_uid = `localidade (uid)`) %>% select(-c(`localidade (uid)`))
mortes_por_doencas_respiratorias_em_2019 <- mortes_por_doencas_respiratorias_em_2019 %>%
  gather(mes, casos, 24:35) %>% mutate(competencia = as.Date(paste("01", mes, `ano (uid)`, sep = "/"), "%d/%b/%Y"))


mortes_por_doencas_respiratorias_em_2019_ <- 
  left_join(mortes_por_doencas_respiratorias_em_2019, municipios, 
  by=c('localidade_uid' = 'codigo_completo_municipio'))

mortes_por_doencas_respiratorias_em_2019 <- rename(mortes_por_doencas_respiratorias_em_2019, localidade_nome = )

sao_goncalo <- municipios %>% filter(Nome_Município == "São Gonçalo")

mortes_por_doencas_respiratorias_em_2019 %>%
  filter(localidade_uid %in% sao_goncalo$codigo_completo_municipio) %>%
  summarise(sum(casos))

doencas_respiratorias <- mortes_por_doencas_respiratorias_em_2019 %>%
  filter(localidade_uid %in% sao_goncalo$codigo_completo_municipio) %>%
  filter(format(competencia, "%m") <= format(Sys.Date(), "%m"))

casos_covid_em_sg <- COVID%>% filter(str_detect(municipio_res, regex(stringi::stri_trans_general(sao_goncalo$Nome_Município[1:1],"Latin-ASCII"), ignore_case = TRUE)))



casos_covid_em_sg_mensal <- casos_covid_em_sg %>% filter(!is.na(dt_sintoma)) %>%
  mutate(competencia = format(dt_sintoma, "1/%m/%Y")) %>%
  group_by(competencia) %>%
  summarise(casos = n())

shapiro.test(casos_covid_em_sg_mensal$casos)

plot_res_1 <- ggplot(doencas_respiratorias, aes(as.Date(competencia, "%d/%m/%Y"), casos)) +
  geom_line(na.rm=TRUE) + 
  (scale_x_date(date_breaks = "1 month", date_labels=format("%b/%Y"))) +
  labs(x="Competência", y= "Número de casos", title = "Casos de doenças respiratorias em São Gonçalo")

plot_res_2 <- ggplot(casos_covid_em_sg_mensal, aes(as.Date(competencia, "%d/%m/%Y"), casos)) +
  geom_line(na.rm=TRUE) + 
  (scale_x_date(date_breaks = "1 month", date_labels=format("%b/%Y"))) +
  labs(x="Competência", y= "Número de casos", title = "Casos de COVID ao longo do tempo em São Gonçalo")
cowplot::plot_grid(plot_res_1, plot_res_2, ncol=2)
prop.table(casos_covid_em_sg_mensal$casos)



#qplot(x=casos_covid_em_sg_mensal$competencia, y=casos_covid_em_sg_mensal$total_de_casos,
#      xlab = "Competência", ylab = "Número de casos", main = "Casos de COVID ao longo do tempo")

help(plot)
sao_goncalo$Nome_Município[1:1]
covid_com_idades <- COVID %>% filter(!is.na(idade))
cidades = covid_com_idades %>% distinct(municipio_res)
cidade_factor = factor(cidades$municipio_res)

mortes_por_doencas_respiratorias_em_2019_rj <- mortes_por_doencas_respiratorias_em_2019 %>%
  filter(localidade_nome %in% cidades)
#http://svs.aids.gov.br/dantps/centrais-de-conteudos/paineis-de-monitoramento/mortalidade/cid10/

View(mortes_por_doencas_respiratorias_em_2019)
frequencia_absoluta_por_sexo <- table(COVID$sexo)
frequencia_relativa_por_sexo <- prop.table(frequencia_absoluta_por_sexo)
frequencia_relativa_por_sexo <- as.data.frame(frequencia_relativa_por_sexo)
frequencia_absoluta_por_idade <- table(COVID$idade)
frequencia_relativa_por_idade <- prop.table(frequencia_absoluta_por_idade)
frequencia_relativa_por_idade <- as.data.frame(frequencia_relativa_por_idade)
plot(frequencia_relativa_por_sexo)
plot(frequencia_relativa_por_idade)
cor(COVID$sexo, COVID$idade)
hist(covid_com_idades$idade)
x <- aov(idade ~ sexo, data = COVID)
summary(x)
library(sm)



sm.density.compare(covid_com_idades$idade, cidade_factor)
legend("topright", levels(cidade_factor))
help("legend")

obitos_por_covid <- COVID %>% filter(!is.na(dt_obito) )
frequencia_relativa_de_obitos_por_idade <- prop.table(table(obitos_por_covid$idade))
frequencia_relativa_de_obitos_por_idade <- as.data.frame(frequencia_relativa_de_obitos_por_idade)
plot(frequencia_relativa_de_obitos_por_idade)

#posso afirmar que morre mais gente acima dos 50?
#pelo p valor baixo, posso afirmar que morrem mais pessoas acima de 60 anos
obitos_acima_de_60_anos <- COVID %>% filter(!is.na(idade)) %>% filter(idade > 60) %>%
  filter(!is.na(dt_obito)) %>% 
  group_by(idade) %>% summarise(quantidade = n())
obitos_abaixo_ou_igual_60_anos <- COVID %>% filter(!is.na(idade)) %>% filter(idade <= 50) %>%
  filter(!is.na(dt_obito)) %>% group_by(idade) %>% summarise(quantidade = n())
t.test(x = obitos_acima_de_60_anos$quantidade, y = obitos_abaixo_ou_igual_60_anos$quantidade, alternative = "greater")
help("prop.test")

#correlacoes
#COVID %>% filter(!is.na(idade)) %>% filter(!is.na(dt_obito)
dados_com_idade <- COVID %>%  filter(!is.na(idade)) %>% 
  mutate(obito = ifelse(is.na(dt_obito), 0, 1))

help("as.factor")
hist(dados_com_idade$idade)
tab<- xtabs(~municipio_res + obito, data = dados_com_idade )
x<- chisq.test(dados_com_idade$idade, dados_com_idade$obito)
x
tab<- xtabs(~idade + obito, data = dados_com_idade )
View(tab)
summary(assocstats(tab))
assocstats(tab)
plot(dados_com_idade$idade ~ dados_com_idade$dias)
dados_com_idade[,c("idade", "dias")]
par(mfrow=c(2,2))
qqnorm(COVID$idade)
#=================

x<- COVID %>% filter(!is.na(sexo)) %>% group_by(sexo) %>% summarise(casos = n(), qtd_obitos = sum(!is.na(dt_obito)))
ggplot(x, aes(x=sexo))+
  geom_bar(aes(weight=casos, fill = sexo)) +
  labs(title = "Casos por gênero no estado do RIO DE JANEIRO", x="gênero", y="Casos")

x<- COVID %>% filter(!is.na(idade)) %>% group_by(idade) %>% summarise(casos = n(), qtd_obitos = sum(!is.na(dt_obito)))
x_ <- x %>% pivot_longer(cols = c(casos, qtd_obitos), names_to = "dados")

ggplot(x_, aes(x=idade, y=value))+
  geom_line(aes(color = dados, linetype = dados)) +
  labs(title = "Casos por idade no estado do RIO DE JANEIRO", x="Idade", y="Casos")


covid_por_sexo <- COVID %>% filter(!is.na(sexo)) %>% group_by(sexo, municipio_res) %>% 
  summarise( casos = n(), qtd_obitos = sum(!is.na(dt_obito))) %>%
  mutate(porcentagem_de_obitos = (qtd_obitos * 100/as.integer(casos)))
femininos <- covid_por_sexo%>%filter(sexo == "F")
masculinos <- covid_por_sexo%>%filter(sexo == "M")
shapiro.test(log10(masculinos$casos))
t.test(x=log10(femininos$casos), y=log10(masculinos$casos), p= 0.5, alternative = "greater", var.equal=T)

ggplot(covid_por_sexo, aes(y=casos, x=sexo)) +
geom_violin() + geom_boxplot(width=0.1)

covid_por_sexo <- covid_por_sexo %>% pivot_longer(cols = c(casos, qtd_obitos), names_to = "dados")

covid <- COVID %>% group_by(municipio_res, classificacao, populacao_estimada) %>% 
  summarise( casos = n(), qtd_obitos = sum(!is.na(dt_obito))) %>%
  mutate(porcentagem_de_casos = (casos*100/as.integer(populacao_estimada)),
         porcentagem_de_obitos = (qtd_obitos * 100/as.integer(casos)), 
         porcentagem_de_obitos_sobre_populacao = (qtd_obitos *100)/as.integer(populacao_estimada))

covid$municipio_res <- factor(covid$municipio_res, levels = covid$municipio_res[order(covid$casos)])
plot_res_1 <- ggplot(head(covid %>% arrange(desc(casos)), n=10), aes( x=casos, y=municipio_res)) +
  geom_col() +
  labs(x="Casos", y="Município")
covid$municipio_res <- factor(covid$municipio_res, levels = covid$municipio_res[order(covid$porcentagem_de_casos)])
plot_res_2 <- ggplot(head(covid %>% arrange(desc(porcentagem_de_casos)), n=10), aes( x=porcentagem_de_casos, y=municipio_res)) +
  geom_col() +
  labs(x="Porcentagem", y="Município")

cowplot::plot_grid(plot_res_1, plot_res_2, ncol=2, labels =c("casos", "porcentagem dos casos"))

#maiores taxas de obitos em porcetagem perante casos
density(COVID$com_obito)

covid$municipio_res <- factor(covid$municipio_res, levels = covid$municipio_res[order(covid$porcentagem_de_obitos)])
covid_ggplot_ <- head(covid %>%arrange(desc(porcentagem_de_obitos)), n=20)
covid_ggplot_ <- covid_ggplot_ %>% pivot_longer(cols = c("porcentagem_de_casos", "porcentagem_de_obitos"), names_to = "dados")

ggplot(covid_ggplot_, aes( x=value, y= municipio_res)) +
  scale_fill_discrete(name="Porcentagem", breaks=c("porcentagem_de_casos", "porcentagem_de_obitos"),
                      labels=c("Casos", "Óbitos")) +
  geom_col(aes(fill = dados)) + 
  theme(legend.position = "top")

covid_rj <- COVID %>% filter(dt_evento != "NULL")  %>%
  mutate(competencia = as.Date(format(as.Date(dt_evento, "%d/%m/%Y"), "01/%m/%Y"), "%d/%m/%Y")) %>%
  group_by(competencia) %>% summarise(qtd_casos = n(), qtd_obitos = sum(com_obito == TRUE) )
grafico <- ggplot(covid_rj, aes(competencia, qtd_casos)) +
  geom_line(na.rm=TRUE)
select(covid_rj, qtd_casos) %>%
  set_names(c("date", "value")) 
require(bestNormalize)
shapiro.test(covid_rj$qtd_obitos)
lam = BoxCox.lambda(covid_rj$qtd_obitos, method = "loglik")
x <- data.frame(qtd_obitos = seq(70000))
ajuste1_ <- boxcox(qtd_casos ~ qtd_obitos, data = covid_rj)
summary(ajuste1_)
int3 <- predict(ajuste1_$x)
matplot(x,int3,lty=c(1,2,2), type="l", xlab="Peso", ylab = "Milhas por Galão", main="Predição resposta individual")

help("predict")
istPred <- predict(model_fit, testData) 
covid_rj + geom_line(aes(model_fit))
covid_rj$dt_evento
format(as.Date(covid_rj$dt_evento, "%d/%m/%Y"), "01/%d/%Y")
#=========================
covid_proporcao_por_cidade <- COVID %>% filter(!is.na(sexo)) %>%
  count(municipio_res, sexo) %>% mutate(proporcao = prop.table(n))

procentagem_de_obitos_por_casos <- COVID %>% group_by(municipio_res) %>% 
  summarise( casos = n(), qtd_obitos = sum(com_obito == TRUE)) %>%
  mutate(porcentagem_de_casos = (casos*100/as.integer(casos)),
         porcentagem_de_obitos = (qtd_obitos * 100/as.integer(casos)))
#write.csv(procentagem_de_obitos_por_casos, "./data/painel_covid19_erj_ses/proporcao_de_obitos_sobre_casos.csv")
boxplot(procentagem_de_obitos_por_casos$porcentagem_de_obitos, main ="Boxplot Porcentagem de Obitos sobre casos")
shapiro.qqnorm(procentagem_de_obitos_por_casos$porcentagem_de_obitos)
shapiro.test(procentagem_de_obitos_por_casos$porcentagem_de_obitos)
boxcox(procentagem_de_obitos_por_casos ~ porcentagem_de_obitos)
help("boxcox")

covid_por_cidade <- COVID %>% filter(!is.na(sexo)) %>%
  group_by(municipio_res, sexo) %>%
  summarise(qtd_casos = n(), qtd_obitos = sum(com_obito == TRUE), qtd_casos_log = log10(n()))
require(useful)
covid <- COVID %>% filter(!is.na(idade)) %>% group_by(idade) %>% 
  summarise( casos = n(), qtd_obitos = sum(!is.na(dt_obito)))
covid_kms <- covid %>%  select(c(idade, casos, qtd_obitos))
covidBest <- FitKMeans(covid_kms, max.clusters=20, nstart=1, seed=0)
PlotHartigan(covidBest)
covid_kms_ <- kmeans(x= na.omit(covid_kms) , centers = 11, nstart = 1)
data.frame(covid_kms)
plot(covid_kms_, data = data.frame(covid_kms))
covid_kms_centroids <- as.data.frame(covid_kms_[["centers"]]) %>%
  arrange(idade, casos, qtd_obitos)

ggplot(covid_kms_centroids, aes( x=idade, y= casos, colour =  rownames(covid_kms_centroids))) +
  scale_fill_discrete(name="caoso", breaks=c("Media de idade", "media de casos"),
                      labels=c("idade", "casos")) +
  geom_col(aes(fill = rownames(covid_kms_centroids))) + 
  geom_point(aes(y=qtd_obitos))+
  theme(legend.position = "top")


ggplot(covid_kms_centroids, aes( x=idade, y= casos), colour = rownames(covid_kms_centroids)) +
  scale_fill_discrete(labels=c("idade", "casos/mortes")) +
  geom_area(mapping = aes(y = casos), fill = "gray") +
  geom_line(mapping = aes(y = qtd_obitos))+
  theme(legend.position = "top")


centroids <- kmeans(x=covid_por_cidade[,c(4,5)], centers=6, nstart = 1)


#========================
skewness(x$qtd_obitos, na.rm = TRUE)
library(moments)

help(transform)
require(MASS)
head(procentagem_de_obitos_por_casos)
boxcox(porcentagem_de_obitos, data = procentagem_de_obitos_por_casos)
boxcox(resp ~ trat, data=tr, lam=seq(-1, 1, 1/10))
set.seed(0)


