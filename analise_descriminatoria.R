library(readr)
library(readxl)
library(dplyr)
library(vcd)
library(tidyverse)
library(rcompanion)
library(lubridate)
library(stringr)
library(ggplot2)

COVID <- read_delim("covid_19/data/painel_covid19_erj_ses/COVID30052020.csv", 
                        ";", escape_double = FALSE, col_types = cols(dias = col_integer(), 
                        dt_coleta_dt_notif = col_date(format = "%d/%m/%Y"), 
                        dt_obito = col_date(format = "%d/%m/%Y"), dt_sintoma = col_date(format = "%d/%m/%Y"), 
                        idade = col_integer(), sexo = col_factor(levels = c("F", "M"))), trim_ws = TRUE)
SIVEP <- read_delim("covid_19/data/painel_covid19_erj_ses/SIVEP30052020.csv", 
                            ";", escape_double = FALSE, col_types = cols(CS_SEXO = col_factor(levels = c("M", 
                                                                                                         "F")), DT_NASC = col_date(format = "%d/%m/%Y"), 
                                                                         NU_IDADE_N = col_integer()), trim_ws = TRUE)
mortes_por_doencas_respiratorias_em_2019 <- read_delim("covid_19/data/painel_covid19_erj_ses/mortes_por_doencas_respiratorias_em_2019.csv", 
                                                       ";", escape_double = FALSE, col_types = cols(`ano (uid)` = col_integer(), 
                                                                                                    `categoria (uid)` = col_integer()), 
                                                       trim_ws = TRUE)

#https://www.ibge.gov.br/explica/codigos-dos-municipios.php#:~:text=C%C3%B3digos%20dos%20munic%C3%ADpios%20IBGE,c%C3%B3digo%20da%20Unidade%20da%20Federa%C3%A7%C3%A3o.
municipios <- read_excel("covid_19/data/painel_covid19_erj_ses/RELATORIO_DTB_BRASIL_MUNICIPIO.xls")
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
View(casos_covid_em_sg)
casos_covid_em_sg_mensal <- casos_covid_em_sg %>% filter(!is.na(dt_sintoma)) %>%
  mutate(competencia = format(dt_sintoma, "1/%m/%Y")) %>%
  group_by(competencia) %>%
  summarise(casos = n())

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
month(today())
numero_
View(mortes_por_doencas_respiratorias_em_2019)
frequencia_absoluta_por_sexo <- table(COVID$sexo)
frequencia_relativa_por_sexo <- prop.table(frequencia_absoluta_por_sexo)
frequencia_relativa_por_sexo <- as.data.frame(frequencia_relativa_por_sexo)
frequencia_absoluta_por_idade <- table(COVID$idade)
frequencia_relativa_por_idade <- prop.table(frequencia_absoluta_por_idade)
frequencia_relativa_por_idade <- as.data.frame(frequencia_relativa_por_idade)
plot(frequencia_relativa_por_sexo)
plot(frequencia_relativa_por_idade)

hist(covid_com_idades$idade)
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
