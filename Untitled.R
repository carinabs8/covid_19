library(readxl)
library(dplyr)
covid19_maio<- read_excel("~/Downloads/HIST_PAINEL_COVIDBR_20mai2020.xlsx", 
                          col_types = c("text", "text", "text", 
                                        "numeric", "numeric", "numeric", 
                                        "text", "text", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric"))
covid19_maio_ <- covid19_maio %>%  filter(!is.na(estado) & !is.na(municipio))
covid19_maio_ <- covid19_maio_ %>% group_by(estado, municipio) %>% 
  mutate(diff = coalesce(obitosAcumulado - lag(obitosAcumulado), 0))

prop.table(maximo_de_mortos_por_cidade$`max(populacaoTCU2019)`)
maximo_de_mortos_por_cidade <- covid19_maio_ %>% group_by(estado, municipio) %>% 
  summarise(obitosAcumulado = max(obitosAcumulado), 
            populacaoTCU2019 = max(populacaoTCU2019))

shapiro.test(maximo_de_mortos_por_cidade$obitosAcumulado)
niteroi <- maximo_de_mortos_por_cidade %>% filter(municipio == "Niterói")
sao_goncalo <- maximo_de_mortos_por_cidade %>% filter(municipio == "São Gonçalo")
t.test(c(niteroi$obitosAcumulado, sao_goncalo$obitosAcumulado),
           c(niteroi$populacaoTCU2019, sao_goncalo$populacaoTCU2019), alternative = "two.side")
# Boxplot basic
help("t.test")
maximo_de_mortos_por_cidade %>% filter(estado == "RJ") %>%
  ggplot( aes(x=municipio, y=obitosAcumulado, fill=municipio)) +
  geom_boxplot() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Basic boxplot") +
  xlab("")

barplot(maximo_de_mortos_por_cidade$obitosAcumulado, maximo_de_mortos_por_cidade$populacaoTCU2019)
View(prop.table(table(maximo_de_mortos_por_cidade$`max(obitosAcumulado)`)))
tapply(maximo_de_mortos_por_cidade$municipio , maximo_de_mortos_por_cidade$`max(obitosAcumulado)`, summary)
help("coalesce")
help("tapply")
covid19_maio_
covid19_maio_ %>% group_by(estado, municipio, data, obitosAcumulado) %>% 
  arrange(desc(data))
coalesce(covid19_maio_$obitosAcumulado - lag(covid19_maio_$obitosAcumulado))
