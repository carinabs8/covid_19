resgatarArquivo <- function(url){
  filename = trimws(paste("./data/COVID-19-geographic-disbtribution-worldwide",
                          format(Sys.time(), "%Y-%m-%d"), ".xlsx", sep = ""))
  GET(url, authenticate(":", ":", type="ntlm"), write_disk(filename, overwrite = TRUE),
      progress())
  return(filename)
}
library(readxl)
filename <- resgatarArquivo("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide.xlsx")

covid_19 <- read_excel(filename) %>% mutate(dia = as.Date(paste(year, month, day, sep = "-")))
covid_19_nacional <- read_excel("./data/covid_19_nacional20200415.XLSX", 
                                                             col_types = c("text", "text", "text", 
                                                                                 "text", "numeric", "numeric", "text",
                                                                                 "date", "numeric", "numeric", "numeric","numeric", "numeric", "numeric"))

plotCasosDeCoronaPorPais <- function(data, paises=NULL){
  covid_19_de_maiores_casos_por_pais <- data %>%
    group_by(countriesAndTerritories, mes_ano = as.Date(format(dateRep, "%Y-%m-%d")))  %>%
    summarise(cases = sum(cases), deaths=sum(deaths))
    
    if(is.null(paises)){
      paises <- data %>%
        group_by(countriesAndTerritories) %>%
        summarise(cases = sum(cases), deaths=sum(deaths)) %>%
        arrange(desc(cases)) %>%
        top_n(10, cases)
      paises <- paises$countriesAndTerritories
    }
  
    covid_19_de_maiores_casos_por_pais <- covid_19_de_maiores_casos_por_pais %>%
      filter(countriesAndTerritories %in% paises)
  
  ggplot(arrange(covid_19_de_maiores_casos_por_pais, desc(mes_ano)),
         aes(x=mes_ano, y=cases, group = 1, tooltip=cases )) +
    geom_col() +
    facet_wrap(vars(countriesAndTerritories)) +
    labs(title = "Países com maiores casos de COVID-19", x="Data", y="Casos")
}

plotMortesCoronaPorPais <- function(dados, paises=NULL){
  dados <- dados %>%
    group_by(countriesAndTerritories, mes_ano = as.Date(format(dateRep, "%Y-%m-01"))) %>%
    summarise(cases = sum(cases), deaths=sum(deaths))
    if(is.null(paises)){
      paises <- dados %>%
        group_by(countriesAndTerritories) %>%
        summarise(cases = sum(cases), deaths=sum(deaths)) %>%
        arrange(desc(deaths)) %>%
        top_n(10, deaths)
      paises <- paises$countriesAndTerritories
    }
    
  dados <- dados %>%
    arrange(desc(deaths)) %>%
    filter(countriesAndTerritories %in% paises)

  ggplot(arrange(dados, desc(deaths)), aes(mes_ano))+
  geom_area(aes(y=cases), fill = "grey70") +
    geom_area(aes(y=deaths, alpha=0.8), fill = "red") +
    facet_wrap(vars(countriesAndTerritories)) +
    labs(title = "Países com maiores mortes de COVID-19", x="Data", y="Mortes X Casos")
}

plotAcumuloDeCasosConfirmados <- function(dados){
  dados <- dados  %>%
    group_by(countriesAndTerritories,dia) %>%
    summarise(numero_de_casos = sum(cases))%>%
    filter(numero_de_casos > 1000 )
  ggplot(dados, aes(x=dia, y=numero_de_casos, color=countriesAndTerritories)) + 
    geom_line() +
    geom_text(aes(angle = 45, label=countriesAndTerritories),
      check_overlap = T)
}

plotNovosCasosVerusNovosObitos <- function(data){
  ggplot(data, aes(casosAcumulado, obitosAcumulado, colour= regiao) ) + geom_point() +
    labs(title = "Números de casos versus número de óbitos", x="Número de Casos Novos", y="Número de obitos novos")
}

plotHistogramaDosCasosNivelNacional <- function(data){
  hist(data$casosAcumulados)
}
