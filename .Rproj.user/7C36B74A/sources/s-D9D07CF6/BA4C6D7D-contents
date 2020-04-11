resgatarArquivo <- function(url){
  filename = trimws(paste("./data/COVID-19-geographic-disbtribution-worldwide",
                          format(Sys.time(), "%Y-%m-%d"), ".xlsx", sep = ""))
  GET(url, authenticate(":", ":", type="ntlm"), write_disk(filename, overwrite = TRUE), progress())
  return(filename)
}

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
         aes(x=mes_ano, y=cases, group = 1)) +
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
    filter(numero_de_casos >1000 )
  ggplot(dados, aes(x=dia, y=numero_de_casos, color=countriesAndTerritories)) + 
    geom_line() +
    geom_text(aes(angle = 45, label=countriesAndTerritories), check_overlap = T, hjust=0, nudge_x=0.5, nudge_y=0.5)
}
  
