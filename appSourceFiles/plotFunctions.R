
resgatarArquivo <- function(url){
  filename = trimws(paste("./data/COVID-19-geographic-disbtribution-worldwide",
                          format(Sys.time(), "%Y-%m-%d"), ".xlsx", sep = ""))
  GET(url, authenticate(":", ":", type="ntlm"), write_disk(filename, overwrite = TRUE),
      progress())
  return(filename)
}

filename <- resgatarArquivo("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide.xlsx")

covid_19 <- read_excel(filename) %>% mutate(dia = as.Date(paste(year, month, day, sep = "-")))
populacao_mundial <- wb(country = "all", indicator = "SP.POP.TOTL", mrv = 1)
covid_19 <- covid_19 %>% inner_join(populacao_mundial, by=c("countryterritoryCode" = "iso3c")) %>% rename(populacao = value)
paises = covid_19 %>%
  group_by(countriesAndTerritories)

covid_19_nacional <- read_excel("./data/covid_19_nacional20200415.XLSX", 
                                                             col_types = c("text", "text", "text", 
                                                                                 "text", "numeric", "numeric", "text",
                                                                                 "date", "numeric", "numeric", "numeric","numeric", "numeric", "numeric"))

plotCasosDeCoronaPorPais <- function(data, paises=NULL){
  covid_19_de_maiores_casos_por_pais <- data %>%
    group_by(countriesAndTerritories, mes_ano = dateRep)  %>%
    summarise(cases = sum(abs(cases)), deaths=sum(deaths))
    
  if(is.null(paises)){
    paises <- paisesComMaioresCasos(data)$countriesAndTerritories
  }
  
  covid_19_de_maiores_casos_por_pais <- covid_19_de_maiores_casos_por_pais %>%
      filter(countriesAndTerritories %in% paises) %>% arrange(desc(abs(cases)))

  ggplot(covid_19_de_maiores_casos_por_pais,
    aes(x=mes_ano, y=cases, group = 1, tooltip=cases )) +
    geom_col() +
    geom_text(aes(angle = 4, label=ifelse(cases > median(cases), cases, "")), check_overlap = T, color="blue") +
    facet_wrap(vars(countriesAndTerritories), scales = "free") +
    labs(title = "Países com maiores casos de COVID-19", x="Data", y="Casos")
}

plotCasosDeCoronaPorPais_a_cada_1000m_habitantes <- function(data, paises=NULL){
  data <- data %>%
    group_by(countriesAndTerritories, mes_ano = format(as.Date(dateRep), "01/%m/%Y"), populacao) %>%
    summarise(cases = sum(cases), deaths = sum(deaths))
  data = data %>% mutate(cases = ((100000 * cases)/populacao), deaths = ((100000 * deaths)/populacao))

  if(is.null(paises)){
    paises <- paisesComMaioresCasos(data)$countriesAndTerritories
  }
  
  data <- data %>%
    filter(countriesAndTerritories %in% paises) %>% arrange(desc(abs(cases)))
  
  ggplot(data,
         aes(x=as.Date(format(mes_ano), "%d/%m/%Y"), y=cases, group = 1, tooltip=cases )) +
    geom_col() +
    #geom_text(aes(angle = 4, label=ifelse(cases > median(cases), cases, "")), check_overlap = T, color="blue") +
    facet_wrap(vars(countriesAndTerritories), scales = "free") +
    labs(title = "Países com maiores casos de COVID-19 por 100.000 habitantes", x="Data", y="Casos")
}

paisesComMaioresCasos <- function(data){
  data %>%
    group_by(countriesAndTerritories) %>%
    summarise(cases = sum(abs(cases)), deaths=sum(deaths)) %>%
    arrange(desc(cases)) %>%
    top_n(5, cases)
}

plotMortesCoronaPorPais <- function(dados, paises=NULL){
  dados <- dados %>%
    group_by(countriesAndTerritories, mes_ano = format(as.Date(dateRep),"01/%m/%Y"), populacao) %>%
    summarise(cases = sum(abs(cases)), deaths=sum(deaths))
  
  if(is.null(paises)){
    paises <- dados %>%
      group_by(countriesAndTerritories) %>%
      summarise(cases = sum(abs(cases)), deaths=sum(deaths)) %>%
      arrange(desc(deaths)) %>%
      top_n(5, deaths)
    paises <- paises$countriesAndTerritories
  }
  
  dados <- dados %>% mutate(
    porcentagem_de_casos_sobre_a_populacao = ((cases * 100)/populacao),
    porcentagem_de_mortes_sobre_casos = ((sum(deaths) * 100)/cases),
    porcentagem_de_mortes_sobre_a_populacao =  ((deaths * 100)/populacao)
  )
  dados <- dados %>%
    filter(countriesAndTerritories %in% paises) %>% arrange(desc(deaths))
  dados <- dados %>% pivot_longer(-c(mes_ano, countriesAndTerritories, cases, deaths, populacao), names_to= "dados")
  
  ggplot(dados, aes(x=as.Date(mes_ano, "%d/%m/%Y"),y=value))+
    geom_line(aes(color = dados, linetype = dados)) +
    geom_text(aes(label = ifelse(round(value, 2) > 0.1, paste(round(value, 2), "%"), "")),
              vjust = "inward", hjust = "inward",
              show.legend = FALSE) +
    scale_color_manual(values = rainbow(3)) + 
    facet_wrap( ~ countriesAndTerritories, scales = "free") +
    labs(title = "Países com maiores mortes de COVID-19 sobre casos e sobre a populacao ", x="Data", y="Mortes sobre casos vs sobre populacao")
}

plotAcumuloDeCasosConfirmados <- function(dados){
  paises = c((dados  %>% group_by(countriesAndTerritories) %>%
    summarise(numero_de_casos = sum(abs(cases))) %>% arrange(desc(numero_de_casos)) %>%
    top_n(5, numero_de_casos))$countriesAndTerritories, c("China", "Brazil"))
  dados <- dados  %>% group_by(countriesAndTerritories,dia) %>%
    summarise(numero_de_casos = sum(abs(cases)))%>%
    filter(countriesAndTerritories %in% unique(paises))
  ggplot(dados, aes(x=dia, y=numero_de_casos, color=countriesAndTerritories)) + 
    geom_line() +
    geom_text(aes(angle = 45, label=countriesAndTerritories),
      check_overlap = T) +
    labs(title = "Progressão dos casos por COVID-19", x="Mes", y="Numero de casos")
}

plotNovosCasosVerusNovosObitos <- function(data){
  ggplot(data, aes(casosAcumulado, obitosAcumulado, colour= regiao) ) + geom_point() +
    labs(title = "Números de casos versus número de óbitos", x="Número de Casos Novos", y="Número de obitos novos")
}

plotHistogramaDosCasosNivelNacional <- function(data){
  hist(data$casosAcumulados)
}
