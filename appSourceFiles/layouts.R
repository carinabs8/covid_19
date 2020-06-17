tabPanelNivelMundial <- function (){
  filename <- resgatarArquivo("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide.xlsx")
  
  covid_19 <- read_excel(filename) %>% mutate(dia = as.Date(paste(year, month, day, sep = "-")))
  populacao_mundial <- wb(country = "all", indicator = "SP.POP.TOTL", mrv = 1)
  covid_19 <- covid_19 %>% inner_join(populacao_mundial, by=c("countryterritoryCode" = "iso3c")) %>% rename(populacao = value)
  paises <- covid_19 %>%
    group_by(countriesAndTerritories)
  
  fluidPage(
    fluidRow(
      column(
        2, 
        selectizeInput(
          'pais', 'Selecione o País', choices =  paises$countriesAndTerritories,
          multiple = TRUE
        ),
      )
    ),
    fluidRow(
      column(
        12,
        plotOutput("casos_de_corona_por_paises"),
        plotOutput("casos_de_corona_por_1000000_habitantes"),
        plotOutput("mortes_por_corona_por_paises"),
        plotOutput("acumulativo_de_casos_confirmados")
      )
    )
  )
}
tabPanelNivelNacional <- function(){
  fluidPage(
    fluidRow(
      column(
        11, plotOutput("casos_novos_por_obitos_nivel_nacional")
      )
    )
  )
}

