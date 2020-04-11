library(shinythemes)
library(readxl)
library(httr)
require(dplyr)
library(ggplot2)

source("appSourceFiles/plotFunctions.R") # source plotting functions
filename <- resgatarArquivo("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide.xlsx")

covid_19 <- read_excel(filename) %>% mutate(dia = as.Date(paste(year, month, day, sep = "-")))
paises = covid_19 %>%
  group_by(countriesAndTerritories)

shinyUI(
  fluidPage(theme=shinytheme("spacelab"),
  navbarPage(
    title=div('Analise do avanço do COVID-19')
  ),
  fluidPage(
    fluidRow(
      column(
        2, 
        selectizeInput(
          'pais', 'Selecione o País', choices =  paises$countriesAndTerritories,
          multiple = TRUE
        ),
        #verbatimTextOutput("value")
      ),
      column(
        10,
        plotOutput("casos_de_corona_por_paises"),
        plotOutput("mortes_por_corona_por_paises", hover = "plotHover"),
        plotOutput("acumulativo_de_casos_confirmados"),
        #verbatimTextOutput("info")
      )
    )
    
  ),
  #mainPanel(
  #  plotOutput("casos_de_corona_por_paises"),
  #  plotOutput("mortes_por_corona_por_paises", hover = "plotHover"),
  #  verbatimTextOutput("info")
  #  )
  )
)
