library(shinythemes)
library(readxl)
library(httr)
require(dplyr)
library(ggplot2)
library(readr)
library(dplyr)
library(epiDisplay)

source("appSourceFiles/plotFunctions.R") # source plotting functions
source("appSourceFiles/layouts.R")

shinyUI(
  fluidPage(
    theme=shinytheme("spacelab"),
    navbarPage(
      title=div('Analise do avanço do COVID-19'),
      tabPanel("Nível Mundial", tabPanelNivelMundial()),
      tabPanel("Nível Nacional", tabPanelNivelNacional())
    ),
    
  )
)
