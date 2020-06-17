
function(input, output) {
  
  # You can access the value of the widget with input$select, e.g.
  output$value <- renderPrint({ input$pais })
  output$casos_de_corona_por_paises <- renderPlot({
    plotCasosDeCoronaPorPais(covid_19, input$pais)
  })
  
  output$casos_de_corona_por_1000000_habitantes <- renderPlot({
    plotCasosDeCoronaPorPais_a_cada_1000m_habitantes(covid_19, input$pais)
  })
  
  output$mortes_por_corona_por_paises <- renderPlot({
    plotMortesCoronaPorPais(covid_19, input$pais)
  })
  
  output$info <- renderText({
      paste0("Numero de Caosos=", input$plotHover$x , "\nNumero de mortes=", input$plotHover$y)
  })
  
  output$acumulativo_de_casos_confirmados <- renderPlot({
    plotAcumuloDeCasosConfirmados(covid_19)
  })
  
  output$casos_novos_por_obitos_nivel_nacional <- renderPlot({
    plotNovosCasosVerusNovosObitos(covid_19_nacional)
  })
  
  output$histograma_dos_casos_nivel_nacional <- renderPlot({
    plotHistogramaDosCasosNivelNacional(covid_19_nacional)
  })
  
}

