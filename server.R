
function(input, output) {
  
  # You can access the value of the widget with input$select, e.g.
  output$value <- renderPrint({ input$pais })
  output$casos_de_corona_por_paises <- renderPlot({
    plotCasosDeCoronaPorPais(covid_19, input$pais)
  })
  output$mortes_por_corona_por_paises <- renderPlot({
    plotMortesCoronaPorPais(covid_19, input$pais)
  })
  
  output$info <- renderText({
      paste0("Numero de Caosos=", input$plotHover$x , "\nNumero de mortes=", input$plotHover$y)
      #paste0("Casos:", round(e$x, 1), " Mortes=", round(e$y, 1), "\n")
  })
  
  output$acumulativo_de_casos_confirmados <- renderPlot({
    plotAcumuloDeCasosConfirmados(covid_19)
  })
  
}
