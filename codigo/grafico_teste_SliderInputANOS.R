library(shiny)
library(scales)

# Função para gerar dados de tamanhos de tubarões
gerar_dados_tubaroes <- function(media, desvio_padrao, n_amostras) {
  tamanhos <- rnorm(n_amostras, mean = media, sd = desvio_padrao)
  tamanhos <- pmax(tamanhos, 1)  # Garante que não haja tamanhos abaixo de 1
  tamanhos <- pmin(tamanhos, 300)  # Garante que não haja tamanhos acima de 300
  data.frame(Tamanho = tamanhos)
}

ui <- fluidPage(
  sliderInput("intervalo_anos", "Intervalo de Anos", min = 2018, max = 2023,
              value = c(2018, 2023), step = 1),
  verbatimTextOutput("anos_selecionados"),
  plotOutput("grafico")
)

server <- function(input, output, session) {
  
  # Gera dados de tamanhos de tubarões com base nos parâmetros escolhidos
  dados_tubaroes <- reactive({
    intervalo <- input$intervalo_anos[1]:input$intervalo_anos[2]
    gerar_dados_tubaroes(150, 10, 1000)  # Média de 150, desvio padrão de 30
  })
  
  # Atualiza o texto de saída com os anos selecionados
  output$anos_selecionados <- renderText({
    intervalo <- input$intervalo_anos[1]:input$intervalo_anos[2]
    paste("Intervalo de anos selecionados: ", paste(intervalo, collapse = "-"))
  })
  
  output$grafico <- renderPlot({
    hist_rel <- hist(dados_tubaroes()$Tamanho, plot = FALSE)
    frequencia_relativa <- hist_rel$counts / sum(hist_rel$counts) * 100
    barplot(frequencia_relativa, names.arg = hist_rel$mids,
            main = "Histograma de Frequência Relativa",
            xlab = "Tamanho (cm)", ylab = "Frequência Relativa (%)",
            col = "lightblue", border = "black")
  })
}

shinyApp(ui, server)
