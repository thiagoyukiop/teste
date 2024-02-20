library(shiny)
library(scales)

# Função para gerar dados de tamanhos de tubarões
gerar_dados_tubaroes <- function(media, desvio_padrao, n_amostras) {
  tamanhos <- rnorm(n_amostras, mean = media, sd = desvio_padrao)
  tamanhos <- pmax(tamanhos, 1)  # Garante que não haja tamanhos abaixo de 1
  tamanhos <- pmin(tamanhos, 300)  # Garante que não haja tamanhos acima de 300
  sexo <- sample(c("M", "F"), size = n_amostras, replace = TRUE)
  data.frame(Tamanho = tamanhos, Sexo = sexo)
}

ui <- fluidPage(
  sliderInput("intervalo_anos", "Intervalo de Anos", min = 2018, max = 2023,
              value = c(2018, 2023), step = 1),
  verbatimTextOutput("anos_selecionados"),
  radioButtons("sexo_escolhido", "Escolha o Sexo:",
               choices = c("Todos", "Macho", "Fêmea"),
               selected = "Todos"),
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
    dados <- dados_tubaroes()
    
    # Filtra os dados com base na escolha do sexo
    if (input$sexo_escolhido == "Macho") {
      dados <- subset(dados, Sexo == "M")
    } else if (input$sexo_escolhido == "Fêmea") {
      dados <- subset(dados, Sexo == "F")
    }
    
    # Cria o histograma e o gráfico de barras
    hist_rel <- hist(dados$Tamanho, plot = FALSE)
    frequencia_relativa <- hist_rel$counts / sum(hist_rel$counts) * 100
    barplot(frequencia_relativa, names.arg = hist_rel$mids,
            main = "Histograma de Frequência Relativa",
            xlab = "Tamanho (cm)", ylab = "Frequência Relativa (%)",
            col = "lightblue", border = "black")
  })
}

shinyApp(ui, server)
