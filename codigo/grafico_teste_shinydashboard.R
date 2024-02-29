library(shiny)
library(scales)
library(ggplot2)
library(shinydashboard)
library(plotly)

# Função para gerar dados de tamanhos de tubarões
gerar_dados_tubaroes <- function(media, desvio_padrao, n_amostras) {
  tamanhos <- rnorm(n_amostras, mean = media, sd = desvio_padrao)
  tamanhos <- pmax(tamanhos, 1)  # Garante que não haja tamanhos abaixo de 1
  tamanhos <- pmin(tamanhos, 300)  # Garante que não haja tamanhos acima de 300
  sexo <- sample(c("M", "F"), size = n_amostras, replace = TRUE)
  data.frame(Tamanho = tamanhos, Sexo = sexo)
}

ui <- dashboardPage(
  dashboardHeader(title = "Dashboard de Tubarões"),
  dashboardSidebar(
    sliderInput("intervalo_anos", "Intervalo de Anos", min = 2018, max = 2023,
                value = c(2018, 2023), step = 1),
    radioButtons("sexo_escolhido", "Escolha o Sexo:",
                 choices = c("Todos", "Macho", "Fêmea"),
                 selected = "Todos")
  ),
  dashboardBody(
    fluidRow(
      box(
        plotOutput("grafico1"),
        title = "Distribuição de Tamanhos de Tubarões",
        status = "primary",
        solidHeader = TRUE
      ),
      box(
        plotlyOutput("grafico2"),
        title = "Distribuição de Tamanhos de Tubarões por Sexo",
        status = "primary",
        solidHeader = TRUE
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Gera dados de tamanhos de tubarões com base nos parâmetros escolhidos
  dados_tubaroes <- reactive({
    intervalo <- input$intervalo_anos[1]:input$intervalo_anos[2]
    gerar_dados_tubaroes(150, 10, 1000)  # Média de 150, desvio padrão de 30
  })
  
  output$grafico1 <- renderPlot({
    dados <- dados_tubaroes()
    
    # Filtra os dados com base na escolha do sexo
    if (input$sexo_escolhido == "Macho") {
      dados <- subset(dados, Sexo == "M")
    } else if (input$sexo_escolhido == "Fêmea") {
      dados <- subset(dados, Sexo == "F")
    }
    
    # Cria o gráfico ggplot
    ggplot(dados, aes(x = Tamanho)) +
      geom_histogram(binwidth = 10, fill = "blue", color = "black") +
      labs(title = "Distribuição de Tamanhos de Tubarões",
           x = "Tamanho (cm)",
           y = "Frequência (%)") +
      scale_y_continuous(labels = scales::label_percent(scale = 0.1))
  })
  
  output$grafico2 <- renderPlotly({
    dados <- dados_tubaroes()
    
    plot_ly(dados, labels = ~Sexo, type = 'pie',
            marker = list(colors = c('blue', 'pink')),
            hole = 0.4) %>%
      layout(title = "Distribuição de Tamanhos de Tubarões por Sexo")
  })
}

shinyApp(ui, server)
