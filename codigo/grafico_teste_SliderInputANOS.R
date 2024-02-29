library(shiny)
library(shinyFeedback)
library(scales)
library(ggplot2)
library(dplyr)

gerar_dados_tubaroes <- function(media, desvio_padrao, n_amostras, ano_inicial, ano_final) {
  tamanhos <- rnorm(n_amostras, mean = media, sd = desvio_padrao)
  tamanhos <- pmax(tamanhos, 1)  # Garante que não haja tamanhos abaixo de 1
  tamanhos <- pmin(tamanhos, 300)  # Garante que não haja tamanhos acima de 300
  sexo <- sample(c("M", "F"), size = n_amostras, replace = TRUE)
  anos <- sample(seq(ano_inicial, ano_final), size = n_amostras, replace = TRUE)
  data.frame(Tamanho = tamanhos, Sexo = sexo, Ano = anos)
}

ui <- fluidPage(
  fluidRow(
    column(3,
           sliderInput("intervalo_anos", "Intervalo de Anos", min = 2018, max = 2023,
                       value = c(2018, 2023), step = 1),
           #verbatimTextOutput("anos_selecionados"),
           radioButtons("sexo_escolhido", "Escolha o Sexo:",
                        choices = c("Todos", "Macho", "Fêmea"),
                        selected = "Todos")
    ),
    column(9,
           plotOutput("grafico1"),
           plotOutput("grafico2")
    )
  )
)

server <- function(input, output, session) {
  
  # Gera dados de tamanhos de tubarões com base nos parâmetros escolhidos
  dados_tubaroes <- reactive({
    #intervalo <- input$intervalo_anos[1]:input$intervalo_anos[2]
    dados_teste
    #gerar_dados_tubaroes(150, 10, 1000, 2018, 2023)  # Média de 150, desvio padrão de 30
  })
  
  # output$anos_selecionados <- renderText({
  #   intervalo <- input$intervalo_anos
  #   paste("Intervalo de anos selecionados: ", paste(intervalo, collapse = "-"))
  # })
  
  output$grafico1 <- renderPlot({
    dados <- dados_tubaroes()
    
    dados <- subset(dados, Ano >= input$intervalo_anos[1] 
                    & Ano <= input$intervalo_anos[2])
    
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
      scale_y_continuous(labels = scales::label_percent(scale = 0.01))
  })
  
  output$grafico2 <-renderPlot({
    dados <- dados_tubaroes()
    
    dados <- subset(dados, Ano >= input$intervalo_anos[1] & Ano <= input$intervalo_anos[2])
    # ggplot(dados, aes(x = "", fill = Sexo)) +
    #   geom_bar(width = 1, position = "stack") +
    #   coord_polar(theta = "y") +
    #   labs(title = "Distribuição de Tamanhos de Tubarões por Sexo",
    #        fill = "Sexo") +
    #   xlim(c(2, 4)) +
    #   theme_void()
    
    # ggplot(dados, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Sexo)) +
    #   geom_rect() +
    #   coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
    #   xlim(c(2, 4)) # Try to remove that to see how to make a pie chart
    
    # ggplot(dados, aes(x = "", fill = Sexo)) +
    #   geom_rect(aes(ymin = 0, ymax = 100, xmin = 3, xmax = 4, fill = Sexo)) +
    #   coord_polar(theta = "y") +
    #   labs(title = "Distribuição de Tamanhos de Tubarões por Sexo") +
    #   theme_void()
    # 
    # gender <- dados_teste %>% 
    #   count(Sexo)
    # 
    # gender$porc <- gender %>% 
    #   mutate(porc = n/cumsum(n))
    
    # ggplot(dados, aes())
  })
}

dados_teste <- gerar_dados_tubaroes(150, 10, 10000, 2018, 2023)

gender <- dados_teste %>% 
  count(Sexo)

shinyApp(ui, server)
