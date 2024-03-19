if (!requireNamespace("plotly", quietly = TRUE)) {
  print("Pacote ainda não instalado. Irei instalar agora!")
  install.packages("plotly", dependencies = TRUE)
}

pacman::p_load(
  plotly,shiny, shinydashboard, shinydashboardPlus, shinythemes,
  readtext, dplyr, ggplot2, ggrepel, scales, webshot, ISLR
)

shinyApp(
  ui = dashboardPage(
    header = dashboardHeader(title = "Teste Plotly"),
    sidebar = dashboardSidebar(collapsed = TRUE, minified = FALSE),
    body = dashboardBody(
      fluidRow(
        column(9, uiOutput("teste_ui")),
        column(9, uiOutput("teste2ui"))
      )
    ),
    controlbar = dashboardControlbar(
      collapsed = FALSE,
      sliderInput("intervalo_anos",
                  "Intervalo de Anos:",
                  min = 70,
                  max = 82,
                  value = c(70, 82),
                  step = 1),
      radioButtons(
        "sexo_escolhido", 
        "Escolha o Sexo:",
        choices = c("Todos", "Macho", "Fêmea"),
        selected = "Todos"
      )
    )
  ),
  server = function(input, output, session) {
    Dados_auto <- reactive({
      Auto
    })
    
    output$teste_ui <- renderUI({
      
      dados <- Dados_auto()
      
      dados <- subset(
        dados,
        year >= input$intervalo_anos[1] & year <= input$intervalo_anos[2])
      
      quantidade <- dados %>%
        count(name)
      
      plot_ly(
        data = quantidade,
        x = ~name,
        y = ~n,
        type = "bar",
        hoverinfo = "text",
        text = ~paste("Carro : ", name,
                      "<br>",
                      "Quantidade: ", n)
      ) %>% 
        layout(title = "Quantidade de Carros por Modelo") %>% 
        layout(list(showlegend = FALSE))
      })
    
    output$teste2ui <- renderUI({
      
      dados <- Dados_auto()
      
      dados <- subset(
        dados,
        year >= input$intervalo_anos[1] & year <= input$intervalo_anos[2])
      
      plot_ly(
        data = dados,
        labels = ~year,
        values = ~mpg,
        type = "pie",
        hole = 0.6,
        textinfo = "none",
        hoverinfo = "text",
        text = ~paste("Carro : ", dados$name,
                      "<br>",
                      "Cavalos: ", dados$horsepower)
      ) %>%
        layout(list(showlegend = FALSE))
    })
  }
)
