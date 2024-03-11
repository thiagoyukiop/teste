if(interactive()){
  
  options(device.ask.default = FALSE)
  
  if(!require(pacman)) {
    print(paste0("Pacote ainda não instalado. Irei instalar agora!"))
    install.packages("pacman", dependencies = TRUE)
    library(pacman)
  } else {
    print(paste0("Pacote já estava instalado e já carregado para o trabalho"))
    library(pacman)
  }
  
  p_load(shiny, shinydashboard, shinydashboardPlus, shinythemes, readtext)
  
  shinyApp(
    ui = dashboardPage(
      options = list(sidebarExpandOnHover = TRUE),
      header = dashboardHeader(
        title = "  Projeto Tubarão Azul"
      ),
      sidebar = dashboardSidebar(
        width = 250,
        id = "sidebar",
        minified = FALSE,
        collapsed = FALSE,
        tags$head(
        tags$style(HTML('
            /* Ajuste o tamanho dos títulos das abas dentro do sidebarPanel */
            .custom-sidebar .nav-tabs > li > a {
              width: 240px; /* Largura fixa para os títulos das abas */
              text-align: center; /* Centraliza o texto */
            }
            /* Estilo para alterar a cor do texto do sidebar para preto */
            #sidebar .sidebar-menu li a,
            #sidebar .sidebar-menu li a:hover,
            #sidebar .sidebar-menu label {
              color: #000000 !important; /* Cor preta */
            }
          '))
        ),
       #     tags$head(
       #       tags$style(HTML("
       #   /* Estilo para alterar a cor do texto do sidebar para preto */
       #   #sidebar .sidebar-menu li a,
       #   #sidebar .sidebar-menu li a:hover,
       #   #sidebar .sidebar-menu label {
       #     color: #000000 !important; /* Cor preta */
       #   }
       #   #sidebar .sidebar-menu span {
       #     color: #000000 !important; /* Cor preta */
       #   }
       #   #sidebar .sidebar-menu p {
       #     color: #000000 !important; /* Cor preta */
       #   }
       # "))
       #     ),
        sidebarMenu(
          id = "sidebarMenu",
          sidebarLayout(
            sidebarPanel(
              width = 250,class = "custom-sidebar",
              tabsetPanel(
                id = "headerTab",
                tabPanel("Apresentação", value = "tab1header"),
                tabPanel("Distribuição de comprimentos", value = "tab2header"),
                tabPanel("Desembarques", value = "tab3header"),
                tabPanel("Distribuição espacial das capturas", value = "tab4header")
                )
              ),
            mainPanel(
              imageOutput(outputId = "imgSide",width = "50%", height = "50%")
              )
            )
          )
        ),
      body = dashboardBody(
        tabsetPanel(
          id = "bodyTab",
          tabPanel("Projeto", value = "tab1body"),
          tabPanel("Leia-me", value = "tab2body")
        ),
        tags$head(
          tags$style(HTML('
            /* Definir a largura do accordion */
            #accordion .box {
              width: 520px; /* Substitua por qualquer valor que desejar */
              text-align: center; /* Centraliza o texto */
            }
          '))
        ),
        uiOutput("accordion_ui")
      ),
      controlbar = dashboardControlbar(
        collapsed = FALSE,
        id = "controlbar",
        controlbarMenu(
          id = "controlbarMenu",
          controlbarItem(
            "Opções",
            sliderInput(
              "obs",
              "Number of observations:",
              min = 0,
              max = 1000,
              value = 10,
              step = 10,
              animate = animationOptions(
                interval = 800,
                playButton = icon("play"),
                pauseButton = icon("pause")
              )
            ),
            checkboxGroupInput(
              "species", "Seletor de Espécies:",
              c("Albacora bandolim","Albacora branca","Albacora laje",
                "Meca", "Outros"),
              selected = c("Albacora bandolim","Albacora branca",
                           "Albacora laje", "Meca","Outros")
            ),
          ),
          
          controlbarItem(
            "Tema",
            "Bem-Vindo ao Seletor de Tema",
            skinSelector()
          )
        )
        
      ),
      title = "Teste ShinyDashboardPlus",
    ),
    server <- function(input, output, session) {
      
      docx_content1 <- readtext("dados_brutos/testeWord.docx")
      
      docx_content2 <- readtext("dados_brutos/leiame.docx")
      
      output$accordion_ui <- renderUI({
        if(input$headerTab == "tab2header") {
          accordion(
            id = "accordion1",
            width = 60,
            accordionItem(
              title = "Visualização dos Dados",
              status = "primary",
              collapsed = FALSE,
              fluidRow(
                column(6, plotOutput("distPlot")),
                column(6, plotOutput("plot2")),
                column(6, plotOutput("plot3")),
                column(6, plotOutput("plot4")),
              )
            )
          )
        } else if(input$headerTab == "tab1header") {
          accordion(
            id = "accordion2",
            width = 60,
            accordionItem(
              title = "Visualização de Texto",
              status = "primary",
              collapsed = FALSE,
              # textOutput("textOut1"),
              # textOutput("textOut2")
              if(input$bodyTab == "tab1body"){
                textOutput("textOut1")
              } else{
                textOutput("textOut2")
              }
            )
          )
        }
      })
      
      # output$textOut <- renderText({
      #   if(input$tabs == "tab1header"){
      #     texto <- readLines("dados_brutos/testeOutputText.txt", warn = FALSE)
      #     paste(texto, collapse = "\n")
      #   }
      # })
      
      output$textOut1 <- renderText({
        if(input$headerTab == "tab1header" && input$bodyTab == "tab1body"){
          paste(docx_content1$text, collapse = "\n")
        }
      })
      
      output$textOut2 <- renderText({
        if(input$headerTab == "tab1header" && input$bodyTab == "tab2body"){
          paste(docx_content2$text, collapse = "\n")
        }
      })
      
      output$distPlot <- renderPlot({
        if(input$obs != 0 && input$headerTab == "tab2header"){
          hist(rnorm(input$obs), col = "blue",
               main = paste("Histograma Teste - Observações:", input$obs),
               xlab = "", freq = TRUE)
        }
      })
      
      output$plot2 <- renderPlot({
        if(input$obs != 0 && input$headerTab == "tab2header"){
         plot(density(rnorm(input$obs)))
        }
      })
      
      output$plot3 <- renderPlot({
        data <- data.frame(Label = c("A", "B", "C"), Value = c(30, 40, 20))
        pie(data$Value, labels = data$Label, col = rainbow(nrow(data)))
      })
      
      output$plot4 <- renderPlot({
        barplot(table(sample(letters[1:5], input$obs, replace = TRUE)))
      })
      
      }
    )
}
