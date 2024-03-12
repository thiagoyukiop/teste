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
        collapsed = F,
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
          .teste-accordion {
          display: inline-block;
          margin: 10px auto;
          }
      
          .teste-accordion .accordion-title {
          text-align: center; /* Centraliza o texto apenas nos títulos */
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
        uiOutput("tabset_ui"),
        uiOutput("accordion_ui")
      ),
      controlbar = dashboardControlbar(
        collapsed = T,
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
      
      pdf_content1 <- readtext("dados_brutos/testepdf.pdf")
      
      pdf_content2 <- readtext("dados_brutos/leiame.pdf")
      
      output$tabset_ui <- renderUI({
        if(input$headerTab == "tab1header") {
          tabsetPanel(
            id = "bodyTab",
            tabPanel("Projeto", value = "tab1body"),
            tabPanel("Leia-me", value = "tab2body")
          )
        }
      })
      
      output$accordion_ui <- renderUI({
      if(input$headerTab == "tab2header") {
        accordion(
          id = "accordion1",
          width = 12,
          accordionItem(
            title = "Visualização dos Dados",
            status = "primary",
            collapsed = FALSE,
            fluidRow(
              column(6, plotOutput("distPlot")),
              column(6, plotOutput("plot2")),
              column(6, plotOutput("plot3")),
              column(6, plotOutput("plot4"))
            )
          )
        )
      } else if(input$headerTab == "tab1header") {
        div(class = "teste-accordion",
          accordion(
            id = "accordion2",
            accordionItem(
              title = "Visualização de Texto",
              status = "primary",
              collapsed = FALSE,
              if(input$bodyTab == "tab1body"){
                uiOutput("textOut1")
              } else{
                uiOutput("textOut2")
              }
            )
          )
        )
      }
      })
      
      output$textOut1 <- renderUI({
        if(input$headerTab == "tab1header" && input$bodyTab == "tab1body") {
          HTML(paste0("<pre>", pdf_content1$text, "</pre>"))
        }
      })
      
      output$textOut2 <- renderUI({
        if(input$headerTab == "tab1header" && input$bodyTab == "tab2body") {
          HTML(paste0("<pre>", pdf_content2$text, "</pre>"))
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
