# dados <- read.table("dados_brutos/dados_brutos.csv",header = T, sep = ";", dec = ",")

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
  
  p_load(shiny, shinydashboard, shinydashboardPlus, shinythemes, 
         readtext, dplyr, ggplot2, ggrepel, scales)
  
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
          .texto-accordion {
          display: inline-block;
          margin: 10px auto;
          }
      
          .texto-accordion .accordion-title {
          text-align: center; /* Centraliza o texto apenas nos títulos */
          }
          
          .graficos-accordion {
          margin: 0 auto;
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
                ),
              imageOutput("creditos_img")
              ),
            mainPanel()
            )
          )
        ),
      body = dashboardBody(
        uiOutput("tabset_ui"),
        fluidRow(
          column(9,
                 uiOutput("accordion_ui")
          )#,
          # column(3,
          #        imageOutput("creditos_img")
          #        )
          )
        # uiOutput("accordion_ui"),
        # uiOutput("creditos_ui")
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
              min = 2018,
              max = 2023,
              value = c(2018,2023),
              step = 1,
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
      
      dados_tubaroes <- reactive({
        # dados <- read.table("dados_brutos/dados_brutos.csv",header = T, sep = ";", dec = ",")
        dados
      })
      
      
      
      
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
      
      output$creditos_img <- renderImage({
        list(src = "dados_brutos/ImagemTeste.png",  # Substitua pelo caminho da sua imagem PNG
             contentType = "image/png",
             # width = 300,  # Largura da imagem
             # height = 440,  # Altura da imagem
          
             alt = "Créditos")  # Texto alternativo para acessibilidade
      }, deleteFile = FALSE)
      
      output$accordion_ui <- renderUI({
      if(input$headerTab == "tab2header") {
        div(class = "graficos-accordion",
            accordion(
              id = "accordion1",
              accordionItem(
                title = "Visualização dos Dados",
                status = "primary",
                collapsed = FALSE,
                fluidRow(
                  # column(6, plotOutput("graficoBarra")),
                  # column(6, plotOutput("graficoRosca")),
                  # column(6, plotOutput("plot3")),
                  # column(6, plotOutput("plot4"))
                )
              )
            )
          )
          
      } else if(input$headerTab == "tab1header") {
        div(class = "texto-accordion",
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
      
      output$graficoBarra <- renderPlot({
        dadostub <- dados_tubaroes()

        dadostub <- subset(dadostub, Ano >= input$intervalo_anos[1]
                        & Ano <= input$intervalo_anos[2])

        # Filtra os dados com base na escolha do sexo
        if (input$sexo_escolhido == "Macho") {
          dadostub <- subset(dadostub, Sexo == "M")
        } else if (input$sexo_escolhido == "Fêmea") {
          dadostub <- subset(dadostub, Sexo == "F")
        }

        # Cria o gráfico ggplot
        ggplot(dadostub, aes(x = Tamanho)) +
          geom_histogram(binwidth = 10, fill = "blue", color = "black") +
          labs(title = "Distribuição de Tamanhos de Tubarões",
               x = "Tamanho (cm)",
               y = "Frequência (%)") +
          scale_y_continuous(labels = scales::label_percent(scale = 0.01))
      })
      
      output$graficoRosca <-renderPlot({
        dadostub <- dados_tubaroes()
        
        dadostub <- subset(
          dadostub, 
          Ano >= input$intervalo_anos[1] & Ano <= input$intervalo_anos[2])
        
        gender <- dadostub %>%
          count(Sexo)
        
        gender <- gender %>%
          mutate(porc = n / sum(n) * 100)
        
        gender$ymax <- cumsum(gender$porc)
        gender$ymin <- c(0, head(gender$ymax, n=1))
        
        ggplot(gender, aes(ymax=ymax, ymin=ymin, xmax = 4, xmin = 3, fill = Sexo)) +
          geom_rect() +
          coord_polar(theta = "y") +
          xlim(c(1,4)) +
          theme_void()
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
      
      # output$distPlot <- renderPlot({
      #   if(input$obs != 0 && input$headerTab == "tab2header"){
      #     hist(rnorm(input$obs), col = "blue",
      #          main = paste("Histograma Teste - Observações:", input$obs),
      #          xlab = "", freq = TRUE)
      #   }
      # })
      # 
      # output$plot2 <- renderPlot({
      #   if(input$obs != 0 && input$headerTab == "tab2header"){
      #    plot(density(rnorm(input$obs)))
      #   }
      # })
      
      output$plot3 <- renderPlot({
        data <- data.frame(Label = c("A", "B", "C"), Value = c(30, 40, 20))
        pie(data$Value, labels = data$Label, col = rainbow(nrow(data)))
      })
      
      output$plot4 <- renderPlot({
        # barplot(table(sample(letters[1:5], input$obs, replace = TRUE)))
      })
      
      }
    )
}
