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
    
    p_load(shiny, shinydashboard, shinydashboardPlus, shinythemes)
    
    shinyApp(
      ui = dashboardPage(
        options = list(sidebarExpandOnHover = TRUE),
        header = dashboardHeader(
           title = "Projeto Tubarão Azul"
        ),
        sidebar = dashboardSidebar(width = 100,
          
          id = "sidebar",
          minified = FALSE,
          collapsed = FALSE,
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
              sidebarPanel(width = 12,
               tabsetPanel(
                 id = "tabs",
                 tabPanel("Apresentação", value = "tab1"),
                 tabPanel("Distribuição de comprimentos", value = "tab2"),
                 tabPanel("Desembarques", value = "tab3"),
                 tabPanel("Distribuição "/n"espacial das capturas", value = "tab4")
               )
              ),
              mainPanel()
            )
          )
        ),
        body = dashboardBody(
          plotOutput("distPlot"),
          actionButton(inputId = "controlbarMenuToggle",
                       label = "Toggle Controlbar")
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
      server = function(input, output, session) {
        observeEvent(input$tabs, {
          selected_tab <- input$tabs
          print(paste("Conteúdo da aba selecionada:", selected_tab))
          print(selected_tab)
        })
        
        observeEvent(input$controlbarMenuToggle, {
          updateControlbarMenu("controlbar")
        })
        
        
        output$distPlot <- renderPlot({
          if(input$obs!=0){
            if(input$tabs=="tab4"){
              hist(rnorm(input$obs), col = "blue",
                   main = paste("Histograma Teste - Observações:", input$obs),
                   xlab = "", freq = TRUE)
            }
          }
        })
      }
    )
  }
  