library(shiny)
library(shinyMobile)
library(apexcharter)
library(dplyr)

dados <-read.csv("dados_brutos/dados_brutos.csv", sep = ";")

# poll2 <- data.frame(
#   answer = c("Yes", "No"),
#   n = c(254, 238)
# )

  poll <- dados %>%
    count(Sexo)

shinyApp(
  ui = f7Page(
    options = list(dark = FALSE, filled = FALSE, theme = "md"),
    title = "My app",
    f7TabLayout(
      panels = tagList(
        f7Panel(title = "Left Panel", side = "left", theme = "light", "Blabla", 
                effect = "cover"),
        f7Panel(title = "Right Panel", side = "right", theme = "dark", "Blabla",
                effect = "cover")
      ),
      navbar = f7Navbar(
        title = "Tabs",
        hairline = TRUE,
        shadow = TRUE,
        leftPanel = TRUE,
        rightPanel = TRUE
      ),
      f7Tabs(
        animated = TRUE,
        #swipeable = TRUE,
        f7Tab(
          title = "Tab 1",
          tabName = "Tab1",
          icon = f7Icon("folder"),
          active = TRUE,
          
          f7Shadow(
            intensity = 10,
            hover = TRUE,
            f7Card(
              title = "Card header",
              apexchartOutput("donut")
            )
          )
        ),
        f7Tab(
          title = "Tab 2",
          tabName = "Tab2",
          icon = f7Icon("keyboard"),
          active = FALSE,
          f7Shadow(
            intensity = 10,
            hover = TRUE,
            f7Card(
              title = "Card header",
              apexchartOutput("scatter")
            )
          )
        ),
        f7Tab(
          title = "Tab 3",
          tabName = "Tab3",
          icon = f7Icon("layers_alt"),
          active = FALSE,
          f7Shadow(
            intensity = 10,
            hover = TRUE,
            f7Card(
              title = "Card header",
              f7SmartSelect(
                "variable",
                "Variables to show:",
                c("Cylinders" = "cyl",
                  "Transmission" = "am",
                  "Gears" = "gear"),
                openIn = "sheet",
                multiple = TRUE
              ),
              tableOutput("data")
            )
          )
        )
      )
    )
  ),
  server = function(input, output, session) {
    
    # river plot
    dates <- reactive(seq.Date(Sys.Date() - 30, Sys.Date(), by = input$by))
    
    output$donut <- renderApexchart({
      apex(
        data = poll,
        type = "donut",
        mapping = aes(x = Sexo, y = n)
      )
    })
    
    output$scatter <- renderApexchart({
      apex(
        data = mtcars,
        type = "scatter",
        mapping = aes(
          x = wt,
          y = mpg,
          fill = cyl
        )
      )
    })
    
    # datatable
    output$data <- renderTable({
      mtcars[, c("mpg", input$variable), drop = FALSE]
    }, rownames = TRUE)
  }
)
plo