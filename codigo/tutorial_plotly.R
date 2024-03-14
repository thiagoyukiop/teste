if(!require(plotly)) {
  print(paste0("Pacote ainda não instalado. Irei instalar agora!"))
  install.packages("plotly", dependencies = TRUE)
  library(plotly)
} else {
  print(paste0("Pacote já estava instalado e já carregado para o trabalho"))
  library(plotly)
}

plot_ly(data = mtcars,
        x = ~wt,
        y = ~mpg,
        mode = "markers",
        type = "scatter",
        # color = ~disp
        color = ~as.factor(cyl),
        size = ~hp
       # colors = "Set1",
        # symbol = ~as.factor(cyl),
        # symbols = c('circle','x','o')
        )
View(mtcars)        
