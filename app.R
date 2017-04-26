setwd("/home/pepe/pr")
source("./prueba_plotly.R")


library("plotly", lib.loc="~/R/x86_64-redhat-linux-gnu-library/3.3")
library(shiny)



# compute a correlation matrix

ui <- fluidPage(
  mainPanel(
    plotlyOutput("plot1"),
    plotlyOutput("plot2")
  ),
  verbatimTextOutput("selection1"),
  verbatimTextOutput("selection2")
)

server <- function(input, output, session) {
  
  
  output$plot1 <- renderPlotly({
    s <- event_data("plotly_click", source = "plot2")
    if (is.null(s)) s<-list(y="4G")
    grafica_comparativa(filter(toro_idg,DATETIME==as.Date("2016-12-01")),"COUNTRY","CONCEPT",s$y[[1]],"plot1")
  })

  output$plot2 <- renderPlotly({
    s <- event_data("plotly_click", source = "plot1")
    if (is.null(s)) s<-list(y="AR")
    grafica_comparativa(filter(toro_idg,DATETIME==as.Date("2016-12-01")),"CONCEPT","COUNTRY",s$y[[1]],"plot2")
  })
  
  output$selection1 <- renderPrint({
    s <- event_data("plotly_click", source = "plot1")
    if (length(s) == 0) {
      "Click on plot1 to display a selection"
    } else {
      cat("You selected: \n\n")
      as.list(s)
    }
  })
  
  output$selection2 <- renderPrint({
    s <- event_data("plotly_click", source = "plot2")
    if (length(s) == 0) {
      "Click on plot2 to display a selection"
    } else {
      cat("You selected: \n\n")
      as.list(s)
    }
  })
  
}

shinyApp(ui, server, options = list(display.mode = "showcase"))