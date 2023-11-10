library(shiny)
library(shinythemes)

ui <- navbarPage(
  title = "ShinyByte",
  theme = shinytheme("darkly"),
  tabPanel(
    "Data",
    sidebarPanel(dataUI("data")$side),
    mainPanel(dataUI("data")$main)
  ),
  tabPanel(
    "Visualize",
    sidebarLayout(
      sidebarPanel(visualizeUI("visualize")$side),
      mainPanel(visualizeUI("visualize")$main)
    )
  ),
  tabPanel(
    "Model",
     sidebarLayout(
       sidebarPanel(modelUI("model")$side),
       mainPanel(modelUI("model")$main)
     )
  )
)

server <- function(input, output, session) {
  data <- dataServer("data")
  visualizeServer("visualize", data)
  modelServer("model", data)
}

shinyApp(ui, server)