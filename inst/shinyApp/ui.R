#
# You can run the application by calling the function 'runAppLocal()'.
#
library(shiny)
library(shinythemes)

# modify upload-size of files to 200MB
options(shiny.maxRequestSize = 200 * 1024 ^ 2)

# Define UI for application
ui <- navbarPage(
  "Visual Diagnostics for Bayesian Optimization",
  theme = shinytheme("yeti"),
  tabPanel(
    "Visualize mlrMBO Run",
    sidebarPanel(
        fileInput("mbo1", "Select mlr run from local directory"),
        textOutput("mbo1Check"),
        uiOutput("mbo1Ui"),
        width = 3
        ),
    mainPanel(
      fluidRow(
      textOutput("mbo1Summary"),
      plotOutput("mbo1Plot")
    ))),
    tabPanel("Diagnostic Tool for Single Iteration"),
    tabPanel("About")
  )


# tags = tagList(
#
# )
#
# shinyUI(tags)

