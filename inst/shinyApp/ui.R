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
      uiOutput("headerSummary"),
      uiOutput("mbo1Summary"),
      tabsetPanel(
        tabPanel(
          "Performance",
          fluidRow(
            uiOutput("headerPerformance"),
            plotOutput("PerformancePlot")
          )
        ),
        tabPanel(
          "Input Space",
          fluidRow(
            uiOutput("headerInputSpace"),
            plotOutput("InputSpacePlot")
          )
        ),
        tabPanel(
          "Search Space Optimizer",
          fluidRow(
            uiOutput("headerSearchSpace"),
            plotOutput("SearchSpacePlot")
          )
        )
    ))),
    tabPanel("Diagnostic Tool for Single Iteration"),
    tabPanel("About")
  )


# tags = tagList(
#
# )
#
# shinyUI(tags)

