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
    theme = shinytheme("darkly"),
    tabPanel("Visualize mlrMBO run",
             fluidRow(
                 column(
                     4,
                     fileInput("mbo1", "Select mlr run from local directory"),
                     textOutput("mbo1Check"),
                     uiOutput("mbo1Ui")
                 ),
                 column(6, plotOutput("mbo1Plot"))
             )),
    tabPanel("BayesOpt Playground"),
    tabPanel("About")

)
