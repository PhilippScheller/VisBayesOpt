#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#



library(shiny)
library(shinythemes)

# modify upload size of files to 200MB
options(shiny.maxRequestSize = 200*1024^2)



# Define UI for application
ui <- navbarPage(
    "Visual Diagnostics for Bayesian Optimization",
    theme = shinytheme("darkly"),
    tabPanel(
        "Visualize mlrMBO run",
        fluidRow( 
            column(5, fileInput("mbo1", "Select mlr run from local directory"),
                   textOutput("mbo1Check")),
            column(7, plotOutput("mbo1Plot"))
        )
    ),
   tabPanel(
       "BayesOpt Playground"
   ),
   tabPanel(
       "About"
   )
    
)
