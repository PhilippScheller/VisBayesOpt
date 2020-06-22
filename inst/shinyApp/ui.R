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
        fileInput("mbo1", h4("Select mlr run from local directory")),
        uiOutput("mbo1Check"),
        wellPanel(p("Input for 'exploration vs. exploitation'"),
        selectInput("distToNeighbor_measure", h5("Select distance measure for explore/exploit plot"),
                      choices = c("min", "max", "mean"), selected = "min")),
        # uiOutput("mbo1Ui"),
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
        ),
        tabPanel(
          "Exploration vs. Exploitation",
          fluidRow(
            uiOutput("headerDist2Neighbor"),
            plotOutput("Dist2NeighborPlot")
          )
        )
    ))),
    tabPanel("Diagnostic Tool for Single Iteration",
             sidebarPanel(
               #fileInput("mbo1", h4("Select mlr run from local directory")),
               #uiOutput("mbo1Check"),
               wellPanel(p("General input for diagnostic tool"),
                         numericInput("highlight_iteration", h5("Select iteration to be analyzed"),
                                      value = 1, min = 1, step = 1)),
               wellPanel(p("PDP input"),
                        selectInput("pdp_feature", h5("Select variable for PDP"),
                           choices = "", selected = "min")),
               width = 3),
             mainPanel(tabsetPanel(tabPanel(
               "Optimization Path",
               fluidRow(uiOutput("headerOptPath"),
                        plotOutput("OptPathPlot"))
             ),
             tabPanel(
               "Run Time",
               fluidRow(uiOutput("headerRuntime"),
                        plotOutput("RuntimePlot"))
             )

             ))),
    tabPanel("About")
  )


# tags = tagList(
#
# )
#
# shinyUI(tags)

