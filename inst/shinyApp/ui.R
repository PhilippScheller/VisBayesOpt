#
# You can run the application by calling the function 'runAppLocal()'.
#
library(shiny)
library(shinyFiles)
library(shinythemes)
library(shinydashboard)
library(markdown)
library(knitr)

source("ui-helpers.R", local = TRUE)
#source("Docs.Rmd", local = TRUE)

# modify upload-size of files to 200MB
options(shiny.maxRequestSize = 200 * 1024 ^ 2)

# knit markdown files
path_app = getwd()
path_rmd = "./Rmd-docs"
setwd(path_rmd)
rmdfiles = list.files(pattern = paste("*.Rmd",sep = ""), full.names = TRUE)
sapply(rmdfiles, knit, quiet = T)
setwd(path_app)

# Define UI for application
ui <- navbarPage(
  "Visual Diagnostics for Bayesian Optimization",
  theme = shinytheme("yeti"),
  tabPanel(
    "Setup",
    sidebarPanel(
      fileInput("mbo1",
                helpText(h4("Select a 'final.opt.state' from a",
                         a(href="https://mlrmbo.mlr-org.com", "mlrMBO"), " run from a local directory"))),
      uiOutput("mbo1Check"),
      wellPanel(p("Export Plots"),
              shinyDirButton("inputDir", "Chose directory", "Upload"),
              uiOutput("directorySuccess"),
              actionButton("exportPlot", "Export Plot as png", icon("paper-plane")),
              uiOutput("saveSuccess"))
    ),
    mainPanel(
      uiOutput("headerSummary"),
      uiOutput("mbo1Summary"))
  ),
  tabPanel(
    "Visualize mlrMBO Run",
    sidebarPanel(
        wellPanel(p("General input for run summary"),
        uiOutput("ui_run")),
        width = 3
        ),
    mainPanel(
      uiOutput("headerSummary1"),
      uiOutput("mbo1Summary1"),
    tabsetPanel(id = "runTab",
    tabPanel(
      "Performance",
      fluidRow(
        uiOutput("headerPerformance"),
        plotOutput("PerformancePlot"),
        withMathJax(includeMarkdown("Rmd-docs/MboPlotProgress.md"))
      )
    ),
    tabPanel(
      "Input Space",
      fluidRow(
        uiOutput("headerInputSpace"),
        plotOutput("InputSpacePlot"),
        withMathJax(includeMarkdown("Rmd-docs/MboPlotInputSpace.md"))
      )
    ),
    tabPanel(
      "Search Space Optimizer",
      fluidRow(
        uiOutput("headerSearchSpace"),
        plotOutput("SearchSpacePlot"),
        withMathJax(includeMarkdown("Rmd-docs/MboPlotSearchSpace.md"))
      )
    ),
    tabPanel(
      "Exploration vs. Exploitation",
      fluidRow(
        uiOutput("headerDist2Neighbor"),
        plotOutput("Dist2NeighborPlot"),
        withMathJax(includeMarkdown("Rmd-docs/MboPlotDistToNeighbor.md"))
      )
    )
    )
    )),
    tabPanel("Diagnostic Tool for Single Iteration",
             sidebarPanel(
               #fileInput("mbo1", h4("Select mlr run from local directory")),
               #uiOutput("mbo1Check"),
               wellPanel(p("General input for diagnostic tool"),
                         uiOutput("ui_diagnost")),
               width = 3),
             mainPanel(tabsetPanel(id = "diagTab",
             tabPanel(
               "Run Time",
               fluidRow(plotOutput("RuntimePlot")),
               withMathJax(includeMarkdown("Rmd-docs/MboPlotRuntime.md"))
             ),
             tabPanel(
               "Fit",
               fluidRow(plotOutput("FitPlot")),
               withMathJax(includeMarkdown("Rmd-docs/MboPlotFit.md"))
             ),
             tabPanel(
               "Uncertainty",
               fluidRow(plotOutput("UncertaintyPlot")),
               withMathJax(includeMarkdown("Rmd-docs/MboPlotEstimationUncertainty.md"))
             ),
             tabPanel(
               "Optimization Path",
               fluidRow(plotOutput("OptPathPlot"))
             )
             ))),
    tabPanel("About")
  )


# tags = tagList(
#
# )
#
# shinyUI(tags)

