library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
source('plots.R')

# load('data/evolution_browser_comparison.RData')

source('data_preparation.R')

df <- build.df()

page_load_map = get.page_load_map()

ui <- dashboardPage(
  dashboardHeader(title = "Regression Testing"),
  dashboardSidebar(menuItem(
    "Page Load",
    tabName = "page_load",
    icon = icon("dashboard")
  ),
  menuItem(
    "Graphics",
    icon = icon("th"),
    tabName = "graphics"
  )
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(
        title = "Description",
        includeMarkdown("description.Rmd")
        # selectInput('browser', 'Browser(s)', unique(df_cut$browser_name_clean), multiple=TRUE, selectize=TRUE),
        # checkboxInput("fit_lm", "Fit Linear Model", FALSE),
        # checkboxInput("yaxis_log10", "Y-axis: Log10", TRUE),
        # checkboxInput("boxplot", "Boxplot", TRUE)
      ),
      
      
      box(
        # title = "Probe",
        selectInput('probe', 'Probe',  page_load_map)
        # selectInput('browser', 'Browser(s)', unique(df_cut$browser_name_clean), multiple=TRUE, selectize=TRUE),
        # checkboxInput("fit_lm", "Fit Linear Model", FALSE),
        # checkboxInput("yaxis_log10", "Y-axis: Log10", TRUE),
        # checkboxInput("boxplot", "Boxplot", TRUE)
      )
    ),
    fluidRow(
      box(
        plotOutput("page_load", height = 500, width=750, 
                   dblclick = "boxplot_comp_dblclick",
                   brush = brushOpts(
                     id = "boxplot_comp_brush",
                     resetOnNew = TRUE
                   )
        ),
        width=12)
    )
  )
)

server <- function(input, output) {
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  output$page_load <- renderPlot({
    # df_comp <- if (input$url == 'All') df_cut else df_cut[df_cut$url==input$url, ]
    plot.crt(df, input, page_load_map)
  })
  
  observeEvent(input$boxplot_comp_dblclick, {
    brush <- input$boxplot_comp_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
