library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
source('plots.R')

source('helpers.R')

initialize()
page_load_map <- get.page_load_map()

ui <- dashboardPage(
  dashboardHeader(title = "Regression Testing"),
  dashboardSidebar(
    menuItem("Page Load",
             tabName = "page_load",
             icon = icon("dashboard")),
    menuItem("Graphics",
             icon = icon("th"),
             tabName = "graphics")
  ),
  dashboardBody(# Boxes need to be put in a row (or column)
    tabBox(
      id = "tabset1",
      width = 12,
      tabPanel('RelDS',
               fluidRow(
                 box(title = "Description",
                     includeMarkdown("page_load_relds_desc.Rmd")),
                 box(selectInput('probe', 'Probe',  page_load_map))
               ),
               fluidRow(box(
                 plotOutput(
                   "page_load",
                   height = 500,
                   width = "100%",
                   click = "page_load_click"
                 ),
                 width = 12
               )),
               fluidRow(box(
                 width = 12,
                 h4("Build Stats"),
                 verbatimTextOutput("click_info")
               ))),
      tabPanel('Client Means',
               fluidRow(
                 box(title = "Description",
                     includeMarkdown("page_load_client_means_desc.Rmd")),
                 box(
                   selectInput('client_mean_probe', 'Probe',  page_load_map),
                   splitLayout(
                     checkboxInput("fit_lm", "Fit Linear Model?", FALSE),
                     checkboxInput("yaxis_log10", "Y-axis: Log10?", FALSE)
                   ),
                   radioButtons('dist_type', 'Plot Type:', c('Boxplot' = 'boxplot', 'Violin' = 'violin')
                                )
                 )
               ),
               fluidRow(box(
                 plotOutput(
                   "client_means",
                   height = 500,
                   width = "100%",
                   dblclick = "page_load_client_means_dblclick",
                   click = "page_load_client_means_click",
                   brush = brushOpts(id = "page_load_client_means_brush",
                                     resetOnNew = TRUE)
                 ),
                 width = 12
               )),
               fluidRow(
                 column(width = 12,
                        h4("Quantiles"),
                        verbatimTextOutput("page_load_client_means_click_info")
                 )
               )
        
      )
    ))
)

server <- function(input, output) {
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  # page load plot
  output$page_load <- renderPlot({
    plot.crt(df, input, page_load_map)
  })
  
  output$click_info <- renderPrint({
    probe_df <-
      df[df$probe == input$probe, c(
        'app_build_id',
        'num_profiles',
        'num_pings',
        'relds_05',
        'relds_25',
        'relds_5',
        'relds_75',
        'relds_95'
      )]
    nearPoints(data.frame(probe_df), input$page_load_click, addDist = FALSE)
  })
  
  output$client_means <- renderPlot({
    plot.client_means(client_means, input, ranges)
  })
  
  observeEvent(input$page_load_client_means_dblclick, {
    brush <- input$page_load_client_means_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  output$page_load_client_means_click_info <- renderPrint({
    if(!is.null(input$page_load_client_means_click$x)){
      x_date <- as_date(round(input$page_load_client_means_click$x))
      client_means_tbl <- client_means[client_means$date==x_date, c('date', input$client_mean_probe)]
      final <- client_means_tbl %>% 
        summarise_at(.vars = c(input$client_mean_probe), funs(!!!calc_quantile())) %>%
        mutate(date = x_date) %>%
        as.data.frame() 
      # counts <- client_means_tbl %>% count(browser_name_clean) %>%  rename(Browser = browser_name_clean, num_runs = n) 
      # final <- merge(final, counts, by='Browser')
      final$num_clients = nrow(client_means_tbl)
    }
    else{
      final <- ""
    }
    final
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
