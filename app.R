library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggridges)
library(lubridate)
source('plots.R')

source('helpers.R')

load('data/live/df.RData')
load('data/live/client_means.RData')
load('data/live/probe_hists.RData')
page_load_map <- get.page_load_map()

end_date <- max(df$app_build_id)
min_default_date <- end_date-28

ui <- dashboardPage(
  dashboardHeader(title = "Regression Testing"),
  dashboardSidebar(
    menuItem("Page Load",
             tabName = "page_load",
             icon = icon("dashboard"))
    # menuItem("Graphics",
    #          icon = icon("th"),
    #          tabName = "graphics")
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    tabBox(
      id = "tabset1",
      width = 12,
      tabPanel('RelDS',
               fluidRow(
                 box(title = "Description",
                     includeMarkdown("page_load_relds_desc.Rmd")),
                 box(selectInput('probe', 'Probe',  page_load_map),
                     dateRangeInput('page_load_relds_date_range', paste('Date Range (from ', min(df$app_build_id), ')'), 
                                    start = min_default_date, end = end_date)
                 )
               ),
               fluidRow(box(
                 plotOutput(
                   "page_load",
                   height = 500,
                   width = "100%",
                   click = "page_load_click",
                   dblclick = "page_load_relds_dblclick",
                   brush = brushOpts(id = "page_load_relds_brush",
                                     resetOnNew = TRUE)
                 ),
                 width = 12
               )),
               fluidRow(box(
                 width = 12,
                 h4("Build Stats"),
                 verbatimTextOutput("click_info")
               ))),
      tabPanel(
        'Client Means',
        fluidRow(
          box(title = "Description",
              includeMarkdown("page_load_client_means_desc.Rmd")),
          box(
            selectInput('client_mean_probe', 'Probe',  page_load_map),
            splitLayout(
              checkboxInput("fit_lm", "Fit Linear Model?", FALSE),
              checkboxInput("yaxis_log10", "Y-axis: Log10?", FALSE)
            ),
            radioButtons(
              'dist_type',
              'Plot Type:',
              c('Boxplot' = 'boxplot', 'Violin' = 'violin'),
              inline = TRUE
            ),
            dateRangeInput('client_means_relds_date_range', paste('Date Range (from ', min(df$app_build_id), ')'), 
                           start = min_default_date, end = end_date)
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
        fluidRow(column(
          width = 12,
          h4("Quantiles"),
          verbatimTextOutput("page_load_client_means_click_info")
        ))
      ),
      tabPanel(
        'Build Histograms',
        fluidRow(
          box(title = "Description",
              includeMarkdown("probe_hists_desc.Rmd")),
          box(
            selectInput('hist_ridge_probe', 'Probe',  page_load_map),
            
            numericInput(
              "ridge_scale",
              "Ridge Plot: Scale",
              5,
              min = 0.1,
              max = 20
            ),
            fluidRow(
              column(
                width = 6,
                selectInput(
                  'app_build',
                  'App Build',
                  unique(probe_hists$app_build_id),
                  multiple = TRUE,
                  selectize = TRUE
                )
              ),
              column(
                width = 3,
                checkboxInput("cdf_recompute", "Recompute CDF?", FALSE)
              )
            )
          )
        ),
        splitLayout(
          box(
            plotOutput(
              "probe_cdf_hists",
              height = 700,
              width = "100%",
              dblclick = "probe_ridge_hists_dblclick",
              # click = "page_load_client_means_click",
              brush = brushOpts(id = "probe_ridge_hist_brush",
                                resetOnNew = TRUE)
            ),
            width = '100%'
          ),
          
          box(
            plotOutput(
              "probe_ridge_hists",
              height = 700,
              width = "100%",
              dblclick = "probe_ridge_hists_dblclick",
              # click = "page_load_client_means_click",
              brush = brushOpts(id = "probe_ridge_hist_brush",
                                resetOnNew = TRUE)
            ),
            width = '100%'
          ),
          height = '100%'
        )
      )
    )
  )
)

server <- function(input, output, session) {
  ranges <- reactiveValues(x = c(min_default_date, end_date), y = NULL)
  page_load_ranges <- reactiveValues(x = c(min_default_date, end_date), y = NULL)
  p_hist_ranges <- reactiveValues(x = NULL, y = NULL)
  
  
  # page load plot
  output$page_load <- renderPlot({
    plot.crt(df, input, page_load_ranges, page_load_map)
  })

  observeEvent(input$page_load_relds_date_range, {
    page_load_ranges$x <- input$page_load_relds_date_range
  })

  observeEvent(input$page_load_relds_dblclick, {
    brush <- input$page_load_relds_brush
    if (!is.null(brush)) {
      page_load_ranges$x <- as_date(c(brush$xmin, brush$xmax))
      page_load_ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      page_load_ranges$x <- c(min_default_date, end_date)
      page_load_ranges$y <- NULL
    }
    updateDateRangeInput(session, "page_load_relds_date_range",
                         start = page_load_ranges$x[1],
                         end = page_load_ranges$x[2]
    )
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
  
  observeEvent(input$client_means_relds_date_range, {
    ranges$x <- input$client_means_relds_date_range
  })
  
  observeEvent(input$page_load_client_means_dblclick, {
    brush <- input$page_load_client_means_brush
    if (!is.null(brush)) {
      ranges$x <- as_date(c(brush$xmin, brush$xmax))
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- c(min_default_date, end_date)
      ranges$y <- NULL
    }
    updateDateRangeInput(session, "client_means_relds_date_range",
                         start = ranges$x[1],
                         end = ranges$x[2]
    )
  })
  
  output$page_load_client_means_click_info <- renderPrint({
    if (!is.null(input$page_load_client_means_click$x)) {
      x_date <- as_date(round(input$page_load_client_means_click$x))
      client_means_tbl <-
        client_means[client_means$date == x_date, c('date', input$client_mean_probe)]
      final <- client_means_tbl %>%
        summarise_at(.vars = c(input$client_mean_probe),
                     funs(!!!calc_quantile())) %>%
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
  
  # probe hist plots
  output$probe_ridge_hists <- renderPlot({
    plot.probe_hist(probe_hists, input, page_load_map, p_hist_ranges)
  })
  
  output$probe_cdf_hists <- renderPlot({
    plot.probe_cdf(probe_hists, input, page_load_map, p_hist_ranges)
  })
  
  observeEvent(input$probe_ridge_hists_dblclick, {
    brush <- input$probe_ridge_hist_brush
    if (!is.null(brush)) {
      p_hist_ranges$x <- c(brush$xmin, brush$xmax)
      p_hist_ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      p_hist_ranges$x <- NULL
      p_hist_ranges$y <- NULL
    }
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
