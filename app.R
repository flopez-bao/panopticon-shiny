## app.R ##
library(shiny)
library(shinydashboard)
library(data.table)
library(fasttime)
library(plotly)
library(DT)

#apiKey <- Sys.getenv("CONNECT_API_KEY")
#connectServer <- Sys.getenv("CONNECT_SERVER")

source("functions.R")
source("data_new.R")

ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(
    tags$li(a(
      img(src = 'baoSystems.png',
          title = "Company Home", height = "50px"),
      style = "padding-top:10px; padding-bottom:10px;"),
      class = "dropdown"),
    title = "Panopticon - Shiny Server"
                  
  ),
    ## Sidebar content
    dashboardSidebar(
        sidebarMenu(
            menuItem("Individual App Metrics", tabName = "individual_metrics", icon = icon("dashboard", verify_fa = FALSE)),
            menuItem("Comparisons", tabName = "comparisons", icon = icon("th")),
            menuItem("Data Bank", tabName = "databank", icon =icon("fas fa-database"))
        )
    ),
    ## Body content
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "individual_metrics",
                    # server wide app wide metrics
                    h4("Server Wide Values"),
                    fluidRow(
                      valueBoxOutput("sumUsers", width = 4),
                      valueBoxOutput("meanTime", width = 4),
                      valueBoxOutput("sumTime", width = 4)
                    ),
                    h4("App Specific Values"),
                    fluidRow(
                      valueBoxOutput("sumUsersApp", width = 4),
                      valueBoxOutput("meanTimeApp", width = 4),
                      valueBoxOutput("sumTimeApp", width = 4)
                    ),
                    fluidRow(
                      box(uiOutput("application_choices"),width = 6),
                      box(textOutput("application_choice_note"), width = 3),
                      actionButton("refresh", "Refresh Data")
                    ),
                    fluidRow(
                        box(plotlyOutput("visits", height = 250)),
                        box(plotlyOutput("users", height = 250)),
                        box(plotlyOutput("sessions_avg_time", height = 250)),
                        box(plotlyOutput("sessions_tot_time", height = 250))
                        
                    )
            ),
            
            # Second tab content
            tabItem(tabName = "comparisons",
                    #h2("Widgets tab content")
                     fluidRow(
                         box(plotlyOutput("visits_c", height = 500)),
                         box(plotlyOutput("users_c", height = 500)),
                         box(plotlyOutput("sessions_avg_time_c", height = 500)),
                         box(plotlyOutput("sessions_tot_time_c", height = 500)),
                         box(plotlyOutput("visits_perc", height = 500)),
                         box(plotlyOutput("sessions_tot_time_perc", height = 500)),
                     )
            ),
            tabItem(tabName = "databank",
                    fluidPage(
                      title = 'Table Access',
                        tabsetPanel(
                          type = "tabs",
                          tabPanel("Usage Table - Raw Data", value = 'current_usage'
                                   , downloadButton('downloadData1', 'Download Data Set')
                                   , DT::dataTableOutput('current_usage_table')
                                   ),
                          tabPanel("Aggregated Metrics", value ='current_usage_agg'
                                   , downloadButton('downloadData2', 'Download Data Set')
                                   , DT::dataTableOutput('current_usage_agg_table')
                                   )
                        )
                        
                    )
        
                    )
        )
    )
)

server <- function(input, output) {
  
  output$application_choice_note <- renderText({
    "*The graphs below will change according to the application choice drop down. Press refresh to see latest data."
  })
    
    # metric choices
    metrics <- names(current_usage_agg_f)
    metrics <- metrics[!(metrics %in% c("year_month", "app"))]
    
    
    # choose your application
    output$application_choices <- renderUI({
        tagList(
            selectInput("application", label = h3("Applications"), choices = unique(current_usage_agg_f$app), selected = "Data Pack")#,
            #h6("Choose an application to see metrics on the dashboard change.")
        )
    })
    
    # choose your metric
    output$metric_choices <- renderUI({
        tagList(
            selectInput("metric", label = h3("Metrics"), choices = metrics, selected = "num_visits")
        )
    })
    
    # data ----
    
    # data sets pulled from data.R
    datasets <- reactiveValues(
      current_usage = current_usage,
      current_usage_agg = current_usage_agg,
      current_usage_tot = current_usage_tot,
      current_usage_agg_f = current_usage_agg_f,
      vb_metrics_app = vb_metrics_app,
      vb_metrics = vb_metrics
    )

    # data frames
    current_usage_agg_reactive <- reactive({
      datasets$current_usage_agg_f[datasets$current_usage_agg_f$app == input$application,]
    })
    
    # value boxes app wide
    vb_metrics_reactive_app <- reactive({
      datasets$vb_metrics_app[datasets$vb_metrics_app$app == input$application,]
    })
    
    # value boxes server wide
    vb_metrics_reactive <- reactive({
      datasets$vb_metrics
    })
    
    # TAB 1 ----
    
    ## graph list ----
    
    graphs <- reactive({
        req(current_usage_agg_reactive())
        df <- as.data.frame(current_usage_agg_reactive())
        graphs <- c("num_visits", "num_users", "mean_sess_time", "sum_sess_time")
        
        gl <- lapply(graphs, function(graph) {
            
            plot_ly(df, 
                    x = ~year_month
                    ,y = ~df[,graph]
                    ,type = 'bar') 
            
        })
        
    })
    
    ## visits ----
    
    output$visits <- renderPlotly({
      
      graphs()[[1]] %>%
        layout(title = "Number of Visits", 
               xaxis = list(           
                 title = "Month",    
                 showgrid = F,
                 tickangle = 90
               ),
               yaxis = list(           
                 title = "Visits"      
               ))
        
    })
    
    # users ----
    
    output$users <- renderPlotly({

      graphs()[[2]] %>%
        layout(title = "Number of Users",
               xaxis = list(
                 title = "Month",
                 showgrid = F,
                 tickangle = 90
               ),
               yaxis = list(
                 title = "Users"
               ))
      
    })
    
    # sessions avg time ----
    
    output$sessions_avg_time <- renderPlotly({

        graphs()[[3]] %>%
            layout(title = "Average User Session Time", 
                   xaxis = list(           
                       title = "Month",    
                       showgrid = F,
                       tickangle = 90
                   ),
                   yaxis = list(           
                       title = "Minutes"      
                   ))
        
    })
    
    # sessions total time ----
    output$sessions_tot_time <- renderPlotly({

        graphs()[[4]] %>%
            layout(title = "Total Session Time", 
                   xaxis = list(           
                       title = "Month",    
                       showgrid = F,
                       tickangle = 90
                   ),
                   yaxis = list(           
                       title = "Minutes"      
                   ))
            
    })
    
    # value boxes app bases ----
    output$sumUsersApp = renderValueBox({

      df <- vb_metrics_reactive_app()
      recent_date = Sys.Date()
      sumUsers <- paste0(df$num_users, " users")
      statement = paste0("Total unique Users for ",input$application," as of 'SAMPLE'")

      valueBox(
        paste0(sumUsers), sub("SAMPLE",recent_date,statement), icon = icon("fas fa-users", verify_fa = FALSE),
        color = "yellow")
    })

    output$meanTimeApp = renderValueBox({

      df <- vb_metrics_reactive_app()
      recent_date = Sys.Date()
      meanTime <- paste0(df$mean_time, " mins")
      statement = paste0("Average time spent on ",input$application, " as of 'SAMPLE'")

      valueBox(
        paste0(meanTime), sub("SAMPLE",recent_date,statement), icon = icon("fas fa-clock-o", verify_fa = FALSE),
        color = "yellow")
    })

    output$sumTimeApp = renderValueBox({

      df <- vb_metrics_reactive_app()
      recent_date = Sys.Date()
      sumTime <- paste0(df$sum_time, " hours")
      statement = paste0("Total time spent on ", input$application," as of 'SAMPLE'")

      valueBox(
        paste0(sumTime), sub("SAMPLE",recent_date,statement), icon = icon("fas fa-clock-o", verify_fa = FALSE),
        color = "yellow")
    })
    
    
    
    # value boxes server app wide ----
    output$sumUsers = renderValueBox({

      recent_date = Sys.Date()
      sumUsers <- paste0(vb_metrics_reactive()$sum_users, " users")
      statement = "Total unique Users as of 'SAMPLE'"
        
      valueBox(
        paste0(sumUsers), sub("SAMPLE",recent_date,statement), icon = icon("fas fa-users", verify_fa = FALSE), color = "green"
        )
    }) 
    
    output$meanTime = renderValueBox({

        recent_date = Sys.Date()
        meanTime <- paste0(vb_metrics_reactive()$mean_time, " mins")
        statement = "Average time on Apps as of 'SAMPLE'"
        
        valueBox(
            paste0(meanTime), sub("SAMPLE",recent_date,statement), icon = icon("fas fa-clock-o", verify_fa = FALSE),
            color = "green")
    }) 
    
    output$sumTime = renderValueBox({

        recent_date = Sys.Date()
        sumTime <- paste0(vb_metrics_reactive()$sum_time, " hours")
        statement = "Total time on Apps as of 'SAMPLE'"
        
        valueBox(
            paste0(sumTime), sub("SAMPLE",recent_date,statement), icon = icon("fas fa-clock-o", verify_fa = FALSE),
            color = "green")
    }) 
    
    
    # TAB 2 ----
    
    ## visits_c ----
    output$visits_c <- renderPlotly({
        
        plot_ly(datasets$current_usage_agg_f, 
                x = ~year_month
                ,y = ~num_visits
                ,type = 'bar'
                ,name = ~app
                ,color = ~app) %>%
            layout(title = "Number of Visits", 
                   xaxis = list(           
                       title = "Month",    
                       showgrid = F,
                       tickangle = 90
                   ),
                   yaxis = list(           
                       title = "Visits"
                   )
                   , barmode = 'stack')
    })
    
    # users_c----
    output$users_c <- renderPlotly({
        
        plot_ly(datasets$current_usage_agg, 
                x = ~year_month
                ,y = ~num_users
                ,type = 'bar'
                ,name = ~app
                ,color = ~app) %>%
            layout(title = "Number of Users", 
                   xaxis = list(           
                       title = "Month",    
                       showgrid = F,
                       tickangle = 90
                   ),
                   yaxis = list(           
                       title = "Users"      
                   )
                   , barmode = 'stack')
        
    })
    
    # sessions avg time ----
    output$sessions_avg_time_c <- renderPlotly({
  
  plot_ly(datasets$current_usage_agg, 
        x = ~year_month
        ,y = ~mean_sess_time
        ,type = 'bar'
        ,name = ~app
        ,color = ~app) %>%
  layout(title = "Average User Session Time", 
               xaxis = list(           
                 title = "Month",    
                 showgrid = F,
                 tickangle = 90
               ),
               yaxis = list(           
                 title = "Minutes"      
               )
         , barmode = 'stack')
  
})
    
    output$sessions_tot_time_c <- renderPlotly({
        
        plot_ly(datasets$current_usage_agg, 
                x = ~year_month
                ,y = ~sum_sess_time
                ,type = 'bar'
                ,name = ~app
                ,color = ~app) %>%
            layout(title = "Total Session Time", 
                   xaxis = list(           
                       title = "Month",    
                       showgrid = F,
                       tickangle = 90
                   ),
                   yaxis = list(           
                       title = "Minutes"      
                   )
                   , barmode = 'stack')
        
    })
    
    output$visits_perc <- renderPlotly({
      plot_ly(datasets$current_usage_agg_f, 
              x = ~year_month
              ,y = ~perc_num_visits
              ,type = 'bar'
              ,name = ~app
              ,color = ~app) %>%
        layout(title = "Percent of Visits", 
               xaxis = list(           
                 title = "Month",    
                 showgrid = F,
                 tickangle = 90
               ),
               yaxis = list(           
                 title = "%"
               )
               , barmode = 'stack')
    })
    
    output$sessions_tot_time_perc <- renderPlotly({
      
      plot_ly(datasets$current_usage_agg_f, 
              x = ~year_month
              ,y = ~perc_sess_time
              ,type = 'bar'
              ,name = ~app
              ,color = ~app) %>%
        layout(title = "Percent of Total Session Time", 
               xaxis = list(           
                 title = "Month",    
                 showgrid = F,
                 tickangle = 90
               ),
               yaxis = list(           
                 title = "%"      
               )
               , barmode = 'stack')
      
    })
    
    
    
    output$downloadData1 = downloadHandler(
      filename = function() {
        paste('raw_usage_data_', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write.csv(
          datasets$current_usage
            , con)
      }
    )
    
    output$downloadData2 = downloadHandler(
      filename = function() {
        paste('aggregated_metrics_', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write.csv(
          datasets$current_usage_agg_f
          , con)
      }
    )
    
    output$current_usage_table = DT::renderDataTable({
      DT::datatable(datasets$current_usage)
    })
    
    output$current_usage_agg_table = DT::renderDataTable({
      DT::datatable(datasets$current_usage_agg_f)
    })
    
    # refresh data ----
    observeEvent(input$refresh, {
      source("data_new.R")
      datasets$current_usage = current_usage
      datasets$current_usage_agg = current_usage_agg
      datasets$current_usage_tot = current_usage_tot
      datasets$current_usage_agg_f = current_usage_agg_f
      datasets$vb_metrics_app = vb_metrics_app
      datasets$vb_metrics = vb_metrics
    })
    
}

shinyApp(ui, server)