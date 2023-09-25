## app.R ##
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(data.table)
library(fasttime)
library(plotly)
library(DT)
library(shinycssloaders)
library(aws.s3)
library(magrittr)

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
            menuItem("API Based Metrics", tabName = "individual_metrics", icon = icon("fas fa-satellite-dish", verify_fa = FALSE)),
            menuItem("S3 Logged Metrics", tabName = "logged_data", icon = icon("gauge", verify_fa = FALSE)),
            menuItem("API Data Comparisons", tabName = "comparisons", icon = icon("th")),
            menuItem(" Data Bank", tabName = "databank", icon =icon("fas fa-database"))
        )
    ),
    ## Body content
    dashboardBody(
      tags$head(tags$style(
        HTML('.wrapper {height: auto !important; position:relative; overflow-x:hidden; overflow-y:hidden}')
      )),
        tabItems(
            # First tab content
            tabItem(tabName = "individual_metrics",
                    # server wide app wide metrics
                    h4("The following page shows metrics based on the rstudio connect api which captures named connect user activity."),
                    h4("Server Wide Values"),
                    fluidRow(
                      withSpinner(valueBoxOutput("sumUsers", width = 4)),
                      withSpinner(valueBoxOutput("meanTime", width = 4)),
                      withSpinner(valueBoxOutput("sumTime", width = 4))
                    ),
                    h4("App Specific Values"),
                    fluidRow(
                      withSpinner(valueBoxOutput("sumUsersApp", width = 4)),
                      withSpinner(valueBoxOutput("meanTimeApp", width = 4)),
                      withSpinner(valueBoxOutput("sumTimeApp", width = 4))
                    ),
                    fluidRow(
                      box(uiOutput("application_choices"),width = 6),
                      box(textOutput("application_choice_note"), width = 3),
                      actionButton("refresh", "Refresh Data"),
                      box(textOutput("data_note"), width = 3),
                    ),
                    fluidRow(
                        box(withSpinner(plotlyOutput("visits", height = 250))),
                        box(withSpinner(plotlyOutput("users", height = 250))),
                        box(withSpinner(plotlyOutput("sessions_avg_time", height = 250))),
                        box(withSpinner(plotlyOutput("sessions_tot_time", height = 250)))
                    )
            ),
            
            # tab on logged data
            tabItem(tabName = "logged_data",
                    h4("The following page shows metrics based on logged events written to S3 for all usersl; this captures ALL users as well as event based activity."),
                    # server wide app wide metrics
                    h4("Server Wide Values"),
                    fluidRow(
                      withSpinner(valueBoxOutput("sumLoggedUsers", width = 4)),
                      withSpinner(valueBoxOutput("meanLoggedTime", width = 4)),
                      withSpinner(valueBoxOutput("sumLoggedTime", width = 4))
                    ),
                    h4("App Specific Values"),
                    fluidRow(
                      withSpinner(valueBoxOutput("sumLoggedUsersApp", width = 4)),
                      withSpinner(valueBoxOutput("meanLoggedTimeApp", width = 4)),
                      withSpinner(valueBoxOutput("sumLoggedTimeApp", width = 4))
                    ),
                    fluidRow(
                      box(uiOutput("application_choices_logged"),width = 6),
                      actionButton("refresh_s3", "Refresh Data")
                    ),
                    fluidRow(
                      box(withSpinner(plotlyOutput("visits_logged", height = 250))),
                      box(withSpinner(plotlyOutput("users_logged", height = 250))),
                      box(withSpinner(plotlyOutput("sessions_avg_time_logged", height = 250))),
                      box(withSpinner(plotlyOutput("sessions_tot_time_logged", height = 250)))
                    ),
                    fluidRow(
                      box(withSpinner(plotlyOutput("events_logged", height = 250))),
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
                          tabPanel("API Data - Usage Table - Raw Data", value = 'current_usage'
                                   , downloadButton('downloadData1', 'Download Data Set')
                                   , DT::dataTableOutput('current_usage_table')
                                   ),
                          tabPanel("API Data - Aggregated Metrics", value ='current_usage_agg'
                                   , downloadButton('downloadData2', 'Download Data Set')
                                   , DT::dataTableOutput('current_usage_agg_table')
                                   ),
                          #logged data
                          tabPanel("S3 Logged Raw Data", value ='s3_all_data'
                                   , downloadButton('downloadData3', 'Download Data Set')
                                   , DT::dataTableOutput('s3_all_data')
                          )
                        )
                        
                    )
        
                    ) 
        ) #end tab items
    ),
  footer = dashboardFooter(
      left = "By Fausto Lopez",
      right = "BAO, 2023"
    ) 
)

server <- function(input, output) {
  
  output$application_choice_note <- renderText({
    "*The graphs below will change according to the application choice drop down. Press refresh to see latest data."
  })
  
  output$data_note <- renderText({
    "*All data on this page reflects connect named users."
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
    
    output$application_choices_logged <- renderUI({
      tagList(
        selectInput("application_logged", label = h3("Applications"), choices = unique(s3_all_data$app), selected = "system_yoda")#,
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
      vb_metrics = vb_metrics,
      # logged metrics below
      s3_all_data = s3_all_data,
      vb_metrics_logged = vb_metrics_logged,
      vb_metrics_app_logged = vb_metrics_app_logged,
      current_usage_agg_logged = current_usage_agg_logged,
      events_logged = events_logged
    )

    # data frames
    current_usage_agg_reactive <- reactive({
      datasets$current_usage_agg_f[datasets$current_usage_agg_f$app == input$application,]
    })
    
    current_usage_agg_reactive_logged <- reactive({
      datasets$current_usage_agg_logged[datasets$current_usage_agg_logged$app == input$application_logged,]
    })
    
    events_reactive_logged <- reactive({
      datasets$events_logged[datasets$events_logged$app == input$application_logged,]
    })
    
    # value boxes app wide
    vb_metrics_reactive_app <- reactive({
      datasets$vb_metrics_app[datasets$vb_metrics_app$app == input$application,]
    })
    
    vb_metrics_reactive_app_logged <- reactive({
      datasets$vb_metrics_app_logged[datasets$vb_metrics_app_logged$app == input$application_logged,]
    })
    
    # value boxes server wide
    vb_metrics_reactive <- reactive({
      datasets$vb_metrics
    })
    
    vb_metrics_logged_reactive <- reactive({
      datasets$vb_metrics_logged
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
    
    graphs_logged <- reactive({
      req(current_usage_agg_reactive_logged())
      df <- as.data.frame(current_usage_agg_reactive_logged())
      graphs <- c("num_sessions", "num_users", "mean_sess_time", "sum_sess_time")
      
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
    
    output$visits_logged <- renderPlotly({
      
      graphs_logged()[[1]] %>%
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
    
    output$users_logged <- renderPlotly({
      
      graphs_logged()[[2]] %>%
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
    
    output$sessions_avg_time_logged <- renderPlotly({
      
      graphs_logged()[[3]] %>%
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
    
    # events logged by app graph ----
    output$events_logged <- renderPlotly({
      req(events_reactive_logged())
      plot_ly(events_reactive_logged(), 
              x = ~year_month
              ,y = ~count_event_type
              ,type = 'bar'
              ,name = ~event_type
              ,color = ~event_type) %>%
        layout(title = "How Are People Using this App?", 
               xaxis = list(           
                 title = "Month",    
                 showgrid = F,
                 tickangle = 90
               ),
               yaxis = list(           
                 title = "Count of Events"      
               )
               , barmode = 'stack')
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
    
    output$sessions_tot_time_logged <- renderPlotly({
      
      graphs_logged()[[4]] %>%
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
    

    
    # value boxes app based ----
    output$sumUsersApp = renderValueBox({

      df <- vb_metrics_reactive_app()
      recent_date = Sys.Date()
      sumUsers <- paste0(df$num_users, " users")
      statement = paste0("Total unique Users for ",input$application," as of 'SAMPLE'")

      valueBox(
        paste0(sumUsers), sub("SAMPLE",recent_date,statement), icon = icon("fas fa-users", verify_fa = FALSE),
        color = "yellow")
    })
    
    output$sumLoggedUsersApp = renderValueBox({
      
      df <- vb_metrics_reactive_app_logged()
      print(df)
      recent_date = Sys.Date()
      print(length(df$num_users))

      sumLoggedUsers <- paste0(df$num_users, " users")
      statement = paste0("Total unique Users for ",input$application," as of 'SAMPLE'")
      
      valueBox(
        paste0(sumLoggedUsers), sub("SAMPLE",recent_date,statement), icon = icon("fas fa-users", verify_fa = FALSE),
        color = "yellow"
        )
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
    
    output$meanLoggedTimeApp = renderValueBox({
      
      df <- vb_metrics_reactive_app_logged()
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
    
    output$sumLoggedTimeApp = renderValueBox({
      
      df <- vb_metrics_reactive_app_logged()
      recent_date = Sys.Date()
      sumTime <- paste0(df$sum_time, " hours")
      statement = paste0("Total time spent on ", input$application," as of 'SAMPLE'")
      
      valueBox(
        paste0(sumTime), sub("SAMPLE",recent_date,statement), icon = icon("fas fa-clock-o", verify_fa = FALSE),
        color = "yellow")
    })
    
    
    
    # value boxes server wide ----
    output$sumUsers = renderValueBox({

      recent_date = Sys.Date()
      sumUsers <- paste0(vb_metrics_reactive()$sum_users, " users")
      statement = "Total unique Users as of 'SAMPLE'"
        
      valueBox(
        paste0(sumUsers), sub("SAMPLE",recent_date,statement), icon = icon("fas fa-users", verify_fa = FALSE), color = "green"
        )
    }) 
    
    output$sumLoggedUsers = renderValueBox({
      
      recent_date = Sys.Date()
      sumLoggedUsers <- paste0(vb_metrics_logged_reactive()$sum_logged_users, " users")
      statement = "Total unique Users as of 'SAMPLE'"
      
      valueBox(
        paste0(sumLoggedUsers), sub("SAMPLE",recent_date,statement), icon = icon("fas fa-users", verify_fa = FALSE), color = "green"
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
    
    output$meanLoggedTime = renderValueBox({
      
      recent_date = Sys.Date()
      meanLoggedTime <- paste0(vb_metrics_logged_reactive()$mean_logged_time, " mins")
      statement = "Average time on Apps as of 'SAMPLE'"
      
      valueBox(
        paste0(meanLoggedTime), sub("SAMPLE",recent_date,statement), icon = icon("fas fa-clock-o", verify_fa = FALSE),
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
    
    output$sumLoggedTime = renderValueBox({
      
      recent_date = Sys.Date()
      sumLoggedTime <- paste0(vb_metrics_logged_reactive()$sum_logged_time, " hours")
      statement = "Total time on Apps as of 'SAMPLE'"
      
      valueBox(
        paste0(sumLoggedTime), sub("SAMPLE",recent_date,statement), icon = icon("fas fa-clock-o", verify_fa = FALSE),
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
    
    output$downloadData3 = downloadHandler(
      filename = function() {
        paste('raw_logged_data_', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write.csv(
          datasets$s3_all_data
          , con)
      }
    )
    
    output$current_usage_table = DT::renderDataTable({
      DT::datatable(datasets$current_usage)
    })
    
    output$current_usage_agg_table = DT::renderDataTable({
      DT::datatable(datasets$current_usage_agg_f)
    })
    
    # logged data
    output$s3_all_data = DT::renderDataTable({
      DT::datatable(datasets$s3_all_data)
    })
    
    # refresh data ----
    observeEvent(input$refresh, {
      showModal(modalDialog(
        div(class = "progress", 
            div(class = "progress-bar progress-bar-striped active", 
                role = "progressbar", 
                style = "width: 100%")
        ), 
        title = "Loading Data ...", 
        size = "s",
        easyClose = FALSE, 
        footer = NULL
      ))
      source("data_new.R")
      datasets$current_usage = current_usage
      datasets$current_usage_agg = current_usage_agg
      datasets$current_usage_tot = current_usage_tot
      datasets$current_usage_agg_f = current_usage_agg_f
      datasets$vb_metrics_app = vb_metrics_app
      datasets$vb_metrics = vb_metrics
      removeModal()
    })
    
    observeEvent(input$refresh_s3, {
      showModal(modalDialog(
        div(class = "progress", 
            div(class = "progress-bar progress-bar-striped active", 
                role = "progressbar", 
                style = "width: 100%")
        ), 
        title = "Loading Data ...", 
        size = "s",
        easyClose = FALSE, 
        footer = NULL
      ))
      source("data_new.R")
      datasets$current_usage = current_usage
      datasets$current_usage_agg = current_usage_agg
      datasets$current_usage_tot = current_usage_tot
      datasets$current_usage_agg_f = current_usage_agg_f
      datasets$vb_metrics_app = vb_metrics_app
      datasets$vb_metrics = vb_metrics
      
      datasets$s3_all_data = s3_all_data
      datasets$vb_metrics_logged = vb_metrics_logged
      datasets$vb_metrics_app_logged = vb_metrics_app_logged
      datasets$current_usage_agg_logged = current_usage_agg_logged
      datasets$events_logged = events_logged
      removeModal()
    })
    
}

shinyApp(ui, server)