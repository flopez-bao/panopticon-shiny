apiKey <- Sys.getenv("API_KEY")
connectServer <- Sys.getenv("CONNECT_SERVER")

print("Fetching data...")

# get general content
df_gc <- getData(
  connectServer = connectServer,
  apiKey = apiKey,
  endpoint = "__api__/v1/content",
  dataframe = T
)
setDT(df_gc)
df_gc <- df_gc[!grepl("test|TEST|testing|Testing|panopticon|Test|Triage|Dev|DEV|TRIAGE", title)]


# get usage stats
# usage <- getData(
#   connectServer = connectServer,
#   apiKey = apiKey,
#   endpoint = "__api__/v1/instrumentation/shiny/usage?asc_order=false&limit=500",
#   dataframe = F
# )
# df_usage <- listToDf(usage$results)
# setDT(df_usage)
df_usage <- unique(df_gc$guid) %>%
  lapply(function(guid) {
    
    usage_list <- 
      getData(
        connectServer = connectServer,
        apiKey = apiKey,
        endpoint = paste0("__api__/v1/instrumentation/shiny/usage?content_guid=",guid,"&asc_order=false&from=2022-01-01T18:00:00Z&limit=500"),
        dataframe = F
      )
    #print(length(usage_list$results))
    
    if(length(usage_list$results) > 0) {
      df <- listToDf(usage_list$results)
    }
    
  }) %>% data.table::rbindlist()

# df_usage <- listToDf(usage$results)
setDT(df_usage)


current_usage <- merge(
  df_usage,
  df_gc,
  by.x = "content_guid",
  by.y = "guid"
)

# convert time stamps 
current_usage[,ended:=fastPOSIXct(ended)][
  ,started:=fastPOSIXct(started)]

# calculate session time
current_usage[
  ,sess_time:=as.numeric(round(difftime(ended, started, units = "mins"), 2))][
    ,year_month:=substr(started,1,7)]

# if the time difference is greater than or equal to 30 seconds assume that
# reconnect timeout is acting so account for 15 second reconnect timeout for ended https://docs.rstudio.com/connect/api/#get-/v1/instrumentation/shiny/usage
current_usage[sess_time > 30
              ,sess_time_adj:=sess_time - 15][is.na(sess_time_adj)
                                              ,sess_time_adj:=sess_time]


# extract names of apps that have had less than 10 visits/sessions in the last 12 months and get rid of them 
current_usage[,total_12_month_sess_count:=.N,.(title)]

current_usage <- current_usage[total_12_month_sess_count > 9]

# agg stats
current_usage_agg <- 
  current_usage[
    ,.(num_users = uniqueN(user_guid),
       num_visits = .N,
       mean_sess_time = round(mean(sess_time),1),
       sum_sess_time = sum(sess_time)
    )
    ,.(app = title,
       year_month
    )
  ] 



# agg denom
current_usage_tot <- 
  current_usage[
    ,.(tot_users = uniqueN(user_guid),
       tot_visits = .N,
       tot_sess_time = sum(sess_time, na.rm = T)
    )
    ,.(
      year_month
    )
  ]

# merge for final metrics table
current_usage_agg_f <- 
  merge(
    current_usage_agg,
    current_usage_tot,
    by = "year_month"
  )

# add percent columns
current_usage_agg_f[
    ,perc_num_visits:=round(num_visits/tot_visits * 100, 1)][
      ,perc_sess_time:=round(sum_sess_time/tot_sess_time * 100, 1)]

setorder(current_usage_agg, app, year_month)

# value box stats app 
vb_metrics_app <- current_usage[,
                    .(num_users= uniqueN(user_guid),
                      mean_time = round(mean(sess_time_adj, na.rm = T),1),
                      sum_time_hr = round(sum(sess_time_adj, na.rm = T)/60,1)
                        )
                    ,.(app = title)]
setDT(vb_metrics_app)

# server wide value boxes
sum_users <- length(unique(current_usage$user_guid))
mean_time <- round(mean(current_usage$sess_time_adj, na.rm = T),1)
sum_time_hr <- round(sum(current_usage$sess_time_adj, na.rm = T)/60,1)

vb_metrics <- data.frame(sum_users, mean_time, sum_time_hr)

current_usage <-  as.data.frame(current_usage)
current_usage_agg <- as.data.frame(current_usage_agg)
current_usage_tot <-  as.data.frame(current_usage_tot)
current_usage_agg_f <-  as.data.frame(current_usage_agg_f)
vb_metrics_app <- as.data.frame(vb_metrics_app)
vb_metrics <- as.data.frame(vb_metrics)

print("data fetch complete!")







