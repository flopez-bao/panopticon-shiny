# convert list to dataframe
listToDf <- function(my_list) {
  
  df_list <- lapply(1:length(my_list), function(y) {
    
    my_list <- my_list[[y]]
    
    my_list <- 
      lapply(my_list, function(y) {
        if (is.null(y)) {
          y = 0
        } else {
          y
        }
      })
    
    res <- as.data.frame(my_list)
    
    return(res)
  })
  
  df <- do.call(rbind, df_list)
  
  return(df)
}

# call endpoint data
getData <- function(connectServer, apiKey, endpoint, dataframe = T) {
  
  url <- paste0(
    connectServer,
    endpoint
  )
  
  res <- httr::GET(
    url,
    httr::add_headers(Authorization = paste("Key", apiKey))
  )
  
  res_get <- httr::content(res)
  
  if(dataframe == T) {
    res_f <- listToDf(res_get)
  } else {
    res_f <- res_get
  }
  
  return( res_f )
  
}
# 
# # get general content
# getData(
#   connectServer = connectServer,
#   apiKey = apiKey,
#   endpoint = "__api__/v1/content",
#   dataframe = T
# )
# 
# # get usage stats
# m <- 
# getData(
#   connectServer = connectServer,
#   apiKey = apiKey,
#   endpoint = "__api__/v1/instrumentation/shiny/usage?asc_order=false&limit=500&from=2022-08-15T18:00:00Z",
#   dataframe = F
# )
# 
# m_df <- listToDf(m$results)
# 
# 
# 
# 
# 
# 
# ?from=2022-08-15T18:00:00Z