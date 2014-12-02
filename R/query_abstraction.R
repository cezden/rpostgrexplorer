#' Query execution
#' 
#' Executes the list of queries providing a list of dataframes equipped with the attributes:
#' \itemize{
#'  \item{data.sqlQuery (the query used for constructing the dataframe)}
#'  \item{data.retirevedOn (the time the query returned values)}
#' }
#' @param queries list of queries to execute
#' @param control.connection the control.connection object
#' @param verbose should additional information be printed out
#' @export
query.load.execute <- function(queries, control.connection, verbose = TRUE) {
  if(is.null(control.connection)) {
    stop("No connection parameters provided")
  }
  drv <- dbDriver("PostgreSQL")
  con <- do.call("dbConnect", c(drv, control.connection))
  queries.nonempty <- 
    Filter(function(x){
            nchar(x)>1 ##TODO: trim before checking
          },queries)
  data <- tryCatch(
    {
      data <- lapply(queries.nonempty,function(query){
        if (verbose) cat(query,"\n")
        queryRes <- dbGetQuery(con, query)
        attr(queryRes, "data.sqlQuery") <- query
        attr(queryRes, "data.retirevedOn") <- Sys.time()
        queryRes
      })
      data
    },
    finally = {
      dbDisconnect(con)
      dbUnloadDriver(drv)  
    }
  ) 
  data  
}  


