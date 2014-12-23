#' Query execution
#' 
#' Executes the list of queries providing a list of dataframes equipped with the attributes:
#' \itemize{
#'  \item{data.sqlQuery (the query used for constructing the dataframe)}
#'  \item{data.retirevedOn (the time the query returned values)}
#' }
#' @param queries list of queries to execute
#' @param control.connection the object of \code{db.connection} class
#' @param verbose should additional information be printed out
#' @export
query.load.execute <- function(queries, control.connection, verbose = TRUE) {
  if(is.null(control.connection)) {
    stop("No connection parameters provided")
  }
  library(RPostgreSQL)  ## TODO: should it be here?
  drv <- DBI::dbDriver("PostgreSQL")
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

query.flatten.results <- function(query.load.execute.results, idfield.proposed = "query.flatten.results.id"){
  qnames <- names(query.load.execute.results)
  if (length(qnames) == 0){
    return(list(results = data.frame(), idfield = "NULL"))
  }
  #checking if there is a name collision
  qcols <- names(query.load.execute.results[[qnames[1]]])
  idfield <- idfield.proposed
  while(idfield %in% qcols){
    idfield <- paste0(idfield, "1")
  }
  results.df <- NULL
  for (res.it in qnames){
    result <- data.frame(query.load.execute.results[[res.it]], stringsAsFactors = FALSE)
    result[,idfield] <- res.it
    results.df <- rbind(results.df, result)
  }
  list(results = results.df, idfield = idfield)
}
