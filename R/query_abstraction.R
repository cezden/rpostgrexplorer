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
  queries.nonempty <- 
    Filter(function(x){
            nchar(x)>1 ##TODO: trim before checking
          },queries)
  library(RPostgreSQL)  ## TODO: should it be here?
  drv <- DBI::dbDriver("PostgreSQL")
  con <- do.call("dbConnect", c(drv, control.connection))
  data <- tryCatch(
    {
      data <- lapply(queries.nonempty, function(query){
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
      connList <- DBI::dbListConnections(drv)
      if (length(connList)==0){
        dbUnloadDriver(drv)  
      } else {
        warning("Can't unload driver - some unknown connections are open")
      }
    }
  ) 
  data  
}  

#' Flattens results of the query.load.execute to a single data frame
#' 
#' Flattens results of the query.load.execute to a single data frame, if results do match the same structure (column names)
#' 
#' 
#' @param query.load.execute.results the results to be processed
#' @param idfield.proposed the proposed name of the column storing the row name (extracted from \code{query.load.execute.results}) 
#' @return \code{list} with following fields:
#'  \itemize{
#'    \item{\code{results}} the resulting data.frame
#'    \item{\code{idfield}} the name of the column in \code{results} preserving the list name of the query (may differ from \code{idfield.proposed} in case of name collision)
#'  }
#'  @export
query.flatten.results <- function(query.load.execute.results, idfield.proposed = "query.flatten.results.id"){
  qnames <- names(query.load.execute.results)
  if (length(qnames) == 0){
    return(list(results = data.frame(), idfield = NULL))
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
