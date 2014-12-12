#' DB connection specification
#' 
#' @examples
#' control.connection <- db.connection(
#'  user = "dbuser", 
#'  host = "dbhost", 
#'  port = 0000, 
#'  dbname = "dbname", 
#'  password = "dbpass"
#'  )  
#' @export
db.connection <- function(...){
  z <- list(...)
  class(z) <- "db.connection" 
  z
}

#' Metadata query
#' 
#' Loads the attributes list and table list
#' @param control.connection the control.connection object
#' @export
load.metadata <- function(control.connection, schemaname = NULL){
  ## TODO: cleanup of documentation
  ## TODO: multiple schemas
  
  queries <- list(tablesList = sql.tables(schemaname),
               attributesList = sql.attributes(schemaname),
               "" # final, closing query (not obligatory, just to simplify notation)
  )
  res <- query.load.execute(queries, control.connection)
  schema.descr <- ""
  
  if (!is.null(schemaname)) schema.descr <- paste0(" from schema ",schemaname)
  
  cat(paste0("Loaded ", nrow(res$tablesList), " table(s)", schema.descr, "\n"))
  cat(paste0("Loaded ", nrow(res$attributesList), " attribute(s)", schema.descr, "\n"))
  
  metadata.info(tables = res$tablesList, 
                attributes = res$attributesList,
                schema = schemaname
                )
}
