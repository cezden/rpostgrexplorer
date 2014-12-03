#' DB connection specification
#' @example
#' control.connection <- db.connection(user="dbuser", host="dbhost", port=0000, dbname="dbname", password="dbpass")  
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
  
  queries <- list(tablesList = get.tables.query(schemaname),
               attributesList = get.attributes.query(schemaname),
               "" # final, closing query (not obligatory, just to simplify notation)
  )
  return (query.load.execute(queries, control.connection))
}
