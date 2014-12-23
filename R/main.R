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
#' @param control.connection the \code{\link{db.connection}} object
#' @return instance of \code{\link{metadata.info}} class
#' @export
load.metadata <- function(control.connection, schemaname = NULL){
  ## TODO: cleanup of documentation
  ## TODO: multiple schemas
  
  queries <- list(
               db.engine.version = "select version();",
               tablesList = sql.tables(schemaname),
               attributesList = sql.attributes(schemaname),
               "" # final, closing query (not obligatory, just to simplify notation)
  )
  res <- query.load.execute(queries, control.connection)
  schema.descr <- ""
  
  if (!is.null(schemaname)) schema.descr <- paste0(" from schema ",schemaname)
  
  cat(paste0("DB engine version: ", res$db.engine.version,"\n"))
  cat(paste0("Loaded ", nrow(res$tablesList), " table(s)", schema.descr, "\n"))
  cat(paste0("Loaded ", nrow(res$attributesList), " attribute(s)", schema.descr, "\n"))
  
  metadata.info(tables = res$tablesList, 
                attributes = res$attributesList,
                schema = schemaname
                )
}

anonymize.str <- function(salt, value, salt2 = salt){
  vals <- paste0(salt, "%irplvk/fsfio390258052%", value, salt2)
  unlist(lapply(vals, function(x) {
    digest::digest(x, algo="md5", serialize = FALSE, file = FALSE, length = Inf, skip = 0, ascii = FALSE, raw = FALSE)
  }))
}


#' Metadata anonymizing query
#' 
#' Loads the attributes list, table list and make anonymization
#' @param control.connection the \code{\link{db.connection}} object
#' @return instance of \code{\link{metadata.info}} class
#' @export
load.metadata.anonymized <- function(control.connection, schemaname = NULL){
  ## TODO: cleanup of documentation
  ## TODO: multiple schemas
  
  queries <- list(
    db.engine.version = "select version();",
    tablesList = sql.tables(schemaname),
    attributesList = sql.attributes(schemaname),
    "" # final, closing query (not obligatory, just to simplify notation)
  )
  res <- query.load.execute(queries, control.connection)
  schema.descr <- ""
  
  if (!is.null(schemaname)) schema.descr <- paste0(" from schema ",schemaname)
  
  cat(paste0("DB engine version: ", res$db.engine.version,"\n"))
  cat(paste0("Loaded ", nrow(res$tablesList), " table(s)", schema.descr, "\n"))
  cat(paste0("Loaded ", nrow(res$attributesList), " attribute(s)", schema.descr, "\n"))
  
  ## schemas
  
  salt <- paste(runif(20), collapse="")  
  salt2 <- paste(runif(20), collapse="")  

  schemaname = anonymize.str(salt, schemaname, salt2)
  
  res$tablesList <- res$tablesList %>% 
    mutate(
      schemaname = anonymize.str(salt, schemaname, salt2))  

  res$attributesList <- res$attributesList %>% 
    mutate(
      schemaname = anonymize.str(salt, schemaname, salt2))  
  
  
  ## tables
  salt <- paste(runif(20), collapse="")  
  salt2 <- paste(runif(20), collapse="")  
  res$tablesList <- res$tablesList %>% 
    mutate(
      tablename = anonymize.str(salt, tablename, salt2))
  
  res$attributesList <- res$attributesList %>% 
    mutate(
      tablename = anonymize.str(salt, tablename, salt2))  

  ## attributes
  salt <- paste(runif(20), collapse="")  
  salt2 <- paste(runif(20), collapse="")  
  res$attributesList <- res$attributesList %>% 
    mutate(
      attname = anonymize.str(salt, attname, salt2))  
  ## clearing frequent vals (most_common_vals, most_common_freqs) and histogram bounds (histogram_bounds)
  #stop("frequent vals (most_common_vals, most_common_freqs) and histogram bounds (histogram_bounds) not cleared")  
  res$attributesList$most_common_vals[!is.na(res$attributesList$most_common_vals)] <- "{...anonymized...}"
  res$attributesList$histogram_bounds[!is.na(res$attributesList$histogram_bounds)] <- "{...anonymized...}"
  zz <- metadata.info(tables = res$tablesList, 
                attributes = res$attributesList,
                schema = schemaname
  )
  attr(zz, "data.createdOn") <- as.POSIXct(strptime("2014-12-12 12:12:12", "%Y-%m-%d %H:%M:%S"))
  zz
}


