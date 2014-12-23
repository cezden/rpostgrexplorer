## METHODS OF metadata.info CONCERNING TABLES


#' S3 generic dispatcher
#' @export
db.tables <- function(x, ...) UseMethod("db.tables")

#' DB tables
#' 
#' Extracts the DB tables from \code{\link{metadata.info}} object
#' @param metadata.inf object of class \code{\link{metadata.info}} to perform extraction from
#' @export
db.tables.metadata.info <- function(metadata.inf){
  metadata.inf$tables %>% dplyr::select(schemaname, tablename, count_estimate, has_index, has_primary_key, internalid, internalname)
}


#' Attribute description generic dispatcher
#' @export
describe.table <- function(x, ...) UseMethod("describe.table")

#' Table description
#' 
#' @param metadata.inf an object of \code{\link{metadata.info}} class
#' @param table.name the table name to describe
#' @export
describe.table.metadata.info <- function(metadata.inf, table.name){
  ## TODO: present table statistics (if present)
  ## TODO: restrict attribute list
  
  ## count attributes with given table name
  tabatts <- metadata.inf$attributes %>% 
    dplyr::filter(tablename == table.name) %>% 
    dplyr::arrange(schemaname, tablename, colposition)
  tabatts
}





#' @export
tables.with.attributes <- function(x, ...) UseMethod("tables.with.attributes")


#' List of tables with containing all attributes from given set
#' 
#' Extracts from an instance of \code{metadata.info} a vector of tables
#' containing all attributes from \code{attribute.names}
#' 
#' @param metadata.info an object of metadata.info class
#' @param attribute.names character vector of attribute names
#' @export
tables.with.attributes.metadata.info <- function(metadata.info, attribute.names){
  attribute.names.unique <- unique(attribute.names)
  att.count <- length(attribute.names.unique)
  res <- metadata.info$attributes %>% 
    dplyr::filter(attname %in% attribute.names.unique) %>% 
    dplyr::group_by(schemaname, tablename) %>% 
    dplyr::summarize(attcount = n()) %>% dplyr::filter(attcount == att.count) %>% dplyr::select(schemaname, tablename)
  res
}


