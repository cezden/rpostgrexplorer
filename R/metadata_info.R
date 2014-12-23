#' Table list and simple metadata extraction
#' 
#' Constructs the query retrieving the metadata information concerning the tables:
#' \itemize{
#'    \item{schemaname}
#'    \item{tablename}
#'    \item{number of attributes}
#'    \item{estimate of counts}
#'    \item{is table indexed?}
#'    \item{has table a primary key?}
#' } 
#' @param schemaname name of specific schema, if not provided tables from all schemas will be returned
#' @return SQL query retrieving the metadata information concerning the tables
sql.tables <- function(schemaname = NULL){
  schema.selector <- ""
  if (!is.null(schemaname)){
    schema.selector <- paste0("and t.schemaname='", schemaname, "'")
  }
  stringi::stri_replace_all_fixed(
    get("tables_list", pkg_globals), 
    c("%%SCHEMA_SELECTOR%%"), c(schema.selector),
    vectorize_all = FALSE)  
}


#' Table list and simple metadata extraction
#' 
#' Constructs the query retrieving the metadata information of each attribute in each table as well as statistics computed by PostreSQL:
#' \itemize{
#'    \item{schemaname}
#'    \item{tablename}
#'    \item{attribute name}
#'    \item{type of attribute}
#'    \item{length of attribute (related to type)}
#'    \item{estimated fraction of NULL values}
#'    \item{estimated number of distinct values (caution!)}
#'    \item{most common values}
#'    \item{frequency of most common values}
#'    \item{histogram bounds}
#' }
#' @param schemaname name of specific schema, if not provided tables from all schemas will be returned
sql.attributes <- function(schemaname = NULL){
  schema.selector <- ""
  if (!is.null(schemaname)){
    schema.selector <- paste0("where a.schemaname='", schemaname, "'")
  } 
  stringi::stri_replace_all_fixed(
    get("table_attributes_list", pkg_globals), 
    c("%%WHERE_SCHEMA_SELECTOR%%"), c(schema.selector),
    vectorize_all = FALSE)    
}


sql.objects <- function(schemaname = NULL){
  schema.selector <- ""
  if (!is.null(schemaname)){
    schema.selector <- paste0("and n.nspname='", schemaname, "'")
  } 
  stringi::stri_replace_all_fixed(
    get("objects_list", pkg_globals), 
    c("%%SCHEMA_SELECTOR%%"), c(schema.selector),
    vectorize_all = FALSE)    
}


#' Metadata S3 class
#' 
#' @param tables result of \code{\link{sql.tables}} query
#' @param attributes result of \code{\link{sql.attributes}} query
#' @param schema the schema(s) restricting the queries
#' @export
metadata.info <- function(tables, attributes, schema = NULL){
  res <- list(tables = tables, 
              attributes = attributes,
              schema = schema)
  res$atts.by.name <- attributes %>% dplyr::group_by(attname) %>% dplyr::summarize(tablecount = n())
  res$atts.by.name.type <- attributes %>% dplyr::group_by(attname, typename) %>% dplyr::summarize(tablecount = n())
  attr(res, "data.createdOn") <- Sys.time()
  class(res) <- append(class(res), "metadata.info")
  res
}

#' @export
print.metadata.info <- function(metadata.inf){
  cat(paste0("metadata.info object created ", attr(metadata.inf, "data.createdOn"), "\n" ))
  cat(paste0("\t ", nrow(metadata.inf$tables), " table(s)", "\n"))
  cat(paste0("\t ", nrow(metadata.inf$attributes), " attribute(s)", "\n"))
  
}

