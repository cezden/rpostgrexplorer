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
    schema.selector.int <- paste0("t.schemaname='", schemaname, "'", collapse=" or ")
    schema.selector <- paste0("and (", schema.selector.int, ")")
  }
  sql.fill.template("tables_list", query.names = NULL, list("%%SCHEMA_SELECTOR%%" = schema.selector))[[1]]
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
    schema.selector.int <- paste0("a.schemaname='", schemaname, "'", collapse=" or ")
    schema.selector <- paste0("where (", schema.selector.int, ")")
  } 
  sql.fill.template("table_attributes_list", query.names = NULL, list("%%WHERE_SCHEMA_SELECTOR%%" = schema.selector))[[1]]
}


sql.objects <- function(schemaname = NULL){
  schema.selector <- ""
  if (!is.null(schemaname)){
    schema.selector.int <- paste0("n.nspname='", schemaname, "'", collapse=" or ")
    schema.selector <- paste0("and (", schema.selector.int, ")")
  } 
  sql.fill.template("objects_list", query.names = NULL, list("%%SCHEMA_SELECTOR%%" = schema.selector))[[1]]
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
  #internal attributes
  res$tables$internalid <- anonymize.str(salt = res$tables$schemaname, value = "-", salt2 = res$tables$tablename)
  if (length(res$tables$internalid)>length(unique(res$tables$internalid))){
    stop("collision during generating internal IDs for tables")
  }
  res$tables$internalname <- sql.table.schemed(tab.name = res$tables$tablename, schema.name = res$tables$schemaname)

  res$attributes$internalid <- anonymize.str(salt = res$attributes$schemaname, value = res$attributes$tablename, salt2 = res$attributes$attname)
  if (length(res$attributes$internalid)>length(unique(res$attributes$internalid))){
    stop("collision during generating internal IDs for attributes")
  }
  res$attributes$internalname <- paste0(
    sql.table.schemed(tab.name = res$attributes$tablename, schema.name = res$attributes$schemaname), 
    ".", 
    res$attributes$attname
    )
  class(res) <- append(class(res), "metadata.info")
  res
}

#' @export
print.metadata.info <- function(metadata.inf){
  cat(paste0("metadata.info object created ", attr(metadata.inf, "data.createdOn"), "\n" ))
  cat(paste0("\t ", nrow(metadata.inf$tables), " table(s)", "\n"))
  cat(paste0("\t ", nrow(metadata.inf$attributes), " attribute(s)", "\n"))
  
}


#' Generic dispatcher
#' @export
restrict <- function(x, ...) UseMethod("restrict")


#' Filtering the metadata.info
#' 
#' The function provides a mechanism for restricting the \code{\link{metadata.info}} object.
#' 
#' It can be provided with:
#' \itemize{
#'  \item{subset of rows or columns from the result of \code{\link{db.attributes}}} function
#'  \item{subset of rows or columns from the result of \code{\link{db.tables}}} function
#' }
#' As a result new  \code{\link{metadata.info}} object is created matching the filtering criteria
#' 
#' @param meta.inf the \code{\link{metadata.info}} object to be restricted
#' @param restr.obj description of the restriction
#' @return restricted \code{\link{metadata.info}} object
#' @examples
#' db.atts <- db.attributes(meta.inf)
#' # let's select attributes with more than 100 instances
#' db.atts.cnt <- db.attributes.counts(meta.inf)  %>% dplyr::filter(tablecount>100)
#' meta.inf.restr <- restrict(meta.inf, db.atts.cnt)
#' 
#' @export
restrict.metadata.info <- function(meta.inf, restr.obj){
  # ugly dispatch mechanism
  if (is(restr.obj, "data.frame") ){
    #what kind of restriction?
    attr.desc.full <- c("attname", "typename")
    attr.desc <- c("attname")
    table.desc <- c("schemaname", "tablename")
    
    restr.obj.names <- names(restr.obj)
    
    table.res <- all(table.desc %in% restr.obj.names)
    attr.res <- all(attr.desc %in% restr.obj.names)
    attr.res.full <- all(attr.desc.full %in% restr.obj.names)
    if (!(table.res || attr.res || attr.res.full)){
      stop("Invalid column names of the restricting argument")
    }
    
    
    db.attribs <- db.attributes(meta.inf)
    db.tabs <- db.tables(meta.inf)
    
    if (length(intersect(restr.obj.names, names(db.attribs)))>0){
      db.attribs <- dplyr::semi_join(db.attribs, restr.obj)
    }
    if (length(intersect(restr.obj.names, names(db.tabs)))>0){
      db.tabs <- dplyr::semi_join(db.tabs, restr.obj)
    }
    # we got an upper-bound on the set: 
    ## there may be some tables removed from db.attribs due to additional fields (need to update db.tabs) 
    ## there may be some tables removed from db.tabs due to additional fields (need to update db.attribs) 
    ## we need 2 additional semi_joins:
    db.attribs <- dplyr::semi_join(db.attribs, db.tabs)
    db.tabs <- dplyr::semi_join(db.tabs, db.attribs)    
    return(metadata.info(tables = db.tabs, attributes = db.attribs))
    
    ## apply restrictions
    #if (table.res){
    #  table.vals <- unique(restr.obj[, table.desc])
    #  
    #  
    #}
  }
  stop(paste("Cannot match restricting argument of class:", paste(class(restr.obj), collapse = ",")))
  
}


