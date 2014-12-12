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



metadata.info <- function(tables, attributes, schema = NULL){
  res <- list(tables = tables, 
              attributes = attributes,
              schema = NULL)
  res$atts.by.name <- attributes %>% dplyr::group_by(attname) %>% dplyr::summarize(tablecount = n())
  res$atts.by.name.type <- attributes %>% dplyr::group_by(attname, typename) %>% dplyr::summarize(tablecount = n())
  attr(res, "data.createdOn") <- Sys.time()
  class(res) <- append(class(res), "metadata.info")
  res
}

attribute.index <- function(x) UseMethod("attribute.index")

attribute.index.default <- function(){
  ## constructor
}

attribute.index.metadata.info <- function(metadata.info){
  ### TODO: write
    #filter-out non-informative attributes
    statDetermined=!is.na(attributesList[,"null_frac"])
    allNull=!(attributesList[,"null_frac"]<1.0)
    someNull=(attributesList[,"null_frac"]>0.0)
    allSame=(attributesList[,"n_distinct"]==1)
    attributeBad=((allSame & !someNull) | allNull)
    attributeOk=(!statDetermined | !attributeBad)
    atttaken=attributesList[attributeOk,]
    tablesTaken=unique(atttaken$tablename)
    attid=paste(atttaken$attname,".",atttaken$typename,sep="")
    att.map=by(atttaken$tablename,attid,function(x){as.character(unique(x))})
    att.counts=unlist(lapply(att.map,length))
    textRep=create.text.representation(atttaken)
    return (list(att.map=att.map,att.counts=att.counts,attid=attid,dtm=textRep$dtm.tfidf,dtm.tf=textRep$dtm.tf))
  
}

#' @export
attribute.instances <- function(x, ...) UseMethod("attribute.instances")


#' List of tables containing one of given attributes
#' 
#' Extracts from an instance of \code{metadata.info} object
#' all occurences of attributes from \code{attribute.names}
#' 
#' @param metadata.info an object of metadata.info class
#' @param attribute.names character vector of attribute names
#' @export
attribute.instances.metadata.info <- function(metadata.info, attribute.names){
  metadata.info$attributes %>% filter(attname %in% attribute.names) %>% select(schemaname, tablename, attname, typename)
}

#' @export
tables.with.attributes <- function(x, ...) UseMethod("tables.with.attributes")


#' List of tables with containing all attributes from given set
#' 
#' Exctracts from an instance of \code{metadata.info} a vector of tables
#' containing all attributes from \code{attribute.names}
#' 
#' @param metadata.info an object of metadata.info class
#' @param attribute.names character vector of attribute names
#' @export
tables.with.attributes.metadata.info <- function(metadata.info, attribute.names){
  attribute.names.unique <- unique(attribute.names)
  att.count <- length(attribute.names.unique)
  res <- metadata.info$attributes %>% 
    filter(attname %in% attribute.names.unique) %>% 
    group_by(schemaname, tablename) %>% 
    summarize(attcount = n()) %>% filter(attcount == att.count) %>% select(schemaname, tablename)
  res
}




#' @export
describe.attribute <- function(x, ...) UseMethod("describe.attribute")

#' @export
describe.attribute.metadata.info <- function(metadata.info, attribute.name){
  
  ## count attributes with given name
  att.instances.count <- metadata.info$attributes %>% dplyr::filter(attname == attribute.name) %>% dplyr::group_by(typename) %>% dplyr::summarize(tablecount = n())
  
  ## similar attributes
  most.sim <- string.get.most.similar(attribute.name, metadata.info$atts.by.name$attname)
  metadata.info$atts.by.name %>% filter(attname %in% most.sim)
  
  
}

#' @export
ambiguous.attributes <- function(x, ...) UseMethod("ambiguous.attributes")

#' Abiguous attributes w.r.t. SQL type
#' 
#' Returns a list of attributes (in the form of data.frame) that provided metadata.info object 
#' are ambiguous w.r.t. the SQL type, that is there exists instances of attribute with the same name and different types.
#' 
#' @param metadata.info the object of \code{metadata.info} class which to use in analysis 
#' @export
ambiguous.attributes.metadata.info <- function(metadata.info){
  att.type.instances <- metadata.info$attributes %>% dplyr::group_by(typename, attname) %>% dplyr::summarize(cnt = 1) 
  att.type.ambiguous <- att.type.instances %>% group_by(attname) %>% summarize(types.used = sum(cnt)) %>% filter(types.used > 1)
  att.type.instances.cnt <- metadata.info$attributes %>% 
    dplyr::filter(attname %in% att.type.ambiguous$attname) %>% 
    dplyr::group_by(attname, typename) %>% 
    dplyr::summarize(tablecount = n()) 
  att.type.instances.cnt
}


