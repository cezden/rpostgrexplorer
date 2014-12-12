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
  attr(res, "data.createdOn") <- Sys.time()
  class(res) <- append(class(res), "metadata.info")
  res
}

attribute.index <- function(x) UseMethod("attribute.index")

attribute.index.default <- function(){
  
}

attribute.index.metadata.info <- function(metadata.info){
  
    #filter-out non-informative attributes
    statDetermined=!is.na(attributesList[,"null_frac"])
    allNull=!(attributesList[,"null_frac"]<1.0)
    someNull=(attributesList[,"null_frac"]>0.0)
    allSame=(attributesList[,"n_distinct"]==1)
    attributeBad=((allSame & !someNull) | allNull)
    attributeOk=(!statDetermined | !attributeBad)
    atttaken=attributesList[attributeOk,]
    tablesTaken=unique(atttaken$tablename)
    attid=paste(atttaken$attname,".",atttaken$typname,sep="")
    att.map=by(atttaken$tablename,attid,function(x){as.character(unique(x))})
    att.counts=unlist(lapply(att.map,length))
    textRep=create.text.representation(atttaken)
    return (list(att.map=att.map,att.counts=att.counts,attid=attid,dtm=textRep$dtm.tfidf,dtm.tf=textRep$dtm.tf))
  
}
