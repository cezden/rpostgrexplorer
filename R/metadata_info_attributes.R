## METHODS OF metadata.info CONCERNING ATTRIBUTES


#' S3 generic dispatcher
#' @export
db.attributes <- function(x, ...) UseMethod("db.attributes")

#' DB attributes
#' 
#' Extracts the DB attributes from \code{\link{metadata.info}} object
#' 
#' Depending on the value of \code{\link{full}} parameter the function returns 
#' \code{data.frame} describing each attribute in \code{\link{metadata.info}} object.
#' 
#' Default (\code{\link{full} = FALSE}) columns of returned object:
#' \itemize{
#'    \item{schemaname}
#'    \item{tablename}
#'    \item{attribute name}
#'    \item{type of attribute}
#' }
#' 
#' More detailed information (\code{\link{full} = TRUE}) includes default columns, and additionally:
#' \itemize{
#'    \item{length of attribute (related to type)}
#'    \item{estimated fraction of NULL values}
#'    \item{estimated number of distinct values (caution!)}
#'    \item{most common values}
#'    \item{frequency of most common values}
#'    \item{histogram bounds}
#' }
#' 
#' 
#' @param metadata.inf object of class \code{\link{metadata.info}} to perform extraction from
#' @param full should full database information be returned?
#' @export
db.attributes.metadata.info <- function(metadata.inf, full = FALSE){
  metadata.inf$attributes %>% dplyr::select(schemaname, tablename, attname, typename, internalid, internalname)
}

#' S3 generic dispatcher
#' @export
db.attributes.counts <- function(x, ...) UseMethod("db.attributes.counts")

#' DB attributes
#' 
#' Extracts the DB attributes from \code{\link{metadata.info}} counting the instances
#' @param metadata.inf object of class \code{\link{metadata.info}} to perform extraction from
#' @param typeinfo should the DB type be included?
#' @export
db.attributes.counts.metadata.info <- function(metadata.inf, typeinfo = FALSE){
  if (typeinfo){
    return(
      metadata.inf$attributes %>% dplyr::group_by(attname, typename) %>% dplyr::summarize(cnt = n()) %>% dplyr::ungroup() %>% dplyr::arrange(desc(cnt))
    )
  }
  metadata.inf$attributes %>% dplyr::group_by(attname) %>% dplyr::summarize(tablecount = n()) %>% dplyr::arrange(desc(tablecount))
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

#' Generic dispatcher
#' @export
attribute.instances <- function(x, ...) UseMethod("attribute.instances")


#' List of tables containing one of given attributes
#' 
#' Extracts from an instance of \code{\link{metadata.info}} object
#' all occurences of attributes from \code{attribute.names}
#' 
#' @param metadata.info an object of \code{\link{metadata.info}} class
#' @param attribute.names character vector of attribute names
#' @param fixed should the attribute names be treated as fixed strings (default) or regular expressions?
#' @param partial should the attribute names be matched partially (e.g. attribute \code{"attrib123"} will match query \code{"attrib12"}) or fully?
#' @export
attribute.instances.metadata.info <- function(metadata.info, attribute.names, fixed = TRUE, partial = FALSE){
  if (fixed && !partial){
    return(
      metadata.info$attributes %>% 
        dplyr::filter(attname %in% attribute.names) %>% 
        dplyr::select(schemaname, tablename, attname, typename, internalid, internalname)
    )
  }
  stop("Not implemented")
  ##stringi::stri_detect_fixed(c("stringi R"), c('in', 'R', '0'))
  ## TODO: write
}



#' Attribute description generic dispatcher
#' @export
describe.attribute <- function(x, ...) UseMethod("describe.attribute")

#' Attribute description
#' @param metadata.info an object of \code{\link{metadata.info}} class
#' @param attribute.name the attribute name to describe
#' @export
describe.attribute.metadata.info <- function(metadata.info, attribute.name){
  
  ## count attributes with given name
  att.instances.count <- metadata.info$attributes %>% 
    dplyr::filter(attname == attribute.name) %>% 
    dplyr::group_by(attname, typename) %>% 
    dplyr::summarize(tablecount = n()) %>% dplyr::ungroup() %>% dplyr::arrange(desc(tablecount))
  
  ## similar attributes
  most.sim <- string.get.most.similar(attribute.name, metadata.info$atts.by.name$attname)
  similar <- metadata.info$atts.by.name %>% dplyr::filter(attname %in% most.sim)
  
  list(instances.count = att.instances.count, similar = similar)
}

#' @export
ambiguous.attributes <- function(x, ...) UseMethod("ambiguous.attributes")

#' Abiguous attributes w.r.t. SQL type
#' 
#' Returns a list of attributes (in the form of data.frame) that provided metadata.info object 
#' are ambiguous w.r.t. the SQL type, that is there exists instances of attribute with the same name and different types.
#' 
#' @param metadata.info the object of \code{\link{metadata.info}} class which to use in analysis 
#' @export
ambiguous.attributes.metadata.info <- function(metadata.info){
  att.type.instances <- metadata.info$attributes %>% dplyr::group_by(typename, attname) %>% dplyr::summarize(cnt = 1) 
  att.type.ambiguous <- 
    att.type.instances %>% 
    dplyr::group_by(attname) %>% 
    dplyr::summarize(types.used = sum(cnt)) %>% 
    dplyr::filter(types.used > 1)
  att.type.instances.cnt <- metadata.info$attributes %>% 
    dplyr::filter(attname %in% att.type.ambiguous$attname) %>% 
    dplyr::group_by(attname, typename) %>% 
    dplyr::summarize(tablecount = n()) 
  att.type.instances.cnt
}

