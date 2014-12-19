to.define <- function(){

db.attrib <- function(attribute.spec, attribute.type = NULL){
  
}

op.all <- function(){
  
}

op.some <- function(min.number = 1){
  
}

op.none <- function(){
  
}


rule.matcher <- function(){
  
}

## op.all("a", "b", op.some("c", "d"), op.none("e", "f"))



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



