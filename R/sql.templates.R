#' Generic SQL template filler 
#' @param template.repo repository of templates
#' @param template.name the name of the template
#' @param query.names the vector of names of the generated queries (in the genereated query list)
#' @param param.list the (named) list with names of parameters to be replaced and the values to be substituted
#' @export
#' @examples
#'  sql.template.fill(
#'    some.repo,
#'    "dictionary_values", 
#'    query.names="q1", 
#'    list("%%DICTKEY%%" = "abd", "%%QUERY%%" = "bca")
#'    )
#'    
#'  sql.template.fill(
#'    some.repo,
#'    "dictionary_values", 
#'    query.names = c("q1","q2"), 
#'    list("%%QUERY%%" = c("bca", "bcb"), "%%DICTKEY%%" = c("abd"))
#'    )
#'    
#'  sql.template.fill(
#'    some.repo,
#'    "dictionary_values", 
#'    query.names = c("q1", "q2"), 
#'    list("%%QUERY%%" = c("bca", "bcb"), "%%DICTKEY%%"=c("abc", "abd"))
#'    )
sql.template.fill <- function(template.repo, template.name, query.names, param.list){
  template.str <- template.repo[[template.name]]
  param.names <- names(param.list)
  single.replacer <- function(...){
    stringi::stri_replace_all_fixed(
      template.str, 
      param.names, ### silently assuming that ... preserves the order of param.names
      c(...),
      vectorize_all = FALSE)  
  }
  if(length(param.list)>0){
    #parametrized
    zz <- do.call("mapply", c(list(FUN=single.replacer), param.list, list(SIMPLIFY=FALSE, USE.NAMES=FALSE)))
    names(zz) <- query.names
  } else {
    #non-parametrised, just return the query
    zz <- list(template.str)
    names(zz) <- query.names
  }
  zz
}

#' SQL template loader
#' @export
sql.template.load <- function(dirs, recursive = TRUE){
  outl <- list()
  for (dirname in dirs){
    tmp <- list.files(path = dirname, pattern = "*.sql", recursive = recursive, include.dirs = TRUE, full.name = TRUE)
    for(ttmp in tmp){
      cat(paste0("Loading ",ttmp,"\n"))
      fname <- basename(ttmp)
      dirname <- dirname(ttmp)
      ## fname ends with .sql, let's strip it
      qname <- stringi::stri_sub(fname, from = 1, length = nchar(fname)-4)
      if (qname %in% names(outl)){
        warning(paste0("The query [",qname,"] already present in the set, ignoring"))
      }
      else{
        sqlfile.contents <- readLines(ttmp)    #read sql file content
        #remove comments
        sqlfile.contents.filtered <- Filter(
          function(x){
            !(stringi::stri_startswith_fixed(stringi::stri_trim_left(x), "--"))
          }
          , sqlfile.contents)
        final.sql <- paste(sqlfile.contents.filtered, collapse = "\n")
        outl[[qname]] <- final.sql
      }
    }
  }
  outl
}

pimko <- function(){
  #### TESTING, TESTING
  pimko2 <- sql.template.load("~/rpostgrexplorer/inst/sql") ## use dir full name 
  sql.template.fill(pimko2, "tables_list", query.names=NULL, param.list = list("%%SCHEMA_SELECTOR%%" = ''))
  sql.template.fill(pimko2, "tables_list", query.names=NULL, param.list = list())
  
}
