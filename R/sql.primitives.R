#' SQL formation: combining schema name with table
#' 
#' For each element of \code{tab.name} the corresponding element from \code{schema.name} 
#' is used for forming the valid table reference in format \code{schema.table}.
#' If the corresponding element of the \code{schema.name} is \code{NA} or empty (consisting of white chars only)
#' the table reference is simply \code{table}.
#' 
#' @param tab.name (\code{character} or \code{character} vector) name(s) of the tables
#' @param schema.name (\code{character} or \code{character} vector) name(s) of the schema(s)
sql.table.schemed <- function(tab.name, schema.name){
  
  schemaselector <- unlist(lapply(schema.name, function(x){
    if (is.na(x) || stringi::stri_length(stringi::stri_trim_both(x))==0){
      ""
    } else {
      paste0(x, ".")
    }
  }))
  
  paste0(schemaselector, tab.name)
}



#' Generic template filler (internal)
#' @param template.name the name of the template
#' @param query.names the vector of names of the generated queries (in the genereated query list)
#' @param param.list the (named) list with names of parameters to be replaced and the values to be substituted
#' @examples
#'  sql.fill.template(
#'    "dictionary_values", 
#'    query.names="q1", 
#'    list("%%DICTKEY%%" = "abd", "%%QUERY%%" = "bca")
#'    )
#'    
#'  sql.fill.template(
#'    "dictionary_values", 
#'    query.names = c("q1","q2"), 
#'    list("%%QUERY%%" = c("bca", "bcb"), "%%DICTKEY%%" = c("abd"))
#'    )
#'    
#'  sql.fill.template(
#'    "dictionary_values", 
#'    query.names = c("q1", "q2"), 
#'    list("%%QUERY%%" = c("bca", "bcb"), "%%DICTKEY%%"=c("abc", "abd"))
#'    )
sql.fill.template <- function(template.name, query.names, param.list){
  qlist <- get("QUERY_REPO", pkg_globals)
  sql.template.fill(qlist, template.name, query.names, param.list)
}



sql.select.table <- function(table.df){
  paste0("select * from ", sql.table.schemed(tab.name = table.df$tablename, schema.name = table.df$schemaname))
}
