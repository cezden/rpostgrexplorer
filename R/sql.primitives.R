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
