
#' @export
experimental.db.dictionary <- function(df.db.table, v.key.field, v.value.field){
  list(db.tables = df.db.table, db.key.cols = v.key.field, db.val.cols = v.value.field)
}

#' @export
experimental.db.dictionary.tab <- function(df.db.table, key.field, value.field){
  z <- list(db.table = df.db.table, key.cols = key.field, val.cols = value.field, keys.v = NA)    
  class(z) <- "db.dictionary.tab"
  z
}


#' @export
db.query.value <- function(x, ...) UseMethod("db.query.value")


query.dictionary.tab.value.generic <- function(dictkey, query){
  stringi::stri_replace_all_fixed(
    get("dictionary_values", pkg_globals), 
    c(
      "%%DICTKEY%%", "%%QUERY%%"
    ), 
    c(
      dictkey, query
    ),
    vectorize_all = FALSE)    
}


#' @export
db.query.value.db.dictionary.tab <- function(db.dictionary.obj){
  
  
  db.dictionary.obj$keys.v <- 
  "select %%KEY%% as dictkey, count(*) as dictkeycount from %%VVV%%% group by %%KEY%%"
  db.dictionary.obj
}

