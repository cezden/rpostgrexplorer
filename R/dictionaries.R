
#@export
#semantics.dictionary <- function(df.db.table, v.key.field, v.value.field){
#  list(db.tables = df.db.table, db.key.cols = v.key.field, db.val.cols = v.value.field)
#}

#' @export
semantics.dictionary <- function(df.db.table, key.field, value.field){
  z <- list(
    db.table = df.db.table, 
    key.cols = key.field, 
    val.cols = value.field, 
    keys.v = NA)    
  class(z) <- "semantics.dictionary"
  z
}

#' @export
semantics.dictionary.simple <- function(meta.info, tab.name, key.attribute.name, value.attribute.name){
  
  dbt <- db.tables(meta.inf)
  dba <- db.attributes(meta.inf)
  ## DIRTY, REFINE
  lapply(1:length(tab.name), function(x){
    tbl <- dbt %>% filter(tablename == tab.name[x])
    tbl.key <- dba %>% filter(tablename == tab.name[x]) %>% filter(attname == key.attribute.name[x])
    tbl.val <- dba %>% filter(tablename == tab.name[x]) %>% filter(attname == value.attribute.name[x])
    if (any(nrow(tbl) == 0, nrow(tbl.key) == 0, nrow(tbl.val) == 0)){
      print(tab.name[x])
      print(key.attribute.name[x])
      print(value.attribute.name[x])
      stop("Error in mapping!")
    }
    semantics.dictionary(tbl, tbl.key, tbl.val)    
  })
  
}

#' @export
semantics.dictionary.set <- function(dictlist){
  #reindexing by internal idx
  internalidx <- unlist(lapply(dictlist, function(x){
    x$db.table$internalid
  }))
  names(dictlist) <- internalidx
  zz <- list(dictlist = dictlist)
  class(zz) <- "semantics.dictionary.set"
  zz
}



#' @export
semantics.dictionary.set.simple <- function(meta.info, tab.name, key.attribute.name, value.attribute.name){
  semantics.dictionary.set(semantics.dictionary.simple(meta.info, tab.name, key.attribute.name, value.attribute.name))  
}




#' @export
db.query.value <- function(x, ...) UseMethod("db.query.value")


query.semantics.dictionary.value.generic <- function(dictkey, query){
  sql.fill.template(
    "dictionary_values", 
    query.names = query, 
    list(
      "%%DICTKEY%%" = dictkey,
      "%%QUERY%%" = query
      )
    )
}


#' @export
db.query.value.semantics.dictionary <- function(semantics.dictionary.obj, control.connection){
  semantics.dictionary.obj$keys.v <- query.load.execute(
    queries = query.semantics.dictionary.value.generic(
      dictkey = semantics.dictionary.obj$key.cols$attname, 
      query = sql.select.table(semantics.dictionary.obj$db.table)
      ), 
    control.connection)[[1]]
  semantics.dictionary.obj
}

#' @export
db.query.value.semantics.dictionary.set <- function(semantics.dictionary.set.obj, control.connection){
  semantics.dictionary.set.obj$dictlist <- lapply(semantics.dictionary.set.obj$dictlist, function(x){
    db.query.value(x, control.connection)
  })
  ## generating common value set
  ### DIRTY, TODO: rewrite
  common.df <- NULL
  for(dictn in names(semantics.dictionary.set.obj$dictlist)){
    common.df <- rbind(common.df, data.frame(semantics.dictionary.set.obj$dictlist[[dictn]]$keys.v, dictname = dictn))
  }
  semantics.dictionary.set.obj$dictkeys <- common.df
  collisions.df <- common.df %>% dplyr::group_by(dictkey) %>% dplyr::summarize(cnt=n()) %>% dplyr::filter(cnt>1)
  semantics.dictionary.set.obj$collidingkeys <- collisions.df
  semantics.dictionary.set.obj
}

