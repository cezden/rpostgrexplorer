#' @export
db.sketch.attribute <- function(db.connection, metadata.inf, attribute.name, att.value, results.limit = 10){
  query.inst <- attribute.instances(metadata.inf, attribute.name)
  
  count.query.gen <- function(fromspec, attribute.name, att.value){
    paste0("select count(*) from ", fromspec, " where ", attribute.name, "=", att.value)
  }
  val.query.gen <- function(fromspec, attribute.name, att.value, limit){
    paste0("select * from ", fromspec, " where ", attribute.name, "=", att.value, " limit ",limit)
  }
  
  qs <- query.inst %>% dplyr::mutate(schemed.tab = sql.table.schemed(tab.name = tablename, schema.name = schemaname) ) %>% 
    dplyr::mutate(count.query = count.query.gen(fromspec = schemed.tab, attribute.name = attribute.name, att.value = att.value)) %>%
    dplyr::mutate(val.query = val.query.gen(fromspec = schemed.tab, attribute.name = attribute.name, att.value = att.value, limit = results.limit)) %>%
    dplyr::mutate(val.query.name = paste(schemed.tab, attribute.name, "value", sep = "."), 
                  count.query.name = paste(schemed.tab, attribute.name, "count", sep = "."))
  
  querylist <- c(qs$val.query, qs$count.query)
  names(querylist) <- c(qs$val.query.name, qs$count.query.name)
  querylist <- as.list(querylist[order(names(querylist))])
  query.load.execute(querylist, db.connection)
}

