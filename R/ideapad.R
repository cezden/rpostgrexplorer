#' Multiselect for given attribute
#' 
#' Function performs a multiple selects from tables (in \code{metadata.inf}) containing the attribute \code{attribute.name}:
#' \itemize{
#'  \item counts all records of attribute \code{attribute.name} with value \code{attribute.value}
#'  \item selects a sample of at most \code{results.limit} rows
#' }
#' @param db.connection object of S3 class \code{\link{db.connection}}
#' @param metadata.inf object of S3 class \code{\link{metadata.info}}
#' @param attribute.name (\code{character}) the name of the attribute
#' @param attribute.value (\code{character}) a value of the attribute
#' @param results.limit (\code{integer}) a maximal number of returned rows
#' @export
experimental.db.sketch.attribute <- function(db.connection, metadata.inf, attribute.name, attribute.value, results.limit = 10){
  query.inst <- attribute.instances(metadata.inf, attribute.name)
  
  count.query.gen <- function(fromspec, attribute.name, att.value){
    paste0("select count(*) from ", fromspec, " where ", attribute.name, "=", att.value)
  }
  val.query.gen <- function(fromspec, attribute.name, att.value, limit){
    paste0("select * from ", fromspec, " where ", attribute.name, "=", att.value, " limit ",limit)
  }
  
  qs <- query.inst %>% dplyr::mutate(schemed.tab = sql.table.schemed(tab.name = tablename, schema.name = schemaname) ) %>% 
    dplyr::mutate(count.query = count.query.gen(fromspec = schemed.tab, attribute.name = attribute.name, att.value = attribute.value)) %>%
    dplyr::mutate(val.query = val.query.gen(fromspec = schemed.tab, attribute.name = attribute.name, att.value = attribute.value, limit = results.limit)) %>%
    dplyr::mutate(val.query.name = paste(schemed.tab, attribute.name, "value", sep = "."), 
                  count.query.name = paste(schemed.tab, attribute.name, "count", sep = "."))
  
  querylist <- c(qs$val.query, qs$count.query)
  names(querylist) <- c(qs$val.query.name, qs$count.query.name)
  querylist <- as.list(querylist[order(names(querylist))])
  query.load.execute(querylist, db.connection)
}

#' Inference of attribute domains
#'
#' Function searches for the frequently co-occuring sets of attributes
#' 
#' @param metadata.inf object of S3 class \code{\link{metadata.info}}
#' @param minimal.support the minimal support (among the tables) for the domain
#' @param maximal.length the maximal number of attributes in the domain
#' @return object of class \code{\link[arules]{itemsets}} from the \emph{arules} package
#' @export
#' @importFrom dplyr %>%
#' @importClassesFrom arules transactions itemsets
experimental.infer.domains <- function(meta.inf, minimal.support = 0.1, maximal.length = 20){
  schemed.attribs <- db.attributes(meta.inf) %>% dplyr::mutate(schema.tab = paste0(schemaname, ".", tablename))
  schemed.attribs.trans <- as(split(schemed.attribs[, "attname"], schemed.attribs[, "schema.tab"]), "transactions")
  freq.sets <- arules::eclat(schemed.attribs.trans, parameter = list(supp = minimal.support, maxlen = maximal.length))
  # infering maximal
  support.vals <- arules::quality(freq.sets)$support
  support.vals.unique <- sort(unique(support.vals))
  leveled.maximals <- lapply(
    support.vals.unique, 
    function(x){
      lev.rules <- freq.sets[support.vals==x]
      lev.rules[arules::is.maximal(lev.rules)]
    }
  )
  maximal.rules <- Reduce(arules::union, leveled.maximals[-1], leveled.maximals[[1]])
  arules::sort(maximal.rules)
}

