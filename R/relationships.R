#' Relationship matching SQL query
#' 
#' The query is constructed on 3 different levels (nested queries):
#' \itemize{
#'    \item{Level 1: construct simple counts for co-occurence of values in both columns}
#'    \item{Level 2: aggregate coocurences, drop the values}
#'    \item{Level 3: calculate the statistics of co-occurences}
#' }
#' 
#' The purpose of \emph{Level 1} query is to perform simple counting of distinct values in both columns resulting in the following table:
#' \itemize{
#'    \item{\emph{eone} the value from combined list of non-NULL values from column1 and column2}
#'    \item{\emph{aone.cnt} number of occurences of the \code{eone} in column1 or \code{NULL} in case of no occurences}
#'    \item{\emph{atwo.cnt} number of occurences of the \code{eone} in column2 or \code{NULL} in case of no occurences}
#' }
#' 
#' The purpose of \emph{Level 2} query is to perform simple processing of \emph{Level 1} query and dropping actual dictionary values, 
#' which results in the following table (described only in part concerning column1 - the part concerning column2 is symmetrical) consisting of 8 columns:
#' \itemize{
#'    \item{\emph{aone_cnt} number of occurences of the value in \emph{column1}, 0 or more}
#'    \item{\emph{aone_cnt_ind} occurence indicator of the value in \emph{column1}, 0 or 1}
#'    \item{\emph{aone_cnt_nullable} number of occurences of the value in \emph{column1} as provided by \emph{Level 1} query (exact copy of \code{aone.cnt})}
#'    \item{\emph{aone_cnt_both_present} serving the purpose of a correlator of the counts from both columns: 
#'      it is \code{NULL} when atwo.cnt is NULL, otherwise it is aone.cnt}
#' }
#' 
#' The purpose of \emph{Level 3} query is to aggregate results of \emph{Level 2} query into single row of the following structure:
#' \itemize{
#'    \item{\code{distvals_cnt}: number of distinct non-NULL values in column1 and column2 (combined)}
#'    \item{\code{f_distentities_frac}: fraction of (combined) distinct values in column1 (first)}
#'    \item{\code{s_distentities_frac}: fraction of (combined) distinct values in column2 (second)}
#'    \item{\code{distentities_match_frac}: fraction of matching values (1+ -- 1+ relationships)}
#'    \item{\code{f_mincnt}, \code{f_avgcnt}, \code{f_maxcnt}, \code{f_sdevcnt}: 
#'        \code{min}, \code{mean}, \code{max} and \code{sd} of distinct value counts in column1 (first) }
#'    \item{\code{s_mincnt}, \code{s_avgcnt}, \code{s_maxcnt}, \code{f_sdevcnt}: 
#'        \code{min}, \code{mean}, \code{max} and \code{sd} of distinct value counts in column2 (second) }
#'    \item{\code{f_sumcnt}: number of non-NULL rows in column1}
#'    \item{\code{s_sumcnt}: number of non-NULL rows in column2}
#'    \item{\code{f_avgcnt_when_present}, \code{f_sdevcnt_when_present}: 
#'        \code{mean} and \code{sd} of distinct value counts in column1 (first) when they are present}
#'    \item{\code{s_avgcnt_when_present}, \code{s_sdevcnt_when_present}: 
#'        \code{mean} and \code{sd} of distinct value counts in column2 (second) when they are present}
#'    \item{\code{f_mincnt_both_present}, \code{f_avgcnt_both_present}, \code{f_maxcnt_both_present}, \code{f_sdevcnt_both_present}: 
#'        \code{min}, \code{mean}, \code{max} and \code{sd} of of distinct value counts in column1 (first) when there are matching rows in column2}
#'    \item{\code{s_mincnt_both_present}, \code{s_avgcnt_both_present}, \code{s_maxcnt_both_present}, \code{s_sdevcnt_both_present}: 
#'        \code{min}, \code{mean}, \code{max} and \code{sd} of of distinct value counts in column2 (second) when there are matching rows in column1}
#' }
#' @export
sql.entity.relation.generic <- function(query1, query1.colname, query1.countname = "cnt", query2, query2.colname, query2.countname = "cnt"){
  ### TODO: pay attention to the restriction of NULL values (!!!)
  
  ## %%Q1_VALCOUNTER%%, %%Q1_VALNAME%%, %%Q1_QUERY%%
  ## %%Q2_VALCOUNTER%%, %%Q2_VALNAME%%, %%Q2_QUERY%%
  stringi::stri_replace_all_fixed(
    get("relationship_miner", pkg_globals), 
    c(
      "%%Q1_QUERY%%", "%%Q1_VALNAME%%","%%Q1_VALCOUNTER%%", 
      "%%Q2_QUERY%%", "%%Q2_VALNAME%%","%%Q2_VALCOUNTER%%"
    ), 
    c(
      query1, query1.colname, query1.countname,
      query2, query2.colname, query2.countname
    ),
    vectorize_all = FALSE)  
  
}

#' SQL groupped counts
create.groupping.query <- function(groupping.col, subquery, na.rm = TRUE){
  variable.name <- "eone"
  count.name <- "cnt"
  na.rm.query <- ""
  if (na.rm){
    na.rm.query <- paste0("where ", groupping.col, " is not null")
  }
  paste0("
         select ", groupping.col, " as ", variable.name, "  
                count(*) as ", count.name, " 
         from 
         (", subquery, ")
         ", na.rm.query, "
           group by ", groupping.col)
}



create.groupping.query <- function(groupping.col, tablename, schemaname = NA){
  variable.name <- "eone"
  count.name <- "cnt"
  paste0("
         select ", groupping.col, " as ", variable.name, "  
                count(*) as ", count.name, " 
         from ", schemaselector, tablename, 
         " where ", groupping.col, " is not null group by ", groupping.col)
}

#sql.entity.relation2("q1", "q1c", "q1cnt", "q2", "q2c", "q2cnt")


#' @export
sql.entity.relation.simple <- function(tab1.name, tab2.name, tab1.groupping.col, tab2.groupping.col = tab1.groupping.col, schemaname1 = NA, schemaname2 = schemaname1){
  q1 <- sql.table.schemed(tab1.name, schemaname1)
  q2 <- sql.table.schemed(tab2.name, schemaname2)
  sql.entity.relation.generic(
    query1 = q1, 
    query1.colname = tab1.groupping.col, 
    query1.countname = "cnt", 
    query2 = q2, 
    query2.colname = tab2.groupping.col, 
    query2.countname = "cnt")
}

experimental.db.infer.relation <- function(db.connection, colName, baseTable, relTables, schemaname=NA){
  relTables.enf=setdiff(unlist(relTables),baseTable)
  
  queryList=list()
  #queryList[baseTable]=get.groupping.att.query(colName,baseTable,schemaname)
  for(tabname in relTables.enf){
    #queryList[tabname]=get.groupping.att.query(colName,tabname,schemaname)
    queryList[tabname]=sql.entity.relation(colName,baseTable,tabname,schemaname)
  }
  mmm=query.load.execute(queryList,control.connection)
  resultRel=NULL
  for(tabname in relTables.enf){
    z=data.frame(f_tablename=baseTable,s_tablename=tabname,groupping_col=colName,mmm[[tabname]],stringsAsFactors=FALSE)    
    resultRel=rbind(resultRel,z)
  }
  resultRel[order(resultRel$distentities_match_frac,decreasing=TRUE),]
}

#' @export
experimental.db.infer.relation.simple <- function(db.connection, base.att, related.att){
  related.att.clean <- dplyr::setdiff(related.att, base.att)
  ### TODO: unique, just in case....
  from.rel <- base.att %>% dplyr::mutate(
    schemed.tab = sql.table.schemed(tab.name = tablename, schema.name = schemaname), 
    relpoint.id = internalid, relpoint.name = paste0(schemed.tab, ".", attname))
  
  to.rel <- related.att.clean %>% dplyr::mutate(
    schemed.tab = sql.table.schemed(tab.name = tablename, schema.name = schemaname), 
    relpoint.id = internalid, relpoint.name = paste0(schemed.tab, ".", attname))
  ##cross_join
  cross.description <- NULL
  cross.description.queries <- list()
  for(from.rel.it in 1:nrow(from.rel)){
    from.rel.elem <- from.rel[from.rel.it,]
    for(to.rel.it in 1:nrow(to.rel)){
      to.rel.elem <- to.rel[to.rel.it,]
      join.name <- paste0("***",from.rel.elem$relpoint.id, "+ + +", to.rel.elem$relpoint.id, "***")
      cross.description <- rbind(
        cross.description, 
        data.frame(
          from.rel.id = from.rel.elem$relpoint.id, 
          from.rel.name = from.rel.elem$relpoint.name,
          to.rel.id = to.rel.elem$relpoint.id, 
          to.rel.name = to.rel.elem$relpoint.name, 
          join.name = join.name
          )
        )
      cross.description.queries[[join.name]] <-
        sql.entity.relation.generic(
        query1 = paste0("select ", from.rel.elem$attname, " from ", from.rel.elem$schemed.tab), 
        query1.colname = from.rel.elem$attname, 
        query1.countname = "cnt", 
        query2 = paste0("select ", to.rel.elem$attname, " from ", to.rel.elem$schemed.tab), 
        query2.colname = to.rel.elem$attname, 
        query2.countname = "cnt")
    }
  }
  rel.results <- query.load.execute(cross.description.queries, db.connection)
  ## repacking as data.frame
  rel.results.repack <- query.flatten.results(rel.results, "join.name")
  rel.results.df <- rel.results.repack$results
  res <- dplyr::inner_join(x = rel.results.df, y = cross.description, by = rel.results.repack$idfield) %>% 
    dplyr::select(-one_of(rel.results.repack$idfield))
  
  
  ## join & rename both from.rel and to.rel
  
  res
}