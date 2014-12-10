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

#' @export
tsttst <- function(){
  cat(get("tables_list", pkg_globals))
}



#sql.entity.relation2("q1", "q1c", "q1cnt", "q2", "q2c", "q2cnt")

#' @export
sql.entity.relation.simple <- function(tab1.name, tab2.name, tab1.groupping.col, tab2.groupping.col = tab1.groupping.col, schemaname1 = NA, schemaname2 = schemaname1){
  ### TODO: pay attention to the restriction of NULL values (!!!)
  schemaselector <- ""
  if (!is.na(schemaname1)){
    schemaselector <- paste0(schemaname1,".")
  }
}

#sql.entity.relation.simple(tab1.name = "T1", tab2.name = "T2", tab1.groupping.col = "col1", tab2.groupping.col = "col2")
#sql.entity.relation.simple(tab1.name = "T1", tab2.name = "T2", tab1.groupping.col = "col1", tab2.groupping.col = "col2", schemaname1 = "S1")
#sql.entity.relation.simple(tab1.name = "T1", tab2.name = "T2", tab1.groupping.col = "col1", tab2.groupping.col = "col2", schemaname1 = "S1", schemaname2 = "S2")
#sql.entity.relation.simple(tab1.name = "T1", tab2.name = "T2", tab1.groupping.col = "col1", schemaname1 = "S1", schemaname2 = "S2")
#sql.entity.relation.simple(tab1.name = "T1", tab2.name = "T2", tab1.groupping.col = "col1", schemaname1 = "S1")
#sql.entity.relation.simple(tab1.name = "T1", tab2.name = "T2", tab1.groupping.col = "col1")
