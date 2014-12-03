#' Table list and simple metadata extraction
#' 
#' Constructs the query retrieving the metadata information concerning the tables:
#' \itemize{
#'    \item{schemaname}
#'    \item{tablename}
#'    \item{number of attributes}
#'    \item{estimate of counts}
#'    \item{is table indexed?}
#'    \item{has table a primary key?}
#' } 
#' @param schemaname name of specific schema, if not provided tables from all schemas will be returned
#' @return SQL query retrieving the metadata information concerning the tables
get.tables.query <- function(schemaname = NULL){
  schema.selector <- ""
  if (!is.null(schemaname)){
    schema.selector <- paste0("and t.schemaname='", schemaname, "'")
  }
  paste("
        select 
          t.schemaname, t.tablename, 
          c.relnatts as attributenumber, c.reltuples as count_estimate, c.relhasindex as has_index, c.relhaspkey as has_primary_key
        from 
          pg_class c, 
          pg_tables t, 
          pg_namespace nsp
        where 
          c.relname = t.tablename and 
          c.relnamespace = nsp.oid and 
          nsp.nspname = t.schemaname 
          ", schema.selector, " 
        order by 
          schemaname, tablename
        ")
}


#' Table list and simple metadata extraction
#' 
#' Constructs the query retrieving the metadata information of each attribute in each table as well as statistics computed by PostreSQL:
#' \itemize{
#'    \item{schemaname}
#'    \item{tablename}
#'    \item{attribute name}
#'    \item{type of attribute}
#'    \item{length of attribute (related to type)}
#'    \item{estimated fraction of NULL values}
#'    \item{estimated number of distinct values (caution!)}
#'    \item{most common values}
#'    \item{frequency of most common values}
#'    \item{histogram bounds}
#' }
#' @param schemaname name of specific schema, if not provided tables from all schemas will be returned
get.attributes.query <- function(schemaname = NULL){
  schema.selector <- ""
  if (!is.null(schemaname)){
    schema.selector <- paste0("where a.schemaname='", schemaname, "'")
  }  
  paste("
        select 
          a.*, 
          b.null_frac, b.n_distinct, b.most_common_vals, b.most_common_freqs, b.histogram_bounds 
        from 
          (select 
              t.schemaname, t.tablename,
              a.attname,
              tp.typname,
              a.attlen
           from 
              pg_attribute a, 
              pg_class c, 
              pg_tables t, 
              pg_namespace nsp, 
              pg_type tp
            where 
              a.attrelid = c.oid and 
              c.relname = t.tablename and 
              c.relnamespace = nsp.oid and 
              a.attnum > 0 and 
              tp.oid=a.atttypid and 
              nsp.nspname = t.schemaname
          ) a
          left join 
          (select 
              t.schemaname, t.tablename,
              a.attname, 
              pgs.null_frac, pgs.n_distinct, pgs.most_common_vals, pgs.most_common_freqs, pgs.histogram_bounds 
          from 
              pg_attribute a, 
              pg_class c, 
              pg_tables t, 
              pg_namespace nsp, 
              pg_type tp, 
              pg_stats pgs 
          where 
              a.attrelid = c.oid and 
              c.relname = t.tablename and 
              c.relnamespace = nsp.oid and 
              a.attnum > 0 and 
              tp.oid=a.atttypid and 
              nsp.nspname = t.schemaname and
              pgs.schemaname = t.schemaname and 
              pgs.tablename = t.tablename and 
              a.attname = pgs.attname 
          ) b
          on (a.tablename = b.tablename and a.attname = b.attname and a.schemaname = b.schemaname)
          ",schema.selector,"
        order by 
          schemaname, tablename; 
        ")
}



