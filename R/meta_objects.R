to.define <- function(){

db.attrib <- function(attribute.spec, attribute.type = NULL){
  
}

op.all <- function(){
  
}

op.some <- function(min.number = 1){
  
}

op.none <- function(){
  
}


rule.matcher <- function(){
  
}

## op.all("a", "b", op.some("c", "d"), op.none("e", "f"))

## op.some <-> or

## op.all <-> and

}

#' Constructor for class db.attribute
#' 
#' Description of PostgreSQL attribute
#' 
#' @param schemaname the name of DB schema
#' @param tablename the name of DB table
#' @param attname the name of the attribute
#' @param attlen primary length of the type
#' @param attlen2 secondary length of the type
#' @param colposition position of the column in table
#' @param internalid (reserved)
#' @param internalname (reserved)
#' @export
db.attribute <- function(schemaname, tablename, attname, 
                         typename, attlen = -1, attlen2 = -1, 
                         colposition,
                         internalid, internalname){
  db.att.obj <- 
  list("schemaname" = schemaname, 
       "tablename" = tablename, 
       "attname" = attname, 
       "typename" = typename, 
       "attlen" = attlen, 
       "attlen2" = attlen2, 
       "colposition" = colposition, 
       "internalid" = internalid, 
       "internalname" = internalname)  
  
  class(db.att.obj) <- "db.attribute"
  db.att.obj
}

#' Constructor for class db.attribute.pg.constraints
#' 
#' PostgreSQL constraints concerning the attribute
#' 
#' @param schemaname the name of DB schema
#' @param tablename the name of DB table
#' @param attname the name of the attribute
#' @param notnull has the attribute NOT NULL constraint?
#' @param hasdefault has the attribute default value?
#' @param internalid (reserved)
#' @param internalname (reserved)
#' @export
db.attribute.pg.constraints <- function(schemaname, tablename, attname, 
                         notnull = FALSE, hasdefault = FALSE, 
                         internalid, internalname){
  
  ### TO GET: 
  # db.constr.pk = NA, 
  # db.constr.not.null = NA, 
  # db.constr.unique = NA, 
  # db.constr.fk = NA, 
  # db.constr.fk.references = NA,
  #### sources: http://www.postgresql.org/docs/8.1/static/catalog-pg-constraint.html
  
  
  db.att.obj <- 
    list("schemaname" = schemaname, 
         "tablename" = tablename, 
         "attname" = attname, 
         "notnull" = notnull, 
         "hasdefault" = hasdefault, 
         "internalid" = internalid, 
         "internalname" = internalname)  
  
  class(db.att.obj) <- "db.attribute.pg.constraints"
  db.att.obj
}


#' Constructor for class db.attribute.pg.stats
#' 
#' PostgreSQL statistics concerning the attribute
#' 
#' @param schemaname the name of DB schema
#' @param tablename the name of DB table
#' @param attname the name of the attribute
#' @param statlevel level of the statistics collected by DB
#' @param null_frac DB-estimated proportion of NULL values
#' @param n_distinct DB-estimated number of distinct values
#' @param most_common_vals JSON-encoded table of most common attribute values (as estimated by DB)
#' @param most_common_freqs JSON-encoded table of the frequencies of most common attribute values (as estimated by DB)
#' @param histogram_bounds JSON-encoded table of the histogram bounds of attribute values
#' @param internalid (reserved)
#' @param internalname (reserved)
#' @export
db.attribute.pg.stats <- function(schemaname, tablename, attname, 
                                  null_frac = NA, 
                                  n_distinct = NA, 
                                  most_common_vals = NA, 
                                  most_common_freqs = NA, 
                                  histogram_bounds = NA,
                                  internalid, internalname){
  db.att.obj <- 
    list("schemaname" = schemaname, 
         "tablename" = tablename, 
         "attname" = attname, 
         "statlevel" = statlevel, 
         "null_frac" = null_frac, 
         "n_distinct" = n_distinct, 
         "most_common_vals" = most_common_vals, 
         "most_common_freqs" = most_common_freqs, 
         "histogram_bounds" = histogram_bounds, 
         "internalid" = internalid, 
         "internalname" = internalname)  
  
  class(db.att.obj) <- "db.attribute.pg.stats"
  db.att.obj
}


db.table <- function(schemaname, tablename, attributenumber, internalid, internalname){
  db.att.obj <- 
    list("schemaname" = schemaname, 
         "tablename" = tablename, 
         "attributenumber" = attributenumber, 
         "count_estimate" = count_estimate, 
         "has_index" = has_index, 
         "has_primary_key" = has_primary_key, 
         "internalid" = internalid, 
         "internalname" = internalname)
  class(db.att.obj) <- "db.table"
  db.att.obj
}
