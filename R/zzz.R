.onLoad <- function(libname, pkgname) {
  assign("pkg_globals", new.env(), envir=parent.env(environment()))
  sql.files <- c("tables_list", "table_attributes_list", "relationship_miner", "objects_list")
  for (sqlfile in sql.files){
    sqlfile.name <- paste0(sqlfile,".sql")
    sqlfile.contents <- readLines(system.file("sql", sqlfile.name, package = pkgname))    #read sql file content
    #remove comments
    sqlfile.contents.filtered <- Filter(
      function(x){
        !(stringi::stri_startswith_fixed(stringi::stri_trim_left(x), "--"))
      }
      , sqlfile.contents)
    final.sql <- paste(sqlfile.contents.filtered, collapse = "\n")
    assign(sqlfile, final.sql, pkg_globals)
  }
}