.onLoad <- function(libname, pkgname) {
  assign("pkg_globals", new.env(), envir=parent.env(environment()))
  sql.files <- c("tables_list", "table_attributes_list")
  for (sqlfile in sql.files){
    sqlfile.name <- paste0(sqlfile,".sql")
    vvv <- readLines(system.file("sql", sqlfile.name, package = pkgname))    
    assign(sqlfile, paste(vvv, collapse = "\n"), pkg_globals)
  }
}