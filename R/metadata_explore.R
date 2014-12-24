### based on http://golden13.blogspot.com/2012/08/how-to-get-some-information-about.html

load.types <- function(db.connection){
  queries <- list(db.base.types = "SELECT oid, format_type(oid, NULL) AS typename FROM pg_type WHERE typtype='b'",
                  
                  
                  "" # final, closing query (not obligatory, just to simplify notation)
  )
  res <- query.load.execute(queries, control.connection)
  
  res  
}