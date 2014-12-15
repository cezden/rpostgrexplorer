rpostgrexplorer
===============

R package for PostgreSQL and Redshift metadata explorations


<h4> Motivation </h4>

Imagine the situation when you are given a poorly (or too-well) documented PostgreSQL-stored data to analyze.
Datatables lacks the constraints, some are empty or residual, all is a bit messy.
The **rpostgrexplorer** package is to help to navigate the analysis on the sea of the messy data.

<h4> Installation </h4>

```{Ruby}
devtools::install_github("cezden/rpostgrexplorer")
```

<h4> Sample session </h4>

<h5> Initialization </h5>


```{Ruby}
library(rpostgrexplorer)

control.connection <- db.connection(user="username", password="userpassword",
                          host="database.server.name", port=5439, 
                          dbname="databasename")  

meta.inf <- rpostgrexplorer::load.metadata(control.connection = control.connection, schemaname = "schema1")

```
