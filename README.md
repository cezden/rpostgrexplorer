rpostgrexplorer
===============

R package for PostgreSQL and Redshift metadata explorations


<h3> Motivation </h3>

Imagine the situation when you are given a poorly (or too-well) documented PostgreSQL-stored data to analyze.
Datatables lacks the constraints, some are empty or residual, all is a bit messy.
The **rpostgrexplorer** package is to help to navigate the analysis on the sea of the messy data.

<h3> Installation </h3>

```{Ruby}
devtools::install_github("cezden/rpostgrexplorer")
```

<h3> Sample session </h3>

<h4> Initialization </h4>


```{Ruby}
library(rpostgrexplorer)

control.connection <- db.connection(user="username", password="userpassword",
                          host="database.server.name", port=5439, 
                          dbname="databasename")  

meta.inf <- rpostgrexplorer::load.metadata(control.connection = control.connection, schemaname = "schema1")

```

<h4> Querying the metadata object </h4>

<h5> Listing the attributes and tables </h5>

```{Ruby}

head(db.attributes(meta.inf))
head(db.tables(meta.inf))
```
The data.frame listing attribute names along with the counts of their occurences in the tables:
```{Ruby}
#without DB type information
head(db.attributes.counts(meta.inf)) 

#with DB type information (important if some attributes share name but not the type)
head(db.attributes.counts(meta.inf), typeinfo = TRUE) 

```

<h5> Describing an attribute </h5>

Description of the attribute consists of:
* number of instances along with db type
* the most similar names of other attributes
 
```{Ruby}

describe.attribute(meta.inf, "cookieid")

```
producing:

```
$instances.count
Source: local data frame [1 x 2]

                attname typename tablecount
1      cookieid             int8         38

$similar
Source: local data frame [3 x 2]

                attname tablecount
1      ccookieid                 1
2      cookieiid                 1
3      coookieid                 1
```

<h5> Tables and attributes search </h5>

Selecting all instances of the attributes with some name:

```{Ruby}
attribute.instances(meta.inf, c("cookieid", "consumerid"))
```

Selecting all tables containing all attributes from given set:

```{Ruby}
tables.with.attributes(meta.inf, c("cookieid", "consumerid"))
```

<h4> Filtering the metadata object </h4>

The contents of the metadata object can be filtered by filtering tables or attributes according to 
arbitrary criteria using the *restrict* function.
Since the results of the query functions (*db.attributes*, *db.attributes.counts* and *db.tables*) are *data.frame*
objects the restricting mechanism is quite flexible.

<h5> Selecting attributes with more than 100 instances </h5>

```{Ruby}
library(dplyr)
restriction <- db.attributes.counts(meta.inf)  %>% dplyr::filter(tablecount>250)
meta.inf.restricted <- restrict(meta.inf, restriction) 
db.attributes.counts(meta.inf.restricted)

```


<h4> Querying the database based on the metadata object </h4>

<h5> Selecting data from all tables containing given attribute </h5>

```{Ruby}

cookie.sketch <- experimental.db.sketch.attribute(control.connection, meta.inf, "cookieid", "31415")

```

<h5> Mining the relationships beetween attribute instances </h5>

In order to check the type of the relationship between instances of the attribute you may use

```{Ruby}

cookieid.atts <- attribute.instances(meta.inf, "cookieid")
cookieid.atts.from <- cookieid.atts[1:3,]

experimental.db.infer.relation.simple(control.connection, cookieid.atts.from, cookie.atts)

```
Based on its output you can infer the type of relationship (0+ -- 0+, 1 -- 0+, etc) and check their quality.
