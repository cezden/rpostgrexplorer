% Generated by roxygen2 (4.0.2): do not edit by hand
\name{ambiguous.attributes.metadata.info}
\alias{ambiguous.attributes.metadata.info}
\title{Abiguous attributes w.r.t. SQL type}
\usage{
\method{ambiguous.attributes}{metadata.info}(metadata.info)
}
\arguments{
\item{metadata.info}{the object of \code{\link{metadata.info}} class which to use in analysis}
}
\description{
Returns a list of attributes (in the form of data.frame) that provided metadata.info object
are ambiguous w.r.t. the SQL type, that is there exists instances of attribute with the same name and different types.
}

