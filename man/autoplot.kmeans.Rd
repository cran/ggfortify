% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fortify_cluster.R
\name{autoplot.kmeans}
\alias{autoplot.kmeans}
\title{Autoplot cluster instances}
\usage{
\method{autoplot}{kmeans}(object, data = NULL, colour = "cluster", ...)
}
\arguments{
\item{object}{Clustered instance}

\item{data}{Original data used for clustering. Mandatory for \code{stats::kmeans}.}

\item{colour}{line colour for points}

\item{...}{other arguments passed to \code{autoplot::prcomp}}
}
\value{
ggplot
}
\description{
Autoplot cluster instances
}
\examples{
\dontrun{
autoplot(stats::kmeans(iris[-5], 3), data = iris)
autoplot(cluster::clara(iris[-5], 3), label = TRUE)
autoplot(cluster::fanny(iris[-5], 3))
autoplot(cluster::fanny(iris[-5], 3), frame = TRUE)
autoplot(cluster::pam(iris[-5], 3), data = iris, colour = 'Species')
autoplot(cluster::pam(iris[-5], 3), data = iris, frame = TRUE, frame.type = 't')
}
}
