% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Monoids.R
\name{is_monoid}
\alias{is_monoid}
\title{is_monoid}
\usage{
is_monoid(x)
}
\arguments{
\item{x}{The object whose type to test}
}
\value{
boolean whether or not the type implements monoid
functionality
}
\description{
Check if a type is a monoid, checks for implementations
of \code{mappend} and \code{mempty}
}
