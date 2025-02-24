% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/status_code.R
\name{status_code}
\alias{status_code}
\title{Status code of a HTTP or HTTPS request}
\usage{
status_code(url)
}
\arguments{
\item{url}{\code{character} string with the URL address.}
}
\value{
Status code of the response to the issued request.
}
\description{
\code{status_code} uses functions from the \code{httr2} package to check and
return the status code of the connection.
}
\examples{
\donttest{
  status_code("https://www.google.com")
}

}
\keyword{internal}
