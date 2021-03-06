% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sports_interests.R
\name{major_sports}
\alias{major_sports}
\title{FB audience sports breakdown}
\usage{
major_sports(id_vector, name_vector, platform)
}
\arguments{
\item{id_vector:}{Vector of one or more facebook API interests}

\item{name_vector:}{Vector of one or more facebook API names}

\item{platform:}{"instagram", "facebook" or c("facebook", "instagram")}
}
\description{
Function that returns a data frame with the total audience for selected major sports.
}
\examples{
major_sports("6003123299417", "Computer science", "facebook")
}
\keyword{FB}
\keyword{ads}
\keyword{audience}
