% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/music_interests.R
\name{music_genres}
\alias{music_genres}
\title{FB audience music breakdown}
\usage{
music_genres(id_vector, name_vector, platform)
}
\arguments{
\item{id_vector:}{Vector of one or more facebook API interests}

\item{name_vector:}{Vector of one or more facebook API names}

\item{platform:}{"instagram", "facebook" or c("facebook", "instagram")}
}
\description{
Function that returns a data frame with the total audience for selected music genres.
}
\examples{
music_genres("6003123299417", "Computer science", "facebook")
}
\keyword{FB}
\keyword{ads}
\keyword{audience}
