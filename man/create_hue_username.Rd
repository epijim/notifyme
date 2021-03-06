% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_hue_username.R
\name{create_hue_username}
\alias{create_hue_username}
\title{Create a username to enable HTTP requests on your hue hub}
\usage{
create_hue_username(bridge_ip = NULL)
}
\arguments{
\item{bridge_ip}{The internal ip address of your hue bridge}
}
\description{
You need a username to access the hub. This function will create a username. Immediately before
running this function you need to press the link button on the bridge, the big button on the
Generation 2 hub, to prove you have access to this hub.
}
\section{Bugs}{

Code repo: \url{https://github.com/epijim/notifyme}
}
\examples{
\dontrun{(create_hue_username(bridge_ip)}
}
\keyword{Hue}
\keyword{R}
\keyword{notify}

