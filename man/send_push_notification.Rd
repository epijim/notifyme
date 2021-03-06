% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/send_push_notification.R
\name{send_push_notification}
\alias{send_push_notification}
\title{Send push notification to devices}
\usage{
send_push_notification(title = "Your R session says:",
  message = paste0("Message sent: ", Sys.time()), api_token = NULL,
  user_key = NULL, priority = "medium", file = "~/r_keychain.rds")
}
\arguments{
\item{title}{Title of the push notification. Defaults to message from r.}

\item{message}{Message body. Default just tells time message sent.}

\item{api_token}{API token - create your own in a few minutes from pushover.net dashboard.}

\item{user_key}{This is the key that identifies you. It's on the pushover.net dashboard.}

\item{priority}{'low' means no beep/vibrate, 'medium' means beep/vibrate, 'high' means require response on device.}

\item{file}{Optional - location of keychain if using.}
}
\description{
This function will send a push notification to your device via the
push over API. You must make an account with that service (pushover.net)
and get an API key and userkey.
}
\section{Bugs}{

Code repo: \url{https://github.com/epijim/notifyme}
}
\examples{
\dontrun{send_push_notification(user_key = "xxxxxx", api_token = "xxxxx")}
}
\keyword{Hue}
\keyword{R}
\keyword{notify}
\keyword{pushover}

