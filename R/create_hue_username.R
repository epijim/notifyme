#' Create a username to enable HTTP requests on your hue hub
#'
#' You need a username to access the hub. This function will create a username. Immediately before
#' running this function you need to press the link button on the bridge, the big button on the
#' Generation 2 hub, to prove you have access to this hub.
#'
#' @section Bugs:
#' Code repo: \url{https://github.com/epijim/notifyme}
#'
#' @param bridge_ip The internal ip address of your hue bridge
#' @keywords R Hue notify
#' @export
#' @importFrom magrittr "%>%"
#' @examples
#' \dontrun{(create_hue_username(bridge_ip)}

create_hue_username <- function(
  bridge_ip = NULL
){
  . <- NULL

  username <- httr::POST(
    url = paste0("http://",bridge_ip,"/api"),
    body = '{"devicetype":"my_hue_app#r notifyme"}',
    httr::verbose()
    ) %>%
    # extract username from response
    httr::content("text") %>%
    # remove start
    gsub('.*username\":\"',"",.) %>%
    # remove end
    gsub('\"}}]',"",.)
  # check result for error
  if (grepl("error", username)) {
    message("We have an error - did you press the link button on your bridge first?")
  } else {
    message("No error detected! A valid username should have been returned")
  }
  return(username)
}
