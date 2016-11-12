#' Get the internal IP address of your Hue bridge
#'
#' This function uses the bridge UPNP service to get the internal IP address
#' of your hue hub. You need to be on the same network, else you'll get an error.
#' This is not the only method to get the bridge's ip address, but is probably the easiest.
#' This function has no parameters/inputs.
#'
#' @section Bugs:
#' Code repo: \url{https://github.com/epijim/notifyme}
#'
#' @keywords R Hue notify
#' @export
#' @importFrom magrittr "%>%"
#' @examples
#' \dontrun{get_hue_ip()}

get_hue_ip <- function(){
  . <- NULL

  # IP address of bridge
  # get IP from discovery service
  bridge_ip <- readLines(
    "https://www.meethue.com/api/nupnp",
    warn = F # not expecting new line
    ) %>%
    # remove start
    gsub('.*internalipaddress":"',"",.) %>%
    # remove end
    gsub('\"}]',"",.)
  message("Your Hue Bridge IP address is: ",bridge_ip)
  return(bridge_ip)
}
