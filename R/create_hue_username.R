#' Get the internal IP address of your Hue bridge
#'
#' Plot the p-value function for one or two confidence interval pairs.
#' See following for example of the use in the literature:
#' Is flutamide effective in patients with bilateral orchiectomy?
#' Rothman, Kenneth J et al.
#' The Lancet , Volume 353 , Issue 9159 , 1184
#'
#' @section Bugs:
#' Code repo: \url{https://github.com/epijim/notifyme}
#'
#' @param
#' @keywords R Hue notify
#' @export
#' @importFrom magrittr "%>%"
#' @examples
#' get_hue_ip()

get_hue_ip <- function(){
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
