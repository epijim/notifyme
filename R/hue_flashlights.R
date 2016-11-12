#' Flash all the lights connected to your hub
#'
#' This function will flash the lights off and on a specified number of times. Currently,
#' it will effect all lights connected to the hub. If light_info is provided, it will also
#' turn all the lights red before flashing the lights, then it will reset the lights back to
#' their previous state afterwards.
#'
#' @section Bugs:
#' Code repo: \url{https://github.com/epijim/notifyme}
#'
#' @param bridge_ip Internal IP address of your hue bridge
#' @param username Username for connecting to hue bridge
#' @param flashes Number of times to flash the lights on and off
#' @param light_info Optional, must be output of get_light_info - if included the function will turn the lights red. The table tells it what state to then reset the lights to.
#' @keywords R Hue notify
#' @export
#' @importFrom magrittr "%>%"
#' @examples
#' \dontrun{hue_flashlights(bridge_ip,username)}

hue_flashlights <- function(
  bridge_ip = NULL,
  username = NULL,
  light_info = NULL,
  flashes = 3
){
  # avoid missing objects in namespace
    hue <- NULL
    lights <- NULL
    . <- NULL
  # make the lights red, flash, then return
    if (!is.null(light_info)) {
      # get coloured lights
      colouredlights <- light_info %>%
        dplyr::filter(hue >= 0)
      # make red
      for(i in colouredlights$id){
        httr::PUT(
          url = paste0(
            "http://",
            bridge_ip,"/api/",
            username,
            "/lights/",
            i,
            "/state"
          ),
          body = '{"on":true,"bri":255,"sat":255,"hue":0}'
        )
        Sys.sleep(0.3)
      }
      message("Setting lights red")
    }

  # flash
    message("Flashing lights")
    for(i in 1:flashes){
      httr::PUT(
        url = paste0(
          "http://",
          bridge_ip,
          "/api/",
          username,
          "/groups/0/action"
        ),
        body = '{"alert":"select"}'
      )
      Sys.sleep(1.3)
    }

  # return to last colour
    if (!is.null(light_info)) {
      for(i in colouredlights$id){
        # reset colour
        httr::PUT(
          url = paste0(
            "http://",
            bridge_ip,"/api/",
            username,
            "/lights/",
            i,
            "/state"
          ),
          body = paste0(
            '{"hue":',colouredlights$hue[colouredlights$id==i],'}')
        )
        # reset state
        Sys.sleep(0.5)
        httr::PUT(
          url = paste0(
            "http://",
            bridge_ip,"/api/",
            username,
            "/lights/",
            i,
            "/state"
          ),
          body = paste0(
            '{"on":',tolower(colouredlights$current[colouredlights$id==i]),'}')
        )
        Sys.sleep(0.5)
      }
    }
}
