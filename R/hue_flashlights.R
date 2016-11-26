#' Flash all the lights connected to your hub
#'
#' This function will flash the lights off and on a specified number of times. Currently,
#' it will effect all lights connected to the hub.
#'
#' @section Bugs:
#' Code repo: \url{https://github.com/epijim/notifyme}
#'
#' @param bridge_ip Internal IP address of your hue bridge
#' @param username Username for connecting to hue bridge
#' @param flashes Number of times to flash the lights on and off
#' @param file optional location of the keychain, if using
#' @param flash_red Do you want the lights to turn red before flashing?
#' @keywords R Hue notify
#' @export
#' @importFrom magrittr "%>%"
#' @examples
#' \dontrun{hue_flashlights(bridge_ip,username)}

hue_flashlights <- function(
  bridge_ip = NULL,
  username = NULL,
  flashes = 10,
  flash_red = TRUE,
  light_name = NULL,
  file = "~/r_keychain.rds"
){
  # avoid missing objects in namespace
    hue <- NULL
    lights <- NULL
    . <- NULL

  # if vars missing, see if saved in keychain
    # if file exists, load it
    if (
      (is.null(bridge_ip) | is.null(username)) & file.exists(file)) {

      # check present
        if (!"hue_ip" %in% readRDS(file)$api_var) stop("hue_ip missing from keychain")
        if (!"hue_username" %in% readRDS(file)$api_var) stop("hue_username missing from keychain")
      # get ip
      bridge_ip <- get_private_keys(
        api_var = "hue_ip",
        file = "~/r_keychain.rds"
      )
      # get api username
      username <- get_private_keys(
        api_var = "hue_username",
        file = "~/r_keychain.rds"
      )
    }

  # get light info if needed
    # get coloured lights
    if (flash_red) {
      colouredlights <- get_light_info(
        bridge_ip,username
        ) %>%
        dplyr::filter(hue >= 0)
    }
    # if a specific light asked for, filter to it
    if (!is.null(light_name)) {
      colouredlights <- colouredlights %>%
        dplyr::filter(name %in% light_name)
    }


  # make the lights red, flash, then return
    if (flash_red) {
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
    # flash all
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
    # flash specific
    for(i in 1:flashes){
      httr::PUT(
        url = paste0(
          "http://",
          bridge_ip,
          "/api/",
          username,
          "/lights//action"
        ),
        body = '{"alert":"select"}'
      )
      Sys.sleep(1.3)
    }

  # return to last colour
    if (flash_red) {
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
