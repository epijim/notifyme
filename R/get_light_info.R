#' Get info on lights connected to your Hue hub
#'
#' This will return a dataframe containing information about the lights
#' connected to your hue hub. It includes info on current, colour, is it on,
#' and is it powered/reeachable.
#'
#' @section Bugs:
#' Code repo: \url{https://github.com/epijim/notifyme}
#'
#' @param bridge_ip Internal IP address of your hue bridge
#' @param username Username for connecting to hue bridge
#' @param file If the file listed here exists, it will try and use the private keys created by the function save_private_keys(). Just ignore if you want to manually give your bridge ip and username.
#' @keywords R Hue notify
#' @export
#' @importFrom magrittr "%>%"
#' @examples
#' \dontrun{get_light_info(bridge_ip,username)}

get_light_info <- function(
  bridge_ip = NULL,
  username = NULL,
  file = "~/r_keychain.rds"
){
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

  # pull down data on lights in bridge
    lights_list <- httr::GET(
        url = paste0(
          "http://",bridge_ip,
          "/api/",username,"/lights"
        )
      ) %>% httr::content()

  # make list into dataframe
    lights <- NULL
    for(i in 1:length(lights_list)){
      lights <- rbind(
        data.frame(
          # light id
          id = i,
          # user defined name
          name = lights_list[[i]]$name,
          # is light on the network now?
          reachable = lights_list[[i]]$state$reachable,
          # is light on now?
          current = lights_list[[i]]$state$on,
          # what type of light is it?
          type = lights_list[[i]]$type,
          # what is the current hue?
          #   slightly messy as some are white only
          hue = ifelse(
            is.null(lights_list[[i]]$state$hue),
            -1,lights_list[[i]]$state$hue
          )
        ),
        lights
      )
    }
    return(lights)
}

