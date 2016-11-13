#' Load a key from the keychain made by save_private_keys()
#'
#' This function will look for a 'keychain' file, and if found load the key you asked for. Designed to
#' be used with save_private_keys()
#'
#' @section Bugs:
#' Code repo: \url{https://github.com/epijim/notifyme}
#'
#' @param api_var The name of the api key, this is user defined.
#' @param file The name and location of the file where you want to store it. Default is same as the save function.
#' @keywords R Hue notify storekeys
#' @export
#' @importFrom magrittr "%>%"
#' @examples
#' \dontrun{get_private_keys("keyImInterestedIn")}

get_private_keys <- function (
  api_var = "pushover_userkey",
  file = "~/r_keychain.rds"
){
  # if file exists, load it
  if (file.exists(file)) {
    keychain <- readRDS(file)
  } else {
    stop("No keyfile found at ",file)
  }
  # check if variable already exists
  if (api_var %in% keychain$api_var) {
    # replace
    key <- keychain$key[keychain$api_var == api_var]
  } else {
    stop("Key not found in file")
  }
  # print code
  message(
    paste0(
      "A key called ",api_var," was found in ",file," and has been used"
    )
  )
  return(key)
}
