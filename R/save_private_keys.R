#' Create a place to store API keys
#'
#' This function will look for a 'keychain' file with your keys at the place you tell it to look with the
#' file parameter. If it doesn't find it, it will make one.
#'
#' This function WILL NOT actually save the file. Instead, it will return a dataframe with the keys,
#' and give you the code to save the file to your system.
#'
#' The intended use is store API keys in the home space.
#'
#' @section Intended use:
#' Save variables in a way that can automatically get read in by my other functions in this package.
#' For the hue lights the variables I expect to see in api_var are  "hue_ip" and "hue_username", while
#' for pushover, the variables I expect to see are "pushover_userkey" and "pushover_apitoken".
#'
#' See the example for a use example.
#'
#' @section Bugs:
#' Code repo: \url{https://github.com/epijim/notifyme}
#'
#' @param api_var The name of the api key, this is user defined.
#' @param key The actual key.
#' @param name_of_outputted_object This is a convenience option, put in the name of the object you are assigning the output of the function to.
#' @param file The name and location of the file where you want to store it. Default is unix home.
#' @keywords R Hue notify storekeys
#' @export
#' @importFrom magrittr "%>%"
#' @examples
#' \dontrun{api_keys <- save_private_keys("new_key","THE KEY")}
#' \dontrun{# message returned is Run this code: saveRDS(api_keys, '~/r_keychain.rds')}
#' \dontrun{saveRDS(api_keys, '~/r_keychain.rds')}

save_private_keys <- function (
  api_var = NULL,
  key = NULL,
  name_of_outputted_object = "api_keys",
  file = "~/r_keychain.rds"
){
  # check key pair present
  if (is.null(api_var) | is.null(key)) stop("Please fill in api_var and key")

  # if file exists, load it
  if (file.exists(file)) {
    keychain <- readRDS(file)
  } else {
    keychain <- data.frame(
      api_var = "identifier for key",key = "the key", stringsAsFactors = F
    )
  }
  # check if variable already exists
  if (api_var %in% keychain$api_var) {
    # replace
    keychain$key[keychain$api_var == api_var] <- key
  } else {
    # add
    keychain <- rbind(
      keychain,
      data.frame(
        api_var = api_var,
        key = key,
        stringsAsFactors = F
      )
    )
  }
  # print code
  message(
    paste0(
      "Run this code: saveRDS(",name_of_outputted_object,", '",file,"')"
    )
  )

  return(keychain)

}
