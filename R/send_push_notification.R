#' Send push notification to devices
#'
#' This function will send a push notification to your device via the
#' push over API. You must make an account with that service (pushover.net)
#' and get an API key and userkey.
#'
#' @section Bugs:
#' Code repo: \url{https://github.com/epijim/notifyme}
#'
#' @param title Title of the push notification. Defaults to message from r.
#' @param message Message body. Default just tells time message sent.
#' @param api_token API token - create your own in a few minutes from pushover.net dashboard.
#' @param user_key This is the key that identifies you. It's on the pushover.net dashboard.
#' @param priority 'low' means no beep/vibrate, 'medium' means beep/vibrate, 'high' means require response on device.
#' @param file Optional - location of keychain if using.
#' @keywords R Hue notify pushover
#' @export
#' @importFrom magrittr "%>%"
#' @examples
#' \dontrun{send_push_notification(user_key = "xxxxxx", api_token = "xxxxx")}

send_push_notification <- function (
  title = "Your R session says:",
  message = paste0("Message sent: ",Sys.time()),
  api_token = NULL,
  user_key = NULL,
  priority = "medium",
  file = "~/r_keychain.rds"
){
  {
  # set priority
    switch(priority,
           low = {
             priority <- "-1"
           },
           medium = {
             priority <- "1"
           },
           high = {
             priority <- "2"
           },
           stop("For priority, please enter 'low', 'medium' or 'high'")
    )

  # if vars missing, see if saved in keychain
    # if file exists, load it
    if (
      (is.null(api_token) | is.null(user_key)) & file.exists(file)) {
      # check present
        if (!"pushover_userkey" %in% readRDS(file)$api_var) stop("hue_ip missing from keychain")
        if (!"pushover_apitoken" %in% readRDS(file)$api_var) stop("hue_username missing from keychain")

      # get key
        user_key <- get_private_keys(
          api_var = "pushover_userkey",
          file = "~/r_keychain.rds"
        )
        # get api token
        api_token <- get_private_keys(
          api_var = "pushover_apitoken",
          file = "~/r_keychain.rds"
        )
    }


  # response

    response <- httr::POST(
      "https://api.pushover.net/1/messages.json",
      body = list(
        user = user_key,
        message = message,
        token = api_token,
        priority = priority)
    )

    # check for error
    if (grepl("error",response %>% httr::content("text"))) {
      #found error
      message(
        response %>% httr::content("text") %>% cat()
      )
    } else {
      # no error, give back message with quota left
      message("Message sent: ",
              round(100*
                      as.numeric(httr::headers(response)$`x-limit-app-remaining`)/
                      as.numeric(httr::headers(response)$`x-limit-app-limit`),
                    1),"% of your allowance is remaining"
      )
    }
  }
}
