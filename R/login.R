#' @title login
#' @description Function for logging into the Cloudstan platform.
#' @import httr
#' @import getPass
#' @import keyring
#' @import dplyr
#' @export
#' @param email A string representing an email of you cloudstan account [""].
#' @param query A boolean whether the entered password will be remebered [FALSE].
#'
login <- function(email) {
  # password
  password <- ""
  remember <- "y"

  # get stored password from the keylist
  keylist <- key_list("cloudstanr")
  if (nrow(keylist) > 0) {
    # get password
    has_password <- keylist %>% filter(username == "cloudstanr_pass")
    if (nrow(has_password) > 0) {
      password <- key_get("cloudstanr_pass", service="cloudstanr")
    }

    # get remember
    has_remember <- keylist %>% filter(username == "cloudstanr_remember")
    if (nrow(has_remember) > 0) {
      remember <- key_get("cloudstanr_remember", service="cloudstanr")
    }
  }

  # if password was not stored query
  if (password == "") {
    # get password
    password <- getPass("Please enter your password:")

    if (remember == "y") {
      # would you like to remember
      remember <- tolower(readline("Would you like R to remember your password [y/n]? "))

      # yes
      if (remember == "y" | remember == "yes") {
        # store
        key_set_with_value("cloudstanr_pass", password=password, service="cloudstanr")
        key_set_with_value("cloudstanr_remember", password="y", service="cloudstanr")
      }
    }
  }

  cat(password)
  #request <- POST("http://18.236.242.235:3000/api/v1.0/users/login",
  #                body = list(email=email,
  #                            password="test1234"),
  #                verbose(),
  #                encode = "json",
  #                timeout(3))

  #return(request)
}

#' @title clear_password
#' @description Function for removing the stored password.
#' @import keyring
#' @import dplyr
#' @export
clear_password <- function() {
  # delete password if stored
  keylist <- key_list("cloudstanr")
  if (nrow(keylist) > 0) {
    # get password
    has_password <- keylist %>% filter(username == "cloudstanr_pass")
    if (nrow(has_password) > 0) {
      key_delete("cloudstanr_pass", service="cloudstanr")
    }

    # get remeber data
    has_remember <- keylist %>% filter(username == "cloudstan_remember")
    if (nrow(has_remember) > 0) {
      key_delete("cloudstanr_remember", service="cloudstanr")
    }
  }
}
