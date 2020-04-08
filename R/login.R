#' @title login
#' @description Function for logging into the Cloudstan platform.
#' @import dplyr
#' @import getPass
#' @import httr
#' @import keyring
#' @export
#' @param email A string representing an email of you cloudstan account.
#' @param password Password behind the providede email account. [""].
#'
login <- function(email, password="") {
  if (password == "") {
    # remember pass?
    remember <- ""

    # get stored data from the keylist
    keylist <- key_list("cloudstanr")
    if (nrow(keylist) > 0) {
      # get password
      key <- keylist %>% filter(username == "password")
      if (nrow(key) > 0) {
        password <- key_get("password", service="cloudstanr")
      }

      # get remember
      key <- keylist %>% filter(username == "remember")
      if (nrow(key) > 0) {
        remember <- key_get("remember", service="cloudstanr")
      }
    }

    # if password was not stored query
    if (password == "") {
      # get password
      password <- getPass("Please enter your password:")
    }
  } else {
    remember = "n"
  }

  # info
  cat("Logging in ...\n")

  request <- POST(get_endpoint("users/login"),
                  body = list(email=email,
                              password=password),
                  encode = "json",
                  timeout(.cloudstanr$API_TIMEOUT))

  if (request$status_code == 200) {
    # info
    cat("Login successful!\n")

    if (remember == "") {
      # would you like to remember
      remember <- tolower(readline("Would you like R to remember your password [y/n]? "))

      # yes
      if (remember == "y" | remember == "yes") {
        # store
        key_set_with_value("password", password=password, service="cloudstanr")
        key_set_with_value("remember", password="y", service="cloudstanr")
      } else if (remember == "n" | remember == "N") {
        key_set_with_value("remember", password="n", service="cloudstanr")
      }
    }

    # store token and id
    c <- content(request)
    key_set_with_value("token", password=c$token, service="cloudstanr")
    key_set_with_value("id", password=c$user$id, service="cloudstanr")
  } else if (request$status_code == 401) {
    # info
    cat("Wrong login credentials!\n")
    logout()
  } else {
    # info
    cat("Something went wrong, please try again!\n")
  }
}

#' @title logout
#' @description Function for removing the stored password.
#' @import dplyr
#' @import keyring
#' @export
logout <- function() {
  # info
  cat("Logging out ...\n")

  # delete password if stored
  keylist <- key_list("cloudstanr")
  if (nrow(keylist) > 0) {
    # delete password
    key <- keylist %>% filter(username == "password")
    if (nrow(key) > 0) {
      key_delete("password", service="cloudstanr")
    }

    # delete remeber
    key <- keylist %>% filter(username == "remember")
    if (nrow(key) > 0) {
      key_delete("remember", service="cloudstanr")
    }

    # delete id
    key <- keylist %>% filter(username == "id")
    if (nrow(key) > 0) {
      key_delete("id", service="cloudstanr")
    }

    # delete token
    key <- keylist %>% filter(username == "token")
    if (nrow(key) > 0) {
      key_delete("token", service="cloudstanr")
    }
  }
}
