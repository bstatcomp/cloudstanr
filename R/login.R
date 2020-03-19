#' @title login
#' @description Functions for logging into Cloudstan API.
#' @import httr
#' @export
#' @param email Email of you cloudstan account [string].
#'
login <- function(email) {
  request <- POST("http://18.236.242.235:3000/api/v1.0/users/login",
                  body = list(email=email,
                              password="test1234"),
                  verbose(),
                  encode = "json",
                  timeout(3))

  return(request)
}
