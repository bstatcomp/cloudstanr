# global settings
.cloudstanr <- new.env(parent = emptyenv())
.cloudstanr$API_URL <- NULL
.cloudstanr$API_TIMEOUT <- 30
.cloudstanr$API_STATUS_REFRESH_RATE <- 3

cloudstanr_default_api_url <- function() {
  "http://34.208.56.19:3000/api/v1.0/"
}

cloudstanr_initialize <- function() {
  api_url <- Sys.getenv("CLOUDSTANR_API_URL")
  if (isTRUE(nzchar(api_url))) {
    .cloudstanr$API_URL <- api_url
  } else {
    .cloudstanr$API_URL <- cloudstanr_default_api_url()
  }
}

# get endpoint
get_endpoint <- function(endpoint) {
  return(paste0(.cloudstanr$API_URL, endpoint))
}

# get the token
get_token <- function() {
  token <- 1
  # get stored token
  keylist <- key_list("cloudstanr")
  if (nrow(keylist) > 0) {
    # get token
    key <- keylist %>% filter(username == "token")
    if (nrow(key) > 0) {
      token <- key_get("token", service="cloudstanr")
    }
  }
  return(token)
}
