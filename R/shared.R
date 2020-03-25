# global settings
api_URL <- "http://18.236.242.235:3000/api/v1.0/"
api_timeout <- 10

# get endpoint
get_endpoint <- function(endpoint) {
  return(paste0(api_URL, endpoint))
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
