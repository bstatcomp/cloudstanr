#' @title sample_model
#' @description Function for starting the sampling process.
#' @import httr
#' @import keyring
#' @export
#' @param id A string representing the id of the model.
#' @param samples An integer representing the desired amount of samples [1000].
#' @param warmup An integer representing the amount of warmup steps [1000].
#' @param async A boolean for triggering asynchronous sampling [FALSE].
#'
sample_model <- function(id, warmup=1000, samples=1000, async=FALSE) {
  token <- get_token()

  # info
  cat("Starting the sampling process ...\n")

  request <- POST(get_endpoint(paste0("models/", id, "/sample")),
                  add_headers(Authorization=token),
                  body = list(num_warmup=warmup,
                              num_samples=samples),
                  encode = "json",
                  timeout(.cloudstanr$API_TIMEOUT))

  if (request$status_code == 200) {
    # info
    cat("Sampling started!\n")

    # if sampling is not async query status every timeout seconds
    if (!async) {
      status <- "sampling"

      while (status == "sampling") {
        # if done return samples
        if (status == "done") {
          return(get_samples(id))
        } else if (status == "sampling") {
          status <- get_sampling_status(id, async)
        } else if (stutus == "failed") {
          return(NULL)
        }

        # refresh every X seconds
        Sys.sleep(status_refresh_rate)
      }
    }
  } else if (request$status_code == 401) {
    # info
    cat("Wrong credentials, please login!\n")
  } else {
    # info
    cat("Something went wrong, please try again!\n")
  }
}


#' @title get_sampling_status
#' @description Function for checking the sampling status.
#' @import httr
#' @import keyring
#' @export
#' @param id A string representing the id of the model.
#'
get_sampling_status <- function(id, async=FALSE) {
  token <- get_token()

  # info
  if (async) {
    cat("Checking the sampling status ...\n")
  }

  request <- GET(get_endpoint(paste0("models/", id, "/fit")),
                  add_headers(Authorization=token),
                  timeout(.cloudstanr$API_TIMEOUT))

  if (request$status_code == 200) {
    # info
    c <- content(request)
    if (c$done == TRUE) {
      cat("Sampling completed!\n")
      return("done")
    } else {
      cat(paste0("State: ", c$state, ", status: ", c$iteration, "/", c$all_iterations, "\n"))
      return("sampling")
    }
  } else if (request$status_code == 401) {
    # info
    cat("Wrong credentials, please login!\n")
    return("failed")
  } else {
    # info
    cat("Something went wrong, please try again!\n")
    return("failed")
  }
}


#' @title get_samples
#' @description Function for retrieving results of sampling.
#' @import httr
#' @import keyring
#' @export
#' @param id A string representing the id of the model.
#'
get_samples <- function(id) {
  token <- get_token()

  # info
  cat("Retrieving the model's samples ...\n")

  request <- GET(get_endpoint(paste0("models/", id, "/fit/samples")),
                 add_headers(Authorization=token),
                 timeout(.cloudstanr$API_TIMEOUT))

  if (request$status_code == 200) {
    # info
    cat("Samples retrieved, printing the sampling log:\n\n")
    c <- content(request)
    cat(c$log)
    return(c)
  } else if (request$status_code == 401) {
    # info
    cat("Wrong credentials, please login!\n")
  } else {
    # info
    cat("Something went wrong, please try again!\n")
  }
}


