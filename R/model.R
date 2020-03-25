#' @title create_model
#' @description Function for creating a model. If successful returns the model's id number.
#' @import httr
#' @import keyring
#' @export
#' @param email A string representing the name of the model [""].
#'
create_model <- function(name) {
  token <- get_token()

  # info
  cat(paste0("Creating model ", name, " ..."))

  request <- POST(get_endpoint("models"),
                  add_headers(Authorization=token),
                  body = list(name=name),
                  encode = "json",
                  timeout(api_timeout))

  if (request$status_code == 201) {
    # info
    cat("Model created!")
    return(content(request)$model_id)
  } else if (request$status_code == 401) {
    # info
    cat("Wrong credentials, please login!")
  } else {
    # info
    cat("Something went wrong, please try again!")
  }
}


#' @title get_models
#' @description Get all of my models.
#' @import dplyr
#' @import httr
#' @import keyring
#' @export
#'
get_models <- function() {
  token <- get_token()

  # info
  cat("Acquiring your models ...")

  request <- GET(get_endpoint("models"),
           add_headers(Authorization=token),
           timeout(api_timeout))

  if (request$status_code == 200) {
    # info
    cat("Model acquisition successful!")

    c <- content(request)

    # cast to tibble
    models <- NULL

    for (model in c$models) {
      models <- rbind(models, tibble(name=model$name, id=model$id))
    }

    return(models)
  } else if (request$status_code == 401) {
    # info
    cat("Wrong credentials, please login!")
  } else {
    # info
    cat("Something went wrong, please try again!")
  }
}


#' @title get_model_details
#' @description Get details of a specific model.
#' @import httr
#' @import keyring
#' @export
#'
get_model_details <- function(id) {
  token <- get_token()

  # info
  cat("Getting model's details ...")

  request <- GET(get_endpoint(paste0("models/", id)),
                 add_headers(Authorization=token),
                 timeout(api_timeout))

  if (request$status_code == 200) {
    # info
    cat("Model details successfully acquired!")

    return(content(request))
  } else if (request$status_code == 401) {
    # info
    cat("Wrong credentials, please login!")
  } else {
    # info
    cat("Something went wrong, please try again!")
  }
}


#' @title delete_model
#' @description Delete a model.
#' @import httr
#' @import keyring
#' @export
#'
delete_model <- function(id) {
  token <- get_token()

  # info
  cat("Deleting the model ...")

  request <- DELETE(get_endpoint(paste0("models/", id)),
                 add_headers(Authorization=token),
                 timeout(api_timeout))

  if (request$status_code == 200) {
    # info
    cat("Model successfully deleted!")

    return(content(request))
  } else if (request$status_code == 401) {
    # info
    cat("Wrong credentials, please login!")
  } else {
    # info
    cat("Something went wrong, please try again!")
  }
}


#' @title edit_model
#' @description Edit a model.
#' @import httr
#' @import keyring
#' @export
#'
edit_model <- function(id, code=NULL, data="", name="") {
  token <- get_token()

  # info
  cat("Uploading model's code and data ...")

  model_parameters <- list()

  # code
  if (code != "") {
    # if code is a file load from file
    if (file.exists(code)) {
      code <- readChar(code, file.info(code)$size)
    }

    model_parameters$code <- code
  }

  # data
  if (!is.null(data)) {
    model_parameters$data <- data
  }

  # name
  if (name != "") {
    model_parameters$name <- name
  }

  request <- POST(get_endpoint(paste0("models/", id)),
                    add_headers(Authorization=token),
                    body = model_parameters,
                    encode = "json",
                    timeout(api_timeout))

  if (request$status_code == 200) {
    # info
    cat("Model successfully updated!")
    return(content(request))
  } else if (request$status_code == 401) {
    # info
    cat("Wrong credentials, please login!")
  } else {
    # info
    cat("Something went wrong, please try again!")
  }
}


#' @title edit_model
#' @description Edit a model.
#' @import httr
#' @import keyring
#' @export
#'
edit_model <- function(id, code=NULL, data="", name="") {
  token <- get_token()

  # info
  cat("Uploading model's code and data ...")

  model_parameters <- list()

  # code
  if (code != "") {
    # if code is a file load from file
    if (file.exists(code)) {
      code <- readChar(code, file.info(code)$size)
    }

    model_parameters$code <- code
  }

  # data
  if (!is.null(data)) {
    model_parameters$data <- data
  }

  # name
  if (name != "") {
    model_parameters$name <- name
  }

  request <- POST(get_endpoint(paste0("models/", id)),
                  add_headers(Authorization=token),
                  body = model_parameters,
                  encode = "json",
                  timeout(api_timeout))

  if (request$status_code == 200) {
    # info
    cat("Model successfully updated!")
    return(content(request))
  } else if (request$status_code == 401) {
    # info
    cat("Wrong credentials, please login!")
  } else {
    # info
    cat("Something went wrong, please try again!")
  }
}


#' @title compile_model
#' @description Compile a model.
#' @import httr
#' @import keyring
#' @export
#'
compile_model <- function(id, compile_time=300) {
  token <- get_token()

  # info
  cat("Compiling the model ...")

  request <- POST(get_endpoint(paste0("models/", id, "/compile")),
                  add_headers(Authorization=token),
                  timeout(compile_time))

  if (request$status_code == 200) {
    # info
    cat("Model successfully compiled!")
    return(content(request))
  } else if (request$status_code == 401) {
    # info
    cat("Wrong credentials, please login!")
  } else {
    # info
    cat("Something went wrong, please try again!")
  }
}
