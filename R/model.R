#' @title create_model
#' @description Function for creating a model. If successful returns the model's id number.
#' @import httr
#' @import keyring
#' @export
#' @param name A string representing the name of the model.
#'
create_model <- function(name) {
  token <- get_token()

  # info
  cat(paste0("Creating model ", name, " ...\n"))

  request <- POST(get_endpoint("models"),
                  add_headers(Authorization=token),
                  body = list(name=name),
                  encode = "json",
                  timeout(.cloudstanr$API_TIMEOUT))

  if (request$status_code == 201) {
    # info
    cat("Model created!\n")
    return(content(request)$model_id)
  } else if (request$status_code == 401) {
    # info
    cat("Wrong credentials, please login!\n")
  } else {
    # info
    cat("Something went wrong, please try again!\n")
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
  cat("Acquiring your models ...\n")

  request <- GET(get_endpoint("models"),
           add_headers(Authorization=token),
           timeout(.cloudstanr$API_TIMEOUT))

  if (request$status_code == 200) {
    # info
    cat("Model acquisition successful!\n")

    c <- content(request)

    # cast to data.frame
    models <- NULL

    for (model in c$models) {
      models <- rbind(models, data.frame(name=model$name, id=model$id))
    }

    return(models)
  } else if (request$status_code == 401) {
    # info
    cat("Wrong credentials, please login!\n")
  } else {
    # info
    cat("Something went wrong, please try again!\n")
  }
}


#' @title get_model_details
#' @description Get details of a specific model.
#' @import httr
#' @import keyring
#' @export
#' @param id A string representing the id of the model.
#'
get_model_details <- function(id) {
  token <- get_token()

  # info
  cat("Getting model's details ...\n")

  request <- GET(get_endpoint(paste0("models/", id)),
                 add_headers(Authorization=token),
                 timeout(.cloudstanr$API_TIMEOUT))

  if (request$status_code == 200) {
    # info
    cat("Model details successfully acquired!\n")

    return(content(request))
  } else if (request$status_code == 401) {
    # info
    cat("Wrong credentials, please login!\n")
  } else {
    # info
    cat("Something went wrong, please try again!\n")
  }
}


#' @title delete_model
#' @description Delete a model.
#' @import httr
#' @import keyring
#' @export
#' @param id A string representing the id of the model.
#'
delete_model <- function(id) {
  token <- get_token()

  # info
  cat("Deleting the model ...\n")

  request <- DELETE(get_endpoint(paste0("models/", id)),
                 add_headers(Authorization=token),
                 timeout(.cloudstanr$API_TIMEOUT))

  if (request$status_code == 200) {
    # info
    cat("Model successfully deleted!\n")
  } else if (request$status_code == 401) {
    # info
    cat("Wrong credentials, please login!\n")
  } else {
    # info
    cat("Something went wrong, please try again!\n")
  }
}


#' @title edit_model
#' @description Edit a model.
#' @import httr
#' @import keyring
#' @export
#' @param id A string representing the id of the model.
#' @param code A string representing the code of the model or a filename that contains the model's code [""].
#' @param data A list containing the input data [NULL].
#' @param name A string representing the new name of the model [""].
#'
edit_model <- function(id, code="", data=NULL, name="") {
  token <- get_token()

  # info
  cat("Uploading model's code and data ...\n")

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
                    timeout(.cloudstanr$API_TIMEOUT))

  if (request$status_code == 200) {
    # info
    cat("Model successfully updated!\n")
    return(content(request))
  } else if (request$status_code == 401) {
    # info
    cat("Wrong credentials, please login!\n")
  } else {
    # info
    cat("Something went wrong, please try again!\n")
  }
}


#' @title compile_model
#' @description Compile a model.
#' @import httr
#' @import keyring
#' @export
#' @param id A string representing the id of the model.
#' @param compile_time An integer representing the amount of seconds we will wait before terminating the compilation process [300].
#'
compile_model <- function(id, compile_time=300) {
  token <- get_token()

  # info
  cat("Compiling the model ...\n")

  request <- POST(get_endpoint(paste0("models/", id, "/compile")),
                  add_headers(Authorization=token),
                  timeout(compile_time))

  if (request$status_code == 200) {
    # info
    cat("Model successfully compiled!\n")
    return(content(request))
  } else if (request$status_code == 401) {
    # info
    cat("Wrong credentials, please login!\n")
  } else if (request$status_code == 400) {
    c <- content(request)
    # info
    cat(c$message)
  }else {
    # info
    cat("Something went wrong, please try again!\n")
  }
}
