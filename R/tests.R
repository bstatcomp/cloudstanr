library(httr)
library(keyring)
library(dplyr)
library(cloudstanr)

email <- "jure@cloudstan.com"

# if password is not provided the system will query you
login(email, pass="test1234")

# create a new model
model_id <- create_model("test model")

# check my existing models
my_models <- get_models()

# get details of a model
details <- get_model_details(model_id)

# delete a model
# delete_model(model_id)

# add model code and data
code <- "model.stan"
name <- "new model name"

n <- 100
y <- rnorm(n, 200, 50)
data <- list(n=n, y=y)

details <- edit_model(id, code=code, data=data, name=name)

# compile
details <- compile_model(id)

# sample
