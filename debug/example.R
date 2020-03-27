# install from github
devtools::install_github("bstatcomp/cloudstanr")

library(cloudstanr)
library(ggplot2)

# if password is not provided the system will query you
login(email="jure@cloudstan.com", password="test1234")

# check my existing models
models <- get_models()

# delete all previous models
#for (model_id in models$id) {
# delete_model(id=model_id)
#}

# create a new model
model_id <- create_model("New model")

# get its
details <- get_model_details(id=model_id)
details

# add model code, can be a string of code or the model's filename
code <- "./debug/model.stan"
#code <- "model.stan"

# change the name of the model?
name <- "Normal model"

# add model data
n <- 100
y <- rnorm(n, 200, 50)
data <- list(n=n, y=y)

details <- edit_model(id=model_id, code=code, data=data, name=name)
details

# compile
details <- compile_model(id=model_id)
details$compiled

# sync (blocking) sampling - not working currently, server times out
#result <- sample_model(id=model_id, warmup=1000000, samples=1000000)
#samples <- result$samples

# async (non-blocking) sampling
# start sampling
result <- sample_model(id=model_id, warmup=1000000, samples=1000000, async=TRUE)

# check status
status <- get_sampling_status(id=model_id)
status

# get results
result <- get_samples(id=model_id)
samples <- result$samples

# traceplot and results plot
data <- data.frame(mu=as.numeric(samples$mu), sigma=as.numeric(samples$sigma))
data$ix <- 1:nrow(data)

# traceplots
ggplot(data=data, aes(x=ix, y=mu)) +
  geom_line(color="navy", alpha=0.3, size=1)

ggplot(data=data, aes(x=ix, y=sigma)) +
  geom_line(color="navy", alpha=0.3, size=1)

# histograms
ggplot(data=data, aes(x=mu)) +
  geom_histogram(fill="navy", alpha=0.3, bins=50)

ggplot(data=data, aes(x=sigma)) +
  geom_histogram(fill="navy", alpha=0.3, bins=50)
