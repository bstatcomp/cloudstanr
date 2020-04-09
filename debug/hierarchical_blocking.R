# install from github
library(cloudstanr)

# if password is not provided the system will query you
login(email="jure@cloudstan.com", password="test1234")

# create a new model
model_id <- create_model("Hierarchical normal model")

# add model code from a filename
code <- "h_model.stan"

# data
n <- 500
m <- 10
y <- matrix( rnorm(m*n,mean=200,sd=50), m, n)
data <- list(n=n, m=m, y=y)

# store online
details <- edit_model(id=model_id, code=code, data=data, name=name)

# compile
details <- compile_model(id=model_id)

# sync (blocking) sampling
result <- sample_model(id=model_id, warmup=1000, samples=9000)
samples <- result$samples
