library(cloudstanr)

email <- "jure@cloudstan.com"

login(email)
clear_password()

library(httr)


request <- POST("http://18.236.242.235:3000/api/v1.0/users/login",
                body = list(email=email,
                            password="test1234"),
                encode = "json",
                timeout(3))

token <- content(request)$token

GET("http://18.236.242.235:3000/api/v1.0/users/me",
    add_headers(Authorization=token),
    timeout(3))$status_code

