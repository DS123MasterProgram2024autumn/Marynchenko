install.packages("rsconnect")

install.packages("config")

library(config)

config <- config::get()

name <- config$name
token <- config$token
secret <- config$secret

rsconnect::setAccountInfo(name=name,
                          token=token,
                          secret=secret)

library(rsconnect)
getwd()
rsconnect::deployApp(getwd())