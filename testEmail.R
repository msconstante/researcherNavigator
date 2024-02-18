library(rentrez)
library(mailR)
source("auth.R")

term <- "Test"
code <- "TEST"
email <- "msconstante@gmail.com"
myCodeMail(term, code, email)
