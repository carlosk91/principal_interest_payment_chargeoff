#### Libraries for algo ####
library(tidyverse)
library(glue)
library(FinancialMath)
library(capitalR)
library(lubridate)
library(magrittr)
library(h2o)
library(furrr)
library(progressr)
plan(multisession, workers = 2)

h2o.init()
# h2o.clusterInfo()
# h2o.shutdown(prompt = FALSE)





