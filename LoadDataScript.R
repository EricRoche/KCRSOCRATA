require(RSocrata)

token <- "JJ_K1U6j1aV1y-3L4BQEDk6gPmdsqYf_evZg"
Data.311 <- read.socrata("https://data.kcmo.org/311/KCMOPS311-Data/7at3-sxhp")
nrow(Data.311)

