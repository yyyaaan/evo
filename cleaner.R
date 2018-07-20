library(tidyverse)
library(odbc)
library(DBI)
con <- dbConnect(odbc::odbc(), "Study Database")

# read data ---------------------------------------------------------------
# d <- dbFetch(dbSendQuery(con, "SELECT * FROM mobile"))
# d <- dbReadTable(con, "mobile")
# data <- readRDS("data.rds")

unique(data$id) %>% length()
unique(data$key) -> test


# latest phones have error in announced date ------------------------------

rowid <- which(data$key %in% unique(data$key)[6:24])
data[rowid -1 ,]$value <- data[rowid,]$key
data <- data[-rowid,]


# key names change --------------------------------------------------------

simpleRemove <- which(data$key %in% test[c(8,15,16,21,29,30,31,32,34,35,37,63,64,69,75,93,107,108,111,118,119,124,134,144,149,151,154,155,156,158,162,163)])
data$key[simpleRemove] <- str_remove_all(data$key[simpleRemove], '\n.*')

  # shall be repeated manually for 33 36 87-92 105 109 125-130 137 147 148
rowid <- which(data$key == test[6]); data$key[rowid]
data$key[rowid] <- "Expected announcement"

  # remove :
data$key <- data$key %>% str_remove_all(":")



# long to wide ------------------------------------------------------------


l <- reshape(d,
             direction = "wide",
             timevar = "key",
             v.names = "value",
             idvar = c("id"))

colnames(l)[3:162] <- gsub("value.", "", colnames(l)[3:162])
l <- l[,-124] #expected announcement duplicated


# save and writeSQL -------------------------------------------------------

res <- tryCatch({
  dbWriteTable(con, "mobile_L", l)
}, error = function(e) e)
cat(conditionMessage(res))
