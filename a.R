library(tidyverse)
data <- readRDS("dataW.rds")

# resolutions -------------------------------------------------------------

value <- str_extract_all(data$Resolution, "[0-9]{1,4}", simplify = T)

data$the.total.pixels <- as.numeric(value[,1]) * as.numeric( value[,2])


# release date ------------------------------------------------------------
  
data$Release.date[2825] <- "Jun  6, 2011" #correct erros

data$the.release.date <- data$Release.date %>%parse_date("%b %d, %Y")


# brand name --------------------------------------------------------------

data$the.brand <- data$phoneName %>% word(1,1)





# ending ------------------------------------------------------------------

saveRDS(data, file = "dataOK.rds")
library(DBI); library(odbc)
con <- dbConnect(odbc::odbc(), "Study Database")
dbWriteTable(con, "mobile", data, overwrite = T)
