library(tidyverse); library(plotly)
myplot <- function(vector) plot_ly(data) %>% add_markers(x = 1:length(vector), y = vector)


# plotting ----------------------------------------------------------------

ggplot(data, aes(x = the.release.date, y = the.total.pixels, color = the.total.pixels.group)) + geom_jitter() + geom_smooth()
ggplot(data, aes(x = the.release.date, y = the.total.pixels.group)) + geom_jitter() + geom_smooth()
ggplot(data, aes(x = the.release.date, y = the.screen2body.percent)) + geom_jitter() + geom_smooth()


# processors --------------------------------------------------------------

data %>% filter(!is.na(System.chip)) %>% with(System.chip) %>% word()


# processed data ----------------------------------------------------------


sceenSpecs <- function(){
  
  # pixels and groups
  value <- str_extract_all(data$Resolution, "[0-9]{1,4}", simplify = T)
  data$the.total.pixels <- as.numeric(value[,1]) * as.numeric( value[,2])
  grp <- c("> 4 million pixels", "1.5 - 4 million pixels" , "0.8 - 1.5 million pixels" , "0.3 - 0.8 million pixels" , "< 0.3 million pixels" )
  grp <- factor(grp, ordered = T, levels = grp[5:1])
  data$the.total.pixels.group <- NA
  data$the.total.pixels.group[between(data$the.total.pixels, 4e6, Inf)] <- grp[1]
  data$the.total.pixels.group[between(data$the.total.pixels, 1.5e6, 4e6)] <- grp[2] 
  data$the.total.pixels.group[between(data$the.total.pixels, 8e5, 1.5e6)] <- grp[3]
  data$the.total.pixels.group[between(data$the.total.pixels, 3e5, 8e5)] <- grp[4]
  data$the.total.pixels.group[between(data$the.total.pixels, 0, 3e5)] <- grp[5]
  remove(value, grp)
  
  # screen to body ratio
  data$the.screen2body.percent <- data$Screen.to.body.ratio %>% parse_number()
}

releaseDateNbrand <- function(){
  data$Release.date[2825] <- "Jun  6, 2011" #correct erros
  data$the.release.date <- data$Release.date %>%parse_date("%b %d, %Y")
  data$the.brand <- data$phoneName %>% word(1,1)
}

saveDatasets <- function(){
  saveRDS(data, file = "dataOK.rds")     # full set
  library(DBI); library(odbc)
  con <- dbConnect(odbc::odbc(), "Study Database")
  dbWriteTable(con, "mobile", data, overwrite = T)
}

