library(tidyverse); library(plotly)
data <- readRDS("dataOK.rds")
myplot <- function(vector) plot_ly(data) %>% add_markers(x = 1:length(vector), y = vector)


# simple plots ------------------------------------------------------------


ggplot(data, aes(x = the.release.date, y = the.total.pixels.group)) + 
  geom_jitter() + geom_smooth()

ggplot(data, aes(x = the.release.date, y = the.screen2body.percent)) + 
  geom_jitter() + geom_smooth()


# complicated plots -------------------------------------------------------

options(stringsAsFactors = F)
temp <- summary(na.omit(data$the.systemchip.brand)) / length(na.omit(data$the.systemchip.brand))
temp <- data.frame(brand = names(temp), value = temp)
temp <- temp[order(temp$value),]
temp <- rbind(temp, c("others", sum(temp$value[1:14])))
temp <- temp[15:nrow(temp),]
ggplot(temp, aes("System Chip", y = as.numeric(value), fill = brand)) +
  geom_col(width = 1) + coord_polar(theta = "y")
remove(temp)

# processed data ----------------------------------------------------------

cleanSystemChip <- function(){
  data$the.systemchip.brand <- data$System.chip %>% word()
  data$the.systemchip.brand[which(data$the.systemchip.brand == "")] <- NA
  data$the.systemchip.brand[which(data$the.systemchip.brand == "Broadcomm")] <- "Broadcom"
  data$the.systemchip.brand[which(data$the.systemchip.brand == "Qulacomm")] <- "Qualcomm"
  data$the.systemchip.brand[which(data$the.systemchip.brand == "Xiaolong")] <- "Qualcomm"
  data$the.systemchip.brand[which(data$the.systemchip.brand == "RDA8810")] <- "RDA"
  data$the.systemchip.brand[which(data$the.systemchip.brand == "RDA8810PL")] <- "RDA"
  data$the.systemchip.brand[which(data$the.systemchip.brand == "MT6276a")] <- "MediaTek"
  data$the.systemchip.brand[which(data$the.systemchip.brand == "Hi-Silicon")] <- "HiSilicon"
  data$the.systemchip.brand[which(data$the.systemchip.brand == "Nvidia")] <- "NVIDIA"
  data$the.systemchip.brand[which(data$the.systemchip.brand == "STE")] <- "ST-Ericsson"
  # factorize
  data$the.systemchip.brand <- as.factor(data$the.systemchip.brand)
  # see it
  data$the.systemchip.brand %>% unique() %>% sort()
}

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

